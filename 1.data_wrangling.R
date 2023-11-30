# Data Wrangling dos arquivos de dados

# Carregando arquivos ---------------------------------------------------------

train <- read_csv(file = "raw_data/train.csv")
test <- read_csv(file = "raw_data/test.csv")

features <- read_csv(file = "raw_data/features.csv")

stores <- read_csv(file = "raw_data/stores.csv")


# Wrangling -------------------------------------------------------------------

summary(train)
summary(test)
summary(stores)
summary(features)

# Transformando os NA´s das colunas markdown em zeros
features$MarkDown1 <- if_else(is.na(features$MarkDown1), 0, 
                              features$MarkDown1)
features$MarkDown2 <- if_else(is.na(features$MarkDown2), 0, 
                              features$MarkDown2)
features$MarkDown3 <- if_else(is.na(features$MarkDown3), 0, 
                              features$MarkDown3)
features$MarkDown4 <- if_else(is.na(features$MarkDown4), 0, 
                              features$MarkDown4)
features$MarkDown5 <- if_else(is.na(features$MarkDown5), 0, 
                              features$MarkDown5)

# Transformando a coluna IsHoliday (booleana) em dummy (0/1) e excluindo as 
# colunas IsHoliday
features <- fastDummies::dummy_columns(.data = features, 
                                       select_columns = "IsHoliday",
                                       remove_most_frequent_dummy = TRUE,
                                       remove_selected_columns = TRUE)

# Transformando a coluna Type em dummy e exluindo a coluna type
stores <- fastDummies::dummy_columns(.data = stores,
                                     select_columns = "Type",
                                     remove_selected_columns = TRUE,
                                     remove_most_frequent_dummy = FALSE)


# Juntando as informações em um único tibble que será usado com os algo do ML
train <- left_join(train, stores, by = c("Store"))
train <- left_join(train, features, by = c("Store","Date")) |> 
  select(-IsHoliday)

# Criando uma coluna ID
train <- train |> 
  mutate(id = paste0(Store, "_", Dept),
         .before = Store)

str(train)
summary(train)

# Salvando os dados tratados em um arquivo
save(train, file = "tidy_data/train.RData")
save(stores, file = "tidy_data/stores.RData")
save(features, file = "tidy_data/features.RData")


# Carregando arquivo RData
load(file = "tidy_data/sales_ml.RData")
load(file = "tidy_data/sales.RData")
load(file = "tidy_data/stores.RData")
load(file = "tidy_data/features.RData")


# Transformando sales em tsibble ----------------------------------------------

# Transformando em tsibble
sales_ts <- sales_ml |> 
  select(Store, Dept, Date, Weekly_Sales) |> 
  as_tsibble(key = Store | Dept,
             index = Date)

# Completando as datas com missing values
sales_ts <- sales_ts |> 
  fill_gaps(.start = min(sales_ts$Date),
            .end = max(sales_ts$Date)) |> 
  arrange(Store, Dept, Date)

# Transformando NA´s em zeros
summary(sales_ts)
sales_ts$Weekly_Sales <- if_else(is.na(sales_ts$Weekly_Sales),
                                 0,
                                 sales_ts$Weekly_Sales)

# # Criando novamente a coluna id 
# sales_ts <- sales_ts |> 
#   mutate(id = paste0(Store, "_", Dept),
#          .before = Store)

# Criando uma coluna para a semana do ano e transformando em index 
sales_ts <- sales_ts |> 
  mutate(week = yearweek(Date),
         .before = Date) |> 
  as_tsibble(index = week) |> 
  arrange(Store, Dept, Date)

# Salvando a tsibble
save(sales_ts, file = "tidy_data/sales_ts.RData")

# Carregando a tsibble
load(file = "tidy_data/sales_ts.RData")

#ver função forecast::tsoutliers()

