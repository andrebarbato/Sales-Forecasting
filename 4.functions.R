# limpa os objetos do ambiente do RStudio que estão na lista
# informar um vetor com os nomes das variáveis ex.: c('obj1', 'obj2', 'obj3')
clean_env <- function(nm) {
  rm(list = nm, envir = .GlobalEnv); invisible(gc())
}

# nomalização da variavel por minimo e maximo
mm_norm <- function(x) {
  y <- (x - min(x,na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))
 return(y)
}

# transforma de volta a variavel nomalizada por minimo e maximo
mm_dnorm <- function(y, min = min, max = max) {
  x <- ((max - min) * y) + min
  return(round(x,2))
}

# função para medir o r2
r2 <- function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}

# decomposição da data em varias features
decompose_date_feat <- function(df) {
  df$Year <- as.integer(format(df$Week, "%y"))
  df  <- df |> 
    mutate(Month = as.integer(month(Week)),
           Week_Year = as.integer(week(Week)),
           Day = as.integer(day(Week)),
    )
  
  df <- df |> 
    mutate(Quarter = as.integer(quarter(Week)),
           Week_Month = as.integer(ceiling(day(Week)/7))
      
    )
  return(df)
}

# Funções para o modelo de RNN ------------------------------------------------

# cria features de data

make_date_features <- function(serie) {
  # Date features
  serie <- serie |> 
    mutate(
      Year = lubridate::year(Date),
      Month = lubridate::month(Date),
      Day = lubridate::day(Date),
      Quarter = lubridate::quarter(Date),
      Week_Y = lubridate::week(Date),
      Week_M = ceiling(Day/7)
    )
  return(serie)
}

# Cria features de lags
make_lag_features <- function(serie) {
  # Lag features 1 a 8
  serie <- serie |>
    arrange(Store, Dept, Date) |> 
    mutate(
      lag_1 = case_when(
        lag(id, n=1) == id ~ lag(Weekly_Sales, n=1),
        .default = NA),
      lag_2 = case_when(
        lag(id, n=2) == id ~ lag(Weekly_Sales, n=2),
        .default = NA),
      lag_3 = case_when(
        lag(id, n=3) == id ~ lag(Weekly_Sales, n=3),
        .default = NA),
      lag_4 = case_when(
        lag(id, n=4) == id ~ lag(Weekly_Sales, n=4),
        .default = NA),
      lag_5 = case_when(
        lag(id, n=5) == id ~ lag(Weekly_Sales, n=5),
        .default = NA),
      lag_6 = case_when(
        lag(id, n=6) == id ~ lag(Weekly_Sales, n=6),
        .default = NA),
      lag_7 = case_when(
        lag(id, n=7) == id ~ lag(Weekly_Sales, n=7),
        .default = NA),
      lag_8 = case_when(
        lag(id, n=8) == id ~ lag(Weekly_Sales, n=8),
        .default = NA)
    )
}

# treina o modelo de rnn
fit_rnn <- function(serie, learningrate, hidden_dim, numepochs, network_type) {
  
  # Date features
  serie <- make_date_features(serie)
  
  # removendo as vendas que estão com valores negativos
  serie$Weekly_Sales <- if_else(serie$Weekly_Sales < 0,
                                0,
                                serie$Weekly_Sales)
  
  # Lag features 1 a 8
  serie <- make_lag_features(serie)
  
  # substituindo NA´s por zeros
  serie[is.na(serie)] = 0
  
  #Inverter a ordem da série para pegar da última para a ´primeira
  serie <- serie[order(serie$Date, decreasing = TRUE),]
  
  # separando as variáveis x e y 
  
  y <- serie |> 
    select(Weekly_Sales) |>
    # normalizando por minimo e maximo
    map_df(mm_norm)
  
  x <- serie |> 
    select(-c(1,2,3,4,5,6,7,8)) |> 
    # normalizando por minimo e maximo
    map_df(mm_norm)
  
  # guardando o minimo e maximo para reverter o resultado da rnn
  min_y <- min(serie$Weekly_Sales, na.rm = TRUE)
  max_y <- max(serie$Weekly_Sales, na.rm = TRUE)
  
  # tranformando em matrix e definindo o tamanho dos pacotes que serão passados
  # para a rede neural no caso será 11 (definido pela quantidade de linhas)
  
  # criando uma lista de matrizes de x
  xl <- list()
  
  for (i in 1:length(x)) {
    X <- data.frame(x[,i])
    xl[[i]] <- matrix(X[,1], nrow = 11)
    
  }
  
  # nomeando os elementos na lista com o nome das variáveis
  names(xl) <- names(x)
  
  
  # criando uma matrix de y 
  Y <- data.frame(y)
  Y <- matrix(Y[,1], nrow = 11)
  
  # Definindo o periodo de traino e test
  index.train <- c(1:11)
  index.test <- c(12:13)
  
  # criando um array multidimensional de variaveis preditoras(x)
  # inicia um vector
  c <- NULL
  
  # coloca dentro do vetor todas os itens da lista xl
  for (i in 1:length(x)) {
    if (length(c)==0) {
      c <- xl[[i]]
    } else {
      c <- c(c,xl[[i]])
    }
  }
  
  # cria o array
  aX <- array(c,
              dim = c(dim(xl[[1]]),length(x)))
  
  
  # treinando o modelo RNN
  # semente 
  set.seed(123)
  
  # modelo
  model_rnn <- trainr(Y = Y[,index.train],
                      X = aX[,index.train,],
                      learningrate = learningrate,
                      hidden_dim = hidden_dim,
                      numepochs = numepochs,
                      network_type = network_type
  )
  
  # Previsão total
  Yfcst <- t(matrix(predictr(model_rnn, aX[]), nrow = 1))
  Yreal <- t(matrix(Y[], nrow = 1))
  
  # transformando para o valor normal
  Yfcst <- mm_dnorm(Yfcst, min_y, max_y)
  Yreal <- mm_dnorm(Yreal, min_y, max_y)
  
  # resultado
  result_df <- data.frame(serie$id, serie$Date, Yreal, Yfcst)
  
  # colocar na ordem normal da menor data para a maior
  result_df <- result_df |> 
    arrange(serie.Date)
  
  return(result_df)
}


# Completa as datas no tibble
complete_date_by_col <- function(x, column, start_date, end_date, by) {
  # número de agrupamentos na coluna selecionada
  n <- x[[column]] |> unique()
  
  # itera por cada grupo acrescentando a quantidade de datas faltantes
  for (i in 1:length(n)) {
    #filtra um id a cada iteração
    y <- x |> 
      filter(id == n[i])
    
    # preenche as datas faltantes
    y <- y |> 
      mutate(Date = as.Date(Date)) |> 
      complete(Date = seq.Date(start_date, 
                               end_date, 
                               by = by), 
               Store, 
               Dept, 
               id)
    
    if(exists('df_final')) {
      df_final <- df_final |> bind_rows(y)
    } else {
      df_final <- y
    }
    
    cat("\r","Processando...",round(i/length(n)*100,2), "%")
    
    invisible(gc())
    Sys.sleep(1)
    
  }
  return(df_final)
}


