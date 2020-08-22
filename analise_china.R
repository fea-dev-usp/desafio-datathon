library(xts)
setwd("C:/Users/JOAO VICTOR/Desktop/env/desafio-datathon")

shangai <- read.csv(file = "dado_shangai.csv", fill = FALSE)
hong_kong <- read.csv(file = "dado_hongkong.csv", fill = FALSE)
crypto <- read.csv(file = "dados_crypto.csv", fill = FALSE)

trans_ts <- function(data_prices){
#transforma em time series
  stock_data <- xts(data_prices[,-1], order.by=as.Date(data_prices[,1], "%Y-%m-%d"))
  return(stock_data)
} 

get_return <- function(data) { 
  data <- trans_ts(data)
#pega o a variação percentual e preenche o primeiro valor com 1
  pct_change_data <- (data/lag(data)) - 1
  
  pct_change_data <- na.fill(pct_change_data, fill = 0)
  
#cálcula o retorno a partir da variação percentual
  return_data <- prod(pct_change_data + 1)
  return(return_data)
}

return_shangai <- get_return(shangai)
return_hongkong <- get_return(hong_kong)
return_crypto <- get_return(crypto)

dado_shangai <- trans_ts(shangai)
dado_hongkong <- trans_ts(hong_kong)
dado_crypto <- trans_ts(crypto)

merg_shangai_crypto <- merge(dado_shangai, dado_crypto, join="inner")
merg_hongkong_crypto <- merge(dado_hongkong, dado_crypto, join="inner")

cor_hongkong <- cor(merg_hongkong_crypto)
cor_shangai <- cor(merg_shangai_crypto)
