library(xts)
setwd("C:/Users/JOAO VICTOR/Desktop/env/desafio-datathon")

shangai <- read.csv(file = "dado_shangai.csv", fill = FALSE)
hong_kong <- read.csv(file = "dado_hongkong.csv", fill = FALSE)
crypto <- read.csv(file = "dados_crypto.csv", fill = FALSE)

trans_ts_and_get_returns <- function(data_prices){
  
#transforma em time series
  stock_data <- xts(data_prices[,-1], order.by=as.Date(data_prices[,1], "%Y-%m-%d"))
  
#pega o a variação percentual e preenche o primeiro valor com 1
  pct_change_data <- (stock_data/lag(stock_data)) - 1
  
  pct_change_data <- na.fill(pct_change_data, fill = 0)
  
#cálcula o retorno a partir da variação percentual
  return_data <- prod(pct_change_data + 1)
  return(return_data)
}

return_shangai <- trans_ts_and_get_returns(shangai)
return_hongkong <- trans_ts_and_get_returns(hong_kong)
return_crypto <- trans_ts_and_get_returns(crypto)

