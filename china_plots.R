#importando bibliotecas e confirmando pasta de trabalho
# o script abaixo é do João, eu irei plotar alguns gráficos baseados nele
library(xts)
library(ggplot2)
library(dplyr)
library(quantmod)
setwd("C:/Users/tiago/Downloads/R_stats-Datathon-FGV")


shangai <- read.csv(file = "dado_shangai.csv", fill = FALSE)
hong_kong <- read.csv(file = "dado_hongkong.csv", fill = FALSE)
crypto <- read.csv(file = "dados_crypto.csv", fill = FALSE)


trans_ts <- function(country_market){
  
  #transforma em time series
  stock_data <- xts(country_market[,-1], order.by=as.Date(country_market[,1], "%Y-%m-%d"))
  return(stock_data)
}


get_return <- function(data) { 
  #pega o a variação percentual e preenche o primeiro valor com 1
  pct_change_data <- Delt(data)
  
  pct_change_data <- na.fill(pct_change_data, fill = 0)
  
  #cálcula o retorno a partir da variação percentual
  #return_data <- pct_change_data
  #return(return_data)
}


shangai_market <- trans_ts(shangai)
hong_kong_market <- trans_ts(hong_kong)
crypto_data <- xts(crypto[,-1], order.by=as.Date(crypto[,1], "%Y-%m-%d"))


return_shangai <- get_return(shangai_market)
return_hongkong <- get_return(hong_kong_market)
return_crypto <- get_return(crypto_data)


#pegando a variação mensal
return_month_shangai <- return_shangai[endpoints(return_shangai, on="months")]
return_month_hongkong <- return_hongkong[endpoints(return_hongkong, on="months")]
return_month_crypto <- return_crypto[endpoints(return_crypto, on="months")]


merg_shangai_crypto <- merge(return_shangai, return_crypto, join="inner")
merg_hongkong_crypto <- merge(return_hongkong, return_crypto, join="inner")
merg_shangai_month_crypto <- merge(return_month_shangai, return_month_crypto, join="inner")
merg_hongkong_month_crypto <- merge(return_month_hongkong, return_month_crypto, join="inner")


cor_hongkong <- cor(merg_hongkong_crypto)
cor_shangai <- cor(merg_shangai_crypto)
cor_hongkong_month <- cor(merg_hongkong_month_crypto)
cor_shangai_month <- cor(merg_shangai_month_crypto)


#plotando os gráficos das séries temporais
#merg_shangai_crypto %>% ggplot(aes(x = dado_crypto, y = dado_shangai)) +
#  geom_point() + scale_x_log10() + scale_y_log10() +geom_smooth(method = 'lm')


merg_shangai_month_crypto %>% ggplot(aes(x = Delt.1.arithmetic, y = Delt.1.arithmetic.1)) +
  geom_point() + scale_x_log10() + scale_y_log10() + geom_smooth(method="lm")

