library(xts)
library(quantmod)
library(ggplot2)
library(dplyr)

mexico <- read.csv(file = 'C:/Users/tiago/Downloads/R_stats-Datathon-FGV/^MXX.csv', header = TRUE,
                   sep = ',', fill = FALSE)

russia <- read.csv(file = 'C:/Users/tiago/Downloads/R_stats-Datathon-FGV/IMOEX.ME.csv', header = TRUE, 
                   sep = ',', fill = FALSE)

turquia <- read.csv(file = 'C:/Users/tiago/Downloads/R_stats-Datathon-FGV/XU100.IS.csv', header = TRUE,
                    sep = ',', fill = FALSE)

crypto <- read.csv(file = 'C:/Users/tiago/Downloads/R_stats-Datathon-FGV/dados_crypto.csv', header = TRUE,
                   sep = ',', fill = FALSE)


trans_ts <- function(country_market){
  
  #transforma em time series
  stock_data <- xts(country_market[,-1], order.by=as.Date(country_market[,1], "%Y-%m-%d"))
  return(stock_data)
}


mexico_market <- trans_ts(mexico)
russia_market <- trans_ts(russia)
turquia_market <- trans_ts(turquia)
crypto_data <- xts(crypto[,-1], order.by=as.Date(crypto[,1], "%Y-%m-%d"))


get_return <- function(data) { 
  #pega o a variação percentual e preenche o primeiro valor com 1
  pct_change_data <- Delt(data)
  
  pct_change_data <- na.fill(pct_change_data, fill = 0)
  
  #cálcula o retorno a partir da variação percentual
  #return_data <- pct_change_data
  #return(return_data)
}


#calculando a variação percentual duiária no preço das séries
return_mexico <- get_return(mexico_market)
return_russia <- get_return(russia_market)
return_turquia <- get_return(turquia_market)
return_crypto <- get_return(crypto_data)


#mensalisando as variações dos índices
return_month_mexico <- return_mexico[endpoints(return_mexico, on='months')]
return_month_crypto <- return_crypto[endpoints(return_crypto, on='months')]
return_month_turquia <- return_turquia[endpoints(return_turquia, on='months')]
return_month_russia <- return_russia[endpoints(return_russia, on='months')]


#ajuste de intersecção das datas
merg_mexico_crypto <- merge(return_mexico, return_crypto, join="inner")
merg_russia_crypto <- merge(return_russia, return_crypto, join="inner")
merg_turquia_crypto <- merge(return_turquia, return_crypto, join="inner")
merg_mexico_month_crypto <- merge(return_month_mexico, return_month_crypto, join = "inner")
merg_turquia_month_crypto <- merge(return_month_turquia, return_month_crypto, join = "inner")
merg_russia_month_crypto <- merge(return_month_russia, return_month_crypto, join = "inner")


#correlacao entre movimento btc e dos mercados locais
correlacao_mexico <- cor(merg_mexico_crypto)
correlacao_russia <- cor(merg_russia_crypto)
correlacao_turquia <- cor(merg_turquia_crypto)
correlacao_mexico_month <- cor(merg_mexico_month_crypto)
correlacao_turquia_month <- cor(merg_turquia_month_crypto)
correlacao_russia_month <-  cor(merg_russia_month_crypto)


#plotando alguns gráficos
merg_russia_month_crypto %>% ggplot(aes(x = Delt.1.arithmetic, y = Delt.1.arithmetic.1)) +
  geom_point() + geom_smooth(method = 'lm')
