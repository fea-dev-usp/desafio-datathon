library(xts)

mexico <- read.csv(file = 'C:/Users/Dell/Documents/GitHub/desafio-datathon/^MXX.csv', header = TRUE,
                   sep = ',', fill = FALSE)

russia <- read.csv(file = 'C:/Users/Dell/Documents/GitHub/desafio-datathon/IMOEX.ME.csv', header = TRUE, 
                   sep = ',', fill = FALSE)

turquia <- read.csv(file = 'C:/Users/Dell/Documents/GitHub/desafio-datathon/XU100.IS.csv', header = TRUE,
                    sep = ',', fill = FALSE)

crypto <- read.csv(file = 'C:/Users/Dell/Documents/GitHub/desafio-datathon/dados_crypto.csv', header = TRUE,
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
  data <- trans_ts(data)
  #pega o a variação percentual e preenche o primeiro valor com 1
  pct_change_data <- (data/lag(data)) - 1
  
  pct_change_data <- na.fill(pct_change_data, fill = 0)
  
  #cálcula o retorno a partir da variação percentual
  return_data <- prod(pct_change_data + 1)
  return(return_data)
}

retorn_mexico <- get_return(mexico)
return_russia <- get_return(russia)
return_turquia <- get_return(turquia)
return_crypyo <- get_return(crypto_data)  #***

#ajuste de intersecção das datas
merg_mexico_crypto <- merge(mexico_market, crypto_data, join="inner")
merg_russia_crypto <- merge(russia_market, crypto_data, join="inner")
merg_turquia_crypto <- merge(turquia_market, crypto_data, join="inner")


#correlacao entre movimento btc e dos mercados locais
correlacao_mexico <- cor(merg_mexico_crypto)
correlacao_russia <- cor(merg_russia_crypto)
correlacao_turquia <- cor(merg_turquia_crypto)





