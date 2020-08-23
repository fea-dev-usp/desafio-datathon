library(xts)
library(ggplot2)
library(tidyr)
library(roll)
setwd("C:/Users/tiago/Downloads/R_stats-Datathon-FGV")

shangai <- read.csv(file = "dado_shangai.csv", fill = FALSE)
hong_kong <- read.csv(file = "dado_hongkong.csv", fill = FALSE)
crypto <- read.csv(file = "dados_crypto.csv", fill = FALSE)


trans_ts <- function(data_prices){
  #transforma em time series
  stock_data <- xts(data_prices[,-1], order.by=as.Date(data_prices[,1], "%Y-%m-%d"))
  return(stock_data)
} 


get_return <- function(data) { 
  #pega o a variação percentual e preenche o primeiro valor com 1
  pct_change_data <- Delt(data)
  
  pct_change_data <- na.fill(pct_change_data, fill = 0)
  
}


dado_shangai <- trans_ts(shangai)
dado_hongkong <- trans_ts(hong_kong)
dado_crypto <- trans_ts(crypto)


merg_shangai_crypto <- merge(dado_shangai, dado_crypto, join="inner")
merg_hongkong_crypto <- merge(dado_hongkong, dado_crypto, join="inner")


#transformando em série mensal(levando em conta o último dia de cada mês)
transf_mes <- function(dados){
  dados[endpoints(dados,on='weeks')]
}


df_shangai_mes <- transf_mes(merg_shangai_crypto)
df_hongkong_mes <- transf_mes(merg_hongkong_crypto)


df_shangai_mes$dado_shangai <- get_return(df_shangai_mes$dado_shangai)
df_shangai_mes$dado_crypto <- get_return(df_shangai_mes$dado_crypto)
df_hongkong_mes$dado_hongkong <- get_return(df_hongkong_mes$dado_hongkong)
df_hongkong_mes$dado_crypto <- get_return(df_hongkong_mes$dado_crypto)


#calculando a correção
correlacao_hongkong <- cor(df_hongkong_mes)
print(correlacao_hongkong)
correlacao_shangai <- cor(df_shangai_mes)
print(correlacao_shangai)

#plotando os gráficos em questão
plot_hongkong_crypto <- df_hongkong_mes %>% ggplot(aes(x = dado_crypto, y = dado_hongkong)) +
  geom_point()

plot_shangai_crypto <- df_shangai_mes %>% ggplot(aes(x = dado_crypto, y = dado_shangai)) +
  geom_point()


#classificando crises de liquidez na bolsa
#dummy filter
dummy_filter<-function(time_series, turn_pct=TRUE){
  if(turn_pct){time_series<-Delt(time_series)}
  names(time_series)<-"PCT_change"
  SD<-roll_sd(time_series, nrow(time_series), min_obs = 12)
  time_series$dummy<-as.numeric(time_series<=-SD)
  return(time_series)
}

dados_shangai_per <- dummy_filter(df_shangai_mes$dado_shangai, FALSE)
dados_hongkong_per <- dummy_filter(df_hongkong_mes$dado_hongkong, FALSE)


#selecionando os períodos de crise
#devolve os escopos de interesse
escp <- function(time_series){
  per <- time_series[time_series$dummy == 1]
  return(per)
}

crise_shangai <- escp(dados_shangai_per)
crise_hongkong <- escp(dados_hongkong_per)

#correlação nos períodos selecionados
#dados1 deve ser as bolsas
crr_crise <- function(dados1,dados2){
  ind <- index(dados1)
  corr_crise <- cor(dados1,dados2[ind])
  return(corr_crise)
}

crise_correlacao_shangai <- crr_crise(crise_shangai$PCT_change, df_shangai_mes$dado_crypto)
crise_correlacao_hongkong <- crr_crise(crise_hongkong$PCT_change, df_hongkong_mes$dado_crypto)


#plotando hongkong em crise
df_hongkong_mes[index(crise_hongkong)] %>% ggplot(aes(x = dado_hongkong, y = dado_crypto)) +
  geom_point() 

df_shangai_mes[index(crise_shangai)] %>% ggplot(aes(x = dado_shangai, y = dado_crypto)) +
  geom_point()
