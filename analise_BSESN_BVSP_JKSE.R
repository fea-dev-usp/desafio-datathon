library(readr)
library(dplyr)
library(magrittr)
library(tidyr)
library(xts)
library(quantmod)
library(ggplot2)
library(roll)

#importando
dados_JKSE <- read.csv("GitHub/desafio-datathon/^JKSE.csv")
dados_BSESN <- read.csv("GitHub/desafio-datathon/^BSESN.csv")
dados_BVSP <- read.csv("GitHub/desafio-datathon/^BVSP.csv")
dados_bitcoin <- read.csv("GitHub/desafio-datathon/dados_crypto.csv")
dados_SP <- read.csv("GitHub/desafio-datathon/bench_^GSPC.csv")

#transformando em ts
transf_ts <- function(dados){
  dados <- xts(dados[,-1],order.by=as.Date(dados[,1], "%Y-%m-%d"))
  return(dados)
}
dados_JKSE <- transf_ts(dados_JKSE)
dados_BSESN <- transf_ts(dados_BSESN)
dados_BVSP <- transf_ts(dados_BVSP)
dados_bitcoin <- transf_ts(dados_bitcoin)

#considerando somente os períodos pertencentes a ambos os índices em comparação
#dados1 deve ser a bolsa 
merg <- function(dados1,dados2){
  inter <- merge(dados1,dados2,join='inner')
  return(inter)
}
dados_JKSE <- merg(dados_JKSE,dados_bitcoin)
dados_BSESN <- merg(dados_BSESN,dados_bitcoin)
dados_BVSP <- merg(dados_BVSP,dados_bitcoin)

#transformando em série mensal(levando em conta o último dia de cada mês)
transf_mes <- function(dados){
  dados[endpoints(dados,on='months')]
}
dados_JKSE <- transf_mes(dados_JKSE)
dados_BSESN <- transf_mes(dados_BSESN)
dados_BVSP <- transf_mes(dados_BVSP)
dados_bitcoin <- transf_mes(dados_bitcoin)

#classifica os períodos a serem considerados
dummy_filter<-function(time_series, turn_pct=TRUE){
  if(turn_pct){time_series<-Delt(time_series)}
  names(time_series)<-"PCT_change"
  SD<-roll_sd(time_series, nrow(time_series), min_obs = 12)
  time_series$dummy<-as.numeric(time_series<=-SD)
  return(time_series)
}
dados_JKSE_per <- dummy_filter(dados_JKSE$dados1)
dados_BSESN_per <- dummy_filter(dados_BSESN$dados1)
dados_BVSP_per <- dummy_filter(dados_BVSP$dados1)

#devolve os escopos de interesse
escp <- function(time_series){
  per <- time_series[time_series$dummy == 1]
  return(per)
}
crise_JKSE <- escp(dados_JKSE_per)
crise_BVSP <- escp(dados_BVSP_per)
crise_BSESN <- escp(dados_BSESN_per)

#correlação nos períodos selecionados
#dados1 deve ser as bolsas
crr_crise <- function(dados1,dados2){
  ind <- index(dados1)
  corr_crise <- cor(dados1,dados2[ind])
  return(corr_crise)
}
crr_crise(crise_JKSE$PCT_change,dados_JKSE$dados2)
crr_crise(crise_BVSP$PCT_change,dados_BVSP$dados2)
crr_crise(crise_BSESN$PCT_change,dados_BSESN$dados2)

#+ TENTATIVAS
crr_ret <- function(dados1,dados2){
  join <- merge(dados1,dados2,join='inner')
  join <- na.omit(join)
  crr <- cor(join)
  return(crr)
}
#correlação entre as variações mensais
crr_bit_JKSE <- crr_ret(ret_mes_JKSE,ret_mes_bitcoin)
crr_bit_BVSP <- crr_ret(ret_mes_BVSP,ret_mes_bitcoin)
crr_bit_BSESN <- crr_ret(ret_mes_BSESN,ret_mes_bitcoin)
