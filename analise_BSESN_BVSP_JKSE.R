library(readr)
library(dplyr)
library(magrittr)
library(tidyr)
library(xts)
library(quantmod)

#importando
dados_JKSE <- read.csv("GitHub/desafio-datathon/^JKSE.csv")
dados_BSESN <- read.csv("GitHub/desafio-datathon/^BSESN.csv")
dados_BVSP <- read.csv("GitHub/desafio-datathon/^BVSP.csv")
dados_bitcoin <- read.csv("GitHub/desafio-datathon/dados_crypto.csv")

#transformando em ts
transf_ts <- function(dados){
  dados <- xts(dados[,-1],order.by=as.Date(dados[,1], "%Y-%m-%d"))
  return(dados)
}
dados_JKSE <- transf_ts(dados_JKSE)
dados_BSESN <- transf_ts(dados_BSESN)
dados_BVSP <- transf_ts(dados_BVSP)
dados_bitcoin <- transf_ts(dados_bitcoin)

#levando em conta o último dia de cada mês
transf_mes <- function(dados){
  dados[endpoints(dados,on='months')]
}
dados_JKSE <- transf_mes(dados_JKSE)
dados_BSESN <- transf_mes(dados_BSESN)
dados_BVSP <- transf_mes(dados_BVSP)
dados_bitcoin <- transf_mes(dados_bitcoin)

#calculando a variação percentual mensal
pc_change <- function(dados){
  ret <- Delt(dados)
  return(ret)
}
ret_mes_JKSE <- pc_change(dados_JKSE)
ret_mes_BSESN <- pc_change(dados_BSESN)
ret_mes_BVSP <- pc_change(dados_BVSP)
ret_mes_bitcoin <- pc_change(dados_bitcoin)

#correlação
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



