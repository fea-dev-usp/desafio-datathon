#Importando bibliotecas que foram usadas no processamento dos dados
library(xts)
library(ggplot2)
library(tidyr)
library(roll)
library(quantmod)
#Definindo pasta de trabalho
setwd("C:/Users/tiago/Downloads/R_stats-Datathon-FGV")


#carregando as bases de dados que ir�o ser processadas
#temos o pre�o de fechamento do bitcoin
#a s�rie do bitcoin come�a no dia 16/09/2014
shangai <- read.csv(file = "dado_shangai.csv", fill = FALSE)
hong_kong <- read.csv(file = "dado_hongkong.csv", fill = FALSE)
crypto <- read.csv(file = "dados_crypto.csv", fill = FALSE)
us <- read.csv(file = "dado_S&P.csv", fill = FALSE)
india <- read.csv(file = "dado_india.csv", fill = FALSE)
indonesia <- read.csv(file = "dado_jacarta.csv", fill = FALSE)
brasil <- read.csv(file = "dado_brasil.csv", fill = FALSE)
mexico <- read.csv(file = "dado_mexico.csv", fill = FALSE)
russia <- read.csv(file = "dado_russia.csv", fill = FALSE)
turquia <- read.csv(file = "dado_turquia.csv", fill = FALSE)


#essa fun��o transforma o dataframe em uma timeseries
trans_ts <- function(data_prices){
  #transforma em time series
  stock_data <- xts(data_prices[,-1], order.by=as.Date(data_prices[,1], "%Y-%m-%d"))
  return(stock_data)
} 


#essa fun��o calcula o returno di�rio dos ativos sendo analisados
get_return <- function(data) { 
  #pega o a varia��o percentual e preenche o primeiro valor com 1
  pct_change_data <- Delt(data)
  
  pct_change_data <- na.fill(pct_change_data, fill = 0)
  
}


#transformando os dataframes em timeseries
dado_shangai <- trans_ts(shangai)
dado_hongkong <- trans_ts(hong_kong)
dado_crypto <- trans_ts(crypto)
dado_us <- trans_ts(us)
dado_india <- trans_ts(india)
dado_indonesia <- trans_ts(indonesia)
dado_brasil <- trans_ts(brasil)
dado_mexico <- trans_ts(mexico)
dado_russia <- trans_ts(russia)
dado_turquia <- trans_ts(turquia)


#unindo as timeseries em dois dataframes
#o argumento inner implica na intersec��o dos indexes
merg_shangai_crypto <- merge(dado_shangai, dado_crypto, join="inner")
merg_hongkong_crypto <- merge(dado_hongkong, dado_crypto, join="inner")
merg_india_crypto <- merge(dado_india, dado_crypto, join = "inner")
merg_indonesia_crypto <- merge(dado_indonesia, dado_crypto, join = "inner")
merg_brasil_crypto <- merge(dado_brasil, dado_crypto, join = "inner")
merg_mexico_crypto <- merge(dado_mexico, dado_crypto, join = "inner")
merg_russia_crypto <- merge(dado_russia, dado_crypto, join = "inner")
merg_turquia_crypto <- merge(dado_turquia, dado_crypto, join = "inner")


#essa fun��o retorna uma nova s�rie que cont�m apenas os resultados para cada semana
#o pre�o semanal � o pre�o de fechamento do �ltimo dia da semana
transf_sem <- function(dados){
  dados[endpoints(dados,on='weeks')]
}


#transformando em dados semanais
df_shangai_sem <- transf_sem(merg_shangai_crypto)
df_hongkong_sem <- transf_sem(merg_hongkong_crypto)
us_serie_sem <- transf_sem(dado_us)
df_india_sem <- transf_sem(merg_india_crypto)
df_indonesia_sem <- transf_sem(merg_indonesia_crypto)
df_brasil_sem <- transf_sem(merg_brasil_crypto)
df_mexico_sem <- transf_sem(merg_mexico_crypto)
df_russia_sem <- transf_sem(merg_russia_crypto)
df_turquia_sem <- transf_sem(merg_turquia_crypto)


#n�s estamos aplicando a fun��o get_return para encontrar os retornos semanais dos ativos
df_shangai_sem$dado_shangai <- get_return(df_shangai_sem$dado_shangai)
df_shangai_sem$dado_crypto <- get_return(df_shangai_sem$dado_crypto)
df_hongkong_sem$dado_hongkong <- get_return(df_hongkong_sem$dado_hongkong)
df_hongkong_sem$dado_crypto <- get_return(df_hongkong_sem$dado_crypto)
us_serie <- get_return(us_serie_sem)
df_india_sem$dado_india <- get_return(df_india_sem$dado_india)
df_india_sem$dado_crypto <- get_return(df_india_sem$dado_crypto)
df_indonesia_sem$dado_indonesia <- get_return(df_indonesia_sem$dado_indonesia)
df_indonesia_sem$dado_crypto <- get_return(df_indonesia_sem$dado_crypto)
df_brasil_sem$dado_brasil <- get_return(df_brasil_sem$dado_brasil)
df_brasil_sem$dado_crypto <- get_return(df_brasil_sem$dado_crypto)
df_mexico_sem$dado_mexico <- get_return(df_mexico_sem$dado_mexico)
df_mexico_sem$dado_crypto <- get_return(df_mexico_sem$dado_crypto)
df_russia_sem$dado_russia <- get_return(df_russia_sem$dado_russia)
df_russia_sem$dado_crypto <- get_return(df_russia_sem$dado_crypto)
df_turquia_sem$dado_turquia <- get_return(df_turquia_sem$dado_turquia)
df_turquia_sem$dado_crypto <- get_return(df_turquia_sem$dado_crypto)


#calculando a correla��o dos ativos e ent�o printando no console
#nesse momento estamos olhando para a correla��o da s�rie inteira
correlacao_hongkong <- cor(df_hongkong_sem)
correlacao_shangai <- cor(df_shangai_sem)
correlacao_india <- cor(df_india_sem)
correlacao_indonesia <- cor(df_indonesia_sem)
correlacao_brasil <- cor(df_brasil_sem)
correlacao_mexico <- cor(df_mexico_sem)
correlacao_russia <- cor(df_russia_sem)
correlacao_turquia <- cor(df_turquia_sem)


#Agora vamos plotar os gr�ficos das correla��es gerais, ou seja, sem selecionar especificamente o per�odo de crise
#como podemos ver pelos scatters, a correla��o entre ambos os ativos � muito baixa
plot_hongkong <- df_hongkong_sem %>% ggplot(aes(x = dado_hongkong, y = dado_crypto)) +
  geom_point() + ggtitle("Correla��o Hong Kong e Bitcoin") + labs(y = "BTC %", x = "Hong Kong %") +
  theme(panel.border = element_blank() ,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


plot_shangai <- df_shangai_sem %>% ggplot(aes(x = dado_shangai, y = dado_crypto)) +
  geom_point() + ggtitle("Correla��o Shangai e Bitcoin") + labs(y = "BTC %", x = "Shangai %") +
  theme(panel.border = element_blank() ,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot_brasil <- df_brasil_sem %>% ggplot(aes(x = dado_brasil, y = dado_crypto)) +
  geom_point() + ggtitle("Correla��o Ibovespa e Bitcoin") + labs(y = "BTC %", x = "Ibovespa %") +
  theme(panel.border = element_blank() ,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot_mexico <- df_mexico_sem %>% ggplot(aes(x = dado_mexico, y = dado_crypto)) +
  geom_point() + ggtitle("Correla��o Mexico e Bitcoin") + labs(y = "BTC %", x = "Mexico %") +
  theme(panel.border = element_blank() ,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot_indonesia <- df_indonesia_sem %>% ggplot(aes(x = dado_indonesia, y = dado_crypto)) +
  geom_point() + ggtitle("Correla��o Indonesia e Bitcoin") + labs(y = "BTC %", x = "Indonesia %") +
  theme(panel.border = element_blank() ,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot_russia <- df_russia_sem %>% ggplot(aes(x = dado_russia, y = dado_crypto)) +
  geom_point() + ggtitle("Correla��o Russia e Bitcoin") + labs(y = "BTC %", x = "Russia %") +
  theme(panel.border = element_blank() ,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot_turquia <- df_turquia_sem %>% ggplot(aes(x = dado_turquia, y = dado_crypto)) +
  geom_point() + ggtitle("Correla��o Turquia e Bitcoin") + labs(y = "BTC %", x = "Turquia %") +
  theme(panel.border = element_blank() ,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot_india <- df_india_sem %>% ggplot(aes(x = dado_india, y = dado_crypto)) +
  geom_point() + ggtitle("Correla��o India e Bitcoin") + labs(y = "BTC %", x = "India %") +
  theme(panel.border = element_blank() ,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


#dummy filter
#nesse filtro nos criamos uma vari�vel quando a varia��o do �ndice � maior do
#que o seu desvio padr�o
#nossa ideia � analisar as quedas acima dos desvio padr�o de cada �ndice
dummy_filter<-function(time_series, turn_pct=TRUE, turn_pct_US=TRUE){
  if(turn_pct){time_series <- Delt(time_series)}
  if(turn_pct_US){us_serie <- Delt(us_serie)}
  names(time_series) <- "PCT_change"
  names(us_serie) <- "PCT_change"
  obj <- merge(time_series[,1], us_serie[,1], join="inner")
  SD <- roll_sd(obj[,1], nrow(obj), min_obs = 12)
  SD_US <- roll_sd(obj[,2], nrow(obj), min_obs = 12)
  obj$dummy <- as.numeric(obj[,1]<=-SD)
  obj$dummy_US <- as.numeric(obj[,2]<=-SD_US)
  return(obj)
}


#aplicando a fun��o dummy_filter para cada um dos �ndices
dados_shangai_per <- dummy_filter(df_shangai_sem$dado_shangai, turn_pct = FALSE)
dados_hongkong_per <- dummy_filter(df_hongkong_sem$dado_hongkong, turn_pct = FALSE)
dados_india_per <- dummy_filter(df_india_sem$dado_india, turn_pct = FALSE)
dados_indonesia_per <- dummy_filter(df_indonesia_sem$dado_indonesia, turn_pct = FALSE)
dados_brasil_per <- dummy_filter(df_brasil_sem$dado_brasil, turn_pct = FALSE)
dados_mexico_per <- dummy_filter(df_mexico_sem$dado_mexico, turn_pct = FALSE)
dados_russia_per <- dummy_filter(df_russia_sem$dado_russia, turn_pct = FALSE)
dados_turquia_per <- dummy_filter(df_turquia_sem$dado_turquia, turn_pct = FALSE)


#selecionando os per�odos de crise
#devolve os escopos de interesse
escp <- function(time_series){
  per <- time_series[time_series$dummy == 1]
  return(per)
}


#estamos selecionando o nosso per�odo de interesse
#esse per�odo s�o os dias em que as bolsas tiveram uma queda maior que um desvio padr�o
# e al�m disso, o �ndice S&P n�o teve uma queda maior que um desvio padr�o
#nossa ideia � tentar captar qual efeito as quedas das bolsas desse pa�ses em momentos de crise
#tem no bitcoin
crise_shangai <- escp(dados_shangai_per)
crise_hongkong <- escp(dados_hongkong_per)
crise_india <- escp(dados_india_per)
crise_indonesia <- escp(dados_indonesia_per)
crise_brasil <- escp(dados_brasil_per)
crise_mexico <- escp(dados_mexico_per)
crise_russia <- escp(dados_russia_per)
crise_turquia <- escp(dados_turquia_per)


#correla��o nos per�odos selecionados
#dados1 deve ser as bolsas
crr_crise <- function(dados1,dados2){
  ind <- index(dados1)
  corr_crise <- cor(dados1,dados2[ind])
  return(corr_crise)
}


#estamos calculando a correl��o do bitcoin com as bolsas em momentos de queda vertiginosa
crise_correlacao_shangai <- crr_crise(crise_shangai$PCT_change, df_shangai_sem$dado_crypto)
crise_correlacao_hongkong <- crr_crise(crise_hongkong$PCT_change, df_hongkong_sem$dado_crypto)
crise_correlacao_india <- crr_crise(crise_india$PCT_change, df_india_sem$dado_crypto)
crise_correlacao_indonesia <- crr_crise(crise_indonesia$PCT_change, df_indonesia_sem$dado_crypto)
crise_correlacao_brasil <- crr_crise(crise_brasil$PCT_change, df_brasil_sem$dado_crypto)
crise_correlacao_mexico <- crr_crise(crise_mexico$PCT_change, df_mexico_sem$dado_crypto)
crise_correlacao_russia <- crr_crise(crise_russia$PCT_change, df_russia_sem$dado_crypto)
crise_correlacao_turquia <- crr_crise(crise_turquia$PCT_change, df_turquia_sem$dado_crypto)


#plotando os gr�ficos com os dias de crise nas bolsas
#comparando esses plots com os feitos anteriormente, n�o vemos nenhuma mudan�a significativa no 
#relacionamento das duas vari�veis
plot_hongkong_crise <- df_hongkong_sem[index(crise_hongkong)] %>% ggplot(aes(x = dado_hongkong, y = dado_crypto)) +
  geom_point() + ggtitle("Correla��o Hong Kong e Bitcoin") + labs(y = "BTC %", x = "Hong Kong %") +
  theme(panel.border = element_blank() ,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


plot_shangai_crise <- df_shangai_sem[index(crise_shangai)] %>% ggplot(aes(x = dado_shangai, y = dado_crypto)) +
  geom_point() + ggtitle("Correla��o Shangai e Bitcoin") + labs(y = "BTC %", x = "Shangai %") +
  theme(panel.border = element_blank() ,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot_brasil_crise <- df_brasil_sem[index(crise_brasil)] %>% ggplot(aes(x = dado_brasil, y = dado_crypto)) +
  geom_point() + ggtitle("Correla��o Ibovespa e Bitcoin") + labs(y = "BTC %", x = "Ibovespa %") +
  theme(panel.border = element_blank() ,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot_mexico_crise <- df_mexico_sem[index(crise_mexico)] %>% ggplot(aes(x = dado_mexico, y = dado_crypto)) +
  geom_point() + ggtitle("Correla��o Mexico e Bitcoin") + labs(y = "BTC %", x = "Mexico %") +
  theme(panel.border = element_blank() ,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot_indonesia_crise <- df_indonesia_sem[index(crise_indonesia)] %>% ggplot(aes(x = dado_indonesia, y = dado_crypto)) +
  geom_point() + ggtitle("Correla��o Indonesia e Bitcoin") + labs(y = "BTC %", x = "Indonesia %") +
  theme(panel.border = element_blank() ,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot_russia_crise <- df_russia_sem[index(crise_russia)] %>% ggplot(aes(x = dado_russia, y = dado_crypto)) +
  geom_point() + ggtitle("Correla��o Russia e Bitcoin") + labs(y = "BTC %", x = "Russia %") +
  theme(panel.border = element_blank() ,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot_turquia_crise <- df_turquia_sem[index(crise_turquia)] %>% ggplot(aes(x = dado_turquia, y = dado_crypto)) +
  geom_point() + ggtitle("Correla��o Turquia e Bitcoin") + labs(y = "BTC %", x = "Turquia %") +
  theme(panel.border = element_blank() ,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

plot_india_crise <- df_india_sem[index(crise_india)] %>% ggplot(aes(x = dado_india, y = dado_crypto)) +
  geom_point() + ggtitle("Correla��o India e Bitcoin") + labs(y = "BTC %", x = "India %") +
  theme(panel.border = element_blank() ,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
