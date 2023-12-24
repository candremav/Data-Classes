#_____________________________________________________________________________________________
##################################### * CLEANING DATA ----------------------------------------
#_____________________________________________________________________________________________


rm(list=ls())
gc()


#_____________________________________________________________________________________________
##################################### * LIVRARIAS --------------------------------------------
#_____________________________________________________________________________________________

pacman::p_load(dplyr,readxl,openxlsx,tidyverse,tibble,Hmisc,ggplot2,corrplot,gridExtra)

#IMPORTAÇÃO DO BANCO DE DADOS

frauds=read.csv('D:/Acadêmico/Docência/FUCAPE/Soluções em Ciências de Dados/Dados1 Cartoes.csv')


#_____________________________________________________________________________________________
##################################### * DATA ANALYSIS ----------------------------------------
#_____________________________________________________________________________________________

#Variáveis:

#distance_from_home - A distância de casa onde a transação ocorreu.
#distance_from_last_transaction - A distância da última transação.
#ratio_to_median_purchase_price - Razão do preço de compra da transação para o preço médio de compra.
#repeat_retailer - Se a transação ocorreu com o mesmo varejista. [0,1]
#used_chip - Se a transação foi realizada através de chip (cartão de crédito). [0,1]
#used_pin_number - Se a transação foi realizada usando número PIN. [0,1]
#online_order - Se a transação é um pedido online. [0,1]
#fraud - Se a transação é fraudulenta. [0,1]

frauds
head(frauds)
tail(frauds)
dim(frauds)
colnames(frauds)
str(frauds)
class(frauds)
glimpse(frauds)
summary(frauds)
describe(frauds)
sapply(frauds[,c(4:8)],table)
cor(frauds)


corrplot(cor(frauds), method = "color", type = "full", tl.col = "black", tl.srt = 90,
         addCoef.col = "black", col = colorRampPalette(c("blue", "white", "red"))(200))

corrplot(cor(frauds), method = "color", type = "lower", tl.col = "black", tl.srt = 90,
         addCoef.col = "black", col = colorRampPalette(c("blue", "white", "red"))(50))



#_____________________________________________________________________________________________
##################################### * CHARTS -----------------------------------------------
#_____________________________________________________________________________________________


#HISTOGRAMS


hist(frauds$distance_from_home, main = "Distância de Casa", breaks=50)
hist(frauds$distance_from_last_transaction, main = "Distância da Última Transação", breaks=50)
hist(frauds$ratio_to_median_purchase_price, main = "Razão em Relação à Mediana do Preço de Compras", breaks=50)

ggplot(frauds, aes(x = distance_from_home)) +
  geom_histogram(bins = 50, color = "black", fill = "blue") +
  ggtitle("Distância de Casa")

ggplot(frauds, aes(x = distance_from_last_transaction)) +
  geom_histogram(bins = 50, color = "black", fill = "blue") +
  ggtitle("Distância da Última Transação")

ggplot(frauds, aes(x = ratio_to_median_purchase_price)) +
  geom_histogram(bins = 50, color = "black", fill = "blue") +
  ggtitle("Razão em Relação à Mediana do Preço de Compras")


#BOXPLOTS

boxplot(frauds$distance_from_home, main = "Distância de Casa")
boxplot(frauds$distance_from_last_transaction, main = "Distância da Última Transação")
boxplot(frauds$ratio_to_median_purchase_price, main = "Razão em Relação à Mediana do Preço de Compras")

ggplot(frauds, aes(y = distance_from_home)) +
  geom_boxplot(color = "black", fill = "blue") +
  ggtitle("Distância de Casa")

ggplot(frauds, aes(y = distance_from_last_transaction)) +
  geom_boxplot(color = "black", fill = "blue") +
  ggtitle("Distância da Última Transação")

ggplot(frauds, aes(y = ratio_to_median_purchase_price)) +
  geom_boxplot(color = "black", fill = "blue") +
  ggtitle("Razão em Relação à Mediana do Preço de Compras")

#OUTLIERS

quantis <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
quantile(frauds$distance_from_home, quantis)
quantile(frauds$distance_from_last_transaction, quantis)
quantile(frauds$ratio_to_median_purchase_price, quantis)


#ADJUSTED HISTOGRAMS

hist(frauds$distance_from_home, main = "Distância de Casa", breaks=1500,xlim = c(-2, 70))
hist(frauds$distance_from_last_transaction, main = "Distância da Última Transação", breaks=15000,
     xlim = c(-0.5, 10))
hist(frauds$ratio_to_median_purchase_price, main = "Razão em Relação à Mediana do Preço de Compras",
     breaks=500, xlim = c(-0.5, 10))


ggplot(frauds, aes(x = distance_from_home)) +
  geom_histogram(bins = 1500, color = "black", fill = "blue") +
  ggtitle("Distância de Casa") +
  coord_cartesian(xlim = c(-2, 70)) #ao contrário do xlim=c(inf,sup) não remove dados fora do intervalo, mas apenas ajusta a visualização

ggplot(frauds, aes(x = distance_from_last_transaction)) +
  geom_histogram(bins = 15000, color = "black", fill = "blue") +
  ggtitle("Distância da Última Transação") +
  coord_cartesian(xlim = c(-0.5, 10))  #ao contrário do xlim=c(inf,sup) não remove dados fora do intervalo, mas apenas ajusta a visualização

ggplot(frauds, aes(x = ratio_to_median_purchase_price)) +
  geom_histogram(bins = 500, color = "black", fill = "blue") +
  ggtitle("Razão em Relação à Mediana do Preço de Compras") +
  coord_cartesian(xlim = c(-0.5, 6))  #ao contrário do xlim=c(inf,sup) não remove dados fora do intervalo, mas apenas ajusta a visualização


#ADJUSTED BOXPLOTS


boxplot(frauds$distance_from_home, main = "Distância de Casa", ylim = c(-2, 70))
boxplot(frauds$distance_from_last_transaction, main = "Distância da Última Transação", ylim = c(-0.5, 10))
boxplot(frauds$ratio_to_median_purchase_price, main = "Razão em Relação à Mediana do Preço de Compras", ylim = c(-0.5, 10))

ggplot(frauds, aes(y = distance_from_home)) +
  geom_boxplot(color = "black", fill = "blue") +
  ggtitle("Distância de Casa") +
  coord_cartesian(ylim = c(-2, 70)) #ao contrário do xlim=c(inf,sup) não remove dados fora do intervalo, mas apenas ajusta a visualização

ggplot(frauds, aes(y = distance_from_last_transaction)) +
  geom_boxplot(color = "black", fill = "blue") +
  ggtitle("Distância da Última Transação") +
  coord_cartesian(ylim = c(-0.5, 10)) #ao contrário do xlim=c(inf,sup) não remove dados fora do intervalo, mas apenas ajusta a visualização

ggplot(frauds, aes(y = ratio_to_median_purchase_price)) +
  geom_boxplot(color = "black", fill = "blue") +
  ggtitle("Razão em Relação à Mediana do Preço de Compras") +
  coord_cartesian(ylim = c(-0.5, 6)) #ao contrário do xlim=c(inf,sup) não remove dados fora do intervalo, mas apenas ajusta a visualização


colunas=c('distance_from_home', 'distance_from_last_transaction', 'ratio_to_median_purchase_price')

# Loop para calcular e imprimir a porcentagem de outliers para cada coluna
for (coluna in colunas) {
  Q1 <- quantile(frauds[[coluna]], 0.25, na.rm = TRUE)
  Q3 <- quantile(frauds[[coluna]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lim_inf <- Q1 - 1.5 * IQR
  lim_sup <- Q3 + 1.5 * IQR
  
  # Contando outliers
  n_outliers <- sum(frauds[[coluna]] < lim_inf | frauds[[coluna]] > lim_sup, na.rm = TRUE)
  
  # Calculando a porcentagem de outliers
  porcentagem_outliers <- (n_outliers / nrow(frauds)) * 100
  
  # Imprimindo os resultados
  cat("Porcentagem de outliers em", coluna, ":", porcentagem_outliers, "%\n")
}

#CONTAGEM DE VALORES

ggplot(frauds, aes(x = as.factor(fraud), fill = as.factor(fraud))) +
  geom_bar() +
  scale_fill_manual(values = c('blue', 'red')) +
  ggtitle("Ocorrência de Fraudes") +
  scale_x_discrete(labels = c('NÃO', 'SIM')) +
  ylim(0, 1000000)+
  theme(legend.position = "none")
  

ggplot(frauds, aes(x = as.factor(online_order), fill = as.factor(online_order))) +
  geom_bar() +
  scale_fill_manual(values = c('blue', 'red')) +
  ggtitle("Compra Online") +
  scale_x_discrete(labels = c('NÃO', 'SIM')) +
  ylim(0, 1000000)+
  theme(legend.position = "none")


ggplot(frauds, aes(x = as.factor(used_pin_number), fill = as.factor(used_pin_number))) +
  geom_bar() +
  scale_fill_manual(values = c('blue', 'red')) +
  ggtitle("Utilização do PIN") +
  scale_x_discrete(labels = c('NÃO', 'SIM')) +
  ylim(0, 1000000)+
  theme(legend.position = "none")


ggplot(frauds, aes(x = as.factor(used_chip), fill = as.factor(used_chip))) +
  geom_bar() +
  scale_fill_manual(values = c('blue', 'red')) +
  ggtitle("Utilização do Chip") +
  scale_x_discrete(labels = c('NÃO', 'SIM')) +
  ylim(0, 1000000)+
  theme(legend.position = "none")


ggplot(frauds, aes(x = as.factor(repeat_retailer), fill = as.factor(repeat_retailer))) +
  geom_bar() +
  scale_fill_manual(values = c('blue', 'red')) +
  ggtitle("Compra no mesmo varejista") +
  scale_x_discrete(labels = c('NÃO', 'SIM')) +
  ylim(0, 1000000)+
  theme(legend.position = "none")


#AMOSTRA COM FRAUDES


ggplot(subset(frauds, fraud == 1), aes(x = as.factor(online_order), fill = as.factor(online_order))) +
  geom_bar() +
  scale_fill_manual(values = c('blue', 'red')) +
  ggtitle("Compra Online") +
  scale_x_discrete(labels = c('NÃO', 'SIM')) +
  ylim(0, 100000)+
  theme(legend.position = "none")


ggplot(subset(frauds, fraud == 1), aes(x = as.factor(used_pin_number), fill = as.factor(used_pin_number))) +
  geom_bar() +
  scale_fill_manual(values = c('blue', 'red')) +
  ggtitle("Utilização do PIN") +
  scale_x_discrete(labels = c('NÃO', 'SIM')) +
  ylim(0, 100000)+
  theme(legend.position = "none")


ggplot(subset(frauds, fraud == 1), aes(x = as.factor(used_chip), fill = as.factor(used_chip))) +
  geom_bar() +
  scale_fill_manual(values = c('blue', 'red')) +
  ggtitle("Utilização do Chip") +
  scale_x_discrete(labels = c('NÃO', 'SIM')) +
  ylim(0, 100000)+
  theme(legend.position = "none")


ggplot(subset(frauds, fraud == 1), aes(x = as.factor(repeat_retailer), fill = as.factor(repeat_retailer))) +
  geom_bar() +
  scale_fill_manual(values = c('blue', 'red')) +
  ggtitle("Compra no mesmo varejista") +
  scale_x_discrete(labels = c('NÃO', 'SIM')) +
  ylim(0, 100000)+
  theme(legend.position = "none")


#AMOSTRA SEM FRAUDES


ggplot(subset(frauds, fraud == 0), aes(x = as.factor(online_order), fill = as.factor(online_order))) +
  geom_bar() +
  scale_fill_manual(values = c('blue', 'red')) +
  ggtitle("Compra Online") +
  scale_x_discrete(labels = c('NÃO', 'SIM')) +
  ylim(0, 900000)+
  theme(legend.position = "none")


ggplot(subset(frauds, fraud == 0), aes(x = as.factor(used_pin_number), fill = as.factor(used_pin_number))) +
  geom_bar() +
  scale_fill_manual(values = c('blue', 'red')) +
  ggtitle("Utilização do PIN") +
  scale_x_discrete(labels = c('NÃO', 'SIM')) +
  ylim(0, 900000)+
  theme(legend.position = "none")


ggplot(subset(frauds, fraud == 0), aes(x = as.factor(used_chip), fill = as.factor(used_chip))) +
  geom_bar() +
  scale_fill_manual(values = c('blue', 'red')) +
  ggtitle("Utilização do Chip") +
  scale_x_discrete(labels = c('NÃO', 'SIM')) +
  ylim(0, 900000)+
  theme(legend.position = "none")


ggplot(subset(frauds, fraud == 0), aes(x = as.factor(repeat_retailer), fill = as.factor(repeat_retailer))) +
  geom_bar() +
  scale_fill_manual(values = c('blue', 'red')) +
  ggtitle("Compra no mesmo varejista") +
  scale_x_discrete(labels = c('NÃO', 'SIM')) +
  ylim(0, 900000)+
  theme(legend.position = "none")


#GRÁFICO DE BARRAS

frauds$fraud_label=ifelse(frauds$fraud == 1, "SIM", "NÃO")

ggplot(frauds, aes(x = as.factor(online_order), fill = as.factor(fraud_label))) +
  geom_bar() +
  scale_fill_manual(values = c('blue', 'red'), name='Fraude') +
  ggtitle("Compra Online") +
  ylim(0, 1000000) +
  scale_x_discrete(labels = c('NÃO', 'SIM'))

ggplot(frauds, aes(x = as.factor(used_pin_number), fill = as.factor(fraud_label))) +
  geom_bar() +
  scale_fill_manual(values = c('blue', 'red'), name='Fraude') +
  ggtitle("Utilização do PIN") +
  ylim(0, 1000000) +
  scale_x_discrete(labels = c('NÃO', 'SIM'))

ggplot(frauds, aes(x = as.factor(used_chip), fill = as.factor(fraud_label))) +
  geom_bar() +
  scale_fill_manual(values = c('blue', 'red'), name='Fraude') +
  ggtitle("Utilização do Chip") +
  ylim(0, 1000000) +
  scale_x_discrete(labels = c('NÃO', 'SIM'))

ggplot(frauds, aes(x = as.factor(repeat_retailer), fill = as.factor(fraud_label))) +
  geom_bar() +
  scale_fill_manual(values = c('blue', 'red'), name='Fraude') +
  ggtitle("Compra no mesmo varejista") +
  ylim(0, 1000000) +
  scale_x_discrete(labels = c('NÃO', 'SIM'))

#GRÁFICOS ADICIONAIS

ggplot(frauds, aes(x = distance_from_home)) +
  geom_density() +
  coord_cartesian(xlim = c(0, 60)) +
  ggtitle("Densidade Kernel de Distância de Casa")


ggplot(frauds, aes(x = distance_from_last_transaction)) +
  geom_histogram(bins = 15000, fill = "blue", color = "black") +  # Você pode ajustar o número de bins
  coord_cartesian(xlim = c(0, 10)) +
  ggtitle("Histograma da Distância da Última Transação")


ggplot(frauds, aes(x = ratio_to_median_purchase_price)) +
  stat_ecdf(geom = "step") +  # 'geom = "step"' cria o gráfico ECDF
  coord_cartesian(xlim = c(0, 4)) +
  ggtitle("ECDF do Preço de Compras em Relação à Mediana")

#ESTATÍSTICAS POR GRUPO

frauds %>%
  group_by(fraud) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))
  
frauds %>%
  group_by(fraud) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  t()
