################################################################################
#                          Linear regression models                            #
#                                                                              #
################################################################################
#                           Valentina Zangirolami                              #
#                       valentina.zangirolami@unimib.it                        #
################################################################################


# caricamento library

library(glmtoolbox)
library(car)
library(moderndive)
library(ggplot2)

# Cosa sono le library? Le library contengono dataset e implementazioni che possiamo
# utilizzare senza dover implementare i modelli o i grafici da zero.
# Quindi quando indichiamo "caricamento library" stiamo caricando tutti gli oggetti 
# in essa contenuti (tra cui implementazioni e dataset)
# Per capire cosa contiene (e quali implementazioni) una library possiamo cercare su google
# e consultare la documentazione di ogni libreria.

# In questo caso vogliamo usare i dati presenti nella libreria glmtoolbox
help(advertising, package=glmtoolbox)

# caricamento dati: quando usiamo dati presenti su R o in una libreria possiamo 
# richiamare il comando "data(nome_dataset)"

data(advertising)
head(advertising) #visualizziamo prime 6 obs del nostro dataset
# I dati contengono variabili relative ai budget di investimenti e le vendite 
#relative al prodotto su n mercati differenti

# prima analisi dei dati: visualizziamo le caratteristiche dei nostri dati
summary(advertising)

# numero mercati n=200
nrow(advertising)

# OBIETTIVO: Vogliamo produrre un marketing plan per il prossimo anno che possa generare
# delle buone vendite.

################################################################################
####################      SIMPLE LINEAR REGRESSION    ##########################

#Inizialmente, vogliamo considerare un'unica variabile TV advertising per studiare
# la relazione con Sales

# Visualizzazione grafica tra le due variabili

theme_set(theme_bw())  ## sets default white background theme for ggplot
ggplot(data = advertising, aes(x=TV, y=sales)) + geom_point(col="red", alpha = 0.5) + labs(x="TV advertising", y="Sales", title = "Plot Sales and TV advertising")

# Si nota una relazione lineare tra le due variabili, quindi il modello di regressione
# lineare potrebbe essere appropriato

# Per implementare la regressione lineare possiamo utilizzare il comando "lm"
# guarda la help page per maggiori informazioni
# help(lm)

lm_sales <- lm(sales ~ TV, data = advertising)

# per visualizzare i principali risultati della regressione lineare possiamo 
# richiamare il comando summary

summary(lm_sales)

#Primo output (residuals): distribuzione dei residui --> mediana vicina allo zero
#Secondo output (Coefficients): Estimate (estimated betas), Std.error dei beta
# tvalue: t-statistic del test di ipotesi H0: beta_i = 0
# p-value del test relativo all'ipotesi di cui sopra

#commenti: I pvalue sono al di sotto di ogni alpha
# --> si rifiuta l'ipotesi nulla H0

# Multiple R2: 0.6119 --> preferiamo R2 vicini a 1
# F-statistic: In questo caso corrisponde all'ipotesi H0: beta_1 = 0
# pvalue < qualsiasi alpha --> rifiuto H0

# verifica assunzioni modello lineare
par(mfrow=c(2,2))
plot(lm_sales)

# confidence intervals

confint(lm_sales)

# Esercizio: Provate ad analizzare la relazione tra Sales e radio

################################################################################
###################      MULTIPLE LINEAR REGRESSION    #########################


esg_data <- read.csv("esg_financial_dataset.csv", row.names = 1)
head(esg_data)

# Vorremmo studiare la relazione tra Market capitalization congiuntamente a diverse covariate
# (Industry, ESG_Overall, Revenue, GrowthRate, WaterUsage, CarbonEmission)

# check variables

summary(esg_data)

#visualize data

Scatter_Matrix <- GGally::ggpairs(esg_data[,-1], 
                                  title = "Scatter Plot Matrix for ESG data", 
                                  axisLabels = "show") 
Scatter_Matrix

# relazioni marginali risposta-covariata: 
# - market cap e revenue hanno una relazione lineare (vediamo anche correlazione = 0.831)
# - WaterUsage e Carbon Emissions hanno una correlazione di circa 0.35 con MarketCap
# - ESG_Overall e MarketCap sembrano non avere una relazione lineare
# - GrowthRate e MarketCap sembrano non avere una relazione lineare
# dobbiamo stare attenti alla correlazione anche tra covariate: dobbiamo evitare la multicollinearità

# plot industry

ggplot(esg_data, aes(x = Industry)) +
  geom_bar(aes(y = ..count..), fill = "pink") +
  labs(x = "Industry",
       y = "Frequency") +
  theme_minimal()

ggplot(esg_data, aes(x = Industry, y = MarketCap, fill = Industry)) + 
  geom_boxplot() +
  ylim(0, 200000) + #migliorare la visualizzazione: abbiamo ridimensionato il range di MarketCap
  labs(title = "Boxplot",
       x = "Industry",
       y = "MarketCap") +
  theme_minimal()

#multiple linear regression
mod <- lm(MarketCap~., data=esg_data)
summary(mod)

# WaterUsage, CarbonEmissions, Revenue, IndustryTechnology e IndustryFinance hanno coefficienti significativi
# R2=0.7226 --> modello abbastanza buono
# F-statistic pone sotto H0: tutti coefficienti (no intercetta)=0 
# il pvalue è al di sotto di ogni soglia alpha --> rifiutiamo H0

# verifica assunzioni modello lineare
par(mfrow=c(2,2))
plot(mod)

# plot in alto-sinistra: sembra ci sia un leggero pattern --> forse problemi con linearità
# plot in alto-destra: code pesanti
# plot in basso-sinistra: omoschedasticità non sembra propriamente rispettata
# plot in alto-destra: qualche problema con influential points (76, 3034)

#interaction

ggplot(esg_data, aes(x = GrowthRate, y = MarketCap, color = Industry)) +
  geom_point(size = 1, shape=3) +
  geom_smooth(method = "lm", se = FALSE) +  # Adds linear regression lines
  labs(
    x = "GrowthRate",
    y = "MarketCap",
    color = "Industry"
  ) + ylim(0, 35000) + 
  theme_minimal()

ggplot(esg_data, aes(x = Revenue, y = MarketCap, color = Industry)) +
  geom_point(size = 1, shape=3) +
  geom_smooth(method = "lm", se = FALSE) +  # Adds linear regression lines
  labs(
    x = "GrowthRate",
    y = "MarketCap",
    color = "Industry"
  ) + ylim(0, 35000) + 
  theme_minimal()

ggplot(esg_data, aes(x = WaterUsage, y = MarketCap, color = Industry)) +
  geom_point(size = 1, shape=3) +
  geom_smooth(method = "lm", se = FALSE) +  # Adds linear regression lines
  labs(
    x = "GrowthRate",
    y = "MarketCap",
    color = "Industry"
  ) + ylim(0, 35000) + 
  theme_minimal()

#dai plot sembra che tutti possano essere buoni candidati per interazioni
# proviamo ad aggiungere GrowthRate:Industry 

#add interaction
mod_int <- lm(MarketCap ~ . + GrowthRate:Industry, data = esg_data)
summary(mod_int)#interazioni non significative
#leggero incremento di R2
plot(mod_int)
# simile interpretazioni

#esercizio: provate a valutare le altre interazioni