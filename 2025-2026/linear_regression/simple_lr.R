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


rm(list=ls())

#### Simulating Data - Analysis of residuals
set.seed(123)
x <- sort(runif(50))

# Issues with Linearity assumption

y1 <- x^2+rnorm(50,sd=.05)
y2 <- -x^2+rnorm(50,sd=.05)
y3 <- (x-.5)^3+rnorm(50,sd=.01)

plot(x,residuals(lm(y1~x)),xlab="X",ylab="residuals")
plot(x,residuals(lm(y2~x)),xlab="X",ylab="residuals")
plot(x,residuals(lm(y3~x)),xlab="X",ylab="residuals")

# Diagnostic plot
par(mfrow=c(2, 2))
plot(lm(y1~x))

# Transformation of the independent variable
par(mfrow=c(2, 2))
plot(lm(y1~poly(x, 2)))

# Issues with Homoscedasticity assumption
y1 <- x+x*rnorm(50,sd=.03)
y2 <- x+(1-x)*rnorm(50,sd=.03)
y3 <- x+c(rnorm(15,sd=.1),rnorm(20,sd=.4),rnorm(15,sd=.02))
y4 <- x+c(rnorm(20,sd=.5),rnorm(15,sd=.1),rnorm(15,sd=.4))

plot(x,residuals(lm(y1~x)),xlab="X",ylab="residuals")
plot(x,residuals(lm(y2~x)),xlab="X",ylab="residuals")
plot(x,residuals(lm(y3~x)),xlab="X",ylab="residuals")
plot(x,residuals(lm(y4~x)),xlab="X",ylab="residuals")

# Issues with Gaussian assumption

y1 <- x+.05*rt(50,df=2)
y2 <- x+.03*(rchisq(50,1)-.5)
y3 <- x-.03*(rchisq(50,1)-.5)
y4 <- x+.07*rt(50,df=2)

plot(x,residuals(lm(y1~x)),xlab="X",ylab="residuals")
plot(x,residuals(lm(y2~x)),xlab="X",ylab="residuals")
plot(x,residuals(lm(y3~x)),xlab="X",ylab="residuals")
plot(x,residuals(lm(y4~x)),xlab="X",ylab="residuals")

# qqplot

qqnorm(residuals(lm(y1~x)), pch = 1, frame = FALSE)
qqline(residuals(lm(y1~x)), col = "steelblue", lwd = 2)

qqnorm(residuals(lm(y2~x)), pch = 1, frame = FALSE)
qqline(residuals(lm(y2~x)), col = "steelblue", lwd = 2)

# transformations of Y

qqnorm(residuals(lm(log(y2)~x)), pch = 1, frame = FALSE)
qqline(residuals(lm(log(y2)~x)), col = "steelblue", lwd = 2)

qqnorm(residuals(lm(sqrt(y2)~x)), pch = 1, frame = FALSE)
qqline(residuals(lm(sqrt(y2)~x)), col = "steelblue", lwd = 2)