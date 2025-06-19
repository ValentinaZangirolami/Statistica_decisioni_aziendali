################################################################################
#                                Serie storiche                                #
#                               Modelli classici                               #
################################################################################
#                           Valentina Zangirolami                              #
#                       valentina.zangirolami@unimib.it                        #
################################################################################

# caricamento libraries

library(ggplot2)
library(fpp2)
library(foreign)

# caricamento dati 

coffee_sales <- read.csv("dataset/coffee_sales.csv", row.names = 1)
retail_sales <- read.csv("dataset/retail_sales.csv",  row.names = 1)

#riconoscimento del formato data

coffee_sales$Date <- as.Date(coffee_sales$Date)
retail_sales$Date <- as.Date(retail_sales$Date)

# Coffee Sales

#visualizzazione della serie storica

View(coffee_sales) #serie storica mensile

ggplot(coffee_sales, aes(x = Date, y = Sales)) +
  geom_line() +
  labs(title = "Serie mensile delle vendite di un bar",
       y = "Sales ($)",
       x = "Date") +
  theme_minimal()

# da questo grafico notiamo un trend crescente e la presenza di stagionalità
# le fluttuazioni stagionali non crescono con il trend quindi probabilmente un modello addittivo 
# può essere adeguato

# Creazione di un "oggetto" time series

coffee_ts <- ts(coffee_sales$Sales, frequency=12, start=c(2015,1))

#frequency = 12 (mensile), prima data disponibile 01/2015

#grafico stagionalità

ggseasonplot(coffee_ts, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Sales") + ggtitle("Seasonplot")

# proviamo a decomporre la serie con un modello addittivo

decomp <- decompose(coffee_ts, type="additive")
plot(decomp)

# vediamo se l'assunzione di normalità degli errori è ragionevole

additive_residuals <- decomp$x - (decomp$trend + decomp$seasonal)

#plot
par(mfrow=c(1,1))
qqnorm(additive_residuals, main="Q-Q Plot of Residuals")
qqline(additive_residuals, col="red")

shapiro.test(additive_residuals) # reject H0: gaussian errors

#Retail Sales

View(retail_sales) #serie storica mensile

ggplot(retail_sales, aes(x = Date, y = Sales)) +
  geom_line() +
  labs(title = "Serie mensile delle vendite di un negozio",
       y = "Sales ($)",
       x = "Date") +
  theme_minimal()

# da questo grafico notiamo un trend crescente e la presenza di stagionalità
# le fluttuazioni stagionali sembrano crescere con il trend quindi probabilmente un modello 
# moltiplicativo può essere più adeguato

# Creazione di un "oggetto" time series

retail_ts <- ts(retail_sales$Sales, frequency=12, start=c(2015,1))
#frequency = 12 (mensile), prima data disponibile 01/2015

#grafico stagionalità

ggseasonplot(retail_ts, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Sales") + ggtitle("Seasonplot")

# proviamo a decomporre la serie con un modello moltiplicativo

decomp <- decompose(retail_ts, type="multiplicative")
plot(decomp)

# vediamo se l'assunzione di normalità degli errori è ragionevole

mult_residuals <- log(decomp$x) - (log(decomp$trend) + log(decomp$seasonal))

#plot
par(mfrow=c(1,1))
qqnorm(mult_residuals, main="Q-Q Plot of Residuals")
qqline(mult_residuals, col="red")

shapiro.test(mult_residuals) # reject H0: gaussian errors

# e se usiamo un modello addittivo? 

decomp <- decompose(retail_ts, type="additive")
plot(decomp)
#vediamo un pattern nella parte random