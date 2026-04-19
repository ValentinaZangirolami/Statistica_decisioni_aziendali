################################################################################
#                               Serie storiche                                 #
#                                ARIMA/SARIMA                                  #
################################################################################
#                           Valentina Zangirolami                              #
#                       valentina.zangirolami@unimib.it                        #
################################################################################

#caricamento librerie

library(forecast)
library(tseries)

#caricamento dati

google_returns <- read.csv("dataset/google_stock_returns.csv", row.names = 1)

#visualizziamo dati
View(google_returns)
summary(google_returns)

#trasformiamo in formato data
google_returns$Date <- as.Date(google_returns$Date, origin= "2020-01-03")

ts.plot(google_returns$Returns, xlab="time", ylab="Returns", main="Google stock returns")

#stazionaria --> proviamo ad usare modelli ARMA

#visualizzazione autocorrelazione e autocorrelazione parziale

par(mfrow = c(1, 2))  # Set up 1 row, 2 columns for plots

# ACF plot (Autocorrelation Function)
acf(google_returns$Returns, 
    lag.max = 20,       # Show up to 20 lags
    main = "ACF of Google Returns",
    ylab = "Autocorrelation",
    ci = 0.95)          # 95% confidence intervals

# PACF plot (Partial Autocorrelation Function)
pacf(google_returns$Returns, 
     lag.max = 20,
     main = "PACF of Google Returns",
     ylab = "Partial Autocorrelation")

# possiamo provare inizialmente con un ARMA(1,1)

# ARMA : modelli per serie storiche stazionarie

arma_mod <- arima(google_returns$Returns, order = c(1,0,1))
print(arma_mod)

# check se residui sono white noise

checkresiduals(arma_mod)

# proviamo con ARMA(1,2)
arma_mod <- arima(google_returns$Returns, order = c(1,0,2))
print(arma_mod)
#migliore AIC
checkresiduals(arma_mod)

# vediamo grafico con fitted values

arma_fit <- google_returns$Returns - residuals(arma_mod) 

par(mfrow=c(1,1))
ts.plot(google_returns$Returns)
points(arma_fit, type = "l", col = 2, lty = 2)

#dataset airpassengers

data("AirPassengers")
air_passenger <- AirPassengers
summary(air_passenger)

start(air_passenger)
end(air_passenger)

frequency(air_passenger) #serie mensile

#plot time series

ts.plot(air_passenger, xlab="Year", ylab="Number of Passengers", main="Monthly totals of international airline passengers, 1949-1960")

abline(reg=lm(air_passenger~time(air_passenger)))

# trend cresce nel tempo
# stagionalità cresce nel tempo

# cerchiamo di rendere la serie storica stazionaria

# traformazione log per stabilizzare variabilità
log_AP <- log(air_passenger)
plot(log_AP, main="Log-Transformed AirPassengers")

# rimoviamo trend
diff_log_AP <- diff(log_AP)
plot(diff_log_AP, main="Log-Transformed AirPassengers w/o trend")

#rimoviamo stagionalità

final_AP <- diff(diff_log_AP, lag=12)
plot(final_AP, main="TS stationary")

# verifichiamo con test
adf.test(na.omit(final_AP)) # rifiuto H0 --> la serie è stazionaria

# analizziamo la correlazione

acf(final_AP, main = "ACF") #sembra suggerire un MA(2)
pacf(final_AP, main = "PACF") #sembra suggerire un AR(1)

# ARIMA : modelli per serie storiche non stazionarie

arma_mod <- arima(final_AP, order=c(1,0,2))
print(arma_mod)
checkresiduals(arma_mod)

# invece di detrendizzare la serie e destagionalizzarla, potevamo applicare
sarima_model <- arima(log_AP, 
                      order=c(1,1,2),          # Non-seasonal (p,d,q)
                      seasonal=list(
                        order=c(0,1,1),        # Seasonal (P,D,Q)
                        period=12))   
print(sarima_model) # AIC peggiore
checkresiduals(sarima_model) 

# auto sarima

auto_model <- auto.arima(log_AP, 
                         seasonal=TRUE,
                         stepwise=FALSE,
                         approximation=FALSE)
summary(auto_model)
checkresiduals(auto_model)
