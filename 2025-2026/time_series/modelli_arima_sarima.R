################################################################################
#                               Serie storiche                                 #
#                                ARIMA/SARIMA                                  #
#                                                                              #
################################################################################
#                           Valentina Zangirolami                              #
#                       valentina.zangirolami@unimib.it                        #
################################################################################


# -----------------------------------------------------------------------------
# 0. Setup
# -----------------------------------------------------------------------------

# Installare i pacchetti se necessario:
# install.packages(c("forecast", "tseries"))

library(forecast)
library(tseries)

read_course_csv <- function(file_name, ...) {
  candidate_paths <- c(file.path("dataset", file_name), file_name)
  existing_path <- candidate_paths[file.exists(candidate_paths)][1]
  if (is.na(existing_path)) {
    stop(
      paste0(
        "File non trovato: ", file_name,
        ". Mettere il CSV nella working directory oppure nella cartella dataset/."
      )
    )
  }
  read.csv(existing_path, ...)
}

# -----------------------------------------------------------------------------
# 1. Google returns: ARMA per serie circa stazionarie
# -----------------------------------------------------------------------------

google_returns <- read_course_csv("google_stock_returns.csv", row.names = 1)

# View(google_returns)

head(google_returns)
summary(google_returns)

# Se la colonna Date è numerica, origin specifica la data di partenza.
# Se è già in formato carattere ISO, as.Date() funziona direttamente.
if (is.numeric(google_returns$Date)) {
  google_returns$Date <- as.Date(google_returns$Date, origin = "2020-01-03")
} else {
  google_returns$Date <- as.Date(google_returns$Date)
}

ts.plot(google_returns$Returns,
        xlab = "time",
        ylab = "Returns",
        main = "Google stock returns")
abline(h = 0, lty = 2)


# I rendimenti finanziari spesso oscillano intorno a zero, senza trend evidente.
# Questo rende plausibile, come primo tentativo, l'uso di modelli ARMA.

# Test ADF: H0 = presenza di radice unitaria, cioè non stazionarietà.
adf.test(na.omit(google_returns$Returns))

# ACF e PACF aiutano a formulare ipotesi su p e q.
par(mfrow = c(1, 2))
acf(google_returns$Returns,
    lag.max = 20,
    main = "ACF of Google Returns",
    ylab = "Autocorrelation",
    ci = 0.95)
pacf(google_returns$Returns,
     lag.max = 20,
     main = "PACF of Google Returns",
     ylab = "Partial Autocorrelation")
par(mfrow = c(1, 1))

# - AR(p): la PACF tende a tagliarsi dopo p lag.
# - MA(q): la ACF tende a tagliarsi dopo q lag.
# - ARMA(p,q): entrambe tendono a decadere gradualmente.

# Primo tentativo: ARMA(1,1).
arma_11 <- arima(google_returns$Returns, order = c(1, 0, 1))
print(arma_11)
checkresiduals(arma_11)

# Secondo tentativo: ARMA(1,2).
arma_12 <- arima(google_returns$Returns, order = c(1, 0, 2))
print(arma_12)
checkresiduals(arma_12)

# Confronto AIC: più basso è meglio, a parità di obiettivo e dati.
AIC(arma_11, arma_12)

# Visualizzazione dei fitted values per il modello scelto.
# In questo esempio usiamo ARMA(1,2), ma discutere la scelta guardando AIC e residui.
arma_selected <- arma_12
arma_fit <- google_returns$Returns - residuals(arma_selected)

ts.plot(google_returns$Returns,
        main = "Google returns e fitted values ARMA",
        ylab = "Returns")
points(arma_fit, type = "l", col = 2, lty = 2)
legend("topright",
       legend = c("Osservato", "Fitted"),
       lty = c(1, 2),
       col = c(1, 2),
       bty = "n")

# Provare ARMA(2,1) e ARMA(2,2). Quale modello ha AIC più basso?
# I residui sembrano white noise?

# -----------------------------------------------------------------------------
# 2. AirPassengers: da serie non stazionaria a SARIMA
# -----------------------------------------------------------------------------

data("AirPassengers")
air_passenger <- AirPassengers

summary(air_passenger)
start(air_passenger)
end(air_passenger)
frequency(air_passenger) # serie mensile: frequency = 12

ts.plot(air_passenger,
        xlab = "Year",
        ylab = "Number of Passengers",
        main = "Monthly totals of international airline passengers, 1949-1960")
abline(reg = lm(air_passenger ~ time(air_passenger)))

# Qui vediamo trend crescente e stagionalità crescente nel tempo.
# Quindi la serie originale non è stazionaria.

# 2.1 Trasformazione logaritmica: stabilizza la variabilità.
log_AP <- log(air_passenger)
plot(log_AP, main = "Log-transformed AirPassengers")

# 2.2 Differenza ordinaria: riduce/rimuove il trend.
diff_log_AP <- diff(log_AP)
plot(diff_log_AP, main = "Log AirPassengers dopo differenza ordinaria")

# 2.3 Differenza stagionale: riduce/rimuove stagionalità annuale.
final_AP <- diff(diff_log_AP, lag = 12)
plot(final_AP, main = "Serie circa stazionaria dopo differenze")

# Test ADF sulla serie trasformata.
adf.test(na.omit(final_AP))

# ACF e PACF della serie trasformata.
par(mfrow = c(1, 2))
acf(final_AP, main = "ACF: AirPassengers trasformata")
pacf(final_AP, main = "PACF: AirPassengers trasformata")
par(mfrow = c(1, 1))

# - ACF compatibile con una componente MA.
# - PACF compatibile con una componente AR.
# La lettura di ACF/PACF non è meccanica: va sempre verificata con diagnostica.

# Modello sulla serie già trasformata: ARMA(1,2).
arma_AP <- arima(final_AP, order = c(1, 0, 2))
print(arma_AP)
checkresiduals(arma_AP)

# Modello SARIMA equivalente sulla serie logaritmica originale.
# order = c(p,d,q) e seasonal$order = c(P,D,Q).
sarima_model <- arima(log_AP,
                      order = c(1, 1, 2),
                      seasonal = list(
                        order = c(0, 1, 1),
                        period = 12))
print(sarima_model)
checkresiduals(sarima_model)

# auto.arima cerca automaticamente una combinazione ragionevole di ordini.
auto_model <- auto.arima(log_AP,
                         seasonal = TRUE,
                         stepwise = FALSE,
                         approximation = FALSE)
summary(auto_model)
checkresiduals(auto_model)

# Confronto AIC tra modello manuale e modello automatico.
AIC(sarima_model, auto_model)

# Previsione finale
ap_forecast <- forecast(auto_model, h = 24)
plot(ap_forecast,
     main = "Forecast AirPassengers, modello auto.arima",
     xlab = "Year",
     ylab = "log(Passengers)")


# MINI-ESERCIZIO FINALE:
# 1. Cambiare h nella previsione da 24 a 12 o 36.
# 2. Osservare come cambia l'incertezza delle previsioni.
# 3. Discutere perché gli intervalli si allargano andando avanti nel tempo.
