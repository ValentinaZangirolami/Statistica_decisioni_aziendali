################################################################################
#                                Serie storiche                                #
#                               Modelli classici                               #
#                                                                              #
################################################################################
#                           Valentina Zangirolami                              #
#                       valentina.zangirolami@unimib.it                        #
################################################################################

# -----------------------------------------------------------------------------
# 0. Setup
# -----------------------------------------------------------------------------

# Installare i pacchetti se necessario:
# install.packages(c("ggplot2", "fpp2", "foreign"))

library(ggplot2)
library(fpp2)
library(foreign)

# Funzione di utilità per rendere il caricamento dati più robusto.
# Il file viene cercato prima in dataset/, poi nella working directory.
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
# 1. Caricamento dati
# -----------------------------------------------------------------------------

coffee_sales <- read_course_csv("coffee_sales.csv", row.names = 1)
retail_sales <- read_course_csv("retail_sales.csv", row.names = 1)

coffee_sales$Date <- as.Date(coffee_sales$Date)
retail_sales$Date <- as.Date(retail_sales$Date)

# View(coffee_sales)
# View(retail_sales)

head(coffee_sales)
summary(coffee_sales)
head(retail_sales)
summary(retail_sales)

# Domanda per la classe:
# Le due serie hanno la stessa scala? Il trend sembra simile? La stagionalità cambia ampiezza?

# -----------------------------------------------------------------------------
# 2. Coffee sales: serie con stagionalità circa additiva
# -----------------------------------------------------------------------------

ggplot(coffee_sales, aes(x = Date, y = Sales)) +
  geom_line() +
  labs(title = "Serie mensile delle vendite di un bar",
       subtitle = "Trend crescente + stagionalità con ampiezza circa costante",
       y = "Sales ($)",
       x = "Date") +
  theme_minimal()

# Se le oscillazioni stagionali hanno ampiezza abbastanza costante nel tempo,
# un modello additivo è spesso un primo candidato ragionevole:
#   dato osservato = trend + stagionalità + errore

coffee_ts <- ts(coffee_sales$Sales, frequency = 12, start = c(2015, 1))

# frequency = 12 perché la serie è mensile.
# start = c(2015, 1) perché la prima osservazione è gennaio 2015.

coffee_ts
frequency(coffee_ts)
start(coffee_ts)
end(coffee_ts)

ggseasonplot(coffee_ts, year.labels = TRUE, year.labels.left = TRUE) +
  ylab("Sales") +
  ggtitle("Coffee sales: seasonplot")

# In quali mesi le vendite sembrano sistematicamente più alte o più basse?

coffee_decomp_add <- decompose(coffee_ts, type = "additive")
plot(coffee_decomp_add)

# Diagnostica semplice dei residui della decomposizione additiva.
# Nota: decompose() produce NA agli estremi della componente trend.

coffee_additive_residuals <- na.omit(
  coffee_decomp_add$x - (coffee_decomp_add$trend + coffee_decomp_add$seasonal)
)

par(mfrow = c(1, 2))
plot(coffee_additive_residuals,
     main = "Coffee: residui additivi",
     ylab = "Residui",
     xlab = "Tempo")
abline(h = 0, lty = 2)
qqnorm(coffee_additive_residuals, main = "Coffee: Q-Q plot")
qqline(coffee_additive_residuals, col = "red")
par(mfrow = c(1, 1))

shapiro.test(coffee_additive_residuals)

# Interpretazione didattica:
# Un p-value piccolo nel test di Shapiro-Wilk suggerisce deviazioni dalla normalità.
# Non significa automaticamente che il modello sia inutile, ma invita a guardare
# attentamente outlier, asimmetrie e pattern residui.

# MINI-ESERCIZIO:
# Provare a cambiare type = "multiplicative" per coffee_ts.
# La componente random appare migliore o peggiore rispetto al modello additivo?

# -----------------------------------------------------------------------------
# 3. Retail sales: serie con stagionalità circa moltiplicativa
# -----------------------------------------------------------------------------

ggplot(retail_sales, aes(x = Date, y = Sales)) +
  geom_line() +
  labs(title = "Serie mensile delle vendite di un negozio",
       subtitle = "Trend crescente + oscillazioni stagionali che aumentano con il livello",
       y = "Sales ($)",
       x = "Date") +
  theme_minimal()

# Se le fluttuazioni stagionali crescono al crescere del livello della serie,
# un modello moltiplicativo è spesso più adeguato:
#   dato osservato = trend * stagionalità * errore

retail_ts <- ts(retail_sales$Sales, frequency = 12, start = c(2015, 1))

ggseasonplot(retail_ts, year.labels = TRUE, year.labels.left = TRUE) +
  ylab("Sales") +
  ggtitle("Retail sales: seasonplot")

retail_decomp_mult <- decompose(retail_ts, type = "multiplicative")
plot(retail_decomp_mult)

# Per il modello moltiplicativo è comodo guardare i residui su scala logaritmica:
# log(x) = log(trend) + log(stagionalità) + log(errore)
retail_mult_residuals <- na.omit(
  log(retail_decomp_mult$x) -
    (log(retail_decomp_mult$trend) + log(retail_decomp_mult$seasonal))
)

par(mfrow = c(1, 2))
plot(retail_mult_residuals,
     main = "Retail: residui moltiplicativi, scala log",
     ylab = "Residui log",
     xlab = "Tempo")
abline(h = 0, lty = 2)
qqnorm(retail_mult_residuals, main = "Retail: Q-Q plot")
qqline(retail_mult_residuals, col = "red")
par(mfrow = c(1, 1))

shapiro.test(retail_mult_residuals)

# Confronto: che cosa succede se forziamo un modello additivo?
retail_decomp_add <- decompose(retail_ts, type = "additive")
plot(retail_decomp_add)

# Nel modello additivo per retail sales, la parte random mostra spesso un pattern
# residuo più evidente. Questo è un segnale che il modello non sta catturando bene
# la relazione tra livello della serie e ampiezza della stagionalità.

# La decomposizione descrive trend e stagionalità, ma non è ancora un vero modello
# dinamico per autocorrelazione e previsione. Per questo introduciamo ARMA, ARIMA e SARIMA.
