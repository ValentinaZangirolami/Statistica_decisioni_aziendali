# =========================================================
# PROBABILITY IN R
# =========================================================

# In R le principali funzioni probabilistiche hanno la forma:
# d = density / probability mass function
# p = cumulative distribution function
# q = quantile function
# r = random generation

# Esempi:
# norm  -> distribuzione normale
# binom -> distribuzione binomiale
# pois  -> distribuzione di Poisson
# t     -> t di Student
# chisq -> chi-quadrato
# f     -> Fisher

# =========================================================
# 1. DISTRIBUZIONE NORMALE
# =========================================================

# P(X <= 0), con X ~ N(0,1)
pnorm(0, mean = 0, sd = 1)

# Genero 5 osservazioni da una N(0,1)
set.seed(123)
rnorm(5, mean = 0, sd = 1)

# Densità della normale standard nel punto x = 0
dnorm(0, mean = 0, sd = 1)

# Quantile al 97.5%
qnorm(0.975, mean = 0, sd = 1)

# Esempio: P(X < -0.03) con X ~ N(0.001, 0.02^2)
pnorm(-0.03, mean = 0.001, sd = 0.02)

# =========================================================
# 2. DISTRIBUZIONE BINOMIALE
# =========================================================

# X = numero di successi in n prove indipendenti
# con probabilità di successo p

# Esempio delle slide:
# un trader ha probabilità di successo p = 0.6
# in n = 10 operazioni
# probabilità di ottenere esattamente 7 successi

dbinom(7, size = 10, prob = 0.6)

# Probabilità di ottenere ALMENO 7 successi
1 - pbinom(6, size = 10, prob = 0.6)

# Probabilità di ottenere AL PIÙ 7 successi
pbinom(7, size = 10, prob = 0.6)

# Simulazione di 10 osservazioni dalla binomiale
set.seed(123)
rbinom(10, size = 10, prob = 0.6)

# =========================================================
# 3. DISTRIBUZIONE DI POISSON
# =========================================================

# X = numero di eventi in un intervallo
# lambda = numero medio di eventi nell'intervallo

# Esempio: media di 2 default al giorno
# probabilità di osservare esattamente 3 default

dpois(3, lambda = 2)

# Probabilità di osservare al più 3 default
ppois(3, lambda = 2)

# Probabilità di osservare almeno 1 evento raro con lambda = 0.00002
1 - dpois(0, lambda = 0.00002)

# Simulazione
set.seed(123)
rpois(10, lambda = 2)

# =========================================================
# 5. SIMULAZIONE DI ESPERIMENTI CASUALI
# =========================================================

# Lancio di una moneta
set.seed(123)
lanci_moneta <- sample(c("Testa", "Croce"), size = 20, replace = TRUE)
lanci_moneta

# Frequenza relativa di Testa
mean(lanci_moneta == "Testa")

# Lancio di un dado
set.seed(123)
lanci_dado <- sample(1:6, size = 20, replace = TRUE)
lanci_dado

# Frequenze relative
table(lanci_dado) / length(lanci_dado)

# =========================================================
# 6. PROBABILITA' CONDIZIONATA
# =========================================================

# Lanciamo una moneta due volte
spazio <- c("TT", "TC", "CT", "CC")
spazio

# A = "al primo lancio esce Testa"
A <- c("TT", "TC")

# B = "in due lanci esce una sola Testa"
B <- c("TC", "CT")

# Intersezione
intersezione <- intersect(A, B)
intersezione

# P(A | B) = P(A ∩ B) / P(B)
P_A_inter_B <- length(intersezione) / length(spazio)
P_B <- length(B) / length(spazio)
P_A_cond_B <- P_A_inter_B / P_B
P_A_cond_B

# =========================================================
# 9. MONTY HALL
# =========================================================

monty_hall <- function(change = TRUE) {
  prize <- sample(1:3, 1)
  choice <- sample(1:3, 1)
  
  # il conduttore apre una porta diversa da scelta e premio
  possible_open <- setdiff(1:3, c(choice, prize))
  opened <- sample(possible_open, 1)
  
  if (change) {
    final_choice <- setdiff(1:3, c(choice, opened))
  } else {
    final_choice <- choice
  }
  
  final_choice == prize
}

set.seed(123)
mean(replicate(10000, monty_hall(change = FALSE)))  # resto
mean(replicate(10000, monty_hall(change = TRUE)))   # cambio

