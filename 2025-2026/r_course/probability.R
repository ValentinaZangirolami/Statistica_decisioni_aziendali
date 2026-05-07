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

#generazione dati

set.seed(123) 
rnorm(5, mean = 0, sd = 1)

# Densità della normale standard nel punto x = 0
dnorm(0, mean = 0, sd = 1)

# Quantile al 97.5%
qnorm(0.975, mean = 0, sd = 1)

# =========================================================
# ESEMPIO ECONOMICO: RENDIMENTO GIORNALIERO DI UN TITOLO
# =========================================================

# Immaginiamo di osservare il rendimento giornaliero di un titolo finanziario.
# Prima della fine della giornata non sappiamo quale sarà il rendimento:
# potrebbe essere positivo, negativo oppure vicino a zero.
#
# Quindi possiamo definire una variabile casuale:
# X = rendimento giornaliero del titolo

# Assumiamo che il rendimento giornaliero segua una distribuzione normale:
# X ~ N(0.001, 0.02^2)
#
# Interpretazione:
# - mean = 0.001 significa rendimento medio giornaliero pari a 0.1%
# - sd = 0.02 significa deviazione standard pari al 2%
#
# Questa è un'ipotesi di modello: stiamo assumendo che la normale
# sia una buona approssimazione del comportamento del rendimento.

# =========================================================
# CALCOLO DI UNA PROBABILITÀ TEORICA
# =========================================================

# Domanda:
# Qual è la probabilità che il titolo perda più del 3% in un giorno?
#
# Perdere più del 3% significa avere un rendimento minore di -0.03.
# Quindi vogliamo calcolare:
# P(X < -0.03)

pnorm(-0.03, mean = 0.001, sd = 0.02)

# Spiegazione:
# pnorm() calcola una probabilità cumulata, cioè l'area sotto la curva
# normale a sinistra del valore -0.03.
#
# In questo caso il risultato rappresenta la probabilità teorica,
# secondo il modello normale assunto, di osservare una perdita
# superiore al 3% in un giorno.

# Genero molte osservazioni
x <- rnorm(10000, mean = 0.001, sd = 0.02)

# Prime osservazioni simulate
head(x)
x

# Istogramma dei valori simulati
hist(
  x,
  breaks = 40,
  probability = TRUE,
  main = "Simulazione da una distribuzione Normale standard",
  xlab = "Valori simulati",
  ylab = "Densità"
)

# Aggiungo la curva teorica della N(0,1)
curve(
  dnorm(x, mean = 0.001, sd = 0.02),
  add = TRUE,
  lwd = 2
)

mean(x)      # media empirica
var(x)       # varianza empirica
sd(x)        # deviazione standard empirica
mean(x< 0)   # 


# =========================================================
# 2. DISTRIBUZIONE BINOMIALE
# =========================================================

# X = numero di successi in n prove indipendenti
# con probabilità di successo p

# Esempio:
# Un trader fa 10 operazioni. Ogni operazione può avere successo oppure no. 
#Supponiamo che la probabilità di successo di ogni operazione sia p=0.6. 
# Definiamo X come il numero di operazioni di successo su 10.

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
# DISTRIBUZIONE DI POISSON
# =========================================================

# La distribuzione di Poisson si usa per modellare
# il numero di eventi che accadono in un intervallo.
#
# Esempi:
# - numero di default in un giorno
# - numero di sinistri assicurativi in un mese
# - numero di richieste di prestito in una settimana
# - numero di transazioni sospette in un intervallo
#
# Definiamo:
# X = numero di eventi osservati nell'intervallo
#
# Il parametro lambda rappresenta il numero medio di eventi
# nell'intervallo considerato.

# =========================================================
# ESEMPIO: DEFAULT GIORNALIERI
# =========================================================

# Supponiamo che in media si osservino 2 default al giorno.
#
# Allora possiamo modellare:
# X = numero di default in un giorno
#
# come:
# X ~ Poisson(lambda = 2)

lambda <- 2

# =========================================================
# 1. PROBABILITÀ DI OSSERVARE ESATTAMENTE 3 DEFAULT
# =========================================================

# Domanda:
# qual è la probabilità di osservare esattamente 3 default
# in un giorno?
#
# In simboli:
# P(X = 3)

prob_esattamente_3 <- dpois(3, lambda = lambda)
prob_esattamente_3

# Spiegazione:
# dpois() calcola una probabilità puntuale.
# Qui calcola la probabilità di osservare esattamente 3 eventi
# quando il numero medio di eventi è 2.

# =========================================================
# 2. PROBABILITÀ DI OSSERVARE AL PIÙ 3 DEFAULT
# =========================================================

# Domanda:
# qual è la probabilità di osservare al massimo 3 default?
#
# Al più 3 significa:
# X <= 3
#
# Quindi include:
# 0, 1, 2, 3

prob_al_piu_3 <- ppois(3, lambda = lambda)
prob_al_piu_3

# =========================================================
# 3. PROBABILITÀ DI OSSERVARE ALMENO 1 EVENTO RARO
# =========================================================

# Ora consideriamo un evento molto raro.
# Supponiamo che il numero medio di eventi nell'intervallo sia:
# lambda = 0.00002

lambda_raro <- 0.00002

# Domanda:
# qual è la probabilità di osservare almeno un evento?
#
# Almeno 1 significa:
# X >= 1
#
# Usiamo il complemento:
# P(X >= 1) = 1 - P(X = 0)

prob_almeno_1_raro <- 1 - dpois(0, lambda = lambda_raro)
prob_almeno_1_raro

# Spiegazione:
# Invece di sommare P(X = 1), P(X = 2), P(X = 3), ...
# facciamo 1 meno la probabilità di osservare zero eventi.
#
# Questo è molto utile quando vogliamo calcolare la probabilità
# che un evento raro accada almeno una volta.

# =========================================================
# 4. SIMULAZIONE DALLA POISSON
# =========================================================

# Ora generiamo 10 osservazioni da una Poisson con lambda = 2.
#
# Possiamo interpretarle come 10 giorni simulati.
# In ogni giorno osserviamo un possibile numero di default.

set.seed(123)
default_10_giorni <- rpois(10, lambda = lambda)
default_10_giorni

# Spiegazione:
# Ogni valore generato rappresenta il numero di default
# osservato in un giorno simulato.
#
# rpois():
# genera possibili dati coerenti con il modello di Poisson scelto.

# =========================================================
# 5. SIMULAZIONE PIÙ GRANDE
# =========================================================

# Con 10 giorni simulati vediamo solo pochi possibili risultati.
# Per osservare meglio il comportamento della distribuzione,
# simuliamo molti più giorni.

set.seed(123)
default_simulati <- rpois(10000, lambda = lambda)

# Prime osservazioni simulate
head(default_simulati)

# Media empirica dei default simulati
mean(default_simulati)

# Varianza empirica dei default simulati
var(default_simulati)

# =========================================================
# 7. PLOT DELLA DISTRIBUZIONE DI POISSON SIMULATA
# =========================================================

barplot(
  freq_empiriche,
  names.arg = valori,
  main = "Distribuzione di Poisson simulata",
  xlab = "Numero di default in un giorno",
  ylab = "Frequenza relativa"
)

# Aggiungiamo i punti delle probabilità teoriche
points(
  x = seq_along(valori),
  y = prob_teoriche,
  pch = 16
)

# Aggiungiamo una linea per collegare le probabilità teoriche
lines(
  x = seq_along(valori),
  y = prob_teoriche,
  lwd = 2
)

# =========================================================
# 5. SIMULAZIONE DI ESPERIMENTI CASUALI
# =========================================================

# Lancio di una moneta
set.seed(123)
lanci_moneta <- sample(c("Testa", "Croce"), size = 20, replace = TRUE)
lanci_moneta

# Frequenza relativa di Testa nei dati simulati
mean(lanci_moneta == "Testa")


# =========================================================
# LANCIO DI UN DADO EQUO
# =========================================================

# Gli esiti possibili sono:
# 1, 2, 3, 4, 5, 6.
#
# Se il dado è equo:
# ogni faccia ha probabilità teorica 1/6.
#
# Quindi:
# P(1) = P(2) = ... = P(6) = 1/6

set.seed(123)
lanci_dado <- sample(1:6, size = 50, replace = TRUE)
lanci_dado

# Frequenze relative
table(lanci_dado) / length(lanci_dado)

# Spiegazione:
# table(lanci_dado) conta quante volte compare ogni faccia.
# Dividendo per il numero totale di lanci otteniamo le frequenze relative.
#
# La probabilità teorica di ogni faccia è 1/6.
# Le frequenze empiriche possono essere diverse da 1/6,
# soprattutto quando il numero di lanci è piccolo.

# =========================================================
# 6. PROBABILITA' CONDIZIONATA
# =========================================================

# La probabilità condizionata risponde alla domanda:
# qual è la probabilità che accada A, sapendo che B è già accaduto?
#
# In simboli:
# P(A | B)
#
# Si legge:
# probabilità di A dato B.
#
# La formula è:
# P(A | B) = P(A ∩ B) / P(B)
#
# dove:
# A ∩ B indica l'intersezione tra A e B,
# cioè gli esiti in cui accadono sia A sia B.

# =========================================================
# ESEMPIO: DUE LANCI DI UNA MONETA
# =========================================================

# Lanciamo una moneta due volte.
#
# Indichiamo:
# T = Testa
# C = Croce
#
# Lo spazio campionario contiene tutti gli esiti possibili
# dei due lanci.

spazio <- c("TT", "TC", "CT", "CC")
spazio

# Interpretazione:
# TT = Testa al primo lancio, Testa al secondo lancio
# TC = Testa al primo lancio, Croce al secondo lancio
# CT = Croce al primo lancio, Testa al secondo lancio
# CC = Croce al primo lancio, Croce al secondo lancio
#
# Se la moneta è equa, tutti questi esiti hanno la stessa probabilità:
# 1/4.

# =========================================================
# DEFINIZIONE DELL'EVENTO A
# =========================================================

# A = "al primo lancio esce Testa"
#
# Gli esiti compatibili con A sono:
# TT e TC

A <- c("TT", "TC")
A

# =========================================================
# DEFINIZIONE DELL'EVENTO B
# =========================================================

# B = "in due lanci esce una sola Testa"
#
# Gli esiti compatibili con B sono:
# TC e CT

B <- c("TC", "CT")
B

# =========================================================
# INTERSEZIONE TRA A E B
# =========================================================

# L'intersezione A ∩ B contiene gli esiti che appartengono
# sia ad A sia a B.
#
# In altre parole:
# vogliamo gli esiti in cui:
# - il primo lancio è Testa
# - in totale esce una sola Testa

intersezione <- intersect(A, B)
intersezione

# In questo caso l'unico esito comune è TC.
#
# TC significa:
# Testa al primo lancio e Croce al secondo.
#
# Quindi:
# A ∩ B = {TC}

# =========================================================
# CALCOLO DELLE PROBABILITÀ
# =========================================================

# Probabilità dell'intersezione:
# P(A ∩ B)
#
# C'è 1 esito favorevole su 4 esiti possibili.

P_A_inter_B <- length(intersezione) / length(spazio)
P_A_inter_B

# Probabilità di B:
# P(B)
#
# B contiene 2 esiti favorevoli su 4:
# TC e CT.

P_B <- length(B) / length(spazio)
P_B

# Probabilità condizionata:
# P(A | B) = P(A ∩ B) / P(B)

P_A_cond_B <- P_A_inter_B / P_B
P_A_cond_B

# =========================================================
# INTERPRETAZIONE
# =========================================================

# Il risultato è 0.5.
#
# Questo significa che:
# sapendo che nei due lanci è uscita una sola Testa,
# la probabilità che la Testa sia uscita al primo lancio è 0.5.
#
# Perché?
#
# Sapere che è uscita una sola Testa restringe lo spazio campionario
# ai soli esiti:
# TC e CT.
#
# Tra questi due esiti:
# - in TC il primo lancio è Testa
# - in CT il primo lancio è Croce
#
# Quindi, tra gli esiti compatibili con B,
# solo 1 su 2 è compatibile anche con A.
#
# Perciò:
# P(A | B) = 1/2.

# =========================================================
# 9. MONTY HALL
# =========================================================

# Il problema di Monty Hall è un classico esempio di probabilità condizionata.
#
# Situazione:
# - ci sono 3 porte
# - dietro una porta c'è un premio
# - dietro le altre due porte non c'è nulla
#
# Il concorrente sceglie inizialmente una porta.
#
# Poi il conduttore, che sa dove si trova il premio,
# apre una delle porte non scelte dal concorrente,
# mostrando sempre una porta senza premio.
#
# A questo punto il concorrente può:
# - restare con la scelta iniziale
# - cambiare e scegliere l'altra porta rimasta chiusa
#
# Domanda:
# conviene restare o cambiare?

# =========================================================
# FUNZIONE CHE SIMULA UNA PARTITA DI MONTY HALL
# =========================================================

monty_hall <- function(change = TRUE) {
  
  # Porta con il premio
  prize <- sample(1:3, 1)
  
  # Scelta iniziale del concorrente
  choice <- sample(1:3, 1)
  
  # Porte che il conduttore può aprire:
  # non deve aprire né la porta scelta né quella con il premio
  possible_open <- setdiff(1:3, c(choice, prize))
  
  # ATTENZIONE:
  # non usiamo sample(possible_open, 1),
  # perché se possible_open contiene un solo valore R può comportarsi male.
  #
  # Usiamo invece un indice.
  opened <- possible_open[sample.int(length(possible_open), 1)]
  
  if (change) {
    
    # Se il concorrente cambia, sceglie l'unica porta rimasta chiusa
    # diversa dalla scelta iniziale e dalla porta aperta
    final_choice <- setdiff(1:3, c(choice, opened))
    
  } else {
    
    # Se non cambia, resta con la scelta iniziale
    final_choice <- choice
  }
  
  # TRUE se vince, FALSE se perde
  final_choice == prize
}

# =========================================================
# SIMULAZIONE DI MOLTE PARTITE
# =========================================================

# Usiamo set.seed() per rendere i risultati replicabili.
# In questo modo, se rieseguiamo il codice, otteniamo gli stessi risultati.

set.seed(123)

# Simuliamo 10000 partite in cui il concorrente NON cambia porta.
#
# replicate(10000, monty_hall(change = FALSE)) ripete 10000 volte
# la funzione monty_hall().
#
# Ogni partita restituisce TRUE se il concorrente vince,
# FALSE se perde.
#
# mean() calcola la proporzione di TRUE,
# cioè la frequenza empirica di vittorie.

prob_resto <- mean(replicate(10000, monty_hall(change = FALSE)))
prob_resto

# Simuliamo 10000 partite in cui il concorrente cambia porta.

prob_cambio <- mean(replicate(10000, monty_hall(change = TRUE)))
prob_cambio

# =========================================================
# INTERPRETAZIONE DEI RISULTATI
# =========================================================

# Ci aspettiamo di ottenere circa:
#
# prob_resto  ≈ 1/3
# prob_cambio ≈ 2/3
#
# Quindi, secondo la simulazione, cambiare porta conviene.
#
# Perché?
#
# All'inizio il concorrente sceglie una porta su tre.
# La probabilità che la scelta iniziale sia corretta è 1/3.
#
# Quindi la probabilità che la scelta iniziale sia sbagliata è 2/3.
#
# Se il concorrente resta, vince solo quando la scelta iniziale
# era già corretta.
#
# Quindi:
# P(vittoria restando) = 1/3
#
# Se invece il concorrente cambia, vince quando la scelta iniziale
# era sbagliata.
#
# Infatti, se la scelta iniziale era sbagliata, il premio si trova
# necessariamente in una delle altre due porte.
# Il conduttore apre una porta perdente, quindi l'altra porta rimasta
# chiusa contiene il premio.
#
# Quindi:
# P(vittoria cambiando) = 2/3
