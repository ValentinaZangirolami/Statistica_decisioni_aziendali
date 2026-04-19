################################################################################
#                                                                              #
#                                  ESERCIZI                                    #
#                                                                              #
################################################################################

# 1. Calcolate il perimetro e l'area del cerchio con raggio 4. E stampa il risultato

# 2. Installare la library dplyr e mostra i datasets disponibili

# 3. Leggete le pagine help di factorial e choose

# 4. Calcolate il numero di modi per ordinare 7 libri tra 20

# 5. Arrotondare il numero n = 3.78957 al secondo decimale

# 6. Visualizzare tutte le variabili nel global environment e cancellarle

# 7. Crea una lista di animali con le seguenti istanze ("Cat", "Dog", "Bird", "Bird", "Cat", "Cat", "Dog").
#    Calcola le frequenze relative e assolute. Stampa la categoria con la frequenza maggiore.

# 8. Considerando i vettori x e y, create un nuovo vettore z unendo ogni elemento di x con il corrispettivo valore
#    di y (es. z[1] <- "Red Tomato"). Usare funzione paste.
x <- c("Red", "Green", "Yellow")
y <- c("Tomato", "Grass", "Sun")
#   Successivamente selezionare l'elemento di z che inizia con R e finisce con o. Usate 
#   le funzioni startsWith and endsWith

# 9. Crea una matrice A contenente gli interi da 10 a 24 con 5 righe e 3 colonne,
#    dove A deve essere riempita per colonne. Mostra le prime 3 righe delle prime 2 colonne.

#    Calcola la somma di A per righe e per colonne (Suggerimento: rowSums(), colSums()).

#    Calcola la trasposta di A.

#    Definisci una nuova matrice B di dimensione appropriata per calcolare il prodotto AB (e calcola tale prodotto).

#    Calcola la deviazione standard di B per riga e poi per colonna (Suggerimento: apply()).

# 10. Crea una funzione che prendendo in input un vettore dia come risultato una lista
#     in cui si calcolano le principali statistiche (min, mean, var, max)

# 11. Considerando che l'indice di asimmetria si calcola nel seguente modo
I <- 3 * (x_bar - q2) / s
#     dove x_bar è la media dei valori del vettore, q2 la mediana e s la deviazione standard.
#     Supponi che un insieme di dati sia memorizzato nel vettore data in R. 
#     Scrivi una funzione skewness(data) che calcoli l’indice di asimmetria 
#     e restituisca, a seconda del caso, “I dati sono significativamente asimmetrici” oppure
#     “I dati non sono significativamente asimmetrici”.