################################################################################
#                             Logistic regression                              #
#                                                                              #
################################################################################
#                           Valentina Zangirolami                              #
#                       valentina.zangirolami@unimib.it                        #
################################################################################

#caricamento librerie

library(car)
library(ggplot2)
library(ISLR2)

data("Default") #carichiamo un dataset presente nella libreria ISLR2
help(Default, package = ISLR2)
head(Default)

# Vogliamo capire chi sarà inadempiente ai debiti della carta di credito utilizzando
# tre covariate: Student (se customer è studente o no), Balance (media del bilancio 
# mensile rimanente su carta di credito) e Income (reddito)

# numero mercati n=10000
nrow(Default)

# prima analisi dei dati: visualizziamo le caratteristiche dei nostri dati
summary(Default)
# notiamo che Default non è bilanciata
# proviamo a vedere graficamente

ggplot(Default, aes(x = default)) +
  geom_bar(aes(y = ..count..), fill = "pink") +
  labs(x = "Default",
       y = "Frequency") +
  theme_minimal()
# questo potrebbe causare un problema nella stima del nostro modello

#analizziamo i dati

ggplot(Default, aes(x = balance, y = income, color = default)) +
  geom_point(size = 1, shape=3) + 
  labs(
    x = "Balance",
    y = "Income",
    color = "Default"
  ) + theme_minimal()

ggplot(Default, aes(x = balance, y = income, color = default)) +
  geom_point(size = 1, shape=3) + 
  labs(
    x = "Balance",
    y = "Income",
    color = "Default"
  ) + facet_wrap(~ student)+ theme_minimal()

Default$default <- ifelse(Default$default == "Yes", 1, 0)
Default$default <- as.factor(Default$default)

# regressione logistica

mod_glm <- glm(default ~ balance + income + student, family="binomial", data=Default)
summary(mod_glm)

# intercetta e coef di balance sembrano essere significative per ogni alpha
# income non è significativa
# la dummy relativa a student è significativa per gli usuali valori di alpha

# la devianza del modello nullo (senza covariate) è 2920.6 con n-1 DoF
# la devianza del modello fittato è 1571 (quindi abbiamo un improvement)
# AIC=1579.5

# proviamo a rimuovere income

mod_glm_2 <- glm(default ~ balance + student, family="binomial", data=Default)
summary(mod_glm_2)

# notiamo che ora tutti i coef sono significativi
# devianza residua è circa la stessa e AIC decresce: preferiamo questo modello

# odds ratio

exp(coef(mod_glm_2)[2])

# l'odds ratio aumenta di 1.0058 quando balance aumenta di 1 

exp(coef(mod_glm_2)[3])

# l'odds ratio aumenta di 0.489 quandoil customer è uno studente (mantenendo costante balance)

# confronto dei due modelli in termini di accuracy, tasso di errore, sensitivity e specificity

# Y previsti

pred_glm_1 <- predict(mod_glm, Default[,-1], type = "response")
pred_glm_2 <- predict(mod_glm_2, Default[,-1], type = "response")

pred_glm_1 <- ifelse(pred_glm_1 >0.5, 1, 0 )
pred_glm_2 <- ifelse(pred_glm_2 >0.5, 1, 0 )

#definiamo accuracy
accuracy <- function(truth, predicted) {
  mean(truth == predicted)
}

(acc_glm_1 <- accuracy(Default$default, pred_glm_1)) #0.9732
(acc_glm_2 <- accuracy(Default$default, pred_glm_2))  #0.9733

#migliore accuracy nel secondo modello

#possiamo anche calcolare l'errore

1-acc_glm_1
1-acc_glm_2

#maggiore errore nel primo modello

#confusion matrix

(conf_matrix_1 <- table(pred_glm_1, Default$default))
(conf_matrix_2 <- table(pred_glm_2, Default$default))

#oppure possiamo calcolarci singolarmente TP, FP, FN, TN

(true_positives_1 <- sum(Default$default == 1 & pred_glm_1 == 1))
(true_positives_2 <- sum(Default$default == 1 & pred_glm_2 == 1))

(false_positives_1 <- sum(Default$default == 0 & pred_glm_1 == 1))
(false_positives_2 <- sum(Default$default == 0 & pred_glm_2 == 1))

(false_negatives_1 <- sum(Default$default == 1 & pred_glm_1 == 0))
(false_negatives_2 <- sum(Default$default == 1 & pred_glm_2 == 0))

(true_negatives_1 <- sum(Default$default == 0 & pred_glm_1 == 0))
(true_negatives_2 <- sum(Default$default == 0 & pred_glm_2 == 0))

#sensitivity

(sensitivity_1 <- true_positives_1/(true_positives_1 + false_negatives_1))
(sensitivity_2 <- true_positives_2/(true_positives_2 + false_negatives_2))

# solo il 31.53% di default=Yes è stato previsto correttamente per entrambi i modelli
# --> può essere dovuto alla classe sbilanciata della risposta

#specificity

(specificity_1 <- true_negatives_1/(true_negatives_1 + false_positives_1))
(specificity_2 <- true_negatives_2/(true_negatives_2 + false_positives_2))

#mentre la classe default=No è stata prevista correttamente il 99.59% per mod 1
#e 99.6% per mod 2

# --> questo a causa della classe sbilanciata!

