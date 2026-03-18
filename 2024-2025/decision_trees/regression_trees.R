################################################################################
#                              Alberi decisionali                              #
#                                 Regressione               ,                  #
################################################################################
#                           Valentina Zangirolami                              #
#                       valentina.zangirolami@unimib.it                        #
################################################################################

# In generale potete scaricare/trovare dataset nei seguenti siti
# 1. Kaggle
# 2. UCI Machine Learning Repository
# 3. Google Dataset search: https://datasetsearch.research.google.com/ 
#    (data from both papers and the above websites)

#caricamento libraries

library(ggplot2)
library(rpart)
library(rpart.plot)
library(vip)

# caricamento dati

esg_data <- read.csv("esg_financial_dataset.csv")
# questa volta analizziamo la versione completa di questo dataset

#esploriamo le caratteristiche delle variabili
summary(esg_data)

#quando consideriamo gli alberi decisionali è sempre meglio fare one-hot encoding delle variabili
#categoriali

# 1. sempre meglio convertire i type "character" in "factor"

esg_data$Industry <- as.factor(esg_data$Industry)
esg_data$Region <- as.factor(esg_data$Region)

# 2. One-hot encoding

encoded_data <- model.matrix(~ . - 1, data = esg_data)
esg_data <- as.data.frame(encoded_data)

# albero decisionale
help(rpart)

mytree <- rpart(MarketCap ~ ., data = esg_data, method = "anova", control = rpart.control(cp = 0.001))
summary(mytree)

#visualizzazione grafica

plot(mytree, uniform = TRUE, main = "Albero di regressione")
text(mytree, use.n = TRUE, all = TRUE, cex = .8)

#feature importance

vip(mytree, geom = "point", horizontal = FALSE) +
  ggtitle("Decision Tree Feature Importance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
# una delle variabili più importanti: revenue

