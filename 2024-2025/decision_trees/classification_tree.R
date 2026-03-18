################################################################################
#                              Alberi decisionali                              #
#                               Classificazione             ,                  #
################################################################################
#                           Valentina Zangirolami                              #
#                       valentina.zangirolami@unimib.it                        #
################################################################################

#caricamento libraries

library(ggplot2)
library(rpart)
library(rpart.plot)
library(vip)

# caricamento dati

churn_data <- read.csv("churn_data.csv", row.names = 1)

#visualizziamo prime sei osservazioni
head(churn_data)
# The data refers to home phone and internet services of customers in California.
# Data description: https://community.ibm.com/community/user/businessanalytics/blogs/steven-macko/2019/07/11/telco-customer-churn-1113

#descrizione variabili
summary(churn_data)

#trasformiamo le variabili categoriali in factor

churn_data[sapply(churn_data, is.character)] <- lapply(churn_data[sapply(churn_data, is.character)], as.factor)

# vediamo se la variabile risposta è sbilanciata

ggplot(churn_data, aes(x = ChurnLabel)) +
  geom_bar(aes(y = ..count..), fill = "pink") +
  labs(x = "Y",
       y = "Frequency") +
  theme_minimal() #sbilanciata

# One-hot encoding

encoded_data <- model.matrix(~ . - 1, data = churn_data)
churn_data <- as.data.frame(encoded_data)

#rimuoviamo encoded_data dall'environment
rm(encoded_data)

# albero di classificazione

mytree <- rpart(ChurnLabelYes ~ ., data = churn_data, method = "class", control = rpart.control(cp = 0.001))
summary(mytree)

#visualizzazione grafica

plot(mytree, uniform = TRUE, main = "Albero di classificazione")
text(mytree, use.n = TRUE, all = TRUE, cex = .5)

#feature importance

vip(mytree, geom = "point", horizontal = FALSE) +
  ggtitle("Decision Tree Feature Importance") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
# variabili più importanti:TotalCharges, TotalRevenue, Tenure, MonthlyCharges