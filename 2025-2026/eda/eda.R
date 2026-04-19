# =========================================================
# EDA IN R
# =========================================================

# Come si carica un dataset? Prima di tutto vediamo in quale cartella siamo
getwd()

# Impostiamo il path giusto
dir <- "C:/Users/valen/Documents/GitHub/Statistical-Learning/Statistica_decisioni_aziendali/2025-2026/eda"
setwd(dir)

# ok verifichiamo
getwd()

# carichiamo il dataset
telco <- read.csv(file = "churn_data.csv", 
                  header = TRUE, sep = ",", row.names = 1)


## ----------------------------------------------------------------------------------
str(telco)


## ----------------------------------------------------------------------------------
head(telco, 10)


# =========================================================
# 1. ANALISI UNIVARIATA
# =========================================================


## ----------------------------------------------------------------------------------
telco$Tenure


## ----------------------------------------------------------------------------------
telco$ChurnLabel


## ----------------------------------------------------------------------------------
summary(telco$Tenure)

boxplot(telco$Tenure)

hist(telco$Tenure,
     main = "Distribuzione di Tenure",
     xlab = "Tenure",
     col = "lightblue")


## ----------------------------------------------------------------------------------
table(telco$ChurnLabel)

barplot(table(telco$ChurnLabel),
        main = "Distribuzione Churn",
        col = "pink")

prop.table(table(telco$ChurnLabel))

## ----------------------------------------------------------------------------------

table(telco$Contract)

barplot(table(telco$Contract),
        main = "Tipo di contratto",
        col = "lightgreen")

# =========================================================
# 2. ANALISI BIVARIATA
# =========================================================

## ----------------------------------------------------------------------------------

table(telco$Contract, telco$ChurnLabel)
prop.table(table(telco$Contract, telco$ChurnLabel), 1)

barplot(table(telco$Contract, telco$ChurnLabel),
        beside = TRUE,
        col = c("blue", "red"),
        legend = TRUE,
        main = "Churn per tipo di contratto")

## ----------------------------------------------------------------------------------

boxplot(Tenure ~ ChurnLabel,
        data = telco,
        col = c("red", "green"),
        main = "Tenure vs Churn")

## ----------------------------------------------------------------------------------

boxplot(TotalCharges ~ ChurnLabel,
        data = telco,
        col = c("red", "green"),
        main = "Total Charges vs Churn")

boxplot(MonthlyCharges ~ ChurnLabel,
        data = telco,
        col = c("red", "green"),
        main = "Monthly Charges vs Churn")

## ----------------------------------------------------------------------------------
boxplot(AvgMonthlyGBDownload ~ ChurnLabel, data = telco)

# =========================================================
# 3. ANALISI TRA NUMERICHE
# =========================================================

cor(telco$Tenure, telco$TotalCharges)

## ----------------------------------------------------------------------------------

pairs(telco[, c("Tenure", "MonthlyCharges", "TotalCharges")])

#esercizio: carica il dataset london sharing bike 
# e ripetete l'eda che abbiamo fatto finora

