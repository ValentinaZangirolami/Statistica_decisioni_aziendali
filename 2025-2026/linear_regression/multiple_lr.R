################################################################################
###################      MULTIPLE LINEAR REGRESSION    #########################


esg_data <- read.csv("esg_financial_dataset.csv", row.names = 1)
head(esg_data)

# Vorremmo studiare la relazione tra Market capitalization congiuntamente a diverse covariate
# (Industry, ESG_Overall, Revenue, GrowthRate, WaterUsage, CarbonEmission)

# check variables

summary(esg_data)

#visualize data

Scatter_Matrix <- GGally::ggpairs(esg_data[,-1], 
                                  title = "Scatter Plot Matrix for ESG data", 
                                  axisLabels = "show") 
Scatter_Matrix

# relazioni marginali risposta-covariata: 
# - market cap e revenue hanno una relazione lineare (vediamo anche correlazione = 0.831)
# - WaterUsage e Carbon Emissions hanno una correlazione di circa 0.35 con MarketCap
# - ESG_Overall e MarketCap sembrano non avere una relazione lineare
# - GrowthRate e MarketCap sembrano non avere una relazione lineare
# dobbiamo stare attenti alla correlazione anche tra covariate: dobbiamo evitare la multicollinearità

# plot industry

ggplot(esg_data, aes(x = Industry)) +
  geom_bar(aes(y = ..count..), fill = "pink") +
  labs(x = "Industry",
       y = "Frequency") +
  theme_minimal()

ggplot(esg_data, aes(x = Industry, y = MarketCap, fill = Industry)) + 
  geom_boxplot() +
  ylim(0, 200000) + #migliorare la visualizzazione: abbiamo ridimensionato il range di MarketCap
  labs(title = "Boxplot",
       x = "Industry",
       y = "MarketCap") +
  theme_minimal()

#multiple linear regression
mod <- lm(MarketCap~., data=esg_data)
summary(mod)

# WaterUsage, CarbonEmissions, Revenue, IndustryTechnology e IndustryFinance hanno coefficienti significativi
# R2=0.7226 --> modello abbastanza buono
# F-statistic pone sotto H0: tutti coefficienti (no intercetta)=0 
# il pvalue è al di sotto di ogni soglia alpha --> rifiutiamo H0

# verifica assunzioni modello lineare
par(mfrow=c(2,2))
plot(mod)

# plot in alto-sinistra: sembra ci sia un leggero pattern --> forse problemi con linearità
# plot in alto-destra: code pesanti
# plot in basso-sinistra: omoschedasticità non sembra propriamente rispettata
# plot in alto-destra: qualche problema con influential points (76, 3034)

#interaction

ggplot(esg_data, aes(x = GrowthRate, y = MarketCap, color = Industry)) +
  geom_point(size = 1, shape=3) +
  geom_smooth(method = "lm", se = FALSE) +  # Adds linear regression lines
  labs(
    x = "GrowthRate",
    y = "MarketCap",
    color = "Industry"
  ) + ylim(0, 35000) + 
  theme_minimal()

ggplot(esg_data, aes(x = Revenue, y = MarketCap, color = Industry)) +
  geom_point(size = 1, shape=3) +
  geom_smooth(method = "lm", se = FALSE) +  # Adds linear regression lines
  labs(
    x = "GrowthRate",
    y = "MarketCap",
    color = "Industry"
  ) + ylim(0, 35000) + 
  theme_minimal()

ggplot(esg_data, aes(x = WaterUsage, y = MarketCap, color = Industry)) +
  geom_point(size = 1, shape=3) +
  geom_smooth(method = "lm", se = FALSE) +  # Adds linear regression lines
  labs(
    x = "GrowthRate",
    y = "MarketCap",
    color = "Industry"
  ) + ylim(0, 35000) + 
  theme_minimal()

#dai plot sembra che tutti possano essere buoni candidati per interazioni
# proviamo ad aggiungere GrowthRate:Industry 

#add interaction
mod_int <- lm(MarketCap ~ . + GrowthRate:Industry, data = esg_data)
summary(mod_int)#interazioni non significative
#leggero incremento di R2
plot(mod_int)
# simile interpretazioni

#esercizio: provate a valutare le altre interazioni