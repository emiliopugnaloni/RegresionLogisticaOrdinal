
# **************************************************
# REGRESION LOGISTICA ORDINAL:Ejemplo de aplicacion
# *************************************************

#----Autores-----
# Alberto Sotelo
# Emilio Pugnaloni
# -----------------

#install.packages("VGAM")  #para Regresion Logistica Multinomial 
#install.packages("MASS")  #para Regresion Logistica Ordinal

require(purrr)
require(ggplot2)
require(corrplot)
require(VGAM) 
require(MASS)
require(dplyr)
require(GGally)

#------------------
# PREPROCESAMIENTO
# ----------------

#Carga del Dataset
setwd("C:/Users/emiba/Documents/EnfoqueEstadisticoAprendizaje/Trabajo Final/Dataset")
data<- read.csv("./WineQT.csv")

#Vemos la cantidad de vinos segun calidad:
round(table(data$quality),2)

#Creamos Variables Target (ordinal) y hacemos feature-selection
data <- data %>%
  select(-c('fixed.acidity', 'residual.sugar', 'chlorides', 'free.sulfur.dioxide', 'pH', 'Id')) %>%
  mutate(quality = factor(case_when(quality<=5 ~ 'Malo',
                             quality == 6 ~ 'Regular',
                             TRUE ~ 'Bueno'),
                          levels = c('Malo', 'Regular', 'Bueno')))

# Vemos el Dataset
View(data) #
glimpse(data)


#---------------------
# ANALISIS DESCRIPTIVO
# --------------------

data %>% ggpairs(.,
               mapping = ggplot2::aes(colour=quality), 
               lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1)),
               upper= list(continuous = wrap("cor", size=4.5))) +
  scale_color_manual(values = c("Bueno" = "darkgreen", "Malo" = "red", "Regular" = "yellow"))+
  scale_fill_manual(values = c("Bueno" = "darkgreen", "Malo" = "red", "Regular" = "yellow"))





##################################
# Multinomial Logisitc Regression
#################################

# Quality como nominal
data$quality_nominal  <- factor( case_when(data$quality == 'Malo'~ 1,
                                       data$quality == 'Regular' ~ 2,
                                       TRUE ~ 3), levels = c(1,2,3))

mnl <- vglm(quality_nominal ~ volatile.acidity, data = data, family="multinomial")
# mnl <- vglm(quality_nominal ~ volatile.acidity
#             + citric.acid
#             + total.sulfur.dioxide
#             + density
#             + sulphates
#             + alcohol
#             , data = data, family="multinomial")

summary(mnl)

#Matriz Confusion
mnl_predictions <- round(mnl@fitted.values,2)
mnl_predicted <- apply(mnl_predictions, 1, which.max)
table(data$quality_nominal, mnl_predicted)
sum(diag(table(data$quality_nominal, mnl_predicted)))/length(data$quality) #accuracy

#Predecimos para un valor de acidez
predict(mnl, data.frame(volatile.acidity=0.5),  type="response")



#####################
# ORDERED LOGIT MODEL
#####################

# Quality como Ordinal
data$quality_ordinal  <- factor( case_when(data$quality == 'Malo'~ 1,
                                           data$quality == 'Regular' ~ 2,
                                           TRUE ~ 3),
                                 levels = c(1,2,3),
                                 ordered = TRUE)

olr <-   polr(quality_ordinal ~ volatile.acidity, data = data, Hess=TRUE)
# olr <-  polr(quality_ordinal ~ volatile.acidity
#             + citric.acid
#             + total.sulfur.dioxide
#             + density
#             + sulphates
#             + alcohol
#             , data = data, Hess=TRUE)

summary(olr)

# Matriz confusion
olr_predictions <- round(olr$fitted.values,2)  #guardamos las predicciones
olt_predicted <- apply(olr_predictions, 1, which.max) #nos quedamos con el valor maximos predicho
table(data$quality_ordinal, olt_predicted) #imprimimos matriz confusion
sum(diag(table(data$quality_ordinal, olt_predicted)))/length(data$quality) #accuracy


#Predecimos para un valor de acidez
predict(olr, data.frame(volatile.acidity=0.5),  type="p")



## --------------------------
## Graficas de probabilidades
## --------------------------

## REGRESION LOGISTICA ORDINAL


## Grafica de Probabilidades

#generamos dataframe para guardar las probabilidades de cada nivel segun valores de "volatile_acidity"
volatile_acidity_values <- seq(0.0, 1, by = 0.001) 

predictions <- data.frame(Malo = numeric(length(volatile_acidity_values)),
                          Regular = numeric(length(volatile_acidity_values)),
                          Bueno = numeric(length(volatile_acidity_values)))



#predecimos las probabilidades
for (i in 1:length(volatile_acidity_values)) {
  predictions[i, ] <- predict(olr, data.frame(volatile.acidity = volatile_acidity_values[i]), type = "p")
}

# Plotting the curves
plot(volatile_acidity_values, predictions$Malo, type = "l", col = "red", xlab = "Volatile Acidity", ylab = "Probability", ylim = c(0, 1))
lines(volatile_acidity_values, predictions$Regular, type = "l", col = "blue")
lines(volatile_acidity_values, predictions$Bueno, type = "l", col = "green")
legend("topright", legend = c("Malo", "Regular", "Bueno"), col = c("red", "blue", "green"), lty = 1)
title(main = "Probabilidad de cada nivel con Modelo Logsitico Ordinal")





## REGRESION LOGISTICA MULTINOMIAL


#generamos dataframe para guardar las probabilidades de cada nivel segun valores de "volatile_acidity"
volatile_acidity_values <- seq(0.0, 1, by = 0.001) 

predictions <- data.frame(Malo = numeric(length(volatile_acidity_values)),
                          Regular = numeric(length(volatile_acidity_values)),
                          Bueno = numeric(length(volatile_acidity_values)))

#predecimos las probabilidades
for (i in 1:length(volatile_acidity_values)) {
  predictions[i, ] <- predict(mnl, data.frame(volatile.acidity = volatile_acidity_values[i]), type = "response")
}

# Plotting the curves
plot(volatile_acidity_values, predictions$Malo, type = "l", col = "red", xlab = "Volatile Acidity", ylab = "Probability", ylim = c(0, 1))
lines(volatile_acidity_values, predictions$Regular, type = "l", col = "blue")
lines(volatile_acidity_values, predictions$Bueno, type = "l", col = "green")
legend("topright", legend = c("Malo", "Regular", "Bueno"), col = c("red", "blue", "green"), lty = 1)
title(main = "Probabilidad de cada nivel con Modelo Multinomial Logsitico")

