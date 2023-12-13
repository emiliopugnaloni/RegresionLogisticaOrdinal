##########################################
# EJEMPLO DE REGRESION LOGISTICA ORDINAL #
##########################################


require(purrr)
require(ggplot2)
require(corrplot)
require(VGAM) 
require(MASS)
require(dplyr)

###############
# PREPROCESSING
###############

#Load Dataset
setwd("C:/Users/epugnalo/OneDrive - Telefonica/Documents/Data Mining UBA/2C - Enfoque Estadistico del Aprendizaje/Trabajo Final/Dataset")
data1<- read.csv("./Sleep_health_and_lifestyle_dataset.csv")

#Descriptive Analysis
summary(data1)
data1 %>% head(4)

#Correlations 
data1 %>%
  select(where(is.numeric)) %>%
  cor() %>%
  round(2)

data1 %>%
  select(where(is.numeric)) %>%
  cor() %>%
  corrplot(., type="upper", order="hclust")

# Cartegorical variables vs quality sleep
boxplot(data1$Quality.of.Sleep~ data1$Gender)
boxplot(data1$Quality.of.Sleep~ data1$BMI.Category)
boxplot(data1$Quality.of.Sleep~ data1$Sleep.Disorder)



#Target Variable
boxplot(data1$Quality.of.Sleep)
table(data1$Quality.of.Sleep) #dividimos en 456, 7, 8 y 9?


# eliminar person.id
# quedarnos con stress.level o heart.rate?
# quedarnos con physical activity level? (daily steps no influye mucho)
# con las variables categoricas, diria de quedarnos con algunas nomas



#Create Target Clase & Feature Selection
data <- data1 %>%
  select(-c('Person.ID', 'Heart.Rate', 'Daily.Steps', 'Occupation', 'Blood.Pressure')) %>%
  
  mutate(Obese = case_when((BMI.Category=="Obese") | (BMI.Category=="Overweight")  ~ 1,
                             TRUE ~ 0),
         Male = case_when(Gender=="Male" ~ 1,
                           TRUE ~ 0)) %>%  
  mutate(Quality.of.Sleep = case_when(Quality.of.Sleep<=6 ~ 'Bad',
                                    Quality.of.Sleep == 7 ~ 'Regular',
                                    Quality.of.Sleep == 8 ~ 'Good',
                             TRUE ~ 'Excellent')) %>%
  mutate( Quality.of.Sleep = factor(.$Quality.of.Sleep, 
                              levels=c('Bad', 'Regular', 'Good', 'Excellent'), 
                              ordered=TRUE)) %>%
  
  select( -c('Gender', 'BMI.Category', 'Sleep.Disorder','Obese' ))


head(data,2)
summary(data)
table(data$Quality.of.Sleep)


# ##################################
# # Multinomial Logisitc Regression
# ################################]#
# 
mlr <- vglm( Quality.of.Sleep ~ Sleep.Duration, data = data, family = multinomial)
#mlr <- vglm( Quality.of.Sleep ~ ., data = data, family = multinomial)




summary(mlr)

mlr_predictions <- round(mlr@fitted.values,2)
mlr_predicted <- apply(mlr_predictions, 1, which.max)
table(data$Quality.of.Sleep, mlr_predicted)

predict(mlr, data.frame(Sleep.Duration=8),  type="response")

#####################
# ORDERED LOGIT MODEL
#####################

#olr <-   polr(Quality.of.Sleep ~ Sleep.Duration, data = data, Hess=TRUE)
olr <-   polr(Quality.of.Sleep ~ . , data = data, Hess=TRUE)

summary(olr)


olr_predictions <- round(olr$fitted.values,2)
olt_predicted <- apply(olr_predictions, 1, which.max)
table(data$Quality.of.Sleep, olt_predicted)


## Cual es la probabilidad para X determinado ....
predict(olr, data.frame(Sleep.Duration=7),  type="p")




###################################
##### GRAFICA PARA MODELO ORDINAL##
###################################

Sleep.Duration_values <- seq(5, 9, by = 0.5) #valores de la variable 

#creamos dataframe para guardar probabilidaddes en dif valores de Sleep.Duration_values
predictions <- data.frame(Bad = numeric(length(Sleep.Duration_values)),
                          Regular = numeric(length(Sleep.Duration_values)),
                          Good = numeric(length(Sleep.Duration_values)),
                          Excellent = numeric(length(Sleep.Duration_values)))
                          
            
#predecimos las probabilidades
for (i in 1:length(Sleep.Duration_values)) {
  predictions[i, ] <- predict(olr, data.frame(Sleep.Duration = Sleep.Duration_values[i]), type = "p")
}

# Plotting the curves
plot(Sleep.Duration_values, predictions$Bad, type = "l", col = "red", xlab = "Sleep.Duration", ylab = "Probability", ylim = c(0, 1))
lines(Sleep.Duration_values, predictions$Regular, type = "l", col = "orange")
lines(Sleep.Duration_values, predictions$Good, type = "l", col = "green")
lines(Sleep.Duration_values, predictions$Excellent, type = "l", col = "darkgreen")
legend("topright", legend = c("Bad", "Regular", "Good", "Excellent"), col = c("red", "yellow", "green", 'darkgreen'), lty = 1)


# You can add a title to the plot if necessary
title(main = "Probabilidad de cada nivel con Modelo Logsitico Ordinal")






###################################
##### GRAFICA PARA MODELO NOMIAL##
###################################

Sleep.Duration_values <- seq(5, 9, by = 0.5) #valores de la variable 

#creamos dataframe para guardar probabilidaddes en dif valores de Sleep.Duration_values
predictions <- data.frame(Bad = numeric(length(Sleep.Duration_values)),
                          Regular = numeric(length(Sleep.Duration_values)),
                          Good = numeric(length(Sleep.Duration_values)),
                          Excellent = numeric(length(Sleep.Duration_values)))


#predecimos las probabilidades
for (i in 1:length(Sleep.Duration_values)) {
  predictions[i, ] <- predict(mlr, data.frame(Sleep.Duration = Sleep.Duration_values[i]), type = "response")
}

# Plotting the curves
plot(Sleep.Duration_values, predictions$Bad, type = "l", col = "red", xlab = "Sleep.Duration", ylab = "Probability", ylim = c(0, 1))
lines(Sleep.Duration_values, predictions$Regular, type = "l", col = "orange")
lines(Sleep.Duration_values, predictions$Good, type = "l", col = "green")
lines(Sleep.Duration_values, predictions$Excellent, type = "l", col = "darkgreen")
legend("topright", legend = c("Bad", "Regular", "Good", "Excellent"), col = c("red", "orange", "green", 'darkgreen'), lty = 1)


# You can add a title to the plot if necessary
title(main = "Probabilidad de cada nivel con Modelo Logsitico Ordinal")
