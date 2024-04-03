Data <- read.csv(file.choose(),header = TRUE)
View(Data)

library(dplyr)
dde1= Data %>% select(country,year,population,gdp,coal_electricity,coal_production,coal_consumption) %>% filter(country == "India" & year >= 1986 & year <=2018)
View(dde1)
summary(dde1)
str(dde1)
dde <- dde1[,-1] #removing the Country column

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

dde_norm <- as.data.frame(lapply(dde[,-1], normalize))
summary(dde_norm)
View(dde_norm)
dde_norm <- cbind(dde1[,2],dde_norm)
colnames(dde_norm)[1]<-("year")

set.seed(1234)
ind <- sample(2, nrow(dde_norm), replace = T, prob = c(0.7, 0.3))
dde_train <- dde_norm[ind == 1,]

dde_test <- dde_norm[ind == 2,]


str(dde_test)
View(dde_test)
View(dde_train)

# training a model on the above data
library(neuralnet)

?neuralnet

dde_model <- neuralnet(coal_consumption~., data = dde_train[,-1], hidden = c(3,2))
dde_model


plot(dde_model)

library(ggplot2)
ggplot(dde1, aes(x = year, y = coal_consumption)) +
  geom_line(color = "red") +
  scale_x_continuous(breaks = seq(1985, 2018, by = 3)) +
  scale_y_continuous(breaks = seq(800, 6000, by = 400)) +
  ggtitle("Coal Consumption - India") +
  xlab("Year") +
  ylab("Primary Energy Consumption sourced from Coal")



# model results
model_results <- compute(dde_model,dde_test[,-6])
model_results 
(predicted_cons <- model_results$net.result)
cor(dde_test$coal_consumption, predicted_cons)

VALIDATION=table(dde_test[,6],predicted_cons)
(ACCURACY=sum(diag(VALIDATION))/sum(VALIDATION)*100)

data_mod <- data.frame(Year = dde_test[,1],Predicted = predicted_cons,
                       Actual = dde_test[,6])
View(data_mod)
## Bar Chart 

Bar <- ggplot(data_mod, aes(Year)) +  
  geom_line(aes(y = Actual, color = "Actual"),size = 1.5) +
  geom_line(aes(y = Predicted,color = "Predicted"),size = 1.5)+
  scale_color_manual(values = c("Actual" = "red", "Predicted" = "blue"))+
  scale_x_continuous(breaks = seq(1994, 2018, by = 4)) +
  xlab("Year") +
  ylab("Values") +
  ggtitle("Actual vs Predicted Values")
Bar
library(plotly)
ggplotly(Bar)
