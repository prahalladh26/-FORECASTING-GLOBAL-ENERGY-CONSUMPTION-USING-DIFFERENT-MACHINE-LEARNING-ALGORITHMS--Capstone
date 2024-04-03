Data <- read.csv(file.choose(),header = TRUE)# Reading the Dataset 

View(Data) # Viewing the Dataset

#install.packages("dplyr")                          

library(dplyr) # Loading the dplyr library

#filtering the United States Data
dde1= Data %>% select(country,year,population,gdp,oil_electricity,oil_production,oil_consumption) %>% filter(country == "United States" & year >= 1990 & year <=2018)
View(dde1)
summary(dde1)
str(dde1)
dde <- dde1[,-1] #removing Country Column
View(dde)
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x))) # normalizing the independent variables
}

dde_norm <- as.data.frame(lapply(dde[,-1], normalize))
summary(dde_norm)
View(dde_norm)
dde_norm <- cbind(dde[,1],dde_norm)
colnames(dde_norm)[1]<-("year")


set.seed(1234) # Randomizing the train and test data
ind <- sample(2, nrow(dde_norm), replace = T, prob = c(0.7, 0.3))
dde_train <- dde_norm[ind == 1,]
dde_test <- dde_norm[ind == 2,]


str(dde_test)
View(dde_test)
View(dde_train[,-1])

# training a model on the above data
library(neuralnet)

?neuralnet

dde_model <- neuralnet(oil_consumption~., data = dde_train[,-1],hidden = c(3,2))
dde_model
plot(dde_model)


library(ggplot2)

ggplot(dde1, aes(x = year, y = oil_consumption)) +
  geom_line(color = "red") +
  scale_x_continuous(breaks = seq(1990, 2018, by = 4)) +
  scale_y_continuous(breaks = seq(9000, 11000, by = 200)) +
  ggtitle("Oil Consumption - United States") +
  xlab("Year") +
  ylab("Oil Consumption")

# model results
model_results <- compute(dde_model,dde_test[,-6])
model_results

predicted_cons <- model_results$net.result
cor(dde_test$oil_consumption, predicted_cons)

VALIDATION=table(dde_test[,6],predicted_cons)
(ACCURACY=sum(diag(VALIDATION))/sum(VALIDATION)*100)

data_mod <- data.frame(Year = dde_test[,1],Predicted = predicted_cons,
                       Actual = dde_test[,6])
View(data_mod)

## Actual vs Predicted Double line graphs 

Bar <- ggplot(data_mod, aes(Year)) +  
  geom_line(aes(y = Actual, color = "Actual"),size = 1.5) +
  geom_line(aes(y = Predicted,color = "Predicted"),size = 1.5)+
  scale_color_manual(values = c("Actual" = "red", "Predicted" = "blue"))+
  scale_x_continuous(breaks = seq(1994, 2018, by = 4)) +
  xlab("Year") +
  ylab("Values") +
  ggtitle("Actual vs Predicted Values")

ggplotly(Bar)
