#Selecting and Viewing the Data set
Data <- read.csv(file.choose(),header = TRUE)
View(Data)

#Filtering the Data
library(dplyr)
dde1= Data %>% select(country,year,population,gdp,coal_electricity,coal_production,coal_consumption) %>% filter(country == "China" & year >= 1986 & year <=2018)
View(dde1)
summary(dde1)
str(dde1)

dde <- dde1[,-1] 

#Scaling the data
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

dde_norm <- as.data.frame(lapply(dde[,-1], normalize))
summary(dde_norm)
View(dde_norm)
dde_norm <- cbind(dde1[,2],dde_norm)
colnames(dde_norm)[1]<-("year")


#Creating test and train datasets
set.seed(1234)
ind <- sample(2, nrow(dde_norm), replace = T, prob = c(0.7, 0.3))
dde_train <- dde_norm[ind == 1,]
dde_test <- dde_norm[ind == 2,]


str(dde_test)
View(dde_test)
View(dde_train)

# training a model on the above data
library(neuralnet)

dde_model <- neuralnet(coal_consumption~., data = dde_train[,-1], hidden = c(3,2))
dde_model




# model results
model_results <- compute(dde_model,dde_test[,-6])
model_results 
(predicted_cons <- model_results$net.result)
cor(dde_test$coal_consumption, predicted_cons)

VALIDATION=table(dde_test[,6],predicted_cons)

(ACCURACY=sum(diag(VALIDATION))/sum(VALIDATION)*100)

#Charts
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


#future prediction 
NEW=data.frame(population=1425891504,gdp=19200000000000,coal_electricity=5445,coal_production=24651.393)
summary(NEW)
check_values <- predict(dde_model, newdata = NEW)
check_values

