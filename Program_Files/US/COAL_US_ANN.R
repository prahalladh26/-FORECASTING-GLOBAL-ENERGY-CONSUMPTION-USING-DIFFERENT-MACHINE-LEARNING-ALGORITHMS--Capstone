#Loading of Data set 
Data <- read.csv(file.choose(),header = TRUE)
#Viewing the Data
View(Data)

#Using the library dplyr filtering the data 
#loading the library 
library(dplyr)
dde1= Data%>%select(country,year,population,gdp,coal_electricity,coal_production,coal_consumption)
dde1 <- dde1[complete.cases(dde1),]%>% filter(year >= 1990 & year <=2018) %>% filter()
#dde1[!(apply(dde1, 1, function(y) any(y == 0))),]
#View(dde1[!complete.cases(dde1),])
View(dde1)
summary(dde1)
str(dde1)
dde <- dde1[,-1:-2] #removing  both Country and year column
View(dde)

#Normalizing the values 
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

dde_norm <- as.data.frame(lapply(dde, normalize))
dde_norm <- cbind(dde1[,2],dde_norm)
colnames(dde_norm)[1]<-("Year")
summary(dde_norm)
View(dde_norm)

#selecting the random test and train values
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

dde_model <- neuralnet(coal_consumption~., data = dde_train[,-1])
dde_model

?neuralnet

plot(dde_model)

library(ggplot2)
ggplot(dde1, aes(x = year, y = coal_consumption)) +
  geom_line(color = "red") +
  geom_point(color = "blue") +
  scale_x_continuous(breaks = seq(1990, 2018, by = 4)) +
  scale_y_continuous(breaks = seq(2000, 6000, by = 500)) +
  ggtitle("Coal Consumption - United States") +
  xlab("Year") +
  ylab("Annual Coal Consumption")

  
  
# model results

model_results <- compute(dde_model,dde_test[,-6])
model_results 

(predicted_cons <- model_results$net.result)
cor(dde_test$coal_consumption, predicted_cons)


VALIDATION=table(dde_test[,6],predicted_cons)
(ACCURACY=sum(diag(VALIDATION))/sum(VALIDATION)*100)

#Actual vs Predicted Graph
data_mod <- data.frame(Year = dde_test[,1],Predicted = predicted_cons,
                       Actual = dde_test[,6])
View(data_mod)

View(dde_test)
summary(dde_test)

dde_test_0 <- dde_test %>% filter(coal_consumption >0.03)
View(dde_test_0)
summary(dde_test_0)

## Double-line Chart 

library(plotly)
(ggplotly(Line<- ggplot(dde_test_0
             , aes(Year,coal_consumption)) +
  geom_point() +
  #scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, by = 1))
   
  #geom_line(aes(y = Actual, color = "Actual"),size = 1.5) +
  #geom_point(aes(y = coal_consumption,color = "Predicted"),size = 1.5)+
  #scale_y_continuous(breaks = seq(0, 14,by = 1)) +
  #scale_color_manual(values = c("Actual" = "red", "Predicted" = "blue"))+
  scale_x_continuous(breaks = seq(1994, 2018, by = 4)) +
  xlab("Year") +
  ylab("Coal Consumption") +
  ggtitle("Year Wise Prediction")))

nrow(dde1)

# Pie - Chart 

#percentages <- round(predicted_cons / sum(predicted_cons) * 100)
#data <- data.frame(dde_test[,1],percentages)
#View(data)

# Create the plot
#Pie<-ggplot(data, aes(x = "", y = percentages, fill = as.factor(dde_test[,1]))) +
 # geom_bar(stat = "identity", width = 1) +
  #coord_polar("y", start = 0) +
  #theme_void() +
  #theme(legend.position = "right",plot.title = element_text(size = 16, face = "bold", color = "violet",hjust = 0.5)) +
  #geom_text(aes(label = paste(percentages, "%")), position = position_stack(vjust = 0.5)) +
  #scale_fill_discrete(name = "Year") + 
  #labs(title = "Year-wise Predicted Values")
#Pie


#predicted year - year wise
#actual year wise
#actual vs 
