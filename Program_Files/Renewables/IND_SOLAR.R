Data <- read.csv(file.choose(),header = TRUE)
View(Data)

library(dplyr)
dde1= Data %>% select(country,year,population,gdp,solar_electricity,solar_cons_change_twh,solar_share_elec,solar_share_energy,solar_consumption) %>% filter(country == "India" & year >= 1985 & year <=2018)
View(dde1)
summary(dde1)
str(dde1)
dde <- dde1[,-1:-2] #removed both Country and year column

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

dde_norm <- as.data.frame(lapply(dde, normalize))
summary(dde_norm)
View(dde_norm)


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

dde_model <- neuralnet(solar_consumption~., data = dde_train , hidden = c(2))
dde_model


plot(dde_model)

library(ggplot2)
ggplot(dde1, aes(x = year, y = solar_consumption)) +
  geom_line(color = "red") +
  geom_point(color = "blue") +
  scale_x_continuous(breaks = seq(1985, 2018, by = 3)) +
  scale_y_continuous(breaks = seq(150, 900, by = 50)) +
  ggtitle("Solar Energy Consumption") +
  xlab("Year") +
  ylab("Solar Energy Consumption")



# model results
model_results <- compute(dde_model,dde_test[,-6])
model_results 
(predicted_cons <- model_results$net.result)
cor(dde_test$solar_consumption, predicted_cons)

VALIDATION=table(dde_test[,6],predicted_cons)
(ACCURACY=sum(diag(VALIDATION))/sum(VALIDATION)*100)

data_mod <- data.frame(Year = dde_test[,1],Predicted = predicted_cons,
                       Actual = dde_test[,6])
View(data_mod)
ggplot(data_mod,                                  
       aes(x = Predicted,
           y = Actual)) +
  geom_point(size = 2) +
  geom_abline(intercept = 0,
              slope = 1,
              color = "red",
              size = 1)+
  ggtitle("Solar energy consumption ") +
  xlab("Predicted") +
  ylab("Actual") 
