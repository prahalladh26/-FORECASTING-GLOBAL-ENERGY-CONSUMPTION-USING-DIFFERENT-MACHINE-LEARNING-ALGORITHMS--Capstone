library(Boruta)

Data <- read.csv(file.choose(),header = TRUE)
View(Data)

library(dplyr)
dde= Data %>% select(country,year,population,gdp,renewables_electricity,renewables_cons_change_twh,renewables_share_elec,renewables_share_energy,renewables_consumption) %>% filter(country == "India" & year >= 1985 & year <=2018)
View(dde)
dde <- dde[,-1]
ind <- sample(2, nrow(dde), replace = T, prob = c(0.7, 0.3)) # no clarityyyy
dde_train <- dde[ind == 1,]
dde_test <- dde[ind == 2,]
#dde_train <- dde[1:26,]
#dde_test <- dde[27:34,]
View(dde_test)
boruta_output <- Boruta(renewables_consumption ~ ., data = dde_train, doTrace = 2)
print(boruta_output)
plot(boruta_output, las = 2)
