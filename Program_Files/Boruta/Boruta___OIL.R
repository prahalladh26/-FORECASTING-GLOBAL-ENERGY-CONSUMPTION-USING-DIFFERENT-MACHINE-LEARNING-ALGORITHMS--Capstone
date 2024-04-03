library(Boruta)

Data <- read.csv(file.choose(),header = TRUE)
View(Data)

library(dplyr)
dde= Data %>% select(country,year,population,gdp,oil_cons_change_twh,oil_electricity,oil_prod_change_twh,oil_production,oil_share_energy,oil_share_elec,oil_consumption) %>% filter(country == "India" & year >= 1985 & year<=2018)
View(dde)
dde <- dde[,-1]
ind <- sample(2, nrow(dde_norm), replace = T, prob = c(0.7, 0.3)) # no clarityyyy
dde_train <- dde_norm[ind == 1,]
dde_test <- dde_norm[ind == 2,]
#dde_train <- dde[1:26,]
#dde_test <- dde[27:34,]
View(dde_test)
boruta_output <- Boruta(oil_consumption ~ ., data = dde_train, doTrace = 2)
print(boruta_output)
plot(boruta_output, las = 2)
