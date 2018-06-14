bt <- read_xlsx("Bitcoin.xlsx")
indx <- sample(1:nrow(bt), as.integer(0.9*nrow(bt)))
indx

launch_train <- bt[indx,]
launch_test <- bt[-indx,]
cor(bt[c("marketcap", "exchangeVolume", "generatedCoins", "GDPC1","Price")])
pairs(bt[c("marketcap", "exchangeVolume", "generatedCoins", "GDPC1","Price")])
#pairs.pa (bt[c("marketcap", "exchangeVolume", "generatedCoins", "GDPC1","Price")])


ins_model <- lm(Price ~ marketcap + exchangeVolume + generatedCoins + GDPC1, data = launch_train)
ins_Predicted <- predict(ins_model, launch_test)
ins_model <- lm(Price ~ marketcap + exchangeVolume + generatedCoins + GDPC1, data = launch_train)
ins_model
summary(ins_model)
ins_model2 <- lm(Price ~ marketcap + exchangeVolume + generatedCoins + GDPC1 + exchangeVolume*fees, data = launch_train)
summary(ins_model2)



m.rpart <- rpart(Price ~ marketcap + exchangeVolume + generatedCoins + GDPC1, data = launch_train)
m.rpart
library(rpart.plot)
rpart.plot(m.rpart, digits = 3)
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE,
           type = 3, extra = 101)
p.rpart <- predict(m.rpart, launch_test)
summary(p.rpart)
MAE <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

MAE(p.rpart, launch_test$Price)
mean(launch_train$Price)
MAE(920.496, launch_test$Price)

library(RWeka)
Bitcoin_model <- M5P(Price ~ marketcap + exchangeVolume + generatedCoins + GDPC1, data = launch_train)
bitcoin_predicted <-predict(Bitcoin_model, launch_test)
m.m5p <- M5P(Price ~ marketcap + exchangeVolume + generatedCoins + GDPC1, data = launch_train)
m.m5p
summary(m.m5p)
p.m5p <- predict(m.m5p, launch_test)
summary(p.m5p)
cor(p.m5p, launch_test$Price)
MAE(launch_test$Price, p.m5p)
