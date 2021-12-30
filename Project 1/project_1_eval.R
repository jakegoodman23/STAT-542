data <- read.csv("Ames_data.csv")
testIDs <- read.table("project1_testIDs.dat")
for(j in (1:10)){
  train <- data[-testIDs[,j], ]
  test <- data[testIDs[,j], ]
  test.y <- test[, c(1, 83)]
  test <- test[, -83]
  write.csv(train,"train.csv",row.names=FALSE)
  write.csv(test, "test.csv",row.names=FALSE)
  write.csv(test.y,"test_y.csv",row.names=FALSE)
  source("mymain.R")
  print(j)
  print("lasso error")
  pred <- read.csv("mysubmission1.txt")
  names(test.y)[2] <- "True_Sale_Price"
  pred <- merge(pred, test.y, by="PID")
  lasso_err = sqrt(mean((log(pred$Sale_Price) - log(pred$True_Sale_Price))^2))
  print(lasso_err)
  print("xgboost error")
  pred <- read.csv("mysubmission2.txt")
  names(test.y)[2] <- "True_Sale_Price"
  pred <- merge(pred, test.y, by="PID")
  xgboost_err = sqrt(mean((log(pred$Sale_Price) - log(pred$True_Sale_Price))^2))
  print(xgboost_err)
}