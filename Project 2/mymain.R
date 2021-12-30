library(lubridate)
library(tidyverse)

# svd preprocessing function
preprocess.svd <- function(train, n.comp){
  train <- train %>%
    select(Store, Dept, Date, Weekly_Sales) %>%
    spread(Date, Weekly_Sales)
  train[is.na(train)] <- 0
  
  train_svd = NULL
  
  for(mydept in unique(train$Dept)){
    dept_data <- train %>% 
      filter(Dept == mydept)
    
    if (nrow(dept_data) > n.comp){
      tmp_data <- dept_data[, -c(1,2)]
      store_means <- rowMeans(tmp_data)
      tmp_data <- tmp_data - store_means
      z <- svd(tmp_data, nu=n.comp, nv=n.comp)
      s <- diag(z$d[1:n.comp])
      tmp_data <- z$u %*% s %*% t(z$v) + store_means
      tmp_data[tmp_data < 0] <- 0
      dept_data[, -c(1:2)] <- z$u %*% s %*% t(z$v) + store_means
    }
    train_svd = rbind(train_svd, dept_data)
  }
  train_svd <- train_svd %>% 
    gather(Date, Weekly_Sales, -Store, -Dept)
  return(train_svd)
}

# our prediction function

mypredict = function(t){
  
  # have the test dataset be the next 2 months of data
  start_date <- ymd("2011-03-01") %m+% months(2 * (t - 1))
  end_date <- ymd("2011-05-01") %m+% months(2 * (t - 1))
  test_current <- test %>%
    filter(Date >= start_date & Date < end_date) %>%
    select(-IsHoliday)  
  # find the unique pairs of (Store, Dept) combo that appeared in both training and test sets
  train = preprocess.svd(train,8)
  train_pairs <- train[, 1:2] %>% count(Store, Dept) %>% filter(n != 0)
  test_pairs <- test_current[, 1:2] %>% count(Store, Dept) %>% filter(n != 0)
  unique_pairs <- intersect(train_pairs[, 1:2], test_pairs[, 1:2])
  
  # pick out the needed training samples, convert to dummy coding, then put them into a list
  train_split <- unique_pairs %>% 
    left_join(train, by = c('Store', 'Dept')) %>% 
    mutate(Wk = factor(ifelse(year(Date) == 2010, week(Date) - 1, week(Date)), levels = 1:52)) %>% 
    mutate(Yr = year(Date))
  train_split = as_tibble(model.matrix(~ Weekly_Sales + Store + Dept + Yr + Wk, train_split)) %>% group_split(Store, Dept)
  
  # do the same for the test set
  test_split <- unique_pairs %>% 
    left_join(test_current, by = c('Store', 'Dept')) %>% 
    mutate(Wk = factor(ifelse(year(Date) == 2010, week(Date) - 1, week(Date)), levels = 1:52)) %>% 
    mutate(Yr = year(Date))
  test_split = as_tibble(model.matrix(~ Store + Dept + Yr + Wk, test_split)) %>% mutate(Date = test_split$Date) %>% group_split(Store, Dept)
  
  # pre-allocate a list to store the predictions
  test_pred <- vector(mode = "list", length = nrow(unique_pairs))
  
  # perform regression for each split, note we used lm.fit instead of lm
  for (i in 1:nrow(unique_pairs)) {
    tmp_train <- train_split[[i]]
    tmp_test <- test_split[[i]]
    
    mycoef <- lm.fit(as.matrix(tmp_train[, -(2:4)]), tmp_train$Weekly_Sales)$coefficients
    mycoef[is.na(mycoef)] <- 0
    tmp_pred <- mycoef[1] + as.matrix(tmp_test[, 4:55]) %*% mycoef[-1]
    
    test_pred[[i]] <- cbind(tmp_test[, 2:3], Date = tmp_test$Date, Weekly_Pred = tmp_pred[,1])
  }
  
  # turn the list into a table at once, 
  # this is much more efficient then keep concatenating small tables
  test_pred <- bind_rows(test_pred)
  return(test_pred)
}

