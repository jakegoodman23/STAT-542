# Authors: Jake Goodman (Net ID: jakeg5) & Michael McClanahan (Net ID: mjm31)

# set seed
set.seed(0531)

# load libraries
library(glmnet)
library(xgboost)



# Function that removes columns provided a data frame and column names to be removed
remove_vars = function(df, cols){
  df = df[, !names(df) %in% cols]
  
  return(df)
}

# Perform winsorization on `train` and `test` data
winsorization = function(df){
  winsor.vars <- c("Lot_Frontage", "Lot_Area", "Mas_Vnr_Area", "BsmtFin_SF_2", "Bsmt_Unf_SF", "Total_Bsmt_SF", "Second_Flr_SF", 'First_Flr_SF', "Gr_Liv_Area", "Garage_Area", "Wood_Deck_SF", "Open_Porch_SF", "Enclosed_Porch", "Three_season_porch", "Screen_Porch", "Misc_Val")
  
  quan.value <- 0.95
  for(var in winsor.vars){
    tmp <- df[, var]
    myquan <- quantile(tmp, probs = quan.value, na.rm = TRUE)
    tmp[tmp > myquan] <- myquan
    df[, var] <- tmp
  }
  
  return(df) 
}

# Function to create dummy variables for categorical variables on `train` and `test` data
dummy_cat_vars = function(df){
  categorical.vars <- colnames(df)[
    which(sapply(df,
                 function(x) mode(x)=="character"))]
  df.matrix <- df[, !colnames(df) %in% categorical.vars, 
                  drop=FALSE]
  n.df <- nrow(df.matrix)
  for(var in categorical.vars){
    mylevels <- sort(unique(df[, var]))
    m <- length(mylevels)
    m <- ifelse(m>2, m, 1)
    tmp.df <- matrix(0, n.df, m)
    col.names <- NULL
    for(j in 1:m){
      tmp.df[df[, var]==mylevels[j], j] <- 1
      col.names <- c(col.names, paste(var, '_', mylevels[j], sep=''))
    }
    colnames(tmp.df) <- col.names
    df.matrix <- cbind(df.matrix, tmp.df)
  }
  
  return(df.matrix)
}

# Function to match up `train` and `test` columns
match_up_test_cols = function(train.matrix, test.matrix){
  train_cols = colnames(train.matrix)
  test_cols = colnames(test.matrix)
  remove_cols_test = colnames(test.matrix[ , !(names(test.matrix) %in% train_cols)])
  add_cols_test = colnames(train.matrix[ , !(names(train.matrix) %in% test_cols)])
  
  test.matrix = test.matrix[ , !(names(test.matrix) %in% remove_cols_test)]
  
  test.matrix[add_cols_test] = 0
  
  test.matrix = test.matrix[,train_cols]
  
  return(test.matrix)
}

# Function to run a lasso/ridge model with `train` data
train_model_lr = function(train.matrix, train.y){
  cv.out <- cv.glmnet(as.matrix(train.matrix), train.y, alpha = 1)
  sel.vars <- predict(cv.out, type="nonzero", 
                      s = cv.out$lambda.min)$s1
  cv.out <- cv.glmnet(as.matrix(train.matrix[, sel.vars]), 
                      train.y, alpha = 0)
  
  return(list(model = cv.out,vars = sel.vars))
  
}

# Function that takes in a lasso/ridge model object and makes predictions on the `test` dataset
make_pred_lr = function(cv.out, test.matrix, sel.vars, test){
  tmp = predict(cv.out, s = cv.out$lambda.min, 
                newx = as.matrix(test.matrix[, sel.vars]))
  pred_out = cbind(test$PID, exp(tmp))
  colnames(pred_out) = c("PID","Sale_Price")
  write.table(pred_out, "mysubmission1.txt", sep = ",", row.names = FALSE)
}

# Function to train xgboost model with `train` data
train_model_xgb = function(train.matrix, train.y){
  xgb.model = xgboost(data = as.matrix(train.matrix), 
                      label = train.y, max_depth = 6,
                      eta = 0.05, nrounds = 5000,
                      subsample = 0.5,
                      verbose = TRUE)
  
  return(xgb.model)
}

# Function to make predictions on test data from xgboost model
make_pred_xgb = function(xgb.model, test.matrix, test){
  tmp = predict(xgb.model, data.matrix(test.matrix))
  
  pred_out = cbind(test$PID, exp(tmp))
  colnames(pred_out) = c("PID","Sale_Price")
  write.table(pred_out, "mysubmission2.txt", sep = ",", row.names = FALSE)
}

# load train data
train = read.csv("train.csv",stringsAsFactors = FALSE)



# fill null instances with a value of 0
train$Garage_Yr_Blt[is.na(train$Garage_Yr_Blt)] = 0


# process train data
train.x = remove_vars(train, c('Street', 'Utilities', 'Condition_2', 'Roof_Matl', 'Heating', 'Pool_QC', 'Misc_Feature', 'Low_Qual_Fin_SF', 'Pool_Area', 'Longitude','Latitude', 'PID', 'Sale_Price'))

train.y = log(train$Sale_Price)

train.x = winsorization(train.x)

train.matrix = dummy_cat_vars(train.x)

# train lasso/ridge model on train split
# model object and selected variables are returned from train_model_lr

trained_model_lr_list = train_model_lr(train.matrix, train.y)
trained_model_lr = trained_model_lr_list$model
sel.vars.lr = trained_model_lr_list$vars

# train xgboost model on train split
# model object  are returned from train_model_xgb

trained_model_xgb = train_model_xgb(train.matrix, train.y)

# load test data
test = read.csv("test.csv",stringsAsFactors = FALSE)

# fill null instances with a value of 0
test$Garage_Yr_Blt[is.na(test$Garage_Yr_Blt)] = 0

# process test data
test.x = remove_vars(test, c('Street', 'Utilities', 'Condition_2', 'Roof_Matl', 'Heating', 'Pool_QC', 'Misc_Feature', 'Low_Qual_Fin_SF', 'Pool_Area', 'Longitude','Latitude', 'PID'))

test.x = winsorization(test.x)

test.matrix = dummy_cat_vars(test.x)

test.matrix = match_up_test_cols(train.matrix, test.matrix)

# make predictions on test split from trained lasso/ridge model
make_pred_lr(trained_model_lr, test.matrix, sel.vars.lr, test)          

# make predictions on test split from trained xgboost model
make_pred_xgb(trained_model_xgb, test.matrix, test)


