# evaluation code
source("mymain.R")
train = readr::read_csv('train_ini.csv')
test = readr::read_csv('test.csv')

# save weighted mean absolute error WMAE
num_folds = 10
wae = rep(0, num_folds)

start_time = Sys.time()

for (t in 1:num_folds) {
  # *** THIS IS YOUR PREDICTION FUNCTION ***
  test_pred = mypredict(t)
  # load fold file 
  fold_file = paste0('fold_', t, '.csv')
  new_train = readr::read_csv(fold_file, 
                              col_types = cols())
  
  train = train %>% add_row(new_train)
  # extract predictions matching up to the current fold
  scoring_tbl = new_train %>% 
    left_join(test_pred, by = c('Date', 'Store', 'Dept'))
  
  
  # compute WMAE
  actuals = scoring_tbl$Weekly_Sales
  
  preds = scoring_tbl$Weekly_Pred
  preds[is.na(preds)] = 0
  weights = if_else(scoring_tbl$IsHoliday, 5, 1)
  wae[t] = sum(weights * abs(actuals - preds)) / sum(weights)
}

end_time = Sys.time()
script_time = as.numeric(difftime(end_time, start_time, units = "secs"))

print(wae)
print(mean(wae))