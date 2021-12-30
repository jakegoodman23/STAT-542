library(pROC)

# for testing purposes, needing to have source file location so we can easily move back and forth in directory
dir = getwd()

# pre-allocate list to store auc values
auc_vals = rep(0,5)

#pre-allocate list to store script_times
script_times = rep(0,5)

# grab the time the script starts to make calls to the mymain file
start_time = Sys.time()
for(j in 1:5){
  
  iteration_start = Sys.time()
  setwd(dir)
  setwd(paste("split_", j, sep=""))
  
  source("../mymain.R")
  
  test.y = read.table("test_y.tsv", header = TRUE)
  pred = read.table("mysubmission.txt", header = TRUE)
  pred = merge(pred, test.y, by="id")
  roc_obj = roc(pred$sentiment, pred$prob)
  auc_vals[j] = as.numeric(pROC::auc(roc_obj))
  print(auc_vals[j])
  cur_time = Sys.time()
  iteration_time = as.numeric(difftime(cur_time, iteration_start, units = "secs"))
  script_times[j] = iteration_time
  print(iteration_time)
}


# grab the time the script is finished so we can gather the script's elapsed time
setwd(dir)
end_time = Sys.time()
script_time = as.numeric(difftime(end_time, start_time, units = "secs"))

print(script_time)

# write out the script time so we don't lose it
write.table(script_times, file = "script_times.txt", 
            row.names = FALSE, sep='\t')

# write out the auc values so we don't lose it
write.table(auc_vals, file = "auc_values.txt", 
            row.names = FALSE, sep='\t')