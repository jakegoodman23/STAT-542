library(text2vec)
library(glmnet)

# set seed based on Jake Goodman's netid
set.seed(6379)

myvocab = scan(file = "myvocab.txt", what = character())

# read in and process train data
train = read.table("train.tsv",
                   stringsAsFactors = FALSE,
                   header = TRUE)
train$review <- gsub('<.*?>', ' ', train$review)
it_train = itoken(train$review,
                  preprocessor = tolower, 
                  tokenizer = word_tokenizer)
vectorizer = vocab_vectorizer(create_vocabulary(myvocab, 
                                                ngram = c(1L, 2L)))
dtm_train = create_dtm(it_train, vectorizer)

# train a Ridge regression model
cv.out = cv.glmnet(x = dtm_train,
                   y = train$sentiment,
                   alpha = 0,
                   family = 'binomial')

# read in and process test data

test = read.table("test.tsv",
                  stringsAsFactors = FALSE,
                  header = TRUE)
test$review <- gsub('<.*?>', ' ', test$review)
it_train = itoken(test$review,
                  preprocessor = tolower, 
                  tokenizer = word_tokenizer)
vectorizer = vocab_vectorizer(create_vocabulary(myvocab, 
                                                ngram = c(1L, 2L)))
dtm_test = create_dtm(it_train, vectorizer)

# make predictions on test data using the trained model from above
pred_output = data.frame(test$id,predict(cv.out,s=cv.out$lambda.min, newx=dtm_test,type="response"))
colnames(pred_output) = c("id","prob")
write.table(pred_output, file = "mysubmission.txt", 
            row.names = FALSE, sep='\t')
