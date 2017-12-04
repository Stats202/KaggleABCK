library(readr)
library("glmnet")
library("pls")
set.seed(1)

#Filenames
train_filename= "~/Stats202/Kaggle/train.csv"
test_filename= "~/Stats202/Kaggle/test.csv"



train <- read_csv(train_filename, 
                  col_types = cols(`Glazing Distr` = col_double(), 
                                   ID = col_skip(), Orientation = col_double()))
#test <- read_csv(test_filename, 
#                 col_types = cols(`Glazing Distr` = col_double(), 
#                                  ID = col_skip(), Orientation = col_double()))


test <- read_csv("~/Stats202/Kaggle/test.csv", 
                 col_types = cols(ID = col_skip()), na = "NA")


train_data=as.data.frame(scale(train[-c(9)],scale= TRUE))
test_data=as.data.frame(scale(test[-c(9)],scale= TRUE))

train_df=as.data.frame(cbind(train_data,train[9]))
test_df=as.data.frame(cbind(test_data,test[9]))


#11a)
#create testing  & training data
n = dim(train_df)[1]
p = dim(train_df)[2]
train_sample = sample(c(TRUE,FALSE), n, rep=TRUE)
test_sample = (!train_sample)

train_sample = train_df[train_sample,]
test_sample = train_df[test_sample,]

# Full Model (Linear)

#for full train data= train_df; for subset=test_sample
m = lm( Outcome ~ ., data=train_df)

#for full test= test_df; for subset= test_df
Y_hat = predict( m, newdata=test_df)
submission=as.data.frame(Y_hat)