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
test <- read_csv(test_filename, 
                 col_types = cols(`Glazing Distr` = col_double(), 
                                  ID = col_skip(), Orientation = col_double()))

#dimensions
train_row = dim(train)[1]
test_row= dim(test)[1]


intercept=matrix(1,train_row,1)
test_intercept=matrix(1,test_row,1)


train_data=as.data.frame(scale(train[-c(9)],scale= TRUE))
test_data=as.data.frame(scale(test_row,scale= TRUE))



#EDIT WHAT df = IN ORDER TO RUN ALL OF THE MODELS 
df=as.data.frame(cbind(intercept,train_data,train[9]))


#not-scaled
#df=as.data.frame(cbind(test_row,test_data)))

#11a)
#create testing  & training data
n = dim(df)[1]
p = dim(df)[2]
train_sample = sample(c(TRUE,FALSE), n, rep=TRUE)
test_sample = (!train_sample)

train_sample = df[train_sample,]
test_sample = df[test_sample,]

# Full Model (Linear)
m = lm( Outcome ~ ., data=test_sample)

Y_hat = predict( m, newdata=test_sample)
MSE_linear = mean( ( test_sample$Outcome - Y_hat )^2 )
#MSE is:
print(MSE_linear)

# Ridge regression: 
#Y = df$Outcome 
model_matrix = model.matrix( Outcome ~ ., data=df )
cv.out = cv.glmnet( model_matrix, df$Outcome, alpha=0 )
plot(cv.out) 
lam_best = cv.out$lambda.1se
#CV Ridge Regression best value of lambda:
print(lam_best)

ridge_model = glmnet( model_matrix, df$Outcome, alpha=0 )

Y_hat = predict( ridge_model, s=lam_best, newx=model.matrix( Outcome ~ ., data=test_sample ) )
MSE_ridge =mean( ( test_sample$Outcome - Y_hat )^2 ) 
#Ridge Regression MSE
print(MSE_ridge)


# The Lasso: 
cv.out = cv.glmnet( model_matrix, df$Outcome, alpha=1 )
plot( cv.out ) 
lam_best = cv.out$lambda.1se
#Lasso CV best lambda value
print( lam_best )

lasso_model = glmnet( model_matrix, df$Outcome, alpha=1 )

Y_hat = predict( lasso_model, s=lam_best, newx=model.matrix( Outcome ~ ., data=test_sample ) )
MSE_lasso =mean( ( test_sample$Outcome - Y_hat )^2 )
#Lasso MSE:
print(MSE_lasso)

#Lasso Coefficients:
print( predict( lasso_model, type="coefficients", s=lam_best ) )

# Principle Component Regression:

pcr_model = pcr( Outcome ~ ., data=df, validation="CV" )
validationplot( pcr_model, val.type="MSEP" ) 
#using 3 predictors
ncomp = 3
Y_hat = predict( pcr_model, test_sample, ncomp=ncomp )
MSE_pcr = mean( ( test_sample$Outcome - Y_hat )^2 )
#PCR MSE
print(MSE_pcr)

# Paritial Least Squares: 
pls_model = plsr( Outcome ~ ., data=df, validation="CV" )

validationplot( pls_model, val.type="MSEP" ) 
#5 predictors
ncomp=5
Y_hat = predict( pls_model, test_sample, ncomp=ncomp )
MSE_pls = mean(( test_sample$Outcome - Y_hat )^2) 
#PLS MSE:
print (MSE_pls)

names= c("linear","ridge","lasso","PCR","PLS")
error_data=data.frame(CV_Name=names,MSE=c(MSE_linear,MSE_ridge,MSE_lasso,MSE_pcr,MSE_pls))
error_data


