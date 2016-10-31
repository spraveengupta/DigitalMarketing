# Reads the data generated from Python
data<-read.csv("updated_data.csv")

if(!require(caret))
  install.packages("caret")
require(caret)

if(!require(rpart.plot))
  install.packages("rpart.plot")
require(rpart.plot)

if(!require(Metrics))
  install.packages("Metrics")
require(Metrics)

if(!require(randomForest))
  install.packages("randomForest")
require(randomForest)

if(!require(car))
  install.packages("car")
require(car)

set.seed(111)

# Splits the given data into train and test to assess final results
inTrain=createDataPartition(y=data$read_rate,
                            p=0.7,
                            list = FALSE)
train <- data[inTrain,]
test <- data[-inTrain,]

attach(train)


# Excluding the id variables and redundant variables such as id, from_domain_hash
f <- read_rate ~ campaign_size + unique_user_cnt + avg_domain_inbox_rate + avg_domain_read_rate +
  avg_user_avg_read_rate + avg_user_domain_avg_read_rate + mb_superuser + mb_engper + mb_supersub +
  mb_engsec + mb_inper + mb_insec + mb_unengsec + mb_idlesub + Collapsed_Domain_extension_0 +
  Collapsed_Domain_extension_1 + Collapsed_Domain_extension_2 +Collapsed_Domain_extension_3 +
  Collapsed_Domain_extension_4 + Collapsed_Domain_extension_5 +Collapsed_Domain_extension_6 +
  Collapsed_Domain_extension_7 + Collapsed_Domain_extension_8 +Collapsed_Domain_extension_9 +
  Collapsed_Domain_extension_10 + day_0 + day_1 + day_2 + day_3 + day_4 + day_5

# performs 5-Fold Cross Validation
train_control<- trainControl(method="cv", number=5, savePredictions = TRUE)

####################################Multi Collinearity####################################

# Checks the Variance Inflation Factor (VIF) of all the variables
vif(lm(f,data=train))

# Removing the variables which have cutoff greater than 4 removing one at a time
f <- read_rate ~ campaign_size + unique_user_cnt + avg_domain_inbox_rate + avg_domain_read_rate +
  avg_user_domain_avg_read_rate + mb_superuser + mb_engper + mb_supersub +
  mb_engsec + mb_inper + mb_insec + mb_unengsec + mb_idlesub + Collapsed_Domain_extension_0 +
  Collapsed_Domain_extension_1 + Collapsed_Domain_extension_2  +
  Collapsed_Domain_extension_4 + Collapsed_Domain_extension_5 +Collapsed_Domain_extension_6 +
  Collapsed_Domain_extension_7 + Collapsed_Domain_extension_8 +Collapsed_Domain_extension_9 +
  Collapsed_Domain_extension_10 + day_0 + day_1 + day_2 + day_3 + day_4 + day_5

# VIFs of variables after removing highly correlated variables
vif(lm(f,data=train))

######
# All the model that are built, follow two major steps -
# 1. Fit the model using Caret package and optimize
# 2. Fit the model with the parameters that are identified from step 1
######

####################################Linear Model####################################
model_lm<- train(f, data=train,
                 trControl=train_control,
                 method="lm")
summary(model_lm)
print(model_lm)                                     # CV:RMSE = 0.0891, R-Sq = 0.7086

# To check performance on the train data
pred_train_lm  <- predict(model_lm, newdata=train) 
rmse(train$read_rate,pred_train_lm)                 # Train: RMSE = 0.08898



# To check performance of best linear model on test data set 
pred_test_lm  <- predict(model_lm, newdata=test) 
rmse(test$read_rate,pred_test_lm)                  # Test: RMSE = 0.08919

####################################Decision Tree####################################
#Decision Tree using caret
treegrid<- expand.grid(cp=seq(0,0.1,0.01))

model_dt<- train(f, data=train,
                 trControl=train_control, 
                 method="rpart", 
                 tuneGrid=treegrid,
                 maxdepth=3)
print(model_dt)

# Parameter tuning
# Used Grid search iterated several times to obtain the best hyper parameters.
# Iteration 1 : Choosing 0.01 as 'cp' whose CV RMSE = 0.0903(best)
# Iteration 2 : Observed the depth was 4 in earlier iteration, reduced it
#               to 3, CV RMSE = 0.0902 (best)

final_tree<-rpart(f, data=train,
                  control = rpart.control(cp=0.01,maxdepth = 3))
prp(final_tree)          #To print the Tree plot
summary(final_tree)      #Gets the detailed description of every split
print(final_tree)        

# To check performance on the train data
pred_train_dt  <- predict(final_tree, newdata=train) 
rmse(train$read_rate,pred_train_dt)                             #train RMSE = 0.09293

# To check performance of best Decision Tree model on test data set 
pred_test_dt  <- predict(final_tree, newdata=test) 
rmse(test$read_rate,pred_test_dt)                  # Test: RMSE = 0.094076


####################################Random Forest####################################
ptm <- Sys.time()
rfgrid<- expand.grid(mtry = seq(10,20,2))
                 
model_rf<- train(f, data=train, 
                 trControl=train_control, nodesize=1000,
                 method="rf", ntree=10, maxnodes=15, tuneGrid=rfgrid,
                 numthreads=4)
print (model_rf)                        # CV RMSE = 0.08206

print (Sys.time()-ptm)
# Observed that mtry = 18 gave best results.  using random forest package in order to
# obtain the variable importance plots

final_rf <- randomForest(f,data = train,
                         ntree=10,mtry=18, 
                         nodesize=1000, maxnodes=15,
                         numthreads=4)
summary(final_rf)      #Gets the detailed description of every split
print(final_rf) 

varImpPlot(final_rf)

# To check performance on the train data
pred_train_rf  <- predict(final_rf, newdata=train) 
rmse(train$read_rate,pred_train_rf)                             #train RMSE = 0.08504 


# To check performance of best RandomForest model on test data set 
pred_test_rf  <- predict(final_rf, newdata=test) 
rmse(test$read_rate,pred_test_rf)                              # Test: RMSE = 0.08303

####################################Model Metrics####################################
modelmetrics <-
  function(a,m){
    metrics <- c(MAD=0,RMSE=0,R2=0,TMAD=0,p90=0)
    metrics["MAD"] <- mean(abs(a-m))
    metrics["RMSE"] <- sqrt(mean((a-m)^2))
    SST= sum((a-mean(a))^2)
    SSE= sum((a-m)^2)
    metrics["R2"]= 1- (SSE/SST)
    metrics["TMAD"]= mean(abs(a-m),tri=0.05)
    metrics["p90"]= quantile(abs(a-m),probs = 0.9)
    return(metrics)
  }

modelmetrics(train$read_rate,pred_train_rf)
modelmetrics(train$read_rate,pred_train_dt)
modelmetrics(train$read_rate,pred_train_lm)

