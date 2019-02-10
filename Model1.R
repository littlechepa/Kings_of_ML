
required_packages <- c("caret", "Metrics","dplyr","tidyr","stringr","rpart","ipred","randomForest","gbm","e1071", "dummies")

not_installed_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

if(length(not_installed_packages) == 0){
  "All required packages are installed"
} else {
  for(i in 1:length(not_installed_packages)){
    install.packages(not_installed_packages[i])
  }
}

for(i in 1:length(required_packages )){
  library(required_packages[i], character.only = T)
}
####################################################
train9d <- read.csv(file.choose()) #Read train data
test9d <- read.csv(file.choose()) #Read test data

train1d <- read.csv(file.choose())
test1d <- read.csv(file.choose())

herod <- read.csv(file.choose()) #Read hero data
###################################################

tr9d <- train9d[,-c(3,5)] #Delete common id and num_wins columns from train data
ts9d <- test9d[,-c(3,5)] #Delete common id and num_wins columns from test data
ts1d <- test1d[,-c(3,5)]
tr1d <- train1d[,-c(3,5)]

#Transform hero data. Here, creating dummy variables for hero roles as they can be used in model building
trhrd <- herod %>%
  select(hero_id, roles) %>%
  mutate(role = str_split(roles,":")) %>%
  select(hero_id,role) %>%
  unnest() %>%
  spread(role,role)

trhrd[is.na(trhrd)] <- 0
trhrd[trhrd != 0] <- 1
trhrd$hero_id <- herod$hero_id

trhrd <- sapply(trhrd,as.integer)
atkr_type  <- as.data.frame(trhrd)
##############################

hero_stat <- herod[,-c(4,5,7,8,10,21)]
hero_stat$attack_type <- ifelse(hero_stat$attack_type == "Melee",1,0)

hero_stat <- cbind(hero_stat,dummy(hero_stat$primary_attr, sep = "_")) #Here, dummy function from dummies package creates dummy variables
hero_stat <- hero_stat[,-c(2, ncol(hero_stat))] #Deleting this column because already created dummy variables with this column levels

hero_all_trsf <- left_join(atkr_type, hero_stat, by = "hero_id")
###################################################
tr9d_atkr_type <- left_join(tr9d,atkr_type, by = "hero_id")
tr9d_hero_stat <-  left_join(tr9d,hero_stat, by = "hero_id")
tr9d_all <- left_join(tr9d, hero_all_trsf, by = "hero_id")

ts9d_atkr_type <- left_join(ts9d,atkr_type, by = "hero_id")
ts9d_hero_stat <-  left_join(ts9d,hero_stat, by = "hero_id")
ts9d_all <- left_join(ts9d, hero_all_trsf, by = "hero_id")
###################################################

features_type <- c("user_id*hero_id",colnames(tr9d_atkr_type)[-c(1,2,4)])
features_stat <- c("user_id*hero_id",colnames(tr9d_hero_stat)[-c(1,2,4)])
features_all <- c("user_id*hero_id",colnames(tr9d_all)[-c(1,2,4)])
formula_type <- as.formula(paste("kda_ratio~ ", paste(features_type, collapse= "+")))
formula_stat <- as.formula(paste("kda_ratio~ ", paste(features_stat, collapse= "+")))
formula_all <- as.formula(paste("kda_ratio~ ", paste(features_all, collapse= "+")))
#@@@@@@@@@@@@@@@@@
trdwf <- tr9d_all
tsdwf <- ts9d_all

trdwf$user_id <- as.factor(trdwf$user_id)
trdwf$hero_id <- as.factor(trdwf$hero_id)
tsdwf$user_id <- as.factor(tsdwf$user_id)
tsdwf$hero_id <- as.factor(tsdwf$hero_id)
#@@@@@@@@@@@@@@@@@

#################################
model <- lm(kda_ratio ~ ., trdwf)

pred <- predict(model, tsdwf[-1])

rmse <- rmse(tsdwf$kda_ratio,pred)

rmse
#@@@@@@@@@@@@

###################################

###################################

model2 <- train(kda_ratio ~ ., data = tr9d, method = "lm", trControl = trainControl(method = "cv", number = 10, verboseIter = T))

pred2 <- predict(model2, ts9d)

rmse2 <- rmse(ts9d$kda_ratio,pred2)
rmse2
###################################

model3 <- rpart(kda_ratio ~ . , data = trdwf)

pred3 <- predict(model3, tsdwf)

rmse3 <- rmse(tsdwf$kda_ratio,pred3)

rmse3

################################Tuning of decision trees for best model################################
set.seed(1)

# Establish a list of possible values for minsplit and maxdepth
minsplit <- seq(1, 30, 1)
maxdepth <- seq(1,30 , 3)

#Using grid search to tyne hyper parameters
# Create a data frame containing all combinations 
hyper_grid <- expand.grid(minsplit = minsplit, maxdepth = maxdepth)

# Check out the grid
head(hyper_grid)

# Print the number of grid combinations
nrow(hyper_grid)

# Number of potential models in the grid
num_models <- nrow(hyper_grid)

# Create an empty list to store models
grade_models <- list()

# Write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:num_models) {
  
  # Get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  # Train a model and store in the list
  grade_models[[i]] <- rpart(formula = kda_ratio ~ ., 
                             data = tr9d, 
                             minsplit = minsplit,
                             maxdepth = maxdepth)
}


# Number of potential models in the grid
num_models <- length(grade_models)

# Create an empty vector to store AUC values
rmse_values <- c()

# Write a loop over the models to compute validation AUC
for (i in 1:num_models) {
  
  # Retreive the i^th model from the list
  model <- grade_models[[i]]
  
  # Generate predictions on grade_valid 
  pred <- predict(object = model,
                  newdata = ts9d)
  
  # Compute validation AUC and add to the 
  rmse_values[i] <- rmse(actual = ts9d$kda_ratio, 
                       predicted = pred)
}

# Identify the model with highest AUC value
which.min(rmse_values)
min(rmse_values)
dt_model <- grade_models[[which.min(rmse_values)]]

# Print the model paramters of the best model
dt_model$control

# Compute test set AUC on best_model
dt_pred <- predict(object = dt_model,
                   newdata = )
dt_auc <- auc(actual = od_test$Dropout, 
              predicted = dt_pred)

dt_auc
################################ Tuning of bagging for best model################################ 
set.seed(1)

# Train a bagged model
bag_model <- bagging(formula = kda_ratio ~ ., 
                     data = trdwf[-1])

# Generate predictions on the test set
bag_pred <- predict(bag_model,
                    tsdwf[-1])

# Compute the AUC
bag_rmse <- rmse(actual = tsdwf$kda_ratio, 
               predicted = bag_pred)  

bag_rmse


################################ Tuning of GBM for best model################################ 
set.seed(1)
gbm_model <- gbm(formula = kda_ratio ~ ., 
                 distribution = "gaussian", 
                 data = trdwf[-1],
                 n.trees = 500,
                 cv.folds = 5)

# Optimal ntree estimate based on CV
ntree_opt_cv <- gbm.perf(object = gbm_model, 
                         method = "cv")

print(paste0("Optimal n.trees (CV Estimate): ", ntree_opt_cv))

# Generate predictions on the test set using ntree_opt_cv number of trees
gbm_pred <- predict(object = gbm_model, 
                    newdata = tsdwf,
                    n.trees = ntree_opt_cv)   

# Generate the test set rmse
gbm_rmse <- rmse(actual = tsdwf$kda_ratio, predicted = gbm_pred )
gbm_rmse

################################ Tuning of glmnet for best model################################ 
model <- train(kda_ratio ~ .,trdwf,
               tuneGrid = expand.grid(alpha = 0:1, lambda = seq(0.0001,1,length = 20)) ,
               method = "glmnet",
              
               trControl = trainControl(
                 method = "cv", number = 10,
                 verboseIter = TRUE),
               preProcess = "pca"
               )

rmse(tsdwf$kda_ratio, predict(model, tsdwf))


##########################
lm_model <- lm(kda_ratio ~ ., train9d[,-c(3,5)])
lm_pred <- predict(lm_model, train1d[,-c(3,5)])
rmse(train1d$kda_ratio, lm_pred)

