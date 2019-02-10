
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
ts9d_atkr_type <- left_join(ts9d,atkr_type, by = "hero_id")
ts9d_hero_stat <-  left_join(ts9d,hero_stat, by = "hero_id")
ts9d_all <- left_join(ts9d, hero_all_trsf, by = "hero_id")

ts1d_atkr_type <- left_join(ts1d,atkr_type, by = "hero_id")
ts1d_hero_stat <-  left_join(ts1d,hero_stat, by = "hero_id")
ts1d_all <- left_join(ts1d, hero_all_trsf, by = "hero_id")
###################################################

#@@@@@@@@@@@@@@@@@
t9dwf <- ts9d_all
t1dwf <- ts1d_all

t9dwf$user_id <- as.factor(t9dwf$user_id)
t9dwf$hero_id <- as.factor(t9dwf$hero_id)
t1dwf$user_id <- as.factor(t1dwf$user_id)
t1dwf$hero_id <- as.factor(t1dwf$hero_id)
#@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@
t9dwfa <- tr9d_atkr_type
t1dwfa <- tr1d_atkr_type

t9dwfa$user_id <- as.factor(t9dwfa$user_id)
t9dwfa$hero_id <- as.factor(t9dwfa$hero_id)
t1dwfa$user_id <- as.factor(t1dwfa$user_id)
t1dwfa$hero_id <- as.factor(t1dwfa$hero_id)
#@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@
t9dwfs <- tr9d_hero_stat
t1dwfs <- tr1d_hero_stat

t9dwfs$user_id <- as.factor(t9dwfs$user_id)
t9dwfs$hero_id <- as.factor(t9dwfs$hero_id)
t1dwfs$user_id <- as.factor(t1dwfs$user_id)
t1dwfs$hero_id <- as.factor(t1dwfs$hero_id)
#@@@@@@@@@@@@@@@@@
#################################
lm_model <- lm(kda_ratio ~ ., t9dwf)

lm_predictions <- predict(lm_model, t1dwf)

write.csv(lm_predictions,"lm_predictions.csv")



rmse <- rmse(t1dwf$kda_ratio,pred)

rmse
###############
set.seed(1)

# Train a bagged model
bag_model <- bagging(formula = kda_ratio ~ ., 
                     data = t9dwf)

# Generate predictions on the test set
bag_predictions <- predict(bag_model,
                    t1dwf)
write.csv(bag_predictions, "bag_predictions.csv")

# Compute the AUC
bag_rmse <- rmse(actual = t1dwf$kda_ratio, 
                 predicted = bag_pred)  

bag_rmse

##########################
################################ Tuning of GBM for best model################################ 
set.seed(1)
gbm_model <- gbm(formula = kda_ratio ~ ., 
                 distribution = "gaussian", 
                 data = t9dwf,
                 n.trees = 5000,
                 cv.folds = 5)

# Optimal ntree estimate based on CV
ntree_opt_cv <- gbm.perf(object = gbm_model, 
                         method = "cv")

print(paste0("Optimal n.trees (CV Estimate): ", ntree_opt_cv))

# Generate predictions on the test set using ntree_opt_cv number of trees
gbm_predictions <- predict(object = gbm_model, 
                    newdata = t1dwf,
                    n.trees = ntree_opt_cv)   
write.csv(gbm_predictions, "gbm_predictions.csv")

# Generate the test set rmse
#gbm_rmse <- rmse(actual = t1dwf$, gbm_pred = pred)
gbm_auc

################################
