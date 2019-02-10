#################Part-1 : install and load all the required packages#################
#Creating vector with all required packages
required_packages <- c("caret", "Metrics","dplyr","tidyr","stringr","rpart","ipred","randomForest","gbm","e1071", "dummies", "Boruta")

#Create a vector which contains packag names, which are not installed in the machine yet
not_installed_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]

#Check wether all required packages installed, if not, install them
if(length(not_installed_packages) == 0){
  print("All required packages are installed")
} else {
  for(i in 1:length(not_installed_packages)){
    install.packages(not_installed_packages[i])
  }
}

#Load all the required packages for this project
for(i in 1:length(required_packages )){
  library(required_packages[i], character.only = T)
}
#################Part-2 : Load the data, clean, tidy and transform the data#################

train9d <- read.csv(file.choose()) #Train 9 data
train1d <- read.csv(file.choose()) #Train 1 data

test9d <- read.csv(file.choose()) #Test 9 data
test1d <- read.csv(file.choose()) #Test 1 data


herod <- read.csv(file.choose()) #Heroes data



#Common id column does not provide any useful information which can be used in model building
#num_wins column also not important
#Delete common id and num_wins columns from both train and test data as they are not important
train9d <- train9d[,-c(3,4,5)] 
train1d <- train1d[,-c(3,4,5)]
test9d <- test9d[,-c(3,4,5)]
test1d <- test1d[,-c(3,4,5)]

str(train9d)
str(train1d)
str(test9d)
str(test1d)
str(herod)

#Transform hero data. Here, creating dummy variables for hero roles as they can be used in model building
trhrd <- herod %>% #Pipe hero data to dplyr's select function and selecting hero_id and roles column
  select(hero_id, roles) %>% 
  mutate(role = str_split(roles,":")) %>% #Creating new column and separate roles in nest format
  select(hero_id,role) %>%
  unnest() %>% #Unnest all the roles
  spread(role,role) #Tidying. Creating new column for every role by specifying role as both column and key

trhrd[is.na(trhrd)] <- 0 #The above resulted data.frame contains NAs because certain heroes do not have certain roles. Simply put 0 where is a value as NA
trhrd[trhrd != 0] <- 1 #Put 1, if the hero plays the role

trhrd <- sapply(trhrd,as.integer) #Convert all the rows as integers
hero_roles  <- as.data.frame(trhrd)
hero_roles$hero_id <- herod$hero_id #Re-assign the hero_id column

hero_stat <- herod[,-c(4,5,7,8,10,21)] #Removing zero variance columns from hero data
hero_stat$attack_type <- ifelse(hero_stat$attack_type == "Melee",1,0)
hero_stat <- cbind(hero_stat,dummy(hero_stat$primary_attr, sep = "_")) #Here, dummy function from dummies package creates dummy variables
hero_stat <- hero_stat[,-c(2, ncol(hero_stat))] #Deleting this column because already created dummy variables with this column levels


hero_all_trsf <- left_join(hero_roles, hero_stat, by = "hero_id")

tr9d_all <- left_join(train9d, hero_all_trsf, by = "hero_id")
tr1d_all <- left_join(train1d, hero_all_trsf, by = "hero_id")
ts9d_all <- left_join(test9d, hero_all_trsf, by = "hero_id")
ts1d_all <- left_join(test1d, hero_all_trsf, by = "hero_id")


#user_id and hero_id columns are contains with integer data. But should be in factor format.
#Converting both user_id and hero_id columns into factor format
tr9d_all$user_id <- as.factor(tr9d_all$user_id) 
tr1d_all$user_id <- as.factor(tr1d_all$user_id)
tr9d_all$hero_id <- as.factor(tr9d_all$hero_id)
tr1d_all$hero_id <- as.factor(tr1d_all$hero_id)

ts9d_all$user_id <- as.factor(ts9d_all$user_id)
ts1d_all$user_id <- as.factor(ts1d_all$user_id)
ts9d_all$hero_id <- as.factor(ts9d_all$hero_id)
ts1d_all$hero_id <- as.factor(ts1d_all$hero_id)

str(tr9d_all)
str(tr1d_all)
str(ts9d_all)
str(ts1d_all)

################################ Tuning of GBM for best model################################ 
set.seed(1)
gbm_model <- gbm(formula = kda_ratio ~ ., 
                 distribution = "gaussian", 
                 data = ts9d_all,
                 n.trees = 10000,
                 cv.folds = 5)

# Optimal ntree estimate based on CV
ntree_opt_cv <- gbm.perf(object = gbm_model, 
                         method = "cv")

print(paste0("Optimal n.trees (CV Estimate): ", ntree_opt_cv))

# Generate predictions on the test set using ntree_opt_cv number of trees
gbm_predictions <- predict(object = gbm_model, 
                           newdata = ts1d_all,
                           n.trees = ntree_opt_cv)   
write.csv(gbm_predictions, "gbm_predictions2.csv")

# Generate the test set rmse
#gbm_rmse <- rmse(actual = t1dwf$, gbm_pred = pred)
#gbm_rmse

################################
