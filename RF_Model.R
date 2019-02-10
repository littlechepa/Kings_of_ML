################################ Tuning of Random Forest for best model################################ 
set.seed(1)
# Establish a list of possible values for mtry, nodesize and sampsize
#mtry <- seq(1, ncol(t9dwf) * 0.8, 2)
mtry <- seq(1, 15, 5)
nodesize <- seq(1, 10, 4)
sampsize <- nrow(t9dwf) * c(0.7, 0.8)

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)

# Create an empty vector to store OOB(Out Of Bag) error values
oob_err <- c()

# Write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:nrow(hyper_grid)) {
  
  # Train a Random Forest model
  model <- randomForest(formula = kda_ratio ~ ., 
                        data = t9dwf[,-c(1,2)],
                        mtry = hyper_grid$mtry[i],
                        nodesize = hyper_grid$nodesize[i],
                        sampsize = hyper_grid$sampsize[i])
  
  # Store OOB error for the model                      
  oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
}

# Identify optimal set of hyperparmeters based on OOB error
opt_i <- which.min(oob_err)
#print(hyper_grid[opt_i,])
hyper_grid2 <- hyper_grid[opt_i,]

rf_model <- randomForest(formula = kda_ratio ~ ., 
                         data = t9dwf[,-c(1,2)])
                         #mtry = hyper_grid$mtry,
                         #nodesize = hyper_grid2$nodesize,
                         #sampsize = hyper_grid2$sampsize)

# Generate predicted probabilities using the model object
rf_pred <- predict(object = rf_model,   # model object 
                   newdata = t1dwf[,-c(1,2)])  # test dataset

# Compute the AUC 
rf_rmse <-rmse(actual = t1dwf$kda_ratio, 
             predicted = rf_pred)

rf_rmse

###################

rfmodel <- train(kda_ratio ~ .,t9dwf,
               tuneGrid = expand.grid(mtry = seq(1,15,5), splitrule = "variance", min.node.size = seq(5,10,2) ) ,
               method = "ranger",
               trControl = trainControl(method = "cv", number = 5,verboseIter = TRUE)
)

rmse(t1dwf$kda_ratio, predict(rfmodel, t1dwf))

