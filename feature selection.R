

correlationMatrix <- cor(t9[,-c(1,2)])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)
#############

control <- trainControl(method="repeatedcv", number=10, repeat = 5,verboseIter = TRUE)
# train the model
model <- train(kda_ratio~., data=t9[,-c(1,2)], method="ranger", preProcess="scale", trControl=control)
# estimate varia   ble importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)
##################
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=1, verbose = TRUE)
# run the RFE algorithm
results <- rfe(t9[,-c(1,2,3)], t9[,3], sizes=c(1:25), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))
