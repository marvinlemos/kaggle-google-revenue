



##### LINEAR MODEL
#linear.model = lm(transactionRevenue ~ have_revenue +
#                              is_chrome +
#                              `day_of_week_Sexta Feira`,
#                              data = trainData)
linear.model = lm(transactionRevenue ~ have_revenue + 
                    continent_Americas, data = trainData)
summary(linear.model)
#parece que o impacto consegue explicar 75% dos dados


#### VALIDATION
revenue_predicted = predict(linear.model, testData)
result.df = select(testData, transactionRevenue)

error = sum((revenue_predicted - testData$transactionRevenue)^2)/nrow(testData)
print(error)


#### DECISION TREE
fitControl <- trainControl(method = "cv", number = 5)
cartGrid <- expand.grid(.cp=(1:50)*0.01)

#decision tree - FIND THE BEST CP
tree_model <- train(transactionRevenue ~ impact + is_chrome, data = trainData,
                      method = "rpart", trControl = fitControl, tuneGrid = cartGrid)
print(tree_model)

# TRAIN THE MAIN TREE

#main_tree <- rpart(transactionRevenue ~ impact + 
#                     have_revenue +
#                     continent_Americas +
#                     channelGrouping_Referral,
#                   data = trainData,
#                   control = rpart.control(cp=0.5))
#prp(main_tree)

#revenue_predicted = predict(main_tree, testData)
#error = sum((revenue_predicted - testData$transactionRevenue)^2)/nrow(testData)
#print(error)

################## RANDOM FOREST