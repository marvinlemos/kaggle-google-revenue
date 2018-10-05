#bestmtry <- tuneRF(trainData[,-39], trainData[,39], stepFactor=1.5, improve=1e-5, ntree=500)
library(randomForest)

rf.model = randomForest(transactionRevenue ~ have_revenue +
                                             impact +
                                             pageviews +
                                             hits +
                                             revenue_mean_by_country +
                                             pageviews_mean_by_country +
                                             hits_mean_by_country +
                                             bounces +
                                             n_countries +
                                             visitNumber +
                                             continent_Americas +
                                             n_revenue_by_city +
                                             visitStartTime +
                                             newVisits +
                                             hits_mean_by_city +
                                             pageviews_mean_by_city +
                                             month +
                                             month_unique_user_count +
                                             visits_by_month +
                                             medium +
                          channelGrouping_Social +
                          n_cities +
                          deviceCategory_desktop +
                          continent_Asia +
                          isTrueDirect +
                          isMobile +
                          operatingSystem_Macintosh +
                          is_monday,
                        data = trainData)

revenue_predicted = predict(rf.model, testData)
revenue_predicted[revenue_predicted < 0.5] = 0
revenue_predicted[testData$bounces == 1] = 0

error = sum((revenue_predicted - testData$transactionRevenue)^2)/nrow(testData)
print(error)

importance    <- importance(rf.model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'IncNodePurity'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() 
