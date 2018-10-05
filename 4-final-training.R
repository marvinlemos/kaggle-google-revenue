##### FINAL TRAINING

rf.model = randomForest(transactionRevenue ~ .,
                        data = new.train.df, 
                        ntree = 500,
                        mtry = 17)

revenue_predicted = predict(rf_final.model, test.flat.df)

test.original.df$revenue_predicted = revenue_predicted
#test.original.df$revenue_predicted[test.original.df$bounces == 1] = 0

result_final.df = select(test.original.df, fullVisitorId, revenue_predicted) %>%
  group_by(fullVisitorId) %>%
  summarise(PredictedLogRevenue = sum(revenue_predicted))

result_final.df$PredictedLogRevenue[result_final.df$PredictedLogRevenue < 0.5] = 0

write.csv(result_final.df, file = 'submission.csv', quote = FALSE, row.names = FALSE)

filter(test.original.df, bounces>0)
