library(dplyr)
library(ggplot2)
library(jsonlite)
library(rlist)

TRAIN_DATASET_FILE = 'dataset/train.csv'
TRAIN_DATASET_RDATA = 'dataset/train.RData'
TEST_DATASET_FILE = 'dataset/test.csv'
TEST_DATASET_RDATA = 'dataset/test.RData'

print('Importing TRAIN DATASET...')
if (!file.exists(TRAIN_DATASET_RDATA)){
  train.df = read.csv(TRAIN_DATASET_FILE, 
                      colClasses=c("fullVisitorId"="character", 
                                   "device"="character",
                                   "geoNetwork"="character",
                                   "totals"="character",
                                   "trafficSource"="character"))
  save(train.df, file = TRAIN_DATASET_RDATA)
}else{
  load(TRAIN_DATASET_RDATA)
}


print('Importing TEST DATASET...')
if (!file.exists(TEST_DATASET_RDATA)){
  test.df = read.csv(TEST_DATASET_FILE,
                     colClasses=c("fullVisitorId"="character", 
                                  "device"="character",
                                  "geoNetwork"="character",
                                  "totals"="character",
                                  "trafficSource"="character"))
  
  save(test.df, file = TEST_DATASET_RDATA)
}else{
  load(TEST_DATASET_RDATA)
}



print('Converting JSON columns from TRAIN...')
### CONVERTER VALORES DOS CAMPOS JSON EM COLUNAS DO DATASET
#based on https://www.kaggle.com/mrlong/r-flatten-json-columns-to-make-single-data-frame

#Function to convert a JSON column
jsontodf <- function(col){
  list.stack(lapply(col, function(j){
    as.list(unlist(fromJSON(j)))}) , fill=TRUE)   
}


if (!file.exists('dataset/train-flat.RData')){
  #Convert each JSON column in the train and test sets
  tr_device <- jsontodf(train.df$device)
  tr_geoNetwork <- jsontodf(train.df$geoNetwork)
  tr_totals <- jsontodf(train.df$totals)
  tr_trafficSource <- jsontodf(train.df$trafficSource)

  #Combine to make the full training and test sets
  train.flat.df <- train.df %>%
    cbind(tr_device, tr_geoNetwork, tr_totals, tr_trafficSource) %>%
    select(-device, -geoNetwork, -totals, -trafficSource)

  train.flat.df$transactionRevenue = as.numeric(train.flat.df$transactionRevenue)

  save(train.flat.df, file = 'dataset/train-flat.RData')
  
  rm(tr_device)
  rm(tr_geoNetwork)
  rm(tr_totals)
  rm(tr_trafficSource)
  
}else{
  load('dataset/train-flat.RData')
}

if (!file.exists('dataset/teste-flat.RData')){
  print('Converting JSON columns from TEST...')
  print('Converting te_device...')
  te_device <- jsontodf(test.df$device)
  
  print('Converting te_geoNetwork...')
  te_geoNetwork <- jsontodf(test.df$geoNetwork)
  
  print('Converting te_totals...')
  te_totals <- jsontodf(test.df$totals)
  
  print('Converting te_trafficSource...')
  te_trafficSource <- jsontodf(test.df$trafficSource)
  
  test.flat.df <- test.df %>%
    cbind(te_device, te_geoNetwork, te_totals, te_trafficSource) %>%
    select(-device, -geoNetwork, -totals, -trafficSource)
  
  rm(te_device)
  rm(te_geoNetwork)
  rm(te_totals)
  rm(te_trafficSource)
  
  save(test.flat.df, file = 'dataset/teste-flat.RData')
}else{
  load('dataset/teste-flat.RData')
}

print('Done')