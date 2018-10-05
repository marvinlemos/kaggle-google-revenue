# LOAD LIBRARIES
library(dplyr)
library(ggplot2)
library(DataExplorer)
library(dummies)
library(corrplot)

# FUNCTIONS
numeric_to_date <- function(x) as.Date(as.character(x), format = "%Y%m%d")
grp_mean <- function(x, grp) ave(x, grp, FUN = function(x) mean(x, na.rm = TRUE))


# LOAD THE DATA
load(file = 'dataset/train-flat.RData')
load(file = 'dataset/teste-flat.RData')

# CHECK WHETHER EXIST DIFFERENT COLUMNS BETWEEN THE TWO DATASETS
setdiff(names(train.flat.df), names(test.flat.df))

train.flat.df = select(train.flat.df, -one_of("campaignCode"))

# LET'S SEE WETHER EXISTS COLUMNS WITH JUST ONE KIND OF VALUE AND REMOVE THEM
cols_uniques_values = sapply(train.flat.df, n_distinct)
cols_to_remove = names(cols_uniques_values[cols_uniques_values == 1])

# HOWEVER, DO NOT REMOVE VISIT, IT SEEMS A GOOD FEATURE
cols_to_remove = cols_to_remove[cols_to_remove != 'visits']

cols_to_remove = c(cols_to_remove, 
                   'adwordsClickInfo.slot',
                   'adwordsClickInfo.isVideoAd',
                   'adwordsClickInfo.gclId',
                   'adContent',
                   'adwordsClickInfo.criteriaParameters',
                   'adwordsClickInfo.page',
                   'adwordsClickInfo.adNetworkType',
                   'keyword')

train.flat.df = select(train.flat.df, -one_of(cols_to_remove))
test.flat.df = select(test.flat.df, -one_of(cols_to_remove))


###############
train.flat.df$transactionRevenue = log(train.flat.df$transactionRevenue)
train.flat.df$transactionRevenue[is.na(train.flat.df$transactionRevenue)] = 0

### LET'S  JOIN THE TWO DATASET TO HELP THE ANALYSIS
test.flat.df$transactionRevenue = NA
train.flat.df = rbind(train.flat.df, test.flat.df)
rm(test.flat.df)


train.flat.df$newVisits = as.numeric(train.flat.df$newVisits)
train.flat.df$date = numeric_to_date(train.flat.df$date)
train.flat.df$newVisits[is.na(train.flat.df$newVisits)] = 0

train.flat.df = mutate(train.flat.df,
                       medium = as.factor(medium),
                       campaign = as.factor(campaign),
                       hits = as.numeric(train.flat.df$hits),
                       visits = as.numeric(visits),
                       isTrueDirect = ifelse(isTrueDirect, 1, 0),
                       pageviews = as.numeric(train.flat.df$pageviews),
                       day_of_week = weekdays(train.flat.df$date),
                       month = format(train.flat.df$date, "%m"),
                       year = format(train.flat.df$date, "%Y"))

train.flat.df$isTrueDirect[is.na(train.flat.df$isTrueDirect)] = 0
train.flat.df$referralPath[is.na(train.flat.df$referralPath)] = ''

n_rows_in_train = nrow(train.flat.df)

count_is_na = table(is.na(train.flat.df))

col_is_na = colSums(is.na(train.flat.df))

cols = cbind(col_is_na, col_is_not_na = n_rows_in_train - col_is_na)
#View(cols)

total_revenue = sum(train.flat.df$transactionRevenue, na.rm = TRUE)
visitors.df = filter(train.flat.df, !is.na(transactionRevenue)) %>% 
  group_by(fullVisitorId) %>% 
  summarise(totalRevenue = sum(transactionRevenue),
            have_revenue = ifelse(max(transactionRevenue)>0,1,0),
            count = n(),
            n_visits_by_user = sum(visits),
            impact = sum(transactionRevenue)/total_revenue)

visitors.df$totalRevenue[is.na(visitors.df$totalRevenue)] = 0
visitors.df$impact[is.na(visitors.df$impact)] = 0

visitors.df = arrange(visitors.df, totalRevenue)

#### PLOTS
#plot(density(visitord.df$totalRevenue))
#polygon(density(visitord.df$totalRevenue), col="red")

#### which browser brings more revenue?

#ggplot(filter(train.flat.df, !is.na(transactionRevenue)), 
#       aes(x=browser, y=transactionRevenue)) + 
#  geom_bar(stat = 'identity')
### It seems Chrome brings more revenue.

#### which campain brings more revenue?

#ggplot(filter(train.flat.df, !is.na(transactionRevenue)), 
#       aes(x=campaign, y=transactionRevenue)) + 
#  geom_bar(stat = 'identity')
### it seems the marjoroty of instances refer to not set campain. Let's remove it

### Day of Week
#ggplot(filter(train.flat.df, !is.na(transactionRevenue)), 
#       aes(x=day_of_week, y=transactionRevenue)) + 
#  geom_bar(stat = 'identity')
###Friday seems to be the best day

### Country
n_records_revenue = sum(train.flat.df$transactionRevenue>0, na.rm = TRUE)
countries.df = group_by(train.flat.df, country) %>% 
    summarise(n_countries = n(), 
              pageviews_mean_by_country = mean(pageviews, na.rm = TRUE),
              hits_mean_by_country = mean(hits, na.rm = TRUE),
              revenue_mean_by_country = mean(transactionRevenue, na.rm = TRUE))

ggplot(filter(countries.df, revenue_mean_by_country>0.5), 
       aes(x=country, y=revenue_mean_by_country)) + 
  geom_bar(stat = 'identity')


train.flat.df = merge(train.flat.df, countries.df, by.x = 'country', by.y = 'country')

#### CITIES
# How many specified cities?
n_records_revenue_withou_not_ava_cities = nrow(filter(train.flat.df, city != 'not available in demo dataset', city != '(not set)', !is.na(transactionRevenue), transactionRevenue>0))

cities.df = filter(train.flat.df, city != 'not available in demo dataset', city != '(not set)') %>%
  group_by( city) %>%
  summarise(n_revenue_by_city =  sum(transactionRevenue>0, na.rm = TRUE) / n_records_revenue_withou_not_ava_cities,
            pageviews_mean_by_city = mean(pageviews, na.rm = TRUE),
            hits_mean_by_city = mean(hits, na.rm = TRUE),
            n_cities = n())

train.flat.df = merge(train.flat.df, cities.df,
                      by.x = 'city',
                      by.y = 'city',
                      all.x = TRUE)

train.flat.df$n_revenue_by_city[is.na(train.flat.df$n_revenue_by_city)] = 0
train.flat.df$pageviews_mean_by_city[is.na(train.flat.df$pageviews_mean_by_city)] = 0
train.flat.df$hits_mean_by_city[is.na(train.flat.df$hits_mean_by_city)] = 0
train.flat.df$n_cities[is.na(train.flat.df$n_cities)] = 0


#### REMOVE SOME FIELDS 
#plot_missing(train.flat.df)
#plot_missing(train.flat.df)


### PORQUE 100 pageviews estao com NA
train.flat.df$pageviews[is.na(train.flat.df$pageviews)] = 0


### QUEM TEM BOUNCES == 1 NAO TEM REVENUE
### OS REGISTROS QUE POSSUEM REVENUE ESTAO COM BOUNCES == NA
train.flat.df$bounces[is.na(train.flat.df$bounces)] = 0


### HOT ENCONDING
#cols_to_encode = c('channelGrouping', 'operatingSystem', 'deviceCategory', 'continent', 'country')
cols_to_encode = c('channelGrouping', 'operatingSystem', 'deviceCategory', 'continent', 'day_of_week')

train.flat.df$isMobile = ifelse(train.flat.df$isMobile == FALSE, 0, 1)
train.flat.df = dummy.data.frame(train.flat.df, names = cols_to_encode , sep = '_')
train.flat.df$is_chrome = ifelse(train.flat.df$browser == 'Chrome', 1, 0)
train.flat.df$browser = NULL


#### FEATURE ENG.
#### Criar uma coluna para indicar se ID ja fez acesso com Revenue?
train.flat.df = merge(train.flat.df, 
                      select(visitors.df, 'fullVisitorId', 'impact', 'have_revenue'), 
                      by.x = 'fullVisitorId', by.y = 'fullVisitorId')


### Unique users count by month
unique_users_by_month.df = group_by(train.flat.df, month) %>% 
  summarise(month_unique_user_count = n_distinct(fullVisitorId),
            visits_by_month = sum(visits, na.rm = TRUE))

unique_users_by_month.df$visits_by_month = unique_users_by_month.df$visits_by_month / max(unique_users_by_month.df$visits_by_month) 

train.flat.df = merge(train.flat.df, unique_users_by_month.df, by.x = 'month', by.y = 'month')

# TODO: CREATE FIELDS TO REPRESENT SOURCE, METRO, NETWORKDOMAIN, REGION, COUTRY, REFERRALPATH
# TODO: HOT-ENCONDING MONTH
##### Let's train a model to predict revenue


cols_to_remove_in_train = c('fullVisitorId', 
                            'date', 
                            'subContinent',
                            'sessionId',
                            'keyword',
                            'visitId',
                            'country', 
                            'region',
                            'source',
                            'referralPath',
                            'networkDomain',
                            'metro',
                            'city',
                            'operatingSystem_Tizen', 
                            'operatingSystem_Nintendo Wii',
                            'operatingSystem_OS/2', 
                            'operatingSystem_Nokia',
                            'operatingSystem_Playstation Vita',
                            'operatingSystem_SymbianOS',
                            'operatingSystem_NTT DoCoMo')

train.flat.df[, cols_to_remove_in_train] = list(NULL)

#corrs = cor(train.flat.df, transactionRevenue)
#corrplot(corrs, method = 'circle')


#### CREATING A BALANCED DATASET
new.train.df = filter(train.flat.df, transactionRevenue>0)
n_rows_in_new = nrow(new.train.df)

no_revenue.df = filter(train.flat.df, transactionRevenue == 0)
index = sample(1:nrow(no_revenue.df), n_rows_in_new)
new.train.df = rbind(new.train.df,
                     no_revenue.df[index, ])

n_rows_in_new = nrow(new.train.df)

names(new.train.df)[names(new.train.df) == "channelGrouping_(Other)"] <- "channelGrouping_Other"
names(new.train.df)[names(new.train.df) == "operatingSystem_(not set)"] <- "operatingSystem_not_set"
names(new.train.df)[names(new.train.df) == "channelGrouping_Organic Search"] <- "channelGrouping_Organic_Search"
names(new.train.df)[names(new.train.df) == "operatingSystem_(not set)"] <- "operatingSystem_not_set"
names(new.train.df)[names(new.train.df) == "channelGrouping_Paid Search"] <- "channelGrouping_Paid_Search"
names(new.train.df)[names(new.train.df) == "continent_(not set)"] <- "continent_not_set"
names(new.train.df)[names(new.train.df) == "day_of_week_Segunda Feira"] <- "is_monday"
names(new.train.df)[names(new.train.df) == "day_of_week_Terça Feira"] <- "is_tuesday"
names(new.train.df)[names(new.train.df) == "day_of_week_Quarta Feira"] <- "is_wednesday"
names(new.train.df)[names(new.train.df) == "day_of_week_Quinta Feira"] <- "is_thursday"
names(new.train.df)[names(new.train.df) == "day_of_week_Sexta Feira"] <- "is_friday"
names(new.train.df)[names(new.train.df) == "day_of_week_Sábado"] <- "is_saturday"
names(new.train.df)[names(new.train.df) == "day_of_week_Domingo"] <- "is_sunday"
names(new.train.df)[names(new.train.df) == "operatingSystem_Nintendo Wii"] <- "operatingSystem_Nintendo_Wii"
names(new.train.df)[names(new.train.df) == "operatingSystem_Nintendo WiiU"] <- "operatingSystem_Nintendo_WiiU"
names(new.train.df)[names(new.train.df) == "operatingSystem_Chrome OS"] <- "operatingSystem_Chrome_OS"
names(new.train.df)[names(new.train.df) == "operatingSystem_Windows Phone"] <- "operatingSystem_Windows_Phone"
names(new.train.df)[names(new.train.df) == "operatingSystem_Firefox OS"] <- "operatingSystem_Firefox_OS"
names(new.train.df)[names(new.train.df) == "operatingSystem_Nintendo 3DS"] <- "operatingSystem_Nintendo_3DS"

#### SPLIT THE DATA
shuffledIndex = sample(1:n_rows_in_new, 0.8 * n_rows_in_new)

trainData = new.train.df[shuffledIndex, ]
testData = new.train.df[-shuffledIndex, ]
