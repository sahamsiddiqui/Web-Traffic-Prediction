########################################################################################################
###################   WEB TRAFFIC TIME SERIES FORECASTING    ###########################################
########################################################################################################

#VISUALIZATION LIBRARIES
library('ggplot2','ggthemes','scales')
library('grid', 'gridExtra')
library('corrplot', 'ggrepel','RColorBrewer')
#Data manipulation LIBRARIES
library('data.table','dplyr','readr')
#data wrangling
library( 'tibble','tidyr','lazyeval','broom')
#string manipulation
library('stringr','purrr')
library('forcats') # factor manipulation
# time series analysis
library('forecast')
library('prophet')
library('lubridate')# data and time#

# Load Data
train <- read.csv('train_1.csv')
key <-read.csv('key_1.csv')

dim(train) #row = 145063(diffrent page info)  , columns =  551(1page+ 550 dates)

head(colnames(train))
tail(colnames(train))
head(train["Page"])

dim(key) # row =8703780     col=  2

head(key) 

is.na(train)%>% any()#True

sum(is.na(train))# 6192931 NA present in data

sum(is.na(train)) / (ncol(train)*nrow(train))#0.07747971 missing data


# Data transformation

#-> Article names and meta-parameter
#split train data into (page col)(time-series data) and 
# split Page column into meta parameter
tdates <- train %>% select(-Page)
foo <- train %>% select(Page) %>% rownames_to_column()
dim(foo)
head(foo)
tail(foo)

mediawiki <-foo %>% filter(str_detect(Page, "mediawiki"))
wikimedia <-foo %>% filter(str_detect(Page, "wikimedia"))
dim(wikimedia)

wikipedia <- foo %>% filter(str_detect(Page, "wikipedia")) %>% filter(!str_detect(Page, "wikimedia")) %>% filter(!str_detect(Page,"mediawiki"))
dim(wikipedia)

head(wikimedia)

wikipedia <- wikipedia %>%
  separate(Page, into = c("foo", "bar"), sep = ".wikipedia.org_") %>%
  separate(foo, into = c("article", "locale"), sep = -3) %>%
  separate(bar, into = c("access", "agent"), sep = "_") %>%
  mutate(locale = str_sub(locale,2,3))
head(wikipedia)
#
wikimedia <- wikimedia %>%
  separate(Page, into = c("article", "bar"), sep = "_commons.wikimedia.org_") %>%
  separate(bar, into = c("access", "agent"), sep = "_") %>%
  add_column(locale = "wikmed")

# rejoin the cleaned data
tpages<-wikipedia %>% full_join(wikimedia,  by= c(colnames(wikipedia)))%>%full_join(mediawiki, by= c(colnames(wikipedia)))

sample_n(tpages,size =5) #select random sample

########################################
# plot meta-parameters
x11();tpages %>% ggplot(aes(agent)) + geom_bar(fill = "red")
x11();tpages %>% ggplot(aes(access)) + geom_bar(fill = "red")
x11();tpages %>% ggplot(aes(locale, fill = locale)) + geom_bar() + theme(legend.position = "none")
########################################

# 3. Basic time series parameter
#begin with basic set of parameters
#mean # std