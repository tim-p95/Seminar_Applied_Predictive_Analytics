user = Sys.getenv("USERPROFILE")
if(user == "C:\\Users\\timpe_000"){
  wd = "C:/Users/timpe_000/Desktop/APA"
}else{
  wd = paste0(user, '/APA')
}
setwd(wd)


###load data
library(readr)
data = read_csv("Data/mail_data.csv")
head(data)

#convert to data frame
data = as.data.frame(data)
str(data)

###data type
#history_segment as factor
data$history_segment = as.factor(data$history_segment)
levels(data$history_segment)

table(data$history_segment)

#zip code as factor
data$zip_code = as.factor(data$zip_code)
levels(data$zip_code)

table(data$zip_code)

#channel as factor
data$channel = as.factor(data$channel)
levels(data$channel)

table(data$channel)

#segment as factor
data$segment = as.factor(data$segment)
levels(data$segment)

table(data$segment)


###check for NA values in the data set
sapply(data[,1:ncol(data)], FUN = function(x){sum(is.na(x))})


###preparation of the data set
#normalize data
library(psycho)
library(tidyverse)
data = standardize(data)
#adding index column
data$idx = seq(1:nrow(data))

#adding binary treatment column
data$treatment = sapply(data$segment, FUN = function(x) ifelse(x == "No E-Mail", 0, 1))

#create dummy features
library(mlr)

data_ = createDummyFeatures(data, target = "visit")
colnames(data_)

