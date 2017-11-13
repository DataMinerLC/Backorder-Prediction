testing_data <- read.csv("C:/Users/Liwei/Dropbox/CS 513/Final Project/Predict Product Backorders/Kaggle_Test_Dataset.CSV", header = TRUE)
training_data <- read.csv("C:/Users/Liwei/Dropbox/CS 513/Final Project/Predict Product Backorders/Kaggle_Training_Dataset.CSV", header = TRUE)

data <- rbind(d1,d2)
?cbind
?write.csv
write.csv(data, "C:/Users/Liwei/Dropbox/CS 513/Final Project/Predict Product Backorders/Backorders_Dataset.CSV")

training_data <- training_data[,-1]
colnames(training_data2)
sapply(training_data2, class)
head(training_data, n=50)
summary(training_data)

install.packages("plyr")
library(plyr)
count(training_data$went_on_backorder=="Yes")
?count()
count(testing_data$went_on_backorder=="Yes")


training_complete <- na.omit(training_data)
summary(training_complete)
head(training_complete)

for (i in 1:length(training_data)){
  n_Na <-sum(is.na(training_data[i]))
  print(n_Na)
}

training_data2 <- training_data
training_data2$perf_6_month_avg[training_data2$perf_6_month_avg==-99.00] <- NA
training_data2$perf_12_month_avg[training_data2$perf_12_month_avg==-99.00] <- NA
head(training_data2, n=50)

for (i in 1:length(training_data2)){
  n_Na <-sum(is.na(training_data2[i]))
  print(n_Na)
}
summary(training_data2)

category_variable <- training_data2[sapply(training_data2, class)=="factor"]
numeric_variable <- training_data2[, -c(12,17:22)]
summary(numeric_variable)


numeric_variable$lead_time <- ifelse(is.na(training_data2$lead_time), 
                                   median(training_data2$lead_time,na.rm=TRUE), 
                                   training_data2$lead_time)

numeric_variable$perf_6_month_avg <- ifelse(is.na(training_data2$perf_6_month_avg), 
                                          median(training_data2$perf_6_month_avg,na.rm = TRUE),
                                          training_data2$perf_6_month_avg)
numeric_variable$perf_12_month_avg <- ifelse(is.na(training_data2$perf_12_month_avg), 
                                          median(training_data2$perf_12_month_avg,na.rm = TRUE),
                                          training_data2$perf_12_month_avg)

norm <- function(x)
{
  z <- (x - min(x,na.rm=TRUE))/ (max(x,na.rm=TRUE) - min(x,na.rm=TRUE))
  return(z)
}


for (j in 1:length(numeric_variable))
{
  numeric_variable2 <- numeric_variable
  numeric_variable2[j] <- norm(numeric_variable2[j])
  print(numeric_variable2)
}


numeric_variable$national_inv <- norm(as.numeric(numeric_variable$national_inv))
numeric_variable$lead_time <- norm(as.numeric(numeric_variable$lead_time))
numeric_variable$in_transit_qty <- norm(as.numeric(numeric_variable$in_transit_qty))
numeric_variable$forecast_3_month <- norm(as.numeric(numeric_variable$forecast_3_month))
numeric_variable$forecast_6_month <- norm(as.numeric(numeric_variable$forecast_6_month))
numeric_variable$forecast_9_month <- norm(as.numeric(numeric_variable$forecast_9_month))
numeric_variable$sales_1_month <- norm(as.numeric(numeric_variable$sales_1_month))
numeric_variable$sales_3_month <- norm(as.numeric(numeric_variable$sales_3_month))
numeric_variable$sales_6_month <- norm(as.numeric(numeric_variable$sales_6_month))
numeric_variable$sales_9_month <- norm(as.numeric(numeric_variable$sales_9_month))
numeric_variable$min_bank <- norm(as.numeric(numeric_variable$min_bank))
numeric_variable$pieces_past_due <- norm(as.numeric(numeric_variable$pieces_past_due))
numeric_variable$perf_6_month_avg <- norm(as.numeric(numeric_variable$perf_6_month_avg))
numeric_variable$perf_12_month_avg <- norm(as.numeric(numeric_variable$perf_12_month_avg))
numeric_variable$local_bo_qty <- norm(as.numeric(numeric_variable$local_bo_qty))

category_variable$potential_issue <- ifelse(category_variable$potential_issue=='Yes',1,0)
category_variable$deck_risk <- ifelse(category_variable$deck_risk=='Yes',1,0)
category_variable$oe_constraint <- ifelse(category_variable$oe_constraint=='Yes',1,0)
category_variable$ppap_risk <- ifelse(category_variable$ppap_risk=='Yes',1,0)
category_variable$stop_auto_buy <- ifelse(category_variable$stop_auto_buy=='Yes',1,0)
category_variable$rev_stop <- ifelse(category_variable$rev_stop=='Yes',1,0)
category_variable$went_on_backorder <- ifelse(category_variable$went_on_backorder=='Yes',1,0)

training_complete <- data.frame(numeric_variable, category_variable)

head(numeric_variable)
head(category_variable)
head(training_complete)

write.csv(training_complete, "C:/Users/Liwei/Dropbox/CS 513/Final Project/Predict Product Backorders/trainging_complete_Dataset.CSV")
