library(forecast)
library(data.table)
library(xts)
library(tseries)
library(RMySQL)
library(scales)
library(lubridate)


#l
rm(list=ls())
#set.seed(99999)
sidc<-fread("http://sidc.be/silso/DATA/SN_y_tot_V2.0.csv",sep = ';')
names(sidc) <- c("Year","Spots","V3","V4","V5")
spots <- sidc[,.(Year,Spots)]
spots <- subset(spots,Year >=1800)
train <- subset(spots,Year <=2008)
train.ts <- ts(train$Spots,start=1999,end=2008)
train.arima <- auto.arima(train.ts)
coef(train.arima)
predict(train.arima,n.head=12,se.fit=T)
spots4cast <- forecast(object=train.arima,h=12)
spots4cast.dt <- as.data.table(spots4cast)
write.csv(spots4cast,file="spots5cast.csv",row.names=TRUE)
#

sidc <- sidc[,.(Year,Spots)]
train <- subset(sidc,Year >=1800 & Year <=2018)
# train <- train[,.(Year,Spots)]
#
train.ts <- ts(train$Spots,start=1800,end=2008)
train.arima <- auto.arima(train.ts)
coef(train.arima)
predict(train.arima,n.head=12,se.fit=T)
monthly4cast <- forecast(object=train.arima,h=12)
monthly4cast.dt <- as.data.table(monthly4cast)
# XTS series for monthly
#
isn.xts <- xts(x = DAILY$Spots, order.by = DAILY$Ymd)
str(isn.xts)
isn.monthly<- apply.monthly(isn.xts, mean)
XTSMONTHLY <- as.data.table(isn.monthly)
colnames(XTSMONTHLY) <- c("Ymd","Spots")



###### MySQL & RMySQL
mydb <- dbConnect(MySQL(),user='root',password='dJj12345',dbname="sidc",
                  host='localhost')
dbWriteTable(mydb, "spots4cast.dt", spots4cast.dt, row.names = FALSE)
#
dbListTables(mydb)
#
