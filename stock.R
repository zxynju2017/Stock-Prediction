library(ggplot2)
#读取数据
stock_data<-read.csv("600519.csv",header = TRUE,stringsAsFactors = FALSE)

#去除无用列，转化数据类型为数值型
stock<-stock_data[,-c(2,3,14)]
stock[,7]<-as.numeric(as.character(stock[,7]))
stock[,8]<-as.numeric(as.character(stock[,8]))

#分离训练集与测试集
stock<-stock[c(1:859),]
stock.test<-stock[c(1:128),]
stock.train<-stock[c(129:859),]

#随机森林
library(randomForest)
set.seed(1234)
stock.forest<-randomForest(收盘价~.,data=stock.train[,-1],na.action=na.roughfix,importance = TRUE)
stock.forest
importance(stock.forest)
#随机森林预测
stock.pred<-predict(stock.forest,stock.test[,-c(1:2)])
pre_data <- data.frame("no"=c(128:1),"data"=stock.test$收盘价,"predict"=stock.pred)
ggplot()+geom_line(data = pre_data,aes(x=no,y=predict,color="predict"))+
  geom_line(data = pre_data,aes(x=no,y=data,color="source_data"))+
  scale_colour_manual("",values = c("source_data" = "blue","predict" = "red"))+
  xlab("No.")+ylab("收盘价")+labs(title="随机森林预测拟合曲线")

#全数据集随机森林
stock.forest.new<-stock.forest<-randomForest(收盘价~.,data=stock[,-1],na.action=na.roughfix,importance = TRUE)
stock.forest.new
importance(stock.forest.new)


#提取变量：最高价、最低价、开盘价、流通市值
stock_time<-stock[,c(1:5,12)]
#数据补齐
all_days<-seq(as.Date("2017/01/03"),as.Date("2020/07/15"),by="days")
all_days<-data.frame("日期"=all_days)
all_days[,1]<-as.character(all_days[,1])
stock_time[,1]<-as.Date(stock_time[,1])
stock_time[,1]<-as.character(stock_time[,1])
stock_all_time<-merge(x=all_days,y=stock_time,by.x = "日期",by.y = "日期",all = TRUE)
for(i in 1:1290){
  if(is.na(stock_all_time$收盘价[i]))stock_all_time[i,c(2:6)]<-stock_all_time[i-1,c(2:6)]
}


#时间序列分析Holt-winters指数光滑#改为预测3天的，测试集只留3天数据
library(forecast)
ts.train<-stock_all_time[c(1:1285),]
ts.test<-stock_all_time[c(1286:1290),]
stock.ts<-ts(ts.train$收盘价,start=c(2017,1,3),frequency=365)
plot(stock.ts)
#增加线性拟合曲线，lm函数中"~"表示左边为因变量，右边为自变量，得到时间序列图及其线性拟合曲线
abline(lm(stock.ts~time(stock.ts)))# or abline(reg = lm(air~time(stock.ts)))
#以年为单位的时间序列图
plot(aggregate(stock.ts,FUN=mean))
#单位根检验
adf.test(dStock)
tsdiag(arima_fit)
# stock.ts.fit<-ets(log(stock.ts),model="AAA")
# stock.ts.fit
# stock.ts.pred<-forecast(stock.ts.fit,199)
# stock.ts.pred
# plot(stock.ts.pred)
# ts_predict<-exp(stock.ts.pred$mean)
# ts_pre_data <- data.frame("no"=c(1:199),"data"=ts.test$收盘价,"predict"=ts_predict)
# ggplot()+geom_line(data = ts_pre_data,aes(x=no,y=predict,color="predict"))+
#   geom_line(data = ts_pre_data,aes(x=no,y=data,color="source_data"))+
#   scale_colour_manual("",values = c("source_data" = "blue","predict" = "red"))+
#   xlab("No.")+ylab("收盘价")+labs(title="时间序列预测拟合曲线")

#arima模型
library(tseries)
stock.ts<-ts(ts.train$收盘价,start=c(2017,1,3),frequency=365)
plot(stock.ts)
#ndiff函数
ndiffs(stock.ts)
dStock<-diff(stock.ts)
plot(dStock)
acf(dStock)
pacf(dStock)
arima_fit<-arima(stock.ts,order = c(0,1,0))
acf(arima_fit$residuals)
Box.test(arima_fit$residuals,type = "Ljung-Box")
forecast(arima_fit,5)
plot(forecast(arima_fit,3),xlab = "date",ylab = "收盘价")


