rm(list = ls())
install.packages("titanic")
library("titanic")
set.seed(12345)
help(par)
par(mar=rep(0.2,4))
data_Matrix<-matrix(rnorm(400),nrow = 40)
image(1:10,1:40,t(data_Matrix)[,nrow(data_Matrix):1])
help("heatmap")
help("rbinom")

set.seed(678910)
for (i in 1:40) {
  coin_Flip<-rbinom(1,size = 1,prob = 0.5)
  if(coin_Flip){
    data_Matrix[i,]<-data_Matrix[i,]+rep(c(0,3),each=5)
  }
}
par(mar=rep(0.2,4))
image(1:10,1:40,t(data_Matrix)[,nrow(data_Matrix):1])
heatmap(data_Matrix)

hh<-hclust(dist(data_Matrix))
data_Matrix_Ordered<-data_Matrix[hh$order,]
par(mfrow=c(1,3))
image(t(data_Matrix_Ordered)[,nrow(data_Matrix_Ordered):1])
plot(rowMeans(data_Matrix_Ordered),40:1,,xlab = "The Row Mean",ylab = "Row",pch=19)
plot(colMeans(data_Matrix_Ordered),xlab = "Column",ylab = "Column Mean", pch=19)

#Lab1_bronx1
install.packages("gdata")
library("gdata")
bronx1<-read.xls(file.choose(),pattern="BOROUGH",stringsAsFactors=FALSE,sheet=1) 
bronx1<-bronx1[which(bronx1$GROSS.SQUARE.FEET!="0" & bronx1$LAND.SQUARE.FEET!="0" & bronx1$SALE.PRICE!="$0"),]
View(bronx1)

#alternate
attach(bronx1)
SALE.PRICE<-sub("\\$","",SALE.PRICE)
SALE.PRICE<-as.numeric(gsub(",","",SALE.PRICE))
GROSS.SQUARE.FEET<-as.numeric(gsub(",","",GROSS.SQUARE.FEET))
LAND.SQUARE.FEET<-as.numeric(gsub(",","",LAND.SQUARE.FEET))
plot(log(GROSS.SQUARE.FEET),log(SALE.PRICE))
m1<-lm(log(SALE.PRICE)~log(GROSS.SQUARE.FEET))
summary(m1)
abline(m1,col="red",lwd=2)
plot(resid(m1))


