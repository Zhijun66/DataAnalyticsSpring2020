#creating a dataframe
rm(list = ls())
days<- c('Mon','Tue','Wed','Thur','Fri','Sat','Sun') 
temp <- c(28,30.5,32,31.2,29.3,27.9,26.4)
snowed <- c('T','T','F','F','T','T,','F')
help("data.frame")
RPI_Weather_Week <- data.frame(days,temp,snowed)

RPI_Weather_Week
head(RPI_Weather_Week)
str(RPI_Weather_Week)
summary(RPI_Weather_Week)

RPI_Weather_Week[1,]
RPI_Weather_Week[,1]
RPI_Weather_Week[,'snowed']
RPI_Weather_Week[,'days']
RPI_Weather_Week[,'temp']
RPI_Weather_Week[1:5,c("days","temp")]
RPI_Weather_Week$temp
subset(RPI_Weather_Week,subset = snowed==TRUE)

sorted.snowed <- order(RPI_Weather_Week['snowed'])
sorted.snowed
RPI_Weather_Week[sorted.snowed,]

dec.snow<-order(-RPI_Weather_Week$temp)
dec.snow

empty.dataframe <- data.frame()
v1 <- 1:10
v1
letters
v2 <- letters[1:10]
df <- data.frame(col.name.1=v1,col.name.2=v2)
df


#Exercise1
rm(list=ls())
EPI_data <- read.csv("/Users/zhijun/Desktop/2010EPI_data.csv",skip = 1)
#or
#EPI_data <- read.xlsx(”<path>/2010EPI_data.xlsx")
# Note: replace default data frame name – cannot start with numbers!
View(EPI_data)
#
# detach(EPI_data)
attach(EPI_data) 	# sets the ‘default’ object
fix(EPI_data) 	# launches a simple data editor
EPI<-EPI_data$EPI			# prints out values EPI_data$EPI
tf <- is.na(EPI) # records True values if the value is NA
E <- EPI[!tf] # filters out NA values, new array

#other data
GRUMP_data <- read.csv("/Users/zhijun/Desktop/GPW3_GRUMP_SummaryInformation_2010.csv")


summary(EPI)
fivenum(EPI,na.rm=TRUE)
stem(EPI)#stem and leaf plot
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPI,na.rm=TRUE,bw=1.)) # or try bw=“SJ”
rug(EPI) 
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE)#Cumulative density function
par(pty="s") 
qqnorm(EPI); qqline(EPI)
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df = 5), x, xlab = "Q-Q plot for t dsn")
qqline(x)
boxplot(EPI,DALY) 
qqplot(EPI,DALY)

#Exercise2
EPILand<-EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob=TRUE)


EPI_data1 <- read.csv("/Users/zhijun/Desktop/water-treatment.csv")


