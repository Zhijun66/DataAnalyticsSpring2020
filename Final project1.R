rm(list=ls())
library(lubridate)
library(corrplot)
library(GGally)
library(plotly)
library(ggplot2) 
library(NbClust)
library(cluster)
library(gridExtra)
library(grid)

salaries <- read.csv("/Users/zhijun/Desktop/project/Employee_Salaries_-_2016.csv")

#transfer date format and calculate days
#install.packages("lubridate")
library(lubridate)
date<-matrix(salaries$Date.First.Hired)
strDates <- salaries$Date.First.Hired
dates <- as.Date(strDates, "%m/%d/%Y")
end.day<-as.Date("2017-01-01")
days.work<-as.matrix(difftime(end.day,dates, units="days"))

#clear data
#first, built a dataframe so that we can easily clean data
myDF<-as.data.frame(matrix(c(salaries$Gender, salaries$Current.Annual.Salary, 
                             salaries$X2016.Gross.Pay.Received,days.work),ncol=4))
names(myDF)<-c("Gender","Current.Annual.Salary","Gross.Pay.Received","Working.Days")
newDF<-na.omit(myDF)# for gender column: female=2; male=3

#see distribution
#gender
hist(newDF$Gender,col=(c("lightblue","darkgreen")))
#Current.Annual.Salary
summary(newDF$Current.Annual.Salary)# check whether it has extreme value
par(mfrow=c(1,2))
#boxplot(newDF$Current.Annual.Salary)
hist(newDF$Current.Annual.Salary,col=(c("lightblue","darkgreen")))
#2016.Gross.Pay.Received
summary(newDF$Gross.Pay.Received)# there are extreme values and check the dataset, some datas are unresonable compared with cuurent salaries
par(mfrow=c(1,2))
#boxplot(newDF$Gross.Pay.Received)
hist(newDF$Gross.Pay.Received,col=(c("lightblue","darkgreen")))
#date.hired
summary(newDF$Working.Days)
par(mfrow=c(1,2))
#boxplot(newDF$Working.Days)
hist(newDF$Working.Days,col=(c("lightblue","darkgreen")))
#clean data again
newDF$Gross.Pay.Received[newDF$Gross.Pay.Received==0]<-NA #delete 0 value in dataframe
newDF<-na.omit(newDF)# for gender column: female=2; male=3
#conclusion: i choose Current.Annual.Salary variable as input when implement model
#male and female 2016 gross pay
salaries1<-salaries[,-12]
salaries1<-cbind(salaries1,days.work)
salaries1$X2016.Gross.Pay.Received[salaries1$X2016.Gross.Pay.Received==0]<-NA
salaries1<-na.omit(salaries1)
salaries1 %>% 
  ggplot(aes(x = Department, y = X2016.Gross.Pay.Received, fill = Department)) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 10, hjust = .5)) +
  labs(y = "2016.Gross.Pay.Received", title = "Salary Distribution", 
       x = "", fill = "")
salaries1 %>% 
  ggplot(aes(x = Assignment.Category, y = X2016.Gross.Pay.Received, fill = Assignment.Category)) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 10, hjust = .5)) +
  labs(y = "2016.Gross.Pay.Received", title = "Salary Distribution", 
       x = "", fill = "")
newDF$Gender <- as.factor(ifelse(newDF$Gender == 3, "Male", "Female"))
newDF %>% 
  ggplot(aes(x = Gender, y = Gross.Pay.Received, fill = Gender)) +
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 10, hjust = .5)) +
  labs(y = "2016.Gross.Pay.Received", title = "Salary Distribution", 
       x = "", fill = "")

#correlation
#check.no1
#install.packages("corrplot")
#install.packages("GGally")
library(corrplot)
library(GGally)
#library(dplyr)
par(mfrow=c(1,1))
M <- cor(newDF)
corrplot(M, method = "circle",type = "upper")

#check.no2
employee_salary_cor <- newDF
ggpairs(employee_salary_cor)
#1. positive and negative
#2. date first hired > gender ,date first hired has stronger correlation than gender with gross pay receiverd and current salary



#Regression Model
library(plotly)
attach(newDF)
lmEmSalary<-lm(Gross.Pay.Received~Working.Days+Gender,data = newDF)
summary(lmEmSalary)

#plot regression model
newDF %>% 
  ggplot(aes(x = newDF$Working.Days, y = newDF$Gross.Pay.Received)) + 
  geom_point() + 
  geom_smooth(method = "lm")
#conclusion:Dates.Hired has positive relationship with Gross.Pay.Received
employee_salary_regression <- 
  newDF %>% select(Gross.Pay.Received, Working.Days, Gender)
lm(Gross.Pay.Received~., data=employee_salary_regression)
#with every day longer hired, increase 5.15 dollars salaries
#prediction
lmEmSalary1<-lm(Gross.Pay.Received~Working.Days,data = newDF)
Working.Days1<-seq(4185,4215,1)
DF<-data.frame(Working.Days1)
pred<-predict(lmEmSalary1,newdata = DF, interval="prediction")
cred<-predict(lmEmSalary1,newdata = DF, interval="confidence")

#male vs female
#newDF$Gender <- as.factor(ifelse(newDF$Gender == 3, "Male", "Female"))
head(newDF)
newDF %>% 
  ggplot(aes(x = newDF$Gross.Pay.Received, y =newDF$Working.Days , colour = Gender)) + 
  geom_point() + 
  geom_smooth(method="lm")
#with same hired date, male salaries received are more than female


#data visualization
plot_ly(data = salaries1, x = ~salaries1$X2016.Gross.Pay.Received, y = ~salaries1$days.work, color = ~salaries1$Department,
        hoverinfo = "text",
        text = ~paste("Name: ", salaries1$Full.Name,
                      "<br>Salary: ", format(salaries1$X2016.Gross.Pay.Received, big.mark = ","),"$",
                      "<br>Working.Days: ", round(salaries1$days.work, digits = 3),
                      "<br>Department: ", salaries1$Department)) %>% 
  layout(
    title = "Salary vs Working.Days",
    xaxis = list(title = "Salary USD"),
    yaxis = list(title = "Working.Days")
  )
#FRS department has relatively highest salaries

#K-means---------------------------------------------------------------------------
salaries5<-salaries[,c(3,4,5)]
salaries5<-cbind(salaries5,days.work)
salaries5$X2016.Gross.Pay.Received[salaries5$X2016.Gross.Pay.Received==0]<-NA
salaries5<-na.omit(salaries5)

corrplot(cor(salaries5), method = "circle",type = "upper")
ggpairs(salaries5)
#high collinearity found between current annual salary and 2016 gross pay received. so remove...
modeldata<-salaries5[,-c(1,3)]
modeldata<-modeldata[sample(nrow(df), 800), ]
df = as.data.frame(scale(modeldata)) 
wssplot <- function(data, nc = 20, seed = 1234) {
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2 : nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withins)}
  plot(1:nc, wss, type = "b", xlab = "Number of Clusters",
       ylab = "Within groups sum of squares")
}
wssplot(df)
set.seed(1234)
nc <- NbClust(df, min.nc = 3, max.nc = 20, method = "kmeans" )
par(mfrow=c(1,1))
barplot(table(nc$Best.nc[1,]))
set.seed(1234)
k3 <- kmeans(df, centers = 3, nstart = 25)
p1 = fviz_cluster(k3, geom = "point", data = df) + ggtitle("k = 3")
p1

set.seed(1234)
final <- kmeans(df, centers=3)
aggregate(modeldata, by=list(final$cluster), mean)

ggpairs(cbind(modeldata, Cluster=as.factor(final$cluster)),
        columns=1:2, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous="blank"),
        axisLabels="none", switch="both") +
  theme_bw()

#K-means---------------------------------------------------------------------------
salaries6<-salaries[,4]
salaries6<-as.data.frame(cbind(salaries6,days.work))
names(salaries6)<-c("X2016.Gross.Pay.Received","days.work")
salaries6$X2016.Gross.Pay.Received[salaries6$X2016.Gross.Pay.Received==0]<-NA
salaries6<-na.omit(salaries6)
salaries6<-salaries6[sample(nrow(salaries6), 2000), ]

tot_withinss <- map_dbl(1:10, function(k){
  model <- kmeans(x = salaries6 %>% 
                    select(X2016.Gross.Pay.Received, days.work), 
                  centers = k)
  model$tot.withinss
})

elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)

sil_width <- map_dbl(2:10, function(k){ 
  model <- pam(x = salaries6 %>% 
                 select(X2016.Gross.Pay.Received, days.work), 
               k = k) 
  model$silinfo$avg.width
})
sil_df <- data.frame( k = 2:10, sil_width = sil_width )
elbow_plot <- elbow_df %>% 
  ggplot(aes(x = k, y = tot_withinss)) + 
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:10) +
  labs(title = "Elbow Method",
       y = "Total Within Sum of Square") +
  geom_vline(xintercept = 2, colour = "red", size = 1,
             linetype = "dashed")

silhouette <- sil_df %>% 
  ggplot(aes(x = k, y = sil_width)) + 
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 2:10) +
  geom_vline(xintercept = 2, colour = "red", size = 1,
             linetype = "dashed") +
  labs(y = "Silhouette Width", title = "Silhouette Analysis")
grid.arrange(elbow_plot, silhouette, ncol = 2,
             top = textGrob("Finding Optimal Number of Clusters",
                            gp=gpar(fontsize=20,font=3)))

pam_k2 <- salaries6 %>% 
  select(X2016.Gross.Pay.Received, days.work) %>% 
  pam(k = 2)
sil_plot <- silhouette(pam_k2)
plot(sil_plot, col=c("pink", "skyblue"), border=NA)
paste0("Average silhouette width is ", round(pam_k2$silinfo$avg.width, digits = 6))

model <- kmeans(salaries6 %>% 
                  select(X2016.Gross.Pay.Received, days.work), center = 2)
salary_km2 <- mutate(salaries6 %>% 
                       select(X2016.Gross.Pay.Received, days.work), cluster = model$cluster)
g <- salary_km2 %>% 
  ggplot(aes(x = X2016.Gross.Pay.Received, y = days.work, colour = factor(cluster)), frame = TRUE) +
  geom_point(size = .9, show.legend = F) +
  geom_smooth(method = "lm", show.legend = F) +
  labs(x = "X2016.Gross.Pay.Received", y = "days.work", title = "Cluster Analysis") +
  stat_ellipse(show.legend = F)
ggMarginal(g, type = "density", 
           groupColour = TRUE, groupFill = TRUE)


