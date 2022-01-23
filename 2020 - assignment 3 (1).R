library(readxl)
data <- read_excel("2020 - assignment 3.xlsx", sheet = "habspecies", na = "NA")
head(data)
str(data)
colnames(data)

par(mfrow=c(1,3))
plot(data$E, data$N,col=factor(data$minkp),pch=16, xlab= "East", ylab = "North",main="mink")
plot(data$E, data$N,col=factor(data$martenp),pch=16, xlab= "East", ylab = "North",main="marten")
plot(data$E, data$N,col=factor(data$otterp),pch=16, xlab= "East", ylab = "North",main="otterp")

#install packages 

install.packages("car")
install.packages("psych")
install.packages("carData")
install.packages("mvtnorm")
install.packages("carData")


library (car)
library(carData)
library(psych)
library(lattice)
library(data.table)
library(plyr)
library(doBy)
library(car)
library(carData)
library(afex)
library(multcomp)
library(lsmeans)


library(readxl)
library(car)
library(effects)
library(lattice)
library(MASS)
library(plot3D)
library(rgl)
library(gmodels)

#Question 1

mytable1=xtabs(~minkp+martenp, data=data)
CrossTable(mytable1, expected=T, chisq=T, fisher = T, format = "SAS")
CrossTable(mytable1, expected=T, chisq=T,  format = "SAS")

mytable3=xtabs(~minkp+otterp, data=data)
CrossTable(mytable3, expected=T, chisq=T, fisher = T, format = "SAS")
CrossTable(mytable3, expected=T, chisq=T,  format = "SAS")

mytable5=xtabs(~martenp+otterp, data=data)
CrossTable(mytable5, expected=T, chisq=T, fisher = T, format = "SAS")
CrossTable(mytable5, expected=T, chisq=T,  format = "SAS")

mytable2=xtabs(~minkc+martenc, data=data)
CrossTable(mytable2, expected=T, chisq=T, fisher = T, format = "SAS")
CrossTable(mytable2, expected=T, chisq=T,  format = "SAS")

mytable4=xtabs(~minkc+otterc, data=data)
CrossTable(mytable4, expected=T, chisq=T, fisher = T, format = "SAS")
CrossTable(mytable4, expected=T, chisq=T,  format = "SAS")

mytable6=xtabs(~martenc+otterc, data=data)
CrossTable(mytable6, expected=T, chisq=T, fisher = T, format = "SAS")
CrossTable(mytable6, expected=T, chisq=T,  format = "SAS")

#Question 2

#create a data set for Figga river, to predict the current of Figga river

data.Figga <-subset(data, rivertype== "main")
data.FiggaRiver <-subset(data, river== "Figga")

# Model selection and statistical inference without log-transforming BD:
#Plot before regression
#Linear regression and ANova test

data1 = lm(current ~ riverwidth * oceandist *riverdepth*bankheight, data=data.Figga) 
summary(data1)
summary(data1)$adj.r.squared 


data2 = lm(current ~riverwidth + oceandist +riverdepth+bankheight, data=data.Figga) 
summary(data2)
Anova(data2, type = 3) # all effects are significant
summary(data2)$adj.r.squared 

data3 = lm(current ~riverwidth + oceandist +riverdepth+bankheight+bankheight*riverdepth, data=data.Figga) 
summary(data3)


riaz1=aov(current ~  riverwidth * oceandist *riverdepth*bankheight, data=data.FiggaRiver)
summary.lm(riaz1) 
riaz2=aov(current ~  riverwidth + oceandist +riverdepth+bankheight, data=data.FiggaRiver)
summary.lm(riaz2) 
riaz3=aov(current ~  riverwidth + oceandist +riverdepth+bankheight+bankheight*riverdepth, data=data.FiggaRiver)
summary.lm(riaz3) 

