library(readxl)
Parameters.data.riaz <- read_excel("2020 - assignment 2.xlsx", sheet="Parameters")

#install packages 

install.packages("car")
install.packages("psych")
install.packages("carData")

library (car)
library(carData)
library(psych)
library(lattice)

library(data.table)
library(plyr)
library(effects)

library(reshape2)

Parameters.data.riaz <- rename (Parameters.data.riaz,c("CPM_Treatment" = "CPM", "Growth (g)" = "Growth"))
Parameters.data.riaz <- rename (Parameters.data.riaz,c("Weight start (g)" = "Weight.start", "Weight end (g)" = "Weight.end"))


Parameters.data.riaz$Growth.log<-log2(Parameters.data.riaz$Growth)
is.factor(Parameters.data$Growth.log)


fit=aov(Growth.log~CPM,data=Parameters.data.riaz)
plot(fit) # plot predicted means and 95% CLs
summary(fit)

fit1=aov(Growth~CPM,data=Parameters.data.riaz)
plot(fit1) # plot predicted means and 95% CLs
summary(fit1)
summary.lm(fit1)




library(doBy)
summaryBy(Growth~CPM+Weight.start, data=Parameters.data.riaz,
          FUN=c(mean,sd,length))

summaryBy(Growth.log~factor(CPM)+Weight.start+Weight.end, data=Parameters.data.riaz,
          FUN=c(mean,sd,length))

bwplot(Growth~Weight.start|factor(CPM), data=Parameters.data.riaz, fill="blue")

bwplot(Growth~Weight.start, data=Parameters.data.riaz, fill="blue")
bwplot(Growth~factor(CPM), data=Parameters.data.riaz, fill="blue")



plot(effect(term="Growth:CPM", mod=fit1,default.levels=20),multiline=TRUE)


leveneTest(Parameters.data.riaz$Growth, Parameters.data.riaz$CPM)

leveneTest(Growth~factor(CPM), data=Parameters.data.riaz)


library(multcomp)

mcp=glht(fit1,linfct = mcp(Growth = "Tukey"))




