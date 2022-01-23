library(readxl)
data <- read_excel("2020 - assignment 3.xlsx", sheet = "habspecies", na = "NA")
head(data)
str(data)
colnames(data)



par(mfrow=c(1,3))
plot(data$E, data$N,col=factor(data$minkp),pch=16, xlab= "East", ylab = "North",main="mink")
plot(data$E, data$N,col=factor(data$martenp),pch=16, xlab= "East", ylab = "North",main="marten")
plot(data$E, data$N,col=factor(data$otterp),pch=16, xlab= "East", ylab = "North",main="otterp")



library(readxl)
library(car)
library(effects)
library(lattice)
library(MASS)
library(plot3D)
library(rgl)
library(gmodels)

#Question 1

mytable=xtabs(~minkp+martenp, data=data)
CrossTable(mytable, expected=T, chisq=T, fisher = T, format = "SAS")
CrossTable(mytable, expected=T, chisq=T,  format = "SAS")

mytable1=xtabs(~minkp+otterp, data=data)
CrossTable(mytable1, expected=T, chisq=T, fisher = T, format = "SAS")
CrossTable(mytable1, expected=T, chisq=T,  format = "SAS")

mytable2=xtabs(~martenp+otterp, data=data)
CrossTable(mytable2, expected=T, chisq=T, fisher = T, format = "SAS")
CrossTable(mytable2, expected=T, chisq=T,  format = "SAS")


#Question 2

# Model selection and statistical inference without log-transforming BD:
data1 = lm(current ~ river* riverwidth * oceandist *riverdepth*bankheight, data=data) 
summary(data1)

data3 = lm(current ~ factor(river)+ riverwidth + oceandist +riverdepth+bankheight, data=data) 
summary(data3)



Anova(data2, type = 3) # all effects are significant
summary(data2)$adj.r.squared 



