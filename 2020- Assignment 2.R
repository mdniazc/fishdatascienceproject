library(readxl)
Parameters.data <- read_excel("2020 - assignment 2.xlsx", sheet="Parameters")



#install packages 

install.packages("car")
install.packages("psych")
install.packages("carData")
install.packages("mvtnorm")

install.packages("survival")
install.packages("TH.data")
install.packages("MASS")
install.packages("emmeans")




library (car)
library(carData)
library(psych)
library(lattice)
library(data.table)
library(plyr)
library(doBy)
library(multcomp)
library(lsmeans)
library(emmeans)
library(tidyverse)
library(car)
library(carData)
library(afex)
library(effects)

# a. Try different models [y ~ x; log(y) ~ x and sqrt(y) ~ x] and check the assumptions of each
## First model

Parameters.data$CPM_Treatment.f<-factor(Parameters.data$CPM_Treatment)
is.factor(Parameters.data$CPM_Treatment.f)

Parameters.data$CPM_Treatment [1:36]

summary (lm(`Growth (g)` ~CPM_Treatment.f, data=Parameters.data))
summary (lm(`Growth (g)` ~factor(CPM_Treatment), data=Parameters.data))

#Rename Variables 
Parameters.data<- rename (Parameters.data,c("CPM_Treatment" = "CPM", "Growth (g)" = "Growth"))
Parameters.data<- rename (Parameters.data,c("Weight start (g)" = "Weight.start", "Weight end (g)" = "Weight.end"))

Parameters.data<- rename (Parameters.data,c("S-methylglutathione" = "methylglutathione"))


#One way ANOVA



summaryBy (Growth ~ CPM, data=Parameters.data,
          FUN=c(mean,sd,length)) 


#Graphically 
 boxplot(Growth ~ CPM, data=Parameters.data,  ylab="Growth")
 
 #T test 


 pairwise.t.test(Parameters.data$Growth, Parameters.data$CPM, p.adj="none",paired=F)
 
# one way ANOVA
 
 riaz1=aov(Growth ~ CPM, data=Parameters.data)
 summary.lm(riaz1) 
 summary (riaz1)
 
 
 #mODEL Diagnosis 
 # variance are homogenous 
 
 #leveneTest(Parameters.data$Growth, Parameters.data$CPM) code doesnt work on car data package 
 
 
 #Residualsa are normally Distributed 
 #normality test
 #Shaprio Wilk's W test
 
 
 shapiro.test(residuals(riaz1))
 
 #Graphically 
 
 
 studres=rstudent(riaz1)
 hist(studres,
      probability=T,
      col="lightgrey",
      xlim=c(-6,6),
      breaks=12,
      main="Distribution of Studentized Residuals",
      xlab="Studentized residuals")
 xfit=seq(-6,6,length=100)
 yfit=dnorm(xfit) # normal fit
 lines(xfit, yfit, col="red",lwd=2)
 
 
 fit=aov(Growth ~ CPM, data=Parameters.data)

 

 
 #gg<-glht(fit,linfct = mcp(CPM = "Tukey"))
 
 #mcp=glht(model4,linfct = mcp(CPM = "Tukey"))
 
 #mcp=glht(fit,linfct = mcp(CPM = "Tukey")) # code not working 
 
 ggplot2(fit)
 
 fit=aov(Growth ~ CPM, data=Parameters.data)
 mcp=glht(fit,linfct = mcp(CPM= "Tukey"))
 
 summary(fit)
 
 AIC(riaz1)
 BIC(riaz1)
 
 
 install.packages("tidyverse")
 
 
 
 ggplot(data = Parameters.data) + 
         geom_point(mapping = aes(x = CPM, y = Growth))
 
 install.packages("car", dependencies=TRUE)
 install.packages("pkgconfig", dependencies = TRUE)
 

 
 
 plot (effects(mod=fit, term="Growth"))
 
 #########################################################################################
 ##########################################################################################
 ############################################################################################
 
 
#linear regression model 

plot(`Growth (g)` ~ factor(CPM), data=Parameters.data)  

model1=lm(`Growth (g)` ~CPM_Treatment, data=Parameters.data) 
summary(model1)


model2= lm(`Growth (g)` ~factor(CPM_Treatment), data=Parameters.data)
summary (model2)


model3=lm (`Growth (g)` ~0+ factor(CPM_Treatment), data=Parameters.data) 
summary(model3)

model4=lm (Growth ~0+ CPM, data=Parameters.data) 
summary(model4)


summary(model4)$coefficients
summary(model4)$r.squared
summary(model4)$adj.r.squared
AIC(model4)
BIC(model4)
anova(model4)
anova(model1)

bwplot(`Growth (g)`~factor(CPM_Treatment), data=Parameters.data, fill="blue")

plot(`Growth (g)`~factor(CPM_Treatment), data=Parameters.data)


# Testing assumptions of linear model

plot(residuals(model4))
plot(model4)


#Normality of residuals

# variances along the regression line
hist(residuals(model4)) 

#The Shapiro Wilk's W test

shapiro.test(residuals(model4))



library(effects)
plot(effects(model4,term="CPM_Treatment",
             confidence.level = 0.95,partial.residuals=TRUE),
     band.colors="grey3", rug=F,
     residuals.color=adjustcolor("blue",alpha.f=0.2),
     residuals.pch=8, smooth.residuals=FALSE)


studres=rstudent(model4)
hist(studres,
     probability=T,
     col="lightgrey",
     xlim=c(-6,6),
     breaks=12,
     main="Distribution of Studentized Residuals",
     xlab="Studentized residuals")
xfit=seq(-6,6,length=100)
yfit=dnorm(xfit) # normal fit
lines(xfit, yfit, col="red",lwd=2)






## Second model with log-transformation


plot(log(`Growth (g)`) ~ factor(CPM_Treatment), data=Parameters.data) 

model5 <-  lm (log(`Growth (g)`) ~CPM_Treatment, data=Parameters.data) 
summary(model5)

model6=lm (log(`Growth (g)`) ~0+ CPM_Treatment, data=Parameters.data) 
summary(model6)



# Testing assumptions of log model

plot(residuals(model6))
plot(mod=model5)

# variances along the regression line
hist(residuals(model6)) 
#The Shapiro Wilk's W test
shapiro.test(residuals(model6))

studres=rstudent(model6)
hist(studres,
     probability=T,
     col="lightgrey",
     xlim=c(-6,6),
     breaks=12,
     main="Distribution of Studentized Residuals",
     xlab="Studentized residuals")
xfit=seq(-6,6,length=100)
yfit=dnorm(xfit) # normal fit
lines(xfit, yfit, col="red",lwd=2)



## Third model with square-root transformation
plot(sqrt(`Growth (g)`) ~ factor(CPM_Treatment), data=Parameters.data) 



model8 <-  lm (sqrt(`Growth (g)`) ~CPM_Treatment, data=Parameters.data) 
summary(model8)

model7=lm (sqrt(`Growth (g)`) ~0+ CPM_Treatment, data=Parameters.data) 
summary(model7)


# Testing assumptions of sqrt model

plot(residuals(model7))

#Normality of residuals

# variances along the regression line
hist(residuals(model7)) 

shapiro.test(residuals(model7))


studres=rstudent(model7)
hist(studres,
     probability=T,
     col="lightgrey",
     xlim=c(-6,6),
     breaks=12,
     main="Distribution of Studentized Residuals",
     xlab="Studentized residuals")
xfit=seq(-6,6,length=100)
yfit=dnorm(xfit) # normal fit
lines(xfit, yfit, col="red",lwd=2)


#Does CPM and weight affect the level of S-methylglutathione? In other words, 
#is there an effect of CPM on the level of S-methylglutathione, after accounting for the effect of weight?

#effect of CPM on the level of S-methylglutathione before considering for the effect of weight

fit5<- lm (`S-methylglutathione` ~ CPM_Treatment, data=Parameters.data)
summary (fit5)


fit6=lm (`S-methylglutathione` ~ 0+ CPM_Treatment, data=Parameters.data)
summary(fit6)


summary(fit6)$coefficients
summary(fit6)$r.squared
summary(fit6)$adj.r.squared
AIC(fit6)
BIC(fit6)
anova(fit6)



# Testing assumptions of linear model

plot(residuals(fit6))
#Normality of residuals

# variances along the regression line

hist(residuals(fit6)) 
#The Shapiro Wilk's W test
shapiro.test(residuals(fit6))



plot(effects(fit6,term="CPM_Treatment",
             confidence.level = 0.95,partial.residuals=TRUE),
     band.colors="grey3", rug=F,
     residuals.color=adjustcolor("blue",alpha.f=0.2),
     residuals.pch=8, smooth.residuals=FALSE)


studres=rstudent(fit6)
hist(studres,
     probability=T,
     col="lightgrey",
     xlim=c(-6,6),
     breaks=12,
     main="Distribution of Studentized Residuals",
     xlab="Studentized residuals")
xfit=seq(-6,6,length=100)
yfit=dnorm(xfit) # normal fit
lines(xfit, yfit, col="red",lwd=2)



#effect of CPM on the level of S-methylglutathione after considering the effect of weight
# multiple linear regression model 

plot(`S-methylglutathione` ~  factor(CPM_Treatment) , data=Parameters.data)
plot(`S-methylglutathione` ~  `Weight start (g)`, data=Parameters.data)
plot(`S-methylglutathione` ~  `Weight end (g)`, data=Parameters.data)



model9<- lm (`S-methylglutathione` ~ CPM_Treatment+ `Weight start (g)`+ `Weight end (g)`, data=Parameters.data)
summary (model9)


model10=lm (`S-methylglutathione` ~ 0+ CPM_Treatment+ `Weight start (g)`+ `Weight end (g)`, data=Parameters.data)
summary(model10)




model76<- lm (methylglutathione ~ CPM+ Weight.start+ Weight.end, data=Parameters.data)
summary (model76)


model77<- lm (methylglutathione ~ CPM+ Weight.start+ Weight.end+Weight.start*CPM+Weight.end*CPM, data=Parameters.data)
summary (model77)

model77<- lm (methylglutathione ~ CPM+ Weight.start+ Weight.end+Weight.start*CPM+Weight.end*CPM, data=Parameters.data)
summary (model77)

model81<- lm (methylglutathione ~ CPM+ Weight.start+ Weight.end+Weight.start:CPM+Weight.end:CPM, data=Parameters.data)
summary (model81)

fit78=aov(methylglutathione ~ CPM+ Weight.start+ Weight.end+Weight.start:CPM+Weight.end:CPM,data=Parameters.data)
summary(fit78)

fit79=aov(methylglutathione ~ CPM+ Weight.start+Weight.start:CPM,data=Parameters.data)
summary(fit79)

fit79=aov(methylglutathione ~ CPM+ Weight.end+Weight.end:CPM,data=Parameters.data)
summary(fit79)

fit80=aov(methylglutathione ~ CPM+ Weight.start+ Weight.end,data=Parameters.data)
summary(fit80)


summary(model10)$coefficients

summary(model10)$r.squared
summary(model10)$adj.r.squared
AIC(model10)
BIC(model10)
anova(model10)



# Testing assumptions of linear model

plot(residuals(model10))



#Normality of residuals

# variances along the regression line

hist(residuals(model10)) 

#The Shapiro Wilk's W test
shapiro.test(residuals(model10))



library(effects)
plot(effects(model10,term="CPM_Treatment",
             confidence.level = 0.95,partial.residuals=TRUE),
     band.colors="grey3", rug=F,
     residuals.color=adjustcolor("blue",alpha.f=0.2),
     residuals.pch=8, smooth.residuals=FALSE)


studres=rstudent(model10)
hist(studres,
     probability=T,
     col="lightgrey",
     xlim=c(-6,6),
     breaks=12,
     main="Distribution of Studentized Residuals",
     xlab="Studentized residuals")
xfit=seq(-6,6,length=100)
yfit=dnorm(xfit) # normal fit
lines(xfit, yfit, col="red",lwd=2)

########################################################################
###################################################################
##########################################################################



fit5<- lm (`S-methylglutathione` ~ CPM, data=Parameters.data)
summary (fit5)


fit6=lm (`S-methylglutathione` ~ 0+ CPM, data=Parameters.data)
summary(fit6)


summary(fit5)$coefficients
summary(fit5)$r.squared
summary(fit5)$adj.r.squared
AIC(fit5)
BIC(fit5)
anova(fit5)

# Testing assumptions of linear model

bwplot(`S-methylglutathione` ~ CPM, data=Parameters.data, fill="blue")


plot(residuals(fit5))
#Normality of residuals

# variances along the regression line

hist(residuals(fit5)) 
#The Shapiro Wilk's W test
shapiro.test(residuals(fit5))



library(effects)
plot(effects(fit6,term="CPM_Treatment",
             confidence.level = 0.95,partial.residuals=TRUE),
     band.colors="grey3", rug=F,
     residuals.color=adjustcolor("blue",alpha.f=0.2),
     residuals.pch=8, smooth.residuals=FALSE)


studres=rstudent(fit5)
hist(studres,
     probability=T,
     col="lightgrey",
     xlim=c(-6,6),
     breaks=12,
     main="Distribution of Studentized Residuals",
     xlab="Studentized residuals")
xfit=seq(-6,6,length=100)
yfit=dnorm(xfit) # normal fit
lines(xfit, yfit, col="red",lwd=2)


##################################################
#####################################################

riaz2=aov(methylglutathione ~ CPM,data=Parameters.data)
summary(riaz2)

riaz3=aov(methylglutathione ~ CPM+Weight.start+Weight.end, data=Parameters.data)
summary(riaz3)


fit7<- lm (`S-methylglutathione` ~ CPM+Weight.start+Weight.end, data=Parameters.data)
summary (fit7)


fit8=lm (`S-methylglutathione` ~ 0+ CPM+Weight.start+Weight.end, data=Parameters.data)
summary(fit8)


summary(fit7)$coefficients
summary(fit7)$r.squared
summary(fit7)$adj.r.squared
AIC(fit7)
BIC(fit7)
anova(fit7)

# Testing assumptions of linear model
bwplot(`S-methylglutathione` ~ CPM+Weight.end, data=Parameters.data, fill="blue")
bwplot(`S-methylglutathione` ~ CPM+Weight.start, data=Parameters.data, fill="blue")
bwplot(`S-methylglutathione` ~ CPM+Weight.end, data=Parameters.data, fill="blue")

bwplot(`S-methylglutathione` ~ Weight.end|CPM, data=Parameters.data, fill="blue")
bwplot(`S-methylglutathione` ~ Weight.start|CPM, data=Parameters.data, fill="blue")



plot(residuals(fit7))
#Normality of residuals

# variances along the regression line

hist(residuals(fit7)) 
#The Shapiro Wilk's W test

shapiro.test(residuals(fit7))



library(effects)
plot(effects(fit7,term="CPM_Treatment",
             confidence.level = 0.95,partial.residuals=TRUE),
     band.colors="grey3", rug=F,
     residuals.color=adjustcolor("blue",alpha.f=0.2),
     residuals.pch=8, smooth.residuals=FALSE)


studres=rstudent(fit7)
hist(studres,
     probability=T,
     col="lightgrey",
     xlim=c(-6,6),
     breaks=12,
     main="Distribution of Studentized Residuals",
     xlab="Studentized residuals")
xfit=seq(-6,6,length=100)
yfit=dnorm(xfit) # normal fit
lines(xfit, yfit, col="red",lwd=2)







