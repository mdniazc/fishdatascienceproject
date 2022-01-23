library(readxl)
Parameters.data <- read_excel("2020 - assignment 2.xlsx", sheet="Parameters")


install.packages("car")

install.packages("psych")

install.packages("carData")

library (car)

library(carData)

library(psych)


Parameters.data$CPM_Treatment.f<-factor(Parameters.data$CPM_Treatment)
is.factor(Parameters.data$CPM_Treatment.f)
Parameters.data$CPM_Treatment [1:36]

summary (lm(`Growth (g)` ~CPM_Treatment.f, data=Parameters.data))
summary (lm(`Growth (g)` ~factor(CPM_Treatment), data=Parameters.data))

plot(`Growth (g)` ~ factor(CPM_Treatment), data=Parameters.data)  

model1=lm(`Growth (g)` ~CPM_Treatment, data=Parameters.data) 
summary(model1)


model2= lm(`Growth (g)` ~factor(CPM_Treatment), data=Parameters.data)
summary (model2)


model3=lm (`Growth (g)` ~0+ factor(CPM_Treatment), data=Parameters.data) 
summary(model3)

model4=lm (`Growth (g)` ~0+ CPM_Treatment, data=Parameters.data) 
summary(model4)

summary(model4)$coefficients
summary(model4)$r.squared
summary(model4)$adj.r.squared
AIC(model4)
BIC(model4)
anova(model4)

plot(residuals(model4))

# variances along the regression line
hist(residuals(model4)) 


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

plotPlaneFancy(model4, plotx1 = "`Weight start (g)`", plotx2 = "`Weight end (g)`", cex=1)

library(lattice)

bwplot(`Growth (g)`~CPM_Treatment|TASK_GROUP, data=Parameters.data, fill="blue")

library(lattice)

bwplot(`Growth (g)`~factor(CPM_Treatment), data=Parameters.data, fill="blue")


