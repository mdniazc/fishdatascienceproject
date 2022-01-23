
library(readxl)
spines.data <- read_excel("2020 - assignment 1.xlsx", sheet="spines")


#data cleaning 
# create new dataset without missing data
newdata <- na.omit(spines.data)



#Summary Statitistics of the variables






newdata # inspect data
head(newdata)
summary(newdata$PS_left) # mean 1
summary(newdata$PS_right) # mean 1
summary(newdata$DS1) # mean 1
summary(newdata$DS2) # mean 1


            

# Are sticklebacks asymmetrical for pelvic spine length


t.test(newdata$DS1, newdata$DS2, paired = TRUE, alternative ="two.sided" )
t.test(newdata$PS_left, newdata$PS_right, paired = TRUE, alternative ="two.sided" )



# DO male and females differ in plate number

# create new dataset without Junivile from Sex varible

newdata.J <-newdata [!(newdata$Sex=='J'),] 


boxplot(PS_left~Sex, ylab="PS_left(cm)", col="lightblue", data = newdata.J )
boxplot(PS_right~Sex, ylab="PS_right(cm)", col="lightblue", data = newdata.J )


t.test(PS_right~Sex, var.equal=TRUE, alternative="two.sided", data=newdata.J)
t.test(PS_left~Sex, var.equal=TRUE, alternative="two.sided", data=newdata.J)

by (newdata.J$Plates_left, newdata.J$Sex, sd)
by (newdata.J$Plates_right, newdata.J$Sex, sd)






#Are all four Spines (DS1, DS2, PS_left and PS_right) postively correale in length?

data=newdata.J[,c("DS1", "DS2", "PS_left", "PS_right" )]

correlation::correlation(data,
                         include_factors = TRUE, methos="auto"
)





#Extra code for corellation #experimental code

data3=newdata[,c("DS1", "DS2", "PS_left", "PS_right" )]

correlation::correlation(data3,
                         include_factors = TRUE, methos="auto"
)

data2=newdata[,c("DS1", "DS2", "PS_left", "PS_right" )]

correlation::correlation(data2,
                         include_factors = TRUE, methos="auto"
)


library("Hmisc")
res2 <-rcorr(as.matrix(data3))
res2


