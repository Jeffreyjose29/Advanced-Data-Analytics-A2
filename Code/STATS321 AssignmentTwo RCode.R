### STATS321 ASSIGNMENT TWO ###
### NAME: JEFFREY JOSE
### ID: 1313512

################################################## QUESTION 1 ################################################################

##Initial set up for the dataset
install.packages("faraway")
library(faraway)
data(phbirths)

#Checking dimension and the first five rows of the dataset
dim(phbirths)
phbirths[1:5,]

help("phbirths")

#Converting both the black and smoke variables to factors as they are TRUE/FALSE
phbirths$black = as.factor(phbirths$black)
phbirths$smoke = as.factor(phbirths$smoke)

#Summary for each variable in the dataset
summary(phbirths)

plot(phbirths$grams)
################################################### QUESTION 2 ###############################################################

# 0 = non-occurance of vasoconstriction
# 1 = occurance of vasoconstriction
getwd()
setwd("C:/Users/Jeffrey Jose/Desktop/Assignment Two")
finney2 = read.table("finney2.txt", sep = " ", header = TRUE)

#Part A
#split the binary response into a seperate variable called binary and so it is easier to call
binary  = factor(finney2$Response, c(0,1), labels = c("No Vasoconstriction","Vasoconstriction"))
#plot the volume vs rate scatterplot defined by the response 
plot(finney2$Volume, y = finney2$Rate, main = "Relation Between Rate of Inhalation vs. Volume of air inhaled", ylab = "Rate (L/s)", 
     xlab = "Volume (L)", col = c("red","blue")[binary], pch = c(19,4)[binary], cex = 1.5)
legend(x = "topright", legend = levels(binary), col = c("red", "blue"), pch = c(19,4))

#Part B
# Inhaling a higher quantity of air would on average result in a slower rate of inhalation as there is a larger volume of air to be inhaled. This a clear reason as to why there is a lack of observation in the top right quadrant of the plot. Inhaling a larger volume of air requires the lungs to do more 'work' and therefore the rate of inhalation decreases. Therefore on average, the larger the volume of air to be inhaled, the longer it takes to inhale hence resulting in a slower rate of inhalation.

#Part C
# I)
binary.logit.glm = glm(Response ~ Volume + Rate, data = finney2, family = binomial(link = logit))
summary(binary.logit.glm)
# II)
binary.probit.glm = glm(Response ~ Volume + Rate, data = finney2, family = binomial(link = probit))
summary(binary.probit.glm)
# III)
binary.cll.glm = glm(Response ~ Volume + Rate, data = finney2, family = binomial(link = cloglog))
summary(binary.cll.glm)

#Part D

#Part E
log.binary.logit.glm = glm(Response ~ log(Volume) + log(Rate), data = finney2, family = binomial(link = logit))
summary(log.binary.logit.glm)

log.binary.probit.glm = glm(Response ~ log(Volume) + log(Rate), data = finney2, family = binomial(link = probit))
summary(log.binary.probit.glm)

log.binary.cll.glm = glm(Response ~ log(Volume) + log(Rate), data = finney2, family = binomial(link = cloglog))
summary(log.binary.cll.glm)

#Part F

#Part G
# With the logit link initial model
new.values <- data.frame(Volume = c(1, 3.1, 0.4), Rate = c(0.9, 1.2, 2.6))
predict(binary.logit.glm, newdata = new.values, type = "response")

# With the log logit link second model
new.values.log <- data.frame(Volume = c(1, 3.1, 0.4), Rate = c(0.9, 1.2, 2.6))
predict(log.binary.logit.glm, newdata = new.values.log, type = "response")

#Part H
gof.link.logit = 1 - pchisq(deviance(binary.logit.glm), df.residual(binary.logit.glm))
gof.link.logit

gof.log.link.logit = 1 - pchisq(deviance(log.binary.logit.glm), df.residual(log.binary.logit.glm))
gof.log.link.logit


#The null model using the logit link
binary.logit.glm.NULL = glm(Response ~ 1, data = finney2, family = binomial(link = logit))

#p-value for goodness of fit for the initial logit link model
pchisq(deviance(binary.logit.glm.NULL)-deviance(binary.logit.glm),
       df.residual(binary.logit.glm.NULL)-df.residual(binary.logit.glm),
       lower.tail=FALSE)

#p-value for the goodness of fit for the log of the logit link model
pchisq(deviance(binary.logit.glm.NULL)-deviance(log.binary.logit.glm),
       df.residual(binary.logit.glm.NULL)-df.residual(log.binary.logit.glm),
       lower.tail=FALSE)


#Part I
new.data.df = data.frame(expand.grid(Volume = finney2$Volume, Rate = finney2$Rate), 0.1)
new.data.df[1:5, ]

#VOlume differs by 0.05
for (volume in seq(from = 0, to = 50, by = 0.05)){

}
# finney2$Volume
# finney2$Rate