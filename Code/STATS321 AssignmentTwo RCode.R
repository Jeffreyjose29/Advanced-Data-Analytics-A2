### STATS321 ASSIGNMENT TWO ###
### NAME: JEFFREY JOSE
### ID: 1313512

################################################## QUESTION 1 ################################################################

##Initial set up for the dataset
install.packages("ggplot")
library(ggplot2)
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

#Summary of smoke dependent on if the mother was black or not
xtabs(~ black + smoke, data = phbirths)

#Summary for each variable in the dataset
summary(phbirths)

install.packages("gclus")
library(gclus)
library(car)

#Adds loess smoother in lower panel and the correlation in the upper panel
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) 
{ 
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits = digits)[1] 
  txt <- paste0(prefix, txt) 
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt) 
  text(0.5, 0.5, txt, cex = cex.cor * r) 
} 
pairs(~grams + black + educ + smoke + gestate, data = phbirths, main = "Data Relationship Plot", lower.panel = panel.smooth, upper.panel = panel.cor, pch = 20)

#Box-plot of weight of babies when the mother is not black vs mother is black and probability density plot of this
plot(phbirths$black, phbirths$grams, main = "Comparison Of Dist. Of Weights Of Babies From Non-Black & Black Mothers",
     xlab = "Is The Mother Black?", ylab = "Weight (g)")
ggplot(phbirths, aes(x = phbirths$grams, fill = phbirths$black)) + geom_density(alpha = 0.3) + ggtitle("Babies Weight Distribution Between Black And Non-Black") + xlab("Weight (g)") + ylab("Probability") + labs(fill = "Black?")

#Box-plot of weight of babies when the mother is a smoker vs non-smoker and probability density plot of this
plot(phbirths$smoke, phbirths$grams, main = "Comparison Of Dist. Of Weights Of Babies From Non-Smokers & Smoker",
     xlab = "Does the mother smoke?", ylab = "Weight (g)")
ggplot(phbirths, aes(x = phbirths$grams, fill = phbirths$smoke)) + geom_density(alpha = 0.3) + ggtitle("Babies Weight Distribution Between Smokers And Non-Smokers") + xlab("Weight (g)") + ylab("Probability") + labs(fill = "Smokes?")

########### MODEL MANIPULATION #############
fullModel.glm = glm(grams ~ ., data = phbirths)
summary(fullModel.glm)
#Diagnostic plot for the full model
par(mfrow = c(2,2))
plot(fullModel.glm)

BSModel.glm = glm(grams ~ black + smoke + black:smoke, data = phbirths)
summary(BSModel.glm)
plot(BSModel.glm)

BSGModel.glm = glm(grams ~ black + smoke + gestate + black:smoke, data = phbirths)
summary(BSGModel.glm)
chisq.test(phbirths$smoke, phbirths$grams)
chisq.test(phbirths$gestate, phbirths$grams)
chisq.test(phbirths$black, phbirths$grams)
chisq.test(phbirths$black:phbirths$smoke, phbirths$grams)

#### STEP-WISE REGRESSION ###########
big.lm = lm(grams ~ (black + smoke)^2 + (black + smoke)*(educ + gestate) ,data = phbirths)
summary(big.lm)
plot(big.lm)

small.lm = step(big.lm, trace = FALSE)
anova(small.lm)
summary(small.lm)

small.faraway.lm = lm(grams ~ black * (smoke) + log(gestate),data = phbirths)
anova(small.faraway.lm)
summary(small.faraway.lm)
plot(small.faraway.lm)

anova(small.lm, big.lm)

logGestate.lm = lm(grams ~ log(gestate) + black:smoke, data = phbirths)
summary(logGestate.lm)
anova(small.lm, logGestate.lm)
anova(logGestate.lm, small.lm)

test = lm(grams ~ black * (smoke + educ) + log1p(gestate),data = phbirths)
anova(test)
summary(test)

fit1.lm = lm(grams ~ black * (smoke) + black + smoke + educ + log(gestate), data = phbirths)
summary(fit1.lm)

#######
plot(small.lm)
phbirths[119,]
phbirths[1045,]
phbirths[152,]

updatePhBirths <- phbirths[-c(119, 1045, 94, 748, 752, 987),]
Ubig.lm = lm(grams ~ (black + smoke)^2 + (black + smoke)*(educ + gestate) ,data = updatePhBirths)
Usmall.lm = step(Ubig.lm, trace = FALSE)
plot(Usmall.lm)
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
plot(finney2$Volume, y = finney2$Rate, main = "Relation Between Rate of Inhalation vs. Volume of Air Inhaled", ylab = "Rate (L/s)", 
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

# #p-value for goodness of fit for the initial logit link model
# pchisq(deviance(binary.logit.glm.NULL)-deviance(binary.logit.glm),
#        df.residual(binary.logit.glm.NULL)-df.residual(binary.logit.glm),
#        lower.tail=FALSE)
# 
# #p-value for the goodness of fit for the log of the logit link model
# pchisq(deviance(binary.logit.glm.NULL)-deviance(log.binary.logit.glm),
#        df.residual(binary.logit.glm.NULL)-df.residual(log.binary.logit.glm),
#        lower.tail=FALSE)


#Part I
install.packages('roperators')
require(roperators)

volArray <- c()
ratArray <- c()
#For the initial logit link model
for (vol in seq(from = 0.0, to = 4, by = 0.25)){
  x <- solve(2.6491, 7.3324 - (3.8822*vol))
  #print(c(vol, x))
  if (x >= 0){
    volArray <- append(volArray, vol, after = length(volArray))
    ratArray <- append(ratArray, x, after = length(ratArray))
  }
}

for (rat in seq(from = 0.0, to = 4, by = 0.25)){
  y <- solve(3.8822, 7.3324 - (2.6491*rat))
  #print(c(rat, y))
  if (y >= 0){
    volArray <- append(volArray, y, after = length(volArray))
    ratArray <- append(ratArray, rat, after = length(ratArray))
  }
}

#For the log logit link model
for (LogVol in seq(from = 0.0, to = 4, by = 0.25)){
  x <- solve(4.562, 0.6778 - (5.179*LogVol))
  print(c(LogVol, x))
}

for (LogRat in seq(from = 0.0, to = 4, by = 0.25)){
  y <- solve(5.179, 0.6778 - (4.562*LogRat))
  print(c(LogRat, y))
}

#Plotting prediction points on the graph

#split the binary response into a seperate variable called binary and so it is easier to call
binary  = factor(finney2$Response, c(0,1), labels = c("No Vasoconstriction","Vasoconstriction"))
#plot the volume vs rate scatterplot defined by the response 
plot(finney2$Volume, y = finney2$Rate, main = "Relation Between Rate of Inhalation vs. Volume of Air Inhaled", ylab = "Rate (L/s)", 
     xlab = "Volume (L)", col = c("red","blue")[binary], pch = c(19,4)[binary], cex = 1.5)
legend(x = "topright", legend = levels(binary), col = c("red", "blue"), pch = c(19,4))
for(i in seq_along(volArray)){
  points.default(volArray[i], ratArray[i], type = "p", pch = 21, col = "black", bg = NA, cex = 1.25)
}

#Plotting points used earlier for prediction
pred.Vol <- c(1, 3.1, 0.4)
pred.Rate <- c(0.9, 1.2, 2.6)

for(j in seq_along(pred.Vol)){
  #print(c(pred.Vol[j], pred.Rate[j]))
  points.default(pred.Vol[j], pred.Rate[j], type = "p", pch = 23, col = "black", bg = "yellow", cex = 1.25)
}
