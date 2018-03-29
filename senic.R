senic = read.table("senic.txt")
head(senic)


senic<-senic[,-1]
head(senic)
names(senic)<-c("length","age","risk","culturing","Xray","beds","medschool","region","census","nurses","service")
senic
attach(senic)

medschool.f<-factor(medschool)
contrasts(medschool.f)

region.f<-factor(region)
contrasts(region.f)



# Forward variable selection
fit.0 <- lm(risk~1, data = senic)
add1(fit.0, risk~length+age+culturing+Xray+beds+medschool.f+region.f+census+nurses+service, test = "F")
fit.1 <- lm(risk~culturing, data = senic)
add1(fit.1, risk~length+age+culturing+Xray+beds+medschool.f+region.f+census+nurses+service, test = "F")
fit.2 <- lm(risk~culturing + length, data = senic)
add1(fit.2, risk~length+age+culturing+Xray+beds+medschool.f+region.f+census+nurses+service, test = "F")
fit.3 <- lm(risk~culturing+length+service, data = senic)
add1(fit.3, risk~length+age+culturing+Xray+beds+medschool.f+region.f+census+nurses+service, test = "F")
fit.4 <- lm(risk~culturing+length+service+Xray, data = senic)
add1(fit.4, risk~length+age+culturing+Xray+beds+medschool.f+region.f+census+nurses+service, test = "F")
fit.5 <- lm(risk~culturing+length+service+Xray+region.f, data = senic)
add1(fit.5, risk~length+age+culturing+Xray+beds+medschool.f+region.f+census+nurses+service, test = "F")
anova(fit.5)
fit.6 <- lm(risk~culturing*length*service*Xray*region.f, data = senic)
anova(fit.6)

anova(fit.1, fit.2)
anova(fit.2, fit.3)

# Backward variable selection 
fit.10 <- lm(risk~length+age+culturing+Xray+beds+medschool.f+region.f+census+nurses+service, data = senic)
drop1(fit.10, risk~length+age+culturing+Xray+beds+medschool.f+region.f+census+nurses+service, test = "F")
###drop age
fit.9 <- lm(risk~length+culturing+Xray+beds+medschool.f+region.f+census+nurses+service, data = senic)
drop1(fit.9,risk~length+culturing+Xray+beds+medschool.f+region.f+census+nurses+service, test = "F")
###drop census
fit.8 <- lm(risk~length+culturing+Xray+beds+medschool.f+region.f+nurses+service, data = senic)
drop1(fit.8, risk~length+culturing+Xray+beds+medschool.f+region.f+nurses+service, test = "F")
###drop beds
fit.7 <- lm(risk~length+culturing+Xray+medschool.f+region.f+nurses+service, data = senic)
drop1(fit.7, risk~length+culturing+Xray+medschool.f+region.f+nurses+service, test = "F")
###drop nurses
fit.6 <- lm(risk~length+culturing+Xray+medschool.f+region.f+service, data = senic)
drop1(fit.6, risk~length+culturing+Xray+medschool.f+region.f+service, test = "F")



# Stepwise variable selection 
fit.0 <- lm(risk~1, data = senic)
add1(fit.0,risk~length+age+culturing+Xray+beds+medschool.f+region.f+census+nurses+service, test = "F")
###add culturing
fit.1 <- lm(risk~culturing, data = senic)
drop1(fit.1, risk~culturing, test = "F")
add1(fit.1, risk~length+age+culturing+Xray+beds+medschool.f+region.f+census+nurses+service, test = "F")
###add length
fit.2 <- lm(risk~length+culturing, data = senic)
drop1(fit.2, risk~length+culturing, test = "F")
#keep both length and culturing
add1(fit.2, risk~length+age+culturing+Xray+beds+medschool.f+region.f+census+nurses+service, test = "F")
###add service
fit.3 <- lm(risk~length+service+culturing, data = senic)
drop1(fit.3, risk~length+service+culturing, test = "F")
###keep length, service and culturing
add1(fit.3, risk~length+age+culturing+Xray+beds+medschool.f+region.f+census+nurses+service, test = "F")
###add Xray
fit.4 <- lm(risk~length+service+Xray+culturing, data = senic)
drop1(fit.4, risk~length+service+Xray+culturing, test = "F")
###keep length,service,Xray and culturing
add1(fit.4, risk~length+age+culturing+Xray+beds+medschool.f+region.f+census+nurses+service, test = "F")
###add region.f
fit.5 <- lm(risk~length+service+Xray+region.f+culturing, data = senic)
drop1(fit.5, risk~length+service+Xray+region.f+culturing, test = "F")
###keep length,service,region.f,culturing and Xray
add1(fit.5, risk~length+age+culturing+Xray+beds+medschool.f+region.f+census+nurses+service, test = "F")
###stop

fit.5.b <- lm(risk~length+service++region.f+Xray+culturing, data = senic)
add1(fit.5.b, risk~length+age+culturing+Xray+beds+medschool.f+region.f+census+nurses+service, test = "F")



#1(intercept)+5+3(region has 3)=9

# checking for Model stability
fit.1 <- lm(risk~length+service+region.f+Xray+culturing, data = senic)
fit.2 <- lm(risk~length+service+region.f+Xray+culturing+medschool.f, data = senic)
plot(fitted.values(fit.1), fitted.values(fit.2), xlab = "Fitted values for Model 1", ylab = "Fitted values for Model 2", main = "Comparison of Predicted Values - Senic Data")
abline(0,1)

#AIC
AIC(fit.1)
AIC(fit.2)

#Interaction effect
fit.inter <- lm(risk~length*service*region.f*Xray*culturing*medschool.f, data = senic)
summary(fit.inter)
###no interaction effect


# variance inflation factors
length<-senic$length
service<-senic$service
Xray<-senic$Xray
culturing<-senic$culturing
x<-cbind(length,service,region.f,medschool.f,Xray,culturing)
solve(cor(x))


####exhaustive method

library(leaps)


all <- regsubsets(x=cbind(length,culturing,Xray,medschool.f,region.f,service), y=risk,  method = "exhaustive", all.best = FALSE, nbest = 20)
summary(all)
Cp <- summary(all)$cp
AdjR2 <- summary(all)$adjr2
SSRes <- summary(all)$rss
R2 <- summary(all)$rsq
Matrix <- summary(all)$which


# We want this for our table
p <- apply(Matrix,1, sum)
MSE <- SSRes/(113-p)

# Create a table
output <- cbind(p, Matrix, SSRes, R2, AdjR2, MSE, Cp)
colnames(output)[3:8] <- c("length", "culturing", "Xray", "medschool.f","region.f","service") 
output

# Sanity check: fit the last model (with all four parameters) by hand and compare the output you get:
fit <- lm(risk~length+culturing+Xray+service+region.f+medschool.f, data = senic)
summary(fit)

# Obtain least squares parameter estimates for all models
coef(all, 1:63)

# Making pretty labels
label <- array(dim = c(63,6))
for (j in 1:63){
	if (output[j,3]=="1") label[j,1] = "length" else label[j,1] = ""
	if (output[j,4]=="1") label[j,2] = paste(label[j,1], "culturing", sep = "") else label[j,2] = label[j,1]	
	if (output[j,5]=="1") label[j,3] = paste(label[j,2], "Xray", sep = "") else label[j,3] = label[j,2]	
	if (output[j,6]=="1") label[j,4] = paste(label[j,3], "service", sep = "") else label[j,4] = label[j,3]
      if (output[j,7]=="1") label[j,5] = paste(label[j,4], "medschool.f", sep = "") else label[j,5] = label[j,4]	
	if (output[j,8]=="1") label[j,6] = paste(label[j,5], "region.f", sep = "") else label[j,6] = label[j,5]


}
label[,6]

# Make R2 plot
library(calibrate)
plot(p, R2, pch = 16, main = "senic- R2 plot")
textxy(p, R2, label[,6], cex = 0.5)

# Cp plot
plot(p, Cp, pch = 16, main = "senic- Cp plot", ylim = c(0,150), xlim = c(0,11))
abline(0,1)
textxy(p, Cp, label[,6], cex = 0.6)

# MSE plot
plot(p, MSE, pch = 16, main = "senic - MSE plot")
textxy(p, MSE, label[,6], cex = 0.6)

# Adjusted R2 plot
plot(p, AdjR2, pch = 16, main = "senic - AdjR2 plot")
textxy(p, AdjR2, label[,6], cex = 0.6)


###final model
fit.00<-lm(risk~length+service+region.f+Xray+culturing+medschool.f,data=senic)
summary(fit.00)

###Check for influential points and leverage point and outlier