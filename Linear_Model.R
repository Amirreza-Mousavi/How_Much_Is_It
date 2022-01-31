###Initialization
library(MASS)
library(dplyr)
library(ggplot2)
rsq <- function (x, y) cor(x, y) ^ 2
data("Cars93")

###Data preparation
dat=select(Cars93,Price,Horsepower,RPM,Length,Width,Luggage.room,Weight)
dat=na.omit(dat)
train_dat=dat[1:20,]
test_dat=dat[21:nrow(dat),]

###Setup linear model
mod=lm(Price~Horsepower+RPM+Length+Width+Luggage.room+Weight,train_dat)

###See how the model performs with test_dat
cat("The linear model characteristics: \n")
print(mod)
cat("\n")

a=coef(mod)
real=test_dat$Price
test_dat=mutate(test_dat[,-1],
      predicted = a["(Intercept)"]+a["Horsepower"]*Horsepower+
       a["RPM"]*RPM+a["Length"]*Length+
       a["Width"]*Width+a["Luggage.room"]*Luggage.room+
       a["Weight"]*Weight)

test_dat$reality=real


###calculate R^2 for the model
cat("R squared is: \n")
cat(rsq(test_dat$predicted,test_dat$reality))
cat("\n")

###Save plots as PDF
pdf("Linear_Model_Results.pdf")

###ERROR=?
plot((test_dat$predicted-test_dat$reality)*100/test_dat$reality,ylab = "% ERROR")

###Horsepower vs price(predictions and reality)
ggplot(test_dat)+
  geom_point(aes(x=Horsepower,y=reality)
                            ,color="#FFAA00") +
  geom_point(aes(x=Horsepower,y=predicted)
                            ,color="#00DDFF") +ylab("Price")

###Weight vs price(predictions and reality)
ggplot(test_dat)+
  geom_point(aes(x=Weight,y=reality)
             ,color="#FFAA00") +
  geom_point(aes(x=Weight,y=predicted)
             ,color="#00DDFF") +ylab("Price")

plot(mod)

dev.off()
