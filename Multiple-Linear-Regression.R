library(ISLR)
library(dplyr)
library(tidyverse)
library(GGally)
library(faraway)
data("Carseats")
attach(Carseats)
model <- lm(Sales ~ Income + Advertising + Price)
model
varnames <- c("Income", "Advertising", "Price")
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = pmax(1, cex.cor * r))
}
ggpairs(Carseats[,c(varnames)], axisLabels = "internal")
summary(model)
vif(model)
incmodel <- lm(Sales~Income)
incmodel
plot(incmodel)
summary(incmodel)
advmodel <- lm(Sales~Advertising)
advmodel
summary(advmodel)
plot(advmodel)
pricemodel <- lm(Sales~Price)
pricemodel
plot(pricemodel)
summary(pricemodel)
carseats2 <- Carseats
attach(carseats2)
model2 <- lm(Sales ~ Age + Urban + Population, data = carseats2)
model2
summary(model2)
varnames2 <- c("Age", "Urban", "Population")
ggpairs(carseats2[,c(varnames2)], axisLabels = "internal")
vif(model2)
