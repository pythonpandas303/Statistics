library(aod)
library(ggplot2)
library(pscl)
library(caret)
data <- read.csv("data.csv")
head(data)
model <- glm(Var1~Var2, data=data, family = "binomial")
summary(model)
ggplot(data, aes(x=Var1, y=Var2)) + 
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial))
confint.default(model)
pscl::pR2(model)["McFadden"]
new <- data.frame(Var1=c(range1:range2))
predict(model, new, type='response')
