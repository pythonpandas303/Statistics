library(dplyr)
library(ggvis)
library(caTools)
library(corrgram)
library(data.table)
library(ggplot2)
### Read CSV ###
file <- na.omit(file_path)
ggvis(file,~VAR1, ~VAR2, fill = ~VALUE) %>% layer_points()
file <- data.frame(file)
setnames(file, old=c("VAR1", "VAR2"), new=c("Var1", "Var2"))
corrgram(file, lower.panel = panel.shade, upper.panel = panel.cor)
split <- sample.split(Y=file$Var1, SplitRatio = 0.7)
trainingset <- subset(x=file, split==TRUE)
testset <- subset(x=file, split==FALSE)
model <- lm(formula = Var1 ~., data=trainingset)
model2 <- lm(formula=Var1~Var2, data=trainingset)
residuals <- as.data.frame(residuals(model))

ggplot(residuals, aes(residuals(model))) +
geom_histogram(fill= "blue", color="black")

preds <- predict(model, testset)
modelEval <- cbind(testset$Var1, preds)
colnames(modelEval) <- c('Actual', 'Predicted')
modelEval <- as.data.frame(modelEval)
### View Model ###

mse <- mean((modelEval$Actual—modelEval$Predicted)^2)
rmse <- sqrt(mse)
