library(FSA)
data <- read.csv("https://raw.githubusercontent.com/ywchiu/rcookbook/master/chapter5/engineer.csv")
headtail(data)
boxplot(Salary~Profession * Region,
        data = data, xlab = 'Profession * Region',
        ylab = "Salary",
        cex.axis = 0.7,
        main = "Salary v. Profession by Region")
boxplot(Salary~Profession, data = data, xlab = 'Profession', ylab = 'Salary', main = 'Salary v. Profession')
boxplot(Salary~Region, data = data, xlab = 'Profession', ylab = 'Region', main = 'Salary v. Region')
interaction.plot(data$Region, data$Profession, data$Salary,
                 type = "b", col = c(1:3), leg.bty = "o",
                 leg.bg = "white", lwd=2,
                 pch = c(16,17,18),
                 xlab = "Region", ylab = "Salary", main = "Interaction Between Variables")
dataanova <- aov(Salary~Profession * Region, data = data)
summary(dataanova)
TukeyHSD(dataanova)
plot(TukeyHSD(dataanova))
