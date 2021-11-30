library(tidyverse)
library(ggpubr)
library(rstatix)
test <- data.frame(tgroup = rep(c("A", "B", "C"), each = 4),
                   Rtime = c(14,24,12,25,20,14,17,18,22,29,36,20))
summary(test)
ggboxplot(test, x = "tgroup", y = "Rtime")
model <- lm(Rtime ~ tgroup, data = test)
ggqqplot(residuals(model))
shapiro_test(residuals(model))
ggqqplot(test, "Rtime", facet.by = "tgroup")
plot(model, 1)
aov <- test %>% anova_test(Rtime ~ tgroup)
aov
