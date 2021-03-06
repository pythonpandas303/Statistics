library(ggplot2)
library(reshape2)
assign_vector <- function(data, n=1000) {
p.5 <- replicate(n=n, expr=shapiro.test(sample(my.data, 5, replace=TRUE))$p.value)
p.10 <- replicate(n=n, expr=shapiro.test(sample(my.data, 10, replace=TRUE))$p.value)
p.1000 <- replicate(n=n, expr=shapiro.test(sample(my.data, 1000, replace=TRUE))$p.value)
p.df <- cbind(p.5, p.10, p.1000)
p.df <- as.data.frame(p.df)
colnames(p.df) <- c("5 samples","10 samples","1000 samples")
p.df.m <- melt(p.df)
p.df.m <- transform(p.df.m, variable = factor(variable, levels = c("5 samples","10 samples","1000 samples")))
return(p.df.m)  
}
n.rand <- 100000
n.test <- 10000
my.data <- rnorm(n.rand)
p.df.m <- assign_vector(my.data, n = n.test)


### Plots ###

plotIt <- function(data){
ggplot(p.df.m, aes(x = value)) + 
geom_histogram(breaks = seq(0, 1, by=.1)) + 
facet_grid(facets=variable ~ ., scales="free_y") + 
ylab("Count of p-values") +
xlab("p-values") +
theme(text = element_text(size = 16))
}
plotIt()

ggplot(NULL, aes(x=x, colour = distribution)) + 
     stat_function(fun=dnorm, data = data.frame(x = c(-6,6), distribution = factor(1))) + 
     stat_function(fun=dt, args = list( df = 20), data = data.frame(x = c(-6,6), distribution = factor(2)), linetype = "dashed") + 
     scale_colour_manual(values = c("blue","red"), labels = c("Normal","T-Distribution")) 
     
### New Set ###

my.data <- rt(n.rand, df = 20)

### Plot ###

plotIt <- function(data){
ggplot(p.df.m, aes(x = value)) + 
geom_histogram(breaks = seq(0, 1, by=.1)) + 
facet_grid(facets=variable ~ ., scales="free_y") + 
ylab("Count of p-values") +
xlab("p-values") +
theme(text = element_text(size = 16))
}
plotIt()

### Tail test ###

my.data <- rt(n.rand, df = 20)
my.data.2 <- rnorm(n.rand)
my.data <- my.data[which(my.data < 3 & my.data > -3)]
my.data <- c(my.data, my.data.2[which(my.data.2 < -3 | my.data.2 > 3)])
p.df.m <- assign_vector(my.data, n = n.test)

### Plot ###

plotIt <- function(data){
ggplot(p.df.m, aes(x = value)) + 
geom_histogram(breaks = seq(0, 1, by=.1)) + 
facet_grid(facets=variable ~ ., scales="free_y") + 
ylab("Count of p-values") +
xlab("p-values") +
theme(text = element_text(size = 16))
}
plotIt()

### New Set ###

my.data <- rnorm(n.rand)
my.data.2 <- rt(n.rand, df = 20)
my.data <- my.data[which(my.data < 3 & my.data > -3)]
my.data <- c(my.data, my.data.2[which(my.data.2 < -3 | my.data.2 > 3)])
p.df.m <- assign_vector(my.data, n = n.test)

### Plot ###

plotIt <- function(data){
ggplot(p.df.m, aes(x = value)) + 
geom_histogram(breaks = seq(0, 1, by=.1)) + 
facet_grid(facets=variable ~ ., scales="free_y") + 
ylab("Count of p-values") +
xlab("p-values") +
theme(text = element_text(size = 16))
}
plotIt()

### Skewed Set ###

my.data <- rlnorm(n.rand, 0, 0.4)
p.df.m <- assign_vector(my.data, n = n.test)

### Plot ###

plotIt <- function(data){
ggplot(p.df.m, aes(x = value)) + 
geom_histogram(breaks = seq(0, 1, by=.1)) + 
facet_grid(facets=variable ~ ., scales="free_y") + 
ylab("Count of p-values") +
xlab("p-values") +
theme(text = element_text(size = 16))
}
plotIt()
