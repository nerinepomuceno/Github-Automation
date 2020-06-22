#Setting working directory
setwd("D:\\Demo")
install.packages("rpart.plot")
#Loading needed packages
library(dplyr)
library(psych)
library(car)
library(lattice)
library(sm)
library(cluster)
library(elasticnet)
library(leaps)
library(rpart)
library(rpart.plot)
library(MASS)
library(readr)
library(ggplot2)

# DAY 1###################

# atomic vector examples ----
VecNum <- c(1, 3.5, 1E2, pi) 
class(VecNum)
typeof(VecNum)
VecInt <- c(1L, 3L, 5L)
VecLog <- c(T, F, 
            FALSE, FALSE, TRUE)
VecLog
VecChar <- c("lans", "TRUE",
             "pao")
VecCom <- c(3 + 4i, 0 + 1i)
VecFac <- factor(c("male","female",
                   "female","male"))
VecFac2 <- c("male","female",
             "female","male")
summary(VecNum)
summary(VecFac)
summary(VecFac2)

# Recursive Vectors -----------
VecList <- list(V1=VecNum, 
                V2=VecInt,
                V3=VecFac)

VecLans <- c(1.2, 3L, T, "F", 
             0+3i)

# Matrix ------
MyMat <- matrix(1:9, nrow = 3)
MyMat2 <- matrix(1:9, nrow = 3, 
                 byrow = T)

# Data frame ----
IDnum <- 1:3
Name <- c("lans","pao","erns")
Age <- c(21L, 19L, 18L)
MyDF <- data.frame(IDnum,Name,
                   Age)
colnames(MyDF)
rownames(MyDF)
attributes(MyDF)

?summary
??"linear regression"

# R as Calcu: Use of Operators -------
3+2
3+2/2
3+-2/2
3+-2^2/2
3+-2^1E1/2
x <- -3:4
y <- c(1,0)
length(x)
length(y)
x+3
x+y
z <- c(1,2,1)
x+z
VecLog
VecLog+2

Inf
-Inf
Inf-Inf
Inf/Inf
Inf/0
0/Inf

#install.packages("car","path")
#library(car)

demo(graphics)

seq(1,5,by = .5)
rep(c(1,2,3),3)
rep(c(1,2,3),times=3)
rep(c(1,2,3),each=3)

set.seed(14344)
rnorm(1)

x <- 1:9
dim(x) <- c(3,3)


MyCars <- mtcars
View(MyCars)
fix(MyCars)

square(4)

MySquare <- function(x) {
  x*x
}


MySquare <- function(x) {
  y <<- x*x
}

MySquare(7)

Y <- MySquare(6)

MySquare <- function(x) {
  y <- x*x
  return(y)
}
Y <- MySquare(9)



MySquare(1:2) #vectorized or element-wise operation



MySquare <- function(Lans) {
  Sab <- Lans*Lans
  return(Sab)
}

x <- MySquare(7)

# DAY 2###################
data()
?mtcars

str(mtcars)
View(mtcars)
head(mtcars,n=2)
tail(mtcars, n=3)
rownames(mtcars)
colnames(mtcars)

mtcars$mpg
mtcars$cyl
mtcars[,1:2]
mtcars[,c(1,3)]
mtcars[,c("mpg","disp")]
mtcars[1:5,c(1,3)]

mtcars1 <- mtcars[1:10,]
mtcars2 <- mtcars[-(1:10),]
mtcars2 <- mtcars[11:nrow(mtcars),]

mt_auto <- mtcars[cond???,] # all auto trans
mt_man <- mtcars[cond???,] # all manual trans

summary(mtcars)
hist(mtcars$mpg)
summary(mtcars$am)

MyCars <- mtcars
as.factor(MyCars$am)
MyCars$FacAM <- as.factor(MyCars$am)
str(MyCars)
summary(MyCars)

MyCars$FacAM <- NULL
MyCars$am <- as.factor(MyCars$am)
summary(MyCars)


complete.cases(mtcars1)
View(mtcars1)
mtcars1[2,2] <- NA
mtcars1[4,4] <- NA

complete.cases(mtcars1)

mtcars1[c(2,4),]

mtcars1[complete.cases(mtcars1),1:4]
mtcars1[!complete.cases(mtcars1),1:4]
mtcars1_comp <- na.omit(mtcars1)

names(MyCars)
colnames(MyCars)
colnames(MyCars)[9]
colnames(MyCars)[9] <- "FacAM"
colnames(MyCars)

# order(MyCars$mpg)
sort(MyCars$mpg, decreasing = T)

MyCars[,1:2]
SortedCars <- MyCars[order(MyCars$mpg),]
SortedCars[,1:2]


Mymat <- matrix(1:12,nrow=3)
Mymat
t(Mymat)
dim(Mymat) <- c(4,3)


x <- c(0, 2, 4, 6, 8, 10)
x[3]
x[-3]
x[c(-3,-6)]
x[c(-3,2)]
x[c(1.7, 4.25)]

y <- c(T,F,F)
x[y]
x[!y]

x
names(x) <- LETTERS[1:6]
x
x["A"]
x[c('A',"F")]
x[c('A',"lans")]
x <- NULL

MyList <- list(a=1:2,
               b=c('lans', 'pao', 'erns'),
               c=matrix(1:9,nrow=3),
               d=data.frame(v1=1:2,v2=LETTERS[1:2]))


MyList[[1]]
MyList$a
MyList[1]
MyList$d[1,]
MyList$d[,2]
MyList$b[3]

x <- c(0, NA, 4, 6, 8, 10)

sum(x, na.rm=T)



# control structures-------

x <- 6
if (x%%2==0) print("The number is even") 

x <- 6
if (x%%2==0) print("The number is even") 

x <- 6
if (x%%2==0) {
  print("The number is even") 
} else print("The number is odd")


x <- -6
if (x<=0) {
  print("Please provide number greater than 0!") 
} else if (x%%2==0) {
  print("The number is even.")
} else print("The number is odd.")


MyEven <- function(x) {
  if (x<=0) {
    print("Please provide number greater than 0!") 
  } else if (x%%2==0) {
    print("The number is even.")
  } else print("The number is odd.")
}

MyEven(9)
MyEven(2)
MyEven(-8)

MyEven(c(1,4))

x <- c(3,4)
ifelse(x%%2==0, "even", "odd")

x <- c(3,-5,4)
ifelse(x<=0, "try again", 
       ifelse(x%%2==0, "even", "odd"))

x <- "B"
switch(x,"A"=5, "B"=3, 1)


for (i in 1:5) {
  print(i)
}

x<-c(0,6,3,4)
for (i in x) {
  print(i)
}

x <- c(3,4)
for (i in x) {
  if (i%%2==0) {
    print("This is even.")
  } else print("This is odd.")
}

x <- c(3,4,9,7,6,4)
for (i in 1:length(x)) {
  if (x[i]%%2==0) {
    print("This is even.")
  } else print("This is odd.")
}


x <- 0
while (x<=10) {
  print(x)
  x <- x+1
}

x <- c(3,4,9,7,6,4)
i <- 1
while (i <= length(x)) {
  if (x[i]%%2==0) {
    print("This is even.")
  } else print("This is odd.")
  i <- i+1
}


x <- c(2,4,6,3,6,5)
for (i in 1:length(x)) {
  if (x[i]%%2==1) {
    print(i)
    break
  }
}

x <- c(2,4,6,3,6,5)
for (i in 1:length(x)) {
  if (x[i]%%2==1) next 
  print(x[i])
}


x <- 1
repeat {
  print(x)
  x <- x+1
  if (x==10) break
}

MyCars$FacAM==0
View(MyCars)
mt_auto <- MyCars[MyCars$FacAM==0,] # all auto trans
mt_man <- MyCars[MyCars$FacAM==1,] # all manual trans

Eff_aut <- MyCars[MyCars$FacAM==0 & 
                    MyCars$mpg >= 20,]

Eff_Lans <- MyCars[MyCars$FacAM==1 | 
                     MyCars$mpg >= 20,]


nrow(MyCars)
set.seed(14344)
sample(nrow(MyCars), size=10, 
       replace=T)


set.seed(14344)
MyCars[sample(nrow(MyCars), size=10), 
       1:2 ]


# aggregate-----
aggregate(MyCars$mpg, by=list(MyCars$FacAM), 
          FUN=mean, na.rm=T)



# Joins -----------

library(dplyr)

Num <- 1:10
Index <- letters[1:10]
Value <- Num*10

One <- data.frame(Num, Index, Value)

Num <- 6:15
Index <- letters[6:15]
Value1 <- Num*10

Two <- data.frame(Num, Index, Value1)

Num <- rep(1:5,2)
Value2 <- 6:15*10+1

Three <- data.frame(Num, Value2)

One
Two
Three

colnames(One)
colnames(Two)
colnames(Three)

See1 <- bind_rows(One, Two, Three)
View(See1)

inner_join(One,Three, by="Num")
left_join(One,Three, by="Num")
semi_join(One,Three, by="Num")
anti_join(One,Three, by="Num")
right_join(One,Three, by="Num")



semi_join(One,Two,by=c("Num","Index"))




## Day 3 Additional ONLY-------------------------

# Visuals ---------
old.par <- par(mar = c(0, 0, 0, 0))

# Creating a Graph
MyCars <- mtcars
plot(MyCars$wt, MyCars$mpg) 
abline(lm(mpg~wt, data=MyCars))
title("Regression of MPG on Weight")

# Simple Histogram
hist(MyCars$mpg)

# Colored Histogram with Different Number of Bins
hist(MyCars$mpg, breaks=12, col="red")
# Add a Normal Curve 
x <- MyCars$mpg 
h<-hist(x, breaks=10, col="red", xlab="Miles Per Gallon", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

# Kernel Density Plot
d <- density(MyCars$mpg) # returns the density data 
plot(d) # plots the results
# Filled Density Plot
plot(d, main="Kernel Density of Miles Per Gallon")
polygon(d, col="red", border="blue")


#Comparing Groups VIA Kernal Density
#The sm.density.compare( ) function in the sm package allows you to superimpose 
#the kernal density plots of two or more groups. The format is 
#sm.density.compare(x, factor) where x is a numeric vector and factor is the 
#grouping variable.

# Example: Compare MPG distributions for cars with 
# 4,6, or 8 cylinders
install.packages("sm")
library(sm)
attach(MyCars) # we are making the vars in MyCars available as objects in the 
# current environment
View(MyCars)
# create value labels 
cyl.f <- factor(cyl, levels= c(4,6,8),
                labels = c("4 cylinder", "6 cylinder", "8 cylinder")) 
# plot densities 
sm.density.compare(mpg, cyl, xlab="Miles Per Gallon")
title(main="MPG Distribution by Car Cylinders")
# add legend via mouse click
colfill<-c(2:(2+length(levels(cyl.f)))) 
legend(locator(1), levels(cyl.f), fill=colfill)


# Simple Dotplot
dotchart(MyCars$mpg,labels=row.names(MyCars),cex=.7,
         main="Gas Milage for Car Models", 
         xlab="Miles Per Gallon")

# Dotplot: Grouped Sorted and Colored
# Sort by mpg, group and color by cylinder 
x <- MyCars[order(MyCars$mpg),] # sort by mpg
x$cyl <- factor(x$cyl) # it must be a factor
x$color[x$cyl==4] <- "red"
x$color[x$cyl==6] <- "blue"
x$color[x$cyl==8] <- "darkgreen"	
dotchart(x$mpg,labels=row.names(x),cex=.7,groups= x$cyl,
         main="Gas Milage for Car Models\ngrouped by cylinder",
         xlab="Miles Per Gallon", gcolor="black", color=x$color)


# Simple Bar Plot 
counts <- table(MyCars$gear)
barplot(counts, main="Car Distribution", 
        xlab="Number of Gears")

# Simple Horizontal Bar Plot with Added Labels 
counts <- table(MyCars$gear)
barplot(counts, main="Car Distribution", horiz=TRUE,
        names.arg=c("3 Gears", "4 Gears", "5 Gears"))

# Stacked Bar Plot with Colors and Legend
counts <- table(MyCars$vs, MyCars$gear)
barplot(counts, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(counts))

# Grouped Bar Plot
counts <- table(MyCars$vs, MyCars$gear)
barplot(counts, main="Car Distribution by Gears and VS",
        xlab="Number of Gears", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)




# Create Line Chart
?Orange
# convert factor to numeric for convenience 
Orange$Tree <- as.numeric(Orange$Tree) 
ntrees <- max(Orange$Tree)

# get the range for the x and y axis 
xrange <- range(Orange$age) 
yrange <- range(Orange$circumference) 

# set up the plot 
plot(xrange, yrange, type="n", xlab="Age (days)",
     ylab="Circumference (mm)" ) 
colors <- rainbow(ntrees) 
linetype <- c(1:ntrees) 
plotchar <- seq(18,18+ntrees,1)


# add lines 
for (i in 1:ntrees) { 
  tree <- subset(Orange, Tree==i) 
  lines(tree$age, tree$circumference, type="b", lwd=1.5,
        lty=linetype[i], col=colors[i], pch=plotchar[i]) 
} 

# add a title and subtitle 
title("Tree Growth", "example of line plot")

# add a legend 
legend(xrange[1], yrange[2], 1:ntrees, cex=0.8, col=colors,
       pch=plotchar, lty=linetype, title="Tree")


# Pie Chart from data frame with Appended Sample Sizes
mytable <- table(MyCars$cyl)
lbls <- paste(names(mytable), " Cylinders", "\n", mytable, sep="")
pie(mytable, labels = lbls, 
    main="Pie Chart of Species\n (with sample sizes)")


# Pie Chart with Percentages
str(mytable)
slices <- rep(0,3)
for (i in 1:3) {
  slices[i] <- mytable[[i]]
} 
lbls <- names(mytable)
lbls <- paste(names(mytable), " Cylinders")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, "\n", pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # add % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Countries")


# Boxplot of MPG by Car Cylinders 
boxplot(mpg~cyl,data=MyCars, main="Car Milage Data", 
        xlab="Number of Cylinders", ylab="Miles Per Gallon")

# Notched Boxplot of Tooth Growth Against 2 Crossed Factors
# boxes colored for ease of interpretation 
?ToothGrowth
boxplot(len~supp*dose, data=ToothGrowth, notch=TRUE, 
        col=(c("gold","darkgreen")),
        main="Tooth Growth", xlab="Suppliment and Dose")


# Simple Scatterplot
# using mycars, with vars already in environment
plot(wt, mpg, main="Scatterplot Example", 
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)
# Add fit lines
abline(lm(mpg~wt), col="red") # regression line (y~x) 
lines(lowess(wt,mpg), col="blue") # lowess line (x,y)

# Enhanced Scatterplot of MPG vs. Weight 
# by Number of Car Cylinders 
install.packages(car)
library(car) 
scatterplot(mpg ~ wt | cyl, data=MyCars, 
            xlab="Weight of Car", ylab="Miles Per Gallon", 
            main="Enhanced Scatter Plot", 
            labels=row.names(mtcars))


# Basic Scatterplot Matrix
pairs(~mpg+disp+drat+wt,data=MyCars, 
      main="Simple Scatterplot Matrix")

# Scatterplot Matrices from the lattice Package 
library(lattice)
super.sym <- trellis.par.get("superpose.symbol")
splom(mtcars[c(1,3,5,6)], groups=cyl, data=mtcars,
      panel=panel.superpose, 
      key=list(title="Three Cylinder Options",
               columns=3,
               points=list(pch=super.sym$pch[1:3],
                           col=super.sym$col[1:3]),
               text=list(c("4 Cylinder","6 Cylinder","8 Cylinder"))))
attach(MyCars)

# OPTIONAL:
# 4 figures arranged in 2 rows and 2 columns
par(old.par)
par(mfrow=c(2,2))
plot(wt,mpg, data=MyCars, main="Scatterplot of wt vs. mpg")
plot(wt,disp, data=MyCars, main="Scatterplot of wt vs disp")
hist(wt, data=MyCars, main="Histogram of wt")
boxplot(wt, data=MyCars, main="Boxplot of wt")

# 3 figures arranged in 3 rows and 1 column
par(mfrow=c(3,1)) 
hist(wt, data=MyCars)
hist(mpg, data=MyCars)
hist(disp, data=MyCars)


# Transformations ------
install.packages("car")
install.packages("MASS")
install.packages("rcompanion")

library(car)
library(MASS)
library(rcompanion)


## Day 3 DEMO-------------------------
install.packages("sm","lattice", "psych",
                 "car")

library(lattice)
library(sm)
library(psych)
library(car)



#Loading needed dataset
data(iris)
str(iris)
View(iris)

#Summary measures
describe(iris[,-5])
describeBy(iris[,-5], group = "Species")

#Histogram
hist(iris$Sepal.Length, breaks = 5)
hist(iris$Sepal.Length, breaks = 20)
hist(iris$Sepal.Length, breaks = 35)
histogram( ~ Sepal.Length | Species, data = iris, breaks = 5)
histogram( ~ Sepal.Length | Species, data = iris, breaks = 20)
histogram( ~ Sepal.Length | Species, data = iris, breaks = 35)



#Smoothing
den_gauss <- density(x = iris$Sepal.Length, kernel = "gaussian")
plot(den_gauss)
den_epanech <- density(x = iris$Sepal.Length, kernel = "epanechnikov")
plot(den_epanech)
den_biwgt <- density(x = iris$Sepal.Length, kernel = "biweight")
plot(den_biwgt)

sm.density.compare(iris$Sepal.Length, iris$Species,col=c("blue","black","red"))
legend(locator(1), levels(iris$Species), fill=c("blue","black","red"))

#Boxplot
boxplot(iris$Sepal.Length)
boxplot(Sepal.Length~Species,data=iris)

#Scatterplot
plot(iris$Petal.Width,iris$Sepal.Length)
scatterplot(Sepal.Length ~ Petal.Width | Species, data = iris)
scatterplot(Sepal.Length ~ Petal.Width | Species, data = iris, smoother = FALSE, reg.line = FALSE)
scatterplotMatrix(~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width|Species, data=iris)
scatterplotMatrix(~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width|Species, data=iris,
                  smoother = FALSE, reg.line = FALSE)


#Loading needed dataset
data(AirPassengers)
str(AirPassengers)
View(AirPassengers)
?plot.ts
#Timeplot
plot.ts(AirPassengers)

#ACF/PACF plots
acf(AirPassengers)
pacf(AirPassengers)

#Pricipal Component Analysis
#Original data
ir.pca <- prcomp(iris[, 1:4], center = TRUE, scale. = TRUE) 

#Scree plot
plot(ir.pca, type='l')

#Print method
print(ir.pca)

#Summary method
summary(ir.pca)

#Eigenvalues
ir.pca$sdev

#PC loadings
ir.pca$rotation

#PC scores
ir.pca$x

#Scatterplot of PC1 and PC2
plot(ir.pca$x[,2],ir.pca$x[,1])
scatterplot(ir.pca$x[,2] ~ ir.pca$x[,1] | iris[,5], smoother = FALSE, reg.line = FALSE)


#Attaching PC scores and Species
iris2 <- data.frame(Species=iris[,5],ir.pca$x)

#Removing Species = setosa
iris3 <- subset(iris2,Species != "setosa")

#Scatterplot of PC1 and PC2 without setosa
scatterplot(iris3$PC3 ~ iris3$PC1 | iris3$Species, smoother = FALSE, reg.line = FALSE)



# Day 4 ----------------
install.packages("cluster", "elasticnet")
library(cluster)
library(elasticnet)





#Loading needed packages
library(dplyr)
library(psych)
library(car)
library(lattice)
library(sm)
library(cluster)
library(elasticnet)


#Loading needed data
iris2 <- iris
iris2$Species <- NULL

#k-means clustering
kmeans.result <- kmeans(iris2, 3)
kmeans.result
#Checking cluster results
table(iris$Species, kmeans.result$cluster)

#Plotting cluster center
plot(iris2[c("Sepal.Length",
             "Sepal.Width")],
     col = kmeans.result$cluster)

points(kmeans.result$centers[,c("Sepal.Length","Sepal.Width")],
       col=1:3, pch=8, cex=2)
plot(iris2,col = kmeans.result$cluster)

#Agglomerative nesting
agn <- agnes(iris[,1:4], 
             metric = "euclidean", 
             stand = TRUE)
agn
agn$order
plot(agn,labels= iris$Species)


#Divisive Analysis
dv <- diana(iris[,1:4], 
            metric = "manhattan", 
            stand = TRUE)
dv$order
print(dv)
plot(dv,labels= iris$Species)
rect.hclust(dv,k=3)
clusters <- cutree(dv,k=3)
clusters
plot(iris2,col=clusters)

#model based clustering
library(mclust)
fitM <- Mclust(iris2)
fitM
summary(fitM)
plot(fitM)





#Scatterplot
scatterplotMatrix(~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width|Species, data=iris,
                  smoother = FALSE, reg.line = FALSE)


#Pricipal Component Analysis
ir.pca <- prcomp(iris[, 1:4],
                 center = TRUE,
                 scale. = TRUE) 

#Scree plot
plot(ir.pca, type='l')


#PC loadings
ir.pca$rotation

#Sparse PCA
sparse.pca.result <- 
  spca(iris[,1:4],
       K = 2,
       type = "predictor",
       sparse = "varnum",
       para = c(3, 1))

#Summary method
ir.pca$rotation
summary(ir.pca)

#Sparse PC loadins
sparse.pca.result$loadings
sparse.pca.result$pev



#Loading needed data
data(mtcars)
View(mtcars)

#Multiple linear regression model
lm.fit <- lm(mpg ~ hp + wt + am, data = mtcars)
summary(lm.fit)

#Coefficient
lm.fit$coefficients

#Testing for independence of error terms
durbinWatsonTest(lm.fit)

#Testing for constant error variance
ncvTest(lm.fit)

#Testing for normality of error terms
shapiro.test(lm.fit$residuals)

#Checking for multicollinearity
vif(lm.fit)

#identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(mtcars)-length(lm.fit$coefficients)-2)) 

# Cook's D plot
plot(lm.fit, which=4, cook.levels=cutoff)

#Obtaining all numeric variables
mtcars.num <- mtcars[c("mpg","disp","hp","drat","wt","am","gear")]

#Simple intercept model
lm.fit.null <- lm(mpg ~ 1, data = mtcars.num)
summary(lm.fit.null)

#Full model with all numeric variables
lm.fit.full <- lm(mpg ~ ., data = mtcars.num)
summary(lm.fit.full)

#ANOVA
anova(lm.fit,lm.fit.full)

#Forward selection
step(lm.fit.null, scope=list(lower=lm.fit.null, upper=lm.fit.full), direction="forward")

#Backward selection
step(lm.fit.full, data=mtcars.num, direction="backward")

#Stepwise selection
step(lm.fit.null, scope = list(upper=lm.fit.full), data=mtcars.num, direction="both")

#New observations for prediction
mtcars.new <- mtcars.num[1:5,-1]

#Prediction
predict(lm.fit,mtcars.new)

#Importing needed data
#stat <- read.csv("Stat_Pass.csv",header=TRUE)
Stat_Pass <- read_csv("C:/Users/acer/OneDrive - University of the Philippines/Stat_Pass.csv")
stat <- Stat_Pass
head(stat)

#Logistic regression
log.fit = glm(STATUS ~ NUM_UNITS + HRS_STUDY + NUM_ORG + VITAMINS + HOURS_SLEEP, data=stat,family=binomial)
summary(log.fit)

#Coefficients
log.fit$coefficients

#Odds ratio
round(exp(log.fit$coefficients),2)


#Importing needed data
#tb_data <- read.csv("Tuberculosis.csv",header=TRUE)
Tuberculosis <- read_csv("C:/Users/acer/OneDrive - University of the Philippines/Tuberculosis.csv")
tb_data <- Tuberculosis
#Poisson regression
poi.fit <- glm(Tuberculosis ~ PerCapitaGovtExpHealth + CleanWater + CleanToilet + PopDen + AdMortMale, data=tb_data, family="poisson")
summary(poi.fit)

#Coefficients
poi.fit$coefficients


#Impact multiplier
exp(poi.fit$coefficients)

#Tree-based method
tree.fit <- rpart(mpg ~ hp + wt + am, data = mtcars)
print(tree.fit)
rpart.plot(tree.fit)

#Pruning tree 
tree.fit.prune <- prune(tree.fit,0.60)
print(tree.fit.prune)
rpart.plot(tree.fit.prune)

#Deriving data for discriminant analysis
set.seed(57317)
training_sample <- sample(c(TRUE, FALSE), nrow(iris), replace = TRUE, prob = c(0.6,0.4))
train <- iris[training_sample,c(2,4,5) ]
test <- iris[!training_sample,c(2,4,5) ]
#View(iris)
#Linear discriminant analysis
lda.fit <- lda(Species ~ ., train)
lda.fit

#Plotting how observations are grouped together
plot(lda.fit, col = as.integer(train$Species))

#Histogram of LD1
plot(lda.fit, dimen = 1, type = "b")

#Predicting groups from training data
lda.train <- predict(lda.fit)
train$lda <- lda.train$class
table(train$lda,train$Species)

#Predicting groups from test data
lda.test <- predict(lda.fit,test)
test$lda <- lda.test$class
table(test$lda,test$Species)



#GGPLOT2

# Use data from data.frame
qplot(mpg, wt, data = mtcars)
qplot(mpg, wt, data = mtcars, colour = cyl)
qplot(mpg, wt, data = mtcars, size = cyl)
qplot(mpg, wt, data = mtcars, facets = vs ~ am)


qplot(1:10, rnorm(10), colour = runif(10))
qplot(1:10, letters[1:10])
mod <- lm(mpg ~ wt, data = mtcars)
qplot(resid(mod), fitted(mod))

# Use different geoms
qplot(mpg, wt, data = mtcars, geom = "path")
qplot(factor(cyl), wt, data = mtcars, geom = c("boxplot", "jitter"))
qplot(mpg, data = mtcars, geom = "dotplot")


# To set aesthetics, wrap in I()
qplot(mpg, wt, data = mtcars, colour = I("red"))

# qplot will attempt to guess what geom you want depending on the input
# both x and y supplied = scatterplot
qplot(mpg, wt, data = mtcars)
# just x supplied = histogram
qplot(mpg, data = mtcars)
# just y supplied = scatterplot, with x = seq_along(y)
qplot(y = mpg, data = mtcars)



#Library to run----
#R Version 3.5.3
library(dplyr)
library(psych)
library(car)
library(lattice)
library(sm)
library(cluster)
library(elasticnet)
library(leaps)
library(rpart)
library(rpart.plot)
library(MASS)
library(vcd)
library(AER)
library(ggpubr)
library(caTools)
library(rms)
library(ResourceSelection)
library(blorr)
library(tidyverse)
library(broom)
library(readxl)

#Data----
setwd("C:/Users/acer/Desktop")
#data<- read_excel("C:/Users/acer/Desktop/Clean Data Recreation.xlsx")
data<- read_excel("C:/Users/acer/Desktop/School '19/Stat 150 Recreational Sites in Iloilo City/SP/Clean Data Recreation.xlsx")
c <- data[-401,]
describe(c)
summary(c)

t <- c[,c(2,3,5,7,8,9,11,14,16,18,22,24,26,27,79,80,81,82,83,84,28:40)]
colnames(t) <- c("Age","Sex", "Status","Highest", "Income","IncomeCat", "District","Preference", "Visits", "Rate","Alone", "Origin", "Mode","Travel","Activities","Facilities","Aesthetic","Safety","Food","Overall","Stroll","Sports","Eat","Fiesta","Jog","Bike","Skate","Relax","Sight","Explore","Meet","Picture","Dance")

t$Preference <- factor(t$Preference,levels=c(0,1),labels=c("Old","New"))
t$Preference <- relevel(t$Preference,ref = "Old")#To predict the odds of Newly established site

t$Sex <- relevel(factor(t$Sex),ref="Male")#Odds of female relative to males
t$Status <- relevel(factor(t$Status),ref="Unemployed/Retired")
t$Alone <- relevel(factor(t$Alone),ref="Alone")
t$Highest <- relevel(factor(t$Highest),ref="Elementary Level")
t$Origin <- relevel(factor(t$Origin),ref="Others")
t$Mode <- relevel(factor(t$Mode),ref="By Privately Owned Vehicle")

table(t$Mode)

#Plots----
shapiro.test(t$Overall)
nortest::lillie.test(t$Age)
nortest::lillie.test(t$Income)


histogram( ~ Age, data = t, breaks = 10)
histogram( ~ Age | Alone, data = t, breaks = 10)
histogram( ~ Age | Status, data = t, breaks = 10)
histogram( ~ Age | Highest, data = t, breaks = 5)

histogram( ~ Income, data = t, breaks = 20)
histogram( ~ Income| Sex, data = t, breaks = 50)
histogram( ~ Income| Alone, data = t, breaks = 50)
histogram( ~ Income | Status, data = t, breaks = 50)
histogram( ~ Income | Highest, data = t, breaks = 50)


scatterplotMatrix(~Activities+Facilities+Aesthetic+Safety+Food+Overall, data= t)
scatterplotMatrix(~Income+Travel, data= t)
scatterplotMatrix(~Rate+Age+Income+Travel|Stroll, data= t)
scatterplotMatrix(~Rate+Age+Income+Travel|Preference, data= t)
scatterplotMatrix(~Rate+Age+Income+Travel|Status, data= t[t$Visits=="Rarely",])
View(t[t$Status=="Self-Employed",])

plot(Overall~ factor(Preference), data = t)
t.test(Overall~ factor(Preference), data = t)


#Test for normality of variables
ggdensity(t$Travel, 
          main = "Density plot of Overall Ratings",
          xlab = "Ratings")

ggqqplot(t$Overall)
qqPlot(t$Overall)

shapiro.test(log(t$Overall))

cor(t$Rate,t$Travel)


#Logistic regression----

#Competing model(Alone and demographics)
log.fit = glm(Preference ~ Age+ Income+Highest+Sex+Status+Alone, data=t,family=binomial)
summary(log.fit)
Anova(log.fit)

#fit null
log.fit0 = glm(Preference ~1, data=t,family=binomial)
summary(log.fit0)

#forward selection
step(log.fit0, scope=list(lower=log.fit0, upper=log.fit), direction="forward")
#Backward selection
step(log.fit, data=t, direction="backward")
#Stepwise selection
step(log.fit0, scope = list(upper=log.fit), data=t, direction="both")


#Manual forward model selection
log.fit1 = glm(Preference ~Alone, data=t,family=binomial)
summary(log.fit1)
Anova(log.fit1)

log.fit2 = glm(Preference ~Alone+Income, data=t,family=binomial)
summary(log.fit2)
Anova(log.fit2)

log.fit3 = glm(Preference ~Alone+Income+Age, data=t,family=binomial)
summary(log.fit3)
Anova(log.fit3)

log.fit4 = glm(Preference ~Alone+Income+Age+Highest, data=t,family=binomial)
summary(log.fit4)
Anova(log.fit4)

likfit <- -2*(logLik(log.fit3)-logLik(log.fit4))#Local Test
p_Highest<-1-pchisq(likfit,df=1)#p-value of the local test
as.numeric(p_Highest)#not significant, don't include Highest in the model

log.fit5 = glm(Preference ~Alone+Income+Age+Highest+Sex, data=t,family=binomial)
summary(log.fit5)
Anova(log.fit5)

likfit1 <- -2*(logLik(log.fit4)-logLik(log.fit5))#Local Test
p_Sex<-1-pchisq(likfit1,df=1)#p-value of the local test
as.numeric(p_Sex)#not significant, don't include sex in the model

log.fit6= glm(Preference ~Alone+Income+Age+Highest+Status, data=t,family=binomial)
summary(log.fit6)
Anova(log.fit6)

likfit3 <- -2*(logLik(log.fit4)-logLik(log.fit6))#Local Test
p_Status<-1-pchisq(likfit3,df=1)#p-value of the local test
as.numeric(p_Status)#not significant, don't include Highest in the model

#End of first model

#First competing model
compete1 <- glm(Preference ~Alone+Income+Age, data=t,family=binomial)
summary(compete1)
Anova(compete1)
compete1$coefficients


#Competing model(Demographics)
log.fit = glm(Preference ~ Age+ Sex+ Income+Status+Highest, data=t,family=binomial)
summary(log.fit)
Anova(log.fit)

#fit null
log.fit0 = glm(Preference ~1, data=t,family=binomial)
summary(log.fit0)

#forward selection
step(log.fit0, scope=list(lower=log.fit0, upper=log.fit), direction="forward")
#Backward selection
step(log.fit, data=t, direction="backward")
#Stepwise selection
step(log.fit0, scope = list(upper=log.fit), data=t, direction="both")

#Second Competing Model
compete2 <- glm(Preference ~Income+Age+Highest, data=t,family=binomial)
summary(compete2)
Anova(compete2)

#Competing model(Factors)
log.fit = glm(Preference ~ Alone, data=t,family=binomial)
summary(log.fit)
Anova(log.fit)

#fit null
log.fit0 = glm(Preference ~1, data=t,family=binomial)
summary(log.fit0)

#forward selection
step(log.fit0, scope=list(lower=log.fit0, upper=log.fit), direction="forward")
#Backward selection
step(log.fit, data=t, direction="backward")
#Stepwise selection
step(log.fit0, scope = list(upper=log.fit), data=t, direction="both")

compete3 <- glm(Preference ~Alone, data=t,family=binomial)
summary(compete3)
Anova(compete3)

#Competing model (Type of activity usually done)
log.fit = glm(Preference ~factor(Stroll)+factor(Sports)+factor(Eat)+factor(Fiesta)+factor(Jog)+factor(Bike)+factor(Relax)+factor(Sight)+factor(Explore)+factor(Meet)+factor(Picture)+factor(Dance), data=t,family=binomial)
summary(log.fit)
Anova(log.fit)

#fit null
log.fit0 = glm(Preference ~1, data=t,family=binomial)
summary(log.fit0)

#forward selection
step(log.fit0, scope=list(lower=log.fit0, upper=log.fit), direction="forward")
#Backward selection
step(log.fit, data=t, direction="backward")
#Stepwise selection
step(log.fit0, scope = list(upper=log.fit), data=t, direction="both")

#Third Competing model
compete4 <-glm(formula = Preference ~ factor(Jog) + factor(Fiesta) + factor(Picture) + 
                 factor(Meet) + factor(Eat) , 
               family = binomial, data = t)
summary(compete4)
Anova(compete4)


#Competing model (All)
log.fit = glm(Preference ~ Age+ Sex+ Income+Alone+Status+Highest+factor(Stroll)+factor(Sports)+factor(Eat)+factor(Fiesta)+factor(Jog)+factor(Bike)+factor(Relax)+factor(Sight)+factor(Explore)+factor(Meet)+factor(Picture)+factor(Dance), data=t,family=binomial)
summary(log.fit)
Anova(log.fit)

stepAIC(log.fit)
#fit null
log.fit0 = glm(Preference ~1, data=t,family=binomial)
summary(log.fit0)

#forward selection
step(log.fit0, scope=list(lower=log.fit0, upper=log.fit), direction="forward")
#Backward selection
step(log.fit, data=t, direction="backward")
#Stepwise selection
step(log.fit0, scope = list(upper=log.fit), data=t, direction="both")

compete5 <-glm(formula = Preference ~ factor(Jog) + factor(Fiesta) + factor(Picture) + 
                 factor(Meet) + factor(Eat) + Alone + Income + factor(Sports) + 
                 Age  , family = binomial, data = t)
summary(compete5)
Anova(compete5)

compete6<- glm(formula = Preference ~ Age + Income + Alone + factor(Sports) + 
                 factor(Eat) + factor(Fiesta) + factor(Jog)  + 
                 factor(Meet) + factor(Picture), family = binomial, data = t)
summary(compete6)


likfit1 <- -2*(logLik(log.fit5)-logLik(log.fit6))#Local Test
p_Sex<-1-pchisq(likfit1,df=1)#p-value of the local test
as.numeric(p_Sex)#not significant, don't include sex in the model


vif(compete5)

#Competing model (Demo & Activities)
log.fit = glm(Preference ~ Age+ Sex+ Income+Status+Highest+factor(Stroll)+factor(Sports)+factor(Eat)+factor(Fiesta)+factor(Jog)+factor(Bike)+factor(Relax)+factor(Sight)+factor(Explore)+factor(Meet)+factor(Picture)+factor(Dance), data=t,family=binomial)
summary(log.fit)
Anova(log.fit)

vif(log.fit)

#fit null
log.fit0 = glm(Preference ~1, data=t,family=binomial)
summary(log.fit0)

#forward selection
step(log.fit0, scope=list(lower=log.fit0, upper=log.fit), direction="forward")
#Backward selection
step(log.fit, data=t, direction="backward")
#Stepwise selection
step(log.fit0, scope = list(upper=log.fit), data=t, direction="both")

compete6 <- glm(formula = Preference ~ factor(Jog) + factor(Fiesta) + factor(Picture) + 
                  factor(Meet) + factor(Eat) + Income + Age + factor(Sports) + 
                  factor(Explore), family = binomial, data = t)
summary(compete6)
Anova(compete6)

#Final Models----
compete1 <- glm(Preference ~Alone+Income+Age, data=t,family=binomial)
summary(compete1)
Anova(compete1)

compete2 <-glm(formula = Preference ~ factor(Jog) + factor(Fiesta) + factor(Picture) + 
                 factor(Meet) + factor(Eat) , 
               family = binomial, data = t)
summary(compete2)
Anova(compete2)

compete3 <-glm(formula = Preference ~ factor(Jog) + factor(Fiesta) + factor(Picture) + 
                 factor(Meet) + factor(Eat) + Alone + Income + factor(Sports) + 
                 Age  , family = binomial, data = t)
summary(compete3)
Anova(compete3)




blr_rsq_cox_snell(compete1)
blr_rsq_cox_snell(compete2)
blr_rsq_cox_snell(compete3)


#Choosing the best model
#smaller deviance, closer fit 
anova(compete1,compete2,compete3)
#smaller AIC the better
AIC(compete1,compete2,compete3)

#Best model
best <- compete3
summary(best)
Anova(best)

hoslem.test(t$Preference,fitted(best))


# Predict the probability (p) of diabete positivity
probabilities <- predict(model, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)


#Diagnostic Checking
#Goodness of fit
hoslem.test(t$Preference,fitted(best))

#Linearity Assumption
probabilities <- predict(best, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "Newly Established", "Old/Renovated")
head(predicted.classes)
mydata <- t[c("Age","Income")] %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

plot(best, which = 4, id.n = 3)

model.data <- augment(best) %>% 
  mutate(index = 1:n()) 

model.data %>% top_n(3, .cooksd)
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = Preference), alpha = .5) +
  theme_bw()


model.data %>% 
  filter(abs(.std.resid) > 3)



#Model Adequacy
plot(residuals(best))
plot(best$fitted.values,residuals(best))
plot(residuals.glm(best,type="deviance"))

dfbetasPlots(best)


p1=matrix(0,nrow=13,ncol=3)
i=1
for(p in seq(min(fitted(best)),.95,.05)){
  t1=table(fitted(best)>p,t$Preference)
  p1[i,]=c(p,(t1[2,2])/sum(t1[,2]),(t1[1,1])/sum(t1[,1]))
  i=i+1
}
plot(1-p1[,3],p1[,2])


t1=table(fitted(best)>.5,t$Preference)
100*(t1[2,1]+t1[1,2])/sum(t1)


res <- predict(best,type = "response")
res

confmatrix <- table(Observed=t$Preference,Predicted=res>0.5)
confmatrix
#Accuracy
paste((confmatrix[1,1]+confmatrix[2,2])/sum(confmatrix)*100,"%")


#Overall fit
wald.test(b = coef(best), Sigma = vcov(best), Terms = 4:6)

#Coefficients
best$coefficients

#Odds ratio
exp(best$coefficients)
round(exp(best$coefficients),2)



#Testing for independence of error terms
durbinWatsonTest(log.fit)

#Testing for constant error variance
ncvTest(log.fit)

#Testing for normality of error terms
shapiro.test(log.fit$residuals)

#Checking for multicollinearity
vif(log.fit)
vif(best)




ggdensity(log.fit$residuals)
ggqqplot(log.fit$residuals)
shapiro.test(log.fit$residuals)


#Poisson Regression Wrong----
c <- Clean_Data_Recreation[-401,]
describe(c)
summary(c)

t <- c[,c(2,3,5,7,8,9,10,16,17,18,19, 23,37,38,53)]
colnames(t) <- c("Age","Sex", "Status","Highest", "Income","Socio", "District", "Visits", "Count", "weight", "Alone", "Travel","Activities","Others1","Facilities")
t
summary(t)

#Negative binomial regression
mean(t$Visits)
var(t$Visits)
var(t$Visits)/mean(t$Visits)


fit <- goodfit(t$Visits) 
summary(fit) 
rootogram(fit)


count <- t$Count
weights <- t$weight

mean(count*weights)
var(count*weights)


histogram( ~ Age | Sex, data = t, breaks = 10)
histogram( ~ Age | Alone, data = t, breaks = 10)
histogram( ~ Age | Status, data = t, breaks = 10)
histogram( ~ Age | Highest, data = t, breaks = 5)

histogram( ~ Income | Sex, data = t, breaks = 50)
histogram( ~ Income| Alone, data = t, breaks = 50)
histogram( ~ Income | Status, data = t, breaks = 50)
histogram( ~ Income | Highest, data = t, breaks = 50)


scatterplotMatrix(~Facilities+Age+Income+Travel, data= t)
scatterplotMatrix(~Visits+Age+Income+Travel|Sex, data= t)
scatterplotMatrix(~Visits+Age+Income+Travel|Alone, data= t)
scatterplotMatrix(~Visits+Age+Income+Travel|Status, data= t)


neg.fit <- glm.nb(Count~ Age+ Sex+ Income+Alone+Status+Highest+ Travel,weights = weight, data= t)

neg.fit <- glm.nb(Facilities~ Age+Sex+Income+Alone+Status+Highest+Travel, data= t)

summary(neg.fit)
anova(neg.fit)


local_age <- glm.nb(Visits~Sex+ Income+Alone+Status+Highest+Travel, data= t)#local test age
likfit <- -(local_age$twologlik-neg.fit$twologlik) #Local Test
p_age<-1- pchisq(likfit,df=1)#p-value of the local test
p_age#not significant, drop age variable

local_sex <- glm.nb(Visits~Age+ Income+Alone+Status+Highest+Travel, data= t)#local test sex
likfit <- -(local_sex$twologlik-neg.fit$twologlik) #Local Test
p_age<-1- pchisq(likfit,df=1)#p-value of the local test
p_age#not significant, drop sex variable

local_income <- glm.nb(Visits~Age+ Sex+ Alone+Status+Highest+Travel, data= t)#local test income
likfit <- -(local_income$twologlik-neg.fit$twologlik) #Local Test
p_age<-1- pchisq(likfit,df=1)#p-value of the local test
p_age#not significant, drop income variable


local_high <- glm.nb(Visits~Age+ Sex+ Income+ Alone+Status+Travel, data= t)#local test highest educational attainment
likfit <- -(local_high$twologlik-neg.fit$twologlik) #Local Test
p_age<-1- pchisq(likfit,df=1)#p-value of the local test
p_age#not significant, drop highest educ variable

#Competing models
com1 <- glm.nb(Visits~ Alone+Status+Travel, data= t)
summary(com1)
anova(com1)
AIC(com1)

com2 <- glm.nb(Visits~ Alone, data= t)
summary(com2)
anova(com2)
AIC(com2)

com3 <- glm.nb(Visits~ Status, data= t)
summary(com3)
anova(com3)
AIC(com3)

com4 <- glm.nb(Visits~ Travel, data= t)
summary(com4)
anova(com4)
AIC(com4)

com5 <-glm.nb(Visits~ Alone+Status, data= t)
summary(com5)
anova(com5)
AIC(com5)

com6 <-glm.nb(Visits~ Alone+Travel, data= t)
summary(com6)
anova(com6)
AIC(com6)

com7 <- glm.nb(Visits~ Status+Travel, data= t)
summary(com7)
anova(com7)
AIC(com7)


best<- stepAIC(neg.fit)
summary(best)
anova(best)

#Coefficients
best$coefficients

summary(best)

exp(predict.glm(best,data.frame(Alone="With Company",Status="Unemployed/Retired",Travel=50)))/48

#Impact multiplier
exp(best$coefficients)

#Testing for independence of error terms
durbinWatsonTest(best)

#Testing for constant error variance
ncvTest(best)
?ncvTest

#Testing for normality of error terms
shapiro.test(best$residuals)

#Checking for multicollinearity
vif(best)


deviance(best)/best$df.residual
dispersiontest(best)

influencePlot(best)


res <- residuals(best, type="deviance")
plot(log(predict(best)), res)
abline(h=0, lty=2)


qqnorm(res)
qqline(res)


halfnorm(residuals(best))













#Survival Analysis

#Data and package needed----
library("KMsurv")
library("OIsurv")
library("survival")
library("MASS")
library("survminer")

data("hodg")
#hodg <- hodg_without_wtime#encoding the data without the waiting time variable
surv <- Surv(hodg$time,hodg$delta)#assigning of the right censored data
summary(hodg)#summary of data
summary(surv)


data("hodg")
##Survival Estimates----

#survival estimate for all patients under study
shat <- survfit(surv~1)#product limit
shat_fl <- survfit(surv~1,type="fl")#nelson allen
summary(shat)#run table of estimate using product limit
summary(shat_fl)#run table of estimate using nelson allen
ggsurvplot(shat_fl,data=hodg,
           risk.table = TRUE, # Add risk table
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw()) # Change ggplot2 theme
med<-min(hodg$time[shat$surv<=0.5])  #median lifetime
#prints the median and the mean restricted 
print(survfit(Surv(hodg$time,hodg$delta)~1),print.rmean=TRUE)

ggsurvplot(shat_fl,
           data=hodg,
           conf.int = TRUE,
           risk.table.col = "strata", # Change risk table color by groups
           ggtheme = theme_bw(), # Change ggplot2 theme
           fun = "cumhaz")

#crude hazard rate estimate for all
hx <- -log(shat_fl$surv[shat_fl$n.event>0])
crude_hr<-shat_fl$n.event/shat_fl$n.risk
crude_hr
crude_all_hr<-cbind(shat_fl$time,shat_fl$n.risk,shat_fl$n.event,crude_hr)
crude_all_hr


#Survival function KM estimates
my.fit<-survfit(surv~1,type='k',conf.int=0.95,conf.type='plain')
fitsum<-summary(my.fit)
my.fitlog<-survfit(surv~1,type='k',conf.int=0.95,conf.type='log')
fitlogsum<-summary(my.fitlog)
my.fitloglog<-survfit(surv~1,type='k',conf.int=0.95,conf.type='log-log')
fitloglogsum<-summary(my.fitloglog)

#CIs for linear and log
attach(hodg)
summary(shat,time=300)$surv 
a1<-summary(shat,time=300)$lower 
a2<-summary(shat,time=300)$upper
a3<-summary(my.fitlog,time=300)$lower  
a4<-summary(my.fitlog,time=300)$upper

#Creating a table for CIs 
tableCI<-matrix(c(a1,a2,a3,a4), ncol=2, byrow=TRUE)
colnames(tableCI)<-c("lower 95% CI" ,"upper 95% CI")
rownames(tableCI)<-c("Linear", "Log Transformation")
table.CI<-as.table(tableCI) 

#survival estimate for lymphoma patients per type of transplant
shat_gtype <- survfit(surv~hodg$gtype,type="fl")#Nelson-aalen estimates per graft type
shat_dtype <- survfit(surv~hodg$dtype,type="fl")#Nelson-aalen estimates per type of lymphoma
summary(shat_gtype)
summary(shat_dtype)
#Plot of nelson-aalen for two types of transplant
ggsurvplot(shat_gtype,data=hodg,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           xlab = "Time in days",   # customize X axis label.
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           legend.labs = 
             c("Allogenic", "Autologous")    # change legend labels.
)
summary(shat_gtype)$table#summary of basic measurements for transplant
summary(shat_dtype)$table#summary of basic measurements for type of lymphoma



#Plot of nelson-aalen for two types of transplant

ggsurvplot(shat_dtype,data=hodg,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           xlab = "Time in days",   # customize X axis label.
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           legend.labs = 
             c("Non-hodgkins", "Hodgkins") )   # change legend labels.


#Test of Difference----
#Logranktest for graft type
survdiff(surv~hodg$gtype,rho=0)
#wilcoxon for graft type
survdiff(surv~hodg$gtype,rho=1)
#Logranktest for d ty
survdiff(surv~hodg$dtype,rho=0)
#wilcoxon
survdiff(surv~hodg$dtype,rho=1)
#Test for difference in the transplant given a fixed disease
survdiff(surv[hodg$dtype==1]~factor(hodg$gtype[hodg$dtype==1]),rho=0)#Logrank test for graft type for patients with non hodgkins lymphoma
survdiff(surv[hodg$dtype==2]~factor(hodg$gtype[hodg$dtype==2]),rho=0)#Logrank test for graft type for patients with hodgkins

#Cox proportional hazard analysis----

#Main effects model
#Global test
cox_main <- coxph(surv~gtype+dtype+score,data=hodg,method="breslow")
summary(cox_main)
stepAIC(cox_main)
local_gtype <- coxph(surv~dtype+score,data=hodg,method="breslow")#local test gtype
summary(local_gtype)
likfit <- -2*(local_gtype$loglik[2]-cox_main$loglik[2])#Local Test
p_gtype<-1-pchisq(likfit,df=1)#p-value of the local test
p_gtype#not significant, retain the gtype variable
local_dtype <- coxph(surv~gtype+score,data=hodg,method="breslow")#local test dtype
summary(local_gtype)
likfit <- -2*(local_dtype$loglik[2]-cox_main$loglik[2])#Local Test
p_dtype<-1-pchisq(likfit,df=1)#p-value of the local test
p_dtype#significant, retain the graft type and dtype variable

#First Competing Model
cox1 <- coxph(surv~score,data= hodg,method="breslow")
summary(cox1)
AIC(cox1)


#second competing model
cox2 <- coxph(surv~gtype+dtype+score+gtype*score+dtype*score,data=hodg, method="breslow")#adopted model after using stepAIC
summary(cox2)
cox_score <- coxph(surv~score,data=hodg,method="breslow")#Test the least significant which is score
summary(cox_score)
likfit <- -2*(cox_score$loglik[2]-cox2$loglik[2])#Local Test
p1<-1-pchisq(likfit,df=4)#p-value of the local test
p1#not significant, retain covariate score

cox_gtype <- coxph(surv~gtype,data=hodg,method="breslow")#Test the least significant which is score
summary(cox_gtype)
likfit <- -2*(cox_gtype$loglik[2]-cox2$loglik[2])#Local Test
p2<-1-pchisq(likfit,df=4)#p-value of the local test
p2#significant, retain the graft type variable

cox3 <- coxph(surv~gtype+dtype+score+gtype*dtype,data=hodg, method="breslow")
summary(cox3)

likfit <- -2*(cox_gtype$loglik[2]-cox3$loglik[2])#Local Test
p3<-1-pchisq(likfit,df=3)#p-value of the local test
p3#significant, retain the graft type variable

cox_intgd  <- coxph(surv~gtype*dtype,data=hodg, method="breslow")
likfit <- -2*(cox_intgd$loglik[2]-cox3$loglik[2])#Local Test
p3<-1-pchisq(likfit,df=3)#p-value of the local test
p3#significant, retain the interaction between graft type and disease type



#Comparing criterion AIC for selection of the best model
AIC(cox_main)#AIC of the main effects
AIC(cox1)#AIC of the first competing model
AIC(cox2)#AIC of the second competing model
AIC(cox3)#AIC of the third competing model

#cox2 is the adopted model since AIC is better
final_model <- coxph(surv~gtype+dtype+score+gtype*score+dtype*score,data=hodg, method="breslow")
summary(final_model)


#Diagnostic Checking----
#Cox-Snell Residuals - testing for overall fit
fit1<- cox1
mres = resid(fit1, type="martingale")
csres = hodg$delta-mres
r.surv = survfit(Surv(csres, hodg$delta)~1, type="fleming-harrington")
par(las=1, mfrow=c(1,1), mai=c(0.5,1.0,1, 0.1), omi=c(1.0,0,0.5,0))
plot(0,0, lty=1, type='n', xlim=c(0,3), ylim=c(0,3), xlab="Residual",
     ylab="Estimated Cum Hazards")
box()
lines(r.surv$time, -log(r.surv$surv), type='s')
lines(c(0,3), c(0,3))
dev.off()

#Testing proportional Hazards assumption
test.ph <- cox.zph(final_model)
test.ph
ggcoxzph(test.ph)

#Testing influential observations
ggcoxdiagnostics(final_model, type ="martingale", linear.predictions = TRUE)

ggcoxdiagnostics(final_model, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())

ggcoxdiagnostics(final_model, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())#The pattern looks fairly symmetric around 0.


#Testing non linearity for continuous covariate
ggcoxfunctional(surv ~ score + log(score) + sqrt(score),data=hodg)#It appears that, nonlinearity is slightly here.










