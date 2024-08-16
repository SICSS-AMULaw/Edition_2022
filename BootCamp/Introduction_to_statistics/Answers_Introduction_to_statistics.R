### 1
library(MPsychoR)
data("FamilyIQ")
mean(FamilyIQ$matrices)
var(FamilyIQ$matrices)
sd(FamilyIQ$matrices)
quantile(FamilyIQ$matrices)
IQR(FamilyIQ$matrices)


### 2
head(iris)
(test <- chisq.test(iris$Species, iris$Sepal.Length))
# We can reject the null hypothesis


### 3
observed<-c( 700, 557, 228)
expected<-c(0.5, 0.4, 0.1)
(result<-chisq.test(observed, p = expected)) 
# We can reject the null hypothesis


### 4
Performance <- 
  matrix(c(600, 50, 380, 570),
         nrow = 2,
         dimnames = list("1st Survey" = c("Approve", "Disapprove"),
                         "2nd Survey" = c("Approve", "Disapprove")))
(result <- mcnemar.test(Performance)) 
# We can reject the null hypothesis

  
### 5
# One sample t-test
head(women)
shapiro.test(women$height)
t.test(women$height, mu=69) # we can reject the null hypothesis

### 6
# The independent samples t-test
library(dplyr)
USArrests %>%
  mutate(type=case_when (UrbanPop >= 72 ~ "More_urbanized", UrbanPop < 72 ~ "Less_urbanized")) ->USArrests_by_urbanpop

USArrests_by_urbanpop %>%
  group_by(type) %>%
  summarise(p.value = shapiro.test(Murder)$p.value)

(bartlett.test(USArrests_by_urbanpop$Murder, USArrests_by_urbanpop$type))

(result<-t.test(USArrests_by_urbanpop$Murder ~USArrests_by_urbanpop$type, alternative = "less", var.equal = TRUE))
# we can not reject the null hypothesis

### 7
# The paired samples t-test
Before <- USArrests$Murder
After <- c(14.2, 3.4,  3.2,  6.3,  9.5,  3.3,  3.0,  9.9,  5.5,  8.8,  4.3, 12.1, 17.0,  4.4,  8.8, 10.2,  8.3,  0.3,  2.1, 13.6, 9.7, 10.1,  8.8, 13.2,  3.5,  16.2, 8.5,  9.2, 13.2,  5.1,  2.6, 11.1,  0.3, 10.8, 8.2,  5.3,  4.1,  1.8,  3.0,  1.9, 13.2,  9.6,  8.8,  3.2, 12.2,  2.1, 11.7,  3.8, 19.0,  9.7)

shapiro.test(Before-After)

(result<-t.test(Before, After, paired =TRUE )) 
#We can not reject the null hypothesis


### 8
# One-way ANOVA
summary(iris) # we have 3 species

library(dplyr)
iris %>% 
  group_by(Species)  %>% 
  summarise(variance=var(Sepal.Width),
            p.value = shapiro.test(Sepal.Width)$p.value)

(bartlett.test(iris$Sepal.Width, iris$Species))

result<-aov(Sepal.Width~Species, iris)

summary(result) #we can reject the null hypothesis
library(agricolae)
TukeyHSD(result)
HSD.test(result, 'Species', group=TRUE, console=TRUE) #All groups combinations differ

### 9
result<-c(12, 34, 22, 14, 22, 17, 24, 22, 18, 14, 18, 12)
shapiro.test(result)
library(outliers)
grubbs.test(result) 
# this is an outlier

### 10
head(cars)
lm(dist~speed,cars)
