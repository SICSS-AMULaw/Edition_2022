#### Exercise 1

?cars

plot(cars, pch = 21)

# linear regression model
model_lin <- lm(dist ~ speed, data = cars)
abline(model_lin, col = "darkred", lwd = 2)
summary(model_lin)

# quadratic regression model
model_qua <- lm(dist ~ speed + I(speed^2), data = cars)
coefs <- coef(model_qua)
curve(coefs[1] + coefs[2] * x + coefs[3] * x^2,
      add = TRUE,
      col = "blue",
      lwd = 2)

# adding legend to plot
legend("topleft", 
       legend = c("linear regression model", "quadratic regression model"), 
       col = c("darkred", "blue"), 
       lty = 1, 
       lwd = 2)

summary(model_qua)

# looking at the R^2, quadratic model is better

# prediction
predict(model_qua, list(speed = 40))

#### Exercise 2

install.packages("MASS")
library(MASS)
?Boston

plot(Boston$medv, Boston$lstat)

# 2th level
model1 <- lm(lstat ~ medv + I(medv^2), data = Boston)
coefs <- coef(model1)
curve(coefs[1] + coefs[2] * x + coefs[3] * x^2,
      add = TRUE,
      col = "darkred",
      lwd = 2)
summary(model1)

# 3th level
model2 <- lm(lstat ~ medv + I(medv^2) + I(medv^3), data = Boston)
coefs <- coef(model2)
curve(coefs[1] + coefs[2] * x + coefs[3] * x^2 + coefs[4] * x^3,
      add = TRUE,
      col = "blue",
      lwd = 2)
summary(model2)

# 4th level
model3 <- lm(lstat ~ medv + I(medv^2) + I(medv^3) + I(medv^4), data = Boston)
coefs <- coef(model3)
curve(coefs[1] + coefs[2] * x + coefs[3] * x^2 + coefs[4] * x^3 + coefs[5] * x^4,
      add = TRUE,
      col = "green",
      lwd = 2)
legend("topright", 
       legend = c("2th level", "3th level", "4th level"), 
       col = c("darkred", "blue", "green"), 
       lty = 1, 
       lwd = 2)
summary(model3)

# In my opinion, 2th level is sufficient.
# There are very small differences at coefficient R^2.

# prediction
predict(model1, list(medv = 50.04))

#### Exercise 3

install.packages("datarium")
library(datarium)
?marketing

# multiple regression model
model <- lm(sales ~ ., data = marketing)
summary(model)

# We use method 2 from the lecture
model2 <- step(model)
summary(model2) # the best model

# prediction
newdata = data.frame(youtube = 150,
                     facebook = 25)
predict(model2, newdata)

#### Exercise 4

library(AER)
data(Affairs)
library(dplyr)
?Affairs

Affairs <- Affairs %>% 
  mutate(ynaffairs = ifelse(affairs > 0, 1, 0))

model_glm <- glm(ynaffairs ~ gender + age +
                   yearsmarried + children + 
                   religiousness + education + 
                   occupation + rating,
                 data = Affairs, 
                 family = 'binomial')
model_glm2 <- step(model_glm)

newdata1 <- data.frame(rating = 3,
                       age = 20,
                       yearsmarried = 2,
                       religiousness = 1,
                       gender = "male")
predict(model_glm2, 
        newdata = newdata1, 
        type = "response")

newdata2 <- data.frame(rating = 5,
                       age = 80,
                       yearsmarried = c(10, 20),
                       religiousness = 3,
                       gender = "female")
predict(model_glm2, 
        newdata = newdata2, 
        type = "response")