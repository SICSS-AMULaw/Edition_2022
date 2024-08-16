#Exercise 2
library(readxl)
library(dplyr)
library(tidyr)
library(FSA)
library(data.table)
PTS <- read_excel("PTS-2021.xlsx",
                  col_types = c("text", "text", "text", "text", "numeric",
                                "text", "numeric", "text", "numeric", "numeric",
                                "numeric", "numeric", "numeric","numeric"))
PTS <- na.omit(PTS)
PTS <- PTS[,c(1,3,9,10,11)]
PTS %>%  pivot_longer(
  cols = starts_with("PTS_"),
  names_to = "Organization",
  names_prefix = "PTS_",
  values_to = "score",
  values_drop_na = TRUE) -> PTS

shapiro.test(lm(score ~ Organization, data = PTS)$residuals)

kruskal.test(score ~ Organization, data = PTS)

kruskal.test(score ~ Year, data = PTS)

kruskal.test(score ~ Country, data = PTS)

dunnTest(score ~ Country, data = PTS)$res %>% head()

PTS %>% mutate(CO=paste0(Country,"-",Organization)) -> PTS_2

kruskal.test(score ~ CO, data = PTS_2)

dunnTest(score ~ CO, data = PTS_2)$res %>% head()

results <- dunnTest(score ~ CO, data = PTS_2)$res

subset(results, P.adj<0.05)[,c(1,4)] %>% head()

kruskal.test(score ~ Country, data = subset(PTS,Organization=="H"))

subset(dunnTest(score ~ Country, data = subset(PTS,Organization=="H"))$res,
       P.adj<0.05)[,c(1,4)] %>% head()

kruskal.test(score ~ Country, data = subset(PTS,Organization=="S"))

subset(dunnTest(score ~ Country, data = subset(PTS,Organization=="S"))$res,
       P.adj<0.05)[,c(1,4)] %>% head()

kruskal.test(score ~ Country, data = subset(PTS,Organization=="A"))

subset(dunnTest(score ~ Country, data = subset(PTS,Organization=="A"))$res,
       P.adj<0.05)[,c(1,4)] %>% head()

#Exercise 3
library(stringr)

SVS <- read_excel("SVS 2016 Aggregate.xlsx",
                  col_types = c("skip", "text", "text", "text", "text", "text"))

SVS %>% pivot_longer(
  cols = starts_with("20"),
  names_to = "year",
  values_to = "score",
  values_drop_na = TRUE) -> SVS

SVS$score <- as.numeric(str_remove(SVS$score, "\\*"))

SVS <- na.omit(SVS)

shapiro.test(lm(score ~ year, data = SVS)$residuals)

kruskal.test(score ~ year, data = SVS)

#Exercise 4
library(UsingR)
?homedata

model <- lm(y2000 ~ y1970, data = homedata)

predict(model, list(y1970 = 75000))

#Exercise 5
library(carData)
?Salaries

model <- lm(salary ~ ., data = Salaries)

summary(model)

# the best multiple regression model
model2 <- step(model)

newdata = data.frame(rank = "Prof",
                     discipline = "A",
                     yrs.since.phd = 30,
                     yrs.service = 35)

predict(model2, newdata)
