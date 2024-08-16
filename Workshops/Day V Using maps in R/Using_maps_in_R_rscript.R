#Exercise 1
library(MASS)
library(ggplot2)
?hills

# linear regression models
disttime = lm(time ~ dist, data = hills)
climbtime = lm(time ~ climb, data = hills)

plot1 <- ggplot(hills, aes(dist, time))+
  geom_point()+
  geom_smooth(se=F,method = "lm")+
  theme_bw()

plot2 <- ggplot(hills, aes(climb, time))+
  geom_point()+
  geom_smooth(se=F,method = "lm")+
  theme_bw()

gridExtra:: grid.arrange(plot1, plot2, ncol=2)

predict(disttime, list(dist = 25))

predict(climbtime, list(climb = 6000))

#Exercise 2
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)

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

PTS <- subset(PTS, Year=="2020")

names(PTS)[1] <- "region"

PTSH <- subset(PTS, Organization=="H")

PTSA <- subset(PTS, Organization=="A")

PTSS <- subset(PTS, Organization=="S")

world <- map_data("world")

worldH <- merge(x = world, y = PTSH, by = "region", all.x = TRUE)
worldH <- worldH[order(worldH$order),]

worldA <- merge(x = world, y = PTSA, by = "region", all.x = TRUE)
worldA <- worldA[order(worldA$order),]

worldS <- merge(x = world, y = PTSS, by = "region", all.x = TRUE)
worldS <- worldS[order(worldS$order),]

ggplot() +
  geom_map(
    data = worldH, map = worldH,
    aes(long, lat, map_id = region, fill = score),
    color = "black", size = 0.1) +
  theme_void()+
  scale_fill_gradientn(colors=c("#0CB702","#F0E442","#F21A00"))

ggplot() +
  geom_map(
    data = worldA, map = worldA,
    aes(long, lat, map_id = region, fill = score),
    color = "black", size = 0.1) +
  theme_void()+
  scale_fill_gradientn(colors=c("#0CB702","#F0E442","#F21A00"))

ggplot() +
  geom_map(
    data = worldS, map = worldS,
    aes(long, lat, map_id = region, fill = score),
    color = "black", size = 0.1) +
  theme_void()+
  scale_fill_gradientn(colors=c("#0CB702","#F0E442","#F21A00"))

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

names(SVS)[1] <- "region"

SVS <- subset(SVS, year=="2016")
SVS[35,1] <- "China"
SVS[175,1] <- "UK"

worldSVS <- merge(x = world, y = SVS, by = "region", all.x = TRUE)
worldSVS <- worldSVS[order(worldSVS$order),]

ggplot(worldSVS, aes(long, lat, map_id = region, fill = score)) +
  geom_map(map = worldSVS,
    color = "black", size = 0.1) +
  theme_void()+
  scale_fill_gradientn(colors=c("#0CB702","#F0E442","#F21A00"))

#Exercise 4
library(tidyverse)
library(ggplot2)
library(cluster)

# We can see our data
head(votes.repub)

# Save the dataset in a new variable 
data <- votes.repub
data$region <- rownames(data)
head(data)

mapdata <- map_data("state")

# The names of the regions vary. We need to change that.
# tolower() function changes uppercase letters to lowercase
data$region %>% 
  tolower() -> data$region
head(data)

# We join tables
mapdata <- left_join(mapdata, data, by = "region")
head(mapdata)

# Let's make sure we don't have any NA in X1964
mapdata1 <- mapdata %>% 
  filter(!is.na(mapdata$X1964))

# Let's create a map for the year 1964
map1 <- ggplot(mapdata1, 
               aes(x = long, y = lat, group = group, map_id = region)) + 
  geom_map(map = mapdata1, aes(fill = X1964), 
               color = "white")+theme_void()+theme(legend.position = "none")

map1

# Same for the year 1976
mapdata2 <- mapdata %>% 
  filter(!is.na(mapdata$X1976))

map2 <-ggplot(mapdata2, 
              aes(x = long, y = lat, group = group, map_id = region)) + 
  geom_map(map = mapdata2, aes(fill = X1976), 
               color = "white")+theme_void()+theme(legend.position = "none")

map2

# Side-by-side graphs
library(gridExtra)
grid.arrange(map1, map2, ncol = 2) 

#Exercise 5
# to remove scientific notation in legend on maps
options(scipen = 100, digits = 4)

library(tidyverse)
library(ggplot2)

# Loading a data set
data <- read.csv(url("https://covid19.who.int/WHO-COVID-19-global-data.csv"))
head(data)

# Counting
data %>% 
  group_by(Country) %>% 
  summarise(all_cases = sum(New_cases), 
            all_deaths = sum(New_deaths)) -> data

# We need to rename a column to join tables
rename(data, region = Country) -> data

head(data)

mapdata <-map_data("world")

# Join 
mapdata <- left_join(mapdata, data, by = "region")

map1 <- ggplot(mapdata, 
               aes(x = long, y = lat, group = group, map_id = region)) + 
  geom_map(map=mapdata,aes(fill = all_cases), 
               color = "black") +
  scale_fill_gradient(name = "Total cases", 
                      low = "lightgreen", 
                      high = "darkgreen",
                      na.value = "grey") + # gradient
  theme_void() + # remove axes
  ggtitle("Cases of COVID infection")

map1

map2 <- ggplot(mapdata, 
               aes(x = long, y = lat, group = group, map_id = region)) + 
  geom_map(map=mapdata,aes(fill = all_deaths), 
               color = "black") +
  scale_fill_gradient(name = "Total deaths", 
                      low = "yellow", 
                      high = "red",
                      na.value = "grey") +
  theme_void() + 
  ggtitle("Deaths from COVID")

map2

#Exercise 6
library(ISLR)
?Default

# logistic regression model
model <- glm(default ~ student + balance + income, 
             data = Default,
             family = 'binomial')

summary(model)

# the best model
model2 <- step(model)

summary(model2)

newdata = data.frame(student = "No",
                     balance = 1400)

# probability of defaulting
predict(model2,
        newdata = newdata,
        type = "response")
