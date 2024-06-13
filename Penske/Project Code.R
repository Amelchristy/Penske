data <- read.csv("aggregated_data.csv")
getwd()
setwd("/Users/mackenzieporter/Desktop/BIS 415")

install.packages("dplyr")
library(dplyr)

data <- select(data, -ANON_SSO, -AREA_NAME.1, -AREA_NAME.2, -AREA_NAME.3, -AREA_NAME.4, -AREA_NAME.5, -AREA_NAME.6, -AREA_NAME.7,-AREA_NAME.8, -AREA_NAME.9, -AREA_NAME.10, -AREA_NAME.11, -AREA_NAME.12, -AREA_NAME.13, -AREA_NAME.14, -AREA_NAME.15, -AREA_NAME.16, -AREA_NAME.17)
data <- select(data, -AREA_NAME.18)
data$STATUS <- as.factor(data$STATUS)

data$STATUS <- ifelse(data$STATUS == "T", 1, 
                           ifelse(data$STATUS == "E", 0, data$STATUS))
data <- select(data, -YEAR_MONTH)

# regression tree
set.seed(1)
train.index <- sample(1:nrow(data), 0.6*nrow(data))

train.df <- data[train.index, ]
valid.df <- data[-train.index, ]

# Classification tree
c.tree <- rpart(STATUS ~ ., data = data, method = "class")
c.tree
prp(c.tree, type = 1, extra = 1, split.font = 1, varlen = -10)

c.tree.full <- rpart(STATUS ~., data=data, minbucket=1, method = "class")

prp(c.tree.full, type =1, extra=1, split.font = 2, varlen=-10)

# Regression trees
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

rt.default <- rpart(STATUS ~ ., data = train.df)
prp(rt.default, type=1, extra=1, split.font=1, varlen= -10, digits=-1)

# print cp table
printcp(rt.default)


# grow a large regression tree with a small cp =0.001
set.seed(1)
rt.full <- rpart(STATUS ~ ., data = train.df, cp = 0.001)
prp(rt.full, type=1, extra=1, split.font=1, varlen= -10, digits=-1)

# print cp table
printcp(rt.full)


