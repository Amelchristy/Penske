setwd("/Users/amelchristy/Documents/Penske")

# Libraries
library(dplyr)

# Read data
aggregate_data <- read.csv("aggregated_data.csv")
str(aggregate_data)

head(aggregate_data)

#----
# GLM 
# Create a new dataset that only covers the AREA_NAME and Target Variable
AREA <- aggregate_data %>%
  select(STATUS, AREA_NAME.1:AREA_NAME.9)

str(AREA)

# Transform the STATUS column, changing "T" to 1 and "E" to 0
AREA <- AREA %>%
  mutate(STATUS = ifelse(STATUS == "T", 1, ifelse(STATUS == "E", 0, NA)))

# partition data
set.seed(1)
options(scipen=999)
train.index = sample(1:nrow(AREA), 0.6*nrow(AREA))

train.df = AREA[train.index, ]
valid.df = AREA[-train.index, ]

str(train.df)

# Create a glm model (logistic regression)
glm_Area <- glm(STATUS ~ ., data = train.df, family = binomial)
summary(glm_Area)



# Interpretation: ----
# The intercept (1.0415, p = 0.02829) is statistically significant, 
#suggesting an inherent bias when all other factors are set to zero.

# Several coefficients are significant, indicating a strong relationship between specific factory locations:
AREA_NAME.1 (-0.9864, p = 0.03926)
AREA_NAME.11 (-1.5683, p = 0.00175)
AREA_NAME.12 (-3.6064, p = 0.00158)
AREA_NAME.3 (-1.0257, p = 0.03313)
AREA_NAME.6 (-1.2515, p = 0.00933)
AREA_NAME.7 (-4.6921, p < 0.001)
AREA_NAME.8 (-2.9957, p < 0.001)

# Coefficients
## Negative: 
### Indicate that the presence of these factories tends to lower the probability of termination. 
### The magnitude of the coefficients suggests the strength of this relationship.
## Positive:
### an increased probability of termination, but none were significant among those estimated.




