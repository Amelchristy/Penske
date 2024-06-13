# library
library(dplyr)

# read.csv
p1 <- read.csv("DCM_DATA_LEHIGH.csv")

# view the dataset
View(p1)

#----
# Data Clean -> to create a new dataset for the first three questions 
# removing these id
p1.1 <- p1 # create a new dataset
str(p1.1)
p1.1$VOLUNTARY_INVOLUNTARY <- NULL # Remove the repeated variable (status can replace it)
p1.1$ANON_SSO <- NULL # Employee ID
p1.1$ANON_AREA_NAME <- NULL # Area Names - unclear, only sequence number
p1.1$ANON_KMA <- NULL # KMA (Key Market Area)  - unclear, only sequence number
p1.1$ANON_ORG_NAME <- NULL # org_name (individual Penske location)  - unclear, only sequence number
p1.1$ANON_REGION_NAME <- NULL # region name - unclear, only sequence number

str(p1.1)

# Is there any NA remain inside the dataset?
na.counts.p1.1 <- p1.1 %>%
  summarise(across(everything(), ~sum(is.na(.))))
na.counts.p1.1

#----
# The relationship between status and voluntary_involuntary
table(p1$VOLUNTARY_INVOLUNTARY)
table(p1$STATUS)
# The result means that, the terminated 'employees' are all the voluntary. 