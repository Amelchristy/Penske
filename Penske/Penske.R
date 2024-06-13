setwd('/Users/amelchristy/Documents/Penske')
penske <- read.csv("DCM_DATA_LEHIGH.csv")
head(penske)

library(ggplot2)
library(corrplot)
library(GGally)
library(tidyr)
library(dplyr)
library(reshape2)
library(caret)
library(xts)

penske$STATUS <- factor(penske$STATUS, levels = c("E", "T"))
penske$STATUS <- as.numeric(penske$STATUS) - 1


# Calculate the correlation matrix for numeric columns in penske_df
numeric_columns <- sapply(penske, is.numeric)
cor.mat <- round(cor(penske[, numeric_columns]), 2)

# Melt the correlation matrix
melted.cor.mat <- melt(cor.mat)

# Create the heatmap using ggplot2
ggplot(data = melted.cor.mat, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + 
  geom_text(aes(label = round(value, 2)), size = 3) + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), name = "Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), axis.text.y = element_text(angle = 45, hjust = 1)) +
  labs(x = '', y = '', title = 'Correlation Matrix of Numeric Columns in Penske Data')


penske$YEAR_MONTH <- as.Date(paste0(penske$YEAR_MONTH, "-01"))  # Convert to Date format assuming the day is the first of the month



####TIMESERIES
# Create a subset of data with terminations
termination_df <- subset(penske, STATUS == "T")

# Aggregate the data to count terminations per month
monthly_termination_counts <- aggregate(termination_df$STATUS, by = list(termination_df$YEAR_MONTH), FUN = length)
colnames(monthly_termination_counts) <- c("Year_Month", "Termination_Count")

# Step 2: Create a time series object
termination_ts <- xts(monthly_termination_counts$Termination_Count, order.by = as.Date(monthly_termination_counts$Year_Month))

# Basic Time Series Plot
plot(termination_ts, main = "Monthly Termination Counts", xlab = "Month", ylab = "Number of Terminations", type = "o")



penske_df <- read.csv("DCM_DATA_LEHIGH.csv")
head(penske_df)

penske_df$YEAR_MONTH <- as.Date(paste0(penske_df$YEAR_MONTH, "-01"))  
termination_df <- subset(penske_df, STATUS == "T")


monthly_termination_counts <- aggregate(termination_df$STATUS, by = list(termination_df$YEAR_MONTH), FUN = length)
names(monthly_termination_counts) <- c("Year_Month", "Termination_Count")

library(xts)
termination_ts <- xts(monthly_termination_counts$Termination_Count, order.by = as.Date(monthly_termination_counts$Year_Month))
plot(termination_ts, main = "Monthly Termination Counts", xlab = "Month", ylab = "Number of Terminations", type = "o")




############trial
library(xts)
library(forecast)
library(ggplot2)

# Assuming penske_df is already read and the YEAR_MONTH column is properly formatted
penske_df <- read.csv("DCM_DATA_LEHIGH.csv")
penske_df$YEAR_MONTH <- as.Date(paste0(penske_df$YEAR_MONTH, "-01"))  
termination_df <- subset(penske_df, STATUS == "T")

# Aggregate the data to count terminations per month
# If YEAR_MONTH is already a Date object, we should format it to yearmon for aggregation
monthly_termination_counts <- aggregate(termination_df$STATUS, by = list(format(termination_df$YEAR_MONTH, "%Y-%m")), FUN = length)
names(monthly_termination_counts) <- c("Year_Month", "Termination_Count")

# Convert the 'Year_Month' to a yearmon object for proper time series plotting
monthly_termination_counts$Year_Month <- as.yearmon(monthly_termination_counts$Year_Month)

# Create a time series object from the counts
# Ensure the frequency aligns with the data (e.g., 12 for monthly data)
# Create a time series object from the counts
# Ensure the frequency aligns with the data (e.g., 12 for monthly data)
# The start should be the c(year, month) where year and month are the numeric values of the first date
termination_ts <- ts(monthly_termination_counts$Termination_Count, frequency = 12, 
                     start = , 1, 4), 
                               as.numeric(substr(monthly_termination_counts$Year_Month[1], 6, 7))))

# Plot the time series data
plot(termination_ts, main = "Monthly Termination Counts", xlab = "Time", ylab = "Number of Terminations", type = "o")
