######GLM ANALYSIS 
setwd("/Users/amelchristy/Documents/Penske")

library(MASS)
library(forecast)
library(caret)
data <- read.csv("aggregated_data.csv")
str(data)\


############EXPLARATORY####################

library(ggplot2)
library(dplyr)
library(ggplot2)
library(dplyr)

# Assuming 'data' is your main DataFrame
# Filter the data for completeness
library(ggplot2)
library(dplyr)

#
data$TIP
data$SHIFT_DIFF

library(ggplot2)

# Assuming 'data' is your data frame
ggplot(data, aes(x=TIP, y=SHIFT_DIFF)) +
  geom_point(aes(color=factor(STATUS)), alpha=0.6) +  # Adding color based on STATUS, adjust alpha for transparency
  scale_color_manual(values=c("1"="blue", "0"="yellow")) +
  labs(title="Shift Differential Pay vs. Time in Position (TIP)",
       x="TIP",
       y="SHIFT_DIFF",
       color="Employment Status") +
  theme_minimal() +
  theme(legend.position="top")
data$STATUS

#Drop ANO_SSO
# Remove unwanted columns using a pattern
data <- data[, !names(data) %in% c("ANON_SSO", "YEAR_MONTH", paste0("AREA_NAME.", 1:18))]
#feature engineering
data$STATUS <- factor(ifelse(data$STATUS == "T", 1, 0), levels = c(0, 1))
data$LEAD
hist(data$LEAD)
#check if there's na
data[is.na(data), ]
#LEAD – pay received if they’re a lead worker
nrow(data$STATUS=="1")
data$STATUS
set.seed(1)
train.index <- sample(1:nrow(data), 0.6*nrow(data))
train.df <- data[train.index, ]
valid.df <- data[-train.index, ]

full_model <- glm(STATUS ~ ., data = train.df, family = binomial())
summary(full_model)

area4 <- glm(STATUS~AREA_NAME.)

null_model <- glm(STATUS ~ 1, data = train.df, family = binomial())

forward_model <- step(null_model, scope=list(lower=null_model,upper=full_model),direction="forward")
summary(forward_model)

backward_model <- step(full_model, direction = "backward")
summary(backward_model)

step_model <- step(full_model, direction="both")
summary(step_model)


aic.df <- rbind(AIC(full_model), AIC(forward_model), AIC(backward_model), AIC(step_model))
rownames(aic.df) <- c("Full","Forward","Backward","Step")
colnames(aic.df) <- c("AIC")
aic.df

library(forecast)
# Predict probabilities on each stepwise
full_probs <- predict(full_model, newdata = valid.df, type = "response")
forward_probs <- predict(forward_model, newdata = valid.df, type = "response")
backward_probs <- predict(backward_model, newdata = valid.df, type = "response")
step_probs <- predict(step_model, newdata = valid.df, type = "response")

# Turn probs into classes
p_class_full <- ifelse(full_probs > 0.5,1,0)
p_class_forward <- ifelse(forward_probs > 0.5, 1,0)
p_class_step <- ifelse(backward_probs > 0.5, 1,0)
p_class_backward <- ifelse(step_probs > 0.5, 1,0)

print(conf_matrix_full <- confusionMatrix(as.factor(p_class_full), as.factor(valid.df$STATUS), positive="1"))
print(conf_matrix_forward <- confusionMatrix(as.factor(p_class_forward), as.factor(valid.df$STATUS), positive="1"))
print(conf_matrix_step <- confusionMatrix(as.factor(p_class_step), as.factor(valid.df$STATUS), positive="1"))
print(conf_matrix_backward <- confusionMatrix(as.factor(p_class_backward), as.factor(valid.df$STATUS), positive="1"))
data$REG_AMT


library(ggplot2)
library(pROC)

#####################################################
####STATUS TOTAL HOURS #####EXPLARATORY WITH JACK 
###############################################

library(ggplot2)

# Assuming your data frame is named 'data' and it contains the variables LEAD, TOTAL_HOURS, and STATUS.
# Also assuming that 'STATUS' contains factors or will be converted to factor with levels "TOTAL_HOURS" and "LEAD".

ggplot(data, aes(x=LEAD, y=TOTAL_HOURS, color=as.factor(STATUS))) +
  geom_point() +  # Adds points to the plot; you can customize the size if needed using size= argument
  scale_color_manual(values=c("1"="blue", "0"="yellow")) +
  theme_minimal() +
  labs(color="Status", x="Lead", y="Total Hours")


e <- ggplot(data,aes(x=START_HOUR_AVG, y= LEAD,color = as.factor(STATUS)))
e +geom_point(alpha=0.5)


if(is.factor(df$STATUS)) {
  df$STATUS <- as.numeric(as.character(df$STATUS))
}



# Calculate the correlation matrix for the selected columns
cor_matrix <- cor(df[, c("LEAD", "VACATION_HOUR", "STATUS", "START_HOUR_AVG", "SHIFT_DIFF")], use = "complete.obs")

# Print the correlation matrix
print(cor_matrix)


attach(data)
data$RATIO <- LEAD/TOTAL_HOURS
data$RATIO
peeps <- data[data$RATIO >= 0.9 & data$RATIO <= 1.1, ]
nrow(peeps)
peeps


peeps.lm <- glm(STATUS ~ RATIO, data = train.df, family = binomialsd())

###########################
# Create a data frame of actual statuses and predicted probabilities
predictions_df <- data.frame(
  ActualStatus = valid.df$STATUS,
  PredictedProbability = full_probs
)

# Plot using ggplot2
library(ggplot2)
ggplot(predictions_df, aes(x = PredictedProbability, fill = as.factor(ActualStatus))) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.5) +
  scale_fill_manual(values = c("1" = "blue", "0" = "yellow")) +
  labs(title = "Predicted Probability Distribution by Actual Status",
       x = "Predicted Probability",
       y = "Count",
       fill = "Actual Status") +
  theme_minimal()




#install.packages("broom")
library(broom)

tidied_model <- tidy(backward_model)


ggplot(tidied_model, aes(x = estimate, y = reorder(term, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_point() +
  geom_segment(aes(x = estimate, xend = 0, yend = term)) +
  labs(x = "Coefficient Estimate", y = "Term", title = "Effect of each variable") +
  theme_minimal()



# Repeat for other variables as needed

#####CART#####
library(rpart)
library(rpart.plot)
set.seed(1)
rt.default <- rpart(STATUS ~ ., data = train.df, method="class")
prp(rt.default, type=1, extra=1, split.font=1, varlen= -10, digits=-1)

# print cp table
printcp(rt.default)


# grow a large regression tree with a small cp =0.00001
# set the smallest value for cp
set.seed(1)
penske.large.ct <- rpart(STATUS ~ ., data = train.df, method = "class", minsplit = 2, 
               cp = 0.00001, xval=5)
prp(penske.large.ct, type =1, extra = 1, split.font = 2, varlen = -10)

printcp(penske.large.ct)

cp <- which.min(penske.large.ct$cptable[, "xerror"])
cp

# Best-pruned tree: use the cp such that the xerror is within min(xerror) + 1xstd: 0.41285 0.012671

pruned.ct.best <- prune(penske.large.ct, cp = penske.large.ct$cptable[which.max(penske.large.ct$cptable[ ,"xerror"]<0.41285+ 0.012671), "CP"])

prp(pruned.ct.best, type=1, extra = 1, split.font = 1, varlen=-10)


# prediction for unseen data

pruned.ct.best.pred <- predict(pruned.ct.best, valid.df, type = "class")

# confusion matrix

print(conf_matrix_full <- confusionMatrix(pruned.ct.best.pred, valid.df$STATUS, positive="1"))




##WAREHOUSE ANALYSIS 

# Load data
warehouse <- read.csv("/Users/amelchristy/Documents/Penske/aggregated_data.csv")
warehouse
# Subset columns
selected_columns <- grep("AREA_NAME\\.", names(warehouse), value = TRUE)
selected_columns <- c(selected_columns, "STATUS")
subset_df <- warehouse[selected_columns]

# Remove AREA_NAME.1
subset_df$AREA_NAME.1 <- NULL

# Recode STATUS
subset_df$STATUS <- ifelse(subset_df$STATUS == "T", 1, ifelse(subset_df$STATUS == "E", 0, subset_df$STATUS))
subset_df
subset_df$STATUS <- as.numeric(subset_df$STATUS)
# Split data into training and validation sets
set.seed(1)
train.index <- sample(1:nrow(subset_df), 0.6 * nrow(subset_df))
train.df <- subset_df[train.index, ]
valid.df <- subset_df[-train.index, ]

# Fit logistic regression model
warehouse.glm <- glm(STATUS ~ ., data = train.df, family = "binomial")
summary(warehouse.glm)

library(caret)  # for confusionMatrix function

# Predict probabilities
predicted_probs <- predict(warehouse.glm, valid.df, type = "response")

# Convert probabilities to binary class predictions using a threshold of 0.5
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)

# Calculate the confusion matrix
conf_matrix <- confusionMatrix(as.factor(predicted_classes), as.factor(valid.df$STATUS), positive="1")

# Print the confusion matrix
print(conf_matrix)


###VISUALIZATION
library(ggplot2)
ware_df <- read.csv("aggregated_data.csv")
ware_df$STATUS <- ifelse(ware_df$STATUS == "T", 1, ifelse(ware_df$STATUS == "E", 0, ware_df$STATUS))

# Filter the subset_df for relevant areas and columns
areas_of_interest <- c("AREA_NAME.11", "AREA_NAME.12", "AREA_NAME.2", "AREA_NAME.6", "AREA_NAME.7", "AREA_NAME.8", "AREA_NAME.9")
cols_of_interest <- c(areas_of_interest, "WORKED_HOURS", "OT_HRS", "CHECK_GROSS","WORKED_HOURS",
                      "TOTAL_HOURS")
filtered_df <- ware_df[, cols_of_interest]

# Melt the data for visualization
melted_data <- reshape2::melt(ware_df, id.vars = areas_of_interest, measure.vars = c("WORKED_HOURS", "OT_HRS", "CHECK_GROSS"))

# Create the plot
ggplot(melted_data, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Distribution of Worked Hours, Overtime Hours, and Gross Check by Area",
       x = "Area", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



####TIME SERIES 
# Assuming penske_df is already read and the YEAR_MONTH column is properly formatted
penske_ts <- read.csv("DCM_DATA_LEHIGH.csv")
penske_ts

termination_df <- subset(penske_df, STATUS == "T")

# Aggregate the data to count terminations per month
# If YEAR_MONTH is already a Date object, we should format it to yearmon for aggregation
monthly_termination_counts <- aggregate(termination_df$STATUS, by = list(format(termination_df$YEAR_MONTH, "%Y-%m")), FUN = length)
names(monthly_termination_counts) <- c("Year_Month", "Termination_Count")

# Convert the 'Year_Month' to a yearmon object for proper time series plotting
monthly_termination_counts$Year_Month <- as.yearmon(monthly_termination_counts$Year_Month)

# Assuming penske_ts$YEAR_MONTH is already in Date format
# Find the oldest and newest dates
oldest_date <- min(penske_ts$YEAR_MONTH)
newest_date <- max(penske_ts$YEAR_MONTH)
oldest_date
newest_date

# Create the time series object with the appropriate start and end dates
termination_ts <- ts(monthly_termination_counts$Termination_Count, frequency = 12, 
                     start =c(2020,1) ,
                     end = c(2022,12))
plot(termination_ts, xlab = "Time", ylab= "Monthly Termination Count", xlim=c(2020,2023))



#partition
nValid <- 3
nTrain <- length(termination_ts) - nValid
train.ts <- window(termination_ts, start=c(2020, 1), end=c(2020, nTrain))
valid.ts <- window(termination_ts, start=c(2020, nTrain+1), 
                   end=c(2020, nTrain + nValid))
# use tslm() with argument lambda=0

train.lm.expo <- tslm(train.ts ~ trend + season, lambda=0)
summary(train.lm.expo)

# Predict for validation set
train.lm.expo.pred <- forecast(train.lm.expo, h=nValid)
train.lm.expo.pred

# Plot predictions vs. actual data
plot(termination_ts, ylab="Monthly Termination Count", main="Exponential Transformation Fit")
lines(train.lm.expo$fitted.values, col=2, lty=2, lwd=2)
lines(train.lm.expo.pred$mean, col=3, lty=2, lwd=2)

##poly

train.lm.poly <- tslm(train.ts ~ trend+season+ I(trend^2) )
summary(train.lm.poly)

# Predict for validation set
train.lm.poly.pred <- forecast(train.lm.poly, h=nValid)
train.lm.poly.pred

# Plot predictions vs.actual

plot(termination_ts, ylab="Monthly Termination Count",main="Poly Fit")
# add linear fit
lines(train.lm.poly$fitted.values, col=2, lty=2, lwd=2)
lines(train.lm.poly.pred$mean, col=3, lty=2, lwd=2)

      