install.packages("MASS")
library(MASS)

data <- read.csv("aggregated_data.csv")
str(data)

data$ANON_SSO <- NULL
data$YEAR_MONTH <- NULL
data$AREA_NAME.1 <- NULL
data$AREA_NAME.2 <- NULL
data$AREA_NAME.3 <- NULL
data$AREA_NAME.4 <- NULL
data$AREA_NAME.5 <- NULL
data$AREA_NAME.6 <- NULL
data$AREA_NAME.7 <- NULL
data$AREA_NAME.8 <- NULL
data$AREA_NAME.9 <- NULL
data$AREA_NAME.10 <- NULL
data$AREA_NAME.11 <- NULL
data$AREA_NAME.12 <- NULL
data$AREA_NAME.13 <- NULL
data$AREA_NAME.14 <- NULL
data$AREA_NAME.15 <- NULL
data$AREA_NAME.16 <- NULL
data$AREA_NAME.17 <- NULL
data$AREA_NAME.18 <- NULL

data$STATUS <- as.factor(ifelse(data$STATUS == "E", 1, 0))
head(data$STATUS)
str(data)

full_model <- glm(STATUS ~ ., data = data, family = binomial(link = "logit"))


backward_model <- step(full_model, direction = "backward")
summary(backward_model)

null_model <- glm(STATUS ~ 1, data = data, family = binomial(link = "logit"))
forward_model <- step(null_model, direction = "forward", scope = list(lower = null_model, upper = full_model))
summary(forward_model)

cat("AIC for Backward Model:", AIC(backward_model), "\n")
cat("AIC for Forward Model:", AIC(forward_model), "\n")

# Use backward model

install.packages("ggplot2")
library(ggplot2)

predictions <- data
predictions$predicted_status <- predict(backward_model, type = "response")

install.packages("pROC")
library(pROC)

roc_curve <- roc(predictions$STATUS, predictions$predicted_status)
plot(roc_curve, main="ROC Curve for Backward Selection Model")

ggplot(predictions, aes(x = predicted_status, fill = as.factor(STATUS))) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.5) +
  labs(title = "Predicted Probability Distribution by Actual Status",
       x = "Predicted Probability",
       fill = "Actual Status")

auc(roc_curve)


install.packages("broom")
library(broom)

tidied_model <- tidy(backward_model)


ggplot(tidied_model, aes(x = estimate, y = reorder(term, estimate))) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_point() +
  geom_segment(aes(x = estimate, xend = 0, yend = term)) +
  labs(x = "Coefficient Estimate", y = "Term", title = "Effect of each variable") +
  theme_minimal()

ggplot(predictions, aes(x = as.factor(STATUS), y = predicted_status)) +
  geom_boxplot() +
  labs(x = "Actual Status", y = "Predicted Probability", title = "Boxplot of Predicted Probabilities by Actual Status")

# residuals
predictions$residuals <- residuals(backward_model, type = "deviance")

ggplot(predictions, aes(x = predicted_status, y = residuals)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_point(alpha = 0.5) +
  labs(x = "Predicted Probability", y = "Residuals", title = "Residuals vs Predicted")

# Cook's distance
cooksd <- cooks.distance(backward_model)
plot(cooksd, type = "h", main = "Cook's Distance", ylab = "Cook's distance", xlab = "Index")
