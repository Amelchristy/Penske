setwd("/Users/amelchristy/Documents/Penske")

library(MASS)
library(forecast)
library(caret)
data <- read.csv("aggregated_data.csv")
str(data)
#Drop ANO_SSO
# Remove unwanted columns using a pattern
data <- data[, !names(data) %in% c("ANON_SSO", "YEAR_MONTH", paste0("AREA_NAME.", 1:18))]
#feature engineering
data$STATUS <- factor(ifelse(data$STATUS == "T", 1, 0), levels = c(0, 1))


#check if there's na
data[is.na(data), ]


set.seed(1)
train.index <- sample(1:nrow(data), 0.6*nrow(data))
train.df <- data[train.index, ]
valid.df <- data[-train.index, ]

full_model <- glm(STATUS ~ ., data = train.df, family = binomial())
summary(full_model)


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



library(ggplot2)
library(pROC)

roc_curve <- roc(predictions$STATUS, predictions$predicted_status)
plot(roc_curve, main="ROC Curve for Backward Selection Model")
auc(roc_curve)


ggplot(predictions, aes(x = predicted_status, fill = as.factor(STATUS))) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.5) +
  scale_fill_manual(values = c("1" = "blue", "0" = "yellow")) +
  labs(title = "Predicted Probability Distribution by Actual Status",
       x = "Predicted Probability",
       fill = "Actual Status") +
  theme_minimal()


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

