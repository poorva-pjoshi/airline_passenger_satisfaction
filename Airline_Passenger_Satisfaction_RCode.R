#GROUP 9: Poorva Joshi, Rupam Kalita, Jaya Mundre
#ALY6015 (Intermediate Analytics), Prof. Richard
#Final Project Draft Report

#Clear the environment

cat('\014') # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()['RStudioGD']), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) #clears packages
options(scipen = 100) #disables scientific notation for entire R session

#Install packages
library(pacman)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(broom)
library(flextable)  #to make dynamic tables
library(car)    #provide functions (vif) for regression diagnostics, model comparisons, and other statistical analyses 
library(leaps)   #to perform all subset regression
library(olsrr)  #mallow's CP
library(gridExtra)   #to combine multiple plots into a single plot 
library(stargazer)
library(gridExtra)
library(corrplot)


#Read the Airline Passenger Satisfaction (APS)
APS <- read.csv('APS.csv', header = TRUE)
APS


################################################################################
                   #Perform Exploratory Data Analysis 
################################################################################


colnames(APS)
summary(APS)
str(APS)
glimpse(APS)
head(APS)
tail(APS)
unique(APS$satisfaction) 
unique(APS$gate_location) 
unique(APS$class) 
unique(APS$type_of_travel)

library(psych)
Table_1 <- describe(APS)
Table_1_df <- as.data.frame(Table_1)
write.csv(Table_1_df, "Table_1.csv", row.names = FALSE)


#Cleaning the data
p_load(janitor)
APS <- clean_names(APS)

#Missing Values
missing_values <- sum(is.na(APS))
missing_values

#We want to replace missing values in a specific column arrival_delay.
APS$arrival_delay[is.na(APS$arrival_delay)] <- 0

#Convert Categorical variables
APS$gender <- as.factor(APS$gender)
APS$customer_type <- as.factor(APS$customer_type)
APS$type_of_travel <- as.factor(APS$type_of_travel)
APS$class <- as.factor(APS$class)
APS <- mutate(APS, satisfaction_binary = ifelse(satisfaction == "Satisfied", 1, 0))

#view(APS)

#Remove Unnecessary Columns
APS <- APS[, -1]


################################################################################
                     # Frequency distribution for Gender, 
                     # Customer Type, Type of travel and Class 
################################################################################


##### Gender
gender_freq <- table(APS$gender)
gender_freq_percent <- prop.table(gender_freq) * 100

gender_summary <- data.frame(Gender = names(gender_freq),
                             Count = as.numeric(gender_freq),
                             Percentage = gender_freq_percent)

print("Frequency distribution for Gender:")
print(gender_summary)


##### Customer type
customer_type_freq <- table(APS$customer_type)
customer_type_freq_percent <- prop.table(customer_type_freq) * 100
customer_type_summary <- data.frame(Customer.Type = names(customer_type_freq),
                                    Count = as.numeric(customer_type_freq),
                                    Percentage = customer_type_freq_percent)

print("Frequency distribution for Customer Type:")
print(customer_type_summary)


##### Type of Travel
type_of_travel_freq <- table(APS$type_of_travel)
type_of_travel_freq_percent <- prop.table(type_of_travel_freq) * 100
type_of_travel_summary <- data.frame(Type.of.Travel = names(type_of_travel_freq),
                                     Count = as.numeric(type_of_travel_freq),
                                     Percentage = type_of_travel_freq_percent)

print("Frequency distribution for Type of Travel:")
print(type_of_travel_summary)


##### Class
class_freq <- table(APS$class)
class_freq_percent <- prop.table(class_freq) * 100
class_summary <- data.frame(Class = names(class_freq),
                            Count = as.numeric(class_freq),
                            Percentage = class_freq_percent)

print("Frequency distribution for Class:")
print(class_summary)


#####Descriptive Statistics
Desc <- c("age", "flight_distance", "departure_delay", "arrival_delay")

Desc_stats <- sapply(APS[, Desc], function(x) {
  c(mean = mean(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE))
})

Desc_stats <- round(Desc_stats, digits = 2)

print("Descriptive Statistics for Desc:")
print(Desc_stats)

write.csv(Desc_stats, 'Table_2.1.csv', row.names = FALSE)


################################################################################
                                #PLOTS
################################################################################


#1 Combined Bar chart of age groups with color based on satisfaction
APS$age_group <- cut(APS$age, breaks = c(0, 20, 30, 40, 50, 60, Inf), labels = c("0-20", "21-30", "31-40", "41-50", "51-60", "61+"))

age_plot <- ggplot(APS, aes(x = age_group, fill = satisfaction)) +
  geom_bar(position = "dodge") +
  labs(title = "Bar Chart of Age Groups",
       x = "Age Group",
       y = "Count",
       fill = "Satisfaction") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility


# Convert Miles to KM
flight_distance_km <- APS$flight_distance * 1.60934

# Define custom breaks and labels
breaks <- c(0, 500, 1000, 1500, Inf)
labels <- c("(0,500]", "(500,1000]", "(1000,1500]", "(1500,Inf]")

#Stacked bar chart of age groups with color based on flight_distance
distance_plot <- ggplot(APS, aes(x = age_group, fill = cut(flight_distance_km, breaks = breaks, labels = labels))) +
  geom_bar(position = "dodge") +
  labs(title = "Stacked Bar Chart of Age Groups by Flight Distance",
       x = "Age Group",
       y = "Count",
       fill = "Flight Distance (km)") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better visibility
  scale_fill_manual(values = c("(0,500]" = "lightblue", "(500,1000]" = "lightcoral", "(1000,1500]" = "lightgreen", "(1500,Inf]" = "lightpink"),
                    name = "Flight Distance (km)",
                    labels = c("0 to 500 km", "500 to 1,000 km", "1,000 to 1,500 km", "1,500 km and above"))


# Combine the plots side by side
library(gridExtra)
grid.arrange(age_plot, distance_plot, ncol = 2)


#2. Select numerical variables for histograms
numeric_vars <- c("age", "flight_distance", "departure_delay", "arrival_delay", "cleanliness")

# Plot histograms for selected numerical variables
hist_plots <- lapply(numeric_vars, function(var) {
  ggplot(APS, aes(x = !!sym(var))) +
    geom_histogram(fill = "skyblue", color = "white", bins = 20) +
    labs(title = paste("Histogram of", var),
         x = var,
         y = "Frequency") 
})

# Display the histograms
grid.arrange(grobs = hist_plots, ncol = 2)


#3. Box plot of 'age' by 'satisfaction'
ggplot(APS, aes(x = satisfaction, y = age, fill = satisfaction)) +
  geom_boxplot(width = 0.1) +
  labs(title = "Box Plot of Age by Satisfaction",
       x = "Satisfaction",
       y = "Age") +
  theme_classic()

# Box plot of 'flight_distance' by 'satisfaction'
ggplot(APS, aes(x = satisfaction, y = flight_distance, fill = satisfaction)) +
  geom_boxplot(width = 0.1) +
  labs(title = "Box Plot of Flight Distance by Satisfaction",
       x = "Satisfaction",
       y = "Flight Distance") +
  theme_classic()


# Convert data to long format for better use with ggplot
long_data <- tidyr::gather(APS, variable, value, age, flight_distance)

# Combined box plots for 'age', 'flight_distance', and 'departure_delay'
ggplot(long_data, aes(x = satisfaction, y = value, fill = satisfaction)) +
  geom_boxplot(width = 0.1) +
  labs(title = "Combined Box Plots for Numeric Variables by Satisfaction",
       x = "Satisfaction",
       y = "") +
  facet_wrap(~ variable, scales = "free_y", ncol = 1) +
  theme_classic() 



################################################################################
                                    #Q1
################################################################################

#QN- Which percentage of airline passengers are satisfied? 
#Does it vary by customer type? 
#What about type of travel?  

# Remove leading and trailing whitespaces from 'satisfaction' variable
APS$satisfaction <- trimws(APS$satisfaction)

unique(APS$satisfaction)

# Calculate the overall satisfaction percentage
overall_satisfaction_percentage <- APS %>%
  summarize(percentage_satisfied = mean(satisfaction_binary == 1) * 100)
overall_satisfaction_percentage

cat("Overall satisfaction percentage: ", overall_satisfaction_percentage$percentage_satisfied, "%\n")

str(APS)

# Calculate satisfaction percentages by customer type
satisfaction_by_customer_type <- APS %>%
  group_by(customer_type) %>%
  summarize(percentage_satisfied = mean(satisfaction_binary == 1) * 100)

cat("\nSatisfaction percentages by customer type:\n")
print(satisfaction_by_customer_type)

# Calculate satisfaction percentages by type of travel
satisfaction_by_type_of_travel <- APS %>%
  group_by(type_of_travel) %>%
  summarize(percentage_satisfied = mean(satisfaction_binary == 1) * 100)

cat("\nSatisfaction percentages by type of travel:\n")
print(satisfaction_by_type_of_travel)



################################################################################
                                    #Q2
################################################################################

#QN- What is the customer profile for a repeating airline passenger?
  
returning_passengers <- APS %>% filter(customer_type == "Returning")

# Age distribution
ggplot(returning_passengers, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Age Distribution of Returning Passengers", x = "Age", y = "Frequency")+
  theme_classic()

# Gender distribution
ggplot(returning_passengers, aes(x = gender, fill = gender)) +
  geom_bar(width = 0.2) +
  labs(title = "Gender Distribution of Returning Passengers", x = "Gender", y = "Count")+
  theme_classic()+
  geom_text(
    aes(label = ..count..), 
    stat = "count",
    position = position_stack(vjust = 0.9),
    show.legend = FALSE
  )

# Satisfaction distribution
ggplot(returning_passengers, aes(x = satisfaction, fill = satisfaction)) +
  geom_bar(width = 0.3) +
  labs(title = "Satisfaction Distribution of Returning Passengers", x = "Satisfaction", y = "Count") +
  geom_text(
    aes(label = ..count..), 
    stat = "count",
    position = position_stack(vjust = 0.9),
    show.legend = FALSE
  )+
  theme_classic()



################################################################################
                                    #Q3
################################################################################

#QN- Does flight distance affect customer preferences or flight patterns?

#Correlation between all numerical columns
numerical_columns <- APS[sapply(APS, is.numeric)]
correlation_matrix <- round(cor(numerical_columns), 1)

print(correlation_matrix)

ggcorrplot(correlation_matrix, 
           type = "lower", 
           hc.order = TRUE,
           show.legend = TRUE,
           ggtheme = theme_classic,
           tl.col = "black",
           method = "square", 
           colors = c("red", "white", "blue"), 
           tl.srt = 30,
           title = "Correlation Table 1",
           outline.color = 'black',
           lab = TRUE)


# Multi-variable Linear Regression to predict satisfaction
lm_model <- lm(satisfaction_binary ~ flight_distance + type_of_travel + check_in_service + online_boarding + 
                 on_board_service + seat_comfort + leg_room_service + ease_of_online_booking + food_and_drink +
                 online_boarding + in_flight_wifi_service + class + leg_room_service + 
                 seat_comfort + food_and_drink, data = APS)

summary(lm_model)
coef(lm_model)
broom::glance(lm_model)

# Make predictions with the linear regression model
predicted_satisfaction <- predict(lm_model)

# Display the summary and predicted values
head(predicted_satisfaction)

plot(lm_model)

#Identify the outliers
residuals <- residuals(lm_model)
residuals 
plot(residuals, main = "Residuals vs Fitted Values", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

#Using GLM - predict satisfaction
library(caret)
set.seed(3456)

trainindex <- createDataPartition(APS$satisfaction_binary, times = 1, p = 0.7, list = FALSE)
train <- APS[trainindex,]
test <- APS[-trainindex,]

model <- glm(satisfaction_binary ~ flight_distance + type_of_travel + check_in_service + online_boarding + 
               on_board_service + seat_comfort + leg_room_service + ease_of_online_booking + food_and_drink +
               online_boarding + in_flight_wifi_service + class + leg_room_service + 
               seat_comfort + food_and_drink, data = train, family = binomial)
summary(model)


# Confusion matrix for train set
train_prob <- predict(model, newdata = train, type = "response")
train_predictions <- ifelse(train_prob > 0.5, 1, 0)
conf_matrix_train <- table(Actual = train$satisfaction_binary, Predicted = train_predictions)
print(conf_matrix_train)

# Confusion matrix for test set
predictions <- predict(model, newdata = test, type = "response")
predicted_classes <- ifelse(predictions > 0.5, 1, 0)
conf_matrix_test <- table(Actual = test$satisfaction_binary, Predicted = predicted_classes)
print(conf_matrix_test)

# Calculate accuracy, sensitivity, and specificity
accuracy_train <- sum(diag(conf_matrix_train)) / sum(conf_matrix_train)
sensitivity_train <- conf_matrix_train[2, 2] / sum(conf_matrix_train[2, ])
specificity_train <- conf_matrix_train[1, 1] / sum(conf_matrix_train[1, ])
precision_train <- conf_matrix_train[2, 2] / sum(conf_matrix_train[, 2])

cat("\nAccuracy on the training set:", accuracy_train)
cat("\nSensitivity on the training set:", sensitivity_train)
cat("\nSpecificity on the training set:", specificity_train)
cat("\nPrecision on the training set:", precision_train)


library(pROC)
# Create ROC curve object
roc_curve <- roc(test$satisfaction_binary, predictions)
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

#Calculate and interpret the AUC
auc_value <- round(auc(roc_curve), 3)
text(0.8, 0.2, paste("AUC =", auc_value), col = "blue")
abline(a = 0, b = 1, lty = 2, col = "gray")
cat("AUC for LR on the test set:", auc_value)



################################################################################
                                    #Q4
################################################################################

#QN- Which factors contribute to customer satisfaction the most? What about dissatisfaction?

# Perform ANOVA for each factor
factors <- c("type_of_travel", "class", "in_flight_service", "food_and_drink", 
             "ease_of_online_booking", "on_board_service", "seat_comfort", 
             "leg_room_service", "cleanliness", "departure_and_arrival_time_convenience", 
             "check_in_service", "online_boarding", "gate_location", 
             "in_flight_wifi_service", "in_flight_entertainment", "baggage_handling")

# Create an empty data frame to store ANOVA results
anova_results <- data.frame(Factor = character(), F_value = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

# Perform ANOVA for each factor and store results
for (factor in factors) {
  anova_result <- aov(satisfaction_binary ~ get(factor), data = APS)
  f_value <- summary(anova_result)[[1]]$`F value`[1]
  p_value <- summary(anova_result)[[1]]$"Pr(>F)"[1]
  anova_results <- rbind(anova_results, data.frame(Factor = factor, F_value = f_value, p_value = p_value))
}

# Sort results by p-value
anova_results <- anova_results %>% arrange(p_value)

# Print ANOVA results
print("ANOVA results:")
print(anova_results)

# Factors contributing to customer satisfaction the most
satisfaction_factors <- anova_results$Factor[1:5]  # Top 5 factors with lowest p-values
print("Factors contributing to customer satisfaction the most:")
print(satisfaction_factors)

# Factors contributing to customer dissatisfaction the most
dissatisfaction_factors <- anova_results$Factor[tail(seq_along(anova_results$Factor), 5)]# Bottom 5 factors with highest p-values
print("Factors contributing to customer dissatisfaction the most:")
print(dissatisfaction_factors)



################################################################################
                          #Regularization - Is our model overfit?
################################################################################


library(glmnet)
library(caret)

#Split Data 80/20
set.seed(3456)

trainindex <- createDataPartition(APS$satisfaction_binary, times = 1, p = 0.8, list = FALSE)
train <- APS[trainindex,]
test <- APS[-trainindex,]  # -ve sign represents the rest of the data which is not in train set

#glmnet requires matrix not dataframe
train_x <- model.matrix(satisfaction_binary ~ flight_distance + type_of_travel + check_in_service + online_boarding + 
                          on_board_service + seat_comfort + leg_room_service + ease_of_online_booking + food_and_drink +
                          online_boarding + in_flight_wifi_service + class + leg_room_service + 
                          seat_comfort + food_and_drink, train)[,-1]

test_x <- model.matrix(satisfaction_binary ~ flight_distance + type_of_travel + check_in_service + online_boarding + 
                         on_board_service + seat_comfort + leg_room_service + ease_of_online_booking + food_and_drink +
                         online_boarding + in_flight_wifi_service + class + leg_room_service + 
                         seat_comfort + food_and_drink, test)[,-1]

train_y <- train$satisfaction_binary
test_y <- test$satisfaction_binary


                        #####     RIDGE     #####

#Estimate the lambda.min and lambda.1se values
#find the best lambda using cross - validation 
set.seed(3456)

#optimal value of lambda; minimizes the prediction error
#lambda min:  minimizes out of sample loss
#lambda max: largest value of lambda within 1 standard error of lambda min
cv_fit <- cv.glmnet(train_x, train_y, alpha = 0)  # alpha = 0 for Ridge Regression

lambda_min_r <- cv_fit$lambda.min
lambda_1se_r <- cv_fit$lambda.1se

# Compare and discuss the values
lambda_min_r
lambda_1se_r

log(cv_fit$lambda.min)
log(cv_fit$lambda.1se)

plot(cv_fit)

############
#Fit a Ridge regression model 
# alpha = 0 for Ridge (L1)

model_min_r <- glmnet(train_x, train_y, alpha = 0, lambda = lambda_min_r)
model_min_r
coef(model_min_r) #display regression coefficients

model_1se_r <- glmnet(train_x, train_y, alpha = 0, lambda = lambda_1se_r)
model_1se_r
coef(model_1se_r)

############
#Calculate RMSE of train set
preds_train_r <- predict(model_1se_r, newx = train_x)
train_RMSE_r_1se <- RMSE(train_y, preds_train_r)
preds_train_r <- predict(model_min_r, newx = train_x)
train_RMSE_r_min <- RMSE(train_y, preds_train_r)

#Calculate RMSE of test set
preds_test_r <- predict(model_1se_r, newx = test_x)
test_RMSE_r_1se <- RMSE(test_y, preds_test_r)
preds_test_r <- predict(model_min_r, newx = test_x)
test_RMSE_r_min <- RMSE(test_y, preds_test_r)

#compare RMSE value
train_RMSE_r_min
train_RMSE_r_1se

test_RMSE_r_min
test_RMSE_r_1se


                         #####      LASSO     ######

#Fit a Lasso regression model 
# alpha = 1 for Ridge (L2)
cv_fit_l <- cv.glmnet(train_x, train_y, alpha = 1)  # alpha = 1 for LASSO Regression
lambda_min_l <- cv_fit_l$lambda.min
lambda_1se_l <- cv_fit_l$lambda.1se

# Compare and discuss the values
lambda_min_l
lambda_1se_l

log(cv_fit_l$lambda.min)
log(cv_fit_l$lambda.1se)

plot(cv_fit_l)

###########
#Fit a LASSO regression model 
# alpha = 1 for LASSO (L2)
model_min_l <- glmnet(train_x, train_y, alpha = 1, lambda = lambda_min_l)
model_min_l
coef(model_min_l) #display regression coefficients

model_1se_l <- glmnet(train_x, train_y, alpha = 1, lambda = lambda_1se_l)
model_1se_l
coef(model_1se_l) # 11 coefficients reduced to zero


############
#Calculate RMSE of train set
preds_train_l <- predict(model_1se_l, newx = train_x)
train_RMSE_l <- RMSE(train_y, preds_train_l)

#Calculate RMSE of test set
preds_test_l <- predict(model_1se_l, newx = test_x)
test_RMSE_l <- RMSE(test_y, preds_test_l)

#compare RMSE values
train_RMSE_l
test_RMSE_l


                      #####      ElasticNet     ######

cv_fit_en <- cv.glmnet(train_x, train_y, alpha = 0.5)  # alpha = 0.5 for ElasticNet Regression
lambda_min_en <- cv_fit_en$lambda.min
lambda_1se_en <- cv_fit_en$lambda.1se

# Compare and discuss the values
lambda_min_en
lambda_1se_en

log(cv_fit_en$lambda.min)
log(cv_fit_en$lambda.1se)
plot(cv_fit_en)

enet_model_min <- glmnet(train_x, train_y, alpha = 0.5, lambda = lambda_min_en)
enet_model_min
coef(enet_model_min) #display regression coefficients

enet_model_1se <- glmnet(train_x, train_y, alpha = 0.5, lambda = lambda_1se_en)
enet_model_1se
coef(enet_model_1se) # 11 coefficients reduced to zero

predict_train_enet <- predict(enet_model_1se, newx = train_x)
train_RMSE_enet <- RMSE(train_y, predict_train_enet)


############
predict_test_enet <- predict(enet_model_1se, newx = test_x)
test_RMSE_enet <- RMSE(test_y, predict_test_enet)
train_RMSE_enet
test_RMSE_enet


############
cat("Ridge Training RMSE (lambda.min):", train_RMSE_r_min, "\n")
cat("Ridge Test RMSE (lambda.min):", test_RMSE_r_min, "\n")
cat("Ridge Training RMSE (lambda.1se):", train_RMSE_r_1se, "\n")
cat("Ridge Test RMSE (lambda.1se):", test_RMSE_r_1se, "\n")

cat("LASSO Training RMSE:", train_RMSE_l, "\n")
cat("LASSO Test RMSE:", test_RMSE_l, "\n")

cat("ElasticNet Training RMSE:", train_RMSE_enet, "\n")
cat("ElasticNet Test RMSE:", test_RMSE_enet, "\n")

cat("Ridge with lambda.min performed the best among others:", train_RMSE_r_min, "\n")


                  #####      stepwise selection     ######

library(MASS)
# Stepwise Selection using AIC
stepwise_model <- stepAIC(lm(satisfaction_binary ~ ., data = train))

# Ridge Regression
ridge_model <- cv.glmnet(train_x, train_y, alpha = 0, nfolds = 10)

# LASSO Regression
lasso_model <- cv.glmnet(train_x, train_y, alpha = 1, nfolds = 10)

# ElasticNet Regression
elasticnet_model <- cv.glmnet(train_x, train_y, alpha = 0.5, nfolds = 10)


# Evaluate models on the test set
# Stepwise
stepwise_predictions_test <- predict(stepwise_model, newdata = test)
stepwise_RMSE_test <- sqrt(mean((test$satisfaction_binary - stepwise_predictions_test)^2))

# Ridge
ridge_predictions_test <- predict(ridge_model, newx = test_x, s = "lambda.min")
ridge_RMSE_test <- sqrt(mean((test_y - ridge_predictions_test)^2))

# LASSO
lasso_predictions_test <- predict(lasso_model, newx = test_x, s = "lambda.min")
lasso_RMSE_test <- sqrt(mean((test_y - lasso_predictions_test)^2))

# ElasticNet
elasticnet_predictions_test <- predict(elasticnet_model, newx = test_x, s = "lambda.min")
elasticnet_RMSE_test <- sqrt(mean((test_y - elasticnet_predictions_test)^2))


# Evaluate models on the train set
# Stepwise
stepwise_predictions_train <- predict(stepwise_model, newdata = train)
stepwise_RMSE_train <- sqrt(mean((train$satisfaction_binary - stepwise_predictions_train)^2))

# Ridge
ridge_predictions_train <- predict(ridge_model, newx = train_x, s = "lambda.min")
ridge_RMSE_train <- sqrt(mean((train_y - ridge_predictions_train)^2))

# LASSO
lasso_predictions_train <- predict(lasso_model, newx = train_x, s = "lambda.min")
lasso_RMSE_train <- sqrt(mean((train_y - lasso_predictions_train)^2))

# ElasticNet
elasticnet_predictions_train <- predict(elasticnet_model, newx = train_x, s = "lambda.min")
elasticnet_RMSE_train <- sqrt(mean((train_y - elasticnet_predictions_train)^2))

# Creating a table with 2 columns using data.frame()
table <- data.frame(ColumnName = c("Stepwise", "Ridge", "Lasso", "ElasticNet"),
                    RMSE_Train = c(stepwise_RMSE_train, ridge_RMSE_train, lasso_RMSE_train, elasticnet_RMSE_train),
                    RMSE_Test = c(stepwise_RMSE_test, ridge_RMSE_test, lasso_RMSE_test, elasticnet_RMSE_test))
view(table)

################################################################################
