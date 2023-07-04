




p <- set_prior("normal(0, 0.01)", class = "b", coef = c("Gender", "Partner", "Dependents",
                                                        "Tenure_Months", "Phone_Service", "Multiple_Lines", "Internet_Service",
                                                        "Online_Security", "Device_Protection", "Tech_Support", "Streaming_TV", "Streaming_Movies",
                                                        "Contract", "Paperless_Billing", "Payment_Method", "Monthly_Charges"))



# Fit the model with the updated priors
m1 <- brm(
  Churn_Value ~ Gender + Partner + Dependents + Tenure_Months + Phone_Service +
    Multiple_Lines + Internet_Service + Online_Security + Device_Protection +
    Tech_Support + Streaming_TV + Streaming_Movies + Contract + Paperless_Billing + Payment_Method + Monthly_Charges,
  data = training_scaled, family = bernoulli(link = 'logit'), prior = p)

m1_int <- brm(
  Churn_Value ~ Gender + Partner + Dependents + Tenure_Months + Phone_Service +
    Multiple_Lines + Internet_Service + Online_Security + Device_Protection +
    Tech_Support + Streaming_TV + Streaming_Movies + Contract + Paperless_Billing + Payment_Method + Monthly_Charges+Online_Security*Internet_Service,
  data = training_scaled, family = bernoulli(link = 'logit'), prior = p)


pp_check(m1)
pp_check(m1,type="hist")

p_train <- fitted(m1)

# Calculate the accuracy of the model on the training set
accuracy_train <- mean(ifelse(training_scaled$Churn_Value == 1, p_train >= 0.7, p_train < 0.7))

#working on validation
# Separate the predictors and the target variable
predictors_val <- validation_scaled[, -which(names(validation_scaled) %in% c("Churn_Value", "Churn_Score"))]
target_val <- validation_scaled$Churn_Value

predictions_val <- posterior_predict(m1, newdata = predictors_val, type = "response")
predicted_val_col<-colMeans(predictions_val)

# Convert probabilities to class labels
predicted_classes <- ifelse(predicted_val_col > 0.7, 1, 0)

# Calculate the accuracy
accuracy_val <- mean(predicted_classes == target)





#working on test
# Separate the predictors and the target variable
predictors_test <- test_scaled[, -which(names(test_scaled) %in% c("Churn_Value", "Churn_Score"))]
target_test <- test_scaled$Churn_Value


predictions_test <- posterior_predict(m1, newdata = predictors_test, type = "response")
predicted_test_col<-colMeans(predictions_test)

# Convert probabilities to class labels
predicted_classes_test <- ifelse(predicted_test_col > 0.7, 1, 0)

# Calculate the accuracy
accuracy_test <- mean(predicted_classes_test == target_test)

confusionMatrix(factor(predicted_classes_test),factor(test_scaled$Churn_Value))


