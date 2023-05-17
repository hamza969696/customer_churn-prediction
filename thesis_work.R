logistic_model <- brm(
  formula=Churn_Value ~ 1 + Payment_Method_Electronic_check + Contract_Month_to_month +
    Streaming_TV_Yes + Tech_Support_No + Online_Backup_No + Online_Backup_No+Online_Security_No +
    Internet_Service_Fiber_optic + Multiple_Lines_Yes + Dependents_No + Paperless_Billing_Yes +
    Monthly_Charges, 
  data=train,
  family = bernoulli(link='logit'),
  prior = c(
    prior_intercept,
    prior_pm_elec_check,
    prior_con_mtmt,
    prior_streaming_tv,
    prior_tech_support_no,
    prior_online_backup_no,
    prior_online_security_no,
    prior_int_service_fiber_optic,
    prior_mult_lines_yes,
    prior_dependents_no,
    prior_paperless_billing_yes,
    prior_monthly_charges
  )
)

pp_check(logistic_model, ndraws = 200)

pp_check(logistic_model, type = "stat_2d")

posterior_preds_log <- posterior_predict(logistic_model, newdata = val)
predicted_means_log <- colMeans(posterior_preds_log)

# Convert the predicted means into binary classes
threshold <- 0.7
predicted_classes_log <- ifelse(predicted_means_log > threshold, 1, 0)

# Calculate accuracy
val_accuracy_log <- mean(predicted_classes_log == val$Churn_Value)
print(paste("Val accuracy:", val_accuracy_log))

# Create a confusion matrix
library(caret)
confusionMatrix(factor(predicted_classes), factor(val$Churn_Value))

#accuracy in test set
posterior_preds_log_test <- posterior_predict(logistic_model, newdata = test)
predicted_means_log_test <- colMeans(posterior_preds_log_test)


predicted_classes_log_test <- ifelse(predicted_means_log_test > threshold, 1, 0)


test_accuracy_log <- mean(predicted_classes_log_test == test$Churn_Value)
print(paste("test accuracy:", test_accuracy_log))

library(yardstick)

#f1 score
f1_score <- function(data, truth, estimate) {
  tp <- sum(truth == 1 & estimate == 1)
  fp <- sum(truth == 0 & estimate == 1)
  fn <- sum(truth == 1 & estimate == 0)
  
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  
  f1 <- 2 * precision * recall / (precision + recall)
  
  return(f1)
}


f1_score(data = val, truth = val$Churn_Value, estimate = predicted_classes_log)

#interaction
logistic_model_interaction <- brm(
  formula=Churn_Value ~ 1 + Payment_Method_Electronic_check + Contract_Month_to_month +
    Streaming_TV_Yes + Tech_Support_No + Online_Backup_No + Online_Backup_No + 
    Online_Security_No * Internet_Service_Fiber_optic +
    Internet_Service_Fiber_optic + Multiple_Lines_Yes + Dependents_No + 
    Paperless_Billing_Yes + Monthly_Charges, 
  data=train,
  family = bernoulli(link='logit'),
  prior = c(
    prior_intercept,
    prior_pm_elec_check,
    prior_con_mtmt,
    prior_streaming_tv,
    prior_tech_support_no,
    prior_online_backup_no,
    prior_online_security_no,
    prior_int_service_fiber_optic,
    prior_mult_lines_yes,
    prior_dependents_no,
    prior_paperless_billing_yes,
    prior_monthly_charges
  )
)

