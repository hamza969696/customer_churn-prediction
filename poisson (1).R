
p1 <- set_prior("normal(10, 0.5)", class = "b", coef = c("Gender", "Partner", "Dependents",
                                                         "Phone_Service", "Multiple_Lines", "Internet_Service",
                                                        "Online_Security", "Device_Protection", "Tech_Support", "Streaming_TV", "Streaming_Movies",
                                                        "Contract", "Paperless_Billing", "Payment_Method"))
p2<-prior("student_t(3,5,6)",class="b",coef=c("Tenure_Months","Monthly_Charges"))

prior_intercept <- prior("student_t(10, 2, 1)", class = "Intercept")
prior_intercept1 <- prior("normal(0.5,0.1)", class = "Intercept")
# Combine the priors for the coefficients and intercept
p_pois <- c(p1, prior_intercept1,p2)



m2 <- brm(
  Churn_Score ~ Gender + Partner + Dependents + Tenure_Months + Phone_Service +
    Multiple_Lines + Internet_Service + Online_Security + Device_Protection +
    Tech_Support + Streaming_TV + Streaming_Movies + Contract + Paperless_Billing + Payment_Method + Monthly_Charges,
  data = training_scaled, family = poisson(link = "log"))

m2_int <- brm(
  Churn_Score ~ Gender + Partner + Dependents + Tenure_Months + Phone_Service +
    Multiple_Lines + Internet_Service + Online_Security + Device_Protection +
    Tech_Support + Streaming_TV + Streaming_Movies + Contract + Paperless_Billing + Payment_Method + Monthly_Charges+Online_Security*Internet_Service,
  data = training_scaled, family = poisson(link = "log"))


pp_check(m2_int)
pp_check(m2_int,type="hist")

predictors_train_p <- training_scaled[, -which(names(test_scaled) %in% c("Churn_Value", "Churn_Score"))]
target_train_p <- test_scaled$Churn_Score


predicted_counts <- posterior_predict(m2, newdata =predictors_train_p , summary = FALSE)
predicted_training_col_p<-colMeans(predicted_counts)


predicted_binary_train <- ifelse(predicted_training_col > 70, 1, 0)

confusionMatrix(factor(predicted_binary), factor(training_scaled$Churn_Value))


#vaidation
predictors_val_p <- validation_scaled[, -which(names(validation_scaled) %in% c("Churn_Value", "Churn_Score"))]
target_val_p <- validation_scaled$Churn_Score


predicted_counts_val <- posterior_predict(m2, newdata =predictors_val_p , summary = FALSE)
predicted_val_col_p<-colMeans(predicted_counts_val)


predicted_binary_val <- ifelse(predicted_val_col_p > 70, 1, 0)

confusionMatrix(factor(predicted_binary_val), factor(validation_scaled$Churn_Value))



#test
predictors_test_p <- test_scaled[, -which(names(validation_scaled) %in% c("Churn_Value", "Churn_Score"))]
target_test_p <- test_scaled$Churn_Score


predicted_counts_test <- posterior_predict(m2, newdata =predictors_test_p , summary = FALSE)
predicted_test_col_p<-colMeans(predicted_counts_test)


predicted_binary_test <- ifelse(predicted_test_col_p > 70, 1, 0)

confusionMatrix(factor(predicted_binary_test), factor(test_scaled$Churn_Value))
m2