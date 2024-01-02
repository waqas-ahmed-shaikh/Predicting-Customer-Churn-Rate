#Read dataset
telco <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")

View(telco)
dim(telco)

#EDA
#check for NA values
sum(is.na(telco))
#Remove observations with NA values
na.indices <- rowSums(is.na(telco))>0
telco <- telco[!na.indices,]
dim(telco)

#Check class of each variable
lapply(telco,class)

library(skimr)
skim(telco)

#Check class imbalance. 5163 "No" and 1869 "Yes", requires oversampling for the class "Yes"
table(telco$Churn)

library(tidyverse)
#Check distribution of continuous variables - Tenure, MonthlyCharges, TotalCharges
plot(density(telco$tenure)) #not normally distributed
hist(telco$tenure)

plot(density(telco$MonthlyCharges)) #not normally distributed
hist(telco$MonthlyCharges)

plot(density(telco$TotalCharges)) #not normally distributed
hist(telco$TotalCharges)

#Check outliers for continuous variables - Tenure, MonthlyCharges, TotalCharges
ggplot(data=telco, mapping=aes(x=tenure))+geom_boxplot() #No outliers
ggplot(data=telco, mapping=aes(x=MonthlyCharges))+geom_boxplot() #No outliers
ggplot(data=telco, mapping=aes(x=TotalCharges))+geom_boxplot() #No outliers

#Check distribution of categorical variables
ggplot(data=telco)+geom_bar(mapping=aes(x=gender))
ggplot(data=telco)+geom_bar(mapping=aes(x=SeniorCitizen))
ggplot(data=telco)+geom_bar(mapping=aes(x=Partner))
ggplot(data=telco)+geom_bar(mapping=aes(x=Dependents))
ggplot(data=telco)+geom_bar(mapping=aes(x=PhoneService))
ggplot(data=telco)+geom_bar(mapping=aes(x=MultipleLines))
ggplot(data=telco)+geom_bar(mapping=aes(x=InternetService))
ggplot(data=telco)+geom_bar(mapping=aes(x=OnlineSecurity))
ggplot(data=telco)+geom_bar(mapping=aes(x=OnlineBackup))
ggplot(data=telco)+geom_bar(mapping=aes(x=DeviceProtection))
ggplot(data=telco)+geom_bar(mapping=aes(x=TechSupport))
ggplot(data=telco)+geom_bar(mapping=aes(x=StreamingMovies))
ggplot(data=telco)+geom_bar(mapping=aes(x=Contract))
ggplot(data=telco)+geom_bar(mapping=aes(x=PaperlessBilling))
ggplot(data=telco)+geom_bar(mapping=aes(x=PaymentMethod))
ggplot(data=telco)+geom_bar(mapping=aes(x=Churn))

#Converting binary variables to 1 and 0 and then converting them to factors
#gender
telco$gender <- ifelse(telco$gender=="Male",1,telco$gender)
telco$gender <- ifelse(telco$gender=="Female",0,telco$gender)
telco$gender <- as.factor(telco$gender)
class(telco$gender)
levels(telco$gender)
#SeniorCitizen
telco$SeniorCitizen <- as.factor(telco$SeniorCitizen)
class(telco$gender)
levels(telco$gender)
#Partner
telco$Partner <- ifelse(telco$Partner=="Yes",1,telco$Partner)
$Partner <- ifelse(telco$Partner=="No",0,telco$Partner)
telco$Partner <- as.factor(telco$Partner)
class(telco$Partner)
levels(telco$Partner)
#Dependents
telco$Dependents <- ifelse(telco$Dependents =="Yes",1,telco$Dependents )
telco$Dependents  <- ifelse(telco$Dependents =="No",0,telco$Dependents )
telco$Dependents  <- as.factor(telco$Dependents )
class(telco$Dependents)
levels(telco$Dependents)
#PhoneService
telco$PhoneService <- ifelse(telco$PhoneService =="Yes",1,telco$PhoneService )
telco$PhoneService  <- ifelse(telco$PhoneService =="No",0,telco$PhoneService )
telco$PhoneService  <- as.factor(telco$PhoneService)
class(telco$PhoneService)
levels(telco$PhoneService)
#MultipleLines
telco$MultipleLines <- ifelse(telco$MultipleLines =="Yes",1,telco$MultipleLines)
telco$MultipleLines <- ifelse(telco$MultipleLines =="No",0,telco$MultipleLines)
telco$MultipleLines <- ifelse(telco$MultipleLines =="No phone service",0,telco$MultipleLines)
telco$MultipleLines  <- as.factor(telco$MultipleLines)
class(telco$MultipleLines)
levels(telco$MultipleLines)
#InternetService
unique(telco$InternetService)
telco$InternetService_DSL <- 1*(telco$InternetService=="DSL")
telco$InternetService_FiberOptic <- 1*(telco$InternetService=="Fiber optic")
telco$InternetService_DSL  <- as.factor(telco$InternetService_DSL)
telco$InternetService_FiberOptic  <- as.factor(telco$InternetService_FiberOptic)
class(telco$InternetService_DSL)
levels(telco$InternetService_DSL)
class(telco$InternetService_FiberOptic )
levels(telco$InternetService_FiberOptic )
#OnlineSecurity
telco$OnlineSecurity <- ifelse(telco$OnlineSecurity =="Yes",1,telco$OnlineSecurity)
telco$OnlineSecurity <- ifelse(telco$OnlineSecurity =="No",0,telco$OnlineSecurity)
telco$OnlineSecurity <- ifelse(telco$OnlineSecurity =="No internet service",0,telco$OnlineSecurity)
telco$OnlineSecurity  <- as.factor(telco$OnlineSecurity)
class(telco$OnlineSecurity)
levels(telco$OnlineSecurity)
#OnlineBackup
telco$OnlineBackup <- ifelse(telco$OnlineBackup =="Yes",1,telco$OnlineBackup)
telco$OnlineBackup <- ifelse(telco$OnlineBackup =="No",0,telco$OnlineBackup)
telco$OnlineBackup <- ifelse(telco$OnlineBackup =="No internet service",0,telco$OnlineBackup)
telco$OnlineBackup  <- as.factor(telco$OnlineBackup)
class(telco$OnlineBackup)
levels(telco$OnlineBackup)
#DeviceProtection
telco$DeviceProtection <- ifelse(telco$DeviceProtection =="Yes",1,telco$DeviceProtection)
telco$DeviceProtection <- ifelse(telco$DeviceProtection =="No",0,telco$DeviceProtection)
telco$DeviceProtection <- ifelse(telco$DeviceProtection =="No internet service",0,telco$DeviceProtection)
telco$DeviceProtection  <- as.factor(telco$DeviceProtection)
class(telco$DeviceProtection)
levels(telco$DeviceProtection)
#TechSupport
telco$TechSupport <- ifelse(telco$TechSupport =="Yes",1,telco$TechSupport)
telco$TechSupport <- ifelse(telco$TechSupport =="No",0,telco$TechSupport)
telco$TechSupport <- ifelse(telco$TechSupport =="No internet service",0,telco$TechSupport)
telco$TechSupport  <- as.factor(telco$TechSupport)
class(telco$TechSupport)
levels(telco$TechSupport)
#StreamingTV
telco$StreamingTV <- ifelse(telco$StreamingTV =="Yes",1,telco$StreamingTV)
telco$StreamingTV <- ifelse(telco$StreamingTV =="No",0,telco$StreamingTV)
telco$StreamingTV <- ifelse(telco$StreamingTV =="No internet service",0,telco$StreamingTV)
telco$StreamingTV  <- as.factor(telco$StreamingTV)
class(telco$StreamingTV)
levels(telco$StreamingTV)
#StreamingMovies
telco$StreamingMovies <- ifelse(telco$StreamingMovies =="Yes",1,telco$StreamingMovies)
telco$StreamingMovies <- ifelse(telco$StreamingMovies =="No",0,telco$StreamingMovies)
telco$StreamingMovies <- ifelse(telco$StreamingMovies =="No internet service",0,telco$StreamingMovies)
telco$StreamingMovies  <- as.factor(telco$StreamingMovies)
class(telco$StreamingMovies)
levels(telco$StreamingMovies)
#Contract
telco$Contract_Month <- 1*(telco$Contract=="Month-to-month")
telco$Contract_1Year <- 1*(telco$Contract=="One year")
telco$Contract_Month <- as.factor(telco$Contract_Month)
telco$Contract_1Year <- as.factor(telco$Contract_1Year)
class(telco$Contract_Month)
class(telco$Contract_1Year)
#PaperlessBilling
telco$PaperlessBilling <- ifelse(telco$PaperlessBilling =="Yes",1,telco$PaperlessBilling)
telco$PaperlessBilling <- ifelse(telco$PaperlessBilling =="No",0,telco$PaperlessBilling)
telco$PaperlessBilling <- as.factor(telco$PaperlessBilling)
class(telco$PaperlessBilling)
levels(telco$PaperlessBilling)
#PaymentMethod
table(telco$PaymentMethod)
telco$PaymentMethod_BankTransfer <- 1*(telco$PaymentMethod=="Bank transfer (automatic)")
telco$PaymentMethod_CreditCard <- 1*(telco$PaymentMethod=="Credit card (automatic)")
telco$PaymentMethod_ECheck <- 1*(telco$PaymentMethod=="Electronic check")
telco$PaymentMethod_BankTransfer <- as.factor(telco$PaymentMethod_BankTransfer)
telco$PaymentMethod_CreditCard <- as.factor(telco$PaymentMethod_CreditCard)
telco$PaymentMethod_ECheck <- as.factor(telco$PaymentMethod_ECheck)
#Churn
telco$Churn <- ifelse(telco$Churn =="Yes",1,telco$Churn)
telco$Churn <- ifelse(telco$Churn =="No",0,telco$Churn)
telco$Churn <- as.factor(telco$Churn)
levels(telco$Churn)

#Rearranging the columns, keeping dummy variables, discarding original variables
churn <- telco %>% 
  as_tibble() %>% 
  select(gender,SeniorCitizen,Partner,Dependents,tenure,PhoneService,MultipleLines,InternetService_DSL,
         InternetService_FiberOptic,OnlineSecurity,OnlineBackup,DeviceProtection,
         TechSupport,StreamingTV,StreamingMovies,Contract_Month,Contract_1Year,PaperlessBilling,
         PaymentMethod_BankTransfer,PaymentMethod_CreditCard,PaymentMethod_ECheck,MonthlyCharges,TotalCharges,Churn)
View(churn)

#Rebalancing - using package "ROSE" (Random Over Sampling Examples)
install.packages("ROSE")
library(ROSE)

#Re-balancing and saving into new dataframe churnBS i.e. Churn Balanced Sample
churnBS <- ovun.sample(Churn~., data = churn, method = "both", p=0.5, seed= 123, N=7032)$data
table(churnBS$Churn)
ggplot(data=churnBS)+geom_bar(mapping=aes(x=Churn))
dim(churnBS)
View(churnBS)

#Re-balancing and saving into new dataframe churnOS i.e. Churn Over Sampled
churnOS <- ovun.sample(Churn~., data = churn, method = "over", p=0.5, seed= 123)$data
table(churnOS$Churn)
ggplot(data=churnOS)+geom_bar(mapping=aes(x=Churn))
dim(churnOS)
View(churnOS)


#train-test split
set.seed(1234)
ind <- sample(2, nrow(churnOS), replace = T, prob = c(0.7,0.3))
training <- churnOS[ind==1,]
test <- churnOS[ind==2,]

#Scaling

# Identify numeric and factor columns
numeric_columns <- sapply(churnOS, is.numeric)
factor_columns <- sapply(churnOS, is.factor)

# Extract numeric and factor columns
numeric_data <- churnOS[, numeric_columns, drop = FALSE]
factor_data <- churnOS[, factor_columns, drop = FALSE]

# Scale the numeric columns
scaled_numeric_data <- as.data.frame(scale(numeric_data))

# Combine the scaled numeric columns with the original factor columns
scaled_df <- cbind(scaled_numeric_data, factor_data)

training1 <- scaled_df[ind==1,]
test1 <- scaled_df[ind==2,]

#KNN Model using caret
library(caret)
#using cross validation. Accuracy is a parameter to select knn
trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3)
#scaling and fitting simultaneously
set.seed(321)
fit <- train(Churn ~ .,
             data = training,
             method = 'knn',
             tuneLength = 20,
             trControl = trControl,
             preProc = c("center","scale"))
fit
plot(fit)
varImp(fit)
plot(varImp(fit))

#predicting 
pred <- predict(fit, newdata = test)
confusionMatrix(pred, test$Churn)

predknn <- predict(fit, newdata= test, type = 'response')

#logistic regression
m <- glm(Churn ~ ., data = training1, family = "binomial")
summary(m)

#predictions for training data
p1 <- predict(m, training1, type = 'response')

#confusion matrix for training data
pred1 <- ifelse(p1>0.5, 1,0)
table(Predicted = pred1, Actual =training1$Churn)
pred1 <- as.factor(pred1)
confusionMatrix(pred1,training1$Churn)

#predictions for testing data
p2 <- predict(m, test1, type = 'response')

#confusion matrix for training data
pred2 <- ifelse(p2>0.5, 1,0)
pred2 <- as.factor(pred2)
table(Predicted = pred2, Actual =test1$Churn)
confusionMatrix(pred2 ,test1$Churn)


#plot ROC curve
library(pROC)
roc_curve <- roc(test1$Churn, p2)
plot(roc_curve, main = "ROC Curve for Testing Data", col = "blue", lwd = 2)

auc_value <- auc(roc_curve)
text(0.8, 0.2, paste("AUC =", round(auc_value, 2)), col = "blue", cex = 1.2)

#Now, only taking significant columns
summary_model <- summary(m)
insignificant_columns <- names(which(summary_model$coefficients[, "Pr(>|z|)"] > 0.05))
print(insignificant_columns)

m1 <- glm(Churn ~ ., data = training1[,-c(1,3,4,6,10,11,12,13,15,22)], family = "binomial")
summary(m1)

#taking predictions
p3 <- predict(m1, training1, type = "response")

#confusion matrix for training data
pred3 <- ifelse(p3>0.5, 1,0)
pred3 <- as.factor(pred3)
confusionMatrix(pred3 ,training1$Churn)

#taking predictions
p4 <- predict(m1, test1, type = "response")

#confusion matrix for test data
pred4 <- ifelse(p4>0.5, 1,0)
pred4 <- as.factor(pred4)
confusionMatrix(pred4 ,test1$Churn)

#plotting ROC curve
roc_curve1 <- roc(test1$Churn, p4)
plot(roc_curve1, main = "ROC Curve for Testing Data", col = "blue", lwd = 2)

auc_value1 <- auc(roc_curve1)
text(0.8, 0.2, paste("AUC =", round(auc_value1, 2)), col = "blue", cex = 1.2)



sum(churnOS$Churn==1)
sum(churnOS$)
#Random Forest
library(randomForest)
set.seed(1234)
#simple mode;
rf <- randomForest(Churn ~., data = training1)
print(rf)

#confusion matrix
rf$confusion

#predict training model
p4 <- predict(rf, training1)
confusionMatrix(p4, training1$Churn)

#predict testing model
p5 <- predict(rf, test1)
confusionMatrix(p5, test1$Churn)

#tune mtry (reducing OOb error by 0.05 percent)
t<- tuneRF(training1[,-24],training1[,24],
           stepFactor = 0.5,
           plot = TRUE,
           ntreeTry = 300,
           trace = TRUE,
           improve = 0.05)
#here, we can see the out of the box error is minimum at mtry 16, so now we will change our code a bit
rf1 <- randomForest(Churn ~., data = training1,
                    ntree = 300,
                    mtry = 16,
                    importance = TRUE,
                    proximity = TRUE)

plot(rf1)
varImpPlot(rf1)
p7 <- predict(rf1, test1, type = "response")
class(p7)
p7 <- as.numeric(p7)
#plotting roc curve
roc_curve7 <- roc(test1$Churn, p7)
plot(roc_curve7, main = "ROC Curve for Testing Data", col = "blue", lwd = 2)
auc_value7 <- auc(roc_curve7)
text(0.8, 0.2, paste("AUC =", round(auc_value7, 2)), col = "blue", cex = 1.2)



#Decision tree
library(rpart)
library(rpart.plot)
train.dt <- rpart(Churn ~ .,
              data = training,
              method = 'class',
              cp = 0.001,
              maxdepth = 5,
              model = TRUE)
prp(train.dt,
    type = 1,
    extra = 1,
    under = TRUE,
    split.font = 1,
    varlen = -10)
train.dt$cptable

prunefit <- prune(train.dt, cp=train.dt$cptable[which.min(train.dt$cptable[,'xerror']),'CP'])
prp(prunefit)

summary(prunefit)

#load the caret package
library(caret)

# Make predictions on the validation set
predictions <- predict(prunefit, newdata = test, type = "class")

# Create a confusion matrix
conf_matrix <- confusionMatrix(predictions, test$Churn)
conf_matrix

# Extract accuracy and other metrics from the confusion matrix
accuracy <- conf_matrix$overall["Accuracy"]
precision <- conf_matrix$byClass["Precision"]
recall <- conf_matrix$byClass["Recall"]
f1_score <- conf_matrix$byClass["F1"]

# Display the metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")

# Install and load the pROC package
install.packages("pROC")
library(pROC)

# Create a ROC curve
roc_curve <- roc(as.numeric(predictions == "1"), as.numeric(test$Churn == "1"))

# Display the AUC
cat("AUC:", auc(roc_curve), "\n")

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)

auc_value <- auc(roc_curve)
text(0.8, 0.2, paste("AUC =", round(auc_value, 2)), col = "blue", cex = 1.2)





#Neural Network
library(neuralnet)

norm.values <- preProcess(training, method="range")
train.norm.df <- predict(norm.values, training)
test.norm.df <- predict(norm.values, test)

# Create the neural network model

nn <- neuralnet(
  Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure + PhoneService + MultipleLines + 
    InternetService_DSL + InternetService_FiberOptic + OnlineSecurity + OnlineBackup + 
    DeviceProtection + TechSupport + StreamingTV + StreamingMovies + Contract_Month + 
    Contract_1Year + PaperlessBilling + PaymentMethod_BankTransfer + PaymentMethod_CreditCard + 
    PaymentMethod_ECheck + MonthlyCharges + TotalCharges,
  data = train.norm.df, 
  linear.output = FALSE,  # Use nonlinear activation for binary classification
  hidden = 3  # Number of neurons in each hidden layer
)


str(train.norm.df)

# Plot the neural network
plot(nn)

# Make predictions on the test set
predictions <- predict(nn, newdata = test.norm.df)

# Convert predictions to binary (1 or 0)
predicted_labels <- ifelse(predictions > 0.5, 1, 0)

# Evaluate the model
confusion_matrix <- table(predicted_labels, test$Churn)
print(confusion_matrix)

# Optionally, plot the ROC curve
library(pROC)
roc_curve <- roc(test$Churn, as.numeric(predictions))
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)





library(adabag)
library(rpart)
library(caret)

#Bagging
bag <- bagging(Churn ~ ., data = training)
pred <- predict(bag, test, type = "class")
confusionMatrix(as.factor(pred$class), test$Churn)

#Boosting
boost <- boosting(Churn ~ ., data = training)
pred <- predict(boost, test, type = "class")
confusionMatrix(as.factor(pred$class), test$Churn)
