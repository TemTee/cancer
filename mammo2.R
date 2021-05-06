library(tidyverse)
library(caret)
library(e1071)
library(dplyr)
library(purrr) 
library(gmodels)
library(mice)
library(VIM)
library(psych)

mass <- data.frame(read_csv("~/R/datasets/mammographic_masses.data", na = "?",
                            col_names = c("bi_rads","age", "shape",
                                          "margin", "density", "severity")
                            ))

# There seem to be an input error in the dataset. This is corrected manually
# since the vector is a categorical and should have only 6 levels

table(mass$bi_rads)
plot(factor(mass$bi_rads))
mass$bi_rads <-  ifelse(mass$bi_rads == 55, 5, mass$bi_rads)

str(mass)

#   Extract 10% of the data for validation. This is to allow for final simulation of new data for the model and the mass data is used as train and test set.

set.seed(123, sample.kind = "Rounding")

valid_index <- createDataPartition(mass$severity, times = 1, p = 0.1, list = FALSE)
mass <- mass [-valid_index, ]
validation_set <- mass [valid_index, ]


#############################           Data Cleaning           ######################

# Parse the data to the proper class

mass <- mass %>% mutate(bi_rads = as_factor(bi_rads),
                        shape = as_factor(shape),
                        margin = as_factor(margin),
                        density = as_factor(density),
                        severity = as_factor(severity))

# Since there are only two values of severity (i.e 0 and 1) in the dataset and this is
# the variable i will be predicting, i therefore converted the o = benign (i.e non fatal mass) 
# and 1 = malignant (i.e deadly mass)

mass$severity <- ifelse(mass$severity == 0, "benign", "malignant")
str(mass)
head(mass)

#########################           Exploration And Visualization

# Check the data for presence of missing values, 
# the number of missing values (NA) in each variable,
# visualize the pattern and variation of the missing values

summary(mass)
colSums(is.na(mass))
table(is.na(mass))
md.pattern(mass)

# What is the proportion (%) of missing values is the data set?

prop.table(table(is.na(mass))) * 100
aggr(mass, col = c("aquamarine4", "red"), numbers =T, sortVars=T)


# Replace NAs with predicted values using the mice package.
# Parameters used meth = imputation method. Methods used are
# polyreg for factors with levels >2, logreg for factors with
# levels = 2 and norm for numeric vectors
# predm = predictor matrix
# m = 5 default value of imputed datasets iteration

init <- mice(mass, maxit = 5)
meth <- init$method
predm <- init$predictorMatrix


# Specify methods for the imputation based on the variable type.
# Norm for numeric and polyreg for categorical with greater than 2 levels

meth[c("severity")] <- ""   #Skip the imputation for the severity variable since it contains no missing value
meth[c("bi_rads")] <- "polyreg"
meth[c("age")] <- "norm"
meth[c("shape")] <- "polyreg"
meth[c("margin")] <- "polyreg"
meth[c("density")] <- "polyreg"

# Run the multiple inputation of values into the missing values

set.seed(1989, sample.kind = "Rounding")

impute <- mice(mass, method = meth, 
               predictorMatrix = predm, m= 5)

# Understand the imputed values:
# The matching shapes of both points (blue and red) tells us that the imputed values are 
# credible and cal be used in the dataset.

summary(impute)

xyplot(impute, density ~ bi_rads + shape + margin + age,
          pch = 18, cex = 1)


imputed_mass <- complete(impute)        # Create a new imputed data set

colSums(is.na(imputed_mass))        #  Check for missing values in the new data set

str(imputed_mass)
head(imputed_mass)
summary(imputed_mass)


# Create a age group column to understand the variability of mass within the class of age


imputed_mass <- imputed_mass %>% mutate(age_group = cut(imputed_mass$age, breaks = seq(0,100, 5)))
imputed_mass$age_group <-  str_replace(imputed_mass$age_group, ",", "-")
imputed_mass$age_group <-  factor(str_remove_all(imputed_mass$age_group, "[\\(,\\]$]"))

str(imputed_mass)
head(imputed_mass)


# Visualization and exploration of the data

# Summary and proportion (in %) of benign and malignant mass in the data


summary(imputed_mass)

round(prop.table(table(imputed_mass$severity)) * 100, digits = 2)


# count and Visual distribution of severity in the data


table(imputed_mass$severity)
imputed_mass %>% ggplot(aes(severity,fill = severity)) +
  geom_bar(width = 0.5)

# The distribution curve of age is almost normal. More of the people 
# in the dataset are between 50-65

histogram(imputed_mass$age, fill = "aquamarine4", xlab = "Age")
histogram(imputed_mass$age_group, fill = "aquamarine4", xlab = "Age Group")
densityplot(imputed_mass$age)

# Visualize the relationship between age and severity.

# There is no correlation or linear relationship between age and severity of the imputed_mass. 
# This means that imputed_mass can occur at any age even if it may not be fatal, 
# although the malignant imputed_mass shifts to the older age.


imputed_mass %>%  
  ggplot(aes(age,severity, color = severity))+ 
  geom_point(size = 1.75, position = "jitter")+
  xlab("Age")+
  ylab("Severity")

imputed_mass %>%  
  ggplot(aes(age_group,severity, color = severity))+ 
  geom_point(size = 1.75, position = "jitter")+
  xlab("Age Group")+
  ylab("Severity")

# Plot of relationship between age count and severity shows a non linear relationship
# Skewness to the right for malignant masses and to the left for benign masses.


imputed_mass %>% group_by(age,severity) %>% summarise(n = n()) %>% 
  ggplot(aes(factor(age),n, color = severity))+ 
  geom_point(size = 1.75, position = "jitter")+
  xlab("Age")+
  ylab("Age Count")

imputed_mass %>% group_by(age_group,severity) %>% summarise(n = n()) %>% 
  ggplot(aes(age_group,n, fill = severity))+ 
  geom_col(color = "grey")+
  xlab("Age")+
  ylab("Age Count")


# The shape of the mass is grouped into 4 distinct densities that
# are not linear and mostly clogged around low density represented by 3 
# with their severity interwoven but mostly malignant when it is
# Fat-Containing(4) and irregular (4) in shape.


imputed_mass %>% 
  ggplot(aes(factor(shape),factor(density), color = severity))+ 
  geom_point(size = 1.75,
             position = "jitter")+
  xlab("Shape")+
  ylab("Density")


# The benign and malignant masses appear to be some what
# separated and can be slightly distinguished in the shape and
# bi_rads visualization. This two variables can be used as good predictor.

imputed_mass %>% 
  ggplot(aes(factor(shape),factor(bi_rads), color = severity))+ 
  geom_point(size = 1.75,
             position = "jitter")+
  xlab("Shape")+
  ylab("Bi-Rads")


# There is no relationship between the bi_rad and density variable, 
# it however shows two distinct group of masses and a little mix of both severity that are not linear.

imputed_mass %>% 
  ggplot(aes(factor(bi_rads),factor(density), color = severity))+ 
  geom_point(size = 1.75,
             position = "jitter")+
  xlab("Bi-Rads")+
  ylab("Density")


# Majority of the round (1) and oval (2) shaped masses 
# with circumscribed and microlobulated margins seem harmless 
# compared to masses of lobular (3) and irregular (4) shape.

imputed_mass %>% 
  ggplot(aes(factor(shape),factor(margin), color = severity))+ 
  geom_point(size = 1.75,
             position = "jitter")+
  xlab("Shape")+
  ylab("Margin")

# **************************    BUILD THE MODEL   **************************************

# I will be using the KNN algorithm to predict severity of mass in the mammographic data. 
# The data is a categorical data of different levels and K-nn being a classification algorithm
# that generalises on categorical data by identifying labels will do well in making the predictions.
# The variables are of different scales e.g range of margin is different from range of bi_rads.
# Because KNN uses distance to correctly make predictions, i would first normalise 
# the numeric variables by substracting the min value from the max value using below function.

# Correlation between variables

pairs.panels(imputed_mass)

# Since all variables are slightly correlated, i will use all predictors except age_range variable.

# Write a function for normalization then normalize the variables

normalize <- function(x) {
  
  return ((x - min(x)) / (max(x) - min(x)))
}

imputed_mass_norm <- imputed_mass %>% select(-age)
imputed_mass_norm <- as.data.frame(lapply(imputed_mass_norm[1:4], as.numeric)) # Converts the scalar to numeric
imputed_mass_norm <- normalize(imputed_mass_norm)   # Applys the normalize function


# Add the missing target variable from imputed_mass dataset to the normalized data


imputed_mass_norm <- data.frame(imputed_mass_norm, imputed_mass[6])
head(imputed_mass_norm)
summary(imputed_mass_norm)


# Split the data into train and test sets of 60% and 40% respectively because the data set is not large.

set.seed(1, sample.kind = "Rounding")
index <- createDataPartition(imputed_mass_norm$severity, times = 1, p = 0.4, list = FALSE)
train_set <- imputed_mass_norm [-index, -5]
test_set <- imputed_mass_norm [ index, -5]

# Create the labels for the train and test sets using the severity variable. 
# The labels are benign and malignant.


train_set_label <- imputed_mass_norm [-index , 5]
test_set_label <- imputed_mass_norm [index , 5]
train_set_label <- factor(train_set_label)      #  parse as factor
test_set_label <- factor(test_set_label)        # parse as factor


# Train the model       
# Standard practise for choosing K is using the square root value of 
# number of observation in the datasets but i choose 25 instead.


fit <- knn3(train_set_label ~ .,
            data = train_set, k = 25)

y_hat_knn <- predict(fit, train_set, type = "class")



# Using the train set, the model predicted to an accuracy of 82%.
# 39 False Negative and 54 False Positive

CrossTable(x = train_set_label, y = y_hat_knn)
confusionMatrix(y_hat_knn, train_set_label)$overall["Accuracy"]


# Evaluate the model using the test set

y_hat_fit <- predict(fit, test_set, type = "class")


CrossTable(x = test_set_label, y = y_hat_fit)
confusionMatrix(test_set_label, y_hat_fit)$overall["Accuracy"] 
confusionMatrix(test_set_label, y_hat_fit)

# The model predicted about 84% accuracy on the test set, meaning i did not overfit the train set.
# At 83% the model predicted 34 false positive and 21 false negative.
# Sensitivity = 87%
# Specificity = 80.4%
# Though the model did well at a specificity and sensitivity above 80%, 
# both error can be very costly, i will try a range of values for k to 
# choose the k that produces the highest accuracy and reduce the mismatch in the severity.



# #################       Improving the Model's performance       ########################

# I will improve the model's performace by using Cross Validation.

set.seed(1, sample.kind = "Rounding")

control <- trainControl(method = "cv", number = 25, p = .9) 

train_knn_cv <- train(train_set, train_set_label, method = "knn",
                      trControl = control) 
pred_knn_cv <- predict(train_knn_cv, test_set, type = "raw")
                      tuneGrid = data.frame(k = seq(21, 100, 2))
CrossTable(x = test_set_label, y = pred_knn_cv)
confusionMatrix(pred_knn_cv, test_set_label)$overall["Accuracy"]

# Which k gave the best accuracy.


train_knn_cv$bestTune

ggplot(train_knn_cv)


#The model was improved to 85% with the FP and FN reduced to 22 and 29 respectively.


##################        Using Naive Bayes Algorithm         ############################

# Fit the naive bayes classifier and train the model

set.seed(1, sample.kind = "Rounding")
imputed_mass$severity <- factor(imputed_mass$severity)
index <- createDataPartition(imputed_mass$severity, times = 1, p = 0.4, list = FALSE)
nb_train_set <- imputed_mass [-index, ]
nb_test_set <- imputed_mass [ index, ]


nb_class <- naiveBayes(nb_train_set$severity ~ bi_rads + shape
                       + margin + density, data = nb_train_set)

nb_class

nb_pred <- predict(nb_class, nb_train_set)
confusionMatrix(nb_pred, nb_train_set$severity)$overall["Accuracy"]
confusionMatrix(nb_pred, nb_train_set$severity)
CrossTable(y = nb_pred, x = nb_train_set$severity)

nb_yhat <- predict(nb_class, nb_test_set)
confusionMatrix(nb_yhat, nb_test_set$severity)$overall["Accuracy"]
confusionMatrix(nb_yhat, nb_test_set$severity)
CrossTable(y = nb_yhat, x = nb_test_set$severity)

# Using the bayes algorithm i arrived at a 84% accuracy without overfitting the train set


#################           Improve Model's Performance       #################

set.seed(1, sample.kind = "Rounding")
model <- c("naive_bayes", "knn")
trn_cntrl <- trainControl(method = "cv", number = 15, savePredictions = TRUE)

nb_fit <- train(severity ~ bi_rads + shape
                + margin + density, data = nb_train_set,
                method = "naive_bayes",
                trControl = trn_cntrl,
                tunelength = 5)
nb_fit
yhat <- predict(nb_fit, nb_test_set)
confusionMatrix(yhat, nb_test_set$severity)$overall["Accuracy"]
CrossTable(y = yhat, x = nb_test_set$severity)


# Results

# Knn result

CrossTable(x = test_set_label, y = pred_knn_cv)
confusionMatrix(pred_knn_cv, test_set_label)$overall["Accuracy"]


# Naive Bayes results

confusionMatrix(yhat, nb_test_set$severity)$overall["Accuracy"]
CrossTable(y = yhat, x = nb_test_set$severity)


#        Summary

#The KNN and Bayes algorithm gave its hightest accuracy at 85% and 84% respectively.
#I was able to archive over 80% accuracy as proposed in the project aim. However,
#using the Knn algorithm gave the best accuracy with both specificity and 
#sensitivity above 80%.