# Mammography is the most effective method for breast cancer screening. 
# However, the low positive predictive value of breast biopsy resulting 
# from mammogram interpretation leads to approximately 70% unnecessary 
# biopsies with benign outcomes. To reduce the high number of unnecessary
# breast biopsies, several computer-aided diagnosis (CAD) systems have been 
# proposed in the last years. These systems help physicians in their decision 
# to perform a breast biopsy on a suspicious lesion seen in a mammogram or to 
# perform a short term follow-up examination instead.

# This data set can be used to predict the severity (benign or malignant) of a 
# mammographic mass lesion from BI-RADS attributes. It contains a BI-RADS
# assessment, the patient's age shape, density, margin and severrity all
# collected at the Institute of Radiology of the University Erlangen-Nuremberg between 2003 and 2006.

#This first step install and loads required packages.


# Attribute Information:
  #1. BI-RADS assessment: 1 to 5 (ordinal)  
  #2. Age: patient's age in years (integer)
  #3. Shape: mass shape: round=1 oval=2 lobular=3 irregular=4 (nominal)
  #4. Margin: mass margin: circumscribed=1 microlobulated=2 obscured=3 ill-defined=4 spiculated=5 (nominal)
  #5. Density: mass density high=1 iso=2 low=3 fat-containing=4 (ordinal)
  #6. Severity: benign=0 or malignant=1 (binominal)

#Read in the data by downloading it from the UCI  web site.


if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")
if(!require(gmodels)) install.packages("gmodels", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(rmarkdown)) install.packages("rmarkdown", repos = "http://cran.us.r-project.org")
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(purrr)
library(gmodels)
library(knitr)
library(rmarkdown)
library(tinytex)
tinytex::install_tinytex()


#     Read in the data and tidying the data


#url <- "https://archive.ics.uci.edu/ml/machine-
#learning-databases/mammographic-masses/mammographic_masses.data"

#destfile <- "~/R/mammographic_masses.data"

#download.file(url, destfile = destfile)


mass <- data.frame(read_csv("~/R/datasets/mammographic_masses.data",
                 col_names = c("bi_rads","age", "shape", "margin", "density", "severity"), 
                 col_types = cols(bi_rads = col_number(),
                                   age = col_number(),
                                   shape = col_number(),
                                   margin = col_number(),
                                   density = col_number(),
                                   severity = col_factor()
                 )
                 ))

str(mass)

#         split 15% of the data for validation purpose.

set.seed(123, sample.kind = "Rounding")
valid_index <- createDataPartition(mass$severity, times = 1, p = 0.15, list = FALSE)
mass <- mass [-valid_index, ]
validation_set <- mass [valid_index, ]

str(mass)
str(validation_set)


#   Check the data for presence and number of missing values NA in each variable

colSums(is.na(mass))
table(is.na(mass))

#   There is a total of 134 missing values in the dataset. Because the data size is small,
#   removing the NAs will do no good to the model. Althought the model is not included in this script,
#   i have previously tried building a model that ignors all missing values but the model predicted
#   badly giving an accuracy of 50% and all predictions made were benign. I will therefore
#   convert all missing values to the mean of each column to see how well the model
#   will predict. 

#   Replace NAs with the mean of its column.

mn_age <- round(mean(mass$age, na.rm = TRUE))

mass <- mass %>% mutate(density = ifelse(is.na(density) %in% density, 
                                        round(mean(density, na.rm = TRUE)),
                                        density),
                       margin = ifelse(is.na(margin) %in% margin, 
                                       round(mean(margin, na.rm = TRUE)),
                                       margin),
               
                       age = replace_na(age, replace = mn_age),
                       shape = ifelse(is.na(shape) %in% shape, 
                                      round(mean(shape, na.rm = TRUE)),
                                      shape),
                       severity = factor(ifelse(severity == 0, 
                                               "benign", "malignant")))


colSums(is.na(mass))
table(is.na(mass))


#   Since there are only two values of severity (i.e 0 and 1) in the dataset
#   and this is the variable i will be predicting,
#   i therefore converted the o = benign (i.e non fatal mass)
#       and 1 = malignant (i.e deadly mass)

#   Create a age group column to identify the variability of mass in the class of age

mass <- mass %>% mutate(age_group = cut(mass$age, breaks = seq(0,100, 5)))
mass$age_group <-  str_replace(mass$age_group, ",", "-")
mass$age_group <-  factor(str_remove_all(mass$age_group, "[\\(,\\]$]"))

str(mass)
head(mass)

#     **********       Visualization and exploration of the data       **************

summary(mass)

#     Proportion (%) of benign and malignant mass in the data

round(prop.table(table(mass$severity)) * 100, digits = 2)

#     count and Visual distribution of severity in the data

table(mass$severity)
mass %>% ggplot(aes(severity,fill = severity)) +
  geom_bar(width = 0.5)

#     The distribution of age follows a normal curve

histogram(mass$age, fill = "aquamarine4", xlab = "Age")
histogram(mass$age_group, fill = "aquamarine4", xlab = "Age Group")


#     Visualize the relationship between age and severity,
#     There is no correlation or linear relationship
#     between age and severity of the mass. This means that 
#     mass can occur at any age even if it may not be fatal, 
#     although the malignant mass shifts to the older age.

mass %>%  
  ggplot(aes(factor(age),severity, color = severity))+ 
  geom_point(size = 1.75, position = "jitter")+
  xlab("Age")+
  ylab("Severity")

mass %>%  
  ggplot(aes(age_group,severity, color = severity))+ 
  geom_point(size = 1.75, position = "jitter")+
  xlab("Age Group")+
  ylab("Severity")

#   Plot of relationship between age count and severity shows a non linear relationship
#   Skewness to the right for malignant masses and to the left for benign masses.


mass %>% group_by(age,severity) %>% summarise(n = n()) %>% 
  ggplot(aes(factor(age),n, color = severity))+ 
  geom_point(size = 1.75, position = "jitter")+
  xlab("Age")+
  ylab("Age Count")

mass %>% group_by(age_group,severity) %>% summarise(n = n()) %>% 
  ggplot(aes(age_group,n, fill = severity))+ 
  geom_col(color = "grey")+
  xlab("Age")+
  ylab("Age Count")

#     The shape of the mass is grouped into 4 distinct densities
#     that are not linear and mostly clogged around low density represented by 3
#     with their severity interwoven but mostly malignant when it is 
#     Fat-Containing(4) and irregular (4) in shape.

mass %>% 
  ggplot(aes(factor(shape),factor(density), color = severity))+ 
  geom_point(size = 1.75,
             position = "jitter")+
  xlab("Shape")+
  ylab("Density")

#     The benign and malignant masses appear to be some what separated and can be 
#     slightly distinctinguished in the shape and bi_rads visualization
#     this two variables can be used as good predictor.

mass %>% 
  ggplot(aes(factor(shape),factor(bi_rads), color = severity))+ 
  geom_point(size = 1.75,
             position = "jitter")+
  xlab("Shape")+
  ylab("Bi-Rads")

#     There is no relationship between the bi_rad and density variable, it
#     however shows two distinct group of masses and a little mix of both severity.

mass %>% 
  ggplot(aes(factor(bi_rads),factor(density), color = severity))+ 
  geom_point(size = 1.75,
             position = "jitter")+
  xlab("Bi-Rads")+
  ylab("Density")

#   majority of the round (1) and oval (2) shaped masses with 
#   circumscribed and microlobulated margins seem harmless
#   compared to lobular (3) and irregular (4) shaped masses

mass %>% 
  ggplot(aes(factor(shape),factor(margin), color = severity))+ 
  geom_point(size = 1.75,
             position = "jitter")+
  xlab("Shape")+
  ylab("Margin")


#     **************************    BUILD THE MODEL   **************************************


#   I will be using the KNN algorithm to predict severity of mass in the
#   the mammographic data. The data is a categorical data of different levels and K-nn been a 
#   classification algorithm that generalises on categorical data by identifying labels will do well
#   in making predictions. 

#   The variables are of different scales e.g range of age is different from range of bi_rads.
#   Because KNN uses distance to correctly make predictions,
#   i would first normalise the numeric variables by substracting the min value from the max value
#   in the function below.


#     Write a function for the normalization that works on numerical values only


normalize <- function(x) {
  x
  (x - min(x)) / (max(x) - min(x))
}

#     Normalize the numeric variables

mass_norm <- normalize(mass[1:5])

#     Add the missing target variable from mass dataset

mass_norm <- data.frame(mass_norm, mass[6])
head(mass_norm)
summary(mass_norm)


#     *****************       Split the data into train and test sets 
#     *****************       to 80% and 20% respectively because the data set is not very large.

set.seed(1, sample.kind = "Rounding")
index <- createDataPartition(mass_norm$severity, times = 1, p = 0.2, list = FALSE)
train_set <- mass_norm [-index, -6]
test_set <- mass_norm [ index, -6]


#     Create the labels for the train and test sets

train_set_label <- mass_norm [-index , 6]
test_set_label <- mass_norm [index , 6]


#       ***********         Train the model         ********************

#   Standard practise for choosing K is using the square root value of number of objects
#   in the datasets. In this case square root of 652 but i choose 21 instead of 26 to avoid
#   over training using big values of k.

fit <- knn3(train_set_label ~ .,
                data = train_set, k = 21)

y_hat_knn <- predict(fit, train_set, type = "class")

#   The train set has an accuracy of 78%

CrossTable(x = train_set_label, y = y_hat_knn)
confusionMatrix(y_hat_knn, train_set_label)$overall["Accuracy"] 




#       **************      Evaluate the model        *****************



#   Confirm if i overfitted the model by checking the accuracy of the train set against
#   the accuracy of the test set.

#     Build the prediction model

y_hat_fit <- predict(fit, test_set, type = "class")

CrossTable(x = test_set_label, y = y_hat_fit)
confusionMatrix(test_set_label, y_hat_fit)$overall["Accuracy"] 

#   The model predicted about 81% correctly on the test set,
#   meaning there is no over fitting and it appear to be better than the
#   first model that predicted 50% and all values predicted were benign.
#   At 81% the model predicted 14 false positive and 17 false negative.
#   Both error can be very costly.To reduce the error, i will try a range
#   of values for k with the highest accuracy and standardizing the values instead of nomalizing.



#     *************          Improving the Model's performance           *******************



#   I will improved the model's performace by standarding the numeric variables
#   Unlike normalization, standardization takes into consideration
#   outliers in datasets.


#     Standardize the numeric variables

mass_z <- scale(mass[1:5])

#     Add missing target variable from mass data set

mass_z <- data.frame(mass_z, mass[6])
head(mass_z)
summary(mass_z)


#     *****************       Split the data into train and test sets 
#     *****************       at 80% and 20% respectively without the target variables


set.seed(46, sample.kind = "Rounding")
index <- createDataPartition(mass_z$severity, times = 1, p = 0.2, list = FALSE)
train_set <- mass_z [-index, -6]
test_set <- mass_z [ index, -6]

#     Create the labels for the train and test sets
train_set_label <- mass_z [-index , 6]
test_set_label <- mass_z [index , 6]


#       ***********         Train the model         ********************

fit <- knn3(train_set_label ~ .,
            data = train_set, k = 21)

y_hat_knn <- predict(fit, train_set, type = "class")
CrossTable(x = train_set_label, y = y_hat_knn)
confusionMatrix(y_hat_knn, train_set_label)$overall["Accuracy"] 


#     Build the prediction model on the test set


y_hat_fit <- predict(fit, test_set, type = "class")

CrossTable(x = test_set_label, y = y_hat_fit)
confusionMatrix(test_set_label, y_hat_fit)$overall["Accuracy"] 



#   The model predicted 78% (95 FN and 48 FP) for the train set and 81% (17 False 
#   Negatives and 14 False Positives) for the test set.

#   I will check for k that gives the highest accuracy and confirm how much the sensitivity
#   and specificity is adjusted.

k <- seq(3, 100,2)

accuracy <- map_df(k, function(k){ 
  fit_k <- knn3(train_set_label ~ ., data = train_set, k = k)
  y_hat <- predict(fit_k, train_set, type = "class")
  cm_train <- confusionMatrix(y_hat, train_set_label)
  train_error <- cm_train$overall["Accuracy"]
  
  y_hat <- predict(fit_k, test_set, type = "class")
  cm_test <- confusionMatrix(y_hat, test_set_label)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(train = train_error, test = test_error) 
})


qplot(k, accuracy$test)
max(accuracy$test)
k_acc <- k[which.max(accuracy$test)]
k_acc


# improve the model using the best k

set.seed(1, sample.kind = "Rounding")

fit <- knn3(train_set_label ~ .,
            data = train_set, k = k_acc)

y_hat_fit <- predict(fit, test_set, type = "class")
CrossTable(x = test_set_label, y = y_hat_fit)
confusionMatrix(test_set_label, y_hat_fit)$overall["Accuracy"] 


# The model was improved from 82% to 87% with the FP and FN reduced to 12 and 10 respectively

