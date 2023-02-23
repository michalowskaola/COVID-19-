setwd("/Users/majaorlowska/Desktop/zajęcia z anną/project")
covid_data <- read.csv("covid_data.csv")

install.packages("dplyr")
library(dplyr)
glimpse(covid_data)

install.packages("ggplot2")
library(ggplot2)

#The purpose of our analysis is to see if 
#age, gender, diseases such as asthma and diabetes, had an impact on whether a covid patient survived 

str(covid_data)

covid_data
summary(covid_data)
colnames(covid_data)

df <- data.frame(covid_data)
df


df <- subset(df, !(PNEUMONIA %in% c(97,99)))
df <- subset(df, !(DIABETES %in% c(97,99)))
df <- subset(df, !(ASTHMA %in% c(97,99)))
df


4.
dim(df)
str(df)
glimpse(df)
install.packages("tidyverse")
library(tidyverse)

install.packages("unikn")
library(unikn) 

head(df)

summary(df)

dim(df)

names(df) <- tolower(names(df))

colnames(df)

covid_data_1 <- df[, -c(1,2,6,9,11,13,14,15,16,17,18,19,20,21)]

df1 <- data_frame(covid_data_1)

df1$date_died <- as.character(df1$date_died)
death <- as.numeric(df1$date_died == "9999-99-99") + 1

head(death)

colnames(covid_data_1)

summary(covid_data_1)

df1 <- drop_na(covid_data_1)
sum(is.na(df1))


sapply(df1, function(x) length(unique(x)))

df1$elderly <- ifelse(df1$age < 65, 2, 1)

table(df1$elderly)

summary(df1)
colnames(df1)



#ML 
install.packages("rpart")
library(rpart)
install.packages("caTools")

df1 <- df1 [, -c(10)]
df1

colnames(df1)
colnames(df)

features <- c( "sex", "patient_type", "pneumonia", "age", "diabetes", "asthma", "elderly") 


# create a dataframe with the specified columns
df_model <- df1[, c("sex", "patient_type", "pneumonia", "age", "diabetes", "asthma", "elderly")]


df1 

death

df1 <- df1 %>% mutate(death)
df1



# split the data into training and testing sets
library(caTools)
set.seed(123)
train_data <- df_model[sample(nrow(df_model), 0.7*nrow(df_model)),]
test_data <- subset(df_model, !(row.names(df_model) %in% row.names(train_data)))

colnames(df1)
# train the decision tree model

train_data$sex <- as.factor(train_data$sex)
train_data$patient_type <- as.factor(train_data$patient_type)


levels(train_data$sex)


colnames(df1)

df1$sex <- replace(df1$sex, df1$sex == 2, 0)

df1$pneumonia <- replace(df1$pneumonia, df1$sex == 2, 0)

df1$patient_type <- replace(df1$patient_type, df1$patient_type == 2, 0)

df1$diabetes <- replace(df1$diabetes, df1$diabetes == 2, 0)

df1$asthma <- replace(df1$asthma, df1$asthma == 2, 0)

df1$elderly <- replace(df1$elderly, df1$elderly == 2, 0)

df1$death <- replace(df1$death, df1$death == 2, 0)



# Load the necessary library
install.packages("MASS")
library(MASS)

covid_data <- rename(covid_data, sex = column_name)

covid_data



# Fit a binomial GLM using the glm() function
model <- glm(death ~ sex + patient_type + pneumonia + age + diabetes + asthma + elderly,
             family = binomial(link = "logit"), data = df1)

# Print the summary of the model
summary(model)



# make predictions on the test data
predictions <- predict(model, type = "response")
predictions



actual <- df1$response
actual <- df1$death
actual


predicted <- ifelse(predictions > 0.5, 1, 0)
predicted


#Check missing values in vectors
any(is.na(predicted))
any(is.na(actual))


#Remove missing values 
predicted <- na.omit(predicted)
actual <- na.omit(actual)


confusion_matrix <- table(predicted, actual)
confusion_matrix

set.seed(1)
train_index <- sample(1:nrow(df1), 0.8*nrow(df1))
train_data <- df1[train_index,]
test_data <- df1[-train_index,]

colnames(train_data)



train_data$response <- train_data$death
test_data$response <- test_data$death

# Fit the model on the train data
train_model <- glm(response ~ sex + patient_type + pneumonia + age + diabetes + asthma + elderly, 
                   family = binomial, data = train_data)
train_model

train_predictions <- predict(train_model, newdata = train_data, type = "response")
train_predictions_class <- ifelse(train_predictions > 0.5, 1, 0)

# Get predictions on the test data
test_predictions <- predict(train_model, newdata = test_data, type = "response")
test_predictions_class <- ifelse(test_predictions > 0.5, 1, 0)

# Compare performance on train and test data
train_accuracy <- mean(train_predictions_class == train_data$response)
train_accuracy

test_accuracy <- mean(test_predictions_class == test_data$response)
test_accuracy




#making the plots

library(ggplot2)
for (col in names(df1)) {
  if (!col %in% c("age", "date_died")) {
    p <- ggplot(data = df1, aes(x = df1[[col]])) + 
      geom_bar(stat = "count") +
      ggtitle(col)
    print(p)
  }
}


#data is not normally distributed

# Check linearity
plot(predict(model), residuals(model))

plot(model,1)

# Or plot the observed versus predicted values (again ideally a horizontal line)
plot(simple_model$fitted.values, simple_model$model$BMI)

# Check homoscedasticity
plot(predict(model), rstandard(model))

colnames(df1)
x <- c("sex", "patient_type", "date_died", "pneumonia", "age", "diabetes",    
      "asthma", "elderly" )
y <- c("death", "death", "death", "death", "alive", "alive", "alive", "alive")

table <- table(x, y)
chisq.test(table)

