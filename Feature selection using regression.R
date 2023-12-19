# The dataset used in this project is the medical cost dataset from kaggle.
# below is the link
# https://www.kaggle.com/datasets/nanditapore/medical-cost-dataset
# We would be using feature selection from regression to get important features

canpath_data <- read.csv("C:\\Users\\olufu\\Downloads\\kaggledata\\medical_cost.csv")
num_observations <- dim(canpath_data) # Checks the rows and columns in the dataset
# There are 1338 rows and 8 columns in the dataset

# As a good data analytics strategy, we are first going to perform vizualizations of the varaibles
#scatter plot of BMI vs Hip and Waist average values

library(ggplot2)
# scattered plot of BMI(continous variables) vs charges(continous variable)

ggplot(canpath_data, aes(x = bmi, y = charges)) +
  geom_point(color = "blue") +
  labs(x = "BMI", y = "Charges", title = "Scatter Plot of BMI vs Charges") +
  theme_minimal()

# Stacked bar plot of age by sex
canpath_data$age_group <- cut(canpath_data$age, breaks = seq(18, max(canpath_data$age) + 4, by = 4), include.lowest = TRUE)
ggplot(canpath_data, aes(x = age_group, fill = sex)) +
  geom_bar(position = "stack", color = "white") +
  labs(x = "Age Group", y = "Count", title = "Distribution of Age by Sex") +
  scale_fill_manual(values = c("male" = "blue", "female" = "pink")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Box plot of BMI vs standing Height and weight
ggplot(canpath_data, aes(x = age, y = age, fill = smoker)) +
  geom_boxplot() +
  labs(x = "Age", y = "Age", title = "Distribution of Age by Sex (Box Plot)")


# Box plot for BMI by education level
ggplot(canpath_data, aes(x = factor(region, labels = c(0, 1, 2, 3)), y = bmi, fill = factor(region))) +
  geom_boxplot() +
  labs(x = "Region", y = "BMI", title = "Distribution of BMI by region") +
  scale_fill_discrete(name = "Region", labels = c("0" = "southwest", "1" = "southeast", "2" = "northwest", "3" =  "northeast"))


# forward stepwise regression
# Checking for missing values
canpath_data <- read.csv("C:\\Users\\olufu\\Downloads\\kaggledata\\medical_cost.csv")

missing_values <- colSums(is.na(canpath_data))
print(missing_values) # there are no missing values in dataset


# Assuming your ID column is named "ID"
canpath_data <- canpath_data[, !(names(canpath_data) %in% c("Id"))]
canpath_data <- canpath_data[, !(names(canpath_data) %in% c("age_group"))]

#convert the categorical variables to as factor
canpath_data$smoker <- as.factor(canpath_data$smoker)
canpath_data$region <- as.factor(canpath_data$region)
canpath_data$sex <- as.factor(canpath_data$sex)
X <- subset(canpath_data, select = -charges)
y <- canpath_data['charges']
# Set seed for reproducibility
set.seed(123)
library(caret)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)

# Train the model
fwd.model <- train(charges ~ ., 
                   data=canpath_data,method = "leapForward", 
                   tuneGrid = data.frame(nvmax = 1:8),
                   trControl = train.control
)

fwd.model$results
#displays the best tunning values
fwd.model$bestTune   # for model the best tunes is 6, it means that best model performances occured when number of variables is 6
#The function summary() reports the best set of variables for each model size
summary(fwd.model$finalModel)
#The regression coefficients of the final model can be access below
coef(fwd.model$finalModel, 6)  # The variables at the optimal model performance are age, BMI, children, smokers(yes), region()

#Backward stepwise regression
# Set seed for reproducibility
set.seed(123)
# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)

# Train the model
back.model <- train(charges ~  age + sex +  bmi + 
                      children + smoker + region,
                    data=canpath_data,method = "leapBackward", 
                    tuneGrid = data.frame(nvmax = 1:8),
                    trControl = train.control
)

back.model$results

#displays the best tunning values
back.model$bestTune

#The function summary() reports the best set of variables for each model size
summary(back.model$finalModel)
#The regression coefficients of the final model can be access below
coef(back.model$finalModel, 6)


