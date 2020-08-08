# Load the Census income dataset.

# First have set Working Dictionary 

censusdata <- read.csv("census-income_ (1) (4) (1) (1) (2) (1) (1) (2) (1).csv" , stringsAsFactors = T)
View(censusdata)

str(censusdata)



#.......................Data Processing.........................................



#Replace all the missing values with NA

# First thing we need to convert factor dataset into character data set 

censusdata$workclass <- as.character(censusdata$workclass)
censusdata$education <- as.character(censusdata$education)
censusdata$marital.status <- as.character(censusdata$marital.status)
censusdata$occupation <- as.character(censusdata$occupation)
censusdata$relationship <- as.character(censusdata$relationship)
censusdata$race <- as.character(censusdata$race)
censusdata$sex <- as.character(censusdata$sex)
censusdata$native.country <- as.character(censusdata$native.country)
censusdata$X <- as.character(censusdata$X)

str(censusdata)


table(is.na(censusdata))

censusdata[censusdata== " ?"] <- NA

View(censusdata)

# b) Remove all the rows that contain NA.

table(is.na(censusdata))

censusdata <- na.omit(censusdata)

# c) Remove all the whitespace from all coulumn
# Install Package ("stringr)
# Install Package ("dplyr")

library(stringr)
library(dplyr)

censusdata <- censusdata %>% mutate_if(is.character, str_trim)

View(censusdata)
str(censusdata)



#........................Data Manipulation.....................................



# Your task is to perform data manipulation to analyze data set using various functions from dplyr package.
# a) Extract the Education Column and store it in "census_edu"

select(censusdata, education) -> census_edu
View(census_edu)

# b) Extract all the column from "age" to  "relationship" and store it in "census_seq"

select(censusdata, age:relationship) -> census_seq
View(census_seq)

# c) Extract the column no "5", "8", "11", and store it in "census_col".

census_col <- censusdata[, c(5,8,11)]
View(census_col)

#or

select(censusdata,c(5,8,11)) -> Census_col1
View(Census_col1)

# d) Extract all the male employee who work in state-gov, and save in "male_gov"

censusdata %>% filter(workclass=="State-gov" & sex=="Male") -> male_gov

View(male_gov)

# e) Extract all the 39 years old who either have a bachelor degree or who are
# native of united state and store the result in "census_us".

censusdata %>% filter(age==39 & (education=="Bachelors" | native.country=="United-States")) -> census_us

View(census_us)

# f) Extract 200 random rows from "census" dataset and store it in "census_200"

sample_n(censusdata,200) -> census_200
View(census_200)

# g) Get the count of different levels of the "workclass" column

table(censusdata$workclass)

#h) Calculate mean of "capital.gain" column grouped according to workclass

View(censusdata[c('capital.gain', 'workclass')])

#or

tapply(censusdata$capital.gain, censusdata$workclass, mean)

#or

censusdata %>% group_by(workclass) %>% summarise(mean(capital.gain))
  
 


#........................Data Visualization.....................................



# a) Build a bar plot for "relationship" column and fill the bars according to the 
#  "race" coulmn,

library(ggplot2)

ggplot(censusdata,aes(x=relationship, fill=race))+ geom_bar()

# i) set X axis label to 'categories of relationship' 
# ii) set y-axis label to 'count of categories'

ggplot(censusdata,aes(x=relationship, fill=race))+ geom_bar()+
  labs(x= "Categories of relationship", y = "Count of categories")

# iii) Fill the bars according to sex

ggplot(censusdata,aes(x=relationship, fill=sex))+ geom_bar()+
  labs(x= "Categories of relationship", y = "Count of categories")

# iv) Set the position of bar for "Dodge"

ggplot(censusdata,aes(x=relationship, fill=sex))+ geom_bar(position= "dodge")+
  labs(x= "Categories of relationship", y = "Count of categories")


# b) Build a histogram for age column with number equal to 50.

ggplot(censusdata, aes(x=age)) + geom_histogram(bins = 100)

# i) Fill the bars of Histogram according to yearly income coulunm i.e X

ggplot(censusdata,aes(x=age,fill=X))+geom_histogram(bins =90)

# ii) Set the title of plot to "Distribution of age"

ggplot(censusdata,aes(x=age,fill=X))+geom_histogram(bins =90)+ labs(title = "Distribution of Age")

# iii) Set the legend title to "yearly Income"

ggplot(censusdata,aes(x=age,fill=X))+geom_histogram(bins =90)+ labs(title = "Distribution of Age",
                                                                    fill= "Yearly Income")

# iv) Set the theme of plot black and white.

ggplot(censusdata,aes(x=age,fill=X))+geom_histogram(bins =90)+ labs(title = "Distribution of Age")+
  theme_dark()


# c)  Build a scatter-plot between "capital.gain" and "hour.per.week". Map "capital.gain" on the 
#     X Axis and "hour.per.week" on the y- axis.

ggplot(censusdata,aes(x=capital.gain, y= hours.per.week))+geom_point()

# i) set the transparency of plot is 40% and size 2

ggplot(censusdata,aes(x=capital.gain, y= hours.per.week))+
  geom_point(alpha=0.6,size=2)

# ii) Set the color of points according to the 'x' (yearly income) column

ggplot(censusdata,aes(x=capital.gain, y= hours.per.week, col= X))+
  geom_point(alpha=0.6,size=2)

# iii) Set the x-axis label to 'capita.gain', y-axis label to 'Hour.per.Week' title to
#     'capital Gain Vs Hours per Week by Income' and legend to 'yearly income'

ggplot(censusdata,aes(x=capital.gain, y= hours.per.week,col=X))+
  geom_point(alpha=0.6,size=2)+
  labs(x="capital gain",y="Hours per week",title = "Capital Gain VS Hours per Week", Fill="Yearly Income")

# d) Build a box plot between 'education' and 'age' column. Map 'education' on X-axis and
#    'age' on y-Axis.

ggplot(censusdata, aes(x= education, y = age)) + geom_boxplot()
 #   i) Fill the box plot according to 'sex' column.
ggplot(censusdata, aes(x= education, y= age, fill= sex)) + geom_boxplot()
#   ii) Set the title of box plot of Age by Education and sex
ggplot(censusdata, aes(x=education,y=age,fill=sex))+geom_boxplot()+labs(title = "Age by Education and Sex")




#..............................Linear Regression.................................................




#a) Build Simple Regression Model as Follow.
#  i) Divide the dataset training and test in 70:30 ratio

library(caTools)
split_data <- sample.split(censusdata$hours.per.week, SplitRatio = 0.70)
View(split_data)
censusTrain <- subset(censusdata, split_data == T)
censusTest <- subset(censusdata, split_data == F)
View(censusTrain)
View(censusTest)
nrow(censusTest)
nrow(censusTrain)

# ii) Build Linear model on test set where depended variable is "hour.per.week" and
#     independent variable is "education.num". 

View(censusdata [c("hours.per.week" , "education.num")])

LR_model <- lm(hours.per.week~education.num, data = censusTrain)
summary(LR_model)

#  iii) predict the values on the train set and find error in prediction
# iv) Find the root mean square error (RMSE)

censusP <- predict(LR_model, newdata=censusTest)
head(censusP)
View(censusP)

Error<- censusTest$hours.per.week - censusP

sqrt(mean(Error)^2)



#...........................Logistic Regression.............................................



# a) Build simple Logistic regression model as follow.

# i) Divide the dataset into training and test sets in 65:35 ratio
split_data1 <- sample.split(censusdata$X, SplitRatio = 0.65)
censusTrain1 <- subset(censusdata, split_data == T)
censusTest1 <- subset(censusdata, split_data == F)
nrow(censusTest1)
nrow(censusTrain1)
class(censusdata$X)

#ii) Build a logistic regression model where the dependent variable is "X" (Yearly Income)
#    and independent variable is "occupation"

log_mod <- glm(X~occupation, data = censusTrain1, family = "binomial")

#iii) predict the values on test dataset
pred_val <- predict(log_mod, newdata = censusTest1, type = "response")
head(pred_val)
range(pred_val)

#Install a pacakage ROCR to decide a accuracy
library(ROCR)

pred_val_ROC <- prediction(pred_val, censusTest1$X)
pred_val_ROC
acc<- performance(pred_val_ROC, "acc")
plot(acc)  # TO check at which point accuracy got constant

# iv) Plot accuracy vs Cut-off and pick and ideal value for cut-off
lm_pred <- ifelse(pred_val>0.47, ">50k", "<=50k")
lm_pred

# v) Build the confusion matrix and find accuracy.
tab2 <- table(lm_pred, censusTest1$X)
tab2

# Finding the accuracy
sum(diag(tab2)/sum(tab2))

#   vi) plot the ROC curve and find the auc (Area under Curve)

roc <- performance(pred_val_ROC, "tpr", "fpr") #("tpr"= True Positive Rate. "fpr' = False positive rate)
plot(roc)
performance(pred_val_ROC, "auc") -> auc
auc
 
auc<- auc@y.values[[1]]
auc





#...........................Decision Tree.............................................




# a) Build a random forest model as folow.

# i) Divide the dataset into training and test sets in 70:30 ratio.
# ii) Build the decision tree model where the dependent variable is "X" (yearly Income)
#    and independent vairable are "age" , "workclass" and "education"
#   iii) Predict the values on the test set.
#   iv) plot accuracy vs cut off and pick an ideal value for cut-off
#    v) Build the confusion matrix and find the accuracy

library(rpart)
library(rpart.plot)

census_model <- rpart(formula = X~., data = censusTrain, method = "class")

rpart.plot(x=census_model, type = 5, extra = 0, tweak = 1.5)

class_predication <- predict(census_model, newdata = censusTest, type = "class")

class_predication

tab <- table(class_predication, censusTest$X)
tab

sum(diag(tab)/sum(tab))




#...............................Random Forest.......................................

# a) Build a Random Forest model as follow.

#   i) Divide the dataset into training and test sets in 80:20 ration.
#   ii) Build a random forest model where the dependent varaible is "X" (Yearly Income)
#     and the rest of the variable as independent vairaible and number of trees as 300

as.factor(censusdata$X) -> censusdata$X

split_data <- sample.split(censusdata$X, SplitRatio = 0.80)
View(split_data)
censusTrain <- subset(censusdata, split_data == T)
censusTest <- subset(censusdata, split_data == F)
nrow(censusTest)
nrow(censusTrain)

library(randomForest)

census_model1 <- randomForest(formula= X~., data = censusTrain, ntree=300)

#  iii) Predict the values on test set
census_prediction <- predict(census_model1, newdata = censusTest, type = "class")

#   iv) Build the confusion matrix and calculate the accuracy
tab <- table(census_prediction, censusTest$X)
tab
sum(diag(tab)/sum(tab))
