
#IMPORTING DATASETS
test <- read.csv("C:/Users/MD/Desktop/WebDev/R/Mini Projects/Credit Default/cs-test.csv")
training <- read.csv("C:/Users/MD/Desktop/WebDev/R/Mini Projects/Credit Default/cs-training.csv")
view("test")


#CLEANING DATASETS
#Removing a column in the test dataset
test$X <- NULL

#Checking the number of missing values - 33655 missing values in dataset
sum(is.na(training))

#How to determine the number of rows in dataset - 150000 number of rows in dataset
nrow(training)

#Number of columns in the dataset - 11 columns
ncol(training)

#Number of values in each column two steps:
  #1. Define a vector
      b <- c()
      
  #2. "for loop"
      for (i in 1:ncol(training)){
        
      a <- sum(is.na(training[[i]]))
      b <- rbind(b,a)
      
      }
      

#"b" holds the vector that includes the missing values for each column - monthly income has 29k missing, number of dependents has almost 4l=k missing values
b

#Running a smmary function helps determines if the average in the coloumn are skewed - mean is skewed because of a large range
#Use the median as the replacement variable
summary(training$MonthlyIncome)

#How to replace a variable:
training$MonthlyIncome[is.na(training$MonthlyIncome)] <- median(training$MonthlyIncome, na.rm = TRUE)

#Check to see if variables are replaced
head(training$MonthlyIncome,100)

#check the mode of each occurance - this is the second column with missing variables being fixed
#Because "0" occurs most of the time it would be the number used to replace missing variables in the "numberof dependents"
table(training$NumberOfDependents)

#how to replace a value without using a function:
#(the statement below will replace the missing valuse in the "numberofdependent" column)
training$NumberOfDependents[is.na(training$NumberOfDependents)] <- 0

#determine outliars - For this project I will use the quantile percentage method
#99% the average is closes to the center without removing or changing the dataset too much
quantile(training$MonthlyIncome,c(0,.1,.5,.75,.90,.99,.999,1))

#replacing the outliars with the 99% figure - 23000
training$MonthlyIncome[training$MonthlyIncome > 23000] <- 23000

#A more formal and strucutred way to ensure to capture the right values the above expression can be written below:
#training$MonthlyIncome[training$MonthlyIncome > quantile(training$MonthlyIncome), c(.99)] <- quantile(training$MonthlyIncome, C(.99))

#converting seriouseDeliquneces to a factor variable
training$SeriousDlqin2yrs <- as.factor(training$SeriousDlqin2yrs)

#BUILDING THE REGRESSION MODEL - glm()
#a way to store dep and indep variables
dep_var <- "SeriousDlqin2yrs"
ind_var <- names(training[,3:12])
#create formula variable
formula <- paste("dep_var ~", paste(ind_var, collapse = "+"))
log_object <- glm(as.formula(formula),data = training,family = "binomial")
 

 
