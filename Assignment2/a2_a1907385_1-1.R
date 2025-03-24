#Load the required packages
library(tidyverse)
library(inspectdf)
library(caret)
library(moments)
library(tidymodels)
library(modelr)

# Q1. Loading the data
# Here we use our ysn to identify which data set we need to use
ysn = 1907385
x <- ysn %% 4
x

# Read in the data using the correct tidyverse command
div1 <- read_csv("./data/division_1.csv")
# Display the first 10 lines of the data
head(div1,10)

dim(div1)

# Q2. Random permutation of the rows
# Set the seed
set.seed(ysn)
# Shuffle the dataset rows without replacement and name it permuted_data
div2 <- sample_n(div1, nrow(div1), replace = FALSE)
# Display the first 10 rows of the permuted dataset
head(div2, 10)

dim(div2)

# Q3. Adding extra column for row numbers
# Add a 'ROWS' column to the left with the row numbers
div3 <- div2 %>%
  mutate(ROWS = row_number()) %>%
  relocate("ROWS", .before = PID)
# Display the first 10 rows of the dataset with the new 'ROWS' column
head(div3, 10)


# Q4. Data Cleaning
## Q4.1  Negative values removal
# Here we remove the negative values
div3_cleaned <- div3 %>%
  filter(`Bugs REM` >= 0)

# Q4.2 Here we removed the impossible values
# First is the number larger than 99999
div3_cleaned1 <- div3_cleaned %>%
  filter(!(LOC > 99999))
div3_cleaned1

# Q4.3 Correction of typos
# The typo is wrong
div3_cleaned2 <- div3_cleaned1 %>%
  mutate(LOC =  str_replace(LOC, "32 thousand 3 hundred and 7", "32307"))

# Here we found a wrong type in Debug time
div3_cleaned3 <- div3_cleaned2 %>%
  mutate(`Debug Time` = str_replace(`Debug Time`, "fourteen", "14"))
head(div3_cleaned3, 10)
dim(div3_cleaned3)

# Q5. Replace Debug time column
div3_tidy <- div3_cleaned3 %>%
  mutate(DB_HRS = str_match(`Debug Time`, "(\\d+) hours")[, 2],
         DB_MINS = str_match(`Debug Time`, "(\\d+) minutes")[, 2]
         )
div3_tidy1 <- div3_tidy %>%
  select(- `Debug Time`)
head(div3_tidy1, 10)
dim(div3_tidy1)

# Q6. Types of Variables
# ROWS: Categorical Ordinal(This variable represents the position of each row in the dataset. Even though it is numerical, the row numbers have a specific order, which makes it an ordinal variable)
# PID: Categorical Nominal(The program name has been replace with this identifier, it means this number is the ID number for the data. Each ID number is unique and represents each program)
# LOC: Quantitative Discrete(Represents the numbers of lines of code in the program)
# Bugs REM: Quantitative Discrete(Represents the number of bugs remaining after debugging.)
# DB_HRS:Quantitative Discrete(As time can be measured with more precision than just integers, as the description said they round it to the integer and this variable ends up being discretised because it is the unit of hours, not continuous time)
# DB_MINS: Quantitative Discrete(As time can be measured with more precision than just integers, as the description said they round it to the integer and this variable ends up being discretised because it is the unit of minutes, not continuous time)

#Q7 Data taming
## Q7.1 Naming Conventions for Variable Names
# Rename the columns using rename()
div3_tame <- div3_tidy1 %>%
  rename(
    rows = ROWS,
    pid = PID,
    loc = LOC,
    bugs_rem = `Bugs REM`,
    db_hrs = DB_HRS,
    db_mins = DB_MINS
  )

## Q7.2 Column Arrangement
# Put the column representing the subjects in the first column of a data frame
div3_tame1 <- div3_tame %>%
  relocate("pid", .before = rows)
div3_tame1

# Ordered Factors, Factors, Characters, and Logicals

#rows should be ordered
div3_tame2 <- div3_tame1 %>%
  mutate(rows = as.ordered(rows))

# The identify column should be factor
div3_tame3 <- div3_tame2 %>%
  mutate(pid = as.factor(pid))
# According to Q6, we need to convert loc, bugs_rem, db_hrs, db_mins to integer
div3_tame4 <- div3_tame3 %>%
  mutate(loc = as.integer(loc),
         bugs_rem = as.integer(bugs_rem),
         db_hrs = as.integer(db_hrs),
         db_mins = as.integer(db_mins)
         )
# Display the data after taming
head(div3_tame4, 10)
# Display the dimension after taming
dim(div3_tame4)

# Q8. Random subset
# Set the seed for reproducibility
set.seed(ysn)
#Here we choose a random sample of 700 programs from the dataset
div3_sampled <- div3_tame4 %>%
  sample_n(700)
#Then we order it by rows
div3_sampled <- div3_sampled %>%
  arrange(rows)
# Display the first 10 random sampled dataset
head(div3_sampled, 10)
dim(div3_sampled)

# Q9.
## Q9.(a) Add new columns
#Here we add 4 new columns into the dataset
div3_new <- div3_sampled %>%
  mutate(
    db_totalh = db_hrs + (db_mins / 60),
    db_totalm = (db_hrs * 60) + db_mins,
    time_per_loc = db_totalm / (loc / 1000),
    bugs_per_loc = bugs_rem / (loc / 1000)
  )
#Remove the db_hrs and db_mins
div3_new1 <- div3_new %>%
  select(- db_hrs, - db_mins)
head(div3_new1,10)
dim(div3_new1)

## Q9.(b) Data Type
# db_totalh: Quantitative Continuous(Represents the total number of hours spent debugging, which can be measured on a continuous scale)
# db_totalm: Quantitative Discrete(Represents the total number of minutes spent debugging, which also can be measured on a continuous scale. However, db_totalm represents it as discrete by measuring it in integer minute)
# time_per_loc: Quantitative Continuous(Represents the total time in minutes spent per 1000 lines of code, which is a calculated ratio and can take any real number values)
# bugs_per_loc: Quantitative Continuous(Represents the number of bugs per 1000 lines of code, although bugs could be considered as discrete, the variable bugs_per_loc is ratio over 1000 of code, it means it can take non-integer values, so it is continuous)

# Here we already check 4 columns maintain in the right data type, so we don't need to change it again

# Q10.
## Q10.(a) Display the summary statistics for the numerical values
div3_summary_stats <- inspect_num(div3_new1)
div3_summary_stats

## Q10.(b)
### i. Median debugging time(per 1000 lines of code)
# Here we calculate the median again to check if it is fitting above answer
median_time_per_loc <- median(div3_new1$time_per_loc, na.rm = TRUE)
median_time_per_loc <- round(median_time_per_loc, 2)
median_time_per_loc

### ii. The IQR of the number of remaining bugs(per 1000 lines of code)
iqr_bugs_per_loc <- IQR(div3_new1$bugs_per_loc, na.rm = TRUE)
iqr_bugs_per_loc <- round(iqr_bugs_per_loc, 3)
iqr_bugs_per_loc

# We know IQR is q3 - q1 = 19.28577 - 10.76313 = 8.52264 from (a)

### iii. The program ID and number of lines of code for the longest program
# We get the max loc then filter the pid 
longest_program <- div3_new1 %>%
  filter(loc == max(loc, na.rm = TRUE)) %>%
  select(pid, loc)


# Q11. Plot the histogram and find the skewness
# Here we plot the bugs_per_loc histogram
ggplot(div3_new1, aes(bugs_per_loc)) +
  geom_histogram(fill = 'grey', col = 'black')

# Here we use skewness() to get the skewed value
skew_bugs_per_loc <- moments:: skewness(div3_new1$bugs_per_loc, na.rm = TRUE)
skew_bugs_per_loc
# From the histogram we can see that it is right-skewed and it is unimodal, main values concentrated towards the left side

# Q12.
## Q12.(a) Scatterplot
ggplot(div3_new1, aes(x = time_per_loc, y = bugs_per_loc)) +
  geom_point() +
  geom_smooth(method = 'lm')
# time_per_loc is the explanatory variable because it is the amount of time spent debugging and it affects the number of bugs remaining

## Q12.(b) Linear relationship
# Although the line of best fit shows a general negative trend, the curved shape of the data points means a non-linear relationship.
# The data points scattered more widely at low time_per_loc values, when they become more tightly as time_per_loc increases.
# These patterns show that a simple linear model may not capture the complexity of the relationship well.

# Q13.
## Q13.(a) Box-cox transformation
# Here we display the BoxCox function, and the range of lambda is between -5 to 5 with 0.1 in each step
div3_bc <- BoxCoxTrans(y = div3_new1$bugs_per_loc, x = div3_new1$time_per_loc, lambda = seq(-5, 5, by = 0.1))
div3_bc

# From the result, we can see that estimated lambda is -1.4 and it is in the range of the search

## Q13.(b) tf_bugs column
# Here we use lambda to apply the transformation and add it to the right of the dataset
# First we set the lambda value from estimated lambda
lambda_est <- -1.4
# Here we transformed the data and add it into tf_bugs
div3_new1$tf_bugs <- predict(div3_bc, div3_new1$bugs_per_loc)

# Display the first 10 rows after adding tf_bugs column
head(div3_new1, 10)
# Display the dimension with after adding tf_bugs column
dim(div3_new1)

# Q14. Produce a scatterplot of the Box-Cox transformed data
# Here we perform the scatterplot with a line of best fit
ggplot(div3_new1, aes(x = time_per_loc,y = tf_bugs)) + 
  geom_point() +
  geom_smooth(method = 'lm')
# Here we show the histogram
ggplot(div3_new1, aes(tf_bugs)) +
  geom_histogram(fill = 'grey', col = 'black')
# This is the corresponding skewness
skew_tf_bugs <- moments::skewness(div3_new1$tf_bugs, na.rm = TRUE)
skew_tf_bugs
# As we can see from the scatterplots between untransformed and transformed data, after transformed we got a linear trend, with the line of best fit fitting well, shows that the transformation made the relationship more suitable for linear modeling.
# The transformed histogram is more symmetrical and less skewed, means the transformation imporved the normality of data.

# Q15. Predict
## Q15.(a) General equation

## Q15.(b) Linear model
# Here we use lm() to fit the linear model
div3_model <- lm(tf_bugs ~ time_per_loc, data = div3_new1)
# summary() provided the details, here we can see the estimated coefficients for the model
summary(div3_model)

# Q16. Linear model assumption
# Linearity
plot(div3_model, which = 1)

# Homoscedasticity (Constant Variance of Errors)
plot(div3_model, which = 3)

# Normality
plot(div3_model, which = 2)

# Independence

# Q17.
## Q17.(a) Mean number of bugs remaining after the median debugging time
# First we calculate the median debugging time
med_time_per_loc <- median(div3_new1$time_per_loc)
med_time_per_loc
# It is the same value with the question 10(b)
# Second, we need to use predict() to get the predicted number of buggs remaining for the median debugging time
div3_new_data <- tibble(
  time_per_loc = med_time_per_loc
)
# Predicting the number of remaining bugs with 98% confidence interval
div3_predict <- predict(div3_model, div3_new_data, interval = "confidence", level = 0.98)
div3_predict
# Final, we need to transform the prediction back to original scale, we knew lambda from Q13(b)
div3_ori_pred <- ((div3_predict * lambda_est) + 1) ^ (1 /lambda_est)
div3_ori_pred

## Q17.(b) The number of bugs remaining for the company’s new software project
# 28 days of 24 hours and convert it to minutes, with 80,000 LOC adjusted to units of 1,000 LOC
new_software_time_per_loc <- (28 * 24 * 60) / (80000 / 1000)
# Create a tibble for the debugging time per 1,000 LOC
div3_new_software <- tibble(
  time_per_loc = new_software_time_per_loc
)
# Predicting the number of remaining bugs with 98% confidence interval
div3_predict1 <- predict(div3_model, div3_new_software, interval = "confidence", level = 0.98)
div3_predict1

div3_new_pred <- ((div3_predict1 * lambda_est) + 1) ^ (1 /lambda_est)
div3_new_pred

# Predicted value for 80,000 LOC
div3_new_pred[, "fit"] * (80000 / 1000)
# Upper bound for 80,000 LOC
div3_new_pred[, "upr"] * (80000 / 1000)
# Lower bound for 80,000 LOC
div3_new_pred[, "lwr"] * (80000 / 1000)
