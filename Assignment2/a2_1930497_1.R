#Load the required packages
library(tidyverse)
library(inspectdf)
library(moments)
library(caret)

# Your student number goes here
ysn = 1930497
# Calculate your student number modulo 3
mod4 <- ysn %% 4
mod4

#1
#1.1
# Read in the data using the correct tidyverse command
data <- read_csv("./data/division_1.csv")
# Display the first 10 lines of the data
head(data, 10)

#1.2
# Display the dimension of the data set
dim(data)

#2
#2.1
# Set the seed
set.seed(1930497)
# Shuffle the dataset rows without replacement and name it permuted_data
permuted_data <- sample_n(data, nrow(data), replace = FALSE)
# Display the first 10 rows of the permuted dataset
head(permuted_data, 10)

#2.2
# Display the dimension of the data set
dim(permuted_data)

#3
# Add a 'Rows' column to the left with the row numbers
permuted_data <- permuted_data %>%
  mutate(ROWS = row_number()) %>%
  relocate(ROWS)
permuted_data
# Display the first 10 rows of the dataset with the new 'Rows' column
head(permuted_data, 10)

#4
#4.1
# Identify and display the duplicate rows, excluding the 'ROWS' column
duplicate_rows <- permuted_data %>% 
  group_by(PID, LOC, `Debug Time`, `Bugs REM`) %>% 
  filter(n() > 1)
# Display the duplicate rows (if any)
duplicate_rows

#4.2
# Look for any rows that contain the word "test" in any of the relevant columns
test_data_rows <- permuted_data %>%
  filter(str_detect(PID, "test") | 
           str_detect(LOC, "test") | 
           str_detect(`Debug Time`, "test") | 
           str_detect(`Bugs REM`, "test"))
# Display rows containing "test" if any are found
test_data_rows

#4.3
# Look for any rows that contain a negative value or a minus sign in any column
negative_data_rows <- permuted_data %>%
  filter(str_detect(as.character(PID), "-") |
           str_detect(as.character(LOC), "-") | 
           str_detect(as.character(`Debug Time`), "-") | 
           str_detect(as.character(`Bugs REM`), "-"))
# Display rows containing negative values or minus signs if any are found
negative_data_rows

# Remove the row due to the corrupted value
cleaned_data <- permuted_data[-504, ]

#4.4
# Look for any rows that contain the value '999999999999999'
impossible_value_rows <- cleaned_data %>%
  filter(LOC == 999999999999999 | 
           `Bugs REM` == 999999999999999)
# Display rows containing the impossible value if any are found
impossible_value_rows

# Remove row due to the impossible value
cleaned_data <- cleaned_data[-520, ]

#4.5
# Display rows 964 and 967 by directly referencing their row indices
typo_rows <- cleaned_data[c(964, 967), ]
# Show the rows with potential typos
typo_rows

# Correct the typo in row 964
cleaned_data[964, "Debug Time"] <- str_replace(cleaned_data[964, "Debug Time"], "fourteen", "14")
#Correct the typo in row 967
cleaned_data[967, "LOC"] <- str_replace(cleaned_data[967, "LOC"], "32 thousand 3 hundred and 7", "32307")

# Display the cleaned dataset after correcting the typos
head(cleaned_data, 10)

# Display the dimension of the data set
dim(cleaned_data)

#5
#5.1
#Extract the number of hours and store it in a new column 'DB_HRS'
cleaned_data <- cleaned_data %>%
  mutate(DB_HRS = as.integer(str_extract(`Debug Time`, "\\d+(?= hours)")))

#5.2
#Extract the number of minutes and store it in a new column 'DB_MINS'
cleaned_data <- cleaned_data %>%
  mutate(DB_MINS = as.integer(str_extract(`Debug Time`, "\\d+(?= minutes)")))

#Remove the original 'Debug Time' column
cleaned_data <- cleaned_data %>%
  select(-`Debug Time`)

#Display the first 10 rows of the dataset with 'DB_HRS' and 'DB_MINS' columns on the right
head(cleaned_data, 10)

#Display the dimensions of the dataset
dim(cleaned_data)

# #6
# - **Rows**:  
#   - **Categorical Ordinal**  
#   - This variable represents the row numbers in the dataset. Although numeric, it solely indicates the position of each row without inherent numerical meaning, classifying it as an ordinal variable.
# 
# - **PID**:  
#   - **Categorical Nominal**  
#   - The `PID` column contains unique identifiers for each program. These identifiers are distinct and unordered, making `PID` a nominal variable as it does not imply ranking or quantity.
# 
# - **LOC**:  
#   - **Quantitative Discrete**  
#   - This variable represents the number of lines of code in each program. Since lines of code are countable whole numbers, `LOC` is a discrete quantitative variable.
# 
# - **Bugs REM**:  
#   - **Quantitative Discrete**  
#   - This column records the total count of bugs remaining after debugging. As bugs are counted in whole numbers, `Bugs REM` is a discrete variable, representing a quantifiable measurement.
# 
# - **DB_HRS**:  
#   - **Quantitative Discrete**  
#   - Time is typically continuous because it can be infinitely divided. However, when time is measured or rounded to whole units (like hours or minutes), it takes on discrete characteristics in the context of our data. This variable captures the total number of hours spent on debugging. Time measured in hours as whole numbers makes `DB_HRS` a discrete quantitative variable in the context of debugging duration.
# 
# - **DB_MINS**:  
#   - **Quantitative Discrete**  
#   - Similar to `DB_HRS`, this variable captures debugging time in minutes. As whole, countable units of time, `DB_MINS` is classified as a discrete quantitative variable.

#7
#7.1
# Rename columns to follow snake case conventions
tamed_data <- cleaned_data %>%
  rename(
    rows = ROWS,
    pid = PID,
    loc = LOC,
    bugs_rem = `Bugs REM`,
    db_hrs = DB_HRS,
    db_mins = DB_MINS
  )

#7.2
# Ensure 'pid' is the first column in the dataset
tamed_data <- tamed_data %>%
  relocate(pid, .before = rows)

#7.3
# Convert quantitative discrete variables to integer types and pid to factor
tamed_data <- tamed_data %>%
  mutate(
    pid = as.factor(pid),
    rows = as.ordered(rows),
    loc = as.integer(loc),
    bugs_rem = as.integer(bugs_rem),
    db_hrs = as.integer(db_hrs),
    db_mins = as.integer(db_mins)
  )

# Display the first 10 rows and the dimensions of the tamed dataset
head(tamed_data, 10)
#Display the dimensions of the dataset
dim(tamed_data)

#q8
# Set the seed for reproducibility using my student ID
set.seed(1930497)

# Take a random sample of 700 rows and order by the 'rows' column
sample_data <- tamed_data %>%
  sample_n(700) %>%
  arrange(rows)

# Display the first 10 lines of the sampled dataset and its dimensions
head(sample_data, 10)
dim(sample_data)

#9
#9.1
# Add new columns to the sampled data for analysis
sample_data <- sample_data %>%
  mutate(
    db_totalh = db_hrs + db_mins / 60,                       # Total debugging time in hours
    db_totalm = db_hrs * 60 + db_mins,                       # Total debugging time in minutes
    time_per_loc = db_totalm / (loc / 1000),                 # Debugging time in minutes per 1000 lines of code
    bugs_per_loc = bugs_rem / (loc / 1000)                   # Remaining bugs per 1000 lines of code
  ) %>%
  select(-db_hrs, -db_mins)                                  # Remove db_hrs and db_mins columns

# Display the first 10 rows and the dimensions of the updated dataset
head(sample_data, 10)
dim(sample_data)

# #9.2
# - **db_totalh**:  
#   - **Quantitative Continuous**  
#   - This column represents the total debugging time in hours, combining both the hours and minutes into a single value in decimal format. Since it includes fractional parts when minutes are converted to a fraction of an hour, `db_totalh` is a continuous variable. In R, it is stored as a double to handle these decimal values.
# 
# - **db_totalm**:  
#   - **Quantitative Discrete**  
#   - This column captures the total debugging time in whole minutes. While time can be a continuous measure, `db_totalm` represents it as discrete by measuring it in integer minutes. However, it is stored as a double in R to maintain consistency across time-related variables, even though its values are in whole minutes.
# 
# - **time_per_loc**:  
#   - **Quantitative Continuous**  
#   - This column calculates the debugging time per 1000 lines of code, measured in minutes. As it involves division, the result can have decimal values, making it a continuous variable. It is stored as a double in R to accommodate these fractional values accurately.
# 
# - **bugs_per_loc**:  
#   - **Quantitative Continuous**  
#   - This column represents the number of remaining bugs per 1000 lines of code. Since the division results in decimal values, it is best classified as continuous. It is also stored as a double in R, which allows for fractional values, ensuring accurate representation of the calculated bugs per 1000 lines of code.

#Changing the data type in the tibble according to the type of variable
sample_data <- sample_data %>%
  mutate(
    db_totalm = as.integer(db_totalm)
  )

# Display the first 10 rows and the dimensions of the updated dataset
head(sample_data, 10)

#10
#10.1
# Use inspect_num() to display summary statistics for numerical columns in the dataset
inspect_num(sample_data)

#10.2
# i. Calculate the median debugging time (per 1000 lines of code), rounded to 2 decimal places
median_time_per_loc <- round(median(sample_data$time_per_loc, na.rm = TRUE), 2)

# ii. Calculate the IQR of the number of remaining bugs (per 1000 lines of code), to 3 significant figures
iqr_bugs_per_loc <- signif(IQR(sample_data$bugs_per_loc, na.rm = TRUE), 3)

# iii. Find the program ID and number of lines of code for the longest program in the dataset
longest_program <- sample_data %>%
  filter(loc == max(loc, na.rm = TRUE)) %>%
  select(pid, loc)

# Display results
list(
  `Median Debugging Time` = median_time_per_loc,
  `IQR of The Number of Remainning Bugs` = iqr_bugs_per_loc,
  `Longest Program` = longest_program
)

#11
#11.1
# Histogram of bugs_per_loc with stat="count"
ggplot(sample_data, aes(x = bugs_per_loc)) +
  geom_histogram(fill = "skyblue", color = "black") +
  labs(title = "Histogram of Bugs per 1000 Lines of Code", x = "Bugs per 1000 LOC", y = "Frequency") +
  theme_minimal()

# Calculate skewness of bugs_per_loc
bugs_per_loc_skewness <- skewness(sample_data$bugs_per_loc, na.rm = TRUE)
bugs_per_loc_skewness

#11.2

#12
#12.1
# Scatterplot of bugs_per_loc vs. time_per_loc with line of best fit
ggplot(sample_data, aes(x = time_per_loc, y = bugs_per_loc)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Relationship between Debugging Time and Remaining Bugs per 1000 LOC",
    x = "Time per 1000 LOC (minutes)",
    y = "Bugs per 1000 LOC"
  ) +
  theme_minimal()

#12.2

#12.3

#13
#13.1
# Perform the Box-Cox transformation on bugs_per_loc with time_per_loc as the predictor
box_cox_trans <- BoxCoxTrans(y = sample_data$bugs_per_loc, x = sample_data$time_per_loc, lambda = seq(-5, 5, 0.1))

# Display the estimated lambda value
box_cox_trans

#13.2
# Apply the Box-Cox transformation to create a new column tf_bugs
sample_data$tf_bugs <- predict (box_cox_trans, sample_data$bugs_per_loc)

# Display the first 10 rows and the dimensions of the updated dataset
head(sample_data, 10)
dim(sample_data)

#14
#14.1
# Scatter plot of Box-Cox transformed data with line of best fit
ggplot(sample_data, aes(x = time_per_loc, y = tf_bugs)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Scatterplot of Transformed Bugs per 1000 LOC vs Time per 1000 LOC",
    x = "Time per 1000 LOC",
    y = "Transformed Bugs per 1000 LOC"
  )

#14.2
# Histogram of the transformed data
ggplot(sample_data, aes(x = tf_bugs)) +
  geom_histogram(fill = "skyblue", color = "black") +
  labs(
    title = "Histogram of Transformed Bugs per 1000 LOC",
    x = "Transformed Bugs per 1000 LOC",
    y = "Frequency"
  )

#14.3
# Calculating skewness of the transformed data
transformed_skewness <- skewness(sample_data$tf_bugs)
transformed_skewness

#14.4

#15
#15.1

#15.2
# Build the linear model for tf_bugs against time_per_loc
linear_model <- lm(tf_bugs ~ time_per_loc, data = sample_data)

# View the summary of the model to get parameter estimates
summary(linear_model)

#15.3

#16
#16.1
#Linearity - Residuals vs Fitted plot
plot(linear_model, which = 1)
# Interpretation: Look for a random scatter around the horizontal line. 
# If there is no clear pattern, the linearity assumption is likely satisfied.

#16.2
#Homoscedasticity - Scale-Location plot
plot(linear_model, which = 3)
# Interpretation: Check if points are evenly spread across the range without a funnel shape. 
# Consistent spread indicates homoscedasticity.

#16.3
#Normality - Q-Q plot of residuals
plot(linear_model, which = 2)
# Interpretation: Points should lie close to the line if residuals are normally distributed. 
# Deviations at the tails may indicate non-normality.

#16.4
#Independence - This is usually checked based on study design, not a plot.
# However, we can look at the Residuals vs Fitted plot again to ensure no patterns over time.

# Summary of the model for additional diagnostic information
summary(linear_model)

#17
#17.1
# Define the median debugging time (in minutes per 1000 lines of code)
median_time_value <- tibble(time_per_loc = median(sample_data$time_per_loc))
median_time_value
# Use the model to predict the mean number of bugs remaining after the median debugging time, with a 98% confidence interval
bugs_prediction <- predict(linear_model, median_time_value, interval = "confidence", level = 0.98)
# Display the prediction and confidence interval in transformed scale
bugs_prediction

# Transform the predictions back to the original scale using the Box-Cox lambda (λ = -1.4)
lambda <- -1.4
mean_bugs_pred <- ((bugs_prediction * lambda) + 1)^(1 / lambda)
# Display the transformed prediction and interval
mean_bugs_pred

#17.2
# Calculate the projected debugging time per 1,000 lines of code for the new project
# 28 days of 24 hours each, converted to minutes, with 80,000 LOC adjusted to units of 1,000 LOC
projected_time_per_loc <- (28 * 24 * 60) / (80000 / 1000)
# Create a tibble containing the projected debugging time per 1,000 LOC
new_project_data <- tibble(time_per_loc = projected_time_per_loc)
# Predict the number of remaining bugs using the linear model with a 98% prediction interval
predicted_bugs_transformed <- predict(linear_model, new_project_data, interval = 'prediction', level = 0.98)
# Display the prediction in the transformed scale
predicted_bugs_transformed

# Apply the inverse Box-Cox transformation to obtain predictions in the original scale
predicted_bugs_original <- ((predicted_bugs_transformed * lambda) + 1)^(1 / lambda)
predicted_bugs_original <- predicted_bugs_original * (80000/1000)
# Display the final predictions in the original scale
predicted_bugs_original

