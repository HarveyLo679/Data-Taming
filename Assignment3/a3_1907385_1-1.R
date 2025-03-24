#Load the required packages
library(tidyverse)
library(tidymodels)
library(modelr)
library(car)

# Your student number goes here
ysn = 1907385
# Calculate your student number modulo 4
mod4 <- ysn %% 4
mod4

#1
# Loading the data using the correct tidyverse command
data = read.csv("./data/programs_1.csv")
# Save the data as tibble
data <- as_tibble(data)
# Display the first 10 lines of the data
head(data, 10)

dim(data)

#2
# Identify what types of variables in the current dataset

# * **FError**: Categorical Nominal. This variable shows whether a fetal error occurred, and it represented by binary values. Since the possible values are 1 or 0 and it represents the meaning of the fatal error occurs or not, thus it is a categorical nominal variable.
# * **LOC**: Quantitative Discrete.  This variable represents the number of lines of code in each program. Since lines of code can not be divided to smaller unit, `LOC` is a discrete quantitative variable.
# * **XP**: Categorical Ordinal. XP is categorized as "minimal", "moderate", or "extensive", and three of them have natural order.
# * **foosball**: Categorical Nominal. This variable means if the office has a foosball table. These categories do not have any natural ordering, thus it is a nominal variable.
# * **Partays**: Categorical Ordinal. This variable shows the number of office parties attended during the debugging project. With "none", "some", or "many", thus the categories have a natural ranking, so it is a ordinal.
# * **B..**: Categorical Nominal. This variable indicates if the team received a bonus (yes or no). There is no natural order, which makes it nominal.
  
#3
#3.1
# Fix and tame all column names
# Rename columns to follow snake case conventions
data <- data %>%
  rename(ferror = FError,
         loc = LOC,
         xp = XP,
         parties = Partays,
         bonus = B..)
data
#3.2 
# Convert the fatal error status
data$ferror <- ifelse(data$ferror == 1, "yes", "no")
# Transfer the variable to factor
data$ferror <- as.factor(data$ferror)
data
#3.3
# Treat loc as quantitative continuous 
data$loc <- as.numeric(data$loc)

#3.4
# Store Categorical Ordinal variables as factor
# Convert xp to an ordered factor
data$xp <- factor(data$xp)

#Convert parties to an ordered factor
data$parties <- factor(data$parties)

head(data, 10)     

#3.5
# Ordered Factors, Factors, Characters, and Logicals
data$foosball <- ifelse(data$foosball == "yes", TRUE, FALSE)
data$foosball <- as.logical(data$foosball)
data$bonus <- ifelse(data$bonus == "yes", TRUE, FALSE)
data$bonus <- as.logical(data$bonus)

# Display the first 10 rows and dimensions
head(data, 10)

dim(data)

#4
# Setting the correct seeds
set.seed(ysn)
# Split the data to training and testing dataset
data_split <- initial_split(data, prop = 25/36)
data_training <- training(data_split)
data_testing <- testing(data_split)
data_split

# Display training dataset
head(data_training, 10)
dim(data_training)
# Display testing dataset
head(data_testing, 10)
dim(data_testing)

#5
#Getting a logistic regression model with glm engine and fitting it into the training model
lr_spec <- logistic_reg(mode = "classification") %>%
  set_engine("glm")
# Using all variables as predictors without interaction terms
lr_model <- lr_spec %>%
  fit(ferror ~ ., data = data_training)
lr_model
# Get the summary from the model
summary(lr_model$fit)

#6
# In our model, loc is the only quantitative continuous predictor. Because we will model it linearly, the results in a set of lines will change based on the values of categorical of predictors.
# For the categorical variables, there are xp, foosball, parties, and bonus.
# For xp, there are three variables("minimal", "moderate", "extensive") 3 lines
# For foosball, there are two variables("TRUE" or "FALSE") 2 lines
# For parties, there are three variables("none","some", "many") 3 lines
# For bonus, there are two variables("TRUE" or "FALSE") 2 lines
# For the total lines: 3*2*3*2 = 36 lines

#7
# From Question 5 we fit our logistic regression model to training dataset
# Include all individual variables and all second-order interaction terms using .^2
lr_model2 <- lr_spec %>%
  fit(ferror ~ .^2, data = data_training)
# Using Anova() to get the p-values for variables and interactions
Anova(lr_model2$fit)

# After we found the p-value, we need to identify all interaction terms that meet 90% significance level(p-value < 0.1). Below all the interaction terms are meet the 90% significance level.
# xp:bonus : p-value < 2.2e-16 (highly significant)
# loc:parties : p-value = 9.035e-12 (highly significant)
# loc:xp : p-value = 0.03362 (significant)
# parties:bonus : p-value = 0.02577 (significant)

#8
#8(a)
# Here we remove all the interaction with foosball
lr_model3 <- lr_spec %>%
  fit(ferror ~ loc + xp + parties + bonus + loc:xp + loc:parties + loc:bonus + xp:parties + xp:bonus + parties:bonus, data = data_training)
# Using Anova() to show the output
Anova(lr_model3$fit)

#8(b)
# From the question, we need to remove the interaction not meet 90% significance level one by one.
# Step 1: We identified loc:bonus has the lowest significance level, which p-value = 0.88183, thus we remove it in this step
lr_model4 <- lr_spec %>%
  fit(ferror ~ loc + xp + parties + bonus + loc:xp + loc:parties + xp:parties + xp:bonus + parties:bonus, data = data_training)
# Showing the Anova output
Anova(lr_model4$fit)

# Step 2: After the first step, we found that xp:parties has the lowest significance level, let's remove it.
lr_model5 <- lr_spec %>%
  fit(ferror ~ loc + xp + parties + bonus + loc:xp + loc:parties + xp:bonus + parties:bonus, data = data_training)
# Showing the Anova output
Anova(lr_model5$fit)

# The final model has no terms should be removed, all interaction terms are all meet 90% significance level now and also adhering to the principle of marginality.

#8(c)
# After applying backward stepwise regression, we have reached a model where all individual terms and interaction terms are significant at the 90% level (p-value < 0.10).
# loc : p-value < 2.2e-16 (high significant)
# xp : p-value < 2.2e-16 (high significant)
# parties : p-value < 2.2e-16 (high significant)
# bonus : p-value = 0.012373 (significant)
# Since there is no terms below 90% level, no further regression steps are needed.

#9
#9(a)
# For the final model, there are 4 interaction terms meet 90% significance level:
# loc:xp : p-value = 0.006139 (high significance)
# loc:parties : p-value = 8.634e-12 (high significance)
# xp:bonus : p-value < 2.2e-16 (high significance)
# parties:bonus : p-value = 0.033446 (high significance)

#9(b)
# For this part, we will use some graphs to prove
# First, we plot interaction between xp and loc(xp:loc)
ggplot(data_training, aes(x = xp, y = loc, col = ferror)) +
  geom_boxplot() +
  labs(title = "Interaction Between XP & LOC",
       x = "XP",
       y = "LOC",
       col = "FError")
# According to the plot, we can see that the relationship between experience level, lines of code, and the occurrence of fatal errors  varies significantly across different levels of experience. For the minimal experience group, the median LOC is lower compared to the moderate and extensive experience groups. Additionally, the proportion of fatal errors is higher for the minimal experience group. The extensive experience group shows higher variability in LOC, with a wider interquartile range, and a lower proportion of fatal errors compared to the minimal group.
# The amount of code written and the fatal errors occurence may be influenced by the experience level of the team leader. Team leader with extensive experience may be involved in more complex or larger projects. Given in higher LOC and greater variability, but they are also more effective in avoiding fatal errors. In contrast, those with minimal experience may work on simpler tasks, producing a lower LOC but facing a higher likelihood of fatal errors due to inexperience.
# In summary, more experienced team members might be trusted with tasks that require writing more lines of code, which could explain the higher LOC observed in the moderate and extensive groups. Their experience may also contribute to fewer fatal errors. Conversely, less experienced members may produce more consistent but smaller outputs, leading to lower median LOC and a higher incidence of errors due to limited experience and familiarity with complex scenarios.

# Second, we plot the interaction between loc and parties(loc:parties)
ggplot(data_training, aes(x = parties, y = loc, col = ferror)) +
  geom_boxplot() +
  labs(title = "Interaction Between LOC & Parties",
       x = "Parties",
       y = "LOC",
       col = "FError")
# The boxplot shows the relationship between the number of office parties and the lines of code (LOC), categorized by whether a fatal error occurred. The boxes represent the interquartile range, with the middle line indicating the median LOC. The colored boxes show the distinction between projects with and without fatal errors.
# A potential hypothesis is the occurrence of fatal errors may be influenced by the number of office parties and the volume of work . Teams that had many office parties tend to have a lower median LOC, but the posibility of fatal error occurs obviously lower than two other groups. This might shows that high workload combined with increased social activities could lead to higher stress and no enough time to create more code of lines. However, due to attending the office parties they obviously lower the rate of fatal error occurred. Maybe the parties helped to create a good team work. In contrast, teams with none or some office parties show higher fatal error occurence rate. Both groups median LOC are higher, possibly indicating better focus or fewer distractions during work. But lower the team integration to prevent fatal error occurred.
# Easier to say, office parties could have a dual impact on productivity. On one hand, they may enhance team morale, leading to lower likelihood of error occurs. On the other hand, frequent social gatherings might also distract the team, resulting of low output (more lines of code). This dual effect can be observed in the variation of LOC and the presence of fatal errors across different levels of office parties.

# Third, we plot the interaction between xp and bonus(xp:bonus)
ggplot(data_training, aes(x = xp, fill = bonus)) +
  geom_bar(position = "fill") + 
  labs(
    title = "Interaction Between XP & Bonus",
    x = "XP",
    y = "Propotion of Total",
    fill = "Bonus") +
  facet_wrap(~ferror)

#According to the plot, we observe that the relationship between experience level and bonus received varies across different experience. For the minimal experience team leader group, the proportion of individuals who received a bonus is lower compared to the other groups. On the other hand, for the extensive experience group, the percentage of individuals receiving a bonus is significantly higher.
#More experienced team leader team are more likely to receive bonuses due to their effort, greater productivity, or higher skill level. They may also be more deserved for bonuses because they are more familiar with expectations and how to exceed them. By a contrast, team leader with minimal experience might be less likely to receive bonuses, as they are still learning and might not be as effective as their more experienced colleagues.
#Bonuses may be used as a motivation tool for experienced workers to keep their performance high, as they are critical for achieving the project's goals. For less experienced team leader, the focus might be more on training and growth rather than immediate reward, which could explain the lower distribution of bonuses among them.

# Forth, we plot the interaction between parties and bonus (parties:bonus)
ggplot(data_training, aes(x = parties, fill = bonus)) +
  geom_bar(position = "fill") +
  labs(
    title = "Interaction Between Parties & Bonus", 
    x = "Parties", 
    y = "Propotion of Total", 
    fill = "Bonus") +
  facet_wrap(~ferror)
# According to the plot, we observe the interaction between office parties, whether a bonus was received, and the occurrence of fatal errors. The plot shows how the distribution of bonuses changes depending on the number of office parties attended, while also differentiating between a fatal error occurred or did not occur. For each level of parties, we can see the proportional difference in the frequency of receiving a bonus across fatal error conditions.
# A potential hypothesis is that the occurrence of fatal errors might effect how bonuses are distributed, depending on participation in office parties. For instance, workers who attend many office parties and experience fewer fatal errors may be more likely to receive bonuses due to improved performance, as their active social engagement could positively impact team cohesion and productivity. By a contrast, those who attend fewer or no office parties and experience fatal errors may be less likely to receive bonuses, potentially due to lower perceived contribution to team dynamics.
# For the interaction between office parties, bonuses, and fatal errors. Active participation in office events could contribute to better communication and morale within the team, leading to a lower likelihood of fatal errors. This might be different for people who increase the chances of receiving bonuses for those individuals. On the other hand, low participation in office activities might correlate with a higher incidence of errors, possibly due to weaker team integration, which could lead to fewer bonuses being awarded to such individuals.




#10

summary(lr_model5$fit)

#11
#12
#13
#14

testing_predictions <- data_testing %>%
  select(ferror) %>%
  bind_cols(
    predict(
      object = lr_model5,
      new_data = data_testing,
      type = "class"
    ) %>% 
      rename(predicted_class = .pred_class)
  ) %>%
  bind_cols(
    predict(
      object = lr_model5,
      new_data = data_testing,
      type = "prob"
    ) %>%
      rename(prediction_prob = .pred_yes)
  )
head(testing_predictions, 10)
dim(testing_predictions)

#15
#15(a)
# Display confusion matrix
testing_predictions %>%
  conf_mat(truth = ferror, estimate = predicted_class)
# Using accuracy() to get the accuracy
testing_predictions %>%
  accuracy(truth = ferror, estimate = predicted_class)

#15(b)
# Here we calculate sensitivity manually
sens <- 4563 / (4563 + 1408)
sens
# Here we calculate specificity manually
spec <- 3516 / (1513 + 3516)
spec

#15(c)
testing_predictions %>%
  roc_curve(truth = ferror, .pred_no) %>%
  autoplot() +
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5)

#15(d)
testing_predictions %>%
  roc_auc(truth = ferror, .pred_no)
#16

bob_team <- tibble(
  loc = 100000,
  xp = "minimal",
  foosball = TRUE,
  parties = "many",
  bonus = TRUE
)

bob_team_prediction <- bob_team %>%
  bind_cols(
    predict(
      object = lr_model5,
      new_data = bob_team,
      type = "class"
    ) %>%
      rename(predicted_class = .pred_class)
  ) %>%
  bind_cols(
    predict(
      object = lr_model5,
      new_data = bob_team,
      type = "prob"
    ) %>%
      rename(prediction_prob = .pred_yes)
  )
bob_team_prediction
