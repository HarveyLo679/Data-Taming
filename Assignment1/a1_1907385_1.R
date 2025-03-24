library(tidyverse)
library(inspectdf)
library(caret)
library(moments)
library(tidymodels)
library(modelr)
library(ISLR)
library(car)

#Setting
ysn = 1907385
# Calculate your student number modulo 3
mod4 <- ysn %% 4
mod4

#Q1
filename <- paste0("a1_1907385_3.pdf")
filename

ind1 <- read_csv("./data/India.csv")
ind1 <- as_tibble(ind1)
head(ind1, 10)

#Q2
dim(ind1)

#Q3
set.seed(1907385)
ind1_permuted <- sample_n(ind1, 50)
head(ind1_permuted, 10)

#Q4
ind1_permuted <- ind1_permuted %>% 
  mutate(Rows=row_number()) %>% relocate("Rows", .before = Player)
head(ind1_permuted, 10)

#Q5
#
ind1_clean <- ind1_permuted %>%
  distinct()
sum(duplicated(ind1_clean))

#
ind1_clean1 <- ind1_clean %>%
  filter(!(Player == "test1test1")
         )

#
ind1_clean1[ind1_clean1$Player == "IK Pathan", "2005"] <- 
  str_replace(ind1_clean1[ind1_clean1$Player == "IK Pathan", "2005"], "-", "")
ind1_clean1

inspect_na(ind1_clean1)

#
ind1_clean2 <- ind1_clean1 %>%
  mutate(`2002` = str_replace(`2002`, "zero", "0"))
print(ind1_clean2[ind1_clean2$Player == "T Yohannan", "2002"])

#
ind1_clean3 <- ind1_clean2 %>%
  filter(!(Player == "NM Kulkarni")
  )

print(ind1_clean3 %>% filter(Player == "NM Kulkarni"))
#
invalid_values <- ind1_clean3[!(ind1_clean3$RightHanded %in% c("yes", "no")),]
invalid_values
#
ind1_clean3$RightHanded[ind1_clean3$RightHanded == "Y"] <- "yes"
ind1_clean3

#Q6
##(a)
ind1_convert <- ind1_clean3 %>%
  gather(key = "year", value = "performance", starts_with("200"))

ind1_convert

##(b)
details <- str_match(ind1_convert$performance, "(\\d+) innings, (\\d+) outs, (\\d+) runs, (\\d+) balls")
ind1_performance <- ind1_convert %>%
  mutate(
    Innings = as.numeric(details[,2]),
    Outs = as.numeric(details[,3]),
    Runs = as.numeric(details[,4]),
    Balls = as.numeric(details[,5])
  )
ind1_performance

#(c)
ind1_cleaned <- ind1_performance %>%
  select(-performance)

ind1_cleaned
#(d)
ind1_cleaned1 <- ind1_cleaned %>%
  mutate(T_rows = row_number()) %>% relocate("T_rows", .before = Player)
ind1_cleaned1
#Q7

#Q8
#
ind1_taming <- ind1_cleaned1 %>%
  rename_with(tolower)

#
ind1_taming$rows <- as.ordered(ind1_taming$rows)

ind1_taming$t_rows <- as.ordered(ind1_taming$t_rows)

ind1_taming$year <- as.ordered(ind1_taming$year)

ind1_taming$player <- as.factor(ind1_taming$player)

head(ind1_taming, 10)
dim(ind1_taming)

#Q9
ind1_balls <- ind1_taming %>%
  filter(balls > 0)
head(ind1_balls, 10)

#Q10
#(a)
set.seed(ysn)
ind1_sample <- sample_n(ind1_balls, 70)

#(b)
ind1_sorted <- ind1_balls %>%
  arrange(t_rows)
head(ind1_sorted, 10)
dim(ind1_sorted)

#Q11
#(a)
ind1_new_columns <- ind1_sorted %>%
  mutate(
    pct_out = (outs/innings)*100,
    run_rate = runs/balls) %>% 
  relocate(pct_out, run_rate, .after = year)
head(ind1_new_columns, 10)
dim(ind1_new_columns)  

#(b)
min_pct_out = ind1_new_columns %>%
  filter(pct_out == min(pct_out)) %>%
  select(player, year, pct_out)
min_pct_out

max_run_rate <- ind1_new_columns %>%
  filter(run_rate == max(run_rate)) %>%
  select(player, year, run_rate)
max_run_rate

#Q12
ind1_new_columns1 <- mutate(ind1_new_columns, year = factor(year, ordered = TRUE))

ggplot(ind1_new_columns1, aes(x = year, y = run_rate, fill = righthanded)) +
  geom_boxplot()

#Q13
#
boxplot_pct_out <- ggplot(ind1_new_columns1, aes(x = righthanded, y = pct_out, fill = righthanded)) +
  geom_boxplot()
boxplot_pct_out
#
boxplot_run_rate <- ggplot(ind1_new_columns1, aes(x = righthanded, y = run_rate, fill = righthanded)) +
  geom_boxplot()
boxplot_run_rate
