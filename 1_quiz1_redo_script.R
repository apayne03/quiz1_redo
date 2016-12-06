library(tidyverse)

raw_data <- read_csv(file = "lab_quiz_week2_data (1).csv")
#View(raw_data)

raw_data <- read_csv(file = "lab_quiz_week2_data (1).csv", na=c("","NA","-999"))
# Result = Now you can see that the -999 values are correctly inidacted as missing (i.e., "NA")


# HANDLING CATEGORICAL VARIABLES TO FACTORS
categorical_variables <- select(raw_data, univ, prog_year, age)

categorical_variables$univ <- as.factor(categorical_variables$univ)
levels(categorical_variables$univ) <- list("Waterloo"=1, "Guelph"=2)

categorical_variables$prog_year <- as.factor(categorical_variables$prog_year)
levels(categorical_variables$prog_year) <- list("First Year"=1, "Second Year"=2, "Third Year"=3, "Fourth Year"=4, "Grad School"=5)


# HANDLING ITEM DATA

# create sets of scale items...
pos_affect_items <- select(raw_data, PA1, PA2, PA3, PA4, PA5)
dep_items <- select(raw_data, D1, D2, D3, D4, D5)
prog_sat_items<- select(raw_data, PS1, PS2, PS3, PS4, PS5)

# check for out of range values in each set of items...
psych::describe(pos_affect_items)
is_bad_value <- pos_affect_items<1 | pos_affect_items>7
pos_affect_items[is_bad_value] <- NA
psych::describe(pos_affect_items)

psych::describe(dep_items)
is_bad_value <- dep_items<1 | dep_items>4
dep_items[is_bad_value] <- NA
psych::describe(dep_items)

psych::describe(prog_sat_items)
is_bad_value <- prog_sat_items<1 | prog_sat_items>6
prog_sat_items[is_bad_value] <- NA
psych::describe(prog_sat_items)

# flip any reverse-key items...
pos_affect_items <- mutate(pos_affect_items, PA1=8-PA1)
dep_items <- mutate(dep_items, D4=5-D4)
dep_items <- mutate(dep_items, D5=5-D5)
prog_sat_items <- mutate(prog_sat_items, PS1=7-PS1)
prog_sat_items <- mutate(prog_sat_items, PS2=7-PS2)

# obtain scale scores...
pos_affect <- psych::alpha(as.data.frame(pos_affect_items), check.keys = FALSE)$scores
dep <- psych::alpha(as.data.frame(dep_items), check.keys = FALSE)$scores
prog_sat <- psych::alpha(as.data.frame(prog_sat_items), check.keys = FALSE)$scores


# COMBINE EVERYTHING INTO ANALYTIC_DATA

analytic_data <- cbind(categorical_variables, pos_affect, dep, prog_sat)

write_csv(analytic_data, path = "quiz1_redo_analytic_data.csv")
