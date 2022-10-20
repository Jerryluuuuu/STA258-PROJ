# Hello
# We begin by reading our "CCHS.csv" file into R

# Load the tidyverse library
# tidyverse: https://www.tidyverse.org/
library(tidyverse)

CCHS <- read_csv("CCHS.csv")
# :)

# Look at the object
head(CCHS)

# Oh! Lots of . missing cases

# Look at structure of object
str(CCHS) 

# All variables are character type, not numeric

# Let's look at frequency table of our response variable
# GEN_015: Perceived Mental Health

# We will use %>% - pipe operator for chaining a sequence of operations
# part of dplyr package (grammar of data manipulation)
# dplyr: https://dplyr.tidyverse.org/

CCHS %>% 
  count(GEN_015)

# First convert character variable columns to numeric variables
CCHS.Num <- CCHS %>% mutate_if(is.character, as.numeric)

# Let's obtain frequency table of our response variable
# GEN_015: Perceived Mental Health
CCHS.Num %>% 
  count(GEN_015)
  
# Let's obtain frequency table of age variable
CCHS.Num %>% 
  count(dhhgage)

# Let's obtain frequency table of attending school/college/university variable
CCHS.Num %>% 
  count(MAC_015)

# Let's filter data 
## by whether or not attending school/college/university variable
  ## 1 = Yes, 2 = No
## and by age range
  ## 2 = 15-17, 3 = 18-19, 4 = 20-24, 5 = 25-29
CCHS.2 <- CCHS.Num %>% filter((MAC_015 == 1 | MAC_015 == 2), (dhhgage > 1 & dhhgage <= 5))

# Let's obtain frequency table of age variable
CCHS.2 %>% 
  count(dhhgage)

# Let's obtain frequency table of attending school/college/university variable
CCHS.2 %>% 
  count(MAC_015)

# Let's obtain frequency table of gender
CCHS.2 %>% 
  count(DHH_SEX)

# We will work with complete cases
# However, if we drop all NA values for any columns
CCHS.drop <- CCHS.2 %>% drop_na()

# We won't have any data left!

# So, depending on our research question,
# We will drop NA values from that analysis
# We will also filter by specific data values since
# for some variables we need to omit some responses:
## 96: Valid skip
## 97: Don't know
## 98: Refusal
## 99: Not stated

# But, let's look at frequency table of our response variable
# GEN_015: Perceived Mental Health
CCHS.2 %>% 
  count(GEN_015)

# Let us drop na values from our response variable
# GEN_015: Perceived Mental Health
CCHS.2 <- CCHS.2 %>% drop_na(GEN_015)

# Let's look at frequency table of our response variable
# GEN_015: Perceived Mental Health
CCHS.2 %>% 
  count(GEN_015)

# Let's work with CCHS.2 data frame
# We can use the package skimr to take a look at the data
install.packages("skimr")
library(skimr)

# Take a look at CCHS.2 data
skim(CCHS.2)

# The janitor packages offers the tabyl() function 
# to produce tabulations # and cross-tabulations, 
# which can be adorned or modified with helper functions 
# to display percents, proportions, counts, etc.
install.packages("janitor")
library(janitor)

## Investigate association between age and whether or not attending school
CCHS.2 %>%                                    # data frame
  tabyl(dhhgage, MAC_015) %>%                 # cross-tabulate counts of two columns
  adorn_totals(where = c("row", "col")) %>%   # add a total row, add a total column
  adorn_percentages(denominator = "row") %>%  # convert to proportions with row denominator
  adorn_pct_formatting() %>%                  # convert proportions to percents
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    row_name = "Age",
    col_name = "Attending School")

## Re-categorize levels of 
# Perceived Mental Health
# Age
# Gender
# Attending School
CCHS.2 <- CCHS.2 %>% 
  mutate(Mental.Health = case_when(GEN_015 == 1 ~ "Excellent", 
                                   GEN_015 == 2 ~ "Very Good",
                                   GEN_015 == 3 ~ "Good",
                                   GEN_015 == 4 ~ "Fair",
                                   GEN_015 == 5 ~ "Poor"),
         Age = case_when(dhhgage == 2 ~ "15-17", 
                         dhhgage == 3 ~ "18-19",
                         dhhgage == 4 ~ "20-24",
                         dhhgage == 5 ~ "25-29"),
         Gender = case_when(DHH_SEX == 1 ~ "Male", 
                            DHH_SEX == 2 ~ "Female"),
         Attend.School = case_when(MAC_015 == 1 ~ "Yes", 
                                   MAC_015 == 2 ~ "No"))

# As factor with ordered levels
Mental.Health.Levels <- c("Excellent", 
                          "Very Good", 
                          "Good", 
                          "Fair", 
                          "Poor")

# Add new variables to CCHS2 Data Set using mutate function
# mutate() dplyr function
CCHS.2 <- CCHS.2 %>% mutate(Mental_Health = factor(Mental.Health, 
                                                   levels = Mental.Health.Levels))

# Let's look at frequency table of our response variable
# Perceived Mental Health
CCHS.2 %>% count(Mental_Health) %>%   # count() dplyr function       
  mutate(prop = prop.table(n)) %>% # mutate() dplyr function
  adorn_totals() -> freq.table.responses # adorn_totals() janitor function 
freq.table.responses

# Load ggplot2 library: Grammar of Graphics for Data Visualization
library(ggplot2)

# Bar Plots of Gender and Mental Health Perception
bar.plot = ggplot(data = CCHS.2, aes(x = Mental_Health, fill = factor(Gender)))
bar.plot = bar.plot + geom_bar(position = "dodge")
bar.plot = bar.plot + labs(fill = "Gender")
bar.plot = bar.plot + ggtitle("Bar Plots of Gender and Perception of Mental Health")
bar.plot = bar.plot + theme(axis.text.x=element_text(angle=90, hjust=1))
bar.plot = bar.plot + xlab("Mental Health Perception")  
bar.plot = bar.plot + ylab("Count")
bar.plot = bar.plot
bar.plot

## Investigate association between Gender and perception of mental health
CCHS.2 %>%                                    # data frame
  tabyl(Gender, Mental_Health) %>%            # cross-tabulate counts of two columns
  adorn_totals(where = c("row", "col")) %>%   # add a total row, add a total column
  adorn_percentages(denominator = "row") %>%  # convert to proportions with row denominator
  adorn_pct_formatting() %>%                  # convert proportions to percents
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    row_name = "Gender",
    col_name = "Perceived Mental Health")

## Re-categorize levels of 
# Perceived Mental Health
# Positive as Excellent, Very Good, Good v.s. Fair or Poor
CCHS.2 <- CCHS.2 %>% 
  mutate(positive.mental.health = case_when(GEN_015 <= 3 ~ "Yes", 
                                            GEN_015 >= 4 ~ "No"))

# Let's look at frequency table of our response variable
# Perceived Mental Health
CCHS.2 %>% count(positive.mental.health) %>%   # count() dplyr function       
  mutate(prop = prop.table(n)) %>% # mutate() dplyr function
  adorn_totals() -> freq.table.responses # adorn_totals() janitor function 
freq.table.responses

## Investigate association between gender and perception of mental health
CCHS.2 %>%                                  # data frame
  tabyl(Gender, positive.mental.health) %>%                 # cross-tabulate counts of two columns
  adorn_totals(where = c("row", "col")) %>%   # add a total row, add a total column
  adorn_percentages(denominator = "row") %>%  # convert to proportions with row denominator
  adorn_pct_formatting() %>%                  # convert proportions to percents
  adorn_ns(position = "front") %>%            # display as: "count (percent)"
  adorn_title(                                # adjust titles
    row_name = "Gender",
    col_name = "Positive Mental Health")

# Bar Plots of Gender and Mental Health Perception
bar.plot = ggplot(data = CCHS.2, aes(x = factor(positive.mental.health), fill = factor(Gender)))
bar.plot = bar.plot + geom_bar(position = "dodge")
bar.plot = bar.plot + labs(fill = "Gender")
bar.plot = bar.plot + ggtitle("Bar Plots of Gender and Perception of Mental Health")
bar.plot = bar.plot + theme(axis.text.x=element_text(angle=90, hjust=1))
bar.plot = bar.plot + xlab("Positive Mental Health")  
bar.plot = bar.plot + ylab("Count")
bar.plot = bar.plot
bar.plot





