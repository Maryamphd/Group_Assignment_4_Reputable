## Set Up


# Libraries

library(tidyverse)
library(ggplot2)
library(DataExplorer)
library(randomcoloR)
library(BSDA)
library(gridExtra)
library(dplyr)
library(skimr)

# Import all data files

poll <- read.csv("../data/poll.csv")
reputation <- read.csv("../data/reputation.csv")


## Organization


# Combining both data sets and removing NA values 

reputation %>%
  group_by(company, industry) %>%
  summarise(score_avg = mean(score,na.rm = TRUE)) -> reputation
poll %>%
  group_by(company, industry) %>%
  summarise(rq_avg = mean(rq,na.rm = TRUE)) %>%
  full_join(reputation) %>%
  na.omit() -> total

?mean()

# ^ added na.omit() because 3 of the observations had NA rq (Big Lots,Shein,Stellantis)

# Average RQ per Industry
total %>%
  group_by(industry) %>%
  summarise(rq_avg = mean(rq_avg)) -> industryRQ


##EDA


# Exploring the Data set

unique(total$industry) #19 industries

glimpse(total)

summary(total)

plot_missing(total)

create_report(total)

skim(total) #Perform skim to display summary statistics, skim() - expands on summary() by providing larger set of statistics


## Plots


# Scatter plot -  Correlation Coefficient of score and RQ

ggplot(total, aes(rq_avg, score_avg)) +
  ggtitle("RQ vs Reputation Score of each Company")+
  geom_point() +
  labs(x= "Poll Score", y="Reputation Score") +
  geom_smooth(formula = y ~ x, method = "lm")
coeff <- cor(total$rq_avg,total$score_avg)

# Bar Plot - Average RQ per industry

ggplot(data = industryRQ, mapping = aes(x = reorder(industry, -rq_avg), rq_avg)) + 
  geom_bar(stat = "identity") +
  labs(y= "RQ Average",x= "Industry") +
  geom_text(aes(label = round(rq_avg, digits = 0), vjust = 1.5)) +
  scale_x_discrete(guide = guide_axis(angle = 90))
