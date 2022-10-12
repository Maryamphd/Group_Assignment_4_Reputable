## Organization 


# Libraries

library(tidyverse)
library(tidytuesdayR)
library(ggplot2)
library(DataExplorer)
library(randomcoloR)

# Download all data files

tuesdata <- tidytuesdayR::tt_load('2022-05-31')

# Individual data frames
poll <- tuesdata$poll
reputation <- tuesdata$reputation

# Cleaning data (removes unwanted columns and NA entries )

poll %>%
  select(company,industry,year,rq) %>%
  na.omit() -> poll

reputation %>%
  select(company,industry,name,score) -> reputation

# Overall Reputation score and RQ for each company 

aggregate(reputation$score, list(reputation$company,
                                 reputation$industry), FUN=mean) -> rep_overall
c("company", "industry", "average_score") -> colnames(rep_overall)

aggregate(poll$rq, list(poll$company,
                        poll$industry), FUN=mean) -> poll_overall
 c("company", "industry", "average_rq") -> colnames(poll_overall)
 
# Merging both data sets

total <- merge(poll_overall,rep_overall,by=c("company","industry"))


## Plots


# Creating Color Palette

n <- 20
palette <- distinctColorPalette(n)

# Plotting Overall Reputation, Poll, and their total

ggplot(rep_overall, aes(average_score, industry)) +
  ggtitle("Industries' Reputation Scores") +
  geom_point()

ggplot(poll, aes(rq, industry)) + 
  ggtitle("Industries' RQ") +
  geom_point()

ggplot(total, aes(average_rq, average_score, colour = industry)) +
  ggtitle("RQ vs Reputation Score of Each Industry")+
  geom_point() +
  scale_color_manual(values = palette)


## Hypothesis Testing


# Overall Reputation hypothesis testing. The null hypothesis is groceries don't have a higher overall reputation than other industries

rep_overall[rep_overall$industry == "Groceries", ] -> rep_groceries

rep_x <- mean(rep_groceries$average_score)
rep_y <- mean(rep_overall$average_score) 
rep_sd <- sd(rep_overall$average_score) 
rep_n <- nrow(rep_groceries)
(rep_x-rep_y)/(rep_sd/sqrt(rep_n)) -> rep_z

if (rep_z > qnorm(p=.05, lower.tail=FALSE)) {
  print("We reject the null hypothesis at the 5% significance level. The grocery industry does have a higher overall reputation than other industries")
} else {
  print("We fail to reject the null hypothesis at the 5% significance level. The grocery industry doesn't have a higher overall reputation than other industries")
}  

# Poll hypothesis testing, The null hypothesis is groceries don't have a higher RQ than other industries

poll[poll$industry == "Groceries", ] -> poll_groceries

poll_x <- mean(poll_groceries$rq)
poll_y <- mean(poll$rq)
poll_sd <- sd(poll$rq)
poll_n <- nrow(poll_groceries)
(poll_x-poll_y)/(poll_sd/sqrt(poll_n)) -> poll_z

if (poll_z > qnorm(p=.05, lower.tail=FALSE)) {
  print("We reject the null hypothesis at the 5% significance level. The grocery industry does have a higher rq than other industries")
} else {
  print("We fail to reject the null hypothesis at the 5% significance level. The grocery industry doesn't have a higher rq than other industries")
}  
