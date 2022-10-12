# Libraries
library(tidyverse)
library(tidytuesdayR)
library(ggplot2)
library(DataExplorer)

# Download all data files
tuesdata <- tidytuesdayR::tt_load('2022-05-31')

# Individual dataframes
poll <- tuesdata$poll
reputation <- tuesdata$reputation


# Cleaning data (removes unwanted columns and NA entries )

poll %>%
  select(company,industry,year,rq) %>%
  na.omit() -> poll

reputation %>%
  select(company,industry,name,score) -> reputation
  

  
# separate poll by year
  poll21 <- filter(poll,year=="2021") 
  poll20 <- filter(poll,year=="2020") 
  poll19 <- filter(poll,year=="2019") 
  poll18 <- filter(poll,year=="2018") 
  poll17 <- filter(poll,year=="2017") 

  
# separate reputation by attribute
  
rep_citi <- filter(reputation, name=="CITIZENSHIP")
rep_pns <- filter(reputation, name=="P&S")
rep_cult <- filter(reputation, name=="CULTURE")
rep_grow <- filter(reputation, name=="GROWTH")
rep_visi<- filter(reputation, name=="VISION")
rep_ethi <- filter(reputation, name=="ETHICS")
rep_trus <- filter(reputation, name=="TRUST")

# Overall Reputation (reputation -> rep_overall)

aggregate(reputation$score, list(reputation$company,
                                 reputation$industry), FUN=mean) -> rep_overall
colnames(rep_overall) <- c("company", "industry", "average_score")
  
aggregate(poll$rq, list(poll$company,
                                 poll$industry), FUN=mean) -> poll_overall
colnames(poll_overall) <- c("company", "industry", "average_rq")


# Split data

t.test(average_score ~ industry, data = rep_overall)

t.test(average_score ~ industry, data = rep_overall, subset =  %in% c())

# Calculations
reputation_citi %>%
group_by(industry)  %>%
summarise(Average_Score = mean(score)) -> rep_citi_ind


?aggregate

reputation_pns %>%
  group_by(industry)  %>%
  summarise(Average_Score = mean(score)) -> rep_pns_ind

reputation_culture %>%
  group_by(industry)  %>%
  summarise(Average_Score = mean(score)) -> rep_cult_ind

reputation_growth %>%
  group_by(industry)  %>%
  summarise(Average_Score = mean(score)) -> rep_grow_ind


reputation_vision %>%
  group_by(industry)  %>%
  summarise(Average_Score = mean(score)) -> rep_vis_ind


reputation_ethics %>%
  group_by(industry)  %>%
  summarise(Average_Score = mean(score)) -> rep_ethi_ind

reputation_trust %>%
  group_by(industry)  %>%
  summarise(Average_Score = mean(score)) -> rep_tru_ind

## H1: The grocery industry have an overall better reputation than other industries


rep_overall %>%
  group_by(industry)  %>%
  summarise(Average_Score = mean(average_score)) -> rep_overall_industry

# Plots

  ggplot(rep_overall, aes(average_score, industry)) +
    ggtitle("Industries' Reputation Scores") +
  geom_point()
  
  ggplot(poll, aes(rq, industry)) + 
    ggtitle("Industries' RQ") +
    geom_point()

allscores 
  
?aes
  
  
  t.test(poll,reputation)
  
  
# Overall Reputation hypothesis testing. The null hypothesis is groceries don't have a higher overall reputation than other industries
  rep_overall[rep_overall$industry == "Groceries", ] -> rep_groceries
  
  rep_x <- mean(rep_groceries$average_score)
  rep_y <- mean(rep_overall$average_score) 
  rep_sd <- sd(rep_overall$average_score) 
  rep_n <- nrow(rep_groceries)
  (rep_x-rep_y)/(rep_sd/sqrt(rep_n)) -> rep_z

 
 if (rep_z > qnorm(p=.05, lower.tail=FALSE)) {
   print("We reject the null hypothesis at the 5% significance level. The grocery industry have a higher overall reputation than other industries")
 } else {
   print("We fail to reject the null hypothesis at the 5% significance level. The grocery industry doesn't have a higher overall reputation than other industries")
 }  
  

# Poll hypothesis testing, The null hypothesis is groceries don't have a higher rq than other industries
  
poll[poll$industry == "Groceries", ] -> poll_groceries
  
  poll_x <- mean(poll_groceries$rq)
  poll_y <- mean(poll$rq)
  poll_sd <- sd(poll$rq)
  poll_n <- nrow(poll_groceries)
  (poll_x-poll_y)/(poll_sd/sqrt(poll_n)) -> poll_z

if (poll_z > qnorm(p=.05, lower.tail=FALSE)) {
  print("We reject the null hypothesis at the 5% significance level. The grocery industry have a higher rq than other industries")
} else {
  print("We fail to reject the null hypothesis at the 5% significance level. The grocery industry doesn't have a higher rq than other industries")
}  


