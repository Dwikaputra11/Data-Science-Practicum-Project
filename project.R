library(readxl)
library(tidyverse)
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(Hmisc)

file <- read_excel(path = here("data-raw/Smartphone_usage_and_Intenational_Tourist_Behavior.xlsx"))
head(file)

# delete na valou from age
class(file)
filter.age <- file %>% as.data.frame() %>% drop_na(p_age)

#Add New Column For Categories
add.category <- filter.age %>% mutate(category = case_when(
  p_age > 0 & p_age <= 20 ~ "0 - 20",
  p_age > 20 & p_age <= 30 ~ "21 - 30",
  p_age > 30 & p_age <= 50 ~ "31 - 50",
  p_age > 50 & p_age <= 85 ~ "51 - 85",
))

country.mean.internet.usage <- add.category %>% select(p_country, internet_daily_usage)
category.mean.internet.usage <- add.category %>% select(category, internet_daily_usage)

country.mean <- country.mean.internet.usage %>% group_by(p_country) %>% dplyr::summarize(average = round(mean(internet_daily_usage), 2)) 
category.mean <- category.mean.internet.usage %>% group_by(category) %>% dplyr::summarize(average = round(mean(internet_daily_usage), 2))


pretrip.source.with.age.category <- add.category %>% 
  select(pre_OA, pre_SM, pre_NP, pre_Mg, pre_TV,pre_TA, pre_AA,pre_Bl, pre_SE, pre_Rec, category) %>%
  rename(
    "Online Advertising" = pre_OA,
    "Social Media" = pre_SM,
    "Newspaper" = pre_NP,
    "Magazine" = pre_Mg,
    "Television" = pre_TV,
    "TripAdvisor" = pre_TA,
    "Advice Agent" = pre_AA,
    "Blog" = pre_Bl,
    "Search Engine" = pre_SE,
    "Recommendation" = pre_Rec,
  )

head(pretrip.source.with.age.category$`Online Advertising`)

pretrip.source <- add.category %>% 
  select(pre_OA, pre_SM, pre_NP, pre_Mg, pre_TV,pre_TA, pre_AA,pre_Bl, pre_SE, pre_Rec) %>%
  rename(
    "Online Advertising" = pre_OA,
    "Social Media" = pre_SM,
    "Newspaper" = pre_NP,
    "Magazine" = pre_Mg,
    "Television" = pre_TV,
    "TripAdvisor" = pre_TA,
    "Advice Agent" = pre_AA,
    "Blog" = pre_Bl,
    "Search Engine" = pre_SE,
    "Recommendation" = pre_Rec,
  )

pretrip.percentage.by.age.category <- pretrip.source.with.age.category %>%
  group_by(category) %>% dplyr::summarize(
    "Online Advertising" = round(sum(`Online Advertising` == 1)/sum(length(`Online Advertising`)), 2),
    "Social Media" = round(sum(`Social Media` == 1)/sum(length(`Social Media`)), 2),
    "Newspaper" = round(sum(Newspaper == 1)/sum(length(Newspaper)), 2),
    "Magazine" = round(sum(Magazine == 1)/sum(length(Magazine)), 2),
    "Television" = round(sum(Television == 1)/sum(length(Television)), 2),
    "TripAdvisor" = round(sum(TripAdvisor == 1)/sum(length(TripAdvisor)), 2),
    "Advice Agent" = round(sum(`Advice Agent` == 1)/sum(length(`Advice Agent`)), 2),
    "Blog" = round(sum(Blog == 1)/sum(length(Blog)), 2),
    "Search Engine" = round(sum(`Search Engine` == 1)/sum(length(`Search Engine`)), 2),
    "Recommendation" = round(sum(Recommendation == 1)/sum(length(Recommendation)), 2),
  )

pretrip.source.precentage <- pretrip.source %>%
  dplyr::summarize(
    "Online Advertising" = round(sum(`Online Advertising` == 1)/sum(length(`Online Advertising`)), 2),
    "Social Media" = round(sum(`Social Media` == 1)/sum(length(`Social Media`)), 2),
    "Newspaper" = round(sum(Newspaper == 1)/sum(length(Newspaper)), 2),
    "Magazine" = round(sum(Magazine == 1)/sum(length(Magazine)), 2),
    "Television" = round(sum(Television == 1)/sum(length(Television)), 2),
    "TripAdvisor" = round(sum(TripAdvisor == 1)/sum(length(TripAdvisor)), 2),
    "Advice Agent" = round(sum(`Advice Agent` == 1)/sum(length(`Advice Agent`)), 2),
    "Blog" = round(sum(Blog == 1)/sum(length(Blog)), 2),
    "Search Engine" = round(sum(`Search Engine` == 1)/sum(length(`Search Engine`)), 2),
    "Recommendation" = round(sum(Recommendation == 1)/sum(length(Recommendation)), 2),
  )
pretrip.source.sum <- pretrip.source %>%
  dplyr::summarize(
    "Online Advertising" = sum(`Online Advertising` == 1),
    "Social Media" = sum(`Social Media` == 1),
    "Newspaper" = sum(Newspaper == 1),
    "Magazine" = sum(Magazine == 1),
    "Television" = sum(Television == 1),
    "TripAdvisor" = sum(TripAdvisor == 1),
    "Advice Agent" = sum(`Advice Agent` == 1),
    "Blog" = sum(Blog == 1),
    "Search Engine" = sum(`Search Engine` == 1),
    "Recommendation" = sum(Recommendation == 1),
  )

#visualize pretrip source by age category
ggplot(data = pretrip.percentage.by.age.category, mapping = aes(x = reorder(category, `Online Advertising`), y = `Online Advertising`)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Pre-trip Source Information by Age Category",
       x = "Age Category",
       y = "Percentage") +
  theme_minimal()


# visualize category mean
ggplot(data = country.mean, mapping = aes(x = reorder(p_country, average), y = average)) + 
    geom_bar(stat = "identity") + 
  labs(title = "Internet Daily Usage By Country",
       x = "Country",
       y = "Average Intenet Usage") +
  coord_flip() +
  theme_minimal()

# visualize category mean
ggplot(data = category.mean, mapping = aes(x = category, y = average)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Internet Daily Usage By Age Category",
       x = "Age Category",
       y = "Average Intenet Usage") +
  coord_flip() +
  theme_minimal()





