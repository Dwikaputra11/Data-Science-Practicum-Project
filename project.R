library(readxl)
library(tidyverse)
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)

file <- read_excel(path = here("data-raw/Smartphone_usage_and_Intenational_Tourist_Behavior.xlsx"))
head(file)

# delete na valou from age
class(file)
filter.age <- file %>% as.data.frame() %>% drop_na(p_age)

#Add New Column For Categories
add.category <- filter.age %>% mutate(Category = case_when(
  p_age > 0 & p_age <= 20 ~ "0 - 20",
  p_age > 20 & p_age <= 30 ~ "21 - 30",
  p_age > 30 & p_age <= 50 ~ "31 - 50",
  p_age > 50 & p_age <= 85 ~ "51 - 85",
))

country.mean.internet.usage <- add.category %>% select(p_country, internet_daily_usage)
category.mean.internet.usage <- add.category %>% select(Category, internet_daily_usage)

country.mean <- country.mean.internet.usage %>% group_by(p_country) %>% summarize(average = round(mean(internet_daily_usage), 2)) 
category.mean <- category.mean.internet.usage %>% group_by(Category) %>% summarize(average = round(mean(internet_daily_usage), 2))


ggplot(data = country.mean, mapping = aes(x = reorder(p_country, average), y = average)) + 
    geom_bar(stat = "identity") + 
  labs(title = "Internet Daily Usage By Country",
       x = "Country",
       y = "Average Intenet Usage") +
  coord_flip() +
  theme_minimal()


ggplot(data = category.mean, mapping = aes(x = reorder(Category, average), y = average)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Internet Daily Usage By Age Category",
       x = "Age Category",
       y = "Average Intenet Usage") +
  coord_flip() +
  theme_minimal()
