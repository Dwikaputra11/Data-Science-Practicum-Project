library(readxl)
library(tidyverse)
library(here)
library(dplyr)

file <- read_excel(path = here("data-raw/Smartphone_usage_and_Intenational_Tourist_Behavior.xlsx"))
head(file)


