library(readxl)
library(here)

file <- read_excel(path = here("Project DS/data-raw", "Smartphone_usage_and_Intenational_Tourist_Behavior.xlsx"))
head(file)

