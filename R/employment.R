#Libraries
library('tidyverse')
library('readxl')
library('here')

# Load employment data
employment <- read_excel(here("Inputs", "Excel", "Unemployment.xls"))

# Change relevant row into variable names and remove "formatting" rows
colnames(employment) <- as.character(employment[7,])
employment <- employment[-1,]
employment <- employment %>%
  drop_na()
employment <- employment %>%
  filter(State == "OH") %>%
  mutate(Area_name = gsub("County, OH", "", Area_name)) # Removes things that would prevent a proper join

# Change variables into correct format
employment <- employment %>%
  mutate_at(vars(matches("20")), as.numeric) %>%
  mutate_at(vars(matches("_code")), as.factor) %>%
  mutate_at(vars(matches("Metro")), as.factor)

# Function for selecting a particular year's employment data and renaming it to enable joins
employment_year <- function(df, year) { #using year will necessitate metaprograming
  year = as.character(year)
  pattern = paste0("_", year)
  df %>%
    select(ends_with(year), Area_name) %>%
    rename(County = Area_name) %>%
    rename_all(list(~ str_replace(., pattern, ""))) %>%
    mutate(County = str_trim(County))
}

# Generate an employment dataframe for each year
employment_container = list()
for (i in 2007:2017) {
  employment_container[[(i - 2006)]] = list(i, employment_year(employment, i))
  #assign(paste0("employment_", i), employment_year(employment, i))
}

# Save dataframes
save(list = ls(pattern = "employment"), file = here("RData", "Employment.rda"))