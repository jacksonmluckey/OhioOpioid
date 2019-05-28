#Libraries
library('tidyverse')
library('readxl')

# Load employment data
employment <- read_excel("./Inputs/Excel/Unemployment.xls")

# Change relevant row into variable names and remove "formatting" rows
colnames(employment) <- as.character(employment[7,])
employment <- employment[-1,]
employment <- employment %>%
  drop_na()
employment <- employment %>%
  filter(State == "OH") %>%
  mutate(Area_name = gsub("County, OH", "", Area_name)) # Removes things that would prevent a proper join

# Function for selecting a particular year's employment data and renaming it to enable joins
employment_year <- function(df, year) { #using year will necessitate metaprograming
  year = as.character(year)
  pattern = paste0("_", year)
  print(pattern)
  df %>%
    select(ends_with(year), Area_name) %>%
    rename(County = Area_name) %>%
    rename_all(list(~ str_replace(., pattern, "")))
}

# Generate an employment dataframe for each year
for (i in 2007:2017) {
  assign(paste0("employment_", i), employment_year(employment, i))
}