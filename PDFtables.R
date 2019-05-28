# Converts PDF tables to a usable format

#Libraries
library('tidyverse')
library('tabulizer')
library('readxl')

# Load all tables from PDF
raw_tables <- extract_tables("Inputs/PDFs/2017_OhioDrugOverdoseReport.pdf")

unintentional_overdose_county_year <- function(table) {
  # Extracting "Table 3. Number of Unintentional Drug Overdose Deaths and Average Crude and Age-Adjusted Annual Death Rates Per 100,000 Population, by County, 2005-2017"
  table <- table %>%
    map(as_data_frame) %>%
    bind_rows() %>%
    mutate(V18 = NULL)
  
  # Hacky solution to the uncallable blank variable names problem
  table[1,16] <- "TotalToMerge"
  table[1,19] <- "AARtoMerge"
  
  # Change first row into variable names
  colnames(table) <- as.character(table[1,])
  table <- table[-1,]
  
  # Drop unnecessary variables, combine the split ones, and rename year columns
  table <- table %>%
    mutate("Age Adjusted Rate" = NULL) %>%
    rename("AgeAdjustedRate" = "AARtoMerge") %>%
    rename("CrudeRate" = "Crude Rate") %>%
    unite("Total2012to2017", "2012-2017 Total", "TotalToMerge", sep = "") %>%
    rename_at(vars(starts_with("2")), list( ~str_replace(., "2", "Year2")))
}

# Generate tables for the 3 pages that make up the unintentional overdose data
table1 <- unintentional_overdose_county_year(raw_tables[7])
table2 <- unintentional_overdose_county_year(raw_tables[8])
table3 <- unintentional_overdose_county_year(raw_tables[9])

# COmbine the tables
table <- table1 %>%
  full_join(table2) %>%
  full_join(table3)

# Change variables to correct type and resolve *s
table <- table %>%
  mutate_at(vars(starts_with("Year")), list( ~as.integer(gsub(",", "", ., fixed = TRUE)))) %>% # turns 2007 into Year2007 and so on while also resolving the instances where a "," in the value would NA instead of switching to int
  mutate(
    Total2012to2017 = as.integer(gsub(",", "", Total2012to2017, fixed = TRUE)),
    CrudeRate = as.numeric(na_if(CrudeRate, "*")), # removes the * from counties without enough data for all stats
    AgeAdjustedRate = as.numeric(na_if(AgeAdjustedRate, "*")), # same as above
    County = gsub("*", "", County, fixed = TRUE) # same as above but for county names
  )

# Load employment data
employment <- read_excel("Inputs/Data/Unemployment.xls")

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

for (i in 2007:2017) {
  assign(paste0("employment_", i), employment_year(employment, i))
}