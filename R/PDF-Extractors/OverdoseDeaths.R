# Converts PDF tables to a usable format

#Libraries
library('tidyverse')
library('tabulizer')
library('readxl')
library('here')

# Load all tables from PDF
raw_tables <- extract_tables(here("Inputs", "PDFs", "2017_OhioDrugOverdoseReport.pdf"))

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

# Combine the tables
table <- table1 %>%
  full_join(table2) %>%
  full_join(table3)

# Remove the now unnecessary tables from workspace
rm(table1, table2, table3)
rm(raw_tables)

# Change variables to correct type and resolve *s
table <- table %>%
  mutate_at(vars(starts_with("Year")), list( ~as.integer(gsub(",", "", ., fixed = TRUE)))) %>% # turns 2007 into Year2007 and so on while also resolving the instances where a "," in the value would NA instead of switching to int
  mutate(
    Total2012to2017 = as.integer(gsub(",", "", Total2012to2017, fixed = TRUE)),
    CrudeRate = as.numeric(na_if(CrudeRate, "*")), # removes the * from counties without enough data for all stats
    AgeAdjustedRate = as.numeric(na_if(AgeAdjustedRate, "*")), # same as above
    County = gsub("*", "", County, fixed = TRUE) # same as above but for county names
  )

# Save under a less bad name
overdose_deaths_wide <- table
rm(table)

# Save the overdose_deaths_wide in rda and csv format
save(overdose_deaths_wide, file = here("Data", "RData", "OverdoseDeathsWide.rda"))
write_csv(overdose_deaths_wide, path = here("Data", "CSV", "OverdoseDeathsWide.csv"))

# Create a tall version of overdose_death_rates_wide
overdose_deaths_tall <- overdose_deaths_wide %>%
  select(-c(Total2012to2017, CrudeRate,AgeAdjustedRate)) %>%
  gather(year, deaths, Year2005:Year2017) %>%
  mutate(year = stringr::str_match(year, "([0-9]{4})")[2])

# Save overdose_deaths_tall in rda and csv format
save(overdose_deaths_tall, file = here("Data", "RData", "OverdoseDeathsTall.rda"))
write_csv(overdose_deaths_tall, path = here("Data", "CSV", "OverdoseDeathsTall.csv"))