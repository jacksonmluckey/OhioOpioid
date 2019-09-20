library(tidyverse)
library(here)
library(readxl)

# Load the Excel file containing timeseries data on Ohio county population
pop_1960 <- read_excel(here("Data", "Excel", "OhioCountyPopulationYear.xls"), sheet = 1)
pop_2000 <- read_excel(here("Data", "Excel", "OhioCountyPopulationYear.xls"), sheet = 2)

# Remove leftover formatting rows/columns
pop_1960 <- pop_1960[5:93, 2:43]
pop_2000 <- pop_2000[7:94,2:13]

# Rename columns
names(pop_2000) <- c("County", 2000:2010)
names(pop_1960) <- c("County", 1960:2000)

# Combine the two population dataframes
PopWide <- full_join(pop_1960, pop_2000)

# Save PopWide as CSV
write_csv(PopWide, here("Data", "CSV", "OhioCountyPopulationYearWide.csv"))

# Create tall version of PopWide
PopTall <- PopWide %>%
  gather(Year, Population, -County)

# Save PopTall as CSV
write_csv(PopTall, here("Data", "CSV", "OhioCountyPopulationYearTall.csv"))