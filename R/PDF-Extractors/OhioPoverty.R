library(tidyverse)
library(here)
library(tabulizer)

# This script will pull all relevant tables out of "OhioCountyPoverty.pdf"

# Load the PDF file and extract every table from the detailed tables section
pdf_path <- here("Data", "PDFs", "OhioCountyPoverty.pdf")
raw_tables <- extract_tables(pdf_path, pages = 58:95)


#############################################################################################################################
# Table A1: Number and Percent of Poor Persons in Ohio and the U.S., 1959, 1969-2017 (in Thousands, Except for Percentages) #
#############################################################################################################################
PoorPersons <- as.tibble(raw_tables[[1]])
# Remove bad formatting rows
PoorPersons <- PoorPersons[4:nrow(PoorPersons),]
# Split PoorPersons in half (because it has the rows duplicated to fit all years on one page)
PoorPersons1 <- PoorPersons[,1:7]
PoorPersons2 <- PoorPersons[,8:14]
# Rename the columns
PoorPersonsColNames <- c("Year", "OhioTotal", "OhioPoorTotal", "OhioPercentPoor", "USTotal", "USPoorTotal", "USPercentPoor")
names(PoorPersons1) <- PoorPersonsColNames
names(PoorPersons2) <- PoorPersonsColNames
# Recombine the tibbles
PoorPersons <- bind_rows(PoorPersons1, PoorPersons2)
# Remove the leftovers
rm(PoorPersons1)
rm(PoorPersons2)
# Remove extra characters (eg #^$, ) from the numerical columns
PoorPersons <- PoorPersons %>%
  mutate_all(parse_number)
# Remove pointless rows (those without a year)
PoorPersons <- PoorPersons %>%
  filter(! is.na(Year))
######################################################
# Reshape that tibble so that US/Ohio becomes a column
# DO this by splitting PoorPersons into 2 tibbles
# Renaming their columns and reordering them
# And then recombining the tibbles
#####################################################
PoorPersonsUS <- PoorPersons[,5:7]
PoorPersonsUS$Year <- c(1959, 1969:2016)
PoorPersonsUS$Area <- "US"
PoorPersonsOhio <- PoorPersons[,1:4]
PoorPersonsOhio$Area <- "Ohio"
# Reorder the PoorPersonsOhio columns so that the names and values will line up after applying the tidy names
PoorPersonsOhio <- PoorPersonsOhio %>%
  select(OhioTotal, OhioPoorTotal, OhioPercentPoor, Year, Area)
TidyPoorPersonsColNames <- c("Total", "Poor", "PercentPoor", "Year", "Area")
names(PoorPersonsOhio) <- TidyPoorPersonsColNames
names(PoorPersonsUS) <- TidyPoorPersonsColNames
PoorPersons <- bind_rows(PoorPersonsOhio, PoorPersonsUS)
# Remove the leftovers
rm(PoorPersonsOhio)
rm(PoorPersonsUS)
# Write to CSV
write_csv(PoorPersons, path = here("Data", "CSV", "PoorPersons.csv"))
# Clean up my global environment
rm(PoorPersons)
rm(PoorPersonsColNames)
rm (TidyPoorPersonsColNames)