library(tidyverse)
library(here)
library(tabulizer)

# This script will pull all relevant tables out of "OhioCountyPoverty.pdf"

# Load the PDF file and extract every table from the detailed tables section
pdf_path <- here("Data", "PDFs", "OhioCountyPoverty.pdf")
raw_tables <- extract_tables(pdf_path, pages = 58:95)


##############################################################################################################################
# Function that extracts:                                                                                                    #
# Table A1: Number and Percent of Poor Persons in Ohio and the U.S., 1959, 1969-2017 (in Thousands, Except for Percentages)  #
# Table A2: Number and Percent of Poor Families in Ohio and the U.S., 1959, 1969-2017 (in Thousands, Except for Percentages) #
##############################################################################################################################
ExtractPoorDemographicsTable <- function(FileName, TableNumber) {
  PoorUnits <- as.tibble(raw_tables[[2]])
  # Remove bad formatting rows
  PoorUnits <- PoorUnits[4:nrow(PoorUnits),]
  # Split PoorUnits in half (because it has the rows duplicated to fit all years on one page)
  PoorUnits1 <- PoorUnits[,1:7]
  PoorUnits2 <- PoorUnits[,8:14]
  # Rename the columns
  PoorUnitsColNames <- c("Year", "OhioTotal", "OhioPoorTotal", "OhioPercentPoor", "USTotal", "USPoorTotal", "USPercentPoor")
  names(PoorUnits1) <- PoorUnitsColNames
  names(PoorUnits2) <- PoorUnitsColNames
  # Recombine the tibbles
  PoorUnits <- bind_rows(PoorUnits1, PoorUnits2)
  # Remove the leftovers
  rm(PoorUnits1)
  rm(PoorUnits2)
  # Remove extra characters (eg #^$, ) from the numerical columns
  PoorUnits <- PoorUnits %>%
    mutate_all(parse_number)
  # Remove pointless rows (those without a year)
  PoorUnits <- PoorUnits %>%
    filter(! is.na(Year))
  ######################################################
  # Reshape that tibble so that US/Ohio becomes a column
  # Do this by splitting PoorUnits into 2 tibbles
  # Renaming their columns and reordering them
  # And then recombining the tibbles
  #####################################################
  PoorUnitsUS <- PoorUnits[,5:7]
  PoorUnitsUS$Year <- c(1959, 1969:2016)
  PoorUnitsUS$Area <- "US"
  PoorUnitsOhio <- PoorUnits[,1:4]
  PoorUnitsOhio$Area <- "Ohio"
  # Reorder the PoorUnitsOhio columns so that the names and values will line up after applying the tidy names
  PoorUnitsOhio <- PoorUnitsOhio %>%
    select(OhioTotal, OhioPoorTotal, OhioPercentPoor, Year, Area)
  TidyPoorUnitsColNames <- c("Total", "Poor", "PercentPoor", "Year", "Area")
  names(PoorUnitsOhio) <- TidyPoorUnitsColNames
  names(PoorUnitsUS) <- TidyPoorUnitsColNames
  PoorUnits <- bind_rows(PoorUnitsOhio, PoorUnitsUS)
  # Remove the leftovers
  rm(PoorUnitsOhio)
  rm(PoorUnitsUS)
  # Write to CSV
  write_csv(PoorUnits, path = here("Data", "CSV", FileName))
  # Clean up my global environment
  rm(PoorUnits)
  rm(PoorUnitsColNames)
  rm (TidyPoorUnitsColNames)
}
# Actually run the function
ExtractPoorDemographicsTable("PoorPersons.csv", 1)
ExtractPoorDemographicsTable("PoorFamilies.csv", 2)