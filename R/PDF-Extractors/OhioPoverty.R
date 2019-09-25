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
# Pages 58 and 59 of PDF
# Pages 52 and 53 of documentS
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


###########################################################################################
# Table Table A4: Number and Percentage of Poor Persons by Ohio County for Selected Years #
# Pages 61-63 of PDF
# Pages 55-57 of document                                                 #
###########################################################################################
Page1 <- raw_tables[[4]]
Page2 <- raw_tables[[5]]
Page3 <- raw_tables[[6]]
# Functiont that takes a dataframe (1 page of the table) and returns a tidy table
ExtractNumPercentPoorCountyTable <- function(df) {
  # Turn df into a tibble
  df <- as_tibble(df)
  # Remove pointless rows at the top
  df <- df[7:nrow(df),]
  # Remove empty columns
  df <- df[-c(5,8)]
  # Seperate the dataframe into small dataframes to prepare for reshaping/tidying
  area <- df[,1]
  df1 <- df[,2:3]
  df2 <- df[,4:5]
  df3 <- df[,6:7]
  # Function for splitting the N/NumPoor column, appending the area, and fixing the col names
  SplitNumPoorN_AddArea_FixNames <- function(df, area) {
    # Names of the variables
    col_names <- c("N", "NumPoor", "PercentPoor", "Area")
    df <- df %>%
      separate(1, c("N", "NumPoor"), sep = " ")
    df <- bind_cols(df, area)
    names(df) <- col_names
    return(df)
  }
  # Actually split the N/NumPoor column and add the area
  df1 <- SplitNumPoorN_AddArea_FixNames(df1, area)
  df2 <- SplitNumPoorN_AddArea_FixNames(df2, area)
  df3 <- SplitNumPoorN_AddArea_FixNames(df3, area)
  # Add year column to each df (done manually because I am bad)
  # I averaged out the split years (e.g. 2007-2011 becomes 2009)
  df1$Year <- 2015
  df2$Year <- 2009
  df3$Year <- 1999
  # Combine the rows
  df <- bind_rows(df1, df2, df3)
  # Mutate numericals to, well, numericals
  df <- df %>%
    mutate_at(c("N", "NumPoor", "PercentPoor"), parse_number)
  # Create Appalachian boolean from Area asterisk
  df <- df %>%
    mutate(Appalachian = str_detect(Area, fixed("*")))
  # Remove asterisk from Area column
  df <- df %>%
    mutate(Area = str_replace(Area, fixed("*"), ""))
}