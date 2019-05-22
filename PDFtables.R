# Converts PDF tables to a usable format

#Libraries
library('tidyverse')
library('tabulizer')

#Load PDF to extract tables from
#download.file("https://github.com/jacksonmluckey/OhioOpioid/raw/master/2017_OhioDrugOverdoseReport.pdf",
#              "OhioOpioid2017PDF")

# Load all tables from PDF
raw_tables <- extract_tables("2017_OhioDrugOverdoseReport.pdf")

unintentional_overdose_county_year <- function(table) {
  # Extracting "Table 3. Number of Unintentional Drug Overdose Deaths and Average Crude and Age-Adjusted Annual Death Rates Per 100,000 Population, by County, 2005-2017"
  table <- table %>%
    map(as_data_frame) %>%
    bind_rows() %>%
    mutate(V18 = NULL)
  
  # Hacky solution to the uncallable blank column names problem
  table[1,16] <- "TotalToMerge"
  table[1,19] <- "AARtoMerge"
  
  # Change first row into column names
  colnames(table) <- as.character(table[1,])
  table <- table[-1,]
  
  # Drop unnecessary columns, combine the split ones, and rename year columns
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