library(tidyverse)
library(here)
library(tabulizer)

# This script will pull all relevant tables out of "OhioCountyPoverty.pdf"

# Load the PDF file and extract every table from the detailed tables section
pdf_path <- here("Data", "PDFs", "OhioCountyPoverty.pdf")
raw_tables <- extract_tables(pdf_path, pages = 58:95)

# Table A1: Number and Percent of Poor Persons in Ohio and the U.S., 1959, 1969-2017 (in Thousands, Except for Percentages)
PoorPersons <- as.tibble(raw_tables[[1]])