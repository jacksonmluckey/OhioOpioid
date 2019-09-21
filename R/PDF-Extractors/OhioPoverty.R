library(tidyverse)
library(here)
library(tabulizer)

# This script will pull all relevant tables out of "OhioCountyPoverty.pdf"

# Load the PDF file and extract every table from it
pdf_path <- here("Data", "PDFs", "OhioCountyPoverty.pdf")
raw_tables <- extract_tables(pdf_path)