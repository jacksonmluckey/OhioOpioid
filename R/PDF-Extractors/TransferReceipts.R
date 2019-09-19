library(tabulizer)
library(tidyverse)
library(here)

# Extracts the TransferReceipts table from OhioCountyIndicators.pdf
raw_pdf_extraction <- extract_tables(here("Data", "PDFs", "OhioCountyIndicators.pdf"), pages = 76:77, output = "data.frame")
# Without further modification this produces 2 lists

# Combine the two dataframes
TransferReceipts <- bind_rows(raw_pdf_extraction[1], raw_pdf_extraction[2])

# Split the accidentally combined 2014 2015 column

# Rename the columns
names(TransferReceipts) <- c("Area", "TotalTransferReceipts2012")