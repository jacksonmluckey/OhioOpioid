library(tabulizer)
library(tidyverse)
library(here)

# Extracts the TransferReceipts table from OhioCountyIndicators.pdf
raw_pdf_extraction <- extract_tables(here("Data", "PDFs", "OhioCountyIndicators.pdf"), pages = 76:77, output = "data.frame")
# Without further modification this produces 2 lists

# Combine the two dataframes
TransferReceipts <- bind_rows(raw_pdf_extraction[1], raw_pdf_extraction[2])

# Split the accidentally combined 2014 2015 column
TransferReceipts <- TransferReceipts %>%
  separate(X.3, c("TotalTransferReceipts2014", "TotalTransferReceipts2015"), sep = " ")

# Rename the columns
names(TransferReceipts) <- c("Area", "TotalTransferReceipts2012", "TotalTransferReceipts2013", "TotalTransferReceipts2014",
                             "TotalTransferReceipts2015", "TotalTransferReceipts2016", "TotalTransferReceipts2017",
                             "TotalIncome", "PercentTotalIncome", "DependencyRank")

# Remove erroneous rows that come from the poor formatting of the pdf
TransferReceipts <- TransferReceipts[5:nrow(TransferReceipts),] %>%
  filter(! Area == "")