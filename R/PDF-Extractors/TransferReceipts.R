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
col_names <- c("Area", "TotalTransferReceipts2012", "TotalTransferReceipts2013", "TotalTransferReceipts2014",
               "TotalTransferReceipts2015", "TotalTransferReceipts2016", "TotalTransferReceipts2017",
               "TotalIncome", "PercentTotalIncome", "DependencyRank")
names(TransferReceipts) <- col_names

# Remove erroneous rows that come from the poor formatting of the pdf
TransferReceipts <- TransferReceipts[5:nrow(TransferReceipts),] %>%
  filter(! Area == "")

# Convert columns to numeric
TransferReceipts <- TransferReceipts %>%
  mutate_at(col_names[2:length(col_names)], parse_number)

# Save wide version of TransferReceipts to CSV
TransferReceiptsWide <- TransferReceipts
write_csv(TransferReceiptsWide, here("Data", "CSV", "TransferReceiptsWide.csv"))

# Create tall version of TransferReceipts
TransferReceiptsTall <- TransferReceipts %>%
  gather(key = "Year", value = "TotalTransferReceipts", -c("Area", "TotalIncome", "PercentTotalIncome", "DependencyRank")) %>%
  mutate(Year = parse_number(Year)) # Removes "TotalTransferReceipts" from the Year column