# Converts PDF tables to a usable format

#Libraries
library('tidyverse')
library('tabulizer')

#Load PDF to extract tables from
#download.file("https://github.com/jacksonmluckey/OhioOpioid/raw/master/2017_OhioDrugOverdoseReport.pdf",
#              "OhioOpioid2017PDF")

# Load all tables from PDF
raw_tables <- extract_tables("2017_OhioDrugOverdoseReport.pdf")

# Extracting "Table 3. Number of Unintentional Drug Overdose Deaths and Average Crude and Age-Adjusted Annual Death Rates Per 100,000 Population, by County, 2005-2017"
table7 <- raw_tables[7] %>%
  map(as_data_frame) %>%
  bind_rows() %>%
  mutate(V18 = NULL)

# Change first row into column names
colnames(table7) <- as.character(table7[1,])
table7 <- table7[-1,]

# Drop unnecessary columns (for now because I kept on messing it up otherwise)

# Requires combining raw_tables[7,8,9]

