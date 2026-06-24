# Load necessary libraries
library(DBI)
library(RSQLite)
library(dplyr)

# Connect to your SQLite database
con <- dbConnect(SQLite(), "db/morha2025.db")

# Load results table and join exhibitor and horse names
results_df <- dbGetQuery(con, "
  SELECT
    r.*,
    p.name AS exhibitor_name,
    h.horse_name
  FROM results r
  LEFT JOIN people p ON r.exhibitor_arha_no = p.arha_no
  LEFT JOIN horses h ON r.horse_arha_no = h.arha_no
")

# Preview
glimpse(results_df)
