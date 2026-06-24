# ------------------- Load necessary libraries ------------------- #
library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)
library(tidyr)

con <- dbConnect(RSQLite::SQLite(), "C:/Users/Terri/Documents/github_projects/ILRHA_Points_2025/DB/ilrha2025v0428.db")

dup_check <- dbGetQuery(con, "
  SELECT
    h.arha_no,
    COUNT(DISTINCT h.horse_name) AS name_count,
    GROUP_CONCAT(h.horse_name, ', ') AS names
  FROM (
    SELECT DISTINCT arha_no, horse_name
    FROM horses
  ) AS h
  GROUP BY h.arha_no
  HAVING name_count > 1
")

dup_check

dbExecute(con, "
  UPDATE horses
  SET horse_name = 'Play the Chick'
  WHERE LOWER(horse_name) IN ('play the chic', 'play the chick')
")

dbExecute(con, "
  UPDATE horses
  SET horse_name = 'Shiny Gypsy Outlaw'
  WHERE LOWER(horse_name) IN ('gypsy shiny outlaw', 'shiny gypsy outlaw')
")

dbExecute(con, "
  UPDATE horses
  SET horse_name = 'Goldun Zipper'
  WHERE LOWER(horse_name) IN ('golden zipper', 'goldun zipper')
")

dbGetQuery(con, "SELECT DISTINCT horse_name FROM horses ORDER BY horse_name")

dbGetQuery(con, "
  SELECT arha_no, horse_name
  FROM horses
  WHERE LOWER(horse_name) LIKE '%gypsy%'
     OR LOWER(horse_name) LIKE '%chick%'
     OR LOWER(horse_name) LIKE '%zipper%'
  ORDER BY horse_name
")

dup_detail <- dbGetQuery(con, "
  SELECT
    h.arha_no,
    h.horse_name,
    h.owner_arha_no,
    o.name AS owner_name,
    COUNT(r.result_id) AS result_count
  FROM horses h
  LEFT JOIN people o ON h.owner_arha_no = o.arha_no
  LEFT JOIN results r ON r.horse_arha_no = h.arha_no
  WHERE LOWER(h.horse_name) IN ('play the chick', 'goldun zipper', 'shiny gypsy outlaw')
  GROUP BY h.arha_no, h.horse_name, h.owner_arha_no, o.name
  ORDER BY h.horse_name, h.arha_no
")

dup_detail

# Goldun Zipper
dbExecute(con, "
  UPDATE results
  SET horse_arha_no = 9500045
  WHERE horse_arha_no = 9500223
")

# Play the Chick
dbExecute(con, "
  UPDATE results
  SET horse_arha_no = 9500145
  WHERE horse_arha_no = 9500003
")

# Shiny Gypsy Outlaw
dbExecute(con, "
  UPDATE results
  SET horse_arha_no = 9500206
  WHERE horse_arha_no = 9500152
")

dbExecute(con, "
  DELETE FROM horses
  WHERE arha_no IN (9500223, 9500003, 9500152)
")

