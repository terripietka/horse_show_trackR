<<<<<<< HEAD
# ============================================================
#               MoRHA 2025 Standings Processing
# ============================================================

=======
# ------------------- Load necessary libraries ------------------- #
>>>>>>> d070d812b221131bd76768ae6615151486d39468
library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)
library(tidyr)

<<<<<<< HEAD
# ------------------- Connect to database ------------------- #
con <- dbConnect(
  SQLite(),
  "C:/Users/Terri/Documents/github_projects/MoRHA_2025/DB/morha_120125.db"
)

=======
# ------------------- Connect to database and load results ------------------- #
# Connect to the SQLite database
con <- dbConnect(SQLite(), "C:/Users/Terri/Documents/github_projects/ILRHA_Points_2025/DB/ilrha2025v0428.db")

# Load results table and join exhibitor names, horse names, show names, and judge names
>>>>>>> d070d812b221131bd76768ae6615151486d39468
results_df <- dbGetQuery(con, "
  SELECT
    r.*,
    p.name AS exhibitor_name,
    h.horse_name,
    h.owner_arha_no,
    o.name AS owner_name,
    s.show_name,
    j.judge_name
  FROM results r
  LEFT JOIN people p   ON r.exhibitor_arha_no = p.arha_no
  LEFT JOIN horses h   ON r.horse_arha_no    = h.arha_no
  LEFT JOIN people o   ON h.owner_arha_no    = o.arha_no
  LEFT JOIN shows s    ON r.show_id          = s.show_id
  LEFT JOIN judges j   ON r.judge_id         = j.judge_id
")

<<<<<<< HEAD
dbDisconnect(con)

# ============================================================
#       CLEAN UP SHOW NAMES / CLASS NAMES / DROP W/W, JACKPOT
# ============================================================

results_df <- results_df %>%
  mutate(
    show_name = str_trim(show_name),
    show_name = str_to_title(show_name),
    show_name = recode(
      show_name,
      "Aug"  = "August",
      "Nov"  = "November",
      "Nov." = "November"
    ),
    class_name = str_replace_all(class_name, "\\.", ""),
    class_name = str_squish(class_name)
  ) %>%
  filter(!class_name %in% c("MoRHA Walk/Whoa", "MoRHA Jackpot Walk/Trot"))

# ============================================================
#      DETERMINE EXHIBITOR’S “REAL DIVISION” (Nov vs Ama/Yth)
# ============================================================

exhibitor_division_lookup <- results_df %>%
  group_by(exhibitor_arha_no) %>%
  summarise(
    has_NA  = any(str_starts(class_name, "NA ")),
    has_AMA = any(str_starts(class_name, "AMA ")),
    has_NY  = any(str_starts(class_name, "NY ")),
    has_YTH = any(str_starts(class_name, "YTH ")),
    .groups = "drop"
  ) %>%
  mutate(
    real_division = case_when(
      has_NA  ~ "Nov Ama",
      has_AMA ~ "Ama",
      has_NY  ~ "Nov Yth",
      has_YTH ~ "Yth",
      TRUE    ~ NA_character_
    )
  ) %>%
  select(exhibitor_arha_no, real_division)

# ============================================================
#            INITIAL DIVISION ASSIGNMENT (BASE RULES)
# ============================================================

results_df <- results_df %>%
  mutate(
    division = case_when(
      # ------------- Novice Walk/Trot ------------- #
      class_name %in% c(
        "MoRHA Novice W/T HMS",
        "MoRHA Novice W/T SMS",
        "MoRHA Novice W/T Trail",
        "MoRHA Novice Walk/Trot"
      ) ~ "Novice W/T",
      
      # ------------- 2/3 YO W/T → Junior Horse ----- #
      class_name == "MoRHA 2/3 Year Old Walk/Trot" ~ "Junior Horse",
      
      # ------------- Amateur (base) ---------------- #
      str_starts(class_name, "AMA ") ~ "Ama",
      
      # ------------- Novice Amateur ---------------- #
      str_starts(class_name, "NA ")  ~ "Nov Ama",
      
      # ------------- Novice Youth ------------------ #
      str_starts(class_name, "NY ")  ~ "Nov Yth",
      
      # ------------- Youth ------------------------- #
      str_starts(class_name, "YTH ") |
        str_starts(class_name, "Youth ") ~ "Yth",
      
      # ------------- Junior Horse ------------------ #
      str_starts(class_name, "Junior Horse ") ~ "Junior Horse",
      
      # ------------- Senior Horse ------------------ #
      str_starts(class_name, "Senior Horse ") ~ "Senior Horse",
      
      # ------------- Open age / conformation / misc #
      class_name %in% c(
        "Ranch Roping",
        "Working Cow Horse",
        "Working Ranch Horse",
        "Three & Over Geldings",
        "Three & Over Mares",
        "Three & Over Stallions",
        "Two & Under Geldings",
        "Two & Under Mares",
        "Two & Under Stallions",
        "Yearling IH Trail", 
        "Two Year Old Ranch Riding",
        "Two Year Old IH Trail"
      ) ~ "Open",
      
=======
# Quick preview
glimpse(results_df)

# Disconnect from the database
dbDisconnect(con)

# ------------------- Assign Divisions Based on Class Names ------------------- #
# Create a new 'division' column based on string matches in the class name
results_df <- results_df %>%
  mutate(
    division = case_when(
      str_detect(class_name, regex("GAG", ignore_case = TRUE)) ~ "GAG",
      str_detect(class_name, regex("WTRD", ignore_case = TRUE)) ~ "WTRD",
      str_detect(class_name, regex("Jr", ignore_case = TRUE)) ~ "Jr Horse",
      str_detect(class_name, regex("Sr", ignore_case = TRUE)) ~ "Sr Horse",
      str_detect(class_name, regex("^NA", ignore_case = TRUE)) ~ "Nov Ama",
      str_detect(class_name, regex("^AMA|Reserve AMA", ignore_case = TRUE)) ~ "Ama",
      str_detect(class_name, regex("NY", ignore_case = TRUE)) ~ "Nov Yth",
      str_detect(class_name, regex("YTH|Youth|Reserve Yth", ignore_case = TRUE)) ~ "Yth",
      str_detect(class_name, regex("Working Ranch Horse|Working Cow Horse|Ranch Roping|All Age Cutting", ignore_case = TRUE)) &
        !str_detect(class_name, regex("YTH|AMA|NA|NY", ignore_case = TRUE)) ~ "Open",
      str_detect(class_name, regex("^Two &|^Three &|^Working|^Ranch Roping|^Yearling|^Two|Reserve AA", ignore_case = TRUE)) ~ "Open",
>>>>>>> d070d812b221131bd76768ae6615151486d39468
      TRUE ~ NA_character_
    )
  )

<<<<<<< HEAD
# ============================================================
#     FIX: REASSIGN AMA/YTH HALTER INTO NOVICE DIVISIONS
# ============================================================

results_df <- results_df %>%
  left_join(exhibitor_division_lookup, by = "exhibitor_arha_no") %>%
  mutate(
    division = case_when(
      # --- Novice Amateur exhibitor using AMA halter --- #
      class_name %in% c(
        "AMA Two & Under Geldings", "AMA Three & Over Geldings",
        "AMA Two & Under Mares",    "AMA Three & Over Mares",
        "AMA Two & Under Stallions","AMA Three & Over Stallions"
      ) &
        real_division == "Nov Ama" ~ "Nov Ama",
      
      # --- Novice Youth exhibitor using YTH halter --- #
      class_name %in% c(
        "YTH Two & Under Geldings", "YTH Three & Over Geldings",
        "YTH Two & Under Mares",    "YTH Three & Over Mares",
        "YTH Two & Under Stallions","YTH Three & Over Stallions"
      ) &
        real_division == "Nov Yth" ~ "Nov Yth",
      
      TRUE ~ division
    )
  )

# ============================================================
#                CLASS POINT CALCULATION
# ============================================================

results_df <- results_df %>%
  filter(!str_detect(class_name, regex("Grand", ignore_case = TRUE))) %>%
  group_by(show_name, judge_id, class_name) %>%
  mutate(
    class_size = n_distinct(horse_arha_no),
    base_points = case_when(
      is.na(placing) ~ 0,
      class_size >= 10 & placing %in% 1:10 ~ 11 - placing,
      class_size < 10 & placing >= 1       ~ pmax(class_size - placing + 1, 0),
      TRUE ~ 0
    ),
    points = base_points          # no double-point shows
  ) %>%
  ungroup()

# ============================================================
#                SHOWS ENTERED CALCULATION
# ============================================================

horse_only_divisions <- c("Junior Horse", "Senior Horse", "Open")

results_df <- results_df %>%
  mutate(exhibitor_arha_no_chr = as.character(exhibitor_arha_no))

show_counts <- results_df %>%
  mutate(
    key_exhibitor = if_else(
      division %in% horse_only_divisions,
      NA_character_,
      exhibitor_arha_no_chr
    )
  ) %>%
  group_by(class_name, horse_arha_no, key_exhibitor) %>%
  summarise(shows_entered = n_distinct(show_id), .groups = "drop")

results_df <- results_df %>%
  mutate(
    key_exhibitor = if_else(
      division %in% horse_only_divisions,
      NA_character_,
      exhibitor_arha_no_chr
    )
  ) %>%
  left_join(show_counts, by = c("class_name", "horse_arha_no", "key_exhibitor")) %>%
  select(-key_exhibitor)

# ============================================================
#        SAVE CLASS STANDINGS (for app class display)
# ============================================================

saveRDS(
  results_df,
  "C:/Users/Terri/Documents/github_projects/MoRHA_2025/MoRHA_Standings_app/data/class_standings.rds"
)

# ============================================================
#                DIVISION SUMMARIES
# ============================================================

results_df_div <- results_df %>%
  filter(!is.na(division), division != "NA")

junior_results <- results_df_div %>% filter(division == "Junior Horse")
senior_results <- results_df_div %>% filter(division == "Senior Horse")
open_results   <- results_df_div %>% filter(division == "Open")

# ---- Identify Jr / Sr horses from Jr/Sr + 2YO etc. ---- #
junior_horses <- unique(junior_results$horse_arha_no)
senior_horses <- unique(senior_results$horse_arha_no)

two_year_horses <- results_df_div %>%
  filter(str_detect(class_name, regex("2/3 Year Old|Two & Under", ignore_case = TRUE))) %>%
  pull(horse_arha_no) %>%
  unique()

junior_horses <- unique(c(junior_horses, two_year_horses))
=======
# ------------------- Calculate Points ------------------- #
# Assign points based on class size and placing
results_df <- results_df %>%
  filter(!str_detect(class_name, regex("Grand", ignore_case = TRUE))) %>% 
  group_by(show_name, judge_id, class_name) %>%
  mutate(
    class_size = n_distinct(horse_arha_no),
    base_points = case_when(
      # str_detect(class_name, regex("Grand", ignore_case = TRUE)) & placing == 1 ~ 2,
      # str_detect(class_name, regex("Grand", ignore_case = TRUE)) & placing == 2 ~ 1,
      # str_detect(class_name, regex("Grand", ignore_case = TRUE)) ~ 0,
      is.na(placing) ~ 0,
      class_size >= 10 & placing >= 1 & placing <= 10 ~ 11 - placing,
      class_size < 10 & placing >= 1 ~ pmax(class_size - placing + 1, 0),
      TRUE ~ 0
    ),
    # Double points if the show is "April"
    points = if_else(show_name == "April", base_points * 2, base_points)
  ) %>%
  ungroup()

# Save full results dataframe
saveRDS(results_df, "C:/Users/Terri/Documents/github_projects/ILRHA_Points_2025/ILRHA_Standings_app/data/class_standings.rds")

# ------------------- Build Division Summaries ------------------- #

# 1. Split results into Junior, Senior, and Open
junior_results <- results_df %>% filter(division == "Jr Horse")
senior_results <- results_df %>% filter(division == "Sr Horse")
open_results   <- results_df %>% filter(division == "Open")

# 2. Identify Jr and Sr horses
junior_horses <- unique(junior_results$horse_arha_no)
senior_horses <- unique(senior_results$horse_arha_no)

# 3. Assign Open class points to Jr/Sr horses where applicable
open_for_juniors <- open_results %>% filter(horse_arha_no %in% junior_horses)
open_for_seniors <- open_results %>% filter(horse_arha_no %in% senior_horses)

# 4. Summarize Jr and Sr divisions (grouped by horse only)
junior_division_summary <- bind_rows(junior_results, open_for_juniors) %>%
  group_by(division = "Jr Horse", horse_arha_no) %>%
  summarise(total_points = sum(points, na.rm = TRUE), .groups = "drop")
>>>>>>> d070d812b221131bd76768ae6615151486d39468

# ---- All-age combined Jr/Sr working classes (Open) ---- #
open_combined <- open_results %>%
  filter(class_name %in% c("Ranch Roping", "Working Cow Horse", "Working Ranch Horse"))

<<<<<<< HEAD
open_for_juniors <- open_combined %>% filter(horse_arha_no %in% junior_horses)
open_for_seniors <- open_combined %>% filter(horse_arha_no %in% senior_horses)
=======
# 5. Attach horse names for Jr/Sr summaries
horse_names <- results_df %>% select(horse_arha_no, horse_name) %>% distinct()
>>>>>>> d070d812b221131bd76768ae6615151486d39468

# ---- OPEN Conformation: route to Jr/Sr using horse lists ---- #
open_halter_two_under <- open_results %>%
  filter(class_name %in% c(
    "Two & Under Geldings",
    "Two & Under Mares",
    "Two & Under Stallions"
  ))

open_halter_three_over <- open_results %>%
  filter(class_name %in% c(
    "Three & Over Geldings",
    "Three & Over Mares",
    "Three & Over Stallions"
  ))

open_halter_for_juniors <- bind_rows(
  open_halter_two_under,
  open_halter_three_over %>% filter(horse_arha_no %in% junior_horses)
)

open_halter_for_seniors <- open_halter_three_over %>%
  filter(horse_arha_no %in% senior_horses)

# ---- Junior Horse division summary (by horse) ---- #
junior_division_summary <- bind_rows(
  junior_results,
  open_for_juniors,
  open_halter_for_juniors
) %>%
  group_by(division = "Junior Horse", horse_arha_no) %>%
  summarise(
    total_points = sum(points, na.rm = TRUE),
    shows        = n_distinct(show_id),
    .groups      = "drop"
  ) %>%
  left_join(
    results_df_div %>% select(horse_arha_no, horse_name) %>% distinct(),
    by = "horse_arha_no"
  ) %>%
  mutate(exhibitor_name = NA_character_)

# ---- Senior Horse division summary (by horse) ---- #
senior_division_summary <- bind_rows(
  senior_results,
  open_for_seniors,
  open_halter_for_seniors
) %>%
  group_by(division = "Senior Horse", horse_arha_no) %>%
  summarise(
    total_points = sum(points, na.rm = TRUE),
    shows        = n_distinct(show_id),
    .groups      = "drop"
  ) %>%
  left_join(
    results_df_div %>% select(horse_arha_no, horse_name) %>% distinct(),
    by = "horse_arha_no"
  ) %>%
  mutate(exhibitor_name = NA_character_)

<<<<<<< HEAD
# ---- All other divisions (by pair) ---- #
other_divisions <- results_df_div %>%
  filter(!division %in% c("Junior Horse", "Senior Horse", "Open"),
         !is.na(points)) %>%
  group_by(division, horse_arha_no, horse_name, exhibitor_name) %>%
  summarise(
    total_points = sum(points, na.rm = TRUE),
    shows        = n_distinct(show_id),
    .groups      = "drop"
  ) %>%
  mutate(division_type = "by_pair")

=======
# 6. Handle Novice divisions (roll halter/conformation classes up correctly)
# conf_keywords <- c("Gelding", "Stallion", "Mare")
# is_conf <- grepl(paste(conf_keywords, collapse = "|"), results_df$class_name, ignore.case = TRUE)
# 
# results_df <- results_df %>%
#   mutate(
#     division_for_total = case_when(
#       division == "Ama" & is_conf & paste(horse_name, exhibitor_name) %in%
#         paste(results_df$horse_name[results_df$division == "Nov Ama"], results_df$exhibitor_name[results_df$division == "Nov Ama"]) ~ "Nov Ama",
#       division == "Yth" & is_conf & paste(horse_name, exhibitor_name) %in%
#         paste(results_df$horse_name[results_df$division == "Nov Yth"], results_df$exhibitor_name[results_df$division == "Nov Yth"]) ~ "Nov Yth",
#       TRUE ~ division
#     )
#   )
# 
# novice_divisions <- results_df %>%
#   # filter(division_for_total %in% c("Nov Ama", "Nov Yth"), !is.na(points)) %>%
#   group_by(division = division_for_total, horse_name, exhibitor_name) %>%
#   summarise(total_points = sum(points, na.rm = TRUE), .groups = "drop") %>%
#   mutate(division_type = "by_pair")

# 7. Summarize all other divisions
other_divisions <- results_df %>%
  filter(!is.na(points), !is.na(division)) %>%
  filter(!division %in% c("Jr Horse", "Sr Horse", "Open")) %>%
  group_by(division, horse_name, exhibitor_name) %>%
  summarise(total_points = sum(points, na.rm = TRUE), .groups = "drop") %>%
  mutate(horse_arha_no = NA, division_type = "by_pair")

# 8. Combine all division summaries
>>>>>>> d070d812b221131bd76768ae6615151486d39468
division_summary <- bind_rows(
  junior_division_summary %>% mutate(division_type = "by_horse"),
  senior_division_summary %>% mutate(division_type = "by_horse"),
  # novice_divisions,
  other_divisions
) %>%
<<<<<<< HEAD
  arrange(division, desc(total_points))

saveRDS(
  division_summary,
  "C:/Users/Terri/Documents/github_projects/MoRHA_2025/MoRHA_Standings_app/data/division_standings.rds"
)

# ============================================================
#              COW & PERFORMANCE SUMMARIES
# ============================================================

cow_keywords <- c("Boxing", "Cutting", "Herd", "Working", "Roping", "Cow Horse")

open_cow_results <- results_df_div %>%
  filter(division == "Open",
         str_detect(class_name,
                    regex(paste(cow_keywords, collapse = "|"), ignore_case = TRUE)))

junior_cow_results <- results_df_div %>%
  filter(division == "Junior Horse",
         str_detect(class_name,
                    regex(paste(cow_keywords, collapse = "|"), ignore_case = TRUE)))

senior_cow_results <- results_df_div %>%
  filter(division == "Senior Horse",
         str_detect(class_name,
                    regex(paste(cow_keywords, collapse = "|"), ignore_case = TRUE)))

open_for_juniors_cow <- open_cow_results %>%
  filter(horse_arha_no %in% junior_horses)

open_for_seniors_cow <- open_cow_results %>%
  filter(horse_arha_no %in% senior_horses)

junior_cow_summary <- bind_rows(junior_cow_results, open_for_juniors_cow) %>%
  group_by(horse_name) %>%
  summarise(
    total_points = sum(points, na.rm = TRUE),
    division     = "Junior Horse",
    .groups      = "drop"
  )

senior_cow_summary <- bind_rows(senior_cow_results, open_for_seniors_cow) %>%
  group_by(horse_name) %>%
  summarise(
    total_points = sum(points, na.rm = TRUE),
    division     = "Senior Horse",
    .groups      = "drop"
  )

other_cow_results <- results_df_div %>%
  filter(!division %in% c("Junior Horse", "Senior Horse", "Open"),
         str_detect(class_name,
                    regex(paste(cow_keywords, collapse = "|"), ignore_case = TRUE))) %>%
  group_by(division, horse_name, exhibitor_name) %>%
  summarise(total_points = sum(points, na.rm = TRUE), .groups = "drop")

cow_horse_summary <- bind_rows(
  junior_cow_summary,
  senior_cow_summary,
  other_cow_results
)

saveRDS(
  cow_horse_summary,
  "C:/Users/Terri/Documents/github_projects/MoRHA_2025/MoRHA_Standings_app/data/top_cow_horse.rds"
)

performance_keywords <- c(
  "Trail", "Riding", "Showmanship", "Ranchmanship",
  "Reining", "Horsemanship", "Horse Trail"
)

performance_results_raw <- results_df_div %>%
  filter(str_detect(
    class_name,
    regex(paste(performance_keywords, collapse = "|"), ignore_case = TRUE)
  ))

performance_results <- performance_results_raw %>%
  mutate(
    division_type = if_else(division %in% c("Junior Horse", "Senior Horse"),
                            "by_horse", "by_pair")
  ) %>%
  group_by(
    division,
    horse_name,
    exhibitor_name = if_else(
      division_type == "by_horse",
      NA_character_,
      exhibitor_name
    )
  ) %>%
  summarise(total_points = sum(points, na.rm = TRUE), .groups = "drop")

saveRDS(
  performance_results,
  "C:/Users/Terri/Documents/github_projects/MoRHA_2025/MoRHA_Standings_app/data/top_performance_horse.rds"
)
=======
  group_by(division, division_type, horse_name, exhibitor_name) %>%
  summarise(total_points = sum(total_points, na.rm = TRUE), .groups = "drop") %>%
  mutate(exhibitor_name = ifelse(division_type == "by_horse", NA_character_, exhibitor_name)) %>%
  arrange(division, desc(total_points))

# 9. Save division summary
saveRDS(division_summary, "C:/Users/Terri/Documents/github_projects/ILRHA_Points_2025/ILRHA_Standings_app/data/division_standings.rds")

# ------------------- Special Categories: Cow Horse and Performance Horse ------------------- #
>>>>>>> d070d812b221131bd76768ae6615151486d39468

# ---- Top Cow Horse Standings ---- #
# Identify cow horse classes
cow_keywords <- c("Boxing", "Cutting", "Herd", "Working", "Roping")

# Split into Open, Jr, Sr
open_cow_results <- results_df %>%
  filter(division == "Open", str_detect(class_name, regex(paste(cow_keywords, collapse = "|"), ignore_case = TRUE)))

junior_cow_results <- results_df %>%
  filter(division == "Jr Horse", str_detect(class_name, regex(paste(cow_keywords, collapse = "|"), ignore_case = TRUE)))

senior_cow_results <- results_df %>%
  filter(division == "Sr Horse", str_detect(class_name, regex(paste(cow_keywords, collapse = "|"), ignore_case = TRUE)))

# Assign Open cow results to Jr/Sr horses
open_for_juniors <- open_cow_results %>% filter(horse_arha_no %in% junior_horses)
open_for_seniors <- open_cow_results %>% filter(horse_arha_no %in% senior_horses)

# Combine
junior_cow_combined <- bind_rows(junior_cow_results, open_for_juniors)
senior_cow_combined <- bind_rows(senior_cow_results, open_for_seniors)

# Summarize Jr/Sr separately
junior_cow_summary <- junior_cow_combined %>%
  group_by(horse_name) %>%
  summarise(total_points = sum(points, na.rm = TRUE), .groups = "drop") %>%
  mutate(division = "Jr Horse")

senior_cow_summary <- senior_cow_combined %>%
  group_by(horse_name) %>%
  summarise(total_points = sum(points, na.rm = TRUE), .groups = "drop") %>%
  mutate(division = "Sr Horse")

# Summarize remaining divisions normally
other_cow_results <- results_df %>%
  filter(!division %in% c("Jr Horse", "Sr Horse", "Open"),
         str_detect(class_name, regex(paste(cow_keywords, collapse = "|"), ignore_case = TRUE))) %>%
  group_by(division, horse_name, exhibitor_name) %>%
  summarise(total_points = sum(points, na.rm = TRUE), .groups = "drop")

# Combine all cow horse results
cow_horse_summary <- bind_rows(
  junior_cow_summary,
  senior_cow_summary,
  other_cow_results
) %>%
  arrange(division, desc(total_points))

# ---- Top Performance Horse Standings ---- #
# Identify performance keywords
performance_keywords <- c("Trail", "Riding", "Showmanship", "Ranchmanship", "Reining", "Horsemanship")

performance_results_raw <- results_df %>%
  filter(str_detect(class_name, regex(paste(performance_keywords, collapse = "|"), ignore_case = TRUE)))

# Processing function for specialty categories
process_specialty <- function(df) {
  df <- df %>%
    mutate(
      division_type = case_when(
        division %in% c("Jr Horse", "Sr Horse") ~ "by_horse",
        TRUE ~ "by_pair"
      )
    )
  
  df_summary <- df %>%
    group_by(
      division,
      horse_name,
      exhibitor_name = if_else(division_type == "by_horse", NA_character_, exhibitor_name)
    ) %>%
    summarise(total_points = sum(points, na.rm = TRUE), .groups = "drop") %>%
    mutate(division_type = if_else(is.na(exhibitor_name), "by_horse", "by_pair"))
  
  return(df_summary)
}

# Process performance horse results
performance_results <- process_specialty(performance_results_raw)

# Save both specialty categories
saveRDS(cow_horse_summary, "C:/Users/Terri/Documents/github_projects/ILRHA_Points_2025/ILRHA_Standings_app/data/top_cow_horse.rds")
saveRDS(performance_results, "C:/Users/Terri/Documents/github_projects//ILRHA_Points_2025/ILRHA_Standings_app/data/top_performance_horse.rds")
