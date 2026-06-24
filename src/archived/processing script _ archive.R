# ============================================================
#               MoRHA 2025 Standings Processing
# ============================================================

library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)
library(tidyr)

# ------------------- Connect to database ------------------- #
con <- dbConnect(
  SQLite(),
  "C:/Users/Terri/Documents/github_projects/MoRHA_2025/DB/morha_120125.db"
)

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
      "Aug" = "August",
      "Nov" = "November",
      "Nov." = "November"
    ),
    class_name = str_replace_all(class_name, "\\.", ""),
    class_name = str_squish(class_name)
  ) %>%
  # No year-end points for these:
  filter(!class_name %in% c("MoRHA Walk/Whoa", "MoRHA Jackpot Walk/Trot"))

# ============================================================
#      DETERMINE EXHIBITOR’S “REAL DIVISION” (Nov vs Ama/Yth)
# ============================================================

# For each exhibitor, detect whether they are Nov Ama / Ama / Nov Yth / Yth
# Novice outranks regular (NA > AMA; NY > YTH).
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
        "Yearling IH Trail"
      ) ~ "Open",
      
      TRUE ~ NA_character_
    )
  )

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
    # MoRHA: no double-point shows
    points = base_points
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

# ---- All-age combined Jr/Sr working classes (Open) ---- #
open_combined <- open_results %>%
  filter(class_name %in% c("Ranch Roping", "Working Cow Horse", "Working Ranch Horse"))

open_for_juniors <- open_combined %>% filter(horse_arha_no %in% junior_horses)
open_for_seniors <- open_combined %>% filter(horse_arha_no %in% senior_horses)

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
  open_halter_two_under,                          # all Two & Under → Junior
  open_halter_three_over %>%                     # Three & Over → Jr if in jr list
    filter(horse_arha_no %in% junior_horses)
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
  summarise(total_points = sum(points, na.rm = TRUE), .groups = "drop") %>%
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
  summarise(total_points = sum(points, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    results_df_div %>% select(horse_arha_no, horse_name) %>% distinct(),
    by = "horse_arha_no"
  ) %>%
  mutate(exhibitor_name = NA_character_)

# ---- All other divisions (Ama, Nov Ama, Yth, Nov Yth, Novice W/T, etc.) ---- #
other_divisions <- results_df_div %>%
  filter(!division %in% c("Junior Horse", "Senior Horse", "Open"),
         !is.na(points)) %>%
  group_by(division, horse_arha_no, horse_name, exhibitor_name) %>%
  summarise(total_points = sum(points, na.rm = TRUE), .groups = "drop") %>%
  mutate(division_type = "by_pair")

division_summary <- bind_rows(
  junior_division_summary %>% mutate(division_type = "by_horse"),
  senior_division_summary %>% mutate(division_type = "by_horse"),
  other_divisions
) %>%
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
    division = "Junior Horse",
    .groups = "drop"
  )

senior_cow_summary <- bind_rows(senior_cow_results, open_for_seniors_cow) %>%
  group_by(horse_name) %>%
  summarise(
    total_points = sum(points, na.rm = TRUE),
    division = "Senior Horse",
    .groups = "drop"
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

