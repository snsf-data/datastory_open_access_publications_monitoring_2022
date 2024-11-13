if (interactive()) {
  
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(forcats)
  library(here)
  
}

#==============================================================================#
# Read and process raw data                                                 ####
#==============================================================================#

# Read data for 2020 directly from the repository of the latest data story on
# Open Access monitoring.
dat_2020 <-
  read_csv(
    paste0(
      "https://raw.githubusercontent.com/snsf-data/",
      "datastory_new_figures_oa_monitoring_2020/main/data/",
      "publications_2020_dec_2021.csv"
    )
  ) |>
  mutate(
    oa_status =
      fct_relevel(
        recode(oa_status, "closed" = "restricted"),
        c("gold", "green", "hybrid", "other OA", "restricted")
      ),
    # SNSF only considers gold, green and hybrid as OA ("other OA" are mainly
    # non-peer-reviewed OA records).
    is_oa = oa_status %in% c("gold", "green", "hybrid")
  )

# Read data for 2021 directly from the repository of the latest data story on
# Open Access monitoring.
dat_2021 <-
  read_csv(
    paste0(
      "https://raw.githubusercontent.com/snsf-data/",
      "datastory_oa_monitoring_2021/main/data/",
      "publications_2021_mar_2023.csv"
    )
  ) |>
  filter(!str_detect(type, "ook|onograph")) |>
  mutate(
    oa_status =
      fct_relevel(
        recode(oa_status, "closed" = "restricted"),
        c("gold", "green", "hybrid", "other OA", "restricted")
      ),
    # SNSF only considers gold, green and hybrid as OA ("other OA" are mainly
    # non-peer-reviewed OA records).
    is_oa = oa_status %in% c("gold", "green", "hybrid")
  )

# Read data on Open Access in 2021 from the data folder
dat_2022 <-
  read_csv(here("data", "publications_2022_feb_2024.csv")) |>
  # Filter publications that are books or monograph
  filter(!str_detect(type, "ook|onograph")) |>
  mutate(
    oa_status =
      fct_relevel(
        recode(oa_status, "closed" = "restricted"),
        c("gold", "green", "hybrid", "other OA", "restricted")
      ),
    # SNSF only considers gold, green and hybrid as OA ("other OA" are mainly
    # non-peer-reviewed OA records).
    is_oa = oa_status %in% c("gold", "green", "hybrid")
  )

#==============================================================================#
# OA share per category                                                     ####
#==============================================================================#

snsf_oa_categories_2020 <-
  dat_2020 |>
  # Remove unknowns
  filter(oa_status != "unknown") |>
  count(oa_status) |>
  mutate(
    freq = n / sum(n),
    data_id = paste0("2020", row_number())
  )

snsf_oa_categories_2021 <-
  dat_2021 |>
  # Remove unknowns
  filter(oa_status != "unknown") |>
  count(oa_status) |>
  mutate(
    freq = n / sum(n),
    data_id = paste0("2021", row_number())
  )

snsf_oa_categories_2022 <-
  dat_2022 |>
  # Remove unknowns
  filter(oa_status != "unknown") |>
  count(oa_status) |>
  mutate(
    freq = n / sum(n),
    data_id = paste0("2022", row_number())
  )

#==============================================================================#
# Combine present and previous monitorings                                  ####
#==============================================================================#

# Summarise for all monitorings share of OA categories and combine all data
former_monitorings <-
  tibble(
    period = c("2013-2015", "2014-2018", "2018-2019"),
    `SNSF OA (gold, green, hybrid)` = c(39, 48, 55),
    restricted = c(44, 33, 23),
    `other OA` = c(17, 19, 22)
  ) |>
  add_row(
    period = "2020",
    `SNSF OA (gold, green, hybrid)` =
      snsf_oa_categories_2020 |>
      filter(!str_detect(oa_status, "^other|^rest")) |>
      summarise(pct = round(sum(freq) * 100)) |>
      pull(pct),
    restricted = snsf_oa_categories_2020 |>
      filter(str_detect(oa_status, "^rest")) |>
      summarise(pct = round(freq * 100)) |>
      pull(pct),
    `other OA` = snsf_oa_categories_2020 |>
      filter(str_detect(oa_status, "^other")) |>
      summarise(pct = round(freq * 100)) |>
      pull(pct)
  ) |> 
  add_row(
    period = "2021",
    `SNSF OA (gold, green, hybrid)` =
      snsf_oa_categories_2021 |>
      filter(!str_detect(oa_status, "^other|^rest")) |>
      summarise(pct = round(sum(freq) * 100)) |>
      pull(pct),
    restricted = snsf_oa_categories_2021 |>
      filter(str_detect(oa_status, "^rest")) |>
      summarise(pct = round(freq * 100)) |>
      pull(pct),
    `other OA` = snsf_oa_categories_2021 |>
      filter(str_detect(oa_status, "^other")) |>
      summarise(pct = round(freq * 100)) |>
      pull(pct)
  ) |>
  add_row(
    period = "2022",
    `SNSF OA (gold, green, hybrid)` =
      snsf_oa_categories_2022 |>
      filter(!str_detect(oa_status, "^other|^rest")) |>
      summarise(pct = round(sum(freq) * 100)) |>
      pull(pct),
    restricted = snsf_oa_categories_2022 |>
      filter(str_detect(oa_status, "^rest")) |>
      summarise(pct = round(freq * 100)) |>
      pull(pct),
    `other OA` = snsf_oa_categories_2022 |>
      filter(str_detect(oa_status, "^other")) |>
      summarise(pct = round(freq * 100)) |>
      pull(pct)
  ) |>
  pivot_longer(cols = -period, names_to = "category")

#==============================================================================#
# Licenses                                                                  ####
#==============================================================================#

license_dat_2022 <-
  dat_2022 |>
  select(license = up_license, oa_status) |>
  filter(oa_status != "unknown") |>
  mutate(
    license = replace_na(license, "Unknown"),
    license =
      case_when(
        
        license %in% c("public-domain",
                       "publisher-specific-oa",
                       "unspecified-oa") ~ "Other OA/public domain",
        
        str_ends(license, "nc-sa|by-nd|by-sa") ~ "Other CC BY",
        
        .default = license
      )
  ) |>
  count(license, oa_status) |>
  mutate(
    pct = n / sum(n),
    oa_status =
      if_else(
        str_starts(oa_status, "other"), "Other OA", str_to_sentence(oa_status)
      ),
    license = str_replace_all(license, "cc-by", "cc by"),
    license = 
      fct_reorder(
        if_else(
          str_starts(license, "cc"),
          str_to_upper(license),
          license
        ),
        n, sum
      ),
    .by = oa_status
  ) |>
  mutate(oa_status = fct_relevel(oa_status, "Restricted", after = 4))

license_dat_2021 <-
  dat_2021 |>
  select(license = up_license, oa_status) |>
  filter(oa_status != "unknown") |>
  mutate(
    license = replace_na(license, "Unknown"),
    license =
      case_when(
        
        str_starts(license, "acs-specific|publisher-specific|NULL|pd") ~ "Unknown",
        
        str_detect(license, "oa user license|implied-oa") ~ "Other OA/public domain",
        
        str_ends(license, "cc0|nc-sa|by-nd|by-sa") ~ "Other CC BY",
        
        .default = license
      )
  ) |>
  count(license, oa_status) |>
  mutate(
    pct = n / sum(n),
    oa_status =
      if_else(
        str_starts(oa_status, "other"), "Other OA", str_to_sentence(oa_status)
      ),
    license = str_replace_all(license, "cc-by", "cc by"),
    license = 
      fct_reorder(
        if_else(
          str_starts(license, "cc"),
          str_to_upper(license),
          license
        ),
        n, sum
      ),
    .by = oa_status
  ) |>
  mutate(oa_status = fct_relevel(oa_status, "Restricted", after = 4))
