
pkgs <- c("readr","dplyr","tidyr","stringr","lubridate","ggplot2","zoo")
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install)) install.packages(to_install, quietly = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

out_dir <- "outputs"
if (!dir.exists(out_dir)) dir.create(out_dir)

zip_path <- if (file.exists("SolarFlare_Data25.zip")) {
  "SolarFlare_Data25.zip"
} else {
  zips <- list.files(pattern = "\\.zip$", full.names = TRUE)
  if (length(zips) == 0) stop("No .zip file found. Put SolarFlare_Data25.zip next to this script.")
  zips[1]
}

unz_dir <- file.path(tempdir(), "solar_zip")
if (!dir.exists(unz_dir)) dir.create(unz_dir)
unzip(zip_path, exdir = unz_dir)

csv_candidates <- list.files(unz_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
if (length(csv_candidates) == 0) {
  csv_candidates <- list.files(pattern = "\\.csv$", full.names = TRUE)
}
if (length(csv_candidates) == 0) stop("No CSV found in the zip or current folder.")
csv_path <- csv_candidates[1]
message("Reading (semicolon-delimited): ", csv_path)

df <- suppressMessages(readr::read_delim(csv_path, delim = ";", trim_ws = TRUE, show_col_types = FALSE))

names(df) <- names(df) |>
  stringr::str_replace_all("[^A-Za-z0-9]+", "_") |>
  stringr::str_replace_all("_+", "_") |>
  stringr::str_replace("^_|_$", "") |>
  stringr::str_to_lower()

pick_col <- function(pattern, nm = names(df)) {
  hits <- grep(pattern, nm, ignore.case = TRUE, value = TRUE)
  if (length(hits) == 0) NA_character_ else hits[1]
}

energy_col    <- pick_col("^energy_?band$|keV|band$")
xpos_col      <- pick_col("^x_?pos$|x_?position|^x$")
ypos_col      <- pick_col("^y_?pos$|y_?position|^y$")
class_col     <- pick_col("^class_str$|^class$|goes.*class")
total_col     <- pick_col("^total_?counts$|counts.*total")
duration_col  <- pick_col("^duration$")
date_col      <- pick_col("^date$")
peakps_col    <- pick_col("^peak_?counts_?per_?sec$|^peak_?counts$")

if (is.na(energy_col)) stop("Could not find an energy band column.")
if (is.na(xpos_col))   stop("Could not find an X position column.")
if (is.na(date_col))   stop("Could not find a date column.")

df <- df %>%
  mutate(
    energy_band    = .data[[energy_col]],
    x_pos          = suppressWarnings(as.numeric(.data[[xpos_col]])),
    y_pos          = if (!is.na(ypos_col)) suppressWarnings(as.numeric(.data[[ypos_col]])) else NA_real_,
    class_str      = if (!is.na(class_col)) as.character(.data[[class_col]]) else NA_character_,
    total_counts   = if (!is.na(total_col)) suppressWarnings(as.numeric(.data[[total_col]])) else NA_real_,
    duration       = if (!is.na(duration_col)) suppressWarnings(as.numeric(.data[[duration_col]])) else NA_real_,
    peak_counts    = if (!is.na(peakps_col)) suppressWarnings(as.numeric(.data[[peakps_col]])) else NA_real_,
    date           = .data[[date_col]]
  )

df <- df %>%
  mutate(
    energy_band = energy_band |> stringr::str_replace_all("\\s", "") |> stringr::str_replace_all("keV", "") |> stringr::str_to_lower()
  )

rows_before <- nrow(df)
df <- df %>% filter(!(x_pos == 0 & stringr::str_detect(energy_band, "^3-6$")))
rows_after <- nrow(df)
message("Removed rows (x_pos==0 & energy_band==3-6): ", rows_before - rows_after)

st_col <- pick_col("^start_time$")
pk_col <- pick_col("^peak_time$")
en_col <- pick_col("^end_time$")

parse_time <- function(x) {
  if (is.null(x) || all(is.na(x))) return(NA_character_)
  suppressWarnings(ifelse(
    stringr::str_detect(x, "^\\d{2}:\\d{2}:\\d{2}$"), x,
    ifelse(stringr::str_detect(x, "^\\d{2}:\\d{2}$"), paste0(x, ":00"), NA_character_)
  ))
}

df <- df %>%
  mutate(
    date       = suppressWarnings(lubridate::ymd(date)),
    start_time = if (!is.na(st_col)) parse_time(.data[[st_col]]) else NA_character_,
    peak_time  = if (!is.na(pk_col)) parse_time(.data[[pk_col]]) else NA_character_,
    end_time   = if (!is.na(en_col)) parse_time(.data[[en_col]]) else NA_character_,
    start_dt   = suppressWarnings(lubridate::ymd_hms(paste(date, start_time), quiet = TRUE)),
    peak_dt    = suppressWarnings(lubridate::ymd_hms(paste(date, peak_time), quiet = TRUE)),
    end_dt     = suppressWarnings(lubridate::ymd_hms(paste(date, end_time), quiet = TRUE)),
    year       = lubridate::year(date),
    month      = lubridate::month(date, label = TRUE, abbr = TRUE),
    ym         = floor_date(date, unit = "month"),
    
    log_total_counts = ifelse(is.finite(total_counts) & total_counts > 0, log10(total_counts), NA_real_)
  )

df <- df %>% filter(date >= as.Date("2008-12-01"), date <= as.Date("2019-12-31"))

t1_median_duration <- df %>%
  filter(!is.na(class_str), !is.na(duration)) %>%
  group_by(class_str) %>%
  summarize(median_duration_sec = median(duration, na.rm = TRUE), .groups = "drop")

t1_avg_counts_by_class <- df %>%
  filter(!is.na(class_str), !is.na(total_counts)) %>%
  group_by(class_str) %>%
  summarize(avg_total_counts = mean(total_counts, na.rm = TRUE), .groups = "drop")

t1_avg_counts_by_year <- df %>%
  filter(!is.na(year), !is.na(total_counts)) %>%
  group_by(year) %>%
  summarize(avg_total_counts = mean(total_counts, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_total_counts))

max_year_val <- max(t1_avg_counts_by_year$avg_total_counts, na.rm = TRUE)
most_active_years <- t1_avg_counts_by_year %>%
  filter(abs(avg_total_counts - max_year_val) < 1e-9) %>%
  pull(year)

t1_avg_counts_by_month_in_best_year <- df %>%
  filter(year %in% most_active_years, !is.na(total_counts), !is.na(ym)) %>%
  group_by(year, ym, month) %>%
  summarize(avg_total_counts = mean(total_counts, na.rm = TRUE), .groups = "drop") %>%
  arrange(year, desc(avg_total_counts))

best_months <- t1_avg_counts_by_month_in_best_year %>%
  group_by(year) %>%
  filter(avg_total_counts == max(avg_total_counts, na.rm = TRUE)) %>%
  ungroup()

flags_col <- pick_col("^flags$")
t1_flags_by_class <- if (!is.na(flags_col)) {
  df %>%
    filter(!is.na(class_str), !is.na(.data[[flags_col]])) %>%
    group_by(class_str, flags = .data[[flags_col]]) %>%
    tally(name = "count") %>%
    arrange(class_str, desc(count)) %>%
    group_by(class_str) %>%
    slice_max(order_by = count, n = 1, with_ties = TRUE) %>%
    ungroup()
} else {
  tibble(class_str = unique(df$class_str), flags = NA_character_, count = NA_integer_)
}

clean_path <- file.path(out_dir, "solar_flares_cleaned_task1.csv")
readr::write_csv(df, clean_path)
readr::write_csv(t1_median_duration, file.path(out_dir, "t1_median_duration_by_class.csv"))
readr::write_csv(t1_avg_counts_by_class, file.path(out_dir, "t1_avg_total_counts_by_class.csv"))
readr::write_csv(t1_avg_counts_by_year,  file.path(out_dir, "t1_avg_total_counts_by_year.csv"))
readr::write_csv(t1_avg_counts_by_month_in_best_year, file.path(out_dir, "t1_avg_total_counts_by_month_in_best_year.csv"))
readr::write_csv(t1_flags_by_class,     file.path(out_dir, "t1_top_flags_by_class.csv"))

message("\n=== Task 1 Results ===")
print(t1_median_duration)
print(t1_avg_counts_by_class)
print(t1_avg_counts_by_year)
message("Most active year(s): ", paste(most_active_years, collapse = ", "))
print(best_months)
message("Top flags by class (mode-like):")
print(t1_flags_by_class)
message("Cleaned data saved to: ", normalizePath(clean_path))

daily <- df %>%
  filter(!is.na(date), !is.na(total_counts)) %>%
  group_by(date) %>%
  summarize(
    daily_mean = mean(total_counts, na.rm = TRUE),
    daily_sum  = sum(total_counts,  na.rm = TRUE),
    n_events   = dplyr::n(),
    .groups = "drop"
  ) %>%
  arrange(date)

k <- 30
int_col <- "daily_mean"

if (nrow(daily) >= k) {
  daily <- daily %>%
    mutate(
      roll_mean = zoo::rollapply(.data[[int_col]], width = k, FUN = mean, align = "right",
                                 fill = NA, na.rm = TRUE),
      roll_std  = zoo::rollapply(.data[[int_col]], width = k, FUN = sd, align = "right",
                                 fill = NA, na.rm = TRUE)
    ) %>%
    mutate(
      resid = .data[[int_col]] - roll_mean,
      z     = resid / roll_std,
      sudden_change = ifelse(!is.na(z) & abs(z) >= 3, TRUE, FALSE)
    )
} else {
  warning("Not enough days for a ", k, "-day rolling window. Plots will omit rolling stats.")
  daily <- daily %>% mutate(roll_mean = NA_real_, roll_std = NA_real_, z = NA_real_, sudden_change = FALSE)
}

# 6a) Intensity over time
p_intensity <- ggplot(daily, aes(x = date, y = .data[[int_col]])) +
  geom_line() +
  geom_point(alpha = 0.4) +
  labs(title = "Flare Intensity Over Time (Daily Mean of total_counts)",
       x = "Date", y = "Daily Mean total_counts")
ggsave(file.path(out_dir, "t4a_intensity_over_time.png"), p_intensity, width = 10, height = 5, dpi = 160)

# 6c) Rolling mean
p_roll_mean <- ggplot(daily, aes(x = date, y = roll_mean)) +
  geom_line() +
  labs(title = paste0("Rolling Mean (", k, "-day) of Daily Mean total_counts"),
       x = "Date", y = "Rolling Mean")
ggsave(file.path(out_dir, "t4c_rolling_mean.png"), p_roll_mean, width = 10, height = 5, dpi = 160)

# 6d) Rolling std
p_roll_std <- ggplot(daily, aes(x = date, y = roll_std)) +
  geom_line() +
  labs(title = paste0("Rolling Std (", k, "-day) of Daily Mean total_counts"),
       x = "Date", y = "Rolling Std")
ggsave(file.path(out_dir, "t4d_rolling_std.png"), p_roll_std, width = 10, height = 5, dpi = 160)

# 6e) Combined with sudden-change annotations
p_combined <- ggplot(daily, aes(x = date)) +
  geom_line(aes(y = .data[[int_col]]), alpha = 0.5) +
  geom_line(aes(y = roll_mean), linewidth = 0.8) +
  geom_ribbon(aes(ymin = roll_mean - roll_std, ymax = roll_mean + roll_std), alpha = 0.15) +
  geom_point(data = subset(daily, sudden_change), aes(y = .data[[int_col]]), size = 2) +
  labs(title = paste0("Daily Intensity, ", k, "-day Rolling Mean & Std; Sudden Changes (|z| ≥ 3)"),
       x = "Date", y = "Intensity (Daily Mean total_counts)")
ggsave(file.path(out_dir, "t4e_combined_with_changes.png"), p_combined, width = 11, height = 6, dpi = 160)

message("\n=== Task 4 Outputs ===")
message("Saved plots to: ", normalizePath(out_dir))
message(" - t4a_intensity_over_time.png")
message(" - t4c_rolling_mean.png")
message(" - t4d_rolling_std.png")
message(" - t4e_combined_with_changes.png")

message("\nDone. Use the cleaned CSV for later tasks, and include the Task 4 figures in your report.")
