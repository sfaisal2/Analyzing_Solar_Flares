
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(readr)
  library(scales)
  library(stringr)
})

data_file <- "C:/Users/Checkout Recon/Desktop/Task5/SolarFlare_Data25/RHESSI_SolarFlares_SolarCycle24_Task3_2025.csv"
grid_n   <- 150
save_out <- TRUE

out_dir <- "flares_outputs"
if (save_out) dir.create(out_dir, showWarnings = FALSE)

message("Reading CSV file: ", data_file)
dat <- read_delim(data_file, delim = ";", show_col_types = FALSE, trim_ws = TRUE)

cat("\nCSV headers:\n"); print(names(dat))

col_date     <- "date"
col_x        <- "x_pos"
col_y        <- "y_pos"
col_duration <- "duration"
col_eband    <- "energy_band"
col_counts   <- "total_counts"

needed <- c(col_date, col_x, col_y, col_duration, col_eband, col_counts)
if (!all(needed %in% names(dat))) {
  missing <- needed[!(needed %in% names(dat))]
  stop("Missing required columns in the CSV: ", paste(missing, collapse = ", "))
}

dat <- dat %>%
  mutate(
    date         = as.character(.data[[col_date]]),
    X_pos        = suppressWarnings(as.numeric(.data[[col_x]])),
    Y_pos        = suppressWarnings(as.numeric(.data[[col_y]])),
    duration     = suppressWarnings(as.numeric(.data[[col_duration]])),
    energy_band  = as.character(.data[[col_eband]]),
    total_counts = suppressWarnings(as.numeric(.data[[col_counts]]))
  ) %>%
  filter(!is.na(X_pos), !is.na(Y_pos), !is.na(duration), !is.na(total_counts)) %>%
  mutate(energy_band = trimws(energy_band))

dat <- dat %>% mutate(intensity_m1 = total_counts)

bands <- str_match(dat$energy_band, "(\\d+(?:\\.\\d+)?)\\D+(\\d+(?:\\.\\d+)?)")
lo <- as.numeric(bands[, 2])
hi <- as.numeric(bands[, 3])

exclude_3_6 <- (!is.na(lo) & !is.na(hi) & lo == 3 & hi == 6)

hi_capped <- ifelse(!is.na(hi), pmin(hi, 800), NA_real_)

mid_keV <- ifelse(!is.na(lo) & !is.na(hi_capped), (lo + hi_capped) / 2, 0)

dat <- dat %>%
  mutate(
    e_weight     = ifelse(exclude_3_6, 0, mid_keV),
    intensity_m2 = duration * e_weight
  )

make_map <- function(df, value_col, title, bins = 150) {
  ggplot(df, aes(x = X_pos, y = Y_pos, z = .data[[value_col]])) +
    stat_summary_2d(bins = bins, fun = sum) +
    scale_fill_gradient(name = "Sum intensity", low = "white", high = "steelblue",
                        labels = label_comma()) +
    coord_equal() +
    labs(title = title, x = "X_pos", y = "Y_pos") +
    theme_minimal(base_size = 12)
}

p1 <- make_map(dat, "intensity_m1", "Method 1: Intensity from total_counts", bins = grid_n)
p2 <- make_map(dat, "intensity_m2", "Method 2: Intensity from duration × energy_band", bins = grid_n)

if (save_out) {
  ggsave(file.path(out_dir, "map_method1_counts.png"), p1, width = 8, height = 6, dpi = 200)
  ggsave(file.path(out_dir, "map_method2_duration_energy.png"), p2, width = 8, height = 6, dpi = 200)
  message("✅ Maps saved to: ", normalizePath(out_dir))
} else {
  print(p1); print(p2)
}

total_m1 <- sum(dat$intensity_m1, na.rm = TRUE)
total_m2 <- sum(dat$intensity_m2, na.rm = TRUE)

cell_summary <- function(df, val_col, n_bins = 50) {
  xbreaks <- pretty(range(df$X_pos, na.rm = TRUE), n = n_bins)
  ybreaks <- pretty(range(df$Y_pos, na.rm = TRUE), n = n_bins)
  cutx <- cut(df$X_pos, breaks = xbreaks, include.lowest = TRUE)
  cuty <- cut(df$Y_pos, breaks = ybreaks, include.lowest = TRUE)
  df %>%
    mutate(cx = cutx, cy = cuty) %>%
    group_by(cx, cy) %>%
    summarise(val = sum(.data[[val_col]], na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(val)) %>%
    slice_head(n = 5)
}

top_cells_m1 <- cell_summary(dat, "intensity_m1")
top_cells_m2 <- cell_summary(dat, "intensity_m2")

cat("\n=== Activity 5 Summary ===\n")
cat(sprintf("Total intensity (Method 1: total_counts): %s\n", comma(total_m1)))
cat(sprintf("Total intensity (Method 2: duration × energy_band): %s\n\n", comma(total_m2)))
cat("Top 5 spatial cells (Method 1):\n"); print(top_cells_m1)
cat("\nTop 5 spatial cells (Method 2):\n"); print(top_cells_m2)
cat("\nInterpretation:\n")
cat("- Method 1 highlights locations with many photons (counts) regardless of band or duration.\n")
cat("- Method 2 upweights long events and higher-energy bands; peaks may shift versus Method 1.\n")
cat("Compare the two heatmaps (PNG files) to discuss spatial differences and totals.\n")

if (save_out) {
  sink(file.path(out_dir, "summary.txt"))
  cat("Activity 1 + 5 Summary\n\n")
  cat("Generated files:\n")
  cat(" - map_method1_counts.png\n")
  cat(" - map_method2_duration_energy.png\n\n")
  cat(sprintf("Total intensity (Method 1: total_counts): %s\n", comma(total_m1)))
  cat(sprintf("Total intensity (Method 2: duration × energy_band): %s\n\n", comma(total_m2)))
  cat("Top 5 spatial cells (Method 1):\n"); print(top_cells_m1)
  cat("\nTop 5 spatial cells (Method 2):\n"); print(top_cells_m2)
  cat("\nNotes:\n")
  cat("- Method 2 parses 'lo-hi' ranges in energy_band; exact '3-6' rows are excluded.\n")
  cat("- Upper bound capped at 800 keV for stability (adjust if your rubric prefers).\n")
  sink()
  message("✅ Wrote summary.txt to: ", normalizePath(out_dir))
}

