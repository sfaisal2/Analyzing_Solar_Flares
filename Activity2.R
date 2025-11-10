
suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(tidyr)
  library(tidymodels)
  library(rpart.plot)
})


file_name <- "RHESSI_SolarFlares_SolarCycle24_Task3_2025.csv"
flares_df <- read.csv(file_name, sep = ";", na.strings = c("", "NA"))


cat("ACTIVITY 1 RESULTS\n")



#1a. Remove rows
cat("--- (a) Data Cleaning ---\n")
initial_rows <- nrow(flares_df)
df_cleaned <- flares_df %>%
  mutate(date = dmy(date)) %>%
  filter(date >= ymd("2008-12-01")) %>%
  filter(!(x_pos == 0 & y_pos == 0 & energy_band == "3-6"))
final_rows <- nrow(df_cleaned)

cat("Initial rows:", initial_rows, "\n")
cat("Rows after cleaning:", final_rows, "\n")
cat("Rows removed:", initial_rows - final_rows, "\n\n")


#1b. Median duration per solar flare class
cat("--- (b) Median Duration per Class (seconds) ---\n")
median_duration <- df_cleaned %>%
  group_by(class_str) %>%
  summarise(median_duration_sec = median(duration, na.rm = TRUE))
print(as.data.frame(median_duration))
cat("\n")


#1c. Average total counts per solar flare class
avg_counts <- df_cleaned %>%
  group_by(class_str) %>%
  summarise(mean_total_counts = mean(total_counts, na.rm = TRUE))
print(as.data.frame(avg_counts))
cat("\n")


#1d. Average total counts yearly & most active year(s)
df_cleaned <- df_cleaned %>% mutate(year = year(date))
yearly_counts <- df_cleaned %>%
  group_by(year) %>%
  summarise(mean_total_counts = mean(total_counts, na.rm = TRUE)) %>%
  arrange(desc(mean_total_counts))


print(as.data.frame(head(yearly_counts, 5)))
cat("Most active year: ", yearly_counts$year[1], "\n\n")


#1e. Most active month(s) during the most active year
most_active_yr <- yearly_counts$year[1]
cat("--- (e) Monthly Activity in ", most_active_yr, "---\n")

monthly_active <- df_cleaned %>%
  filter(year == most_active_yr) %>%
  mutate(month = month(date, label = TRUE, abbr = FALSE)) %>%
  group_by(month) %>%
  summarise(mean_total_counts = mean(total_counts, na.rm = TRUE)) %>%
  arrange(desc(mean_total_counts))

print(as.data.frame(head(monthly_active, 5)))
cat("Most active month: ", as.character(monthly_active$month[1]), "\n\n")



#1f. Most common flags per class
common_flags <- df_cleaned %>%
  mutate(flags = as.character(flags)) %>%
  separate_rows(flags, sep = " ") %>%
  filter(flags != "" & !is.na(flags)) %>%
  group_by(class_str, flags) %>%
  summarise(count = n(), .groups = 'drop_last') %>%
  slice_max(order_by = count, n = 1) %>%
  ungroup()

print(as.data.frame(common_flags))
cat("\n")


#1g. Add new columns (Feature Engineering)
df_final_features <- df_cleaned %>%
  mutate(month = month(date, label = TRUE, abbr = FALSE)) %>%
  mutate(
    log_total_counts = log10(total_counts + 1),
    duration_min = duration / 60,
    peak_to_total_ratio = ifelse(total_counts > 0, peak_counts_per_sec / total_counts, 0)
  )


cat("New columns created: 'log_total_counts', 'duration_min', 'peak_to_total_ratio'\n")
print(head(df_final_features %>% 
             select(class_str, log_total_counts, duration_min, peak_to_total_ratio), 3))
cat("\n\n")



cat("ACTIVITY 2 RESULTS\n")


#2a & 2b. Setup and Split
df_ml <- df_final_features %>%
  filter(class_str %in% c("C", "M", "X", "NF")) %>%
  mutate(class = factor(class_str, levels = c("NF", "C", "M", "X"))) %>%
  arrange(date)


set.seed(42)
split <- initial_time_split(df_ml, prop = 0.8)
train_data <- training(split)
test_data <- testing(split)
cat("Training samples: ", nrow(train_data), "\n")
cat("Testing samples: ", nrow(test_data), "\n\n")


#Recipe
flare_recipe <- recipe(class ~ duration + peak_counts_per_sec + total_counts + 
                           x_pos + y_pos + SESC_sunspot_number + sunspot_area + 
                          log_total_counts + duration_min + peak_to_total_ratio + 
                          energy_band, 
                         data = df_ml) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())

#2c. Fast Tuning - Training Decision Tree
tree_spec <- decision_tree(tree_depth = tune(), min_n = tune()) %>%
  set_engine("rpart") %>% set_mode("classification")


#Minimal grid and folds for speed
tree_grid <- grid_regular(tree_depth(range = c(3, 5)), min_n(range = c(10, 20)), levels = 2)
time_folds <- rolling_origin(train_data, initial = floor(nrow(train_data)*0.6), 
                             assess = floor(nrow(train_data)*0.2), 
                             skip = floor(nrow(train_data)*0.2), cumulative = TRUE)

wf <- workflow() %>% add_recipe(flare_recipe) %>% add_model(tree_spec)
tune_results <- tune_grid(wf, resamples = time_folds, grid = tree_grid, 
                          metrics = metric_set(f_meas, accuracy))


#2d Performance Reporting 

cat("Cross-Validation:\n")
best_cv <- show_best(tune_results, metric = "f_meas", n = 1)
print(best_cv %>% select(mean, std_err, tree_depth, min_n))
cat("\n")


best_params <- select_best(tune_results, metric = "f_meas")
final_fit <- finalize_workflow(wf, best_params) %>% fit(data = train_data)



test_results <- bind_cols(test_data %>% select(class), predict(final_fit, new_data = test_data))
cat("Test Accuracy: ", accuracy(test_results, truth = class, estimate = .pred_class)$.estimate, "\n")
cat("Test F1-Score (Weighted): ", f_meas(test_results, truth = class, estimate = .pred_class)$.estimate, "\n\n")

cat("Confusion Matrix:\n")
print(conf_mat(test_results, truth = class, estimate = .pred_class))
cat("\n")

#2e. Tree Analysis 
final_tree <- extract_fit_engine(final_fit)
rpart.plot::rpart.rules(final_tree, extra = 4, cover = TRUE, roundint=FALSE)

