### required libraries
library(arrow)
library(tidyverse)
library(sf)
library(tigris)
library(cowplot)
library(data.table)
library(mobility)
library(jsonlite)
library(purrr)
library(viridis)
library(pROC)
library(broom)

options(tigris_use_cache = TRUE)


#############
#############
#### travel and movement het summary
#############
#############

#### bring in travel het summary table
travel_het_summary <- open_dataset("~/Documents/Mobility/Cuebiq_datasets/TRAVEL_HET_SUMMARY_STATS_EXPORT_CLEAN") %>%
  collect()

## rename columns and take only rows of interest (i.e. no rows of 8 hr max loc across day, because that only allows for 2 trips and no travel het)
colnames(travel_het_summary) <- c("mean", "2.5%", "97.5%", "name")
travel_het_summary_clean <- travel_het_summary[c(1:12, 17, 18),]

## clean percentages
travel_het_summary_clean$mean = travel_het_summary_clean$mean * 100
travel_het_summary_clean$`2.5%` = travel_het_summary_clean$`2.5%` * 100
travel_het_summary_clean$`97.5%` = travel_het_summary_clean$`97.5%` * 100

## add in variables to describe each row
travel_het_summary_clean$het_type <- "Travel"
travel_het_summary_clean$admin <- c(rep(c("County", "Census tract"), 7))
travel_het_summary_clean$time_agg <- c(rep("Week", 6), rep("Month", 6), rep("Day", 2))
travel_het_summary_clean$max_loc <- c(rep(c(rep("8hr", 2), rep("24hr", 2), rep("All trips", 2)), 2), "All trips", "All trips")

#### bring in movement het summary table
move_het_summary <- open_dataset("~/Documents/Mobility/Cuebiq_datasets/MOVE_HET_SUMMARY_STATS_EXPORT_CLEAN") %>%
  collect()

## rename columns, clean percentages, and add descriptor variables
colnames(move_het_summary) <- c("mean", "2.5%", "97.5%", "name")
move_het_summary_clean <- move_het_summary
move_het_summary_clean$het_type <- "Movement"
move_het_summary_clean$admin <- c(rep(c("County", "Census tract"), 9))
move_het_summary_clean$time_agg <- c(rep("Week", 6), rep("Month", 6), rep("Day", 2), rep("3 Days", 2), rep("Day", 2))
move_het_summary_clean$max_loc <- c(rep(c(rep("8hr", 2), rep("24hr", 2), rep("All trips", 2)), 3))

## combine both het tables
both_het_summary <- rbind(move_het_summary_clean, travel_het_summary_clean)

## factor time aggregation and maximum location variable for plotting purposes
both_het_summary$time_agg <- factor(both_het_summary$time_agg, levels = c("Day", "3 Days", "Week", "Month"))
both_het_summary$max_loc <- factor(both_het_summary$max_loc, levels = c("8hr", "24hr", "All trips"))

## plot both on one plot
quartz()
ggplot(both_het_summary, aes(x = time_agg, y = mean, color = admin, group = admin)) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5), size = 4) +
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`),
                width = 0.1,
                position = position_dodge(width = 0.5)) +
  facet_grid(rows = vars(het_type), cols = vars(max_loc), scales = "free") +
  scale_color_discrete(drop = TRUE) +
  labs(x = "Temporal Aggregation", y = "Mean", color = "Spatial Aggregation") +
  theme_minimal(base_size = 14)


######
## do it invdividually for each het type
######

## change movement het from non-movers to movers
move_het_summary_movers <- move_het_summary_clean %>%
  mutate(mean = 100 - mean,
         `2.5%` = 100 - `2.5%`,
         `97.5%` = 100 - `97.5%`)

### factor graphing variables and plot
move_het_summary_movers$time_agg <- factor(move_het_summary_movers$time_agg, levels = c("Day", "3 Days", "Week", "Month"))
move_het_summary_movers$max_loc <- factor(move_het_summary_movers$max_loc, levels = c("8hr", "24hr", "All trips"))
move_het_summary_movers$admin <- factor(move_het_summary_movers$admin, levels = c("County", "Census tract"))

move_het_summary_movers_plot <- ggplot(move_het_summary_movers, aes(x = time_agg, y = mean, color = admin, group = admin)) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5), size = 4) +
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`),
                width = 0.1,
                position = position_dodge(width = 0.5)) +
  facet_grid(cols = vars(max_loc), scales = "free") +
  scale_color_viridis(discrete = T, option = "B", begin = 0.4, end = 0.7) +
  labs(x = "", y = "Percent of people who move", color = "Spatial Aggregation") +
  theme(axis.title.x = element_blank()) +
  theme_minimal(base_size = 14)
move_het_summary_movers_plot

### factor travel het plotting variables and plot
travel_het_summary_clean$time_agg <- factor(travel_het_summary_clean$time_agg, levels = c("Day", "3 Days", "Week", "Month"))
travel_het_summary_clean$max_loc <- factor(travel_het_summary_clean$max_loc, levels = c("8hr", "24hr", "All trips"))
travel_het_summary_clean$admin <- factor(travel_het_summary_clean$admin, levels = c("County", "Census tract"))

travel_het_summary_clean_plot <- ggplot(travel_het_summary_clean, aes(x = time_agg, y = mean, color = admin, group = admin)) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5), size = 4) +
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`),
                width = 0.1,
                position = position_dodge(width = 0.5)) +
  facet_grid(cols = vars(max_loc), scales = "free") +
  scale_color_viridis(discrete = T, option = "B", begin = 0.4, end = 0.7) +
  labs(x = "Temporal Aggregation", y = "Percent of OD pairs \n with travel heterogeneity", color = "Spatial Aggregation") +
  theme_minimal(base_size = 14)
travel_het_summary_clean_plot

## create combined layout and combine graphs vertically
het_summary_layout <- "
  1
  2
"

het_summary_plot <- (move_het_summary_movers_plot + travel_het_summary_clean_plot +
                       plot_layout(guides = "collect") & theme(legend.position = "right")) +
  plot_layout(design = het_summary_layout, widths = c(1),
              heights = c(1, 1)) +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = "bold"))
het_summary_plot

ggsave(het_summary_plot, filename = "~/Desktop/test_het_summary_plot.png", height = 6, width = 8)








############
############
####### travel het preds
############
############

## load in travel het dataset with all max loc per day over a month for county
travel_het_month_daily_county <- open_dataset("~/Documents/Mobility/Cuebiq_datasets/TRAVEL_HET_MONTH_DAILYLOC_COUNTY_EXPORT_CLEAN") %>%
  collect()

## rename columns
colnames(travel_het_month_daily_county) <- c("start_county", "end_county", "start_date",
                                             "end_date", "trips_per_person", "num_people", "aggregated_total")

## filter only OD pairs that are diff ODs
travel_het_month_daily_county_diffOD <- filter(travel_het_month_daily_county, start_county != end_county)

## clean trips per person (which is really omf) caused by cuebiq exporting weirdness
travel_het_month_daily_county_diffOD <- travel_het_month_daily_county_diffOD %>%
  mutate(
    trips_per_person_clean = map(trips_per_person, function(s) {
      parsed <- fromJSON(s)
      max_index <- max(as.integer(names(parsed)))
      out <- numeric(max_index)
      out[as.integer(names(parsed))] <- parsed
      out
    })
  )

## create heterogeneity in trip frequency indicator
travel_het_month_daily_county_diffOD <- travel_het_month_daily_county_diffOD %>% rowwise() %>%
  mutate(has_het = ifelse(length(trips_per_person_clean) > 1, 1, 0))

table(travel_het_month_daily_county_diffOD$has_het)


### quickly check the max amount of travel in one OD
travel_het_month_daily_county_diffOD_max_travel <- travel_het_month_daily_county_diffOD %>% rowwise() %>%
  mutate(max_trav = length(trips_per_person_clean))

max(travel_het_month_daily_county_diffOD_max_travel$max_trav)


### clean the start and end counties
### first use the fips_codes df to get mapping of state_fips
state_fips <- fips_codes %>%
  distinct(state, state_code) %>%
  deframe()

### with the cuebiq geo ID, seperate into state and county to get clean origin and destination ids that correspond to other datasets
travel_het_month_daily_county_diffOD <- travel_het_month_daily_county_diffOD %>%
  mutate(
    origin_state_abbr = sub("^US\\.([A-Z]{2})\\..*", "\\1", start_county),
    origin_countyfp   = sub("^US\\.[A-Z]{2}\\.([0-9]{3}).*", "\\1", start_county),

    destination_state_abbr = sub("^US\\.([A-Z]{2})\\..*", "\\1", end_county),
    destination_countyfp   = sub("^US\\.[A-Z]{2}\\.([0-9]{3}).*", "\\1", end_county),

    origin_geoid = paste0(state_fips[origin_state_abbr], origin_countyfp),
    destination_geoid = paste0(state_fips[destination_state_abbr], destination_countyfp)
  )



#### read in demographics for counties in mid-atlantic counties
mid_atl_county_full_dems <- read_csv(file= "~/Documents/Mobility/Dems/mid_atl_county_full_dems.csv")
mid_atl_county_full_dems$origin_county <- as.character(mid_atl_county_full_dems$origin_county)
mid_atl_county_full_dems$destination_county <- as.character(mid_atl_county_full_dems$destination_county)

travel_het_month_daily_county_diffOD_dems <- left_join(travel_het_month_daily_county_diffOD,
                                                       mid_atl_county_full_dems,
                                                       by = c(
                                                         "origin_geoid" = "origin_county",
                                                         "destination_geoid" = "destination_county"
                                                       ))

#### make any <10 agg total 5. Calculate prop of origin travel
travel_het_month_daily_county_diffOD_dems <- travel_het_month_daily_county_diffOD_dems %>%
  mutate(aggregated_total_clean = ifelse(aggregated_total == "<10", 5, as.numeric(aggregated_total))) %>%
  group_by(start_county, start_date, end_date) %>%
  mutate(total_trips_per_origin = sum(aggregated_total_clean, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(prop_of_origin_travel = round((aggregated_total_clean / total_trips_per_origin) * 100, 4))


## scale all explanatory variables except grouped.total
travel_het_month_daily_county_diffOD_dems$scaled_distance_km = scale(travel_het_month_daily_county_diffOD_dems$distance_km)
travel_het_month_daily_county_diffOD_dems$scaled_destination_pop_per100k = scale(travel_het_month_daily_county_diffOD_dems$destination_pop_per100k)
travel_het_month_daily_county_diffOD_dems$scaled_origin_pop_per100k = scale(travel_het_month_daily_county_diffOD_dems$origin_pop_per100k)
travel_het_month_daily_county_diffOD_dems$scaled_prop_of_origin_travel = scale(travel_het_month_daily_county_diffOD_dems$prop_of_origin_travel)



#create a binomial model that models the indicator variable
cuebiq_het_model <- glm(has_het ~ log(aggregated_total_clean) + scaled_distance_km + scaled_destination_pop_per100k +
                        scaled_origin_pop_per100k + scaled_prop_of_origin_travel,
                   data = travel_het_month_daily_county_diffOD_dems,
                   family = "binomial"
)

summary.glm(cuebiq_het_model)

#add predictions of the indicator value to the oroginal dataset
travel_het_month_daily_county_diffOD_dems_with_het_preds <- travel_het_month_daily_county_diffOD_dems %>% mutate(has_het_preds = predict.glm(cuebiq_het_model, type = "response"))

cuebiq_roc_curve <- roc(travel_het_month_daily_county_diffOD_dems_with_het_preds$has_het, travel_het_month_daily_county_diffOD_dems_with_het_preds$has_het_preds)

# Find the optimal threshold
cuebiq_optimal_cutoff <- coords(cuebiq_roc_curve, "best", ret = "threshold", best.method = "closest.topleft")

###take only the ODs where there is travel het
travel_het_month_daily_county_diffOD_dems_has_het <- filter(travel_het_month_daily_county_diffOD_dems, has_het == 1)

# Function to create trips per person list for each OD
expand_trips <- function(prob_vec_char, total) {
  prob_vec <- as.numeric(prob_vec_char)
  trip_vals <- seq_along(prob_vec)  # 0-based trip counts
  counts <- round(prob_vec * total)
  rep(trip_vals, counts)
}

#add pois and negbi dist fits to this dataset
travel_het_month_daily_county_diffOD_dems_has_het <- travel_het_month_daily_county_diffOD_dems_has_het %>%
  mutate(trips_per_person = map2(trips_per_person_clean, aggregated_total_clean, ~ expand_trips(as.numeric(.x), .y)))


### chunk data for faster processing
cuebiq_month_daily_diffOD_has_het_chunked_data <- split(travel_het_month_daily_county_diffOD_dems_has_het, (seq_len(nrow(travel_het_month_daily_county_diffOD_dems_has_het)) - 1) %/% 1000)  # 1000 rows per chunk

travel_het_month_daily_county_diffOD_dems_has_het_dists <- map_dfr(cuebiq_month_daily_diffOD_has_het_chunked_data, add_pois_negbi_dists)

travel_het_month_daily_county_diffOD_dems_has_het_dists <- map2_dfr(cuebiq_month_daily_diffOD_has_het_chunked_data, seq_along(cuebiq_month_daily_diffOD_has_het_chunked_data), function(chunk, i) {
  cat("Processing chunk", i, "of", length(cuebiq_month_daily_diffOD_has_het_chunked_data), "\n")
  add_pois_negbi_dists(chunk)
})


###filter for only those that fit to neg bi
cuebiq_diff_negbi <- filter(travel_het_month_daily_county_diffOD_dems_has_het_dists, better_fit == "NegBi")


#model for size
cuebiq.negbi.size.mod <- lm(negbi_size ~ log(aggregated_total_clean) + scaled_distance_km + scaled_destination_pop_per100k +
                       scaled_origin_pop_per100k + scaled_prop_of_origin_travel,
                     data = cuebiq_diff_negbi)
summary.lm(cuebiq.negbi.size.mod)
AIC(cuebiq.negbi.size.mod)

#model for mu
cuebiq.negbi.mu.mod <- lm(negbi_mu ~ log(aggregated_total_clean) + scaled_distance_km + scaled_destination_pop_per100k +
                     scaled_origin_pop_per100k + scaled_prop_of_origin_travel,
                   data = cuebiq_diff_negbi)
summary.lm(cuebiq.negbi.mu.mod)


#### take negative binomial only prediction models from Prediction_models.R
cuebiq.has.het.mod.results <- tidy(cuebiq_het_model) |> filter(term != "(Intercept)") |> mutate(model = "Heterogeneity Indicator")
cuebiq.negbi.size.mod.results <- tidy(cuebiq.negbi.size.mod) |> filter(term != "(Intercept)") |> mutate(model = "NegBi Size")
cuqbiq.negbi.mu.mod.results <- tidy(cuebiq.negbi.mu.mod) |> filter(term != "(Intercept)") |> mutate(model = "NegBi Mu")

### combine all three model results
cuebiq_prediction_results <- rbind(cuebiq.has.het.mod.results, cuebiq.negbi.size.mod.results, cuqbiq.negbi.mu.mod.results)


## plot prediction model results
cuebiq_prediction_results_plot <- dwplot(cuebiq_prediction_results, dot_args = list(size = 3),
                                  model_order = c("Heterogeneity Indicator", "NegBi Size", "NegBi Mu")) %>%
  relabel_predictors(c("log(aggregated_total_clean)" = "Aggregate Trip Count (log)",
                       "scaled_distance_km" = "Distance (km)",
                       "scaled_destination_pop_per100k" = "Destination Population (100k)",
                       "scaled_origin_pop_per100k" = "Origin Population (100k)",
                       "scaled_prop_of_origin_travel" = "Proportion of Origin Travel")) +
  annotate(geom = "text", size = 4, label = "Cuebiq/US", x = 1.25, y = 4.2) +
  scale_color_manual(values = c("Heterogeneity Indicator" = "#482677FF",
                                "NegBi Size" = "#29AF7FFF", "NegBi Mu" = "#B8DE29FF")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.5) +
  theme_minimal(base_size = 12) +
  theme(legend.title = element_blank(),
        legend.position = "top",
        plot.title = element_text(hjust = 0),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank()) +
  scale_x_continuous(limits = c(-1.5, 1.9), breaks = c(-1.5, -1, -0.5, 0.0, 0.5, 1.0, 1.5))
cuebiq_prediction_results_plot

term_labels <- c(
  "log(grouped.total)" = "Aggregate Trip Count",
  "scaled_distance_km" = "Distance (km)",
  "scaled_destination_pop_per100k" = "Destination Population (100k)",
  "scaled_origin_pop_per100k" = "Origin Population (100k)",
  "scaled_prop_of_origin_travel" = "Proportion of Origin Travel"
)

term_order <- rev(unname(term_labels))  # keep the order you want

# Apply recoding and reordering
cuebiq_prediction_results <- cuebiq_prediction_results %>%
  mutate(
    term = recode(term, !!!term_labels),
    term = fct_relevel(term, !!!term_order),
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )


# Basic coefficient plot
cuebiq_new <- ggplot(cuebiq_prediction_results, aes(x = estimate, y = term, color = model)) +
  geom_point(position = position_dodge(width = -0.5), size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = -0.5),
                 height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
  annotate(geom = "text", size = 5, label = "United States", x = 1.25, y = 4.2) +
  scale_color_manual(values = c("#FF6E3A",
                                "#8400CD",
                     "#008DF9")) +
  labs(x = "Coefficient Estimate", y = NULL, color = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 11),
    plot.title = element_text(face = "bold"),
    axis.text.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +  scale_x_continuous(limits = c(-1.5, 2), breaks = c(-1.5, -1, -0.5, 0.0, 0.5, 1.0, 1.5))
cuebiq_new




summary_het_model_test <- as.data.frame(cbind(Beta = coef(het_model_test), confint(het_model_test)))

summary_ind_100_model_test <- as.data.frame(cbind(Beta = coef(ind_100_mod), confint(ind_100_mod)))

summary_het_model_test$model <- "Cuebiq"
summary_ind_100_model_test$model <- "Zambia"

total_mod_sum <- rbind(summary_het_model_test, summary_ind_100_model_test)
total_mod_sum$var_names <- c(rep(c("Intercept", "Log Aggregated Total", "Log Distance",
                                   "Log Destination Pop", "Log Origin Pop", "Prop of Origin Travel"), 2))



ggplot(data = total_mod_sum, aes(x = Beta, y = var_names, color = model, group = model)) +
  geom_point(position = position_dodge(width = 0.3), size = 2) +
  geom_errorbar(aes(xmin = `2.5 %`, xmax = `97.5 %`),
                width = 0.1,
                position = position_dodge(width = 0.3))











#############
######## census tracts
##############

## load in travel het dataset with all trips over a day for census tracts
travel_het_day_all_trips_ct <- open_dataset("~/Documents/Mobility/Cuebiq_datasets/TRAVEL_HET_DAY_ALLTRIPS_CENSUS_TRACT_EXPORT_CLEAN") %>%
  collect()

## rename columns
colnames(travel_het_day_all_trips_ct) <- c("start_ct", "end_ct", "date", "trips_per_person", "num_people", "aggregated_total")

#### make any <10 agg total 5. Calculate prop of origin travel
travel_het_day_all_trips_ct <- travel_het_day_all_trips_ct %>%
  mutate(aggregated_total_clean = ifelse(aggregated_total == "<10", 5, as.numeric(aggregated_total))) %>%
  group_by(start_ct, date) %>%
  mutate(total_trips_per_origin = sum(aggregated_total_clean, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(prop_of_origin_travel = round((aggregated_total_clean / total_trips_per_origin) * 100, 4))

## filter only OD pairs that have over 10 trips and are diff ODs
travel_het_day_all_trips_ct_diffOD <- filter(travel_het_day_all_trips_ct, aggregated_total != "<10")
travel_het_day_all_trips_ct_diffOD <- filter(travel_het_day_all_trips_ct_diffOD, start_ct != end_ct)

setDT(travel_het_day_all_trips_ct_diffOD)

# Parse JSON more efficiently
travel_het_day_all_trips_ct_diffOD[, trips_per_person_clean := lapply(trips_per_person, function(s) {
  parsed <- fromJSON(s)
  max_index <- max(as.integer(names(parsed)))
  out <- numeric(max_index)
  out[as.integer(names(parsed))] <- parsed
  out
})]

travel_het_day_all_trips_ct_diffOD <- travel_het_day_all_trips_ct_diffOD %>% rowwise() %>%
  mutate(has_het = ifelse(length(trips_per_person_clean) > 1, 1, 0))

table(travel_het_day_all_trips_ct_diffOD$has_het)


state_fips <- fips_codes %>%
  distinct(state, state_code) %>%
  deframe()

travel_het_day_all_trips_ct_diffOD <- travel_het_day_all_trips_ct_diffOD %>%
  mutate(
    origin_state_abbr = sub("^US\\.([A-Z]{2})\\..*", "\\1", start_ct),
    origin_countyfp   = sub("^US\\.[A-Z]{2}\\.([0-9]{3})\\..*", "\\1", start_ct),
    origin_tract      = sub("^US\\.[A-Z]{2}\\.[0-9]{3}\\.(.*)", "\\1", start_ct),

    destination_state_abbr = sub("^US\\.([A-Z]{2})\\..*", "\\1", end_ct),
    destination_countyfp   = sub("^US\\.[A-Z]{2}\\.([0-9]{3})\\..*", "\\1", end_ct),
    destination_tract      = sub("^US\\.[A-Z]{2}\\.[0-9]{3}\\.(.*)", "\\1", end_ct),

    origin_geoid = paste0(state_fips[origin_state_abbr], origin_countyfp, origin_tract),
    destination_geoid = paste0(state_fips[destination_state_abbr], destination_countyfp, destination_tract)
  )



states <- c("MD", "VA", "DC", "WV", "DE", "PA", "NY", "NJ")
# Load tracts for selected states and combine
tracts_list <- lapply(states, function(st) {
  tracts(state = st, cb = FALSE, year = 2024)
})
tracts_sf <- do.call(rbind, tracts_list)

# Transform to an appropriate CRS for distance (EPSG:2163 = US National Atlas Equal Area)
tracts_sf <- st_transform(tracts_sf, crs = 2163)

# Get centroids
tracts_sf <- tracts_sf %>%
  mutate(centroid = st_centroid(geometry)) %>%
  select(GEOID, centroid)



# Join origin and destination centroids
travel_het_day_all_trips_ct_diffOD <- travel_het_day_all_trips_ct_diffOD %>%
  left_join(tracts_sf, by = c("origin_geoid" = "GEOID")) %>%
  rename(origin_centroid = centroid) %>%
  left_join(tracts_sf, by = c("destination_geoid" = "GEOID")) %>%
  rename(destination_centroid = centroid)

# Compute distance in kilometers
travel_het_day_all_trips_ct_diffOD <- travel_het_day_all_trips_ct_diffOD %>%
  mutate(
    distance_km = as.numeric(st_distance(origin_centroid, destination_centroid, by_element = TRUE)) / 1000
  )


## remove those without distance
travel_het_day_all_trips_ct_diffOD_with_distance <- filter(travel_het_day_all_trips_ct_diffOD, !is.na(distance_km))

#### add in demographics for census tracts
census_tract_dems <- read.csv("~/Downloads/USA_2020_Census_Population_Characteristics/Census Tract_3.csv")
colnames(census_tract_dems)

census_tract_dems_clean <- census_tract_dems %>% dplyr::select(Geographic.Identifier, Total.Population) %>%
  mutate(Geographic.Identifier = as.character(Geographic.Identifier))

travel_het_day_all_trips_ct_diffOD_with_distance <- travel_het_day_all_trips_ct_diffOD_with_distance %>%
  left_join(census_tract_dems_clean, by = c("origin_geoid" = "Geographic.Identifier")) %>%
  rename(origin_pop = Total.Population) %>%
  mutate(origin_pop_per100k = origin_pop / 100000) %>%
  left_join(census_tract_dems_clean, by = c("destination_geoid" = "Geographic.Identifier")) %>%
  rename(destination_pop = Total.Population) %>%
  mutate(destination_pop_per100k = destination_pop / 100000) %>%
  filter(!is.na(destination_pop_per100k) & !is.na(origin_pop_per100k))




## scale all explanatory variables except grouped.total
travel_het_day_all_trips_ct_diffOD_with_distance$scaled_distance_km = scale(travel_het_day_all_trips_ct_diffOD_with_distance$distance_km)
travel_het_day_all_trips_ct_diffOD_with_distance$scaled_destination_pop_per100k = scale(travel_het_day_all_trips_ct_diffOD_with_distance$destination_pop_per100k)
travel_het_day_all_trips_ct_diffOD_with_distance$scaled_origin_pop_per100k = scale(travel_het_day_all_trips_ct_diffOD_with_distance$origin_pop_per100k)
travel_het_day_all_trips_ct_diffOD_with_distance$scaled_prop_of_origin_travel = scale(travel_het_day_all_trips_ct_diffOD_with_distance$prop_of_origin_travel)



#create a binomial model that models the indicator variable
ct_cuebiq_het_model <- glm(has_het ~ log(aggregated_total_clean) + scaled_distance_km + scaled_destination_pop_per100k +
                          scaled_origin_pop_per100k + scaled_prop_of_origin_travel,
                        data = travel_het_day_all_trips_ct_diffOD_with_distance,
                        family = "binomial"
)

summary.glm(ct_cuebiq_het_model)

predict.glm(ct_cuebiq_het_model, type = "response")

#add predictions of the indicator value to the oroginal dataset
travel_het_day_all_trips_ct_diffOD_with_distance$has_het_preds <- predict.glm(ct_cuebiq_het_model, type = "response")

ct_cuebiq_roc_curve <- roc(travel_het_day_all_trips_ct_diffOD_with_distance$has_het, travel_het_day_all_trips_ct_diffOD_with_distance$has_het_preds)

# Find the optimal threshold
ct_cuebiq_optimal_cutoff <- coords(ct_cuebiq_roc_curve, "best", ret = "threshold", best.method = "closest.topleft")

## scale data for prediction
pred.dat <- may_2024_s2_diffODs
pred.dat$aggregated_total_clean <- pred.dat$grouped.total
pred.dat$scaled_distance_km = scale(pred.dat$distance_km)
pred.dat$scaled_destination_pop_per100k = scale(pred.dat$destination_pop_per100k)
pred.dat$scaled_origin_pop_per100k = scale(pred.dat$origin_pop_per100k)
pred.dat$scaled_prop_of_origin_travel = scale(pred.dat$prop_of_origin_travel)
pred.dat$has_het <- ifelse(pred.dat$percentage_ones == 100, 0, 1)

preds_het <- pred.dat %>% mutate(has_het_pred = ifelse(predict.glm(ct_cuebiq_het_model, newdata = pred.dat, type = "response") > ct_cuebiq_optimal_cutoff$threshold, 1, 0))

table(pred = preds_het$has_het_pred, obs = preds_het$has_het)

(1672+1135) / (1672+1135+97+362)


#add predictions of the indicator value to the oroginal dataset
travel_het_month_daily_county_diffOD_dems_with_het_preds <- travel_het_month_daily_county_diffOD_dems %>% mutate(has_het_preds = predict.glm(cuebiq_het_model, type = "response"))

cuebiq_roc_curve <- roc(travel_het_month_daily_county_diffOD_dems_with_het_preds$has_het, travel_het_month_daily_county_diffOD_dems_with_het_preds$has_het_preds)

# Find the optimal threshold
cuebiq_optimal_cutoff <- coords(cuebiq_roc_curve, "best", ret = "threshold", best.method = "closest.topleft")

###take only the ODs where there is travel het
travel_het_month_daily_county_diffOD_dems_has_het <- filter(travel_het_month_daily_county_diffOD_dems, has_het == 1)

# Function to create trips per person list for each OD
expand_trips <- function(prob_vec_char, total) {
  prob_vec <- as.numeric(prob_vec_char)
  trip_vals <- seq_along(prob_vec)  # 0-based trip counts
  counts <- round(prob_vec * total)
  rep(trip_vals, counts)
}

#add pois and negbi dist fits to this dataset
travel_het_month_daily_county_diffOD_dems_has_het <- travel_het_month_daily_county_diffOD_dems_has_het %>%
  mutate(trips_per_person = map2(trips_per_person_clean, aggregated_total_clean, ~ expand_trips(as.numeric(.x), .y)))


### chunk data for faster processing
cuebiq_month_daily_diffOD_has_het_chunked_data <- split(travel_het_month_daily_county_diffOD_dems_has_het, (seq_len(nrow(travel_het_month_daily_county_diffOD_dems_has_het)) - 1) %/% 1000)  # 1000 rows per chunk

travel_het_month_daily_county_diffOD_dems_has_het_dists <- map2_dfr(cuebiq_month_daily_diffOD_has_het_chunked_data, seq_along(cuebiq_month_daily_diffOD_has_het_chunked_data), function(chunk, i) {
  cat("Processing chunk", i, "of", length(cuebiq_month_daily_diffOD_has_het_chunked_data), "\n")
  add_pois_negbi_dists(chunk)
})

###filter for only those that fit to neg bi
cuebiq_diff_negbi <- filter(travel_het_month_daily_county_diffOD_dems_has_het_dists, better_fit == "NegBi")


#model for size
cuebiq.negbi.size.mod <- lm(negbi_size ~ log(aggregated_total_clean) + scaled_distance_km + scaled_destination_pop_per100k +
                              scaled_origin_pop_per100k + scaled_prop_of_origin_travel,
                            data = cuebiq_diff_negbi)
summary.lm(cuebiq.negbi.size.mod)
AIC(cuebiq.negbi.size.mod)

#model for mu
cuebiq.negbi.mu.mod <- lm(negbi_mu ~ log(aggregated_total_clean) + scaled_distance_km + scaled_destination_pop_per100k +
                            scaled_origin_pop_per100k + scaled_prop_of_origin_travel,
                          data = cuebiq_diff_negbi)
summary.lm(cuebiq.negbi.mu.mod)






############
##### for results section, single trips in het OD pairs for month daily loc
############

## bring in travel het in a month woth daily location at county level
travel_het_month_daily_county <- open_dataset("~/Documents/Mobility/Cuebiq_datasets/TRAVEL_HET_MONTH_DAILYLOC_COUNTY_EXPORT_CLEAN") %>%
  collect()

## rename columns
colnames(travel_het_month_daily_county) <- c("start_county", "end_county", "start_date",
                                             "end_date", "trips_per_person", "num_people", "aggregated_total")

## filter only OD pairs that are diff ODs
travel_het_month_daily_county_diffOD <- filter(travel_het_month_daily_county, start_county != end_county)

## clean the trips per person column
travel_het_month_daily_county_diffOD <- travel_het_month_daily_county_diffOD %>%
  mutate(
    trips_per_person_clean = map(trips_per_person, function(s) {
      parsed <- fromJSON(s)
      max_index <- max(as.integer(names(parsed)))
      out <- numeric(max_index)
      out[as.integer(names(parsed))] <- parsed
      out
    })
  )

## create indicator for having travel het and find percent of OD pairs with het
travel_het_month_daily_county_diffOD <- travel_het_month_daily_county_diffOD %>% rowwise() %>%
  mutate(has_het = ifelse(length(trips_per_person_clean) > 1, 1, 0))

table(travel_het_month_daily_county_diffOD$has_het)

162578/(476860+162578)

## filter for OD pairs with travel het and find the percent of people that only take one trip between an OD pair
th_month_daily_only_het <- filter(travel_het_month_daily_county_diffOD, has_het == 1)
th_month_daily_only_het <- th_month_daily_only_het %>% rowwise() %>%
  mutate(one_trips = trips_per_person_clean[[1]][[1]])

th_month_daily_only_het$trips_per_person_clean[1][[1]][[1]]

median(th_month_daily_only_het$one_trips)




#############
#############
#### cuebiq fig 1
#############
#############

## bring in unique monthly users by county
unique_monthly_users <- open_dataset("~/Documents/Mobility/Cuebiq_datasets/UNIQUE_USERS_PER_MONTH_COUNTY_EXPORT_CLEAN") %>%
  collect()

## assign column names
colnames(unique_monthly_users) <- c("start_date", "end_date", "county", "monthly_users")

## make all observations of <10 into numeric 1
unique_monthly_users <- unique_monthly_users %>%
  mutate(monthly_users_clean = ifelse(monthly_users == "<10", 1, as.numeric(monthly_users)))

## group by county to find monthly average users over 2024
grouped_county_counts <- unique_monthly_users %>% group_by(county) %>%
  summarize(mean_monthly_users = sum(monthly_users_clean)/12)


# State FIPS codes ---
state_fips <- tigris::fips_codes %>%
  distinct(state_code, state_name, state)  # state = abbreviation

# Split county into state and county parts and filter out territories
grouped_county_counts <- grouped_county_counts %>%
  separate(county, into = c("country", "state_abbr", "county_code"), sep = "\\.") %>%
  left_join(state_fips, by = c("state_abbr" = "state")) %>%
  mutate(
    county_code = str_pad(county_code, 3, pad = "0"),
    GEOID = paste0(state_code, county_code)
  ) %>%
  filter(!(state_abbr %in% c("GU", "PR", "VI", "MP", "AS")))  # remove territories


# get US county shapefile from 2020
counties_sf <- counties(cb = TRUE, resolution = "5m", year = 2020)

# Join shapefile monthly users data
# take the log of mean monthly users to increase visibility
monthly_users_map_data <- counties_sf %>%
  left_join(grouped_county_counts, by = "GEOID") %>%
  mutate(
    mean_monthly_users_log = ifelse(mean_monthly_users > 0, log10(mean_monthly_users), NA),
    mean_monthly_users_clean = ifelse(is.na(mean_monthly_users), NA,
                                      ifelse(mean_monthly_users < 10, "<10", as.character(mean_monthly_users)))
  )

## get state outlines so we can highlight select states
state_outline <- states(cb = TRUE, resolution = "20m", year = 2020) %>%
  filter(!STATEFP %in% c("02", "15", "72"))  # Exclude AK, HI, PR

## states to highlight
highlight_states <- c("MD", "VA", "DC", "PA", "NY", "DE", "WV", "NJ")
state_outline_subset <- state_outline %>%
  filter(STUSPS %in% highlight_states)


## lower 48 map of monthly users
monthly_users_plot <- monthly_users_map_data %>%
  filter(!STATEFP %in% c("02", "15", "72")) %>%  # Remove AK (02), HI (15), PR (72)
  ggplot() +
  geom_sf(aes(fill = mean_monthly_users_log), color = NA) +
  geom_sf(data = state_outline_subset, fill = NA, color = "red", linewidth = 0.5) +  # Red outline
  scale_fill_viridis_c(na.value = "gray90", name = "Average monthly\nusers (log)", limits = c(0, 6),
                       breaks = c(0, 2, 4, 6),
                       labels = c(0, 2, 4, 6)) +
  coord_sf(
    xlim = c(-125, -66),  # longitude bounds (approx. mainland US)
    ylim = c(24, 50)      # latitude bounds (approx. mainland US)
  ) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = margin(t = 0, r = 0, b = 0, l = 20),
        legend.title = element_text(size = 8, vjust = 1, hjust = 0.5),
        legend.text = element_text(size = 8)
  )
monthly_users_plot

# Alaska inset
ak_monthly_users_plot <- monthly_users_map_data %>%
  filter(STATEFP == "02") %>%  # Alaska only
  ggplot() +
  geom_sf(aes(fill = mean_monthly_users_log), color = NA) +
  scale_fill_viridis_c(na.value = "gray90", guide = "none", limits = c(0, 6)) +
  coord_sf(
    xlim = c(-180, -130),  # tighter longitude limits
    ylim = c(50, 72),      # tighter latitude limits
    expand = FALSE
  ) +
  theme_void()
ak_monthly_users_plot

# Hawaii inset
hi_monthly_users_plot <- monthly_users_map_data %>%
  filter(STATEFP == "15") %>%  # Hawaii only
  ggplot() +
  geom_sf(aes(fill = mean_monthly_users_log), color = NA) +
  scale_fill_viridis_c(na.value = "gray90", guide = "none", limits = c(0, 6)) +
  theme_void()
hi_monthly_users_plot



######
## bring in OD monthly trips by counties in 2024
OD_ct <- open_dataset("~/Documents/Mobility/Cuebiq_datasets/OD_SELECT_STATES_CENSUS_TRACT_EXPORT_CLEAN") %>%
  collect()

### take only the useful columns and name them
OD_ct <- OD_ct[, c(1,2,4)]
colnames(OD_ct) <- c("start_ct", "end_ct", "daily_trips")

## set as data.table for faster processing
setDT(OD_ct)

# Split the census_tract columns by "." into 4 new columns for each part of the census tract code
OD_ct[, c("start_country", "start_state", "start_county", "start_ct_part") := tstrsplit(start_ct, ".", fixed = TRUE)]
OD_ct[, c("end_country", "end_state", "end_county", "end_ct_part") := tstrsplit(end_ct, ".", fixed = TRUE)]

## create a start and end county variable
OD_ct <- OD_ct %>% mutate(start_county_full = paste0(start_state, ".", start_county),
                          end_county_full = paste0(end_state, ".", end_county))

## filter for only the mid-atlantic states
mid_atl_OD_ct <- filter(OD_ct, start_state %in% c("PA", "MD", "DE", "NY", "NJ", "DC", "VA", "WV"),
                        end_state %in% c("PA", "MD", "DE", "NY", "NJ", "DC", "VA", "WV"))

## for any census tract OD pair that is "<0.5", convert to 0.1. Everything else, convert to numeric
mid_atl_OD_ct_clean <- mid_atl_OD_ct %>% dplyr::select(start_ct, end_ct, start_county_full, end_county_full, daily_trips) %>%
  mutate(daily_trips_clean = ifelse(daily_trips == "<0.5", 0.1, as.numeric(daily_trips)))

## group by start and end county to find daily trips across county OD pairs
mid_atl_OD_by_county <- mid_atl_OD_ct_clean %>% group_by(start_county_full, end_county_full) %>%
  summarize(daily_trips_county = sum(daily_trips_clean))


state_to_county_map <- OD_ct %>% dplyr::select(start_county_full, start_state) %>%
  group_by(start_county_full) %>% slice(1)

### make a OD matrix using average monthly trips
mid_atl_OD_county_mob_matrix <- get_mob_matrix(orig = mid_atl_OD_by_county$start_county_full,
                                               dest = mid_atl_OD_by_county$end_county_full,
                                               value = mid_atl_OD_by_county$daily_trips_county)


plot_mob_matrix_by_state <- function(mob_matrix){
  #turn matrix into dataframe and join to mapping df to match districts to province
  mapped_by_state <- melt(mob_matrix) %>%
    left_join(state_to_county_map, by = c("origin" = "start_county_full")) %>%
    rename(origin_state = start_state) %>%
    left_join(state_to_county_map, by = c("destination" = "start_county_full")) %>%
    rename(destination_state = start_state)


  custom_state_order <- c("NY", "NJ", "PA", "DE", "MD", "DC", "VA", "WV")

  # Make origin_state and destination_state factors
  mapped_by_state <- mapped_by_state %>%
    mutate(
      origin_state = factor(origin_state, levels = custom_state_order),
      destination_state = factor(destination_state, levels = custom_state_order)
    )


  origin_levels <- mapped_by_state %>%
    distinct(origin, origin_state) %>%
    arrange(origin_state, origin) %>%
    pull(origin)

  destination_levels <- mapped_by_state %>%
    distinct(destination, destination_state) %>%
    arrange(destination_state, destination) %>%
    pull(destination)

  mapped_by_state <- mapped_by_state %>%
    mutate(
      origin = factor(origin, levels = origin_levels),
      destination = factor(destination, levels = destination_levels)
    )


  # create breaks where each province ends (y = origin, x = destination)
  origin_breaks <- mapped_by_state %>%
    arrange(origin_state) %>%
    distinct(origin, origin_state) %>%
    group_by(origin_state) %>%
    summarise(n = n()) %>%
    mutate(pos = cumsum(n) + 0.5)

  destination_breaks <- mapped_by_state %>%
    distinct(destination, destination_state) %>%
    count(destination_state, name = "n") %>%
    mutate(pos = cumsum(n) + 0.5)

  destination_labels <- mapped_by_state %>%
    distinct(destination, destination_state) %>%
    count(destination_state) %>%
    mutate(pos = cumsum(n) - n / 2)

  origin_labels <- mapped_by_state %>%
    distinct(origin, origin_state) %>%
    count(origin_state) %>%
    mutate(pos = cumsum(n) - n / 2)

  ggplot(mapped_by_state) +
    geom_tile(aes(x = destination, y = origin, fill = (value))) +

    # Add province separation lines
    geom_vline(xintercept = destination_breaks$pos, color = "white", linewidth = 0.4) +
    geom_hline(yintercept = origin_breaks$pos, color = "white", linewidth = 0.4) +

    # Add province labels using numeric axis positions
    annotate("text",
             x = destination_labels$pos,
             y = rep(0.01, nrow(destination_labels)),  # just below the plot area
             label = destination_labels$destination_state,
             angle = 90, hjust = 1, size = 2) +

    annotate("text",
             y = origin_labels$pos,
             x = rep(0.11, nrow(origin_labels)),  # just to the left
             label = origin_labels$origin_state,
             hjust = 1, size = 2) +
    scale_x_discrete(limits = destination_levels, expand = c(0, 0)) +
    scale_y_discrete(limits = origin_levels, expand = c(0, 0)) +
    scale_fill_viridis(option = 'B', trans = "log",
                       limits = c(0.01, 12000),
                       breaks = c(0.1, 1, 10, 100, 1000, 10000),
                       labels = c("<0.5", "1", "10", "100", "1e3", "1e4")) +
    coord_cartesian(clip = "off") +  # key to allow labels outside plot
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      axis.title.x = element_text(margin = margin(t = 30)),
      axis.title.y = element_text(margin = margin(r = 30)),
      legend.position = "right",
      legend.title = element_blank(),
      legend.text = element_text(size = 8),
      plot.title = element_text(size = 2)
    ) +
    labs(x = "Destination", y = "Origin")
}


### make plot using this OD matrix
avg_mob_county_US_trips_plot <- plot_mob_matrix_by_state(mid_atl_OD_county_mob_matrix)
avg_mob_county_US_trips_plot


quartz()
# Draw final map with AK and HI insets
monthly_users_total_map <- ggdraw() +
  draw_plot(monthly_users_plot, 0, 0, 1, 1) +                        # Full base
  draw_plot(ak_monthly_users_plot, x = 0.01, y = 0.85, width = 0.2, height = 0.2) +  # AK inset
  draw_plot(hi_monthly_users_plot, x = 0.05, y = 0.16, width = 0.15, height = 0.15) +

  # Add arrow from state area (e.g., NY) to matrix region
  draw_line(
    x = c(0.87, 1),
    y = c(0.5, 0.55),
    arrow = arrow(length = unit(0.02, "npc")),
    color = "red",
    linewidth = 0.8)

monthly_users_total_map



