##### load in required packages
library(dplyr)
library(tidyverse)
library(ggplot2)
library(viridis)
library(sf)
library(gridExtra)
library(mobility)
library(dotwhisker)
library(broom)
library(scales)
library(ggplotify)
library(patchwork)
library(broom)
library(broom.helpers)
library(arrow)

#### census tract
ct_dems <- read.csv("~/Documents/Mobility/Accessibility Measures/census_tracts_max_block_id_travel_dems.csv")


### county

counties_max_tract_travel_dems <- read_csv(file = "~/Documents/Mobility/Dems/counties_max_tract_travel_dems.csv")


## bring in move het month daily loc dataset at couny level
move_het_month_daily_county <- open_dataset("~/Documents/Mobility/Cuebiq_datasets/MOVE_HET_MONTH_DAILYLOC_COUNTY_EXPORT_CLEAN") %>%
  collect()

## assign column names
colnames(move_het_month_daily_county) <- c("start_date", "end_date", "county", "perc_non_movers", "total_users")

# --- Step 1: State FIPS codes ---
state_fips <- tigris::fips_codes %>%
  distinct(state_code, state_name, state)  # state = abbreviation

# --- Step 2: Split "AL.103" into state and county parts ---
move_het_month_daily_county <- move_het_month_daily_county %>%
  separate(county, into = c("country", "state_abbr", "county_code"), sep = "\\.") %>%
  left_join(state_fips, by = c("state_abbr" = "state")) %>%
  mutate(
    county_code = str_pad(county_code, 3, pad = "0"),
    GEOID = paste0(state_code, county_code)
  ) %>%
  filter(!(state_abbr %in% c("GU", "PR", "VI", "MP", "AS")))  # remove territories


# --- Step 3: Get US counties shapefile ---
counties_sf <- counties(cb = TRUE, resolution = "5m", year = 2020)

# --- Step 4: Join shapefile with your data ---
# --- Merge with shapefile and transform value ---
move_het_map_plot_data <- counties_sf %>%
  left_join(move_het_month_daily_county, by = "GEOID")


# transform to percent movers
move_het_map_plot_data$perc_movers <- 100 - move_het_map_plot_data$perc_non_movers

move_het_map_plot <- move_het_map_plot_data %>%
  filter(!STATEFP %in% c("02", "15", "72")) %>%  # Remove AK (02), HI (15), PR (72)
  ggplot() +
  geom_sf(aes(fill = perc_movers), color = NA) +
  scale_fill_viridis_c(na.value = "gray90", name = "Percent\nmovers", limits = c(0, 100)) +
  coord_sf(
    xlim = c(-125, -66),  # longitude bounds (approx. mainland US)
    ylim = c(24, 50)      # latitude bounds (approx. mainland US)
  ) +
  theme_void() +
  theme(legend.position = "right",
            plot.title = element_text(hjust = 0.5),
            legend.title = element_text(hjust = 1))
move_het_map_plot



labs(title = "Percent of people that move from their home\ncounty over a month (using maximum location per day)")

##### pred model

### merge move het df with dems
move_het_month_daily_county_dems <- merge(move_het_month_daily_county, counties_max_tract_travel_dems,
                                          by.x = "GEOID", by.y = "origin_county")
move_het_month_daily_county_dems$perc_non_movers_clean = move_het_month_daily_county_dems$perc_non_movers / 100


### create model
test_mod <- glm(perc_non_movers_clean ~ mean_time + Pop_density_sqkm + Percent_pop_rural +
                  factor(region) + factor(state),
                data = move_het_month_daily_county_dems, family = "binomial")
summary.glm(test_mod)

tidy_test <- tidy_plus_plus(test_mod)

tidy_test2 <- tidy_test[c(1:3),] %>% dplyr::select(term, estimate, conf.low, conf.high)

test2 <- as.data.frame(cbind(Beta = coef(test_mod), confint(test_mod)))
test2 <- test2[c(1:4),]


#### look at specific states
nj_counties <- filter(move_het_month_daily_county_dems, abbr == "NJ")
ca_counties <- filter(move_het_month_daily_county_dems, abbr == "CA")
wy_counties <- filter(move_het_month_daily_county_dems, abbr == "WY")
ak_counties <- filter(move_het_month_daily_county_dems, abbr == "AK")

nj_mod <- glm(perc_non_movers_clean ~ mean_time + Pop_density_sqkm + Percent_pop_rural,
                data = nj_counties, family = "binomial")
summary.glm(nj_mod)

ca_mod <- glm(perc_non_movers_clean ~ mean_time + Pop_density_sqkm + Percent_pop_rural,
              data = ca_counties, family = "binomial")
summary.glm(ca_mod)

wy_mod <- glm(perc_non_movers_clean ~ mean_time + Pop_density_sqkm + Percent_pop_rural,
              data = wy_counties, family = "binomial")
summary.glm(wy_mod)

ak_mod <- glm(perc_non_movers_clean ~ mean_time + Pop_density_sqkm + Percent_pop_rural,
              data = ak_counties, family = "binomial")
summary.glm(ak_mod)



### load in demographic information
counties_max_tract_travel_dems <- read_csv(file = "~/Documents/Mobility/Dems/counties_max_tract_travel_dems.csv")

census_tracts_max_block_id_travel_dems <- read_csv(file = "~/Documents/Mobility/Dems/census_tracts_max_block_id_travel_dems.csv")


library(broom.helpers)
#### make preds function for county and census tract
### takes list of dataframes in form "month_het" = month_het_daily
move_het_preds_county <- function(list_of_dfs){
  results <- data.frame("label" = character(),
                        "estimate" = integer(),
                        "conf.low" = integer(),
                        "conf.high" = integer(),
                        "model" = character())

  for (model_name in names(list_of_dfs)) {
    df <- list_of_dfs[[model_name]]

    ### for each dataframe, get a GEOID from the cuebiq form of county
    with_geoid <- move_het_month_daily_county %>%
      separate(county, into = c("country", "state_abbr", "county_code"), sep = "\\.") %>%
      left_join(state_fips, by = c("state_abbr" = "state")) %>%
      mutate(
        county_code = str_pad(county_code, 3, pad = "0"),
        GEOID = paste0(state_code, county_code)
      ) %>%
      filter(!(state_abbr %in% c("GU", "PR", "VI", "MP", "AS")))  # remove territories

    ## merge with county demographics
    merged_with_dems <- merge(with_geoid, counties_max_tract_travel_dems,
                              by.x = "GEOID", by.y = "origin_county")

    ## convert movers to proportion
    merged_with_dems$perc_movers_clean = merged_with_dems$perc_movers / 100

    ## run model
    mod <- glm(perc_movers_clean ~ scale(mean_time) + scale(Pop_density_sqkm) +
                 scale(Percent_pop_rural) + scale(Land_area_sqkm) + factor(region) + factor(state),
                    data = merged_with_dems, family = "binomial")

    ## extract coefficeint estimates and confidence intervals
    mod_sum <- tidy_plus_plus(mod)
    mod_sum <- mod_sum[c(1:4),] %>% dplyr::select(label, estimate, conf.low, conf.high)
    mod_sum$model <- model_name
    print(model_name)

    results <- rbind(results, mod_sum)

  }
  return(results)
}


### census tract version
move_het_preds_ct <- function(list_of_dfs){
  results <- data.frame("Beta" = integer(),
                        "2.5 %" = integer(),
                        "97.5 %" = integer(),
                        "model" = character())

  for (model_name in names(list_of_dfs)) {
    df <- list_of_dfs[[model_name]]

    merged_with_dems <- merge(move_het_month_daily_ct, census_tracts_max_block_id_travel_dems,
                              by = "census_tract")

    merged_with_dems$perc_movers_clean = merged_with_dems$perc_movers / 100

    mod <- glm(perc_movers_clean ~ scale(mean_time) + scale(Pop_density_sqkm) +
                 scale(Percent_pop_rural) + scale(Land_area_sqkm) + factor(region) + factor(state),
               data = merged_with_dems, family = "binomial")

    mod_sum <- tidy_plus_plus(mod)
    mod_sum <- mod_sum[c(1:4),] %>% dplyr::select(label, estimate, conf.low, conf.high)
    mod_sum$model <- model_name
    print(model_name)

    results <- rbind(results, mod_sum)

  }
  return(results)
}


### bring in all datasets, rename columns and create a percent movers (perc_movers) column
move_het_day_8hr_county <- open_dataset("~/Documents/Mobility/Cuebiq_datasets/MOVE_HET_DAY_8HRLOC_COUNTY_EXPORT_CLEAN") %>%
  collect()
colnames(move_het_day_8hr_county) <- c("date", "county", "perc_non_movers", "total_users")

move_het_day_8hr_ct <- open_dataset("~/Documents/Mobility/Cuebiq_datasets/MOVE_HET_DAY_8HRLOC_CENSUS_TRACT_EXPORT_CLEAN") %>%
  collect()
colnames(move_het_day_8hr_ct) <- c("date", "census_tract", "perc_non_movers", "total_users")

move_het_day_alltrips_county <- open_dataset("~/Documents/Mobility/Cuebiq_datasets/MOVE_HET_DAY_ALLTRIPS_COUNTY_EXPORT_CLEAN") %>%
  collect()
colnames(move_het_day_alltrips_county) <- c("date", "county", "perc_non_movers", "total_users")

move_het_day_alltrips_ct <- open_dataset("~/Documents/Mobility/Cuebiq_datasets/MOVE_HET_DAY_ALLTRIPS_CENSUS_TRACT_EXPORT_CLEAN") %>%
  collect()
colnames(move_het_day_alltrips_ct) <- c("date", "census_tract", "perc_non_movers", "total_users")

move_het_week_alltrips_county <- open_dataset("~/Documents/Mobility/Cuebiq_datasets/MOVE_HET_WEEK_ALLTRIPS_COUNTY_EXPORT_CLEAN") %>%
  collect()
colnames(move_het_week_alltrips_county) <- c("start_date", "end_date", "county", "perc_non_movers", "total_users")

move_het_week_alltrips_ct <- open_dataset("~/Documents/Mobility/Cuebiq_datasets/MOVE_HET_WEEK_ALLTRIPS_CENSUS_TRACT_EXPORT_CLEAN") %>%
  collect()
colnames(move_het_week_alltrips_ct) <- c("start_date", "end_date", "census_tract", "perc_non_movers", "total_users")

move_het_month_daily_county <- open_dataset("~/Documents/Mobility/Cuebiq_datasets/MOVE_HET_MONTH_DAILYLOC_COUNTY_EXPORT_CLEAN") %>%
  collect()
colnames(move_het_month_daily_county) <- c("start_date", "end_date", "county", "perc_non_movers", "total_users")
move_het_month_daily_county$perc_movers <- 100 - move_het_month_daily_county$perc_non_movers

move_het_month_daily_ct <- open_dataset("~/Documents/Mobility/Cuebiq_datasets/MOVE_HET_MONTH_DAILYLOC_CENSUS_TRACT_EXPORT_CLEAN") %>%
  collect()
colnames(move_het_month_daily_ct) <- c("start_date", "end_date", "census_tract", "perc_non_movers", "total_users")
move_het_month_daily_ct$perc_movers <- 100 - move_het_month_daily_ct$perc_non_movers


### run for just the county and census tract dataset corresponding to Zambia data (daily loc for a month)
move_het_month_daily_ct_results <- move_het_preds_ct(list(
  "Month_daily_ct" = move_het_month_daily_ct
))

move_het_month_daily_county_results <- move_het_preds_county(list(
  "Month_daily_county" = move_het_month_daily_county
))


### combine the two and rename the labels for cleaner plots
move_het_month_daily_results_combined <- rbind(move_het_month_daily_ct_results, move_het_month_daily_county_results)
move_het_month_daily_results_combined$var = rep(c("Mean time between\nsmall admin units", "Population density\n(square km)", "Percent of population\nthat is rural", "Land area (square km)"), 2)

move_het_month_daily_results_combined$model <- factor(
  move_het_month_daily_results_combined$model,
  levels = c("Month_daily_ct", "Month_daily_county")
)

### plot both
dotplot_move_het_month_daily <- ggplot(data = move_het_month_daily_results_combined,
                           aes(x = estimate, y = var, color = model, group = model)) +
  geom_point(position = position_dodge(width = 0.3), size = 2) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                width = 0.1,
                position = position_dodge(width = 0.3)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.5) +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 0.5),
        legend.position = "top") +
  guides(color = guide_legend(title = NULL)) +
  scale_color_viridis(discrete = T, option = "B", begin = 0.4, end = 0.7,
                      breaks=c('Month_daily_county', 'Month_daily_ct'),
                      labels = c("County", "Census Tract")) +
  ylab("Variable") +
  xlab("Estimate")
dotplot_move_het_month_daily



#### combine the map and above dotplot for the movement heterogeneity figure plot
move_het_fig_layout <- "
  1
  2"

quartz()
move_het_fig <- (move_het_map_plot) + (dotplot_move_het_month_daily) +
  plot_layout(design = move_het_fig_layout, widths = c(1, 1),
              heights = c(1,0.6)) +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = "bold"))
move_het_fig

ggsave(move_het_fig, filename = "~/Desktop/test_move_het_fig.png", width = 9, height = 7.5, units = "in")




### run the function for pred model
move_het_pred_model_results <- move_het_preds(list(
  "Day_8hr_county" = move_het_day_8hr_county,
  "Day_8hr_ct" = move_het_day_8hr_ct,
  "Day_alltrips_county" = move_het_day_alltrips_county,
  "Day_alltrips_ct" = move_het_day_alltrips_ct,
  "Week_alltrips_county" = move_het_week_alltrips_county,
  "Week_alltrips_ct" = move_het_week_alltrips_ct,
  "Month_daily_county" = move_het_month_daily_county,
  "Month_daily_ct" = move_het_month_daily_ct
))


move_het_pred_model_results <- move_het_preds_county(list(
  "Day_8hr_county" = move_het_day_8hr_county,
  "Day_alltrips_county" = move_het_day_alltrips_county,
  "Week_alltrips_county" = move_het_week_alltrips_county,
  "Month_daily_county" = move_het_month_daily_county
))




move_het_month_daily_ct_results <- move_het_preds_ct(list(
  "Month_daily_ct" = move_het_month_daily_ct
))

move_het_month_daily_county_results <- move_het_preds_county(list(
  "Month_daily_county" = move_het_month_daily_county
))

results_df <- rbind(move_het_month_daily_ct_results, move_het_month_daily_county_results)
results_df <- results_df %>% mutate(var = rep(c("Intercept", "mean_time", "Pop_density_sqkm", "Percent_pop_rural", "Area_km"), 2))



dotplot_move_het <- ggplot(data = filter(results_df, var != "Intercept"), aes(x = Beta, y = var, color = model, group = model)) +
  geom_point(position = position_dodge(width = 0.3), size = 2) +
  geom_errorbar(aes(xmin = `2.5%`, xmax = `97.5%`),
                width = 0.1,
                position = position_dodge(width = 0.3))
dotplot_move_het

move_het_fig_layout <- "
  1
2"

move_het_fig <- (move_het_map_plot) + (dotplot_move_het) +
  plot_layout(design = move_het_fig_layout, widths = c(1, 1),
              heights = c(1,0.5)) +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = "bold")) +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
move_het_fig
