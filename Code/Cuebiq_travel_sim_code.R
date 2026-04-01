######################################
######################################
############### Simulate travel
######################################
######################################
library(reticulate)
library(arrow)

## load in travel het dataset with all max loc per day over a month for county
travel_het_month_daily_county <- open_dataset("~/Documents/Mobility/Cuebiq_datasets/TRAVEL_HET_MONTH_DAILYLOC_COUNTY_EXPORT_CLEAN") %>%
  collect()

## rename columns
colnames(travel_het_month_daily_county) <- c("start_county", "end_county", "start_date",
                                             "end_date", "trips_per_person", "num_people", "aggregated_total")

## clean trips per person column
travel_het_month_daily_county <- travel_het_month_daily_county %>%
  mutate(
    trips_per_person_clean = map(trips_per_person, function(s) {
      parsed <- fromJSON(s)
      max_index <- max(as.integer(names(parsed)))
      out <- numeric(max_index)
      out[as.integer(names(parsed))] <- parsed
      out
    })
  )

## make indicator variable if OD pair has travel het
travel_het_month_daily_county <- travel_het_month_daily_county %>% rowwise() %>%
  mutate(has_het = ifelse(length(trips_per_person_clean) > 1, 1, 0))


### clean the start and end counties
state_fips <- fips_codes %>%
  distinct(state, state_code) %>%
  deframe()

travel_het_month_daily_county <- travel_het_month_daily_county %>%
  mutate(
    origin_state_abbr = sub("^US\\.([A-Z]{2})\\..*", "\\1", start_county),
    origin_countyfp   = sub("^US\\.[A-Z]{2}\\.([0-9]{3}).*", "\\1", start_county),

    destination_state_abbr = sub("^US\\.([A-Z]{2})\\..*", "\\1", end_county),
    destination_countyfp   = sub("^US\\.[A-Z]{2}\\.([0-9]{3}).*", "\\1", end_county),

    origin_geoid = paste0(state_fips[origin_state_abbr], origin_countyfp),
    destination_geoid = paste0(state_fips[destination_state_abbr], destination_countyfp)
  )



#### add in demographics for counties
mid_atl_county_full_dems <- read_csv(file= "~/Documents/Mobility/Dems/mid_atl_county_full_dems.csv")
mid_atl_county_full_dems$origin_county <- as.character(mid_atl_county_full_dems$origin_county)
mid_atl_county_full_dems$destination_county <- as.character(mid_atl_county_full_dems$destination_county)


travel_het_month_daily_county_dems <- left_join(travel_het_month_daily_county,
                                                       mid_atl_county_full_dems,
                                                       by = c(
                                                         "origin_geoid" = "origin_county",
                                                         "destination_geoid" = "destination_county"
                                                       ))

#### make any <10 agg total 5. Calculate prop of origin travel
travel_het_month_daily_county_dems <- travel_het_month_daily_county_dems %>%
  mutate(aggregated_total_clean = ifelse(aggregated_total == "<10", 5, as.numeric(aggregated_total))) %>%
  group_by(start_county, start_date, end_date) %>%
  mutate(total_trips_per_origin = sum(aggregated_total_clean, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(prop_of_origin_travel = round((aggregated_total_clean / total_trips_per_origin) * 100, 4))


save(travel_het_month_daily_county_dems, file = "~/Desktop/travel_het_month_daily_county_dems")



travel_het_month_daily_county_dems$same_OD_ind <- ifelse(travel_het_month_daily_county_dems$start_county ==
                                                           travel_het_month_daily_county_dems$end_county, 1, 0)

#add in observed multiplier, which is applied to districts with heterogeneity that results in the mean distribution of
#likelihood of travel equal to the observed proportion of origin travel. If no heterogeneity, then simply proportion of origin travel
travel_het_month_daily_county_dems <- travel_het_month_daily_county_dems %>% rowwise() %>%
  mutate(obs_pmf = list(trips_per_person_clean),
         people_clean = ifelse(num_people == "<10", aggregated_total_clean / (sum(unlist(obs_pmf) * seq_along(unlist(obs_pmf)))), as.numeric(num_people)),
    obs_multiplier = (people_clean*prop_of_origin_travel)/aggregated_total_clean,
         obs_pmf_clean = ifelse(is.null(obs_pmf), list(1),
                                ifelse(same_OD_ind == 1, list(1), list(obs_pmf))),
    dist_percent = ifelse(same_OD_ind == 0 & has_het == 1,
                          list(c(1:length(obs_pmf)) * obs_multiplier),
                          list(prop_of_origin_travel)))



travel_het_month_daily_county_dems_september <- filter(travel_het_month_daily_county_dems, start_date == "20240901")

# take only the necessary variables
travel_het_month_daily_county_dems_clean <- travel_het_month_daily_county_dems_september %>% dplyr::select(start_county, end_county, same_OD_ind,
                                                                             prop_of_origin_travel, obs_pmf,
                                                                             obs_multiplier, dist_percent, obs_pmf_clean, distance_km)


##### create mobility matrix of observed proportion of origin travel
us_county_mob_matrix_obs <- get_mob_matrix(orig=travel_het_month_daily_county_dems_clean$start_county,
                                           dest=travel_het_month_daily_county_dems_clean$end_county,
                                           value= (travel_het_month_daily_county_dems_clean$prop_of_origin_travel))
us_county_mob_matrix_obs[is.na(us_county_mob_matrix_obs)] = 0

### create a filtered population dataset for the simulation function. Only takes districts in the observed data
counties <- travel_het_month_daily_county_dems_september %>% group_by(start_county) %>%
  slice(1)
us_counties_pops <- counties %>% dplyr::select(start_county, origin_pop_per100k) %>%
  rename(county = start_county, Population = origin_pop_per100k) %>% arrange(county)
us_counties_pops$Population[1] <- 6.89545

#### simulate travel with return for observed districts
#### now simulate travel with return following James Giles paper's parameters
## parameters from paper
beta_0 <- 0.4   # Maximum decay parameter from the plot
alpha <- 0.0014 # Scaling factor

county_dist_data_km <- get_mob_matrix(orig=travel_het_month_daily_county_dems_clean$start_county,
                                      dest=travel_het_month_daily_county_dems_clean$end_county,
                                      value= (travel_het_month_daily_county_dems_clean$distance_km))

# Generate the beta matrix using distances
us_beta_matrix <- compute_beta_ij(county_dist_data_km, beta_0, alpha)



### simulate travel with return with heterogeneity in observed districts
### first create matrix with poot (either multiple values corresponding to number of trips/month or one value if no heterogeneity)
# Convert to matrix format
us_county_df_dist_percent <- travel_het_month_daily_county_dems_clean %>% dplyr::select(start_county, end_county, dist_percent) %>%
  arrange(start_county, end_county) %>%
  pivot_wider(names_from = end_county, values_from = dist_percent, values_fill = list(0))


us_county_dist_percent_matrix <- as.matrix(us_county_df_dist_percent[, -1])  # Remove origin column for matrix format
rownames(us_county_dist_percent_matrix) <- us_county_df_dist_percent$start_county  # Set row names

### now create matrix with probability of each probability of travel (i.e. a 70% chance of having a 0.12% chance of travel from A to B)
# Convert to matrix format
us_county_df_obs_pmf <- travel_het_month_daily_county_dems_clean %>% dplyr::select(start_county, end_county, obs_pmf_clean) %>%
  arrange(start_county, end_county) %>%
  pivot_wider(names_from = end_county, values_from = obs_pmf_clean, values_fill = list(1))

us_county_obs_pmf_matrix <- as.matrix(us_county_df_obs_pmf[, -1])  # Remove origin column for matrix format
rownames(us_county_obs_pmf_matrix) <- us_county_df_obs_pmf$start_county  # Set row names


#######################
#######################
########## simulate travel
#######################
#######################

########
##### simulate 10 dfs using aggregated OD (with return travel)
########
set.seed(3546)
n_people <- 20000
n_timesteps <- 300
# Define folder path
output_folder <- "~/Documents/Mobility/Cuebiq_sims/Simulated_mobility_datasets/Aggregate"
n_simulations = 10


# Run and save each simulation
for (i in 1:n_simulations) {
  df <- simulate_mobility_return(n_people, n_timesteps, us_county_mob_matrix_obs, us_counties_pops, us_beta_matrix, chunk_size = 50)
  df$simulation <- i  # Add a simulation ID
  file_name <- sprintf("%s/sim_agg_%d.csv", output_folder, i)
  fwrite(df, file_name)  # Save as CSV
  print(i)
}




########
##### simulate 10 dfs using OD matrix accounting for heterogeneity
########

set.seed(3546)
n_people <- 20000
n_timesteps <- 300

# Define folder path
output_folder <- "~/Documents/Mobility/Cuebiq_sims/Simulated_mobility_datasets/Travel_het"
n_simulations = 10

# Run and save each simulation
for (i in 1:n_simulations) {
  df <- simulate_mobility_het_obs_only(n_people, n_timesteps, us_county_dist_percent_matrix, us_county_obs_pmf_matrix, pop_data = us_counties_pops,
                                       us_beta_matrix, chunk_size = 50)
  df$simulation <- i  # Add a simulation ID
  file_name <- sprintf("%s/sim_th_%d.csv", output_folder, i)
  fwrite(df, file_name)  # Save as CSV
  print(i)
}



########
##### simulate 10 dfs using aggregated OD and percent of pop that moves
########

set.seed(3546)
n_people <- 20000
n_timesteps <- 300

# Define folder path
output_folder <- "~/Documents/Mobility/Cuebiq_sims/Simulated_mobility_datasets/Move_het"
n_simulations = 10

# Run and save each simulation
for (i in 1:n_simulations) {
  df <- simulate_mobility_return_prob_moving(n_people, n_timesteps, us_county_mob_matrix_obs, us_counties_pops,
                                             us_beta_matrix, never_mover_prob = 0.63, chunk_size = 1)
  df$simulation <- i  # Add a simulation ID
  file_name <- sprintf("%s/sim_mh_%d.csv", output_folder, i)
  fwrite(df, file_name)  # Save as CSV
  print(i)
}

########
##### simulate 10 dfs using OD matrix accounting for travel heterogeneity and percent non movers
########

set.seed(3546)
n_people <- 20000
n_timesteps <- 300

# Define folder path
output_folder <- "~/Documents/Mobility/Cuebiq_sims/Simulated_mobility_datasets/Both_het"
n_simulations = 10

# Run and save each simulation
for (i in 9:n_simulations) {
  df <-  simulate_mobility_het_obs_only_prob_move(n_people, n_timesteps, us_county_dist_percent_matrix, us_county_obs_pmf_matrix, pop_data = us_counties_pops,
                                                  us_beta_matrix, never_mover_prob = 0.63, chunk_size = 1)
  df$simulation <- i  # Add a simulation ID
  file_name <- sprintf("%s/sim_bh_%d.csv", output_folder, i)
  fwrite(df, file_name)  # Save as CSV
  print(i)
}


## create dataframe of us county names (in mid-atlantic) and save as a csv
county_names <- us_counties_pops %>% dplyr::select(county)
write_csv(county_names, file = "~/Documents/Mobility/Cuebiq_sims/county_names.csv")



#######################
#######################
########## starsim results
#######################
#######################


### load in all python objects for each locaiton strategy and seeding location for COVID-19

cuebiq_agg_manhattan_COVID <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Aggregate/Agg_COVID_Manhattan_sims.pkl")
cuebiq_agg_elk_COVID <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Aggregate/Agg_COVID_Elk_sims.pkl")
cuebiq_agg_howard_COVID <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Aggregate/Agg_COVID_Howard_sims.pkl")

cuebiq_th_manhattan_COVID <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Travel_het/Travel_het_COVID_Manhattan_sims.pkl")
cuebiq_th_elk_COVID <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Travel_het/Travel_het_COVID_Elk_sims.pkl")
cuebiq_th_howard_COVID <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Travel_het/Travel_het_COVID_Howard_sims.pkl")

cuebiq_mh_manhattan_COVID <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Move_het/Move_het_COVID_Manhattan_sims.pkl")
cuebiq_mh_elk_COVID <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Move_het/Move_het_COVID_Elk_sims.pkl")
cuebiq_mh_howard_COVID <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Move_het/Move_het_COVID_Howard_sims.pkl")

cuebiq_bh_manhattan_COVID <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Both_het/Both_het_COVID_Manhattan_sims.pkl")
cuebiq_bh_elk_COVID <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Both_het/Both_het_COVID_Elk_sims.pkl")
cuebiq_bh_howard_COVID <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Both_het/Both_het_COVID_Howard_sims.pkl")


### put all the COVID sims together
cuebiq_covid_all_sims <- list(
  Aggregate = list(Urban = cuebiq_agg_manhattan_COVID, Rural = cuebiq_agg_elk_COVID, "Semi-urban" = cuebiq_agg_howard_COVID),
  Travel_het = list(Urban = cuebiq_th_manhattan_COVID, Rural = cuebiq_th_elk_COVID, "Semi-urban" = cuebiq_th_howard_COVID),
  Move_het = list(Urban = cuebiq_mh_manhattan_COVID, Rural = cuebiq_mh_elk_COVID, "Semi-urban" = cuebiq_mh_howard_COVID),
  Both_het = list(Urban = cuebiq_bh_manhattan_COVID, Rural = cuebiq_bh_elk_COVID, "Semi-urban" = cuebiq_bh_howard_COVID)
)

###create dataframe for plotting COVID simulation results
cuebiq_covid_infections_df <- make_sim_results_df_for_plot(cuebiq_covid_all_sims)

### plot COVID-19 simulations
cuebiq_covid_new_infections_plot <- ggplot(cuebiq_covid_infections_df) +
  geom_line(aes(x = step, y = new_infections, group = sim, color = group, alpha = highlight), linewidth = 1) +
  scale_y_continuous(position = "right", breaks = pretty_breaks(3)) +
  scale_x_continuous(breaks = c(0, 50, 100), limits = c(0, 100)) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.1), guide = "none") +
  scale_color_viridis(discrete = T, begin = 0.1, end = 0.8, option = "B") +
  facet_grid(group ~ start_loc, switch="y",
             labeller = labeller(group = c("Move_het" = "Aggregate +\nproportion travelled",
                                           "Travel_het" = "Aggregate +\ntrip frequency",
                                           "Both_het" = "Aggregate +\nproportion travelled +\ntrip frequency"))) +
  labs(title = "SARS-Cov-2",
       x = "Timestep") +
  theme_minimal() +
  theme(
    strip.text.x = element_text(face = "bold.italic"),
    strip.text.y = element_blank(),
    legend.position = "none",
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5))
cuebiq_covid_new_infections_plot

### create table for comparing simulation descriptive results

cuebiq_COVID_sims_table <- compare_table_func(folder_name = "~/Documents/Mobility/Zambia/Starsim_results",
                                       c("Actual/Actual_COVID_Lusaka",
                                         "Actual/Actual_COVID_Kalulushi",
                                         "Actual/Actual_COVID_Luampa",
                                         "Aggregate/Aggregate_COVID_Lusaka",
                                         "Aggregate/Aggregate_COVID_Kalulushi",
                                         "Aggregate/Aggregate_COVID_Luampa",
                                         "Move_het/Move_het_COVID_Lusaka",
                                         "Move_het/Move_het_COVID_Kalulushi",
                                         "Move_het/Move_het_COVID_Luampa",
                                         "Travel_het/Travel_het_COVID_Lusaka",
                                         "Travel_het/Travel_het_COVID_Kalulushi",
                                         "Travel_het/Travel_het_COVID_Luampa",
                                         "Both_het/Both_het_COVID_Lusaka",
                                         "Both_het/Both_het_COVID_Kalulushi",
                                         "Both_het/Both_het_COVID_Luampa"),
                                       pops.filt, c(rep(150, 15)))




### load in all python objects for each locaiton strategy and seeding location for Flu

cuebiq_agg_manhattan_Flu <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Aggregate/Agg_Flu_Manhattan_sims.pkl")
cuebiq_agg_elk_Flu <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Aggregate/Agg_Flu_Elk_sims.pkl")
cuebiq_agg_howard_Flu <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Aggregate/Agg_Flu_Howard_sims.pkl")

cuebiq_th_manhattan_Flu <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Travel_het/Travel_het_Flu_Manhattan_sims.pkl")
cuebiq_th_elk_Flu <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Travel_het/Travel_het_Flu_Elk_sims.pkl")
cuebiq_th_howard_Flu <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Travel_het/Travel_het_Flu_Howard_sims.pkl")

cuebiq_mh_manhattan_Flu <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Move_het/Move_het_Flu_Manhattan_sims.pkl")
cuebiq_mh_elk_Flu <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Move_het/Move_het_Flu_Elk_sims.pkl")
cuebiq_mh_howard_Flu <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Move_het/Move_het_Flu_Howard_sims.pkl")

cuebiq_bh_manhattan_Flu <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Both_het/Both_het_Flu_Manhattan_sims.pkl")
cuebiq_bh_elk_Flu <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Both_het/Both_het_Flu_Elk_sims.pkl")
cuebiq_bh_howard_Flu <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Both_het/Both_het_Flu_Howard_sims.pkl")


### put all the Flu sims together
cuebiq_Flu_all_sims <- list(
  Aggregate = list(Urban = cuebiq_agg_manhattan_Flu, Rural = cuebiq_agg_elk_Flu, "Semi-urban" = cuebiq_agg_howard_Flu),
  Travel_het = list(Urban = cuebiq_th_manhattan_Flu, Rural = cuebiq_th_elk_Flu, "Semi-urban" = cuebiq_th_howard_Flu),
  Move_het = list(Urban = cuebiq_mh_manhattan_Flu, Rural = cuebiq_mh_elk_Flu, "Semi-urban" = cuebiq_mh_howard_Flu),
  Both_het = list(Urban = cuebiq_bh_manhattan_Flu, Rural = cuebiq_bh_elk_Flu, "Semi-urban" = cuebiq_bh_howard_Flu)
)

###create dataframe for plotting Flu simulation results
cuebiq_Flu_infections_df <- make_sim_results_df_for_plot(cuebiq_Flu_all_sims)

cuebiq_Flu_new_infections_plot <- ggplot(cuebiq_Flu_infections_df) +
  geom_line(aes(x = step, y = new_infections, group = sim, color = group, alpha = highlight), linewidth = 1) +
  scale_y_continuous(position = "right", breaks = pretty_breaks(3)) +
  scale_x_continuous(breaks = c(0, 100, 200, 300), limits = c(0, 300)) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.10), guide = "none") +
  scale_color_viridis(discrete = T, begin = 0.1, end = 0.8, option = "B") +
  facet_grid(group ~ start_loc, switch="y",
             labeller = labeller(group = c("Move_het" = "Aggregate +\nproportion travelled",
                                           "Travel_het" = "Aggregate +\ntrip frequency",
                                           "Both_het" = "Aggregate +\nproportion travelled +\ntrip frequency"))) +
  labs(title = "Influenza",
       x = "Timestep") +
  theme_minimal() +
  theme(
    strip.text.x = element_text(face = "bold.italic"),
    strip.text.y = element_text(face = "bold.italic"),
    legend.position = "none",
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5))
cuebiq_Flu_new_infections_plot




### load in all python objects for each locaiton strategy and seeding location for Measles
cuebiq_agg_manhattan_Measles <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Aggregate/Agg_Measles_Manhattan_sims.pkl")
cuebiq_agg_elk_Measles <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Aggregate/Agg_Measles_Elk_sims.pkl")
cuebiq_agg_howard_Measles <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Aggregate/Agg_Measles_Howard_sims.pkl")

cuebiq_th_manhattan_Measles <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Travel_het/Travel_het_Measles_Manhattan_sims.pkl")
cuebiq_th_elk_Measles <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Travel_het/Travel_het_Measles_Elk_sims.pkl")
cuebiq_th_howard_Measles <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Travel_het/Travel_het_Measles_Howard_sims.pkl")

cuebiq_mh_manhattan_Measles <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Move_het/Move_het_Measles_Manhattan_sims.pkl")
cuebiq_mh_elk_Measles <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Move_het/Move_het_Measles_Elk_sims.pkl")
cuebiq_mh_howard_Measles <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Move_het/Move_het_Measles_Howard_sims.pkl")

cuebiq_bh_manhattan_Measles <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Both_het/Both_het_Measles_Manhattan_sims.pkl")
cuebiq_bh_elk_Measles <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Both_het/Both_het_Measles_Elk_sims.pkl")
cuebiq_bh_howard_Measles <- py_load_object("~/Documents/Mobility/Cuebiq_sims/Starsim_sims/Both_het/Both_het_Measles_Howard_sims.pkl")


### put all the Measles sims together
cuebiq_Measles_all_sims <- list(
  Aggregate = list(Urban = cuebiq_agg_manhattan_Measles, Rural = cuebiq_agg_elk_Measles, "Semi-urban" = cuebiq_agg_howard_Measles),
  Travel_het = list(Urban = cuebiq_th_manhattan_Measles, Rural = cuebiq_th_elk_Measles, "Semi-urban" = cuebiq_th_howard_Measles),
  Move_het = list(Urban = cuebiq_mh_manhattan_Measles, Rural = cuebiq_mh_elk_Measles, "Semi-urban" = cuebiq_mh_howard_Measles),
  Both_het = list(Urban = cuebiq_bh_manhattan_Measles, Rural = cuebiq_bh_elk_Measles, "Semi-urban" = cuebiq_bh_howard_Measles)
)

###create dataframe for plotting Measles simulation results
cuebiq_Measles_infections_df <- make_sim_results_df_for_plot(cuebiq_Measles_all_sims)

cuebiq_Measles_new_infections_plot <- ggplot(cuebiq_Measles_infections_df) +
  geom_line(aes(x = step, y = new_infections, group = sim, color = group, alpha = highlight), linewidth = 1) +
  scale_y_continuous(position = "right", breaks = pretty_breaks(3)) +
  scale_x_continuous(breaks = c(0, 50, 100), limits = c(0, 100)) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.1), guide = "none") +
  scale_color_viridis(discrete = T, begin = 0.1, end = 0.8, option = "B") +
  facet_grid(group ~ start_loc, switch="y",
             labeller = labeller(group = c("Move_het" = "Aggregate +\nproportion travelled",
                                           "Travel_het" = "Aggregate +\ntrip frequency",
                                           "Both_het" = "Aggregate +\nproportion travelled +\ntrip frequency"))) +
  labs(title = "Measles",
       x = "Timestep") +
  theme_minimal() +
  theme(
    strip.text.x = element_text(face = "bold.italic"),
    strip.text.y = element_blank(),
    legend.position = "none",
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5))
cuebiq_Measles_new_infections_plot


### combine all three plots for total Cuebiq sim results plot
cuebiq_simplot <- (cuebiq_Flu_new_infections_plot) + cuebiq_covid_new_infections_plot + cuebiq_Measles_new_infections_plot & theme(plot.tag = element_text(face = "bold")) +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
cuebiq_simplot




