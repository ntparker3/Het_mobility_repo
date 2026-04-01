library(tidyverse)
library(epidatr)
library(epiprocess)
library(epipredict)
library(arrow)
library(data.table)
library(mobility)
library(reticulate)
library(patchwork)



#### pull 7day incidence for all of us
cases_national <- pub_covidcast(
  source = "jhu-csse",
  signals = "confirmed_7dav_incidence_prop",
  time_type = "day",
  geo_type = "nation",
  time_values = epirange(20200601, 20210501))

## plot national cases
ggplot() +
  geom_line(data = cases_national, aes(x = time_value, y = value), color = "red", alpha = 0.5) +
  theme_minimal()


#### pull 7day incidence for all US states
cases_states <- pub_covidcast(
  source = "jhu-csse",
  signals = "confirmed_7dav_incidence_prop",
  time_type = "day",
  geo_type = "state",
  time_values = epirange(20200601, 20210501))


## plot national cases and every state's cases
ggplot() +
  geom_line(data = cases_national, aes(x = time_value, y = value), color = "black", alpha = 1) +
  geom_line(data = cases_states, aes(x = time_value, y = value, color = geo_value), alpha = 0.5) +
  theme_minimal()

## look specifically at maryland and california compared to national
ggplot() +
  geom_line(data = cases_national, aes(x = time_value, y = value, color = "US"), alpha = 1) +
  geom_line(data = filter(cases_states, geo_value == "pa"), aes(x = time_value, y = value, color = "Pennsylvania"), alpha = 1) +
  geom_line(data = filter(cases_states, geo_value == "md"), aes(x = time_value, y = value, color = "Maryland"), alpha = 1) +
  scale_x_date(limits = c(as.Date("20201001", "%Y%m%d"), as.Date("20210301", "%Y%m%d")),
               date_labels = ("%b, %Y"),
               date_breaks = "1 months") +
  xlab("Date") +
  ylab("7-Day Incidence") +
  scale_color_manual(values = c("US" = "black", "Pennsylvania" = "#B1325AFF", "Maryland" = "#F17020FF"),
                     breaks = c("US", "Maryland", "Pennsylvania")) +
  guides(color = guide_legend(title = element_blank())) +
  theme_minimal()


#### pull 7day incidence for all US counties
cases_county <- pub_covidcast(
  source = "jhu-csse",
  signals = "confirmed_7dav_incidence_prop",
  time_type = "day",
  geo_type = "county",
  time_values = epirange(20200601, 20210501))

## select only useful counties
countycases <- cases_county %>% dplyr::select(geo_value, time_value, cases = value)

# Extract state and county parts
countycases$state_part <- substr(countycases$geo_value, 1, 2)
countycases$county_part <- substr(countycases$geo_value, 3, 5)

## dataframe for matching states to fip abbreviations
state_fips_to_abbr <- data.frame(
  state_fips = sprintf("%02d", c(1:56)),
  abbr = c("AL","AK", NA,"AZ","AR","CA",NA, "CO","CT","DE","DC",
           "FL","GA", NA,"HI","ID","IL","IN","IA","KS","KY",
           "LA","ME","MD","MA","MI","MN","MS","MO","MT","NE",
           "NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR",
           "PA", NA,"RI","SC","SD","TN","TX","UT","VT","VA",
           NA,"WA","WV","WI","WY"),
  stringsAsFactors = FALSE
)

## match countycases dataframe to states
countycases <- merge(countycases, state_fips_to_abbr, by.x = "state_part", by.y = "state_fips", all.x = TRUE)

## plot cases by state
quartz()
ggplot(countycases) +
  geom_line(aes(x = time_value, y = cases, group = abbr, color = abbr), linewidth = 1) +
  theme_minimal() +
  theme(
    strip.text.x = element_text(face = "bold.italic"),
    strip.text.y = element_blank(),
    plot.title = element_text(hjust = 0.5))

## plot just New York and Massachussetts
ggplot(countycases) +
  geom_line(data = filter(countycases, abbr == "NY"), aes(x = time_value, y = cases), color = "red", alpha = 0.5) +
  geom_line(data = filter(countycases, abbr == "MA"), aes(x = time_value, y = cases), color = "blue", alpha = 0.5) +
  theme_minimal() +
  theme(
    strip.text.x = element_text(face = "bold.italic"),
    strip.text.y = element_blank(),
    plot.title = element_text(hjust = 0.5))



#### take just MA counties and plot
mass_counties <- filter(countycases, abbr == "MA", time_value > as.Date("20201001", "%Y%m%d"), time_value < as.Date("20210301", "%Y%m%d"))
ggplot(mass_counties) +
  geom_line(aes(x = time_value, y = cases), color = "red", alpha = 0.5) +
  theme_minimal() +
  theme(
    strip.text.x = element_text(face = "bold.italic"),
    strip.text.y = element_blank(),
    plot.title = element_text(hjust = 0.5))



### maryland incidence from 10/2020 to 03/2021
maryland <- pub_covidcast(
  source = "jhu-csse",
  signals = "confirmed_7dav_incidence_prop",
  time_type = "day",
  geo_type = "state",
  time_values = epirange(20201001, 20210301),
  geo_values = "md")


### clean and plot maryland data
maryland <- maryland %>% dplyr::select(geo_value, time_value, cases = value)
ggplot(maryland) +
  geom_line(aes(x = time_value, y = cases), color = "red", alpha = 0.5) +
  theme_minimal() +
  theme(
    strip.text.x = element_text(face = "bold.italic"),
    strip.text.y = element_blank(),
    plot.title = element_text(hjust = 0.5))

### write file for use in starsim
write_csv(maryland, file = "~/Desktop/maryland_inc_rates.csv")

### transform md incidence data to adjust by under reporting rate of 5
md_inc <- read_csv(file = "~/Desktop/maryland_inc_rates.csv")
cases <- md_inc$cases * 5
md_inc$cases <- cases
write_csv(md_inc, file = "~/Desktop/maryland_inc_rates2.csv")



### Pennsylvania incidence from 10/2020 to 03/2021
penn <- pub_covidcast(
  source = "jhu-csse",
  signals = "confirmed_7dav_incidence_prop",
  time_type = "day",
  geo_type = "state",
  time_values = epirange(20201001, 20210301),
  geo_values = "pa")


### clean and plot penn data
penn <- penn %>% dplyr::select(geo_value, time_value, cases = value)
ggplot(penn) +
  geom_line(aes(x = time_value, y = cases), color = "red", alpha = 0.5) +
  theme_minimal() +
  theme(
    strip.text.x = element_text(face = "bold.italic"),
    strip.text.y = element_blank(),
    plot.title = element_text(hjust = 0.5))

### write file for use in starsim
write_csv(penn, file = "~/Desktop/penn_inc_rates.csv")

### transform md incidence data to adjust by under reporting rate of 5
cases <- penn$cases * 5
penn$cases <- cases
write_csv(penn, file = "~/Desktop/penn_inc_rates2.csv")




#####
##### Maryland
#####

#### load in county pops
county_pops <- read.csv("/Users/nparke19/Downloads/USA_2020_Census_Population_Characteristics/County_2.csv")

## clean the given geographic code
county_pops <- county_pops %>% mutate(clean_county_code = ifelse(Geographic.Identifier < 10000,
                                                                 paste0("0", as.character(Geographic.Identifier)),
                                                                 as.character(Geographic.Identifier)))

## take only md counties (geo id starts with 24)
md_county_pops <- county_pops %>% filter(str_starts(clean_county_code, "24"))  %>%
  dplyr::select(clean_county_code, Name, Total.Population)

## take each county's proportion of total population
md_county_pops$prop <- md_county_pops$Total.Population / sum(md_county_pops$Total.Population)



#### create districts df and save for use in Starsim
md_counties <- md_county_pops %>% dplyr::select(District = Name)

write_csv(md_counties, "~/Documents/Mobility/Zambia/Cleaning_modeling_code/sim_objects/Maryland_districts.csv")


###########
##### simulation location data where people dont move from their home location
##########

# Define folder path
set.seed(3546)
output_folder <- "~/Documents/Mobility/Python_Code/Maryland_no_mobility"
n_simulations = 1

## define function for no mobility sims, where 20000 people are selected based on county population proportion and stay in the same spot
run_simulation <- function() {
  people <- md_county_pops %>%
    mutate(n_people = round(prop * 20000))

  # Adjust to fix total
  diff <- 20000 - sum(people$n_people)
  if (diff != 0) {
    # Add/subtract the difference to the largest county (or randomly)
    adjust_idx <- which.max(people$n_people)
    people$n_people[adjust_idx] <- people$n_people[adjust_idx] + diff
  }

  people <- people %>%
    uncount(n_people, .id = "uid") %>%
    dplyr::select(uid, location = Name)

  sim_df <- expand_grid(step = 0:250, people)
}

# Run and save each simulation
for (i in 1:n_simulations) {
  df <- run_simulation()
  df$simulation <- i  # Add a simulation ID
  file_name <- sprintf("%s/sim_no_mob_%d.csv", output_folder, i)
  fwrite(df, file_name)  # Save as CSV
  print(i)
}




##############
##############
##### travel between maryland counties
##############
##############

### load in 2020 OD matrix
OD_county_2020 <- open_dataset("~/Documents/Mobility/Cuebiq_datasets/OD_SELECT_STATES_COUNTY_2020_EXPORT_CLEAN") %>%
  collect()

## rename columns
colnames(OD_county_2020) <- c("start_county", "end_county", "total_trips", "trips_per_day")

## set as data.table for faster processing
setDT(OD_county_2020)

# Split the census_tract columns by "." into 4 new columns for each part of the census tract code
OD_county_2020[, c("start_country", "start_state", "start_county") := tstrsplit(start_county, ".", fixed = TRUE)]
OD_county_2020[, c("end_country", "end_state", "end_county") := tstrsplit(end_county, ".", fixed = TRUE)]

## filter for only the mid-atlantic states
md_OD_county_2020 <- filter(OD_county_2020, start_state == "MD", end_state == "MD")

## trips per day is censored under 0.5 for the cuebiq data. For those under 0.5, assume its 0.1.
## convert others to numeric version
md_OD_county_2020$trips_per_day_clean <- ifelse(md_OD_county_2020$trips_per_day == "<0.5", 0.1, as.numeric(md_OD_county_2020$trips_per_day))

## function for making proportion of origin travel in cuebiq data
make_proportion_column_cuebiq <- function(data) {
  # Calculate total trips per origin per month
  data_with_totals <- data %>%
    group_by(start_county) %>%
    mutate(total_trips_per_origin = sum(trips_per_day_clean, na.rm = TRUE)) %>%
    ungroup()

  # Calculate proportions for each OD per month
  data_with_proportions <- data_with_totals %>%
    mutate(prop_of_origin_travel = (trips_per_day_clean / total_trips_per_origin) * 100)

  # Round the proportions for clarity
  data_with_proportions <- data_with_proportions %>%
    mutate(prop_of_origin_travel = round(prop_of_origin_travel, 4))

  return(data_with_proportions)
}

## add in proportions of origin travel
md_OD_county_2020 <- make_proportion_column_cuebiq(md_OD_county_2020)

### make clean county code for merging
md_OD_county_2020$start_county_clean <- paste0(24, md_OD_county_2020$start_county)
md_OD_county_2020$end_county_clean <- paste0(24, md_OD_county_2020$end_county)

### merge with population demographics to get names
md_OD_county_2020_names <- merge(md_OD_county_2020, md_county_pops, by.x = "start_county_clean", by.y = "clean_county_code") %>%
  rename("start_county_name" = "Name",
         "origin_pop" = "Total.Population")
md_OD_county_2020_names <- merge(md_OD_county_2020_names, md_county_pops, by.x = "end_county_clean", by.y = "clean_county_code") %>%
  rename("end_county_name" = "Name",
         "destination_pop" = "Total.Population")

### make mob matrix. Any OD combo with no travel is changed to 0
maryland_mob_matrix <- get_mob_matrix(orig = md_OD_county_2020_names$start_county_name,
                           dest = md_OD_county_2020_names$end_county_name,
                           value = md_OD_county_2020_names$prop_of_origin_travel)
maryland_mob_matrix[is.na(maryland_mob_matrix)] = 0



##########
########## read in mid_atl_county_full_dems.csv, whcih has distances between counties
mid_atl_county_full_dems <- read_csv(file= "~/Documents/Mobility/Dems/mid_atl_county_full_dems.csv")
mid_atl_county_full_dems$origin_county <- as.character(mid_atl_county_full_dems$origin_county)
mid_atl_county_full_dems$destination_county <- as.character(mid_atl_county_full_dems$destination_county)

## join with list of md counties to get full list of OD pairs
md_OD_county_2020_names_dems <- left_join(md_OD_county_2020_names,
                                                       mid_atl_county_full_dems,
                                                       by = c(
                                                         "start_county_clean" = ("origin_county"),
                                                         "end_county_clean" = "destination_county"
                                                       ))

### use get_mob_matrix to get matrix of distances between OD pairs
maryland_dist_matrix <- get_mob_matrix(orig = md_OD_county_2020_names_dems$start_county_name,
               dest = md_OD_county_2020_names_dems$end_county_name,
               value = md_OD_county_2020_names_dems$distance_km)


# Compute route-specific decay rates
compute_beta_ij <- function(distance_matrix, beta_0, alpha) {
  beta_matrix <- beta_0 * exp(-alpha * distance_matrix)  # Apply exponential decay
  diag(beta_matrix) <- Inf  # No decay for same-location trips
  return(beta_matrix)
}

beta_0 <- 0.4   # Maximum decay parameter from Hay paper
alpha <- 0.0014 # Scaling factor


# Generate the beta matrix using distances
maryland_beta_matrix <- compute_beta_ij(maryland_dist_matrix, beta_0, alpha)


## finally, need population by county
maryland_pops <- md_county_pops %>% dplyr::select(Name, Total.Population) %>% rename("Population" = "Total.Population")



###############
####### simulate travel using aggregated mobility data
###############

# Define folder path
output_folder <- "~/Documents/Mobility/Zambia/Cleaning_modeling_code/Simulated_mobility_datasets/Maryland_agg_mobility"
n_simulations = 10

# Run and save each simulation
for (i in 1:n_simulations) {
  df <- simulate_mobility_return(n_people = 20000, n_timesteps = 250,
                                 travel_matrix = maryland_mob_matrix,
                                 pop_data = maryland_pops,
                                 beta_matrix = maryland_beta_matrix, chunk_size = 1)
  df$simulation <- i  # Add a simulation ID
  file_name <- sprintf("%s/sim_return_%d.csv", output_folder, i)
  fwrite(df, file_name)  # Save as CSV
  print(i)
}


###############
####### simulate travel using aggregated mobility data, but adjusted for both heterogeneities
###############

#### clean up md counties with dems by adding origin/dest pop/100k, a same OD indicator variable, and clean distance_km
md_OD_county_2020_names_dems_clean <- md_OD_county_2020_names_dems %>%
  mutate(origin_pop_per100k = ifelse(is.na(origin_pop_per100k), origin_pop/100000, origin_pop_per100k),
         destination_pop_per100k = ifelse(is.na(destination_pop_per100k), destination_pop/100000, destination_pop_per100k),
         same_OD_ind = ifelse(start_county_name == end_county_name, 1, 0),
         distance_km = ifelse(start_county_name == end_county_name, 0, distance_km)) %>%
  rename("aggregated_total_clean" = "trips_per_day_clean") %>%
  dplyr::select(start_county_name, end_county_name, aggregated_total_clean, prop_of_origin_travel,
               origin_pop_per100k, destination_pop_per100k, distance_km, same_OD_ind)


### scale predictor variables
md_OD_county_2020_names_dems_clean$scaled_distance_km = scale(md_OD_county_2020_names_dems_clean$distance_km)
md_OD_county_2020_names_dems_clean$scaled_destination_pop_per100k = scale(md_OD_county_2020_names_dems_clean$destination_pop_per100k)
md_OD_county_2020_names_dems_clean$scaled_origin_pop_per100k = scale(md_OD_county_2020_names_dems_clean$origin_pop_per100k)
md_OD_county_2020_names_dems_clean$scaled_prop_of_origin_travel = scale(md_OD_county_2020_names_dems_clean$prop_of_origin_travel)


### seperate by intra and inter-district pairs
md_OD_county_2020_names_dems_clean_diffODS <- filter(md_OD_county_2020_names_dems_clean, same_OD_ind == 0)
md_OD_county_2020_names_dems_clean_sameODS <- filter(md_OD_county_2020_names_dems_clean, same_OD_ind == 1)

### since we don't have travel frequency heterogeneity for 2020, we have to predict it
### predict both the existence of travel het and shape of trips per person distribution

## here, predict whether there is travel het in an OD pair using Cuebiq prediction model (see Cuebiq_het_clean.R)
md_OD_county_2020_het_preds <- md_OD_county_2020_names_dems_clean_diffODS %>%
  mutate(has_het_pred = ifelse(predict.glm(cuebiq_het_model,
                                           newdata = md_OD_county_2020_names_dems_clean_diffODS,
                                           type = "response") > cuebiq_optimal_cutoff$threshold, 1, 0))


###now take only the ones that were predicted to have het
md_OD_county_2020_preds_has_het <- filter(md_OD_county_2020_het_preds, has_het_pred == 1)

## now predict negative binomial size and mu for those that are predicted to have travel het
md_OD_county_2020_preds_negbi_params <- md_OD_county_2020_preds_has_het %>%
  mutate(pred_neg_bi_size = predict.lm(cuebiq.negbi.size.mod, newdata = md_OD_county_2020_preds_has_het, type = "response"),
                                              pred_neg_bi_mu = predict.lm(cuebiq.negbi.mu.mod, newdata = md_OD_county_2020_preds_has_het, type = "response"))

#if the predictions are less than 0, put them in as 0.01
md_OD_county_2020_preds_negbi_params <- md_OD_county_2020_preds_negbi_params %>% mutate(pred_neg_bi_size = ifelse(pred_neg_bi_size < 0 , 0.01, pred_neg_bi_size),
                                                    pred_neg_bi_mu = ifelse(pred_neg_bi_mu < 0 , 0.01, pred_neg_bi_mu))

####now merge the together dataset with the preds_het to get all diff ODs together
md_OD_county_2020_all_preds <-merge(md_OD_county_2020_preds_negbi_params, md_OD_county_2020_het_preds,
                                    by = c("start_county_name", "end_county_name", "aggregated_total_clean",
                                           "prop_of_origin_travel", "origin_pop_per100k", "same_OD_ind",
                                           "destination_pop_per100k", "distance_km", "scaled_origin_pop_per100k",
                                           "scaled_prop_of_origin_travel", "has_het_pred"), all = T,
                                    no.dups = T)

## now merger with same ODs to get all OD pairs together with predictions
md_OD_county_2020_all_preds <- bind_rows(md_OD_county_2020_all_preds, md_OD_county_2020_names_dems_clean_sameODS)

## as with Zambia data, find the
## predicted number of people who travelled in each OD pair
## predicted multiplier: used for pred_dist_percent. basically averages out proportion of origin travel for people who take multiple trips
## pred_pmf: porportion of people that take 1,2,3,etc. trips in OD pairs (i.e. (80%, 10%, 10%) means 80% take 1 trip, 10% 2 trips, 10% 3 trips)
## pred_dist_percent: proportion of origin travel as a function of trips per person distribution (i.e. 5% becomes (2.5%, 5%, and 7.5%) in OD pair that has 1,2,3 trips per person
md_OD_county_2020_all_preds_pmf <- md_OD_county_2020_all_preds %>%
  rowwise() %>%
  mutate(predicted_people = ifelse(has_het_pred == 1 & same_OD_ind == 0, aggregated_total_clean/(1+pred_neg_bi_mu), aggregated_total_clean),
         pred_multiplier = (predicted_people*prop_of_origin_travel)/aggregated_total_clean,
         pred_pmf = ifelse(has_het_pred == 1, list(get_nb_pmf(pred_neg_bi_size, pred_neg_bi_mu)), list(1)),
         pred_pmf_clean = ifelse(is.null(pred_pmf), list(1), list(pred_pmf)),
         pred_dist_percent = ifelse(has_het_pred == 1 & same_OD_ind == 0,
                                    list(c(1:length(pred_pmf_clean)) * pred_multiplier),
                                    list(prop_of_origin_travel))) %>%
  ungroup()


## select only the useful variables
md_OD_county_2020_all_preds_pmf_clean <- md_OD_county_2020_all_preds_pmf %>%
  dplyr::select(start_county_name, end_county_name, same_OD_ind,
                prop_of_origin_travel, pred_multiplier, pred_dist_percent,
                pred_pmf_clean)


### first create matrix with prop of origin travel with either multiple values (if travel het) or single value (no travel het or same+
# Convert to matrix format
md_OD_county_2020_dist_percent <- md_OD_county_2020_all_preds_pmf_clean %>% dplyr::select(start_county_name, end_county_name, pred_dist_percent) %>%
  arrange(start_county_name, end_county_name) %>%
  pivot_wider(names_from = end_county_name, values_from = pred_dist_percent, values_fill = list(0))

md_OD_county_2020_dist_percent_matrix <- as.matrix(md_OD_county_2020_dist_percent[, -1])  # Remove origin column for matrix format
rownames(md_OD_county_2020_dist_percent_matrix) <- md_OD_county_2020_dist_percent$start_county_name  # Set row names

### now create matrix with probability of each probability of travel (i.e. a 70% chance of having a 0.12% chance of travel from A to B)
# Convert to matrix format
md_OD_county_2020_pred_pmf <- md_OD_county_2020_all_preds_pmf_clean %>% dplyr::select(start_county_name, end_county_name, pred_pmf_clean) %>%
  arrange(start_county_name, end_county_name) %>%
  pivot_wider(names_from = end_county_name, values_from = pred_pmf_clean, values_fill = list(1))

md_OD_county_2020_pred_pmf_matrix <- as.matrix(md_OD_county_2020_pred_pmf[, -1])  # Remove origin column for matrix format
rownames(md_OD_county_2020_pred_pmf_matrix) <- md_OD_county_2020_pred_pmf$start_county_name  # Set row names


###############
######## simulate location using aggregated totals adjusted for both heterogeneities
###############

#### same as previous both het function, but with different non-mover percentage that resets every day
### this data used all trips at the county level looking over a day, which is 19% movers, 81% non-movers
simulate_mobility_het_obs_only_prob_move_cuebiq <- function(n_people, n_timesteps, dist_percent_matrix, pmf_matrix, pop_data,
                                                     beta_matrix, chunk_size = 100) {
  # Function to sample trip duration using exponenitla decayscaled by β_ij
  sample_trip_length <- function(beta_ij) {  # Ensures min 1-day trips
    return(ceiling(rexp(1, rate = beta_ij)))  # Scale by inverse decay
  }

  # Create initial locations based on population proportions
  locations <- rownames(dist_percent_matrix)
  population_probs <- pop_data$Population / sum(pop_data$Population)
  initial_locations <- sample(locations, size = n_people, replace = TRUE, prob = population_probs)

  # Sample individual travel probabilities from OD-specific probability distributions
  individual_travel_probs <- matrix(NA, nrow = n_people, ncol = length(locations), dimnames = list(NULL, locations))

  individ_probs_function <- function(people){
    for (person in people) {
      origin <- initial_locations[person]
      for (destination in locations) {
        individual_travel_probs[person, destination] <- ifelse(length(pmf_matrix[[origin, destination]]) == 1,
                                                               dist_percent_matrix[[origin, destination]],
                                                               sample(dist_percent_matrix[[origin, destination]], size = 1,
                                                                      prob = pmf_matrix[[origin, destination]]))
      }
    }
    return(individual_travel_probs)
  }
  individual_travel_probs <- individ_probs_function(people = 1:n_people)

  # Initialize an empty list to store chunk results
  chunk_results <- list()

  # Process people in chunks
  for (chunk_start in seq(1, n_people, by = chunk_size)) {
    chunk_end <- min(chunk_start + chunk_size - 1, n_people)
    chunk_ids <- chunk_start:chunk_end

    # Create an empty data frame for the current chunk
    sim_results_chunk <- data.frame(
      step = rep(1:n_timesteps, each = length(chunk_ids)),
      uid = rep(chunk_ids, times = n_timesteps),
      location = NA,
      return_step = NA
    )

    # Set initial locations for the chunk
    sim_results_chunk$location[sim_results_chunk$step == 1] <- initial_locations[chunk_ids]

    # Loop over each person in the current chunk
    for (person in chunk_ids) {
      origin <- initial_locations[person]
      never_mover_prob = 0.81
      never_mover <- runif(1) < never_mover_prob
      return_step <- NA
      current_location <- origin



      for (t in 2:n_timesteps) {
        never_mover <- runif(1) < never_mover_prob


        if (never_mover) {
          current_location <- origin
          return_step <- NA
        }
        else {
          prev_location <- sim_results_chunk$location[sim_results_chunk$uid == person & sim_results_chunk$step == (t - 1)]

          individual_travel_probs <- individ_probs_function(people = person)

          # If person is scheduled to return, force return
          if (!is.na(return_step) && t == return_step) {
            current_location <- origin
            return_step <- NA
          } else if (is.na(return_step)) {
            # Sample movement based on the individual's sampled probability
            travel_probs <- individual_travel_probs[person, ]
            new_location <- ifelse(sum(travel_probs) != 0, sample(names(travel_probs), size = 1, prob = travel_probs), current_location)

            # Compute β_ij for this trip
            beta_ij <- beta_matrix[prev_location, new_location]

            # If the person moves away, set return time based on β_ij
            if (new_location != origin) {
              return_step <- t + sample_trip_length(beta_ij)
            }
            current_location <- new_location
          }
        }


        # Store results for the current person and timestep
        sim_results_chunk$location[sim_results_chunk$uid == person & sim_results_chunk$step == t] <- current_location
        sim_results_chunk$return_step[sim_results_chunk$uid == person & sim_results_chunk$step == t] <- return_step
      }
    }

    # Store the results of the current chunk in the list
    chunk_results[[length(chunk_results) + 1]] <- sim_results_chunk

    # Optionally, print progress
    cat("Processed chunk:", chunk_start, "to", chunk_end, "\n")
  }

  # Combine all chunks into one data frame
  sim_results <- do.call(rbind, chunk_results)
  sim_results <- sim_results %>% mutate(uid = uid - 1,
                                        step = step - 1)

  return(sim_results)
}



# Define folder path
output_folder <- "~/Documents/Mobility/Zambia/Cleaning_modeling_code/Simulated_mobility_datasets/Maryland_both_het_mobility"
n_simulations = 10

# Run and save each simulation
for (i in 4:n_simulations) {
  df <- simulate_mobility_het_obs_only_prob_move_cuebiq(n_people = 20000, 225, md_OD_county_2020_dist_percent_matrix,
                                                 md_OD_county_2020_pred_pmf_matrix, pop_data = maryland_pops,
                                                 maryland_beta_matrix, chunk_size = 50)
  df$simulation <- i  # Add a simulation ID
  file_name <- sprintf("%s/sim_both_het_%d.csv", output_folder, i)
  fwrite(df, file_name)  # Save as CSV
  print(i)
}









################
################
###### analyzing sim results from starsim
################
################




#### function for cleaning mobility sim data from starsim
clean_alphawave_func <- function(sim_list, sim_length){
  all_data <- data.frame()

  for (sim in 1:length(sim_list)){
    sim_df <- sim_list[[sim]]
    colnames(sim_df) <- maryland_pops$Name
    sim_df <- sim_df %>%
      mutate(time_step = 0:sim_length) %>%  # Add time step column
      relocate(time_step)


    cum_sim_df <- sim_df
    cum_sim_df[, -1] <- apply(cum_sim_df[, -1], 2, cumsum)
    cum_sim_df$total_count <- rowSums(cum_sim_df[, -1], na.rm = TRUE)  # Excluding the time column
    cum_sim_df$nonzero_count <- rowSums(cum_sim_df[, c(-1, -26)] != 0, na.rm = TRUE)  # Excluding the time column

    sim_df$total_new_inf <- rowSums(sim_df[, -1], na.rm = TRUE)


    total_infections_sim <- max(cum_sim_df$total_count)
    num_districts_inf_sim <- max(cum_sim_df$nonzero_count)
    time_to_10_sim <- cum_sim_df$time_step[which(cum_sim_df$nonzero_count >= 10)[1]]
    time_to_50per_sim <- cum_sim_df$time_step[which(cum_sim_df$nonzero_count >= 12)[1]]
    time_to_24_sim <- cum_sim_df$time_step[which(cum_sim_df$nonzero_count >= 24)[1]]
    time_max_inf_sim <- sim_df$time_step[which.max(sim_df$total_new_inf)[1]]

    sim_df$sim_id <- as.character(sim)
    all_data <- bind_rows(all_data, sim_df, .id = "sim_id")
  }
  return(all_data)
}


##########
##########
#### sim results using beta fit to even mixedly pop
##########
##########


### read in and clean aggregated and both het mobility sims
md_agg_mobility_even_mixed_sims <- py_load_object("~/Documents/Mobility/Alpha_wave/Starsim_sims/Maryland_agg_mobility_fit_well_mixed_sims.pkl")
md_agg_mobility_even_mixed_sims_clean <- clean_alphawave_func(md_agg_mobility_even_mixed_sims, 224)

md_both_het_mobility_even_mixed_sims <- py_load_object("~/Documents/Mobility/Alpha_wave/Starsim_sims/Maryland_both_het_mobility_fit_well_mixed_sims.pkl")
md_both_het_mobility_even_mixed_sims_clean <- clean_alphawave_func(md_both_het_mobility_even_mixed_sims, 224)

## read in in no mobility sim results (already in median)
md_no_mobility_even_mixed_sims <- read.csv("~/Documents/Mobility/Alpha_wave/Starsim_sims/Maryland_even_mobility_fit_well_mixed_sims.csv")
md_no_mobility_even_mixed_sims <- md_no_mobility_even_mixed_sims[,c("timevec.1", "seir_new_infections")]

### read in maryland inc rates empirical
maryland_inc <- read.csv(file = "~/Desktop/maryland_inc_rates.csv") %>% dplyr::select(time_value, cases)


### get median new infections across all sims for agg and both het
md_agg_mobility_even_mixed_new_inf <- md_agg_mobility_even_mixed_sims_clean %>%
  group_by(time_step) %>%
  summarise(median_new_infections = median(total_new_inf, na.rm = TRUE), .groups = "drop")

md_both_het_mobility_even_mixed_new_inf <- md_both_het_mobility_even_mixed_sims_clean %>%
  group_by(time_step) %>%
  summarise(median_new_infections = median(total_new_inf, na.rm = TRUE), .groups = "drop")

### change time step to date for necessary dfs
md_agg_mobility_even_mixed_new_inf$time_value <- seq(as.Date("2020-09-17"), as.Date("2021-4-29"), by = "day")
md_both_het_mobility_even_mixed_new_inf$time_value <- seq(as.Date("2020-09-17"), as.Date("2021-4-29"), by = "day")

md_agg_mobility_even_mixed_new_inf <- md_agg_mobility_even_mixed_new_inf[, c("time_value", "median_new_infections")]
md_both_het_mobility_even_mixed_new_inf <- md_both_het_mobility_even_mixed_new_inf[, c("time_value", "median_new_infections")]

### align columns names
colnames(md_agg_mobility_even_mixed_new_inf) <- c("Date", "New_infections")
colnames(md_both_het_mobility_even_mixed_new_inf) <- c("Date", "New_infections")
colnames(md_no_mobility_even_mixed_sims) <- c("Date", "New_infections")
colnames(maryland_inc) <- c("Date", "New_infections")

### divide all but maryland_inc cases by 5 to compare to per 100,000
md_agg_mobility_even_mixed_new_inf$New_infections <- (md_agg_mobility_even_mixed_new_inf$New_infections)
md_both_het_mobility_even_mixed_new_inf$New_infections <- md_both_het_mobility_even_mixed_new_inf$New_infections
md_no_mobility_even_mixed_sims$New_infections <- md_no_mobility_even_mixed_sims$New_infections

## add in data generating name
md_agg_mobility_even_mixed_new_inf$data <- "Aggregated mobility"
md_both_het_mobility_even_mixed_new_inf$data <- "Both heterogeneities mobility"
md_no_mobility_even_mixed_sims$data <- "Evenly mixed population"
maryland_inc$data <- "Observed"

## combine all four datatypes
combined_alphawave_even_mixed_newinf <- rbind(md_agg_mobility_even_mixed_new_inf, md_both_het_mobility_even_mixed_new_inf, md_no_mobility_even_mixed_sims,
                                   maryland_inc)

## factor data type levels
combined_alphawave_even_mixed_newinf$data <- factor(combined_alphawave_even_mixed_newinf$data,
                                         levels = c("Observed", "Evenly mixed population",
                                                    "Aggregated mobility", "Both heterogeneities mobility"))
## plot all four mean trajectories on the same graph
md_even_mixed <- ggplot(combined_alphawave_even_mixed_newinf) +
  geom_line(aes(x = Date, y = New_infections, color = data)) +
  scale_color_viridis(discrete = T, begin = 0.1, end = 0.8, option = "B") +
  theme_minimal() +
  scale_x_date(date_labels = "%b' %y", limits = c(as.Date("2020-10-01"), as.Date("2021-02-28"))) +
  ylab("New infections (per 100,000 people)") +
  theme(legend.title = element_blank(),
        legend.position = c(0.2, 0.8),
        legend.background = element_rect(fill="white",
                                         size=0.3, linetype="solid"))



sum(md_no_mobility_sims$New_infections)
sum(maryland_inc$New_infections)


#### time of max infections
md_agg_mobility_even_mixed_new_inf$Date[which.max(md_agg_mobility_even_mixed_new_inf$New_infections)[1]]
md_both_het_mobility_even_mixed_new_inf$Date[which.max(md_both_het_mobility_even_mixed_new_inf$New_infections)[1]]
md_no_mobility_even_mixed_sims$Date[which.max(md_no_mobility_even_mixed_sims$New_infections)[1]]
maryland_inc$Date[which.max(maryland_inc$New_infections)[1]]

### difference between max infections of real incidence versus each simulation
md_agg_mobility_even_mixed_new_inf$New_infections[which.max(md_agg_mobility_even_mixed_new_inf$New_infections)[1]] - maryland_inc$New_infections[which.max(maryland_inc$New_infections)[1]]
md_both_het_mobility_even_mixed_new_inf$New_infections[which.max(md_both_het_mobility_even_mixed_new_inf$New_infections)[1]] - maryland_inc$New_infections[which.max(maryland_inc$New_infections)[1]]
md_no_mobility_even_mixed_sims$New_infections[which.max(md_no_mobility_even_mixed_sims$New_infections)[1]] - maryland_inc$New_infections[which.max(maryland_inc$New_infections)[1]]
maryland_inc$New_infections[which.max(maryland_inc$New_infections)[1]]




##########
##########
#### sim results using beta fit to non-mixed pop
##########
##########

### read in and clean aggregated and both het mobility sims
md_agg_mobility_no_mob_sims <- py_load_object("~/Documents/Mobility/Alpha_wave/Starsim_sims/Maryland_agg_mobility_fit_no_mob_sims.pkl")
md_agg_mobility_no_mob_sims_clean <- clean_alphawave_func(md_agg_mobility_no_mob_sims, 224)

md_both_het_mobility_no_mob_sims <- py_load_object("~/Documents/Mobility/Alpha_wave/Starsim_sims/Maryland_both_het_mobility_fit_no_mob_sims.pkl")
md_both_het_mobility_no_mob_sims_clean <- clean_alphawave_func(md_both_het_mobility_no_mob_sims, 224)

## read in in no mobility sim results (already in median)
md_no_mobility_no_mob_sims <- read.csv("~/Documents/Mobility/Alpha_wave/Starsim_sims/Maryland_even_mobility_fit_no_mob_sims.csv")
md_no_mobility_no_mob_sims <- md_no_mobility_no_mob_sims[,c("timevec.1", "seir_new_infections")]

### read in maryland inc rates empirical
maryland_inc <- read.csv(file = "~/Desktop/maryland_inc_rates.csv") %>% dplyr::select(time_value, cases)


### get median new infections across all sims for agg and both het
md_agg_mobility_no_mob_new_inf <- md_agg_mobility_no_mob_sims_clean %>%
  group_by(time_step) %>%
  summarise(median_new_infections = median(total_new_inf, na.rm = TRUE), .groups = "drop")

md_both_het_mobility_no_mob_new_inf <- md_both_het_mobility_no_mob_sims_clean %>%
  group_by(time_step) %>%
  summarise(median_new_infections = median(total_new_inf, na.rm = TRUE), .groups = "drop")

### change time step to date for necessary dfs
md_agg_mobility_no_mob_new_inf$time_value <- seq(as.Date("2020-09-17"), as.Date("2021-4-29"), by = "day")
md_both_het_mobility_no_mob_new_inf$time_value <- seq(as.Date("2020-09-17"), as.Date("2021-4-29"), by = "day")

md_agg_mobility_no_mob_new_inf <- md_agg_mobility_no_mob_new_inf[, c("time_value", "median_new_infections")]
md_both_het_mobility_no_mob_new_inf <- md_both_het_mobility_no_mob_new_inf[, c("time_value", "median_new_infections")]

### align columns names
colnames(md_agg_mobility_no_mob_new_inf) <- c("Date", "New_infections")
colnames(md_both_het_mobility_no_mob_new_inf) <- c("Date", "New_infections")
colnames(md_no_mobility_no_mob_sims) <- c("Date", "New_infections")
colnames(maryland_inc) <- c("Date", "New_infections")

### divide all but maryland_inc cases by 5 to compare to per 100,000
md_agg_mobility_no_mob_new_inf$New_infections <- (md_agg_mobility_no_mob_new_inf$New_infections)
md_both_het_mobility_no_mob_new_inf$New_infections <- md_both_het_mobility_no_mob_new_inf$New_infections
md_no_mobility_no_mob_sims$New_infections <- md_no_mobility_no_mob_sims$New_infections

## add in data generating name
md_agg_mobility_no_mob_new_inf$data <- "Aggregated mobility"
md_both_het_mobility_no_mob_new_inf$data <- "Both heterogeneities mobility"
md_no_mobility_no_mob_sims$data <- "Evenly mixed population"
maryland_inc$data <- "Observed"

## combine all four datatypes
combined_alphawave_no_mob_newinf <- rbind(md_agg_mobility_no_mob_new_inf, md_both_het_mobility_no_mob_new_inf, md_no_mobility_no_mob_sims,
                                              maryland_inc)

## factor data type levels
combined_alphawave_no_mob_newinf$data <- factor(combined_alphawave_no_mob_newinf$data,
                                                    levels = c("Observed", "Evenly mixed population",
                                                               "Aggregated mobility", "Both heterogeneities mobility"))
## plot all four mean trajectories on the same graph
md_no_mob <- ggplot(combined_alphawave_no_mob_newinf) +
  geom_line(aes(x = Date, y = New_infections, color = data)) +
  scale_color_viridis(discrete = T, begin = 0.1, end = 0.8, option = "B") +
  theme_minimal() +
  scale_x_date(date_labels = "%b' %y", limits = c(as.Date("2020-10-01"), as.Date("2021-02-28"))) +
  ylab("New infections (per 100,000 people)") +
  theme(legend.title = element_blank(),
        legend.position = c(0.2, 0.8),
        legend.background = element_rect(fill="white",
                                         size=0.3, linetype="solid"))



sum(md_no_mobility_sims$New_infections)
sum(maryland_inc$New_infections)


#### time of max infections
md_agg_mobility_no_mob_new_inf$Date[which.max(md_agg_mobility_no_mob_new_inf$New_infections)[1]]
md_both_het_mobility_no_mob_new_inf$Date[which.max(md_both_het_mobility_no_mob_new_inf$New_infections)[1]]
md_no_mobility_no_mob_sims$Date[which.max(md_no_mobility_no_mob_sims$New_infections)[1]]
maryland_inc$Date[which.max(maryland_inc$New_infections)[1]]

### difference between max infections of real incidence versus each simulation
md_agg_mobility_no_mob_new_inf$New_infections[which.max(md_agg_mobility_no_mob_new_inf$New_infections)[1]] - maryland_inc$New_infections[which.max(maryland_inc$New_infections)[1]]
md_both_het_mobility_no_mob_new_inf$New_infections[which.max(md_both_het_mobility_no_mob_new_inf$New_infections)[1]] - maryland_inc$New_infections[which.max(maryland_inc$New_infections)[1]]
md_no_mobility_no_mob_sims$New_infections[which.max(md_no_mobility_no_mob_sims$New_infections)[1]] - maryland_inc$New_infections[which.max(maryland_inc$New_infections)[1]]
maryland_inc$New_infections[which.max(maryland_inc$New_infections)[1]]







##########
##########
#### sim results using beta fit to both heterogeneities model
##########
##########

### read in and clean aggregated and both het mobility sims
md_agg_mobility_bh_sims <- py_load_object("~/Documents/Mobility/Alpha_wave/Starsim_sims/Maryland_agg_mobility_fit_bh_sims.pkl")
md_agg_mobility_bh_sims_clean <- clean_alphawave_func(md_agg_mobility_bh_sims, 224)

md_both_het_mobility_bh_sims <- py_load_object("~/Documents/Mobility/Alpha_wave/Starsim_sims/Maryland_both_het_mobility_fit_bh_sims.pkl")
md_both_het_mobility_bh_sims_clean <- clean_alphawave_func(md_both_het_mobility_bh_sims, 224)

## read in in no mobility sim results (already in median)
md_no_mobility_bh_sims <- read.csv("~/Documents/Mobility/Alpha_wave/Starsim_sims/Maryland_even_mobility_fit_bh_sims.csv")
md_no_mobility_bh_sims <- md_no_mobility_bh_sims[,c("timevec.1", "seir_new_infections")]

### read in maryland inc rates empirical
maryland_inc <- read.csv(file = "~/Desktop/maryland_inc_rates.csv") %>% dplyr::select(time_value, cases)


### get median new infections across all sims for agg and both het
md_agg_mobility_bh_new_inf <- md_agg_mobility_bh_sims_clean %>%
  group_by(time_step) %>%
  summarise(median_new_infections = median(total_new_inf, na.rm = TRUE), .groups = "drop")

md_both_het_mobility_bh_new_inf <- md_both_het_mobility_bh_sims_clean %>%
  group_by(time_step) %>%
  summarise(median_new_infections = median(total_new_inf, na.rm = TRUE), .groups = "drop")

### change time step to date for necessary dfs
md_agg_mobility_bh_new_inf$time_value <- seq(as.Date("2020-09-17"), as.Date("2021-4-29"), by = "day")
md_both_het_mobility_bh_new_inf$time_value <- seq(as.Date("2020-09-17"), as.Date("2021-4-29"), by = "day")

md_agg_mobility_bh_new_inf <- md_agg_mobility_bh_new_inf[, c("time_value", "median_new_infections")]
md_both_het_mobility_bh_new_inf <- md_both_het_mobility_bh_new_inf[, c("time_value", "median_new_infections")]

### align columns names
colnames(md_agg_mobility_bh_new_inf) <- c("Date", "New_infections")
colnames(md_both_het_mobility_bh_new_inf) <- c("Date", "New_infections")
colnames(md_no_mobility_bh_sims) <- c("Date", "New_infections")
colnames(maryland_inc) <- c("Date", "New_infections")

### divide all but maryland_inc cases by 5 to compare to per 100,000
md_agg_mobility_bh_new_inf$New_infections <- (md_agg_mobility_bh_new_inf$New_infections)
md_both_het_mobility_bh_new_inf$New_infections <- md_both_het_mobility_bh_new_inf$New_infections
md_no_mobility_bh_sims$New_infections <- md_no_mobility_bh_sims$New_infections

## add in data generating name
md_agg_mobility_bh_new_inf$data <- "Aggregated mobility"
md_both_het_mobility_bh_new_inf$data <- "Both heterogeneities mobility"
md_no_mobility_bh_sims$data <- "Evenly mixed population"
maryland_inc$data <- "Observed"

## combine all four datatypes
combined_alphawave_bh_newinf <- rbind(md_agg_mobility_bh_new_inf, md_both_het_mobility_bh_new_inf, md_no_mobility_bh_sims,
                                          maryland_inc)

## factor data type levels
combined_alphawave_bh_newinf$data <- factor(combined_alphawave_bh_newinf$data,
                                                levels = c("Observed", "Evenly mixed population",
                                                           "Aggregated mobility", "Both heterogeneities mobility"))
## plot all four mean trajectories on the same graph
md_bh <- ggplot(combined_alphawave_bh_newinf) +
  geom_line(aes(x = Date, y = New_infections, color = data)) +
  scale_color_viridis(discrete = T, begin = 0.1, end = 0.8, option = "B") +
  theme_minimal() +
  scale_x_date(date_labels = "%b' %y", limits = c(as.Date("2020-10-01"), as.Date("2021-02-28"))) +
  ylab("New infections (per 100,000 people)") +
  theme(legend.title = element_blank(),
        legend.position = c(0.2, 0.8),
        legend.background = element_rect(fill="white",
                                         size=0.3, linetype="solid"))



sum(md_no_mobility_sims$New_infections)
sum(maryland_inc$New_infections)


#### time of max infections
md_agg_mobility_bh_new_inf$Date[which.max(md_agg_mobility_bh_new_inf$New_infections)[1]]
md_both_het_mobility_bh_new_inf$Date[which.max(md_both_het_mobility_bh_new_inf$New_infections)[1]]
md_no_mobility_bh_sims$Date[which.max(md_no_mobility_bh_sims$New_infections)[1]]
maryland_inc$Date[which.max(maryland_inc$New_infections)[1]]

### difference between max infections of real incidence versus each simulation
md_agg_mobility_bh_new_inf$New_infections[which.max(md_agg_mobility_bh_new_inf$New_infections)[1]] - maryland_inc$New_infections[which.max(maryland_inc$New_infections)[1]]
md_both_het_mobility_bh_new_inf$New_infections[which.max(md_both_het_mobility_bh_new_inf$New_infections)[1]] - maryland_inc$New_infections[which.max(maryland_inc$New_infections)[1]]
md_no_mobility_bh_sims$New_infections[which.max(md_no_mobility_bh_sims$New_infections)[1]] - maryland_inc$New_infections[which.max(maryland_inc$New_infections)[1]]
maryland_inc$New_infections[which.max(maryland_inc$New_infections)[1]]







###############
###############
##### Pennslyvania
###############
###############

#### load in county pops
county_pops <- read.csv("/Users/nparke19/Downloads/USA_2020_Census_Population_Characteristics/County_2.csv")

## clean the given geographic code
county_pops <- county_pops %>% mutate(clean_county_code = ifelse(Geographic.Identifier < 10000,
                                                                 paste0("0", as.character(Geographic.Identifier)),
                                                                 as.character(Geographic.Identifier)))

## take only md counties (geo id starts with 42)
pa_county_pops <- county_pops %>% filter(str_starts(clean_county_code, "42"))  %>%
  dplyr::select(clean_county_code, Name, Total.Population)

## take each county's proportion of total population
pa_county_pops$prop <- pa_county_pops$Total.Population / sum(pa_county_pops$Total.Population)



#### create districts df and save for use in Starsim
pa_counties <- pa_county_pops %>% dplyr::select(District = Name)

write_csv(pa_counties, "~/Documents/Mobility/Zambia/Cleaning_modeling_code/sim_objects/Penn_districts.csv")


###########
##### simulation location data where people dont move from their home location
##########

# Define folder path
set.seed(3546)
output_folder <- "~/Documents/Mobility/Alpha_wave/Location_sims/Penn_no_mobility"
n_simulations = 10

run_simulation <- function() {
  people <- pa_county_pops %>%
    mutate(n_people = round(prop * 20000))

  # Adjust to fix total
  diff <- 20000 - sum(people$n_people)
  if (diff != 0) {
    # Add/subtract the difference to the largest county (or randomly)
    adjust_idx <- which.max(people$n_people)
    people$n_people[adjust_idx] <- people$n_people[adjust_idx] + diff
  }

  people <- people %>%
    uncount(n_people, .id = "uid") %>%
    dplyr::select(uid, location = Name)

  sim_df <- expand_grid(step = 0:250, people)
}

# Run and save each simulation
for (i in 1:n_simulations) {
  df <- run_simulation()
  df$simulation <- i  # Add a simulation ID
  file_name <- sprintf("%s/sim_no_mob_%d.csv", output_folder, i)
  fwrite(df, file_name)  # Save as CSV
  print(i)
}




##############
##############
##### travel between penn counties
##############
##############

### load in 2020 OD matrix
OD_county_2020 <- open_dataset("~/Documents/Mobility/Cuebiq_datasets/OD_SELECT_STATES_COUNTY_2020_EXPORT_CLEAN") %>%
  collect()

## rename columns
colnames(OD_county_2020) <- c("start_county", "end_county", "total_trips", "trips_per_day")

## set as data.table for faster processing
setDT(OD_county_2020)

# Split the census_tract columns by "." into 4 new columns for each part of the census tract code
OD_county_2020[, c("start_country", "start_state", "start_county") := tstrsplit(start_county, ".", fixed = TRUE)]
OD_county_2020[, c("end_country", "end_state", "end_county") := tstrsplit(end_county, ".", fixed = TRUE)]

## filter for only the mid-atlantic states
pa_OD_county_2020 <- filter(OD_county_2020, start_state == "PA", end_state == "PA")

## trips per day is censored under 0.5 for the cuebiq data. For those under 0.5, assume its 0.1.
## convert others to numeric version
pa_OD_county_2020$trips_per_day_clean <- ifelse(pa_OD_county_2020$trips_per_day == "<0.5", 0.1, as.numeric(pa_OD_county_2020$trips_per_day))

## function for making proportion of origin travel in cuebiq data
make_proportion_column_cuebiq <- function(data) {
  # Calculate total trips per origin per month
  data_with_totals <- data %>%
    group_by(start_county) %>%
    mutate(total_trips_per_origin = sum(trips_per_day_clean, na.rm = TRUE)) %>%
    ungroup()

  # Calculate proportions for each OD per month
  data_with_proportions <- data_with_totals %>%
    mutate(prop_of_origin_travel = (trips_per_day_clean / total_trips_per_origin) * 100)

  # Round the proportions for clarity
  data_with_proportions <- data_with_proportions %>%
    mutate(prop_of_origin_travel = round(prop_of_origin_travel, 4))

  return(data_with_proportions)
}

pa_OD_county_2020 <- make_proportion_column_cuebiq(pa_OD_county_2020)

### make clean county code for merging
pa_OD_county_2020$start_county_clean <- paste0(42, pa_OD_county_2020$start_county)
pa_OD_county_2020$end_county_clean <- paste0(42, pa_OD_county_2020$end_county)

### merge with population demographics to get names
pa_OD_county_2020_names <- merge(pa_OD_county_2020, pa_county_pops, by.x = "start_county_clean", by.y = "clean_county_code") %>%
  rename("start_county_name" = "Name",
         "origin_pop" = "Total.Population")
pa_OD_county_2020_names <- merge(pa_OD_county_2020_names, pa_county_pops, by.x = "end_county_clean", by.y = "clean_county_code") %>%
  rename("end_county_name" = "Name",
         "destination_pop" = "Total.Population")

### make mob matrix. Any OD combo with no travel is changed to 0
penn_mob_matrix <- get_mob_matrix(orig = pa_OD_county_2020_names$start_county_name,
                                      dest = pa_OD_county_2020_names$end_county_name,
                                      value = pa_OD_county_2020_names$prop_of_origin_travel)
penn_mob_matrix[is.na(penn_mob_matrix)] = 0


double = penn_mob_matrix + penn_mob_matrix



##########
########## read in mid_atl_county_full_dems.csv, whcih has distances between counties
mid_atl_county_full_dems <- read_csv(file= "~/Documents/Mobility/Dems/mid_atl_county_full_dems.csv")
mid_atl_county_full_dems$origin_county <- as.character(mid_atl_county_full_dems$origin_county)
mid_atl_county_full_dems$destination_county <- as.character(mid_atl_county_full_dems$destination_county)

## join with list of pa counties to get full list of OD pairs
pa_OD_county_2020_names_dems <- left_join(pa_OD_county_2020_names,
                                          mid_atl_county_full_dems,
                                          by = c(
                                            "start_county_clean" = ("origin_county"),
                                            "end_county_clean" = "destination_county"
                                          ))

### use get_mob_matrix to get matrix of distances between OD pairs
penn_dist_matrix <- get_mob_matrix(orig = pa_OD_county_2020_names_dems$start_county_name,
                                       dest = pa_OD_county_2020_names_dems$end_county_name,
                                       value = pa_OD_county_2020_names_dems$distance_km)


# Compute route-specific decay rates
compute_beta_ij <- function(distance_matrix, beta_0, alpha) {
  beta_matrix <- beta_0 * exp(-alpha * distance_matrix)  # Apply exponential decay
  diag(beta_matrix) <- Inf  # No decay for same-location trips
  return(beta_matrix)
}

beta_0 <- 0.4   # Maximum decay parameter from Hay paper
alpha <- 0.0014 # Scaling factor


# Generate the beta matrix using distances
penn_beta_matrix <- compute_beta_ij(penn_dist_matrix, beta_0, alpha)


## finally, need population by county
penn_pops <- pa_county_pops %>% dplyr::select(Name, Total.Population) %>% rename("Population" = "Total.Population")



###############
####### simulate travel using aggregated mobility data
###############

# Define folder path
output_folder <- "~/Documents/Mobility/Alpha_wave/Location_sims/Penn_agg_mobility"
n_simulations = 10

# Run and save each simulation
for (i in 1:n_simulations) {
  df <- simulate_mobility_return(n_people = 20000, n_timesteps = 250,
                                 travel_matrix = penn_mob_matrix,
                                 pop_data = penn_pops,
                                 beta_matrix = penn_beta_matrix, chunk_size = 1)
  df$simulation <- i  # Add a simulation ID
  file_name <- sprintf("%s/sim_agg_%d.csv", output_folder, i)
  fwrite(df, file_name)  # Save as CSV
  print(i)
}


###############
####### simulate travel using aggregated mobility data, but adjusted for both heterogeneities
###############

#### clean up md counties with dems by adding origin/dest pop/100k, a same OD indicator variable, and clean distance_km
pa_OD_county_2020_names_dems_clean <- pa_OD_county_2020_names_dems %>%
  mutate(origin_pop_per100k = ifelse(is.na(origin_pop_per100k), origin_pop/100000, origin_pop_per100k),
         destination_pop_per100k = ifelse(is.na(destination_pop_per100k), destination_pop/100000, destination_pop_per100k),
         same_OD_ind = ifelse(start_county_name == end_county_name, 1, 0),
         distance_km = ifelse(start_county_name == end_county_name, 0, distance_km)) %>%
  rename("aggregated_total_clean" = "trips_per_day_clean") %>%
  dplyr::select(start_county_name, end_county_name, aggregated_total_clean, prop_of_origin_travel,
                origin_pop_per100k, destination_pop_per100k, distance_km, same_OD_ind)


### scale predictor variables
pa_OD_county_2020_names_dems_clean$scaled_distance_km = scale(pa_OD_county_2020_names_dems_clean$distance_km)
pa_OD_county_2020_names_dems_clean$scaled_destination_pop_per100k = scale(pa_OD_county_2020_names_dems_clean$destination_pop_per100k)
pa_OD_county_2020_names_dems_clean$scaled_origin_pop_per100k = scale(pa_OD_county_2020_names_dems_clean$origin_pop_per100k)
pa_OD_county_2020_names_dems_clean$scaled_prop_of_origin_travel = scale(pa_OD_county_2020_names_dems_clean$prop_of_origin_travel)


### seperate by intra and inter-district pairs
pa_OD_county_2020_names_dems_clean_diffODS <- filter(pa_OD_county_2020_names_dems_clean, same_OD_ind == 0)
pa_OD_county_2020_names_dems_clean_sameODS <- filter(pa_OD_county_2020_names_dems_clean, same_OD_ind == 1)

### since we don't have travel frequency heterogeneity for 2020, we have to predict it
### predict both the existence of travel het and shape of trips per person distribution

## here, predict whether there is travel het in an OD pair using Cuebiq prediction model (see Cuebiq_het_clean.R)
pa_OD_county_2020_het_preds <- pa_OD_county_2020_names_dems_clean_diffODS %>%
  mutate(has_het_pred = ifelse(predict.glm(cuebiq_het_model,
                                           newdata = pa_OD_county_2020_names_dems_clean_diffODS,
                                           type = "response") > cuebiq_optimal_cutoff$threshold, 1, 0))


###now take only the ones that were predicted to have het
pa_OD_county_2020_preds_has_het <- filter(pa_OD_county_2020_het_preds, has_het_pred == 1)

## now predict negative binomial size and mu for those that are predicted to have travel het
pa_OD_county_2020_preds_negbi_params <- pa_OD_county_2020_preds_has_het %>%
  mutate(pred_neg_bi_size = predict.lm(cuebiq.negbi.size.mod, newdata = pa_OD_county_2020_preds_has_het, type = "response"),
         pred_neg_bi_mu = predict.lm(cuebiq.negbi.mu.mod, newdata = pa_OD_county_2020_preds_has_het, type = "response"))

#if the predictions are less than 0, put them in as 0.01
pa_OD_county_2020_preds_negbi_params <- pa_OD_county_2020_preds_negbi_params %>% mutate(pred_neg_bi_size = ifelse(pred_neg_bi_size < 0 , 0.01, pred_neg_bi_size),
                                                                                        pred_neg_bi_mu = ifelse(pred_neg_bi_mu < 0 , 0.01, pred_neg_bi_mu))

####now merge the together dataset with the preds_het to get all diff ODs together
pa_OD_county_2020_all_preds <-merge(pa_OD_county_2020_preds_negbi_params, pa_OD_county_2020_het_preds,
                                    by = c("start_county_name", "end_county_name", "aggregated_total_clean",
                                           "prop_of_origin_travel", "origin_pop_per100k", "same_OD_ind",
                                           "destination_pop_per100k", "distance_km", "scaled_origin_pop_per100k",
                                           "scaled_prop_of_origin_travel", "has_het_pred"), all = T,
                                    no.dups = T)

## now merger with same ODs to get all OD pairs together with predictions
pa_OD_county_2020_all_preds <- bind_rows(pa_OD_county_2020_all_preds, pa_OD_county_2020_names_dems_clean_sameODS)

## as with Zambia data, find the
## predicted number of people who travelled in each OD pair
## predicted multiplier: used for pred_dist_percent. basically averages out proportion of origin travel for people who take multiple trips
## pred_pmf: porportion of people that take 1,2,3,etc. trips in OD pairs (i.e. (80%, 10%, 10%) means 80% take 1 trip, 10% 2 trips, 10% 3 trips)
## pred_dist_percent: proportion of origin travel as a function of trips per person distribution (i.e. 5% becomes (2.5%, 5%, and 7.5%) in OD pair that has 1,2,3 trips per person
pa_OD_county_2020_all_preds_pmf <- pa_OD_county_2020_all_preds %>%
  rowwise() %>%
  mutate(predicted_people = ifelse(has_het_pred == 1 & same_OD_ind == 0, aggregated_total_clean/(1+pred_neg_bi_mu), aggregated_total_clean),
         pred_multiplier = (predicted_people*prop_of_origin_travel)/aggregated_total_clean,
         pred_pmf = ifelse(has_het_pred == 1, list(get_nb_pmf(pred_neg_bi_size, pred_neg_bi_mu)), list(1)),
         pred_pmf_clean = ifelse(is.null(pred_pmf), list(1), list(pred_pmf)),
         pred_dist_percent = ifelse(has_het_pred == 1 & same_OD_ind == 0,
                                    list(c(1:length(pred_pmf_clean)) * pred_multiplier),
                                    list(prop_of_origin_travel))) %>%
  ungroup()


## select only the useful variables
pa_OD_county_2020_all_preds_pmf_clean <- pa_OD_county_2020_all_preds_pmf %>%
  dplyr::select(start_county_name, end_county_name, same_OD_ind,
                prop_of_origin_travel, pred_multiplier, pred_dist_percent,
                pred_pmf_clean)


### first create matrix with prop of origin travel with either multiple values (if travel het) or single value (no travel het or same+
# Convert to matrix format
pa_OD_county_2020_dist_percent <- pa_OD_county_2020_all_preds_pmf_clean %>% dplyr::select(start_county_name, end_county_name, pred_dist_percent) %>%
  arrange(start_county_name, end_county_name) %>%
  pivot_wider(names_from = end_county_name, values_from = pred_dist_percent, values_fill = list(0))

pa_OD_county_2020_dist_percent_matrix <- as.matrix(pa_OD_county_2020_dist_percent[, -1])  # Remove origin column for matrix format
rownames(pa_OD_county_2020_dist_percent_matrix) <- pa_OD_county_2020_dist_percent$start_county_name  # Set row names

### now create matrix with probability of each probability of travel (i.e. a 70% chance of having a 0.12% chance of travel from A to B)
# Convert to matrix format
pa_OD_county_2020_pred_pmf <- pa_OD_county_2020_all_preds_pmf_clean %>% dplyr::select(start_county_name, end_county_name, pred_pmf_clean) %>%
  arrange(start_county_name, end_county_name) %>%
  pivot_wider(names_from = end_county_name, values_from = pred_pmf_clean, values_fill = list(1))

pa_OD_county_2020_pred_pmf_matrix <- as.matrix(pa_OD_county_2020_pred_pmf[, -1])  # Remove origin column for matrix format
rownames(pa_OD_county_2020_pred_pmf_matrix) <- pa_OD_county_2020_pred_pmf$start_county_name  # Set row names


###############
######## simulate location using aggregated totals adjusted for both heterogeneities
###############

#### same as previous both het function, but with different non-mover percentage that resets every day
### this data used all trips at the county level looking over a day, which is 19% movers, 81% non-movers
simulate_mobility_het_obs_only_prob_move_cuebiq <- function(n_people, n_timesteps, dist_percent_matrix, pmf_matrix, pop_data,
                                                            beta_matrix, chunk_size = 100) {
  # Function to sample trip duration using exponenitla decayscaled by β_ij
  sample_trip_length <- function(beta_ij) {  # Ensures min 1-day trips
    return(ceiling(rexp(1, rate = beta_ij)))  # Scale by inverse decay
  }

  # Create initial locations based on population proportions
  locations <- rownames(dist_percent_matrix)
  population_probs <- pop_data$Population / sum(pop_data$Population)
  initial_locations <- sample(locations, size = n_people, replace = TRUE, prob = population_probs)

  # Sample individual travel probabilities from OD-specific probability distributions
  individual_travel_probs <- matrix(NA, nrow = n_people, ncol = length(locations), dimnames = list(NULL, locations))

  individ_probs_function <- function(people){
    for (person in people) {
      origin <- initial_locations[person]
      for (destination in locations) {
        individual_travel_probs[person, destination] <- ifelse(length(pmf_matrix[[origin, destination]]) == 1,
                                                               dist_percent_matrix[[origin, destination]],
                                                               sample(dist_percent_matrix[[origin, destination]], size = 1,
                                                                      prob = pmf_matrix[[origin, destination]]))
      }
    }
    return(individual_travel_probs)
  }
  individual_travel_probs <- individ_probs_function(people = 1:n_people)

  # Initialize an empty list to store chunk results
  chunk_results <- list()

  # Process people in chunks
  for (chunk_start in seq(1, n_people, by = chunk_size)) {
    chunk_end <- min(chunk_start + chunk_size - 1, n_people)
    chunk_ids <- chunk_start:chunk_end

    # Create an empty data frame for the current chunk
    sim_results_chunk <- data.frame(
      step = rep(1:n_timesteps, each = length(chunk_ids)),
      uid = rep(chunk_ids, times = n_timesteps),
      location = NA,
      return_step = NA
    )

    # Set initial locations for the chunk
    sim_results_chunk$location[sim_results_chunk$step == 1] <- initial_locations[chunk_ids]

    # Loop over each person in the current chunk
    for (person in chunk_ids) {
      origin <- initial_locations[person]
      never_mover_prob = 0.81
      never_mover <- runif(1) < never_mover_prob
      return_step <- NA
      current_location <- origin



      for (t in 2:n_timesteps) {
        never_mover <- runif(1) < never_mover_prob


        if (never_mover) {
          current_location <- origin
          return_step <- NA
        }
        else {
          prev_location <- sim_results_chunk$location[sim_results_chunk$uid == person & sim_results_chunk$step == (t - 1)]

          individual_travel_probs <- individ_probs_function(people = person)

          # If person is scheduled to return, force return
          if (!is.na(return_step) && t == return_step) {
            current_location <- origin
            return_step <- NA
          } else if (is.na(return_step)) {
            # Sample movement based on the individual's sampled probability
            travel_probs <- individual_travel_probs[person, ]
            new_location <- ifelse(sum(travel_probs) != 0, sample(names(travel_probs), size = 1, prob = travel_probs), current_location)

            # Compute β_ij for this trip
            beta_ij <- beta_matrix[prev_location, new_location]

            # If the person moves away, set return time based on β_ij
            if (new_location != origin) {
              return_step <- t + sample_trip_length(beta_ij)
            }
            current_location <- new_location
          }
        }


        # Store results for the current person and timestep
        sim_results_chunk$location[sim_results_chunk$uid == person & sim_results_chunk$step == t] <- current_location
        sim_results_chunk$return_step[sim_results_chunk$uid == person & sim_results_chunk$step == t] <- return_step
      }
    }

    # Store the results of the current chunk in the list
    chunk_results[[length(chunk_results) + 1]] <- sim_results_chunk

    # Optionally, print progress
    cat("Processed chunk:", chunk_start, "to", chunk_end, "\n")
  }

  # Combine all chunks into one data frame
  sim_results <- do.call(rbind, chunk_results)
  sim_results <- sim_results %>% mutate(uid = uid - 1,
                                        step = step - 1)

  return(sim_results)
}



# Define folder path
output_folder <- "~/Documents/Mobility/Alpha_wave/Location_sims/Penn_both_het_mobility"
n_simulations = 10

# Run and save each simulation
for (i in 3:n_simulations) {
  df <- simulate_mobility_het_obs_only_prob_move_cuebiq(n_people = 20000, 225, pa_OD_county_2020_dist_percent_matrix,
                                                        pa_OD_county_2020_pred_pmf_matrix, pop_data = penn_pops,
                                                      penn_beta_matrix, chunk_size = 1)
  df$simulation <- i  # Add a simulation ID
  file_name <- sprintf("%s/sim_both_het_%d.csv", output_folder, i)
  fwrite(df, file_name)  # Save as CSV
  print(i)
}








################
################
###### analyzing sim results from starsim
################
################




#### function for cleaning mobility sim data from starsim
clean_alphawave_func <- function(sim_list, sim_length){
  all_data <- data.frame()

  for (sim in 1:length(sim_list)){
    sim_df <- sim_list[[sim]]
    colnames(sim_df) <- penn_pops$Name
    sim_df <- sim_df %>%
      mutate(time_step = 0:sim_length) %>%  # Add time step column
      relocate(time_step)


    cum_sim_df <- sim_df
    cum_sim_df[, -1] <- apply(cum_sim_df[, -1], 2, cumsum)
    cum_sim_df$total_count <- rowSums(cum_sim_df[, -1], na.rm = TRUE)  # Excluding the time column
    cum_sim_df$nonzero_count <- rowSums(cum_sim_df[, c(-1, -69)] != 0, na.rm = TRUE)  # Excluding the time column

    sim_df$total_new_inf <- rowSums(sim_df[, -1], na.rm = TRUE)


    total_infections_sim <- max(cum_sim_df$total_count)
    num_districts_inf_sim <- max(cum_sim_df$nonzero_count)
    time_to_10_sim <- cum_sim_df$time_step[which(cum_sim_df$nonzero_count >= 10)[1]]
    time_to_50per_sim <- cum_sim_df$time_step[which(cum_sim_df$nonzero_count >= 12)[1]]
    time_to_24_sim <- cum_sim_df$time_step[which(cum_sim_df$nonzero_count >= 24)[1]]
    time_max_inf_sim <- sim_df$time_step[which.max(sim_df$total_new_inf)[1]]

    sim_df$sim_id <- as.character(sim)
    all_data <- bind_rows(all_data, sim_df, .id = "sim_id")
  }
  return(all_data)
}


##########
##########
#### sim results using beta fit to even mixedly pop
##########
##########


### read in and clean aggregated and both het mobility sims
pa_agg_mobility_even_mixed_sims <- py_load_object("~/Documents/Mobility/Alpha_wave/Starsim_sims/Penn_agg_mobility_fit_well_mixed_sims.pkl")
pa_agg_mobility_even_mixed_sims_clean <- clean_alphawave_func(pa_agg_mobility_even_mixed_sims, 224)

pa_both_het_mobility_even_mixed_sims <- py_load_object("~/Documents/Mobility/Alpha_wave/Starsim_sims/Penn_both_het_mobility_fit_well_mixed_sims.pkl")
pa_both_het_mobility_even_mixed_sims_clean <- clean_alphawave_func(pa_both_het_mobility_even_mixed_sims, 224)

## read in in no mobility sim results (already in median)
pa_no_mobility_even_mixed_sims <- read.csv("~/Documents/Mobility/Alpha_wave/Starsim_sims/Penn_even_mobility_fit_well_mixed_sims.csv")
pa_no_mobility_even_mixed_sims <- pa_no_mobility_even_mixed_sims[,c("timevec.1", "seir_new_infections")]

### read in Penn inc rates empirical
penn_inc <- read.csv(file = "~/Desktop/penn_inc_rates.csv") %>% dplyr::select(time_value, cases)


### get median new infections across all sims for agg and both het
pa_agg_mobility_even_mixed_new_inf <- pa_agg_mobility_even_mixed_sims_clean %>%
  group_by(time_step) %>%
  summarise(median_new_infections = median(total_new_inf, na.rm = TRUE), .groups = "drop")

pa_both_het_mobility_even_mixed_new_inf <- pa_both_het_mobility_even_mixed_sims_clean %>%
  group_by(time_step) %>%
  summarise(median_new_infections = median(total_new_inf, na.rm = TRUE), .groups = "drop")

### change time step to date for necessary dfs
pa_agg_mobility_even_mixed_new_inf$time_value <- seq(as.Date("2020-09-17"), as.Date("2021-4-29"), by = "day")
pa_both_het_mobility_even_mixed_new_inf$time_value <- seq(as.Date("2020-09-17"), as.Date("2021-4-29"), by = "day")

pa_agg_mobility_even_mixed_new_inf <- pa_agg_mobility_even_mixed_new_inf[, c("time_value", "median_new_infections")]
pa_both_het_mobility_even_mixed_new_inf <- pa_both_het_mobility_even_mixed_new_inf[, c("time_value", "median_new_infections")]

### align columns names
colnames(pa_agg_mobility_even_mixed_new_inf) <- c("Date", "New_infections")
colnames(pa_both_het_mobility_even_mixed_new_inf) <- c("Date", "New_infections")
colnames(pa_no_mobility_even_mixed_sims) <- c("Date", "New_infections")
colnames(penn_inc) <- c("Date", "New_infections")

### divide all but penn_inc cases by 5 to compare to per 100,000
pa_agg_mobility_even_mixed_new_inf$New_infections <- (pa_agg_mobility_even_mixed_new_inf$New_infections)
pa_both_het_mobility_even_mixed_new_inf$New_infections <- pa_both_het_mobility_even_mixed_new_inf$New_infections
pa_no_mobility_even_mixed_sims$New_infections <- pa_no_mobility_even_mixed_sims$New_infections

## add in data generating name
pa_agg_mobility_even_mixed_new_inf$data <- "Aggregated mobility"
pa_both_het_mobility_even_mixed_new_inf$data <- "Both heterogeneities mobility"
pa_no_mobility_even_mixed_sims$data <- "Evenly mixed population"
penn_inc$data <- "Observed"

## combine all four datatypes
pa_combined_alphawave_even_mixed_newinf <- rbind(pa_agg_mobility_even_mixed_new_inf, pa_both_het_mobility_even_mixed_new_inf, pa_no_mobility_even_mixed_sims,
                                                 penn_inc)

## factor data type levels
pa_combined_alphawave_even_mixed_newinf$data <- factor(pa_combined_alphawave_even_mixed_newinf$data,
                                                    levels = c("Observed", "Evenly mixed population",
                                                               "Aggregated mobility", "Both heterogeneities mobility"))
## plot all four mean trajectories on the same graph
pa_even_mixed <- ggplot(pa_combined_alphawave_even_mixed_newinf) +
  geom_line(aes(x = Date, y = New_infections, color = data)) +
  scale_color_viridis(discrete = T, begin = 0.1, end = 0.8, option = "B") +
  theme_minimal() +
  scale_x_date(date_labels = "%b' %y", limits = c(as.Date("2020-10-01"), as.Date("2021-02-28"))) +
  ylab("New infections (per 100,000 people)") +
  theme(legend.title = element_blank(),
        legend.position = c(0.2, 0.8),
        legend.background = element_rect(fill="white",
                                         size=0.3, linetype="solid"))

pa_even_mixed

sum(pa_no_mobility_sims$New_infections)
sum(penn_inc$New_infections)


#### time of max infections
pa_agg_mobility_even_mixed_new_inf$Date[which.max(pa_agg_mobility_even_mixed_new_inf$New_infections)[1]]
pa_both_het_mobility_even_mixed_new_inf$Date[which.max(pa_both_het_mobility_even_mixed_new_inf$New_infections)[1]]
pa_no_mobility_even_mixed_sims$Date[which.max(pa_no_mobility_even_mixed_sims$New_infections)[1]]
penn_inc$Date[which.max(penn_inc$New_infections)[1]]

### difference between max infections of real incidence versus each simulation
pa_agg_mobility_even_mixed_new_inf$New_infections[which.max(pa_agg_mobility_even_mixed_new_inf$New_infections)[1]] - penn_inc$New_infections[which.max(penn_inc$New_infections)[1]]
pa_both_het_mobility_even_mixed_new_inf$New_infections[which.max(pa_both_het_mobility_even_mixed_new_inf$New_infections)[1]] - penn_inc$New_infections[which.max(penn_inc$New_infections)[1]]
pa_no_mobility_even_mixed_sims$New_infections[which.max(pa_no_mobility_even_mixed_sims$New_infections)[1]] - penn_inc$New_infections[which.max(penn_inc$New_infections)[1]]
penn_inc$New_infections[which.max(penn_inc$New_infections)[1]]




##########
##########
#### sim results using beta fit to non-mixed pop
##########
##########

### read in and clean aggregated and both het mobility sims
pa_agg_mobility_no_mob_sims <- py_load_object("~/Documents/Mobility/Alpha_wave/Starsim_sims/Penn_agg_mobility_fit_no_mob_sims.pkl")
pa_agg_mobility_no_mob_sims_clean <- clean_alphawave_func(pa_agg_mobility_no_mob_sims, 224)

pa_both_het_mobility_no_mob_sims <- py_load_object("~/Documents/Mobility/Alpha_wave/Starsim_sims/Penn_both_het_mobility_fit_no_mob_sims.pkl")
pa_both_het_mobility_no_mob_sims_clean <- clean_alphawave_func(pa_both_het_mobility_no_mob_sims, 224)

## read in in no mobility sim results (already in median)
pa_no_mobility_no_mob_sims <- read.csv("~/Documents/Mobility/Alpha_wave/Starsim_sims/Penn_even_mobility_fit_no_mob_sims.csv")
pa_no_mobility_no_mob_sims <- pa_no_mobility_no_mob_sims[,c("timevec.1", "seir_new_infections")]

### read in Penn inc rates empirical
penn_inc <- read.csv(file = "~/Desktop/penn_inc_rates.csv") %>% dplyr::select(time_value, cases)


### get median new infections across all sims for agg and both het
pa_agg_mobility_no_mob_new_inf <- pa_agg_mobility_no_mob_sims_clean %>%
  group_by(time_step) %>%
  summarise(median_new_infections = median(total_new_inf, na.rm = TRUE), .groups = "drop")

pa_both_het_mobility_no_mob_new_inf <- pa_both_het_mobility_no_mob_sims_clean %>%
  group_by(time_step) %>%
  summarise(median_new_infections = median(total_new_inf, na.rm = TRUE), .groups = "drop")

### change time step to date for necessary dfs
pa_agg_mobility_no_mob_new_inf$time_value <- seq(as.Date("2020-09-17"), as.Date("2021-4-29"), by = "day")
pa_both_het_mobility_no_mob_new_inf$time_value <- seq(as.Date("2020-09-17"), as.Date("2021-4-29"), by = "day")

pa_agg_mobility_no_mob_new_inf <- pa_agg_mobility_no_mob_new_inf[, c("time_value", "median_new_infections")]
pa_both_het_mobility_no_mob_new_inf <- pa_both_het_mobility_no_mob_new_inf[, c("time_value", "median_new_infections")]

### align columns names
colnames(pa_agg_mobility_no_mob_new_inf) <- c("Date", "New_infections")
colnames(pa_both_het_mobility_no_mob_new_inf) <- c("Date", "New_infections")
colnames(pa_no_mobility_no_mob_sims) <- c("Date", "New_infections")
colnames(penn_inc) <- c("Date", "New_infections")

### divide all but penn_inc cases by 5 to compare to per 100,000
pa_agg_mobility_no_mob_new_inf$New_infections <- (pa_agg_mobility_no_mob_new_inf$New_infections)
pa_both_het_mobility_no_mob_new_inf$New_infections <- pa_both_het_mobility_no_mob_new_inf$New_infections
pa_no_mobility_no_mob_sims$New_infections <- pa_no_mobility_no_mob_sims$New_infections

## add in data generating name
pa_agg_mobility_no_mob_new_inf$data <- "Aggregated mobility"
pa_both_het_mobility_no_mob_new_inf$data <- "Both heterogeneities mobility"
pa_no_mobility_no_mob_sims$data <- "Evenly mixed population"
penn_inc$data <- "Observed"

## combine all four datatypes
pa_combined_alphawave_no_mob_newinf <- rbind(pa_agg_mobility_no_mob_new_inf, pa_both_het_mobility_no_mob_new_inf, pa_no_mobility_no_mob_sims,
                                             penn_inc)

## factor data type levels
pa_combined_alphawave_no_mob_newinf$data <- factor(pa_combined_alphawave_no_mob_newinf$data,
                                                levels = c("Observed", "Evenly mixed population",
                                                           "Aggregated mobility", "Both heterogeneities mobility"))
## plot all four mean trajectories on the same graph
pa_no_mob <- ggplot(pa_combined_alphawave_no_mob_newinf) +
  geom_line(aes(x = Date, y = New_infections, color = data)) +
  scale_color_viridis(discrete = T, begin = 0.1, end = 0.8, option = "B") +
  theme_minimal() +
  scale_x_date(date_labels = "%b' %y", limits = c(as.Date("2020-10-01"), as.Date("2021-02-28"))) +
  ylab("New infections (per 100,000 people)") +
  theme(legend.title = element_blank(),
        legend.position = c(0.2, 0.8),
        legend.background = element_rect(fill="white",
                                         size=0.3, linetype="solid"))
pa_no_mob


sum(pa_no_mobility_sims$New_infections)
sum(penn_inc$New_infections)


#### time of max infections
pa_agg_mobility_no_mob_new_inf$Date[which.max(pa_agg_mobility_no_mob_new_inf$New_infections)[1]]
pa_both_het_mobility_no_mob_new_inf$Date[which.max(pa_both_het_mobility_no_mob_new_inf$New_infections)[1]]
pa_no_mobility_no_mob_sims$Date[which.max(pa_no_mobility_no_mob_sims$New_infections)[1]]
penn_inc$Date[which.max(penn_inc$New_infections)[1]]

### difference between max infections of real incidence versus each simulation
pa_agg_mobility_no_mob_new_inf$New_infections[which.max(pa_agg_mobility_no_mob_new_inf$New_infections)[1]] - penn_inc$New_infections[which.max(penn_inc$New_infections)[1]]
pa_both_het_mobility_no_mob_new_inf$New_infections[which.max(pa_both_het_mobility_no_mob_new_inf$New_infections)[1]] - penn_inc$New_infections[which.max(penn_inc$New_infections)[1]]
pa_no_mobility_no_mob_sims$New_infections[which.max(pa_no_mobility_no_mob_sims$New_infections)[1]] - penn_inc$New_infections[which.max(penn_inc$New_infections)[1]]
penn_inc$New_infections[which.max(penn_inc$New_infections)[1]]







##########
##########
#### sim results using beta fit to both heterogeneities model
##########
##########

### read in and clean aggregated and both het mobility sims
pa_agg_mobility_bh_sims <- py_load_object("~/Documents/Mobility/Alpha_wave/Starsim_sims/Penn_agg_mobility_fit_bh_sims.pkl")
pa_agg_mobility_bh_sims_clean <- clean_alphawave_func(pa_agg_mobility_bh_sims, 224)

pa_both_het_mobility_bh_sims <- py_load_object("~/Documents/Mobility/Alpha_wave/Starsim_sims/Penn_both_het_mobility_fit_bh_sims.pkl")
pa_both_het_mobility_bh_sims_clean <- clean_alphawave_func(pa_both_het_mobility_bh_sims, 224)

## read in in no mobility sim results (already in median)
pa_no_mobility_bh_sims <- read.csv("~/Documents/Mobility/Alpha_wave/Starsim_sims/Penn_even_mobility_fit_bh_sims.csv")
pa_no_mobility_bh_sims <- pa_no_mobility_bh_sims[,c("timevec.1", "seir_new_infections")]

### read in Penn inc rates empirical
penn_inc <- read.csv(file = "~/Desktop/penn_inc_rates.csv") %>% dplyr::select(time_value, cases)


### get median new infections across all sims for agg and both het
pa_agg_mobility_bh_new_inf <- pa_agg_mobility_bh_sims_clean %>%
  group_by(time_step) %>%
  summarise(median_new_infections = median(total_new_inf, na.rm = TRUE), .groups = "drop")

pa_both_het_mobility_bh_new_inf <- pa_both_het_mobility_bh_sims_clean %>%
  group_by(time_step) %>%
  summarise(median_new_infections = median(total_new_inf, na.rm = TRUE), .groups = "drop")

### change time step to date for necessary dfs
pa_agg_mobility_bh_new_inf$time_value <- seq(as.Date("2020-09-17"), as.Date("2021-4-29"), by = "day")
pa_both_het_mobility_bh_new_inf$time_value <- seq(as.Date("2020-09-17"), as.Date("2021-4-29"), by = "day")

pa_agg_mobility_bh_new_inf <- pa_agg_mobility_bh_new_inf[, c("time_value", "median_new_infections")]
pa_both_het_mobility_bh_new_inf <- pa_both_het_mobility_bh_new_inf[, c("time_value", "median_new_infections")]

### align columns names
colnames(pa_agg_mobility_bh_new_inf) <- c("Date", "New_infections")
colnames(pa_both_het_mobility_bh_new_inf) <- c("Date", "New_infections")
colnames(pa_no_mobility_bh_sims) <- c("Date", "New_infections")
colnames(penn_inc) <- c("Date", "New_infections")

### divide all but penn_inc cases by 5 to compare to per 100,000
pa_agg_mobility_bh_new_inf$New_infections <- (pa_agg_mobility_bh_new_inf$New_infections)
pa_both_het_mobility_bh_new_inf$New_infections <- pa_both_het_mobility_bh_new_inf$New_infections
pa_no_mobility_bh_sims$New_infections <- pa_no_mobility_bh_sims$New_infections

## add in data generating name
pa_agg_mobility_bh_new_inf$data <- "Aggregated mobility"
pa_both_het_mobility_bh_new_inf$data <- "Both heterogeneities mobility"
pa_no_mobility_bh_sims$data <- "Evenly mixed population"
penn_inc$data <- "Observed"

## combine all four datatypes
pa_combined_alphawave_bh_newinf <- rbind(pa_agg_mobility_bh_new_inf, pa_both_het_mobility_bh_new_inf, pa_no_mobility_bh_sims,
                                         penn_inc)

## factor data type levels
pa_combined_alphawave_bh_newinf$data <- factor(pa_combined_alphawave_bh_newinf$data,
                                            levels = c("Observed", "Evenly mixed population",
                                                       "Aggregated mobility", "Both heterogeneities mobility"))
## plot all four mean trajectories on the same graph
pa_bh <- ggplot(pa_combined_alphawave_bh_newinf) +
  geom_line(aes(x = Date, y = New_infections, color = data)) +
  scale_color_viridis(discrete = T, begin = 0.1, end = 0.8, option = "B") +
  theme_minimal() +
  scale_x_date(date_labels = "%b' %y", limits = c(as.Date("2020-10-01"), as.Date("2021-02-28"))) +
  ylab("New infections (per 100,000 people)") +
  theme(legend.title = element_blank(),
        legend.position = c(0.2, 0.8),
        legend.background = element_rect(fill="white",
                                         size=0.3, linetype="solid"))
pa_bh


sum(pa_no_mobility_sims$New_infections)
sum(penn_inc$New_infections)


#### time of max infections
pa_agg_mobility_bh_new_inf$Date[which.max(pa_agg_mobility_bh_new_inf$New_infections)[1]]
pa_both_het_mobility_bh_new_inf$Date[which.max(pa_both_het_mobility_bh_new_inf$New_infections)[1]]
pa_no_mobility_bh_sims$Date[which.max(pa_no_mobility_bh_sims$New_infections)[1]]
penn_inc$Date[which.max(penn_inc$New_infections)[1]]

### difference between max infections of real incidence versus each simulation
pa_agg_mobility_bh_new_inf$New_infections[which.max(pa_agg_mobility_bh_new_inf$New_infections)[1]] - penn_inc$New_infections[which.max(penn_inc$New_infections)[1]]
pa_both_het_mobility_bh_new_inf$New_infections[which.max(pa_both_het_mobility_bh_new_inf$New_infections)[1]] - penn_inc$New_infections[which.max(penn_inc$New_infections)[1]]
pa_no_mobility_bh_sims$New_infections[which.max(pa_no_mobility_bh_sims$New_infections)[1]] - penn_inc$New_infections[which.max(penn_inc$New_infections)[1]]
penn_inc$New_infections[which.max(penn_inc$New_infections)[1]]







#### put even_mixed plots
md_even_mixed <- md_even_mixed + ggtitle("Maryland")
pa_even_mixed <- pa_even_mixed + ggtitle("Pennsylvania")

even_mixed_plot <- md_even_mixed + pa_even_mixed + plot_layout(guides = "collect") & theme(legend.position = "bottom")
even_mixed_plot



### rest of plots
alpha_supp_layout <- "
  12
  34
"

md_no_mob <- md_no_mob + labs(title = "Maryland",
                             subtitle = "Fit to no mobility model")
pa_no_mob <- pa_no_mob + labs(title = "Pennsylvania",
                              subtitle = "Fit to no mobility model")
md_bh <- md_bh + labs(subtitle = "Fit to heterogeneous travel model")
pa_bh <- pa_bh + labs(subtitle = "Fit to heterogeneous travel model")

alpha_supp_plot <- md_no_mob + pa_no_mob + md_bh + pa_bh +
  plot_layout(design = alpha_supp_layout, guides = "collect", widths = c(1, 1),
              heights = c(1, 1)) & theme(legend.position = "bottom")
alpha_supp_plot

md_no_mob











###############
######### Table of fits
###############

maryland_actual <- maryland_inc %>% filter(Date >= "2020-10-01" & Date <= "2021-02-28")

md_agg_mobility_no_mob_fortable <- md_agg_mobility_no_mob_new_inf %>% filter(Date >= "2020-10-01" & Date <= "2021-02-28")
md_both_het_mobility_no_mob_fortable <- md_both_het_mobility_no_mob_new_inf %>% filter(Date >= "2020-10-01" & Date <= "2021-02-28")
md_no_mobility_no_mob_fortable <- md_no_mobility_no_mob_sims %>% filter(Date >= "2020-10-01" & Date <= "2021-02-28")

md_agg_no_mob_mse <- mean((maryland_actual$New_infections - md_agg_mobility_no_mob_fortable$New_infections)^2)
md_agg_no_mob_mse

md_bh_no_mob_mse <- mean((maryland_actual$New_infections - md_both_het_mobility_no_mob_fortable$New_infections)^2)
md_bh_no_mob_mse

md_no_no_mob_mse <- mean((maryland_actual$New_infections - md_no_mobility_no_mob_fortable$New_infections)^2)
md_no_no_mob_mse

md_agg_mobility_even_mixed_fortable <- md_agg_mobility_even_mixed_new_inf %>% filter(Date >= "2020-10-01" & Date <= "2021-02-28")
md_both_het_mobility_even_mixed_fortable <- md_both_het_mobility_even_mixed_new_inf %>% filter(Date >= "2020-10-01" & Date <= "2021-02-28")
md_no_mobility_even_mixed_fortable <- md_no_mobility_even_mixed_sims %>% filter(Date >= "2020-10-01" & Date <= "2021-02-28")

md_agg_even_mixed_mse <- mean((maryland_actual$New_infections - md_agg_mobility_even_mixed_fortable$New_infections)^2)
md_agg_even_mixed_mse

md_bh_even_mixed_mse <- mean((maryland_actual$New_infections - md_both_het_mobility_even_mixed_fortable$New_infections)^2)
md_bh_even_mixed_mse

md_no_even_mixed_mse <- mean((maryland_actual$New_infections - md_no_mobility_even_mixed_fortable$New_infections)^2)
md_no_even_mixed_mse

md_agg_mobility_bh_fortable <- md_agg_mobility_bh_new_inf %>% filter(Date >= "2020-10-01" & Date <= "2021-02-28")
md_both_het_mobility_bh_fortable <- md_both_het_mobility_bh_new_inf %>% filter(Date >= "2020-10-01" & Date <= "2021-02-28")
md_no_mobility_bh_fortable <- md_no_mobility_bh_sims %>% filter(Date >= "2020-10-01" & Date <= "2021-02-28")

md_agg_bh_mse <- mean((maryland_actual$New_infections - md_agg_mobility_bh_fortable$New_infections)^2)
md_agg_bh_mse

md_bh_bh_mse <- mean((maryland_actual$New_infections - md_both_het_mobility_bh_fortable$New_infections)^2)
md_bh_bh_mse

md_no_bh_mse <- mean((maryland_actual$New_infections - md_no_mobility_bh_fortable$New_infections)^2)
md_no_bh_mse







penn_actual <- penn_inc %>% filter(Date >= "2020-10-01" & Date <= "2021-02-28")

pa_agg_mobility_no_mob_fortable <- pa_agg_mobility_no_mob_new_inf %>% filter(Date >= "2020-10-01" & Date <= "2021-02-28")
pa_both_het_mobility_no_mob_fortable <- pa_both_het_mobility_no_mob_new_inf %>% filter(Date >= "2020-10-01" & Date <= "2021-02-28")
pa_no_mobility_no_mob_fortable <- pa_no_mobility_no_mob_sims %>% filter(Date >= "2020-10-01" & Date <= "2021-02-28")

pa_agg_no_mob_mse <- mean((penn_actual$New_infections - pa_agg_mobility_no_mob_fortable$New_infections)^2)
pa_agg_no_mob_mse

pa_bh_no_mob_mse <- mean((penn_actual$New_infections - pa_both_het_mobility_no_mob_fortable$New_infections)^2)
pa_bh_no_mob_mse

pa_no_no_mob_mse <- mean((penn_actual$New_infections - pa_no_mobility_no_mob_fortable$New_infections)^2)
pa_no_no_mob_mse

pa_agg_mobility_even_mixed_fortable <- pa_agg_mobility_even_mixed_new_inf %>% filter(Date >= "2020-10-01" & Date <= "2021-02-28")
pa_both_het_mobility_even_mixed_fortable <- pa_both_het_mobility_even_mixed_new_inf %>% filter(Date >= "2020-10-01" & Date <= "2021-02-28")
pa_no_mobility_even_mixed_fortable <- pa_no_mobility_even_mixed_sims %>% filter(Date >= "2020-10-01" & Date <= "2021-02-28")

pa_agg_even_mixed_mse <- mean((penn_actual$New_infections - pa_agg_mobility_even_mixed_fortable$New_infections)^2)
pa_agg_even_mixed_mse

pa_bh_even_mixed_mse <- mean((penn_actual$New_infections - pa_both_het_mobility_even_mixed_fortable$New_infections)^2)
pa_bh_even_mixed_mse

pa_no_even_mixed_mse <- mean((penn_actual$New_infections - pa_no_mobility_even_mixed_fortable$New_infections)^2)
pa_no_even_mixed_mse

pa_agg_mobility_bh_fortable <- pa_agg_mobility_bh_new_inf %>% filter(Date >= "2020-10-01" & Date <= "2021-02-28")
pa_both_het_mobility_bh_fortable <- pa_both_het_mobility_bh_new_inf %>% filter(Date >= "2020-10-01" & Date <= "2021-02-28")
pa_no_mobility_bh_fortable <- pa_no_mobility_bh_sims %>% filter(Date >= "2020-10-01" & Date <= "2021-02-28")

pa_agg_bh_mse <- mean((penn_actual$New_infections - pa_agg_mobility_bh_fortable$New_infections)^2)
pa_agg_bh_mse

pa_bh_bh_mse <- mean((penn_actual$New_infections - pa_both_het_mobility_bh_fortable$New_infections)^2)
pa_bh_bh_mse

pa_no_bh_mse <- mean((penn_actual$New_infections - pa_no_mobility_bh_fortable$New_infections)^2)
pa_no_bh_mse










