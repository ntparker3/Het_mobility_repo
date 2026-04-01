########################
########################
######## load in required packages
library(dplyr)
library(data.table)
library(ggplot2)
library(pROC)
library(fitdistrplus)
library(transport)
library(philentropy)
library(mobility)
library(tidyr)
library(reticulate)
library(purrr)




######################################
######################################
############### Setting up population and distance data
######################################
######################################


####### create population and distance for each district, even those not included in the CRD

##read in the distance and population datasets
dist_data_full <- readRDS("~/Documents/Mobility/Zambia/Updated_CDR/dist_data_full.rds")
new_pops <- readxl::read_excel("~/Documents/Mobility/Zambia/Zambia_Population_2022.xlsx")

## fix format of population data
new_pops$District <- toupper(new_pops$District)
newpops <- rename(new_pops, 'Population' = '2022 Population')

newpops <- newpops %>%
  mutate(
    District = recode(District,
                      CHIENGE = "CHIENGI",
                      "LUNTE" = "LUNTE DISTRICT",
                      MILENGE = "MILENGI",
                      MUSHINDAMO = "MUSHINDANO",
                      CHIKANKATA = "CHIKANKANTA",
                      "SHIWANG'ANDU" = "SHIWAMG'ANDU"))


## fix format of distance data and convert to matrix
rownames(dist_data_full) <- toupper(rownames(dist_data_full))
colnames(dist_data_full) <- toupper(colnames(dist_data_full))

dist_data_full <- as.matrix(dist_data_full)


# create a distance dataframe with a row for each OD pair and its distance
dist.df <- reshape2::melt(dist_data_full, varnames = c("origin", "destination"), value.name = "distance")
dist.df$distance_km = dist.df$distance / 1000

## merge the two datasets together
pop.and.dist <- merge(dist.df, newpops, by.x = "origin", by.y = "District")
pop.and.dist <- rename(pop.and.dist, "origin_pop" = "Population")

pop.and.dist <- merge(pop.and.dist, newpops, by.x = "destination", by.y = "District")
pop.and.dist <- rename(pop.and.dist, "destination_pop" = "Population")


## mutate to include formatted variables (pop per 100k, log_distance), as well as indicators of whether its a new OD
##(not in May 2024 CDR) or a same OD (origin == destination)

#######
pop.and.dist <- pop.and.dist %>% mutate(origin_pop_per100k = origin_pop/100000,
                                        destination_pop_per100k = destination_pop/100000,
                                        log_distance_km = ifelse(distance_km == 0, 0, log(distance_km)),
                                        same_OD_ind = ifelse(origin == destination, 1, 0),
                                        new_OD = ifelse(!paste(origin, destination, sep = "_") %in%
                                                          paste(may_2024_s2$origin_clean, may_2024_s2$destination_clean, sep = "_"), 1, 0))

save(pop.and.dist, file = "~/Documents/Mobility/Zambia/Cleaning_modeling_code/Saved_objects/pop_and_dist")

load(file = "~/Documents/Mobility/Zambia/Cleaning_modeling_code/Saved_objects/pop_and_dist")





######################################
######################################
############### Cleaning up CDR data
######################################
######################################


### bring in May 2024 strategy 2 data to see which ODs are included in the CDR data

may_2024_s2 <- readRDS(file = "~/Documents/Mobility/Zambia/may_2024_15_strat2_new.rds")


#add in location indicators and a proportion of travel from origin column
may_2024_s2 <- add_location_indicator(may_2024_s2)
may_2024_s2 <- make_proportion_column_one_month(may_2024_s2)

# remove any NA in origin and destination
may_2024_s2 <- na.omit(may_2024_s2)

## drop unnecessary columns

may_2024_s2_clean <- may_2024_s2 %>% dplyr::select(- total_pop, - total_pop_per100k)

may_2024_s2_clean$month <- "May"


## add in heterogeneity indicator
may_2024_s2_clean <- mutate(may_2024_s2_clean, has_het = ifelse(percentage_ones == 100, 0, 1))

##save(may_2024_s2_clean, file = "~/Documents/Mobility/Zambia/Cleaning_modeling_code/Saved_objects/may_2024_s2_clean")

## load in clean file
load(file = "~/Documents/Mobility/Zambia/Cleaning_modeling_code/Saved_objects/may_2024_s2_clean")

#seperate between same and different ODs
may_2024_s2_diffODs <- filter(may_2024_s2_clean, same_OD_ind == 0)
may_2024_s2_sameODs <- filter(may_2024_s2_clean, same_OD_ind == 1)

#########
#########
## do the same thing with the train dataset (may, 2023 - april, 2024)
mta_s2 <- readRDS(file = "~/Documents/Mobility/Zambia/may_to_april_15_strat2_new.rds")

mta_s2 <- add_location_indicator(mta_s2)
mta_s2 <- make_proportion_column(mta_s2)
mta_s2 <- na.omit(mta_s2)

## drop unnecessary columns
mta_s2_clean <- mta_s2 %>% dplyr::select(- total_pop, - total_pop_per100k)

save(mta_s2_clean, file = "~/Documents/Mobility/Zambia/Cleaning_modeling_code/Saved_objects/mta_s2_clean")
## load in saved dataset
load(file = "~/Documents/Mobility/Zambia/Cleaning_modeling_code/Saved_objects/mta_s2_clean")

#seperate between same and different ODs
mta_s2_diffODs <- filter(mta_s2_clean, same_OD_ind == 0)
mta_s2_sameODs <- filter(mta_s2_clean, same_OD_ind == 1)

## summary stats for manuscript
median(mta_s2_sameODs$grouped.total)
median(mta_s2_diffODs$grouped.total)

mta_s2_diffODs <- mutate(mta_s2_diffODs, has_het = ifelse(percentage_ones == 100, 0, 1))
table(mta_s2_diffODs$has_het)

#### ?
mta_s2_diffODs_with_obs_pmf <- make_dist_preds_only_negbi(data = mta_s2_diffODs, has_het_mod = has_het_mod, opt_threshold_100 = optimal_cutoff$threshold,
                                                         negbi_size_mod = negbi.size.mod, negbi_mu_mod = negbi.mu.mod)
mta_s2_diffODs_with_obs_pmf <- mta_s2_diffODs_with_obs_pmf %>% rowwise() %>% mutate(length_pmf = length(obs_pmf))



#### combine may to april and may 2024 datasets to get full may to may diff ODs dataset
mtm_s2_diffODs <- bind_rows(mta_s2_diffODs, may_2024_s2_diffODs)

## for summary stats in manuscript (what percent of ODs have travel frequency het/of those, what percent of people take only one trip?)
mtm_s2_diffODs <- mtm_s2_diffODs %>%
  mutate(has_het = ifelse(percentage_ones == 100, 0, 1))

table(mtm_s2_diffODs$has_het)
15167/(15167+28032)

mtm_only_het <- filter(mtm_s2_diffODs, has_het == 1)

table(mtm_only_het$percentage_ones)
median(mtm_only_het$percentage_ones)

### max amount of travel between one OD
mtm_s2_diffODs_max_travel <- mtm_s2_diffODs %>% rowwise() %>%
  mutate(max_trav = max(trips_per_person, na.rm = T))
max(mtm_s2_diffODs_max_travel$max_trav)

######################################
######################################
############### Predicting distributions of trips per people for May, 2024 data
######################################
######################################


### get models from Zambia_travel_prediction_models.R

may.2024.negbi.pois.preds <- make_dist_preds(data = may_2024_s2_diffODs, has_het_mod = has_het_mod, opt_threshold_100 = optimal_cutoff$threshold,
                                             opt_threshold_negbi = optimal_cutoff_neg_bi$threshold, negbi_ind_mod = neg_bi_ind_mod,
                                             negbi_size_mod = negbi.size.mod, negbi_mu_mod = negbi.mu.mod,
                                             poisson_rate_mod = poisson.rate.mod)
may.2024.just.negbi.preds <- make_dist_preds_only_negbi(data = may_2024_s2_diffODs, has_het_mod = has_het_mod, opt_threshold_100 = optimal_cutoff$threshold,
                                                        negbi_size_mod = negbi.size.mod, negbi_mu_mod = negbi.mu.mod)
may.2024.just.pois.preds <- make_dist_preds_only_pois(data = may_2024_s2_diffODs, has_het_mod = has_het_mod, opt_threshold_100 = optimal_cutoff$threshold,
                                                      poisson_rate_mod = poisson.rate.mod)

## show the prediction errors for each model
predictions.errors.may.2024 <- tibble(model = c("Negbi + Pois", "Negbi only", "Pois only"),
                                mean_KL = c(mean(0.5),
                                            mean(may.2024.just.negbi.preds$kl.est_dens),
                                            mean(0.5)),
                                mean_wasserstein = c(mean(0.3),
                                                     mean(may.2024.just.negbi.preds$wasserstein_dens),
                                                     mean(0.3)))
predictions.errors.may.2024





######################################
######################################
############### Simulate travel
######################################
######################################




############ simulating travel in the observed 81 districts

### join the clean strategy 2 may 2024 dataset with the population and distance dataset
may_2024_obs_districts <- left_join(may_2024_s2_clean, pop.and.dist, by = c("origin_clean", "destination_clean")) %>%
  dplyr::select(-matches("\\.y$")) %>%  # Remove duplicate columns from may_2024_s2_clean
  rename_with(~ gsub("\\.x$", "", .x))

### seperate by intra and inter-district pairs
may_2024_obs_districts_diffODS <- filter(may_2024_obs_districts, same_OD_ind == 0)
may_2024_obs_districts_sameODS <- filter(may_2024_obs_districts, same_OD_ind == 1)

## run this function to get obs_pmf, we don't care about the predictions right now
may_2024_obs_districts_diffODS_preds <- make_dist_preds_only_negbi(data = may_2024_obs_districts_diffODS, ind_100_mod = ind_100_mod, opt_threshold_100 = optimal_cutoff$threshold,
                                                        negbi_size_mod = negbi.size.mod, negbi_mu_mod = negbi.mu.mod)

may_2024_obs_districts_pmf <- bind_rows(may_2024_obs_districts_sameODS, may_2024_obs_districts_diffODS_preds)

#add in observed multiplier, which is applied to districts with heterogeneity that results in the mean distribution of
#likelihood of travel equal to the observed proportion of origin travel. If no heterogeneity, then simply proportion of origin travel
may_2024_obs_districts_pmf <- may_2024_obs_districts_pmf %>% rowwise() %>%
  mutate(obs_multiplier = (total_trips*prop_of_origin_travel)/grouped.total,
         dist_percent = ifelse(new_OD == 0 & same_OD_ind == 0 & percentage_ones != 100,
                               list(c(1:length(obs_pmf)) * obs_multiplier),
                               list(prop_of_origin_travel)),
         obs_pmf_clean = ifelse(is.null(obs_pmf), list(1), list(obs_pmf)))


# take only the necessary variables
may_2024_obs_districts_clean <- may_2024_obs_districts_pmf %>% dplyr::select(origin_clean, destination_clean, same_OD_ind, new_OD,
                                                                         prop_of_origin_travel, obs_pmf,
                                                                                   obs_multiplier, dist_percent, obs_pmf_clean)

##### create mobility matrix of observed proportion of origin travel
mob_matrix_obs_districts <- get_mob_matrix(orig=may_2024_obs_districts_clean$origin_clean,
                                           dest=may_2024_obs_districts_clean$destination_clean,
                                           value=(may_2024_obs_districts_clean$prop_of_origin_travel))
mob_matrix_obs_districts[is.na(mob_matrix_obs_districts)] = 0

### create a filtered population dataset for the simulation function. Only takes districts in the observed data
districts <- unique(may_2024_obs_districts_clean$origin_clean)
pops.filt <- newpops %>% filter(District %in% districts) %>% arrange(District)

#### simulate travel with return for observed districts
#### now simulate travel with return following James Giles paper's parameters
## parameters from paper
beta_0 <- 0.4   # Maximum decay parameter from the plot
alpha <- 0.0014 # Scaling factor

dist_data_km <- dist_data_full/1000

# Generate the beta matrix using distances
beta_matrix <- compute_beta_ij(dist_data_km, beta_0, alpha)



### simulate travel with return with heterogeneity in observed districts
### first create matrix with poot (either multiple values corresponding to number of trips/month or one value if no heterogeneity)
# Convert to matrix format
old_het_df_dist_percent <- may_2024_obs_districts_clean %>% dplyr::select(origin_clean, destination_clean, dist_percent) %>%
  arrange(origin_clean, destination_clean) %>%
  pivot_wider(names_from = destination_clean, values_from = dist_percent, values_fill = list(0))


old_het_dist_percent_matrix <- as.matrix(old_het_df_dist_percent[, -1])  # Remove origin column for matrix format
rownames(old_het_dist_percent_matrix) <- old_het_df_dist_percent$origin_clean  # Set row names

### now create matrix with probability of each probability of travel (i.e. a 70% chance of having a 0.12% chance of travel from A to B)
# Convert to matrix format
old_het_df_obs_pmf <- may_2024_obs_districts_clean %>% dplyr::select(origin_clean, destination_clean, obs_pmf_clean) %>%
  arrange(origin_clean, destination_clean) %>%
  pivot_wider(names_from = destination_clean, values_from = obs_pmf_clean, values_fill = list(1))

old_het_obs_pmf_matrix <- as.matrix(old_het_df_obs_pmf[, -1])  # Remove origin column for matrix format
rownames(old_het_obs_pmf_matrix) <- old_het_df_obs_pmf$origin_clean  # Set row names

test <- simulate_mobility_het_obs_only(n_people = 200, 100, old_het_dist_percent_matrix, old_het_obs_pmf_matrix, pop_data = pops.filt,
                                                   beta_matrix, chunk_size = 50)

### save objects to transfer to IDD server

saveRDS(pops.filt, "~/Documents/Mobility/Zambia/Cleaning_modeling_code/sim_objects/pops_filt.rds")
saveRDS(beta_matrix, "~/Documents/Mobility/Zambia/Cleaning_modeling_code/sim_objects/beta_matrix.rds")
saveRDS(mob_matrix_obs_districts, "~/Documents/Mobility/Zambia/Cleaning_modeling_code/sim_objects/mob_matrix_obs_districts.rds")
saveRDS(old_het_dist_percent_matrix, "~/Documents/Mobility/Zambia/Cleaning_modeling_code/sim_objects/old_het_dist_percent_matrix.rds")
saveRDS(old_het_obs_pmf_matrix, "~/Documents/Mobility/Zambia/Cleaning_modeling_code/sim_objects/old_het_obs_pmf_matrix.rds")


pops.filt <- readRDS("~/Documents/Mobility/Zambia/Cleaning_modeling_code/sim_objects/pops_filt.rds")

#######################
#######################
########## examples of how to run simulations (run on server)
#######################
#######################

########
##### simulate 10 dfs using aggregated OD (with return travel)
########
set.seed(3546)
n_people <- 20000
n_timesteps <- 100
# Define folder path
output_folder <- "~/Documents/Mobility/Zambia/Cleaning_modeling_code/Simulated_mobility_datasets/"
n_simulations = 10


# Run and save each simulation
for (i in 1:n_simulations) {
  df <- simulate_mobility_return(200, n_timesteps, mob_matrix_obs_districts, pops.filt, beta_matrix, chunk_size = 50)
  df$simulation <- i  # Add a simulation ID
  file_name <- sprintf("%s/sim_return_%d.csv", output_folder, i)
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
output_folder <- "~/Documents/Mobility/Zambia/Cleaning_modeling_code/Simulated_mobility_datasets/"
n_simulations = 10

# Run and save each simulation
for (i in 1:n_simulations) {
  df <- simulate_mobility_het_obs(n_people, n_timesteps, old_het_dist_percent_matrix, old_het_obs_pmf_matrix, pop_data = pops.filt,
                                  beta_matrix, chunk_size = 50)
  df$simulation <- i  # Add a simulation ID
  file_name <- sprintf("%s/sim_return_%d.csv", output_folder, i)
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
output_folder <- "~/Documents/Mobility/Zambia/Cleaning_modeling_code/Simulated_mobility_datasets/"
n_simulations = 10

# Run and save each simulation
for (i in 1:n_simulations) {
  df <- simulate_mobility_return_prob_moving(n_people, n_timesteps, mob_matrix_obs_districts, pops.filt, beta_matrix, chunk_size = 50)
  df$simulation <- i  # Add a simulation ID
  file_name <- sprintf("%s/sim_return_%d.csv", output_folder, i)
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
output_folder <- "~/Documents/Mobility/Zambia/Cleaning_modeling_code/Simulated_mobility_datasets/"
n_simulations = 10

# Run and save each simulation
for (i in 1:n_simulations) {
  df <-  simulate_mobility_het_obs_only_prob_move(n_people = 20000, n_timesteps, old_het_dist_percent_matrix, old_het_obs_pmf_matrix, pop_data = pops.filt,
                                                  beta_matrix, b, chunk_size = 50)
  df$simulation <- i  # Add a simulation ID
  file_name <- sprintf("%s/sim_return_%d.csv", output_folder, i)
  fwrite(df, file_name)  # Save as CSV
  print(i)
}





###################
###################
######## making an OD matrix based off the predictd heterogeneity. Same process as above, but with predicted pmf instead

may_2024_obs_districts_pred_pmf <- may_2024_obs_districts_pmf %>%
  rowwise() %>%
  mutate(pred_pmf = ifelse(is.100.pred == 0, list(get_nb_pmf(pred.neg.bi.size, pred.neg.bi.mu)), list(1)),
         pred_pmf_clean = ifelse(is.null(pred_pmf), list(1), list(pred_pmf)),
         pred_dist_percent = ifelse(new_OD == 0 & same_OD_ind == 0 & is.100.pred == 0,
                               list(c(1:length(pred_pmf_clean)) * obs_multiplier),
                               list(prop_of_origin_travel))) %>%
  ungroup()

may_2024_obs_districts_pred_pmf_clean <- may_2024_obs_districts_pred_pmf %>%
  dplyr::select(origin_clean, destination_clean, same_OD_ind, new_OD,
                prop_of_origin_travel, obs_pmf, obs_multiplier, pred_dist_percent, obs_pmf_clean,
                pred_pmf_clean)


##### predicted densities of travel het
pred_old_het_df_dist_percent <- may_2024_obs_districts_pred_pmf_clean %>% dplyr::select(origin_clean, destination_clean, pred_dist_percent) %>%
  arrange(origin_clean, destination_clean) %>%
  pivot_wider(names_from = destination_clean, values_from = pred_dist_percent, values_fill = list(0))


pred_old_het_dist_percent_matrix <- as.matrix(pred_old_het_df_dist_percent[, -1])  # Remove origin column for matrix format
rownames(pred_old_het_dist_percent_matrix) <- pred_old_het_df_dist_percent$origin_clean  # Set row names

### now create matrix with probability of each probability of travel (i.e. a 70% chance of having a 0.12% chance of travel from A to B)
# Convert to matrix format
pred_old_het_df_obs_pmf <- may_2024_obs_districts_pred_pmf_clean %>% dplyr::select(origin_clean, destination_clean, pred_pmf_clean) %>%
  arrange(origin_clean, destination_clean) %>%
  pivot_wider(names_from = destination_clean, values_from = pred_pmf_clean, values_fill = list(1))

pred_old_het_obs_pmf_matrix <- as.matrix(pred_old_het_df_obs_pmf[, -1])  # Remove origin column for matrix format
rownames(pred_old_het_obs_pmf_matrix) <- pred_old_het_df_obs_pmf$origin_clean  # Set row names

test <- simulate_mobility_het_obs_only(n_people = 200, 100, pred_old_het_dist_percent_matrix, pred_old_het_obs_pmf_matrix, pop_data = pops.filt,
                                  beta_matrix, chunk_size = 50)


### save objects for use in server
saveRDS(pred_old_het_dist_percent_matrix, "~/Documents/Mobility/Zambia/Cleaning_modeling_code/sim_objects/pred_old_het_dist_percent_matrix.rds")
saveRDS(pred_old_het_obs_pmf_matrix, "~/Documents/Mobility/Zambia/Cleaning_modeling_code/sim_objects/pred_old_het_obs_pmf_matrix.rds")







#######################
#######################
########## simulation results
#######################
#######################

#########
#########
##### Flu
#########
#########

###load in all the pkl file simulation results for flu
actual_lusaka_Flu <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Actual/Actual_Flu_Lusaka_sims.pkl")
actual_luampa_Flu <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Actual/Actual_Flu_Luampa_sims.pkl")
actual_kalulushi_Flu <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Actual/Actual_Flu_Kalulushi_sims.pkl")

agg_lusaka_Flu <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Aggregate/Aggregate_Flu_Lusaka_sims.pkl")
agg_luampa_Flu <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Aggregate/Aggregate_Flu_Luampa_sims.pkl")
agg_kalulushi_Flu <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Aggregate/Aggregate_Flu_Kalulushi_sims.pkl")

mh_lusaka_Flu <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Move_het/Move_het_Flu_Lusaka_sims.pkl")
mh_luampa_Flu <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Move_het/Move_het_Flu_Luampa_sims.pkl")
mh_kalulushi_Flu <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Move_het/Move_het_Flu_Kalulushi_sims.pkl")

th_lusaka_Flu <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Travel_het/Travel_het_Flu_Lusaka_sims.pkl")
th_luampa_Flu <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Travel_het/Travel_het_Flu_Luampa_sims.pkl")
th_kalulushi_Flu <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Travel_het/Travel_het_Flu_Kalulushi_sims.pkl")

bh_lusaka_Flu <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Both_het/Both_het_Flu_Lusaka_sims.pkl")
bh_luampa_Flu <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Both_het/Both_het_Flu_Luampa_sims.pkl")
bh_kalulushi_Flu <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Both_het/Both_het_Flu_Kalulushi_sims.pkl")

### put all the flu sims together
flu_all_sims <- list(
  Actual = list(Urban = actual_lusaka_Flu, Rural = actual_luampa_Flu, "Semi-urban" = actual_kalulushi_Flu),
  Aggregate = list(Urban = agg_lusaka_Flu, Rural = agg_luampa_Flu, "Semi-urban" = agg_kalulushi_Flu),
  Travel_het = list(Urban = th_lusaka_Flu, Rural = th_luampa_Flu, "Semi-urban" = th_kalulushi_Flu),
  Move_het = list(Urban = mh_lusaka_Flu, Rural = mh_luampa_Flu, "Semi-urban" = mh_kalulushi_Flu),
  Both_het = list(Urban = bh_lusaka_Flu, Rural = bh_luampa_Flu, "Semi-urban" = bh_kalulushi_Flu)
)

###create dataframe for plotting flu simulation results
flu_infections_df <- make_sim_results_df_for_plot(flu_all_sims)

### create table for comparing simulation descriptive results
flu_sims_table <- compare_table_func(folder_name = "~/Documents/Mobility/Zambia/Starsim_results",
                                  c("Actual/Actual_Flu_Lusaka",
                                    "Actual/Actual_Flu_Kalulushi",
                                    "Actual/Actual_Flu_Luampa",
                                    "Aggregate/Aggregate_Flu_Lusaka",
                                    "Aggregate/Aggregate_Flu_Kalulushi",
                                    "Aggregate/Aggregate_Flu_Luampa",
                                    "Move_het/Move_het_Flu_Lusaka",
                                    "Move_het/Move_het_Flu_Kalulushi",
                                    "Move_het/Move_het_Flu_Luampa",
                                    "Travel_het/Travel_het_Flu_Lusaka",
                                    "Travel_het/Travel_het_Flu_Kalulushi",
                                    "Travel_het/Travel_het_Flu_Luampa",
                                    "Both_het/Both_het_Flu_Lusaka",
                                    "Both_het/Both_het_Flu_Kalulushi",
                                    "Both_het/Both_het_Flu_Luampa"),
                                  pops.filt, c(rep(299, 15)))


#########
#########
##### COVID
#########
#########

###load in all the pkl file simulation results for COVID
actual_lusaka_COVID <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Actual/Actual_COVID_Lusaka_sims.pkl")
actual_luampa_COVID <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Actual/Actual_COVID_Luampa_sims.pkl")
actual_kalulushi_COVID <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Actual/Actual_COVID_Kalulushi_sims.pkl")

agg_lusaka_COVID <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Aggregate/Aggregate_COVID_Lusaka_sims.pkl")
agg_luampa_COVID <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Aggregate/Aggregate_COVID_Luampa_sims.pkl")
agg_kalulushi_COVID <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Aggregate/Aggregate_COVID_Kalulushi_sims.pkl")

mh_lusaka_COVID <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Move_het/Move_het_COVID_Lusaka_sims.pkl")
mh_luampa_COVID <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Move_het/Move_het_COVID_Luampa_sims.pkl")
mh_kalulushi_COVID <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Move_het/Move_het_COVID_Kalulushi_sims.pkl")

th_lusaka_COVID <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Travel_het/Travel_het_COVID_Lusaka_sims.pkl")
th_luampa_COVID <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Travel_het/Travel_het_COVID_Luampa_sims.pkl")
th_kalulushi_COVID <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Travel_het/Travel_het_COVID_Kalulushi_sims.pkl")

bh_lusaka_COVID <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Both_het/Both_het_COVID_Lusaka_sims.pkl")
bh_luampa_COVID <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Both_het/Both_het_COVID_Luampa_sims.pkl")
bh_kalulushi_COVID <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Both_het/Both_het_COVID_Kalulushi_sims.pkl")

### put all the COVID sims together
covid_all_sims <- list(
  Actual = list(Urban = actual_lusaka_COVID, Rural = actual_luampa_COVID, "Semi-urban" = actual_kalulushi_COVID),
  Aggregate = list(Urban = agg_lusaka_COVID, Rural = agg_luampa_COVID, "Semi-urban" = agg_kalulushi_COVID),
  Travel_het = list(Urban = th_lusaka_COVID, Rural = th_luampa_COVID, "Semi-urban" = th_kalulushi_COVID),
  Move_het = list(Urban = mh_lusaka_COVID, Rural = mh_luampa_COVID, "Semi-urban" = mh_kalulushi_COVID),
  Both_het = list(Urban = bh_lusaka_COVID, Rural = bh_luampa_COVID, "Semi-urban" = bh_kalulushi_COVID)
)

###create dataframe for plotting COVID simulation results
covid_infections_df <- make_sim_results_df_for_plot(covid_all_sims)

### create table for comparing simulation descriptive results

COVID_sims_table <- compare_table_func(folder_name = "~/Documents/Mobility/Zambia/Starsim_results",
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





#########
#########
##### measles
#########
#########

###load in all the pkl file simulation results for measles
actual_lusaka_Measles <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Actual/Actual_Measles_Lusaka_sims.pkl")
actual_luampa_Measles <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Actual/Actual_Measles_Luampa_sims.pkl")
actual_kalulushi_Measles <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Actual/Actual_Measles_Kalulushi_sims.pkl")

agg_lusaka_Measles <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Aggregate/Aggregate_Measles_Lusaka_sims.pkl")
agg_luampa_Measles <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Aggregate/Aggregate_Measles_Luampa_sims.pkl")
agg_kalulushi_Measles <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Aggregate/Aggregate_Measles_Kalulushi_sims.pkl")

mh_lusaka_Measles <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Move_het/Move_het_Measles_Lusaka_sims.pkl")
mh_luampa_Measles <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Move_het/Move_het_Measles_Luampa_sims.pkl")
mh_kalulushi_Measles <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Move_het/Move_het_Measles_Kalulushi_sims.pkl")

th_lusaka_Measles <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Travel_het/Travel_het_Measles_Lusaka_sims.pkl")
th_luampa_Measles <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Travel_het/Travel_het_Measles_Luampa_sims.pkl")
th_kalulushi_Measles <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Travel_het/Travel_het_Measles_Kalulushi_sims.pkl")

bh_lusaka_Measles <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Both_het/Both_het_Measles_Lusaka_sims.pkl")
bh_luampa_Measles <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Both_het/Both_het_Measles_Luampa_sims.pkl")
bh_kalulushi_Measles <- py_load_object("~/Documents/Mobility/Zambia/Starsim_results/Both_het/Both_het_Measles_Kalulushi_sims.pkl")

### put all the measles sims together
measles_all_sims <- list(
  Actual = list(Urban = actual_lusaka_Measles, Rural = actual_luampa_Measles, "Semi-urban" = actual_kalulushi_Measles),
  Aggregate = list(Urban = agg_lusaka_Measles, Rural = agg_luampa_Measles, "Semi-urban" = agg_kalulushi_Measles),
  Travel_het = list(Urban = th_lusaka_Measles, Rural = th_luampa_Measles, "Semi-urban" = th_kalulushi_Measles),
  Move_het = list(Urban = mh_lusaka_Measles, Rural = mh_luampa_Measles, "Semi-urban" = mh_kalulushi_Measles),
  Both_het = list(Urban = bh_lusaka_Measles, Rural = bh_luampa_Measles, "Semi-urban" = bh_kalulushi_Measles)
)

###create dataframe for plotting meales simulation results
measles_infections_df <- make_sim_results_df_for_plot(measles_all_sims)

### create table for comparing simulation descriptive results
measles_sims_table <- compare_table_func(folder_name = "~/Documents/Mobility/Zambia/Starsim_results",
                                       c("Actual/Actual_Measles_Lusaka",
                                         "Actual/Actual_Measles_Kalulushi",
                                         "Actual/Actual_Measles_Luampa",
                                         "Aggregate/Aggregate_Measles_Lusaka",
                                         "Aggregate/Aggregate_Measles_Kalulushi",
                                         "Aggregate/Aggregate_Measles_Luampa",
                                         "Move_het/Move_het_Measles_Lusaka",
                                         "Move_het/Move_het_Measles_Kalulushi",
                                         "Move_het/Move_het_Measles_Luampa",
                                         "Travel_het/Travel_het_Measles_Lusaka",
                                         "Travel_het/Travel_het_Measles_Kalulushi",
                                         "Travel_het/Travel_het_Measles_Luampa",
                                         "Both_het/Both_het_Measles_Lusaka",
                                         "Both_het/Both_het_Measles_Kalulushi",
                                         "Both_het/Both_het_Measles_Luampa"),
                                       pops.filt, c(rep(100, 15)))





##############
##############
#### Cuebiq data summary
##############
##############


movement_het_monthly_all_trips_county <- open_dataset("~/Documents/Mobility/Cuebiq_datasets/MOVE_HET_MONTH_ALLTRIPS_COUNTY_EXPORT_CLEAN") %>%
  collect()
colnames(movement_het_monthly_all_trips_county) <- c("start_date", "end_date", "census_tract", "perc_non_movers", "total_people")

movement_het_monthly_all_trips_county <- movement_het_monthly_all_trips_county %>%
  mutate(total_people_clean = ifelse(total_people == "<10", 1, as.numeric(total_people)))

grouped_month_tot_people <- movement_het_monthly_all_trips_county %>% group_by(start_date, end_date) %>%
  summarize(tot_people = sum(total_people_clean))

mean(grouped_month_tot_people$tot_people)



####
## bring in OD monthly trips by counties in 2024
OD_ct <- open_dataset("~/Documents/Mobility/Cuebiq_datasets/OD_SELECT_STATES_CENSUS_TRACT_EXPORT_CLEAN") %>%
  collect()

### take only the useful columns and name them
colnames(OD_ct) <- c("start_ct", "end_ct", "total_trips / 180 days", "daily_trips")

## set as data.table for faster processing
setDT(OD_ct)

# Split the census_tract columns by "." into 4 new columns for each part of the census tract code
OD_ct[, c("start_country", "start_state", "start_county", "start_ct_part") := tstrsplit(start_ct, ".", fixed = TRUE)]
OD_ct[, c("end_country", "end_state", "end_county", "end_ct_part") := tstrsplit(end_ct, ".", fixed = TRUE)]

## create a start and end county variable
OD_ct <- OD_ct %>% mutate(start_county_full = paste0(start_state, ".", start_county),
                          end_county_full = paste0(end_state, ".", end_county))

### indicator for same county and same ct
OD_ct$same_county_ind <- ifelse(OD_ct$start_county_full == OD_ct$end_county_full, 1, 0)
OD_ct$same_ct_ind <- ifelse(OD_ct$start_ct == OD_ct$end_ct, 1, 0)


### group by ct
grouped_ct_ind <- OD_ct %>% mutate(total_trips_clean = as.numeric(`total_trips / 180 days`)) %>%
  group_by(same_ct_ind) %>% summarize(avg_total_trips = (sum(total_trips_clean) / n()),
                                                                avg_monthly_trips = avg_total_trips/6)

### group by county
grouped_county_ind <- OD_ct %>% mutate(total_trips_clean = as.numeric(`total_trips / 180 days`)) %>%
  group_by(start_county_full, end_county_full) %>% summarize(total_county_trips = sum(total_trips_clean)) %>%
  mutate(same_county_ind = ifelse(start_county_full == end_county_full, 1, 0)) %>%
  group_by(same_county_ind) %>% summarize(avg_total_trips = (sum(total_county_trips) / n()),
                                      avg_monthly_trips = avg_total_trips/6)


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





#########
########

users_per_loc_month <- readRDS(file = "/Users/nparke19/Documents/Mobility/Zambia/users_per_loc_per_months.rds")

users_per_loc_avg <- users_per_loc_month %>% filter(sub_region != "") %>%
  mutate(
    district = recode(sub_region,
                      KAPIRI = "KAPIRI MPOSHI",
                      SHANGOMBO = "SHANG'OMBO",
                      MILENGE = "MILENGI",
                      "ITHEZI-TEZHI" = "ITEZHI-TEZHI",
                      MALOLE = "MUNGWI",
                      MUYOMBE = "ISOKA")) %>%
  group_by(district) %>%
  summarize(avg_monthly_users = mean(unique_users, na.rm = T)) %>%
  mutate(log_avg_monthly_users = log10(avg_monthly_users))


### read in the shape file for Zamiba
shapefile_path <- "~/Documents/Mobility/Zambia/Cleaning_modeling_code/Zambia_shape_files/Zambia_-_Administrative_District_Boundaries_2022.shp"
zambia_districts <- st_read(shapefile_path)

# Ensure district names are consistent with names in our CDR data
zambia_districts <- zambia_districts %>%
  mutate(DISTRICT = toupper(DISTRICT)) %>%
  mutate(
    DISTRICT = recode(DISTRICT,
                      KAPIRI = "KAPIRI MPOSHI",
                      SHANGOMBO = "SHANG'OMBO",
                      MILENGE = "MILENGI",
                      "ITHEZI-TEZHI" = "ITEZHI-TEZHI",
                      MALOLE = "MUNGWI",
                      MUYOMBE = "ISOKA"))


# merge with move het to get perc_mov
zambia_monthly_users <- merge(zambia_districts, users_per_loc_avg, by.x = "DISTRICT", by.y = "district", all.x = T)


# plot map of included districts
quartz()
zambia_monthly_users_map <- ggplot(data = zambia_monthly_users) +
  geom_sf(aes(fill = log_avg_monthly_users), color = "white", size = 0.1) +
  scale_fill_viridis_c(direction = 1, na.value = "gray90", name = "Average monthly\nusers (log)", limits = c(0, 6),
                       breaks = c(0, 2, 4, 6),
                       labels = c(0, 2, 4, 6)) +
  theme_minimal(base_size = 10) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 8, vjust = 1, hjust = 0.5),
        legend.text = element_text(size = 8),
        legend.justification = "center",
        legend.box.margin = margin(t = 20),
        plot.margin = margin(t = 0)
      )
zambia_monthly_users_map




########################zambia_move_het_group_loc
##### movement het
############

zambia_move_het_ind_month <- readRDS(file = "/Users/nparke19/Library/CloudStorage/OneDrive-JohnsHopkins/Zambia/Cleaning_modeling_code/home_location_by_month_new.rds")

table(zambia_move_het_ind_month$num_loc)

perc_move <- sum(zambia_move_het_ind_month$num_loc == 1)/nrow(zambia_move_het_ind_month)


zambia_move_het_group_month <- zambia_move_het_ind_month %>% group_by(month) %>% summarize(perc_move = sum(num_loc > 1) / n(),
                                                                                         n = n())

mean_estimate_month <- mean(zambia_move_het_group_month$perc_move)
n_month = nrow(zambia_move_het_group_month)

ggplot(zambia_move_het_group_month, aes(x = perc_move)) +
  geom_histogram()

sd_estimate_month <- sd(zambia_move_het_group_month$perc_move, na.rm = TRUE)
se_month = sd_estimate_month / sqrt(n_month)
ci_lower_month = mean_estimate_month - qt(0.975, df = n_month - 1) * se_month
ci_upper_month = mean_estimate_month + qt(0.975, df = n_month - 1) * se_month


##### week level
zambia_move_het_ind_week <- readRDS(file = "~/Documents/Mobility/Manuscript_code_objects/Raw_data_and_dems/Zambia/home_location_by_week.rds")

table(zambia_move_het_ind_week$num_loc)

perc_move_week <- sum(zambia_move_het_ind_week$num_loc > 1)/nrow(zambia_move_het_ind_week)
head(zambia_move_het_ind_week, 100)


zambia_move_het_group_week <- zambia_move_het_ind_week %>% group_by(week_group) %>% summarize(perc_move = sum(num_loc > 1) / n(),
                                                                                              n = n())

mean_estimate_week <- mean(zambia_move_het_group_week$perc_move)
n_week = nrow(zambia_move_het_group_week)

ggplot(zambia_move_het_group_week, aes(x = perc_move)) +
  geom_histogram()

sd_estimate_week <- sd(zambia_move_het_group_week$perc_move, na.rm = TRUE)
se_week = sd_estimate_week / sqrt(n_week)
ci_lower_week = mean_estimate_week - qt(0.975, df = n_week - 1) * se_week
ci_upper_week = mean_estimate_week + qt(0.975, df = n_week - 1) * se_week


#### 3 day level
zambia_move_het_ind_3day <- readRDS(file = "~/Documents/Mobility/Manuscript_code_objects/Raw_data_and_dems/Zambia/home_location_by_3day.rds")

table(zambia_move_het_ind_3day$num_loc)

perc_move_3day <- sum(zambia_move_het_ind_3day$num_loc > 1)/nrow(zambia_move_het_ind_3day)
head(zambia_move_het_ind_3day, 100)
sd_estimate <- sd(perc_move, na.rm = TRUE)
se = sd_estimate / sqrt(n)
ci_lower = mean_estimate - qt(0.975, df = n - 1) * se
ci_upper = mean_estimate + qt(0.975, df = n - 1) * se

zambia_move_het_group_3day <- zambia_move_het_ind_3day %>% group_by(three_day_label) %>% summarize(perc_move = sum(num_loc > 1) / n(),
                                                                               n = n())

mean_estimate_3day <- mean(zambia_move_het_group_3day$perc_move)
n_3day = nrow(zambia_move_het_group_3day)

ggplot(zambia_move_het_group_3day, aes(x = perc_move)) +
  geom_histogram()

sd_estimate_3day <- sd(zambia_move_het_group_3day$perc_move, na.rm = TRUE)
se_3day = sd_estimate_3day / sqrt(n_3day)
ci_lower_3day = mean_estimate_3day - qt(0.975, df = n_3day - 1) * se_3day
ci_upper_3day = mean_estimate_3day + qt(0.975, df = n_3day - 1) * se_3day



#######combine into dataset
zambia_move_het_summary <- tibble(
  mean = c(mean_estimate_3day, mean_estimate_week, mean_estimate_month) * 100,
  "2.5%" = c(ci_lower_3day, ci_lower_week, ci_lower_month) * 100,
  "97.5%" = c(ci_upper_3day, ci_upper_week, ci_upper_month) * 100,
  admin = c(rep("Zambia districts", 3)),
  time_agg = c("3 Days", "Week", "Month"),
  max_loc = c(rep("24hr", 3))
)

### combine with cuebiq move het summary
zambia_us_move_het_summary <- bind_rows(move_het_summary_movers, zambia_move_het_summary)

### factor graphing variables and plot
zambia_us_move_het_summary$time_agg <- factor(zambia_us_move_het_summary$time_agg, levels = c("Day", "3 Days", "Week", "Month"))
zambia_us_move_het_summary$max_loc <- factor(zambia_us_move_het_summary$max_loc, levels = c( "All trips", "8hr", "24hr"))
zambia_us_move_het_summary$admin <- factor(zambia_us_move_het_summary$admin, levels = c("Zambia districts", "County", "Census tract"))

move_het_summary_movers_plot <- ggplot(zambia_us_move_het_summary,
                                       aes(x = time_agg, y = mean, color = admin, group = admin)) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5), size = 4) +
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`),
                width = 0.1,
                position = position_dodge(width = 0.5)) +
  facet_grid(cols = vars(max_loc), scales = "free") +
  scale_color_manual(values = c('County' = "#8400CD",
                                'Census tract' = "#008DF9",
                                'Zambia districts' = "#FF6E3A"),
                     labels = c('Zambian districts', "US county", "US census tract")) +
  labs(x = "", y = "Percent of people who move", color = "Spatial Aggregation") +
  theme(axis.title.x = element_blank()) +
  theme_minimal(base_size = 14)
move_het_summary_movers_plot


move_het_summary_movers_plot_filt <- ggplot(filter(zambia_us_move_het_summary, max_loc == "24hr"),
                                       aes(x = time_agg, y = mean, color = admin, group = admin)) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5), size = 4) +
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`),
                width = 0.1,
                position = position_dodge(width = 0.5)) +
  scale_y_continuous(limits = c(0, 60), breaks = c(20,40,60)) +
  scale_color_manual(values = c('County' = "#8400CD",
                                'Census tract' = "#008DF9",
                                'Zambia districts' = "#FF6E3A"),
                     labels = c('Zambian districts', "US county", "US census tract")) +
  labs(x = "", y = "Percent of people who move", color = "Spatial Aggregation") +
  theme(axis.title.x = element_blank()) +
  theme_minimal(base_size = 14)
move_het_summary_movers_plot_filt

ggsave(move_het_summary_movers_plot_filt, file = "~/Desktop/move_het_all.png",
       width = 6, height = 3)


#############
##### travel het summary
#############

#### combine may to april and may 2024 datasets to get full may to may diff ODs dataset
may_2024_s2_diffODs$month <- "May, 2024"
mtm_s2_diffODs <- bind_rows(mta_s2_diffODs, may_2024_s2_diffODs)

## for summary stats in manuscript (what percent of ODs have travel frequency het/of those, what percent of people take only one trip?)
mtm_s2_diffODs <- mtm_s2_diffODs %>%
  mutate(has_het = ifelse(percentage_ones == 100, 0, 1))

table(mtm_s2_diffODs$has_het)
15167/(15167+28032)

zambia_travel_het_group_month <- mtm_s2_diffODs %>% group_by(month) %>% summarize(perc_has_het = sum(has_het == 1) / n(),
                                                                                              n = n())

mean_estimate_th_month <- mean(zambia_travel_het_group_month$perc_has_het)
n_th_month = nrow(zambia_travel_het_group_month)

ggplot(zambia_travel_het_group_month, aes(x = perc_move)) +
  geom_histogram()

sd_estimate_th_month <- sd(zambia_travel_het_group_month$perc_has_het, na.rm = TRUE)
se_th_month = sd_estimate_th_month / sqrt(n_th_month)
ci_lower_th_month = mean_estimate_th_month - qt(0.975, df = n_th_month - 1) * se_th_month
ci_upper_th_month = mean_estimate_th_month + qt(0.975, df = n_th_month - 1) * se_th_month


##### week level
mta_week <- readRDS(file = "~/Documents/Mobility/Manuscript_code_objects/Raw_data_and_dems/Zambia/may_to_april_15_strat2_by_week.rds")
mta_week <- na.omit(mta_week)

may_2024_week <- readRDS(file = "~/Documents/Mobility/Manuscript_code_objects/Raw_data_and_dems/Zambia/may_2024_15_strat2_by_week.rds")
may_2024_week <- na.omit(may_2024_week)

mta_week_diffODs <- filter(mta_week, origin_clean != destination_clean)
may_2024_week_diffODs <- filter(may_2024_week, origin_clean != destination_clean)

mtm_week_diffODs <- rbind(mta_week_diffODs, may_2024_week_diffODs)

mtm_week_diffODs <- mtm_week_diffODs %>%
  mutate(has_het = ifelse(percentage_ones == 100, 0, 1))

table(mtm_week_diffODs$has_het)
25252/(25252+111255)

zambia_travel_het_group_week <- mtm_week_diffODs %>% group_by(week_group) %>% summarize(perc_has_het = sum(has_het == 1) / n(),
                                                                                  n = n())

mean_estimate_th_week <- mean(zambia_travel_het_group_week$perc_has_het)
n_th_week = nrow(zambia_travel_het_group_week)

ggplot(zambia_travel_het_group_week, aes(x = perc_move)) +
  geom_histogram()

sd_estimate_th_week<- sd(zambia_travel_het_group_week$perc_has_het, na.rm = TRUE)
se_th_week = sd_estimate_th_week / sqrt(n_th_week)
ci_lower_th_week = mean_estimate_th_week - qt(0.975, df = n_th_week - 1) * se_th_week
ci_upper_th_week = mean_estimate_th_week + qt(0.975, df = n_th_week - 1) * se_th_week



#######combine into dataset
zambia_travel_het_summary <- tibble(
  mean = c(mean_estimate_th_week, mean_estimate_th_month) * 100,
  "2.5%" = c(ci_lower_th_week, ci_lower_th_month) * 100,
  "97.5%" = c(ci_upper_th_week, ci_upper_th_month) * 100,
  admin = c(rep("Zambia districts", 2)),
  time_agg = c("Week", "Month"),
  max_loc = c(rep("24hr", 2))
)

### combine with cuebiq move het summary
zambia_us_travel_het_summary <- bind_rows(travel_het_summary_clean, zambia_travel_het_summary)

### factor travel het plotting variables and plot
zambia_us_travel_het_summary$time_agg <- factor(zambia_us_travel_het_summary$time_agg, levels = c("Day", "3 Days", "Week", "Month"))
zambia_us_travel_het_summary$max_loc <- factor(zambia_us_travel_het_summary$max_loc, levels = c("All trips", "8hr", "24hr"))
zambia_us_travel_het_summary$admin <- factor(zambia_us_travel_het_summary$admin, levels = c("Zambia districts", "County", "Census tract"))

travel_het_summary_clean_plot <- ggplot(zambia_us_travel_het_summary, aes(x = time_agg, y = mean, color = admin, group = admin)) +
  geom_line(position = position_dodge(width = 0.5)) +
  geom_point(position = position_dodge(width = 0.5), size = 4) +
  geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`),
                width = 0.1,
                position = position_dodge(width = 0.5)) +
  facet_grid(cols = vars(max_loc), scales = "free") +
  scale_color_manual(values = c('County' = "#8400CD",
                                'Census tract' = "#008DF9",
                                'Zambia districts' = "#FF6E3A"),
                     labels = c('Zambian districts', "US county", "US census tract")) +
  labs(x = "Temporal Aggregation", y = "Percent of OD pairs \n with travel heterogeneity", color = "Spatial Aggregation") +
  theme_minimal(base_size = 14)
travel_het_summary_clean_plot

ggsave(travel_het_summary_clean_plot, file = "~/Desktop/travel_het_summary_clean_plot.png",
       width = 8, height = 4)

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








library(readxl)
zambia_district_density <- readxl::read_excel("~/Downloads/zambia_density-converted.xlsx")
zambia_district_density <- zambia_district_density[, c(1,5,6,7)]
colnames(zambia_district_density) <- c("District", "area_km2", "population", "density_km2")
zambia_district_density$District <- toupper(zambia_district_density$District)



zambia_move_het_group_loc_month <- zambia_move_het_ind %>% group_by(month, home_loc) %>% summarize(perc_move = sum(num_loc > 1) / n(),
                                                                               n = n())

zambia_move_het_group_density <- merge(zambia_move_het_group_loc_month, zambia_district_density, by.x = "home_loc",
                                       by.y = "District")
zambia_move_het_group_density$coverage <- zambia_move_het_group_density$n / zambia_move_het_group_density$population
zambia_move_het_group_density$density_group <- cut(zambia_move_het_group_density$density_km2, c(0, 10, 25, 100, Inf),
                                                   c("0-10", "11-25", "26-100", "101+"), include.lowest = T)
table(zambia_move_het_group_density$density_group, zambia_move_het_group_density$density_km2)


density_group_summary <- zambia_move_het_group_density %>% group_by(density_group) %>%
  summarize(mean_perc_move = mean(perc_move),
            median_perc_move = median(perc_move))

zambia_move_het_group_density_prov <- merge(zambia_move_het_group_density, district_province_mapping,
                                            by.x = "home_loc", by.y = "sub_region_clean")


zambia_mh_glm <- glm(perc_move ~ scale(area_km2) + scale(density_km2) + factor(province), data = zambia_move_het_group_density_prov, family = "binomial")
summary.glm(zambia_mh_glm)

zam_mh_results_table <- data.frame(var = c("Land area (square km)", "Population density\n(square km)"),
                                   estimate = c(0.128340, -0.005166),
                                   se = c(0.084810, 0.072515),
                                   model = c("Zambia", "Zambia"))
zam_mh_results_table$conf.low <- zam_mh_results_table$estimate - 1.96*zam_mh_results_table$se
zam_mh_results_table$conf.high <- zam_mh_results_table$estimate + 1.96*zam_mh_results_table$se

move_het_month_daily_results_combined_with_zam <- bind_rows(zam_mh_results_table, move_het_month_daily_results_combined)

move_het_month_daily_results_combined_with_zam$model <- factor(
  move_het_month_daily_results_combined_with_zam$model,
  levels = c("Month_daily_ct", "Month_daily_county", "Zambia")
)

dotplot_move_het_month_daily <- ggplot(data = move_het_month_daily_results_combined_with_zam,
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
                      breaks=c('Month_daily_county', 'Month_daily_ct', 'Zambia'),
                      labels = c("US county", "US census tract", 'Zambian districts')) +
  ylab("Variable") +
  xlab("Estimate")
dotplot_move_het_month_daily

### read in the shape file for Zamiba
shapefile_path <- "~/Documents/Mobility/Zambia/Cleaning_modeling_code/Zambia_shape_files/Zambia_-_Administrative_District_Boundaries_2022.shp"
zambia_districts <- st_read(shapefile_path)

# Ensure district names are consistent with names in our CDR data
zambia_districts <- zambia_districts %>%
  mutate(DISTRICT = toupper(DISTRICT)) %>%
  mutate(
    DISTRICT = recode(DISTRICT,
                      KAPIRI = "KAPIRI MPOSHI",
                      SHANGOMBO = "SHANG'OMBO",
                      MILENGE = "MILENGI",
                      "ITHEZI-TEZHI" = "ITEZHI-TEZHI",
                      MALOLE = "MUNGWI",
                      MUYOMBE = "ISOKA"))


zambia_move_het_group_loc <- zambia_move_het_ind %>% group_by(home_loc) %>%
  summarize(perc_move = sum(num_loc > 1) / n(), n = n()) %>% mutate(perc_move_clean = perc_move * 100)

# merge with move het to get perc_mov
zambia_districts_mh <- merge(zambia_districts, zambia_move_het_group_loc, by.x = "DISTRICT", by.y = "home_loc", all.x = T)


# plot map of included districts
quartz()
zambia_mh_map <- ggplot(data = zambia_districts_mh) +
  geom_sf(aes(fill = perc_move_clean), color = "white", size = 0.1) +
  scale_fill_viridis_c(direction = 1, na.value = "gray90", name = "% movers") +
  theme_minimal(base_size = 10) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 8))
zambia_mh_map



library(patchwork)

mh_map_layout <- "
12
33"

quartz()
mh_map <- free(move_het_map_plot) + free(zambia_mh_map) + dotplot_move_het_month_daily +
  plot_layout(design = mh_map_layout, widths = c(1, 1),
              heights = c(1.5,1)) +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = "bold"))
mh_map





