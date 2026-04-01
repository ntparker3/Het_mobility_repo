#### all necessary functions
library(fitdistrplus)

#add correct population
add_correct_pops <- function(data){
  new_pops <- readxl::read_excel("~/Documents/Mobility/Zambia/Zambia_Population_2022.xlsx")

  new_pops$District <- toupper(new_pops$District)

  new_pops <- new_pops %>%
    mutate(
      District = recode(District,
                        CHIENGE = "CHIENGI",
                        "LUNTE" = "LUNTE DISTRICT",
                        MILENGE = "MILENGI",
                        MUSHINDAMO = "MUSHINDANO",
                        CHIKANKATA = "CHIKANKANTA",
                        "SHIWANG'ANDU" = "SHIWAMG'ANDU"))


  data <- data %>% dplyr::select(-destination_pop, -origin_pop, -destination_pop_per100k, -origin_pop_per100k)

  # Join the population data to results
  data1 <- data %>%
    left_join(new_pops, by = c("destination_clean" = "District")) %>%
    rename(destination_pop = '2022 Population')

  # Add origin population
  data2 <- data1 %>%
    left_join(new_pops, by = c("origin_clean" = "District")) %>%
    rename(origin_pop = '2022 Population')
  return(data2)

  data3 <- data2 %>% mutate(destination_pop_per100k = destination_pop/100000,
                            origin_pop_per100k = origin_pop/100000)

  return(data3)


}



### add in population within a certain radius of location

add_pops_within_xkm <- function(data, distance_within){
  dist_data_full_df <- as.data.frame(dist_data_full)
  dist_data_full_df$row_name <- rownames(dist_data_full_df)

  # Convert to long format
  dist_long <- dist_data_full_df %>%
    pivot_longer(
      cols = -row_name,  # All columns except 'row_name'
      names_to = "col_name",
      values_to = "value"
    )


  dist_long <- rename(dist_long, origin_clean = row_name, destination_clean = col_name, distance = value)
  dist_long$distance_km <- dist_long$distance/1000


  dist_long_within100km <- filter(dist_long, distance_km <= distance_within)

  dist_long_within100km_origin <- dist_long_within100km %>% group_by(origin_clean) %>% summarize(districts_within_100 = list(destination_clean))

  dist_long_within100km_origin <- rename(dist_long_within100km_origin, District = origin_clean)

  districts_with_pops <- merge(dist_long_within100km_origin, new_pops, by = "District", all.x = T)

  for(i in 1:nrow(districts_with_pops)){
    dists_within100 <- districts_with_pops$districts_within_100[i][[1]]
    filt_dat <- filter(districts_with_pops, District %in% dists_within100)
    districts_with_pops$pop_within100km[i] <- sum(filt_dat$'2022 Population', na.rm = T)
  }

  # Join the population data to results
  data1 <- data %>%
    left_join(districts_with_pops, by = c("destination_clean" = "District")) %>%
    rename(pop_within_x_destination = 'pop_within100km') %>% dplyr::select(-'2022 Population', - districts_within_100)

  # Add origin population
  data2 <- data1 %>%
    left_join(districts_with_pops, by = c("origin_clean" = "District")) %>%
    rename(pop_within_x_origin = 'pop_within100km')  %>% dplyr::select(-'2022 Population', - districts_within_100)

  data3 <- data2 %>% mutate(pop_within_x_destination_per100k = pop_within_x_destination/100000,
                            pop_within_x_origin_per100k = pop_within_x_origin/100000)

  return(data3)

}



####same OD indicator
#create a function that adds an indicator variable about whether O and D is the same
add_location_indicator <- function(data){
  data1 <- data %>% mutate(same_OD_ind = ifelse(origin_clean == destination_clean, 1, 0))
  return(data1)
}



### make proportion of origin travel column

make_proportion_column <- function(data) {
  # Calculate total trips per origin per month
  data_with_totals <- data %>%
    group_by(origin_clean, month) %>%
    mutate(total_trips_per_origin = sum(grouped.total, na.rm = TRUE)) %>%
    ungroup()

  # Calculate proportions for each OD per month
  data_with_proportions <- data_with_totals %>%
    mutate(prop_of_origin_travel = (grouped.total / total_trips_per_origin) * 100)

  # Round the proportions for clarity
  data_with_proportions <- data_with_proportions %>%
    mutate(prop_of_origin_travel = round(prop_of_origin_travel, 4))

  return(data_with_proportions)
}

### make proportion of origin travel column for one month

make_proportion_column_one_month <- function(data) {
  # Calculate total trips per origin per month
  data_with_totals <- data %>%
    group_by(origin_clean) %>%
    mutate(total_trips_per_origin = sum(grouped.total, na.rm = TRUE)) %>%
    ungroup()

  # Calculate proportions for each OD per month
  data_with_proportions <- data_with_totals %>%
    mutate(prop_of_origin_travel = (grouped.total / total_trips_per_origin) * 100)

  # Round the proportions for clarity
  data_with_proportions <- data_with_proportions %>%
    mutate(prop_of_origin_travel = round(prop_of_origin_travel, 4))

  return(data_with_proportions)
}


### add fits to pois and neg binom distributions

library(fitdistrplus)

add_pois_negbi_dists <- function(data) {
  data1 <- data %>%
    mutate(
      list_minus_one = lapply(trips_per_person, function(x) x - 1),
      mean = lapply(list_minus_one, function(trips) {
        if (is.numeric(trips) && length(trips) > 1) mean(trips) else NA
      }),
      var = lapply(list_minus_one, function(trips) {
        if (is.numeric(trips) && length(trips) > 1) var(trips) else NA
      }),
      poisson_fit = lapply(list_minus_one, function(trips) {
        if (is.numeric(trips) && length(unique(trips)) > 1) {
          tryCatch(fitdist(trips, "pois"), error = function(e) NULL)
        } else {
          NULL
        }
      }),
      poisson_aic = sapply(poisson_fit, function(fit) {
        if (!is.null(fit)) fit$aic else NA
      }),
      poisson_rate = sapply(poisson_fit, function(fit) {
        if (!is.null(fit)) fit$estimate[1] else NA  # Extract lambda
      }),
      negbi_fit = lapply(list_minus_one, function(trips) {
        if (is.numeric(trips) && length(unique(trips)) > 1) {
          tryCatch(fitdist(trips, "nbinom"), error = function(e) NULL)
        } else {
          NULL
        }
      }),
      negbi_aic = sapply(negbi_fit, function(fit) {
        if (!is.null(fit)) fit$aic else NA
      }),
      negbi_size = sapply(negbi_fit, function(fit) {
        if (!is.null(fit)) fit$estimate["size"] else NA
      }),
      negbi_mu = sapply(negbi_fit, function(fit) {
        if (!is.null(fit)) fit$estimate["mu"] else NA
      }),
      # Compare AIC values and determine the best fit
      better_fit = mapply(function(pois, negbi) {
        aics <- c(Poisson = pois, NegBi = negbi)
        aics <- aics[!is.na(aics)]  # Remove NA values
        if (length(aics) == 0) {
          return("No dist")  # If both are NA
        }
        names(which.min(aics))  # Return the distribution with lowest AIC
      }, poisson_aic, negbi_aic)
    )

  return(data1)
}






####################
###put it all together as if predicting itself

### predict first which have 100% ones and which do not

make_dist_preds <- function(data, ind_100_mod = ind_100_mod, opt_threshold_100 = optimal_cutoff$threshold,
                            opt_threshold_negbi = optimal_cutoff_neg_bi$threshold, negbi_ind_mod = neg_bi_ind_mod,
                            negbi_size_mod = negbi.size.mod, negbi_mu_mod = negbi.mu.mod,
                            poisson_rate_mod = poisson.rate.mod){
  pred.dat <- data
  preds.100 <- pred.dat %>% mutate(is.100.pred = ifelse(predict.glm(ind_100_mod, newdata = pred.dat, type = "response") > opt_threshold_100, 1, 0))

  ###now take only the ones that were predicted to not be 100% 1's
  pred.not.100 <- filter(preds.100, is.100.pred == 0)


  #now predict which are neg binom
  preds.negbi <- pred.not.100 %>% mutate(is.neg.bi = ifelse(predict.glm(negbi_ind_mod, newdata = pred.not.100, type = "response") > opt_threshold_negbi, 1, 0))


  #for those that are predicted to be negbi, find the predicted size and mu values and predicted trip values
  preds.is.negbi <- filter(preds.negbi, is.neg.bi == 1)

  preds.negbi.params <- preds.is.negbi %>% mutate(pred.neg.bi.size = predict.lm(negbi.size.mod, newdata = preds.is.negbi, type = "response"),
                                                  pred.neg.bi.mu = predict.lm(negbi.mu.mod, newdata = preds.is.negbi, type = "response"))

  #if the predictions are less than 0, put them in as 0.01
  preds.negbi.params <- preds.negbi.params %>% mutate(pred.neg.bi.size = ifelse(pred.neg.bi.size < 0 , 0.01, pred.neg.bi.size),
                                                      pred.neg.bi.mu = ifelse(pred.neg.bi.mu < 0 , 0.01, pred.neg.bi.mu))


  #for those that are predicted to be poisson, find the predicted lambda values and predicted trip values
  preds.is.poisson <- filter(preds.negbi, is.neg.bi == 0)

  preds.poisson.params <- preds.is.poisson %>% mutate(pred.poisson.rate = predict.lm(poisson_rate_mod, newdata = preds.is.poisson, type = "response"))

  #if the predictions are less than 0, put them in as 0.01
  preds.poisson.params <- preds.poisson.params %>% mutate(pred.poisson.rate = ifelse(pred.poisson.rate < 0 , 0.01, pred.poisson.rate))



  ######now stack the negative binomial and poisson datasets

  neg.bi.poisson.tog <- bind_rows(preds.negbi.params, preds.poisson.params)

  neg.bi.poisson.tog <- neg.bi.poisson.tog %>% dplyr::select(origin_clean, destination_clean, month, is.neg.bi,
                                                             pred.neg.bi.size, pred.neg.bi.mu, pred.poisson.rate)


  ####now merge the together dataset with the preds.100

  total.preds <- merge(preds.100, neg.bi.poisson.tog, by = c("origin_clean", "destination_clean", "month"), all = T)

  ####now make a column for predicted
  total.preds <- total.preds %>% rowwise() %>%
    mutate(obs_pmf = list(as.numeric(table(unlist(trips_per_person)) / length(unlist(trips_per_person))))) %>%
    mutate(predicted_density = ifelse(is.100.pred == 1, list(c(1, rep(0, length(unlist(obs_pmf)) - 1))),
                                      ifelse(is.neg.bi == 1, list(dnbinom(x = 0:(length(unlist(obs_pmf))-1), size = pred.neg.bi.size, mu = pred.neg.bi.mu)),
                                             list(dpois(x = 0:(length(unlist(obs_pmf))-1), lambda = pred.poisson.rate))))) %>%
    ungroup()

  total.preds <- total.preds %>% rowwise() %>%
    mutate(wasserstein_dens = wasserstein1d(unlist(obs_pmf), unlist(predicted_density)),
           kl.est_dens = tryCatch({KL(rbind(unlist(obs_pmf), unlist(predicted_density)))
           }, error = function(e) {
             # Handle the error and return 0
             0
           })) %>% ungroup()

  return(total.preds)
}


make_dist_preds_only_negbi <- function(data, has_het_mod = has_het_mod,
                                       opt_threshold_100 = optimal_cutoff$threshold,
                                       negbi_size_mod = negbi.size.mod, negbi_mu_mod = negbi.mu.mod){

  ## scale data for prediction
  pred.dat <- data
  pred.dat$scaled_distance_km = scale(pred.dat$distance_km)
  pred.dat$scaled_destination_pop_per100k = scale(pred.dat$destination_pop_per100k)
  pred.dat$scaled_origin_pop_per100k = scale(pred.dat$origin_pop_per100k)
  pred.dat$scaled_prop_of_origin_travel = scale(pred.dat$prop_of_origin_travel)

  preds_het <- pred.dat %>% mutate(has_het_pred = ifelse(predict.glm(has_het_mod, newdata = pred.dat, type = "response") > opt_threshold_100, 1, 0))

  ###now take only the ones that were predicted to have het
  pred_has_het <- filter(preds_het, has_het_pred == 1)

  preds.negbi.params <- pred_has_het %>% mutate(pred.neg.bi.size = predict.lm(negbi_size_mod, newdata = pred_has_het, type = "response"),
                                                pred.neg.bi.mu = predict.lm(negbi_mu_mod, newdata = pred_has_het, type = "response"))

  #if the predictions are less than 0, put them in as 0.01
  preds.negbi.params <- preds.negbi.params %>% mutate(pred.neg.bi.size = ifelse(pred.neg.bi.size < 0 , 0.01, pred.neg.bi.size),
                                                      pred.neg.bi.mu = ifelse(pred.neg.bi.mu < 0 , 0.01, pred.neg.bi.mu))


  ####now merge the together dataset with the preds_het

  preds.negbi.params <- preds.negbi.params %>% dplyr::select(origin_clean, destination_clean, month,
                                                             pred.neg.bi.size, pred.neg.bi.mu,)

  total.preds <- merge(preds_het, preds.negbi.params, by = c("origin_clean", "destination_clean", "month"), all = T)

  ####now make a column for predicted
  total.preds <- total.preds %>% rowwise() %>%
    mutate(obs_pmf = list(as.numeric(table(unlist(trips_per_person)) / length(unlist(trips_per_person))))) %>%
    mutate(predicted_density = ifelse(has_het_pred == 0, list(c(1, rep(0, length(unlist(obs_pmf)) - 1))),
                                      list(dnbinom(x = 0:(length(unlist(obs_pmf))-1), size = pred.neg.bi.size, mu = pred.neg.bi.mu)))) %>%
    ungroup()

  total.preds <- total.preds %>% rowwise() %>%
    mutate(wasserstein_dens = wasserstein1d(unlist(obs_pmf), unlist(predicted_density)),
           kl.est_dens = tryCatch({KL(rbind(unlist(obs_pmf), unlist(predicted_density)))
           }, error = function(e) {
             # Handle the error and return 0
             0
           })) %>% ungroup()

  return(total.preds)
}



make_dist_preds_only_pois <- function(data, ind_100_mod = ind_100_mod,
                                      opt_threshold_100 = optimal_cutoff$threshold,
                                      poisson_rate_mod = poisson.rate.mod){
  pred.dat <- data
  preds.100 <- pred.dat %>% mutate(is.100.pred = ifelse(predict.glm(ind_100_mod, newdata = pred.dat, type = "response") > opt_threshold_100, 1, 0))

  ###now take only the ones that were predicted to not be 100% 1's
  pred.not.100 <- filter(preds.100, is.100.pred == 0)

  preds.poisson.params <- pred.not.100 %>% mutate(pred.poisson.rate = predict.lm(poisson_rate_mod, newdata = pred.not.100, type = "response"))

  #if the predictions are less than 0, put them in as 0.01
  preds.poisson.params <- preds.poisson.params %>% mutate(pred.poisson.rate = ifelse(pred.poisson.rate < 0 , 0.01, pred.poisson.rate))


  ####now merge the together dataset with the preds.100

  preds.poisson.params <- preds.poisson.params %>% dplyr::select(origin_clean, destination_clean, month,
                                                                 pred.poisson.rate)

  total.preds <- merge(preds.100, preds.poisson.params, by = c("origin_clean", "destination_clean", "month"), all = T)

  ####now make a column for predicted
  total.preds <- total.preds %>% rowwise() %>%
    mutate(obs_pmf = list(as.numeric(table(unlist(trips_per_person)) / length(unlist(trips_per_person))))) %>%
    mutate(predicted_density = ifelse(is.100.pred == 1, list(c(1, rep(0, length(unlist(obs_pmf)) - 1))), list(dpois(x = 0:(length(unlist(obs_pmf))-1), lambda = pred.poisson.rate)))) %>%
    ungroup()

  total.preds <- total.preds %>% rowwise() %>%
    mutate(wasserstein_dens = wasserstein1d(unlist(obs_pmf), unlist(predicted_density)),
           kl.est_dens = tryCatch({KL(rbind(unlist(obs_pmf), unlist(predicted_density)))
           }, error = function(e) {
             # Handle the error and return 0
             0
           })) %>% ungroup()

  return(total.preds)
}


predict_poot_and_standardize_diffODs <- function(CDR_dist_dat){
  ## predict for both same and diff ODs
  sameOD_dat <- filter(CDR_dist_dat, same_OD_ind == 1)
  diffOD_dat <- filter(CDR_dist_dat, same_OD_ind == 0)

  sameOD_dat$poot.preds <- predict.glm(poot.sameOD.model, newdata = sameOD_dat, type = "response")
  diffOD_dat$poot.preds <- predict.glm(poot.diffOD.model, newdata = diffOD_dat, type = "response")

  #combine the two
  both <- rbind(sameOD_dat, diffOD_dat)

  #turn percentage into a proportion
  both$prop_of_origin_travel <- both$prop_of_origin_travel/100

  #if poot observed is NA (meaning its a new OD), then fill in with predicted poot. If not a new OD, keep observed poot
  both <- both %>% mutate(poot.combined = ifelse(!is.na(prop_of_origin_travel), prop_of_origin_travel, poot.preds))

  # sum all poot.combined in diff ODs
  summed.poot.preds <- both %>% filter(same_OD_ind == 0) %>% group_by(origin_clean) %>% summarize(summed_poot_preds = sum(poot.combined))

  # for same ODs, simply take either the observed or predicted poot
  origin.travel <- both %>% filter(same_OD_ind == 1) %>% group_by(origin_clean) %>% summarize(origin_travel = poot.combined)

  #merge these two datasets
  summed.poot.origin.travel <- merge(summed.poot.preds, origin.travel)

  #calculate the standardization multiplier(what it will take to standardize diff ODs)
  summed.poot.origin.travel$standardize.mult <- (1 - summed.poot.origin.travel$origin_travel) / summed.poot.origin.travel$summed_poot_preds

  # attach multiplier column to prediction dataset
  both.with.summed.poot <- merge(both, summed.poot.origin.travel, by = c("origin_clean"))

  # for all diff ODs, standardize poot observed/predicted. For same ODs, simply keep observed/predicted poot
  final.df <- both.with.summed.poot %>% mutate(poot.standardized = ifelse(
    same_OD_ind == 0, (poot.combined*standardize.mult), poot.combined))

  return(final.df)
}






simulate_mobility_no_return <- function(n_people, n_timesteps, od_matrix, population_df) {

  # Replace NAs with a small probability
  od_matrix[is.na(od_matrix)] <- 0.00000005

  # Normalize rows to ensure probabilities sum to 1
  od_matrix <- sweep(od_matrix, 1, rowSums(od_matrix), "/")

  # Create initial locations based on population proportions
  locations <- rownames(od_matrix)
  population_probs <- population_df$Population / sum(population_df$Population)
  initial_locations <- sample(locations, size = n_people, replace = TRUE, prob = population_probs)

  # Data storage for simulation
  results <- data.frame(step = integer(), uid = integer(), location = character(), stringsAsFactors = FALSE)

  # Simulate movement
  for (t in 0:n_timesteps) {
    if (t == 0) {
      current_locations <- initial_locations
    } else {
      current_locations <- sapply(current_locations, function(loc) {
        sample(locations, size = 1, replace = TRUE, prob = od_matrix[loc, ])
      })
    }

    # Store results for this time step
    results <- rbind(results, data.frame(step = t, uid = 0:(n_people-1), location = current_locations, stringsAsFactors = FALSE))
  }

  return(results)
}





##########
simulate_mobility_return <- function(n_people, n_timesteps, travel_matrix, pop_data, beta_matrix, chunk_size = 100) {

  # Function to sample trip duration using exponenitla decayscaled by β_ij
  sample_trip_length <- function(beta_ij) {  # Ensures min 1-day trips
    return(ceiling(rexp(1, rate = beta_ij)))  # Scale by inverse decay
  }

  # Initialize results storage
  chunk_results <- list()

  # Set initial locations
  locations <- rownames(travel_matrix)
  population_probs <- pop_data$Population / sum(pop_data$Population)
  initial_locations <- sample(locations, size = n_people, replace = TRUE, prob = population_probs)

  for (chunk_start in seq(1, n_people, by = chunk_size)) {
    chunk_end <- min(chunk_start + chunk_size - 1, n_people)
    chunk_ids <- chunk_start:chunk_end

    sim_results_chunk <- data.frame(
      step = rep(1:n_timesteps, each = length(chunk_ids)),
      uid = rep(chunk_ids, times = n_timesteps),
      location = NA,
      return_step = NA
    )

    sim_results_chunk$location[sim_results_chunk$step == 1] <- initial_locations[chunk_ids]

    for (person in chunk_ids) {
      origin <- initial_locations[person]
      return_step <- NA
      current_location <- origin

      for (t in 2:n_timesteps) {
        prev_location <- sim_results_chunk$location[sim_results_chunk$uid == person & sim_results_chunk$step == (t - 1)]

        # If person is scheduled to return, force return
        if (!is.na(return_step) && t == return_step) {
          current_location <- origin
          return_step <- NA
        } else if (is.na(return_step)) {
          travel_probs <- travel_matrix[prev_location, ]
          new_location <- sample(names(travel_probs), size = 1, prob = travel_probs)

          # Compute β_ij for this trip
          beta_ij <- beta_matrix[prev_location, new_location]

          # If the person moves away, set return time based on β_ij
          if (new_location != origin) {
            return_step <- t + sample_trip_length(beta_ij)
          }
          current_location <- new_location
        }

        # Store results
        sim_results_chunk$location[sim_results_chunk$uid == person & sim_results_chunk$step == t] <- current_location
        sim_results_chunk$return_step[sim_results_chunk$uid == person & sim_results_chunk$step == t] <- return_step
      }
    }

    chunk_results[[length(chunk_results) + 1]] <- sim_results_chunk
    cat("Processed chunk:", chunk_start, "to", chunk_end, "\n")
  }

  sim_results <- do.call(rbind, chunk_results)
  sim_results <- sim_results %>% mutate(uid = uid - 1,
             step = step - 1)
  return(sim_results)
}



# Compute route-specific decay rates
compute_beta_ij <- function(distance_matrix, beta_0, alpha) {
  beta_matrix <- beta_0 * exp(-alpha * distance_matrix)  # Apply exponential decay
  diag(beta_matrix) <- Inf  # No decay for same-location trips
  return(beta_matrix)
}


simulate_mobility_reset <- function(n_people, n_timesteps, travel_matrix, pop_data, reset_interval = 7, chunk_size = 100) {
  # Create initial locations based on population proportions
  locations <- rownames(travel_matrix)
  population_probs <- pop_data$Population / sum(pop_data$Population)
  initial_locations <- sample(locations, size = n_people, replace = TRUE, prob = population_probs)

  # Initialize a list to store chunk results
  chunk_results <- list()

  # Process people in chunks to improve performance
  for (chunk_start in seq(1, n_people, by = chunk_size)) {
    chunk_end <- min(chunk_start + chunk_size - 1, n_people)
    chunk_ids <- chunk_start:chunk_end

    # Create an empty data frame for the current chunk
    sim_results_chunk <- data.frame(
      step = rep(1:n_timesteps, each = length(chunk_ids)),
      uid = rep(chunk_ids, times = n_timesteps),
      location = NA
    )

    # Set initial locations for the chunk
    sim_results_chunk$location[sim_results_chunk$step == 1] <- initial_locations[chunk_ids]

    # Loop over each time step
    for (t in 2:n_timesteps) {
      previous_locations <- sim_results_chunk$location[sim_results_chunk$step == (t - 1)]

      # Check if it's a reset day
      if ((t - 1) %% reset_interval == 0) {
        current_locations <- initial_locations[chunk_ids]  # Reset to origin
      } else {
        # Normal movement based on the probability matrix
        current_locations <- sapply(previous_locations, function(prev_loc) {
          travel_probs <- travel_matrix[prev_loc, ]
          sample(names(travel_probs), size = 1, prob = travel_probs)
        })
      }

      # Store results
      sim_results_chunk$location[sim_results_chunk$step == t] <- current_locations
    }

    # Store the results of the current chunk
    chunk_results[[length(chunk_results) + 1]] <- sim_results_chunk

    # Print progress
    cat("Processed chunk:", chunk_start, "to", chunk_end, "\n")
  }

  # Combine all chunks into one data frame
  sim_results <- do.call(rbind, chunk_results)

  return(sim_results)
}


### simulate mobility that include heterogeneity in travel in only old OD's. New OD's have no distribution, just poot.standardized.
### Assumes return according to parameters in James Hay papers

simulate_mobility_het_old_districts_return <- function(n_people, n_timesteps, dist_percent_matrix, pmf_matrix, pop_data,
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

  for (person in 1:n_people) {
    origin <- initial_locations[person]
    for (destination in locations) {
      individual_travel_probs[person, destination] <- sample(dist_percent_matrix[[origin, destination]], size = 1,
                                                             prob = pmf_matrix[[origin, destination]])
    }
  }

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
      return_step <- NA
      current_location <- origin


      for (t in 2:n_timesteps) {
        prev_location <- sim_results_chunk$location[sim_results_chunk$uid == person & sim_results_chunk$step == (t - 1)]

        # If person is scheduled to return, force return
        if (!is.na(return_step) && t == return_step) {
          current_location <- origin
          return_step <- NA
        } else if (is.na(return_step)) {
          # Sample movement based on the individual's sampled probability
          travel_probs <- individual_travel_probs[person, ]
          new_location <- sample(names(travel_probs), size = 1, prob = travel_probs)

          # Compute β_ij for this trip
          beta_ij <- beta_matrix[prev_location, new_location]

          # If the person moves away, set return time based on β_ij
          if (new_location != origin) {
            return_step <- t + sample_trip_length(beta_ij)
          }
          current_location <- new_location
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



### simulate mobility that include heterogeneity in travel in only old OD's.
### Assumes return according to parameters in James Hay papers

simulate_mobility_het_obs_only <- function(n_people, n_timesteps, dist_percent_matrix, pmf_matrix, pop_data,
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

  for (person in 1:n_people) {
    origin <- initial_locations[person]
    for (destination in locations) {
      individual_travel_probs[person, destination] <- ifelse(length(pmf_matrix[[origin, destination]]) == 1,
                                                             dist_percent_matrix[[origin, destination]],
                                                             sample(dist_percent_matrix[[origin, destination]], size = 1,
                                                                    prob = pmf_matrix[[origin, destination]]))
    }
  }

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
      return_step <- NA
      current_location <- origin


      for (t in 2:n_timesteps) {
        prev_location <- sim_results_chunk$location[sim_results_chunk$uid == person & sim_results_chunk$step == (t - 1)]

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


##########
## simulate mobility using aggregated travel matrix, but factoring in percent of people that never move
simulate_mobility_return_prob_moving <- function(n_people, n_timesteps, travel_matrix, pop_data, beta_matrix, never_mover_prob = 0.65, chunk_size = 100) {

  # Function to sample trip duration using exponenitla decayscaled by β_ij
  sample_trip_length <- function(beta_ij) {  # Ensures min 1-day trips
    return(ceiling(rexp(1, rate = beta_ij)))  # Scale by inverse decay
  }

  # Initialize results storage
  chunk_results <- list()

  # Set initial locations
  locations <- rownames(travel_matrix)
  population_probs <- pop_data$Population / sum(pop_data$Population)
  initial_locations <- sample(locations, size = n_people, replace = TRUE, prob = population_probs)

  for (chunk_start in seq(1, n_people, by = chunk_size)) {
    chunk_end <- min(chunk_start + chunk_size - 1, n_people)
    chunk_ids <- chunk_start:chunk_end

    sim_results_chunk <- data.frame(
      step = rep(1:n_timesteps, each = length(chunk_ids)),
      uid = rep(chunk_ids, times = n_timesteps),
      location = NA,
      return_step = NA
    )

    sim_results_chunk$location[sim_results_chunk$step == 1] <- initial_locations[chunk_ids]

    for (person in chunk_ids) {
      origin <- initial_locations[person]
      never_movers = never_mover_prob
      never_mover <- runif(1) < never_movers  # Assign initial never_mover status
      current_location <- origin
      return_step <- NA

      for (t in 2:n_timesteps) {
        if (t %% 31 == 1) {  # Reset never_mover status every 31 days
          never_mover <- runif(1) < never_movers
        }

        if (never_mover) {
          current_location <- origin
          return_step <- NA
        } else {
          prev_location <- sim_results_chunk$location[sim_results_chunk$uid == person & sim_results_chunk$step == (t - 1)]

          # Travel decision
          if (!is.na(return_step) && t == return_step) {
            current_location <- origin
            return_step <- NA
          } else if (is.na(return_step)) {
            travel_probs <- travel_matrix[prev_location, ]
            new_location <- sample(names(travel_probs), size = 1, prob = travel_probs)

            # Compute β_ij for this trip
            beta_ij <- beta_matrix[prev_location, new_location]

            # If the person moves away, set return time based on β_ij
            if (new_location != origin) {
              return_step <- t + sample_trip_length(beta_ij)
            }
            current_location <- new_location
          }
        }

        # Store results
        sim_results_chunk$location[sim_results_chunk$uid == person & sim_results_chunk$step == t] <- current_location
        sim_results_chunk$return_step[sim_results_chunk$uid == person & sim_results_chunk$step == t] <- return_step
      }
    }

    chunk_results[[length(chunk_results) + 1]] <- sim_results_chunk
    cat("Processed chunk:", chunk_start, "to", chunk_end, "\n")
  }

  sim_results <- do.call(rbind, chunk_results)
  sim_results <- sim_results %>% mutate(uid = uid - 1,
                                        step = step - 1)
  return(sim_results)
}


###########
### accounting for both immobility and heterogeniety in travel
simulate_mobility_het_obs_only_prob_move <- function(n_people, n_timesteps, dist_percent_matrix, pmf_matrix, pop_data,
                                                     beta_matrix, never_mover_matrx, chunk_size = 100) {
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
      never_mover_prob = never_mover_matrx[origin,]
      never_mover <- runif(1) < never_mover_prob
      return_step <- NA
      current_location <- origin



      for (t in 2:n_timesteps) {
        if (t %% 31 == 1) {  # Reset never_mover status every 31 days
          never_mover <- runif(1) < never_mover_prob
        }

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



### function to retrieve summary statistics from a list of simulation
get_summary_sim_stats <- function(sim_list, districts_included, num_steps){
  district_list_data <- pop.and.dist %>% arrange(origin) %>% head(115)
  district_names <- (district_list_data$destination)

  sim_summary_results <- data.frame(sim = integer(),
                                    total_infections = integer(),
                                    num_districts_inf = integer(),
                                    time_to_10 = integer(),
                                    time_to_50per = integer(),
                                    time_to_81 = integer(),
                                    time_max_inf = integer())

  for (sim in 1:length(sim_list)){
    sim_df <- sim_list[[sim]]
    colnames(sim_df) <- district_names
    sim_df <- sim_df %>% dplyr::select(districts_included$District)
    sim_df <- sim_df %>%
      mutate(time_step = 0:num_steps) %>%  # Add time step column
      relocate(time_step)


    cum_sim_df <- sim_df
    cum_sim_df[, -1] <- apply(cum_sim_df[, -1], 2, cumsum)
    cum_sim_df$total_count <- rowSums(cum_sim_df[, -1], na.rm = TRUE)  # Excluding the time column
    cum_sim_df$nonzero_count <- rowSums(cum_sim_df[, c(-1, -83)] != 0, na.rm = TRUE)  # Excluding the time column

    sim_df$total_new_inf <- rowSums(sim_df[, -1], na.rm = TRUE)

    total_infections_sim <- max(cum_sim_df$total_count)
    num_districts_inf_sim <- max(cum_sim_df$nonzero_count)
    time_to_10_sim <- cum_sim_df$time_step[which(cum_sim_df$nonzero_count >= 10)[1]]
    time_to_50per_sim <- cum_sim_df$time_step[which(cum_sim_df$nonzero_count >= 41)[1]]
    time_to_81_sim <- cum_sim_df$time_step[which(cum_sim_df$nonzero_count >= 81)[1]]
    time_max_inf_sim <- sim_df$time_step[which.max(sim_df$total_new_inf)[1]]

    sim_summary_results <- rbind(sim_summary_results,
                                 data.frame(sim = sim,
                                            total_infections = total_infections_sim,
                                            num_districts_inf = num_districts_inf_sim,
                                            time_to_10 = time_to_10_sim,
                                            time_to_50per = time_to_50per_sim,
                                            time_to_81 = time_to_81_sim,
                                            time_max_inf = time_max_inf_sim))
  }
  return(sim_summary_results)
}


### function that takes a folder of simulations and using get_summary_sim_stats above returns a table comparing simulations across all types of aggregations
compare_table_func <- function(folder_name, file_names, districts_used, time_follow_up_list){
  compare_table <- data.frame(sim = character(),
                              extinctons_before_10 = integer(),
                              extinctions_before_50per = integer(),
                              infected_all = integer(),
                              num_districts_inf = integer(),
                              mean_tot = integer(),
                              median_tot = integer(),
                              mean_time_to_10 = integer(),
                              median_time_to_50per = integer(),
                              mean_time_to_81 = integer(),
                              mean_time_max_inf = integer(),
                              median_time_max_inf = integer())

  for (name in file_names){
    folder = folder_name
    sprint_name <- sprintf("%s/%s_sims.pkl", folder, name)
    data <- py_load_object(sprint_name)
    position = which(file_names == name)
    sum_stats <- get_summary_sim_stats(data, districts_used, time_follow_up_list[position][[1]])

    extinctons_before_10 <- sum(is.na(sum_stats$time_to_10))
    extinctions_before_50per <- sum(is.na(sum_stats$time_to_50per))
    infected_all <- sum(!is.na(sum_stats$time_to_81))
    num_districts_inf <- median(sum_stats$num_districts_inf)

    mean_tot <- mean(sum_stats$total_infections, na.rm = T)
    median_tot <- median(sum_stats$total_infections, na.rm = T)


    mean_time_to_10 <- mean(sum_stats$time_to_10, na.rm = T)
    median_time_to_50per <- median(sum_stats$time_to_50per, na.rm = T)
    mean_time_to_81 <- mean(sum_stats$time_to_81, na.rm = T)
    mean_time_max_inf <- mean(sum_stats$time_max_inf)
    median_time_max_inf <- median(sum_stats$time_max_inf)

    compare_table <- rbind(compare_table,
                           data.frame(sim = name,
                                      extinctons_before_10 = extinctons_before_10,
                                      extinctions_before_50per = extinctions_before_50per,
                                      infected_all = infected_all,
                                      num_districts_inf = num_districts_inf,
                                      mean_tot = mean_tot,
                                      median_tot = median_tot,
                                      mean_time_to_10 = mean_time_to_10,
                                      median_time_to_50per = median_time_to_50per,
                                      mean_time_to_81 = mean_time_to_81,
                                      mean_time_max_inf = mean_time_max_inf,
                                      median_time_max_inf = median_time_max_inf))
  }
  return(compare_table)
}




#### function for making dataframe ready for plotting for simulation results
make_sim_results_df_for_plot <- function(simulation_list){
  #make combined dataframe with group, starting location, simulation id (1-100), step, new infections, and cumulative infections
  #for each simulation in the list
  infections_df <- imap_dfr(simulation_list, function(group_list, group_name) {
    imap_dfr(group_list, function(sim_list, start_loc_name) {
      imap_dfr(sim_list, function(sim, sim_id) {
        total_new <- rowSums(sim)
        tibble(
          group = group_name,
          start_loc = start_loc_name,
          sim = sim_id,
          step = seq_along(total_new),
          new_infections = total_new,
          cum_infections = cumsum(total_new)
        )
      })
    })
  })

  #find the median # of new infections at each step for each simulation group
  medians <- infections_df %>%
    group_by(group, start_loc, step) %>%
    summarise(med = median(cum_infections), .groups = "drop")

  #find the distance from the median # of new infections for each simulation in a group
  distances <- infections_df %>%
    left_join(medians, by = c("group", "start_loc", "step")) %>%
    group_by(group, start_loc, sim) %>%
    summarise(dist = sum((cum_infections - med)^2), .groups = "drop")

  #take the closest simulation to that median
  closest <- distances %>%
    group_by(group, start_loc) %>%
    slice_min(order_by = dist, n = 1, with_ties = FALSE)

  #make an indicator variable to highlight the closest simulation to the median for each group
  infections_df <- infections_df %>%
    mutate(highlight = paste(group, start_loc) %in% paste(medians$group, medians$start_loc, medians$step))

  #make sure group and starting location are in correct order
  infections_df$group <- factor(infections_df$group, levels = c("Actual", "Aggregate", "Move_het",
                                                                "Travel_het", "Both_het"))
  infections_df$start_loc <- factor(infections_df$start_loc, levels = c("Urban", "Semi-urban", "Rural"))

  return(infections_df)

}


#### function for making dataframe ready for plotting for simulation results
make_sim_results_df_for_plot <- function(simulation_list){
  #make combined dataframe with group, starting location, simulation id (1-100), step, new infections, and cumulative infections
  #for each simulation in the list
  infections_df <- imap_dfr(simulation_list, function(group_list, group_name) {
    imap_dfr(group_list, function(sim_list, start_loc_name) {
      imap_dfr(sim_list, function(sim, sim_id) {
        total_new <- rowSums(sim)
        tibble(
          group = group_name,
          start_loc = start_loc_name,
          sim = as.character(sim_id),
          step = seq_along(total_new),
          new_infections = total_new,
          cum_infections = cumsum(total_new)
        )
      })
    })
  })

  #find the median # of new infections at each step for each simulation group
  medians <- infections_df %>%
    group_by(group, start_loc, step) %>%
    summarise(med = median(cum_infections), .groups = "drop")


  # add 'highlight' = TRUE for rows that are the step-level medians
  medians_df <- medians %>%
    mutate(highlight = TRUE) %>%
    rename(cum_infections = med)   # Optional cum sum for completeness

  # Combine with infections_df
  infections_df <- infections_df %>%
    mutate(highlight = FALSE) %>%
    bind_rows(medians_df, .)

  #make sure group and starting location are in correct order
  infections_df$group <- factor(infections_df$group, levels = c("Actual", "Aggregate", "Move_het",
                                                                "Travel_het", "Both_het"))
  infections_df$start_loc <- factor(infections_df$start_loc, levels = c("Urban", "Semi-urban", "Rural"))

  return(infections_df)

}

# Function to compute normalized truncated PMF vector
get_nb_pmf <- function(size, mu, cutoff_prob = 0.99, max_val = 15) {
  # Convert mean to prob
  prob <- size / (size + mu)

  x_vals <- 0:max_val
  pmf <- dnbinom(x_vals, size = size, prob = prob)
  cum_pmf <- cumsum(pmf)

  # Cutoff when cumulative probability reaches target (e.g., 99.9%)
  cutoff <- which(cum_pmf >= cutoff_prob)[1]

  truncated_pmf <- pmf[1:cutoff]
  truncated_pmf / sum(truncated_pmf)  # Normalize
}




### accounting for both immobility and heterogeniety in travel
simulate_mobility_het_obs_only_prob_move <- function(n_people, n_timesteps, dist_percent_matrix, pmf_matrix, pop_data,
                                                     beta_matrix, never_mover_prob = 0.65, chunk_size = 100) {
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
      never_mover_probability = never_mover_prob
      never_mover <- runif(1) < never_mover_probability
      return_step <- NA
      current_location <- origin



      for (t in 2:n_timesteps) {
        if (t %% 31 == 1) {  # Reset never_mover status every 31 days
          never_mover <- runif(1) < never_mover_probability
        }

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
