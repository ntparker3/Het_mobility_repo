###### predicting code
library(pROC)

### load in the train data
load(file = "~/Documents/Mobility/Zambia/Cleaning_modeling_code/Saved_objects/mta_s2_clean")


#seperate between same and different ODs
mta_s2_diffODs <- filter(mta_s2_clean, same_OD_ind == 0)
mta_s2_sameODs <- filter(mta_s2_clean, same_OD_ind == 1)



##create an indicator variable that is 1 if the percetnage_ones is 100
mta_s2_diffODs <- mutate(mta_s2_diffODs, has_het = ifelse(percentage_ones == 100, 0, 1))

## scale all explanatory variables except grouped.total
mta_s2_diffODs$scaled_distance_km = scale(mta_s2_diffODs$distance_km)
mta_s2_diffODs$scaled_destination_pop_per100k = scale(mta_s2_diffODs$destination_pop_per100k)
mta_s2_diffODs$scaled_origin_pop_per100k = scale(mta_s2_diffODs$origin_pop_per100k)
mta_s2_diffODs$scaled_prop_of_origin_travel = scale(mta_s2_diffODs$prop_of_origin_travel)


#create a binomial model that models the indicator variable
has_het_mod <- glm(has_het ~ log(grouped.total) + scaled_distance_km + scaled_destination_pop_per100k +
                     scaled_origin_pop_per100k + scaled_prop_of_origin_travel,
                   data = mta_s2_diffODs,
                   family = "binomial"
)
has_het_mod$aic
summary.glm(has_het_mod)

#add predictions of the indicator value to the oroginal dataset
mta_s2_diffODs_with_het_preds <- mta_s2_diffODs %>% mutate(has_het_preds = predict.glm(has_het_mod, type = "response"))

roc_curve <- roc(mta_s2_diffODs_with_het_preds$has_het, mta_s2_diffODs_with_het_preds$has_het_preds)

# Find the optimal threshold
optimal_cutoff <- coords(roc_curve, "best", ret = "threshold", best.method = "closest.topleft")

# Make indicator predictions based on the optimal cutoff
mta_s2_diffODs_with_het_preds <- mta_s2_diffODs_with_het_preds %>% mutate(het_preds = ifelse(has_het_preds > optimal_cutoff$threshold, 1, 0))

# Calculate accuracy
accuracy <- sum(mta_s2_diffODs_with_het_preds$has_het == mta_s2_diffODs_with_het_preds$het_preds) / nrow(mta_s2_diffODs_with_het_preds)
accuracy


####### predicting neg.binom

###take only the ODs where there is travel het
non_100_mta_s2_diffODs <- filter(mta_s2_diffODs_with_het_preds, has_het == 1)

#add pois and negbi dist fits to this dataset
non_100_mta_s2_diffODs <- add_pois_negbi_dists(non_100_mta_s2_diffODs)

#make an indicator variable for NegBi
non_100_mta_s2_diffODs <- non_100_mta_s2_diffODs %>% mutate(neg.bi.ind = ifelse(better_fit == "NegBi", 1, 0))

ggplot(non_100_mta_s2_diffODs) +
  geom_point(aes(x = log(grouped.total), y = neg.bi.ind))

#create a binomial model that models the neg binomial indicator variable
neg_bi_ind_mod <- glm(neg.bi.ind ~ log(grouped.total) + scaled_distance_km + scaled_destination_pop_per100k +
                        scaled_origin_pop_per100k + scaled_prop_of_origin_travel,
                      data = non_100_mta_s2_diffODs,
                      family = "binomial")
neg_bi_ind_mod$aic
summary.glm(neg_bi_ind_mod)

#add predictions of the indicator value to the oroginal dataset
non_100_mta_s2_diffODs_with_negbi_preds <- non_100_mta_s2_diffODs %>% mutate(neg.bi.predictions = predict.glm(neg_bi_ind_mod, type = "response"))

roc_curve_neg_bi <- roc(non_100_mta_s2_diffODs_with_negbi_preds$neg.bi.ind, non_100_mta_s2_diffODs_with_negbi_preds$neg.bi.predictions)

# Find the optimal threshold
optimal_cutoff_neg_bi <- coords(roc_curve_neg_bi, "best", ret = "threshold", best.method = "closest.topleft")

# Make indicator predictions based on the optimal cutoff
non_100_mta_s2_diffODs_with_negbi_preds <- non_100_mta_s2_diffODs_with_negbi_preds %>% mutate(neg_bi_preds = ifelse(neg.bi.predictions > optimal_cutoff_neg_bi$threshold, 1, 0))

# Calculate accuracy
accuracy_neg_bi <- sum(non_100_mta_s2_diffODs_with_negbi_preds$neg.bi.ind == non_100_mta_s2_diffODs_with_negbi_preds$neg_bi_preds) / nrow(non_100_mta_s2_diffODs_with_negbi_preds)
accuracy_neg_bi


###filter for only those that fit to neg bi
mta_s2_diff_negbi <- filter(non_100_mta_s2_diffODs_with_negbi_preds, better_fit == "NegBi")

ggplot(data = mta_s2_diff_negbi) +
  geom_point(aes(x = log(destination_pop_per100k), y = negbi_size)) + ylim(0,10)

#model for size
negbi.size.mod <- lm(negbi_size ~ log(grouped.total) + scaled_distance_km + scaled_destination_pop_per100k +
                       scaled_origin_pop_per100k + scaled_prop_of_origin_travel,
                     data = mta_s2_diff_negbi)
summary.lm(negbi.size.mod)
AIC(negbi.size.mod)

#model for mu
negbi.mu.mod <- lm(negbi_mu ~ log(grouped.total) + scaled_distance_km + scaled_destination_pop_per100k +
                     scaled_origin_pop_per100k + scaled_prop_of_origin_travel,
                   data = mta_s2_diff_negbi)
summary.lm(negbi.mu.mod)



##### now with data that fits best to poisson, try to predict the lambda


###filter for only those that fit to neg bi
mta_s2_diff_poisson <- filter(non_100_mta_s2_diffODs_with_negbi_preds, better_fit == "Poisson")

#model for rate (lambda)
ggplot(data = mta_s2_diff_poisson) +
  geom_point(aes(x = log(prop_of_origin_travel), y = poisson_rate))

poisson.rate.mod <- lm(poisson_rate ~ log(grouped.total) + scaled_distance_km + scaled_destination_pop_per100k +
                         scaled_origin_pop_per100k + scaled_prop_of_origin_travel,
                       data = mta_s2_diff_poisson)
summary(poisson.rate.mod)
AIC(poisson.rate.mod)




