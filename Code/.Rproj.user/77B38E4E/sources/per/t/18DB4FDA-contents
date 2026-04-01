###### visitation law



library(dplyr)
library(geosphere)
library(data.table)



may.2024 <- read.csv("~/Documents/Mobility/Zambia/Updated_CDR/Updated_May_2024.csv")

more14 <- may.2024 %>% group_by(hashed_msisdn_key) %>% summarize(n = n()) %>% filter(n >= 14)

more14ids <- more14$hashed_msisdn_key

filt.may.2024 <- may.2024 %>% filter(hashed_msisdn_key %in% more14ids)

home_location <- filt.may.2024 %>% group_by(hashed_msisdn_key) %>% summarize(home_loc = max(sub_region))



# Merge probabilities back to people_df

home_location <- home_location %>%
  mutate(
    home_loc = recode(home_loc,
                      KAPIRI = "KAPIRI MPOSHI",
                      SHANGOMBO = "SHANG'OMBO",
                      MILENGE = "MILENGI",
                      "ITHEZI-TEZHI" = "ITEZHI-TEZHI",
                      MALOLE = "MUNGWI",
                      MUYOMBE = "ISOKA")) %>% filter(home_loc != "ZICTA") %>% filter(home_loc != "")

home_location <- na.omit(home_location)



setDT(filt.may.2024)

filt.may.2024[, Call_DayA := as.Date(Call_DayA)]

# Sort data by individual and date
setorder(filt.may.2024, hashed_msisdn_key, Call_DayA)

may.2024.filled <- filt.may.2024[, .(Call_DayA = seq(min(Call_DayA, na.rm = TRUE),
                                                          max(as.Date("2024-05-31"), na.rm = TRUE),
                                                          by = "1 day")), by = hashed_msisdn_key]

# Merge and fill missing locations
dat_filled <- merge(may.2024.filled, filt.may.2024, by = c("hashed_msisdn_key", "Call_DayA"), all.x = TRUE)
dat_filled[, sub_region := zoo::na.locf(sub_region, na.rm = FALSE), by = hashed_msisdn_key]


dat_filled_w_home <- merge(dat_filled, home_location, by = "hashed_msisdn_key", all.x = T)

# Use shift to calculate "next day" values efficiently
dat_filled_w_home[, `:=`(
  next_day = shift(Call_DayA, type = "lead"),                  # Next day's Call_DayA
  next_day_location = shift(sub_region, type = "lead"),       # Next day's location
  next_day_individ = shift(hashed_msisdn_key, type = "lead")  # Next day's ID
)]

# destination.strat2: Same ID as the next observation
dat_filled_w_home[, destination := fifelse(hashed_msisdn_key == next_day_individ, next_day_location, NA_character_)]

# Add binary trip indicators
dat_filled_w_home[, `:=`(
  trips = fifelse(!is.na(destination), 1, NA_integer_)
)]

# Rename sub_region to origin
setnames(dat_filled_w_home, "sub_region", "origin")

dat_filled_w_home <- filter(dat_filled_w_home, !is.na(destination))

load(file = "~/Documents/Mobility/Zambia/Cleaning_modeling_code/Saved_objects/pop_and_dist")

dat_filled_w_homew_dist <- merge(dat_filled_w_home, pop.and.dist, by = c("origin", "destination"))

t <- head(dat_filled_w_homew_dist, 8000000)

dat_filled_w_homew_dist_diff_ODS <- filter(dat_filled_w_homew_dist, same_OD_ind == 0)

remove(dat_filled)


zambia_district_density <- readxl::read_excel("~/Downloads/zambia_density-converted.xlsx")
zambia_district_density <- zambia_district_density[, c(1,5,6,7)]
colnames(zambia_district_density) <- c("District", "area_km2", "population", "density_km2")
zambia_district_density$District <- toupper(zambia_district_density$District)


location_stats <- dat_filled_w_homew_dist %>%
  group_by(destination, hashed_msisdn_key) %>%
  summarise(r = mean(distance_km),
            f = n(), .groups = "drop") %>%
  group_by(destination) %>%
  summarise(r = mean(r),
            f = mean(f),
            V = n_distinct(hashed_msisdn_key),
            .groups = "drop") %>%
  mutate(destination = recode(destination,
                              "CHIENGI" = "CHIENGE",
                              "MUSHINDANO" = "MUSHINDAMO")) %>%
  left_join(zambia_district_density, by = join_by("destination" == "District")) %>%
  mutate(V_density = V / area_km2)


library(ggplot2)

location_stats <- location_stats %>%
  mutate(rf = r * f)

ggplot(location_stats, aes(x = log10(rf), y = log10(V_density))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "rf (km per month)", y = "Visitors (per km2)") +
  theme_minimal()


model <- lm(log10(V_density) ~ log10(rf), data = location_stats)
summary.lm(model)


