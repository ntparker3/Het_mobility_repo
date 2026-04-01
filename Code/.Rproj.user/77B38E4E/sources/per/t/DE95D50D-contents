library(arrow)
library(dplyr)

times <- read_parquet(paste0(
  "https://data.opentimes.org/times/version=0.0.1/mode=car/year=2024",
  "/geography=tract/state=17/times-0.0.1-car-2024-tract-17-0.parquet"
))

times.county <- read_parquet(paste0(
  "https://data.opentimes.org/times/version=0.0.1/mode=car/year=2024",
  "/geography=county/state=17/times-0.0.1-car-2024-county-17-0.parquet"
))

first100 <- head(times, 100000)
str(first100)

times$origin_tract <- substr(times$origin_id, 1, 11)
times$dest_tract <- substr(times$destination_id, 1, 11)

sametract.notsameblock <- filter(times, origin_tract == dest_tract,
                                 origin_id != destination_id)

sametract.notsameblock2 <- sametract.notsameblock %>% group_by(origin_tract) %>%
  summarize(max_time = max(duration_sec),
            mean_time = mean(duration_sec))

state_fips <- c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13",
                "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25",
                "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36",
                "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48",
                "49", "50", "51", "53", "54", "55", "56")

# Filter out unused FIPS codes (like 03, 07, 14, etc. if needed)
# You can use a predefined valid FIPS list if necessary

# Base URL
base_url <- "https://data.opentimes.org/times/version=0.0.1/mode=car/year=2024/geography=tract/state="

all_states <- data.frame()

for (state in state_fips){
  state_dat <- read_parquet(paste0(base_url, state, "/times-0.0.1-car-2024-tract-", state, "-0.parquet"))
  state_dat$origin_tract <- substr(state_dat$origin_id, 1, 11)
  state_dat$dest_tract <- substr(state_dat$destination_id, 1, 11)

  sametract.notsameblock <- filter(state_dat, origin_tract == dest_tract,
                                   origin_id != destination_id)

  sametract.notsameblock2 <- sametract.notsameblock %>% group_by(origin_tract) %>%
    summarize(max_time = max(duration_sec),
              mean_time = mean(duration_sec))

  all_states <- rbind(all_states,
        data.frame(sametract.notsameblock2))
}

write.csv(all_states, "~/Documents/Mobility/Accessibility Measures/census_tracts_max_block_id_travel.csv")


state_fips <- c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13",
                "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25",
                "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36",
                "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48",
                "49", "50", "51", "53", "54", "55", "56")

# Filter out unused FIPS codes (like 03, 07, 14, etc. if needed)
# You can use a predefined valid FIPS list if necessary

# Base URL
base_url <- "https://data.opentimes.org/times/version=0.0.1/mode=car/year=2024/geography=tract/state="

all_states <- data.frame()


for (state in state_fips){
  state_dat <- read_parquet(paste0(base_url, state, "/times-0.0.1-car-2024-tract-", state, "-0.parquet"))
  state_dat$origin_county <- substr(state_dat$origin_id, 1, 5)
  state_dat$dest_county <- substr(state_dat$destination_id, 1, 5)

  samecounty.notsametract <- filter(state_dat, origin_county == dest_county,
                                   origin_id != destination_id)

  samecounty.notsametract2 <- samecounty.notsametract %>% group_by(origin_county) %>%
    summarize(max_time = max(duration_sec),
              mean_time = mean(duration_sec))

  all_states <- rbind(all_states,
                      data.frame(samecounty.notsametract2))
}


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


# Extract state and county parts
all_states$state_part <- substr(all_states$origin_county, 1, 2)
all_states$county_part <- substr(all_states$origin_county, 3, 5)

# Join with the mapping
all_states_clean_code <- merge(all_states, state_fips_to_abbr, by.x = "state_part", by.y = "state_fips", all.x = TRUE)

# Create the final formatted string
all_states_clean_code$admin2 <- paste0("US.", all_states_clean_code$abbr, ".", all_states_clean_code$county_part)

print(df$formatted)


write.csv(all_states_clean_code, "~/Documents/Mobility/Accessibility Measures/counties_max_tract_travel.csv")




str(all_states_clean_code)

##### bring in country demographics
county_dems <- read.csv("~/Downloads/USA_2020_Census_Population_Characteristics/county_2.csv")
all_states_clean_code <- read_csv("~/Documents/Mobility/Accessibility Measures/counties_max_tract_travel.csv")

county_dems <- county_dems %>% mutate(clean_county_code = ifelse(Geographic.Identifier < 10000,
                                                                 paste0("0", as.character(Geographic.Identifier)),
                                                                 as.character(Geographic.Identifier)),
                                      Land_area_sqkm = Area.of.Land..Square.Meters. / 1000000)

county_dems_clean <- dplyr::select(county_dems, clean_county_code, Population.Density..people.per.square.kilometer.,
                                   Percent.of.population.that.is.rural, Land_area_sqkm)


all_states_clean_code_with_dems <- merge(all_states_clean_code, county_dems_clean, by.x = "origin_county",
                                         by.y = "clean_county_code") %>%
  rename("Pop_density_sqkm" = Population.Density..people.per.square.kilometer.,
         "Percent_pop_rural" = Percent.of.population.that.is.rural,
         "Percent_pop_rural" = Percent.of.population.that.is.rural)


state_region_map <- list(
  Northeast = c("CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA"),
  Midwest   = c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD"),
  South     = c("DE", "DC", "FL", "GA", "MD", "NC", "SC", "VA", "WV",
                "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX"),
  West      = c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY",
                "AK", "CA", "HI", "OR", "WA")
)

get_region <- function(state_abbr) {
  for (region in names(state_region_map)) {
    if (state_abbr %in% state_region_map[[region]]) {
      return(region)
    }
  }
  return(NA)  # if state abbreviation is not found
}

all_states_clean_code_with_dems$region <- sapply(all_states_clean_code_with_dems$abbr, get_region)

all_states_clean_code_with_dems$region <- factor(all_states_clean_code_with_dems$region)

all_states_clean_code_with_dems$state <- factor(all_states_clean_code_with_dems$abbr)

write.csv(all_states_clean_code_with_dems, "~/Documents/Mobility/Dems/counties_max_tract_travel_dems.csv")











census_tracts_max_block <- read.csv("~/Documents/Mobility/Accessibility Measures/census_tracts_max_block_id_travel.csv")

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

census_tracts_max_block <- census_tracts_max_block %>% mutate(clean_census_tract_code = ifelse(origin_tract < 10000000000,
                                                                 paste0("0", as.character(origin_tract)),
                                                                 as.character(origin_tract)))

# Extract state and county parts
census_tracts_max_block$state_part <- substr(census_tracts_max_block$clean_census_tract_code, 1, 2)
census_tracts_max_block$county_part <- substr(census_tracts_max_block$clean_census_tract_code, 3, 5)
census_tracts_max_block$census_tract_part <- substr(census_tracts_max_block$clean_census_tract_code, 6, 11)

clean_census_tracts_max_block <- merge(census_tracts_max_block, state_fips_to_abbr, by.x = "state_part", by.y = "state_fips", all.x = TRUE)

# Create the final formatted string
clean_census_tracts_max_block$census_tract <- paste0("US.", clean_census_tracts_max_block$abbr,
                                               ".", clean_census_tracts_max_block$county_part,
                                               ".", clean_census_tracts_max_block$census_tract_part)



##### bring in ct demographics
census_tract_dems <- read.csv("~/Downloads/USA_2020_Census_Population_Characteristics/Census Tract_3.csv")


shape_ct <- census_tract_dems %>% dplyr::select(clean_ct_code, Shape.Area, Area.of.Land..Square.Meters., Area.of.Water..Square.Meters.)

census_tract_dems <- census_tract_dems %>% mutate(clean_ct_code = ifelse(Geographic.Identifier < 10000000000,
                                                                 paste0("0", as.character(Geographic.Identifier)),
                                                                 as.character(Geographic.Identifier)),
                                                  Land_area_sqkm = Area.of.Land..Square.Meters. / 1000000)

census_tract_dems_clean <- dplyr::select(census_tract_dems, clean_ct_code, Population.Density..people.per.square.kilometer.,
                                   Percent.of.population.that.is.rural, Land_area_sqkm)


clean_census_tracts_max_block_with_dems <- merge(clean_census_tracts_max_block, census_tract_dems_clean, by.x = "clean_census_tract_code",
                                         by.y = "clean_ct_code") %>%
  rename("Pop_density_sqkm" = Population.Density..people.per.square.kilometer.,
         "Percent_pop_rural" = Percent.of.population.that.is.rural)


state_region_map <- list(
  Northeast = c("CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA"),
  Midwest   = c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD"),
  South     = c("DE", "DC", "FL", "GA", "MD", "NC", "SC", "VA", "WV",
                "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX"),
  West      = c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY",
                "AK", "CA", "HI", "OR", "WA")
)

get_region <- function(state_abbr) {
  for (region in names(state_region_map)) {
    if (state_abbr %in% state_region_map[[region]]) {
      return(region)
    }
  }
  return(NA)  # if state abbreviation is not found
}

clean_census_tracts_max_block_with_dems$region <- sapply(clean_census_tracts_max_block_with_dems$abbr, get_region)

clean_census_tracts_max_block_with_dems$region <- factor(clean_census_tracts_max_block_with_dems$region)

clean_census_tracts_max_block_with_dems$state <- factor(clean_census_tracts_max_block_with_dems$abbr)

clean_census_tracts_max_block_with_dems2 <- na.omit(clean_census_tracts_max_block_with_dems)

write.csv(clean_census_tracts_max_block_with_dems2, "~/Documents/Mobility/Dems/census_tracts_max_block_id_travel_dems.csv")

str(clean_census_tracts_max_block_with_dems2)




####
### relationship between accessibility measures and other variables

# first basic histogram of both

ggplot() +
  geom_histogram(aes(clean_census_tracts_max_block_with_dems2$mean_time), color = "blue", fill = "white", alpha = 0.3) +
  geom_histogram(aes(clean_census_tracts_max_block_with_dems2$max_time), color = "red", fill = "white", alpha = 0.3) +
  xlim(0, 4000)

ggplot() +
  geom_histogram(aes(all_states_clean_code_with_dems$mean_time), color = "blue", fill = "white", alpha = 0.3) +
  geom_histogram(aes(all_states_clean_code_with_dems$max_time), color = "red", fill = "white", alpha = 0.3) +
  xlim(0, 10000)


### mean time verus other variables
ggplot() +
  geom_point(aes(x = clean_census_tracts_max_block_with_dems2$Percent_pop_rural, y = clean_census_tracts_max_block_with_dems2$mean_time)) +
  ylim(0, 4000)

ggplot() +
  geom_point(aes(x = all_states_clean_code_with_dems$Percent_pop_rural, y = all_states_clean_code_with_dems$mean_time)) +
  ylim(0, 10000)

ggplot() +
  geom_point(aes(x = clean_census_tracts_max_block_with_dems2$Pop_density_sqkm, y = clean_census_tracts_max_block_with_dems2$mean_time)) +
  ylim(0, 4000) +
  xlim(0, 5000)

ggplot() +
  geom_point(aes(x = all_states_clean_code_with_dems$Pop_density_sqkm, y = all_states_clean_code_with_dems$mean_time)) +
  ylim(0, 10000) +
  xlim(0, 1000)



### max time verus other variables
ggplot() +
  geom_point(aes(x = clean_census_tracts_max_block_with_dems2$Percent_pop_rural, y = clean_census_tracts_max_block_with_dems2$max_time)) +
  ylim(0, 4000)

ggplot() +
  geom_point(aes(x = all_states_clean_code_with_dems$Percent_pop_rural, y = all_states_clean_code_with_dems$max_time)) +
  ylim(0, 10000)

ggplot() +
  geom_point(aes(x = clean_census_tracts_max_block_with_dems2$Pop_density_sqkm, y = clean_census_tracts_max_block_with_dems2$max_time)) +
  ylim(0, 4000) +
  xlim(0, 5000)

ggplot() +
  geom_point(aes(x = all_states_clean_code_with_dems$Pop_density_sqkm, y = all_states_clean_code_with_dems$max_time)) +
  ylim(0, 10000) +
  xlim(0, 1000)


### between each other
ggplot() +
  geom_point(aes(x = clean_census_tracts_max_block_with_dems2$mean_time, y = clean_census_tracts_max_block_with_dems2$max_time)) +
  ylim(0, 4000)

ggplot() +
  geom_point(aes(x = all_states_clean_code_with_dems$mean_time, y = all_states_clean_code_with_dems$max_time)) +
  ylim(0, 20000) +
  xlim(0,20000)









