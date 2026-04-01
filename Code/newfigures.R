library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(colorspace)
library(scales)



colorPalette_08 = c("#2271B2","#F748A5","#359B73","#F0E442","#D55E00","#3DB7E9","#E69F00","#000000")
colorPalette_12 = c("#E20134","#FF6E3A","#008DF9","#8400CD","#FFC33B","#9F0162","#009F81","#FF5AAF","#00FCCF","#00C2F9","#FFB2FD","#A40122")
colorPalette_15 = c("#F60239","#003C86","#EF0096","#9400E6","#009FFA","#008169","#68023F","#00DCB5","#FFCFE2","#FF71FD","#7CFFFA","#6A0213","#008607","#00E307","#FFDC3D")

sequential_palette <- iwanthue(
  n = 10,  # Number of colors
  hmin = 220, hmax = 260, # Blue hue range
  cmin = 50, cmax = 100, # Higher chroma
  lmin = 30, lmax = 80 # Varying lightness
)


# Pick anchor colors with iwanthue
anchor_colors <- c("#6A0213", "#2271B2", "#E69F00")

anchor_colors <- c("#359B73", "#FFC33B", "#6A0213")

anchor_colors <- c("#b8d8a7" ,"#93c1ed")

anchor_colors <- c("#6A0213", "#66C2A5", "#FFFFBF")

anchor_colors <- c("#3288BD", "#5E4FA2", "#9E4FB3")



library(paletteer)

# Build continuous palette
my_cont_palette <- colorRampPalette(anchor_colors)

my_cont_palette <- colorRampPalette(paletteer_d("MoMAColors::Ernst"))
my_cont_palette
paletteer_d("MoMAColors::Ernst")

us_neighbor_labels <- data.frame(
  admin = c("Canada", "Mexico"),
  lon = c(-100, -105),
  lat = c(50.3, 28)
)

us_border_countries <- ne_countries(scale = "medium", country = c("US", "Canada", "Mexico"), returnclass = "sf")


monthly_users_plot <- monthly_users_map_data %>%
  filter(!STATEFP %in% c("02", "15", "72")) %>%
  ggplot() +
  geom_sf(aes(fill = mean_monthly_users_log), color = NA) +
  geom_sf(data = state_outline_subset, fill = NA, color = "#E20134", linewidth = 0.5) +
  geom_sf(data = us_border_countries, fill = NA, color = "gray70", linewidth = 0.3) +
  geom_text(data = us_neighbor_labels,
            aes(x = lon, y = lat, label = admin),
            size = 3, color = "black") +
  scale_fill_gradientn(colors = my_cont_palette(256),
                       limits = c(0,6),
                       na.value = "gray90", name = "Log average \nmonthly users") +
  coord_sf(xlim = c(-135, -68), ylim = c(24, 49.4)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(t = 0, r = 0, b = 0, l = 40)
  ) +
  annotation_scale(location = "br", width_hint = 0.2, pad_x = unit(5, "cm"))  # scale bar bottom right

monthly_users_plot

ggsave("~/Desktop/us_monthly_users_plot.png", monthly_users_plot, width = 10, height = 7)

ak_monthly_users_plot <- monthly_users_map_data %>%
  filter(STATEFP == "02") %>%  # Alaska only
  ggplot() +
  geom_sf(aes(fill = mean_monthly_users_log), color = NA) +
  scale_fill_gradientn(colors = my_cont_palette(256),
                         limits = c(0,6),
                         na.value = "gray90", name = "Log average \nmonthly users") +
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
  scale_fill_gradientn(colors = my_cont_palette(256),
                         limits = c(0,6),
                         na.value = "gray90", name = "Log average \nmonthly users") +
  theme_void()
hi_monthly_users_plot

ggsave("~/Desktop/ak_monthly_users_plot.png", ak_monthly_users_plot, width = 10, height = 7)
ggsave("~/Desktop/hi_monthly_users_plot.png", hi_monthly_users_plot, width = 10, height = 7)



############
###### Zambia
############
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

# Get Africa country boundaries
africa <- ne_countries(scale = "medium", continent = "Africa", returnclass = "sf")
zambia <- filter(africa, admin == "Zambia")

africa_labs <- africa %>%
  mutate(label_pt = st_point_on_surface(geometry)) %>%
  st_as_sf()  # turn back into sf

neighbor_labels <- data.frame(
  admin = c("Angola", "DRC", "Tanzania", "Malawi", "Mozambique",
            "Zimbabwe", "Botswana", "Namibia"),
  lon = c(22, 27.2, 32.5, 33.5, 32, 29.7, 24.7, 23.8),
  lat = c(-12.5, -10.9, -8.8, -13.3, -15, -16.8, -18.5, -17.8)
)

# plot map of included districts
zambia_monthly_users_map <- ggplot() +
  geom_sf(data = africa, fill = "NA", color = "gray70", size = 0.1) +
  geom_sf(data = zambia, color = "gray90", size = 0.1) +
  geom_sf(data = zambia_monthly_users, aes(fill = log_avg_monthly_users), color = "white", size = 0.1) +
  geom_text(data = neighbor_labels,
            aes(x = lon, y = lat, label = admin),
            size = 3, color = "black") +
  scale_fill_gradientn(colors = my_cont_palette(256),
                         limits = c(0,6),
                         na.value = "gray90", name = "Log average \nmonthly users",
                       breaks = c(0, 2, 4, 6),
                       labels = c(0, 2, 4, 6)) +
  annotation_scale(location = "tr", width_hint = 0.3, text_cex = 0.6, pad_y = unit(0.8, "cm"),
                   pad_x = unit(0, "cm")) +
  coord_sf(xlim = c(20, 35), ylim = c(-19.5, -7), expand = FALSE) +
  theme_minimal(base_size = 10) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = c(0.7, 0.08),
        legend.direction = "horizontal",
        legend.title = element_text(size = 12, vjust = 1, hjust = 0.5),
        legend.text = element_text(size = 12),
        legend.justification = "center",
        legend.box.margin = margin(t = 20),
        plot.margin = margin(t = 0)
  ) + guides(fill = guide_colourbar(barwidth = 9, barheight = 1.6))
zambia_monthly_users_map

ggsave("~/Desktop/zambia_monthly_users_map.png", zambia_monthly_users_map, width = 10, height = 6)




# Get world polygons
world <- ne_countries(scale = "medium", returnclass = "sf")

# Highlight Zambia and USA
world$highlight <- ifelse(world$admin %in% c("Zambia", "United States of America"),
                          "highlight", "other")

# Define a central point for the globe (longitude and latitude)
central_lon <- 0     # center on Africa
central_lat <- 10

# Plot globe
world_plot <- ggplot(world) +
  geom_sf(aes(fill = highlight), color = "gray50", size = 0.2) +
  scale_fill_manual(values = c("highlight" = "#E20134", "other" = "gray90"),
                    guide = FALSE) +
  coord_sf(crs = "+proj=robin +lat_0=10 +lon_0=0") +  # orthographic projection
  theme_minimal() +
  theme(
    panel.grid = element_blank())

world_plot


ggsave("~/Desktop/world.jpeg", world_plot)





anchor_colors <- c("#6A0213", "#D55E00", "#FFFFBF")
# Build continuous palette
my_cont_palette <- colorRampPalette(anchor_colors)



### function to plot mobility matrix of districts grouped by procinve
plot_mob_matrix_by_province <- function(mob_matrix, province_mapping_df){
  #turn matrix into dataframe and join to mapping df to match districts to province
  mapped_by_province <- melt(mob_matrix) %>%
    left_join(province_mapping_df, by = c("origin" = "sub_region_clean")) %>%
    rename(origin_province = province) %>%
    left_join(province_mapping_df, by = c("destination" = "sub_region_clean")) %>%
    rename(destination_province = province)

  #arrange origin by province
  origin_levels <- mapped_by_province %>%
    distinct(origin, origin_province) %>%
    arrange(origin_province, origin) %>%
    pull(origin)

  #arrange destination by province
  destination_levels <- mapped_by_province %>%
    distinct(destination, destination_province) %>%
    arrange(destination_province, destination) %>%
    pull(destination)

  #factor both origin and destination
  mapped_by_province$origin <- factor(mapped_by_province$origin, levels = origin_levels)
  mapped_by_province$destination <- factor(mapped_by_province$destination, levels = destination_levels)

  # create breaks where each province ends (y = origin, x = destination)
  origin_breaks <- mapped_by_province %>%
    distinct(origin, origin_province) %>%
    count(origin_province, name = "n") %>%
    mutate(pos = cumsum(n) + 0.5)

  destination_breaks <- mapped_by_province %>%
    distinct(destination, destination_province) %>%
    count(destination_province, name = "n") %>%
    mutate(pos = cumsum(n) + 0.5)

  destination_labels <- mapped_by_province %>%
    distinct(destination, destination_province) %>%
    count(destination_province) %>%
    mutate(pos = cumsum(n) - n / 2)

  origin_labels <- mapped_by_province %>%
    distinct(origin, origin_province) %>%
    count(origin_province) %>%
    mutate(pos = cumsum(n) - n / 2)

  ggplot(mapped_by_province) +
    geom_tile(aes(x = destination, y = origin, fill = (value))) +

    # Add province separation lines
    geom_vline(xintercept = destination_breaks$pos, color = "white", linewidth = 0.4) +
    geom_hline(yintercept = origin_breaks$pos, color = "white", linewidth = 0.4) +

    # Add province labels using numeric axis positions
    annotate("text",
             x = destination_labels$pos,
             y = rep(0.01, nrow(destination_labels)),  # just below the plot area
             label = destination_labels$destination_province,
             angle = 90, hjust = 1, size = 4) +

    annotate("text",
             y = origin_labels$pos,
             x = rep(0.11, nrow(origin_labels)),  # just to the left
             label = origin_labels$origin_province,
             hjust = 1, size = 4) +

    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_fill_gradientn(colors = my_cont_palette(256),
                         trans = "log",
                         limits = c(1, 10000000),
                         breaks = c(1, 10, 1000, 100000, 1000000),
                         na.value = "gray88",
                         labels = c("1", "10", "1e3", "1e5", "1e6")) +
    coord_cartesian(clip = "off") +  # key to allow labels outside plot
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      axis.title.x = element_text(margin = margin(t = 90), face = "bold"),
      axis.title.y = element_text(margin = margin(r = 90), face = "bold"),
      legend.position = "right",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 2)
    ) +
    labs(x = "Destination", y = "Origin")
}

upper_to_title <- function(x){
  paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, nchar(x))))}

may_to_may_obs_grouped$origin_clean <- sapply(may_to_may_obs_grouped$origin_clean,
                                              upper_to_title)
may_to_may_obs_grouped$destination_clean <- sapply(may_to_may_obs_grouped$destination_clean,
                                              upper_to_title)

district_province_mapping$province <- sapply(district_province_mapping$province,
                                              upper_to_title)
district_province_mapping$sub_region_clean <- sapply(district_province_mapping$sub_region_clean,
                                                   upper_to_title)
### make a OD matrix using average monthly trips
avg_mob_matrix_trips <- get_mob_matrix(orig = may_to_may_obs_grouped$origin_clean,
                                       dest = may_to_may_obs_grouped$destination_clean,
                                       value = may_to_may_obs_grouped$avg_trips)


### make plot using this OD matrix
avg_mob_matrix_trips_plot <- plot_mob_matrix_by_province(avg_mob_matrix_trips, district_province_mapping)
avg_mob_matrix_trips_plot

ggsave("~/Desktop/avg_mob_matrix_trips_plot.png", avg_mob_matrix_trips_plot, width = 8, height = 7)


state_to_county_map <- state_to_county_map %>%
  mutate(start_state = abb_to_name[start_state])


plot_mob_matrix_by_state <- function(mob_matrix){
  library(dplyr)
  library(ggplot2)
  library(reshape2)
  library(viridis)

  # mapping abbrev → full
  state_abb <- c(state.abb, "DC")
  state_name <- c(state.name, "Washington, DC")
  abb_to_name <- setNames(state_name, state_abb)

  mapped_by_state <- melt(mob_matrix) %>%
    left_join(state_to_county_map, by = c("origin" = "start_county_full")) %>%
    rename(origin_state = start_state) %>%
    left_join(state_to_county_map, by = c("destination" = "start_county_full")) %>%
    rename(destination_state = start_state) %>%
    mutate(
      origin_state = abb_to_name[origin_state],
      destination_state = abb_to_name[destination_state]
    )

  # custom order, but in full names
  custom_state_order <- c("New York", "New Jersey", "Pennsylvania",
                          "Delaware", "Maryland", "Washington, DC",
                          "Virginia", "West Virginia")

  mapped_by_state <- mapped_by_state %>%
    mutate(
      origin_state = factor(origin_state, levels = custom_state_order),
      destination_state = factor(destination_state, levels = custom_state_order)
    )

  # build factor levels for rows/cols
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

  # breaks for grid lines + labels
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
    mutate(
      pos = cumsum(n) - n / 2
    )

  origin_labels <- mapped_by_state %>%
    distinct(origin, origin_state) %>%
    count(origin_state) %>%
    mutate(
      pos = cumsum(n) - n / 2
    )

  ggplot(mapped_by_state) +
    geom_tile(aes(x = destination, y = origin, fill = value)) +
    geom_vline(xintercept = destination_breaks$pos, color = "white", linewidth = 0.4) +
    geom_hline(yintercept = origin_breaks$pos, color = "white", linewidth = 0.4) +
    annotate("text",
             x = destination_labels$pos,
             y = rep(0.01, nrow(destination_labels)),
             label = destination_labels$destination_state,
             angle = 90, hjust = 1, size = 4) +
    annotate("text",
             y = origin_labels$pos,
             x = rep(0.11, nrow(origin_labels)),
             label = origin_labels$origin_state,
             hjust = 1, size = 4) +
    scale_x_discrete(limits = destination_levels, expand = c(0, 0)) +
    scale_y_discrete(limits = origin_levels, expand = c(0, 0)) +
    scale_fill_gradientn(colors = my_cont_palette(256),
                         trans = "log",
                         limits = c(0.01, 12000),
                         breaks = c(0.1, 1, 10, 100, 1000, 10000),
                         labels = c("<0.5", "1", "10", "100", "1e3", "1e4"),
                         na.value = "gray88") +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      axis.title.x = element_text(margin = margin(t = 90), face = "bold"),
      axis.title.y = element_text(margin = margin(r = 90), face = "bold"),
      legend.position = "right",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      plot.title = element_text(size = 2)
    ) +
    labs(x = "Destination", y = "Origin")
}


### make plot using this OD matrix
avg_mob_county_US_trips_plot <- plot_mob_matrix_by_state(mid_atl_OD_county_mob_matrix)
avg_mob_county_US_trips_plot

ggsave("~/Desktop/avg_mob_county_US_trips_plot.png", avg_mob_county_US_trips_plot, width = 8, height = 7)






####################
######### plot figure 2 with country outlines
####################

zambia_mh_map <- ggplot() +
  geom_sf(data = africa, fill = "NA", color = "gray70", size = 0.1) +
  geom_sf(data = zambia, color = "gray90", size = 0.1) +
  geom_sf(data = zambia_districts_mh, aes(fill = perc_move_clean), color = "white", size = 0.1) +
  geom_text(data = neighbor_labels,
            aes(x = lon, y = lat, label = admin),
            size = 3, color = "black") +
  scale_fill_gradientn(colors = my_cont_palette(1000),
                       limits = c(0,100),
                       na.value = "gray90", name = "Percent of people\nthat move",
                       breaks = c(0, 25, 50, 75, 100),
                       labels = c(0, 25, 50, 75, 100)) +
  coord_sf(xlim = c(20, 35), ylim = c(-19.5, -7), expand = FALSE) +
  theme_minimal(base_size = 10) + theme(axis.text = element_blank(),
                                        axis.ticks = element_blank(),
                                        axis.title = element_blank(),
                                        panel.grid = element_blank(),
                                        legend.title = element_blank(),
                                        legend.position = "none",
                                        legend.text = element_text(size = 8))
zambia_mh_map

ggsave("~/Desktop/zambia_mh_map.png", zambia_mh_map, width = 9, height = 5.5)


zambia_move_het_hisogram <- zambia_districts_mh %>%
  ggplot() +
  geom_histogram(aes(x = perc_move_clean, fill = after_stat(x)), color = "white", binwidth = 4) +
  xlim(0, 101) +
  scale_fill_gradientn(colors = my_cont_palette(256),
                       limits = c(0,101), name = "Percent of people\nthat move",
                       breaks = c(0, 25, 50, 75, 100),
                       labels = c(0, 25, 50, 75, 100)) +
  labs(x = "Percent of people in district that move",
       y = "Count") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        panel.grid  = element_blank())
zambia_move_het_hisogram

ggsave("~/Desktop/zambia_move_het_hisogram.png", zambia_move_het_hisogram, width = 7, height = 5.5)


move_het_map_plot <- move_het_map_plot_data %>%
  filter(!STATEFP %in% c("02", "15", "72")) %>%  # Remove AK (02), HI (15), PR (72)
  ggplot() +
  geom_sf(aes(fill = perc_movers), color = NA) +
  geom_sf(data = state_outline_subset, fill = NA, color = "#E20134", linewidth = 0.5) +
  geom_sf(data = us_border_countries, fill = NA, color = "gray70", linewidth = 0.3) +
  geom_text(data = us_neighbor_labels,
            aes(x = lon, y = lat, label = admin),
            size = 3, color = "black") +
  scale_fill_gradientn(colors = my_cont_palette(256),
                       limits = c(0,100),
                       na.value = "gray90", name = "Percent of people\nthat move",
                       breaks = c(0, 25, 50, 75, 100),
                       labels = c(0, 25, 50, 75, 100)) +
  coord_sf(xlim = c(-135, -68), ylim = c(24, 49.4)) +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(hjust = 1))
move_het_map_plot

ggsave("~/Desktop/move_het_map_plot.png", move_het_map_plot, width = 9, height = 5.5)


state_outline <- states(cb = TRUE, resolution = "20m", year = 2020) %>%
  filter(!STATEFP %in% c("02", "15", "72"))  # Exclude AK, HI, PR

mid_atl_outline <- state_outline %>%
  filter(STUSPS %in% c("MD", "VA", "DC", "PA", "NY", "DE", "WV", "NJ")) %>%
  st_union() %>%
  st_as_sf()

mid_atl_move_het_plot <- move_het_map_plot_data %>%
  filter(state_abbr %in% c("MD", "VA", "DC", "PA", "NY", "DE", "WV", "NJ")) %>%  # Remove AK (02), HI (15), PR (72)
  ggplot() +
  geom_sf(aes(fill = perc_movers), color = NA) +
  geom_sf(data = filter(state_outline, STUSPS %in% c("MD", "VA", "DC", "PA", "NY", "DE", "WV", "NJ")),
          fill = "NA", color = "gray90", linewidth = 0.3) +
  geom_sf(data = mid_atl_outline, fill = NA, color = "#E20134", linewidth = 1.5) +
  scale_fill_gradientn(colors = my_cont_palette(256),
                       limits = c(0,100),
                       na.value = "gray90", name = "Percent of people\nthat move",
                       breaks = c(0, 25, 50, 75, 100),
                       labels = c(0, 25, 50, 75, 100)) +
  theme_void() +
  theme(legend.position = "none")
mid_atl_move_het_plot

ggsave("~/Desktop/mid_atl_move_het_plot.png", mid_atl_move_het_plot, width = 9, height = 5.5)



us_move_het_hisogram <- move_het_map_plot_data %>%
  ggplot() +
  geom_histogram(aes(x = perc_movers, fill = after_stat(x)), color = "white", binwidth = 4) +
  scale_fill_gradientn(colors = my_cont_palette(256),
                       limits = c(0,100), name = "Percent of people\nthat move",
                       breaks = c(0, 25, 50, 75, 100),
                       labels = c(0, 25, 50, 75, 100)) +
  labs(x = "Percent of people in county that move",
       y = "Count") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        panel.grid  = element_blank())
us_move_het_hisogram

ggsave("~/Desktop/us_move_het_hisogram.png", us_move_het_hisogram, width = 7, height = 5.5)


legend_plot <- move_het_map_plot_data %>%
  filter(!STATEFP %in% c("02", "15", "72")) %>%  # Remove AK (02), HI (15), PR (72)
  ggplot() +
  geom_sf(aes(fill = perc_movers), color = NA) +
  scale_fill_gradientn(colors = my_cont_palette(256),
                       limits = c(0,100),
                       na.value = "gray90", name = "Percent of people\nthat move",
                       breaks = c(0, 25, 50, 75, 100),
                       labels = c(0, 25, 50, 75, 100)) +
  theme(legend.direction = "horizontal",
        legend.title.position = "top",
        legend.title = element_text(hjust= 0.5))

# Extract legend
legend <- get_legend(legend_plot)

# Display only the legend
plot(legend)
ggsave("~/Desktop/mh_legend.jpeg", legend, width = 2, height = 1)



dotplot_move_het_month_daily <- ggplot(data = move_het_month_daily_results_combined_with_zam,
                                       aes(x = estimate, y = var, color = model, group = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.5) +
  geom_point(position = position_dodge(width = 0.3), size = 3.5) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                width = 0.2,
                position = position_dodge(width = 0.3)) +
  theme_minimal(base_size = 14) +
  theme(axis.text.y = element_text(hjust = 0.5),
        legend.position = "top") +
  guides(color = guide_legend(title = NULL)) +
  scale_color_manual(values = c('Month_daily_county' = "#8400CD",
                                'Month_daily_ct' = "#008DF9",
                                'Zambia' = "#FF6E3A"),
                      labels = c("US county", "US census tract", 'Zambian districts')) +
  ylab("Variable") +
  xlab("Estimate") +
  theme(axis.text.y = element_text(size = 16),
        legend.text = element_text(size = 16))
dotplot_move_het_month_daily

ggsave("~/Desktop/dotplot_move_het_month_daily.png", dotplot_move_het_month_daily, width = 15, height = 5.5)


##############
##############
##### supplement showing monthly users normalized by population
##############
##############

##### bring in country demographics
county_dems <- read.csv("~/Downloads/USA_2020_Census_Population_Characteristics/county_2.csv")

county_dems <- county_dems %>% mutate(clean_county_code = ifelse(Geographic.Identifier < 10000,
                                                                 paste0("0", as.character(Geographic.Identifier)),
                                                                 as.character(Geographic.Identifier)),
                                      Land_area_sqkm = Area.of.Land..Square.Meters. / 1000000) %>%
  dplyr::select(Total.Population, clean_county_code)

monthly_users_map_data_with_pop <- left_join(monthly_users_map_data, county_dems,
                                             by = join_by(GEOID == clean_county_code))

monthly_users_map_data_with_pop <- monthly_users_map_data_with_pop %>%
  mutate(mean_monthly_users_normalized = mean_monthly_users / Total.Population)



monthly_users_normalized_plot <- monthly_users_map_data_with_pop %>%
  filter(!STATEFP %in% c("02", "15", "72")) %>%
  ggplot() +
  geom_sf(aes(fill = mean_monthly_users_normalized), color = NA) +
  geom_sf(data = state_outline_subset, fill = NA, color = "#E20134", linewidth = 0.5) +
  geom_sf(data = us_border_countries, fill = NA, color = "gray70", linewidth = 0.3) +
  geom_text(data = us_neighbor_labels,
            aes(x = lon, y = lat, label = admin),
            size = 3, color = "black") +
  scale_fill_gradientn(colors = my_cont_palette(256),
                       limits = c(0, 1),
                       na.value = "black", name = "Log average \nmonthly users") +
  coord_sf(xlim = c(-135, -68), ylim = c(24, 49.4)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.margin = margin(t = 0, r = 0, b = 0, l = 40)
  ) +
  annotation_scale(location = "br", width_hint = 0.2, pad_x = unit(5, "cm"))  # scale bar bottom right

monthly_users_normalized_plot

ggsave("~/Desktop/us_monthly_users_normalized_plot.png", monthly_users_normalized_plot, width = 10, height = 7)

ak_monthly_users_normalized_plot <- monthly_users_map_data_with_pop %>%
  filter(STATEFP == "02") %>%  # Alaska only
  ggplot() +
  geom_sf(aes(fill = mean_monthly_users_normalized), color = NA) +
  scale_fill_gradientn(colors = my_cont_palette(256),
                       limits = c(0,1),
                       na.value = "black", name = "Log average \nmonthly users") +
  coord_sf(
    xlim = c(-180, -130),  # tighter longitude limits
    ylim = c(50, 72),      # tighter latitude limits
    expand = FALSE
  ) +
  theme_void()
ak_monthly_users_normalized_plot

# Hawaii inset
hi_monthly_users_normalized_plot <- monthly_users_map_data_with_pop %>%
  filter(STATEFP == "15") %>%  # Hawaii only
  ggplot() +
  geom_sf(aes(fill = mean_monthly_users_normalized), color = NA) +
  scale_fill_gradientn(colors = my_cont_palette(256),
                       limits = c(0,1),
                       na.value = "black", name = "Log average \nmonthly users") +
  theme_void()
hi_monthly_users_normalized_plot

ggsave("~/Desktop/ak_monthly_users_normalized_plot.png", ak_monthly_users_normalized_plot, width = 10, height = 7)
ggsave("~/Desktop/hi_monthly_users_normalized_plot.png", hi_monthly_users_normalized_plot, width = 10, height = 7)



##########
### now for Zambia
##########

zambia_monthly_users <- merge(zambia_districts, users_per_loc_avg, by.x = "DISTRICT", by.y = "district", all.x = T)

clean_zambia_pop <- pop.and.dist %>% group_by(origin) %>% slice(1) %>%
  mutate(population = origin_pop_per100k * 100000)

zambia_monthly_users_with_pop <- left_join(zambia_monthly_users, clean_zambia_pop,
                                           by = join_by("DISTRICT" == "origin"))

zambia_monthly_users_with_pop <- zambia_monthly_users_with_pop %>%
  mutate(mean_monthly_users_normalized = avg_monthly_users / population)





# plot map of included districts
zambia_monthly_users_normalized_map <- ggplot() +
  geom_sf(data = africa, fill = "NA", color = "gray70", size = 0.1) +
  geom_sf(data = zambia, color = "gray90", size = 0.1) +
  geom_sf(data = zambia_monthly_users_with_pop, aes(fill = mean_monthly_users_normalized), color = "white", size = 0.1) +
  geom_text(data = neighbor_labels,
            aes(x = lon, y = lat, label = admin),
            size = 3, color = "black") +
  scale_fill_gradientn(colors = my_cont_palette(256),
                       limits = c(0,1),
                       na.value = "gray90", name = "Average monthly users\nnormalized by population",
                       breaks = c(0, 0.25, 0.5, 0.75, 1),
                       labels = c(0, 0.25, 0.5, 0.75, 1)) +
  annotation_scale(location = "tr", width_hint = 0.3, text_cex = 0.6, pad_y = unit(0.8, "cm"),
                   pad_x = unit(0, "cm")) +
  coord_sf(xlim = c(20, 35), ylim = c(-19.5, -7), expand = FALSE) +
  theme_minimal(base_size = 10) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = c(0.7, 0.08),
        legend.direction = "horizontal",
        legend.title = element_text(size = 12, vjust = 1, hjust = 0.5),
        legend.text = element_text(size = 12),
        legend.justification = "center",
        legend.box.margin = margin(t = 20),
        plot.margin = margin(t = 0)
  ) + guides(fill = guide_colourbar(barwidth = 9, barheight = 1.6))
zambia_monthly_users_normalized_map

ggsave("~/Desktop/zambia_monthly_users_normalized_map.png", zambia_monthly_users_normalized_map, width = 10, height = 6)
