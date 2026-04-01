##### load in packages

library(dplyr)
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


##############
##############
####### figure 1
##############
##############

###load in necessary objects

## created in Zambia/manuscript_code/descriptive.R in server
sbd <- readRDS("~/Documents/Mobility/Zambia/Cleaning_modeling_code/sbd.rds")
sbdfilt <- readRDS("~/Documents/Mobility/Zambia/Cleaning_modeling_code/sbdfilt.rds")


### plot subscribers per day before filtering
sbdplot <- ggplot(sbd, aes(x = Call_DayA, y = individuals, color = individuals)) +
  geom_point() +
  geom_line() +
  ylim(0, 1200000) +
  xlab("Date") +
  ylab("Individuals") +
  theme_classic() +
  scale_color_viridis(option = "F", direction = -1) +
  theme(legend.position = "none")
sbdplot

### plot subscribers per day after filtering
sbdfiltplot <- ggplot(sbdfilt, aes(x = Call_DayA, y = individuals, color = individuals)) +
  geom_point() +
  geom_line() +
  ylim(0, 1200000) +
  xlab("Date") +
  ylab("Individuals") +
  theme_classic() +
  scale_color_viridis(option = "F", direction = -1) +
  theme(legend.position = "none")
sbdfiltplot

### plot both together
sbdcombinedplot <- ggplot() +
  geom_point(data = sbd, aes(x = Call_DayA, y = individuals, color = "All Individuals"), size = 0.5) +
  geom_line(data = sbd, aes(x = Call_DayA, y = individuals, color = "All Individuals"), linewidth = 0.25) +
  geom_point(data = sbdfilt, aes(x = Call_DayA, y = individuals, color = "Individuals With >=14 Days of Data/Month"), size = 0.5) +
  geom_line(data = sbdfilt, aes(x = Call_DayA, y = individuals, color = "Individuals With >=14 Days of Data/Month"), linewidth = 0.25)  +
  scale_color_manual(values = c("All Individuals" = "#2D708EFF", "Individuals With >=14 Days of Data/Month" = "#94D840FF")) +
  theme_minimal(base_size = 10) +
  ylim(400000, 1200000) +
  xlab("Date") +
  ylab("Individuals") +
  guides(color = guide_legend(ncol = 1, title = NULL)) +
  theme(legend.title = element_blank(),
        legend.position = "top",
        legend.box = "vertical",
        legend.text = element_text(size = 8))
sbdcombinedplot

#######
### make the included district plots
######


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

# Define the districts to highlight as included
highlight_districts <- unique(mta_s2_clean$origin_clean)

# Add a column to indicate highlight status
zambia_districts <- zambia_districts %>%
  mutate(highlight = ifelse(DISTRICT %in% highlight_districts, "highlight", "normal"))

zambia_districts <- zambia_districts %>%
  mutate(highlight = case_when(
    DISTRICT %in% highlight_districts ~ "Included",
    TRUE ~ "No subscribers"
  ))


# plot map of included districts
quartz()
zambia_included_map <- ggplot(data = zambia_districts) +
  geom_sf(aes(fill = highlight), color = "white", size = 0.1) +
  scale_fill_manual(values = c("Included" = "#2D708EFF", "No subscribers" = "gray80")) +
  theme_minimal(base_size = 10) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 8))
zambia_included_map

figure1 <- grid.arrange(zambia_included_map, sbdcombinedplot,
                        layout_matrix = cbind(c(1), c(2)),
                        widths = c(1, 1))

fig1_layout <- "
  12
  34
"

figure1 <- free(zambia_included_map) + free(sbdcombinedplot) + plot_spacer() + plot_spacer() +
  plot_layout(design = fig1_layout, widths = c(1, 1),
              heights = c(1,0.8)) +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = "bold")) +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
figure1



##################
##################
####### figure 2
##################
##################

###load in necessary objects

## created in Zambia/manuscript_code/descriptive.R in server
load("~/Documents/Mobility/Zambia/Cleaning_modeling_code/Saved_objects/may_2024_s2_clean")
load("~/Documents/Mobility/Zambia/Cleaning_modeling_code/Saved_objects/mta_s2_clean")

### Join centroids of districts
zambia_districts_centroids <- zambia_districts %>%
  st_centroid() %>%
  dplyr::select(DISTRICT, geometry)


### bring together OD pairs in both the training (mta) and testing (may_2024) sets
may_to_may_obs <- bind_rows(may_2024_s2_clean, mta_s2_clean) %>% dplyr::select(origin_clean, destination_clean, grouped.total, prop_of_origin_travel)

### find the average number of trips per month and average proportion of origin travel per month for each OD pair
may_to_may_obs_grouped <- may_to_may_obs %>% group_by(origin_clean, destination_clean) %>% summarize(avg_trips = mean(grouped.total, na.rm = T),
                                                                                                     avg_poot = mean(prop_of_origin_travel, na.r = T))

### Join origin/destination pairs with centroid coordinates
may_2024_geo <- may_to_may_obs_grouped %>%
  left_join(zambia_districts_centroids, by = c("origin_clean" = "DISTRICT")) %>%
  rename(origin_geom = geometry) %>%
  left_join(zambia_districts_centroids, by = c("destination_clean" = "DISTRICT")) %>%
  rename(dest_geom = geometry)

### make sure geometry is in correct format (coordinates). Also filter so that no intra-district travel is included
### also filter for OD pairs with an average of more than 20 trips so the plot is not too busy
may_2024_geo <- may_2024_geo %>%
  mutate(
    origin_lon = st_coordinates(origin_geom)[,1],
    origin_lat = st_coordinates(origin_geom)[,2],
    dest_lon   = st_coordinates(dest_geom)[,1],
    dest_lat   = st_coordinates(dest_geom)[,2]
  ) %>%
  filter(
    !is.na(origin_lon) & !is.na(origin_lat) &
      !is.na(dest_lon) & !is.na(dest_lat) &
      (origin_lon != dest_lon | origin_lat != dest_lat)
  ) %>%
  filter(avg_trips >= 20)


### plot showing travel between ODs
connect_map <- ggplot() +
  # Optional: show district boundaries
  geom_sf(data = zambia_districts, fill = "gray95", color = "white", size = 0.2) +

  # Curved arrows between centroids
  geom_curve(
    data = may_2024_geo,
    aes(x = origin_lon, y = origin_lat,
        xend = dest_lon, yend = dest_lat,
        size = log2(avg_trips),
        alpha = log2(avg_trips)),
    color = "mediumorchid",
    curvature = 0.25,
    arrow = arrow(length = unit(0.1, "cm"), type = "closed")
  ) +

  # Adjust visuals
  scale_size(range = c(0.05, 0.5)) +
  scale_alpha(range = c(0.1, 0.5)) +
  coord_sf() +
  theme_void() +
  theme(legend.position = "top") +
  labs(
    size = "Number of monthly trips\n(log2)",
    alpha = "Number of monthly trips\n(log2)"
  ) +
  theme(legend.title = element_text(size = 8, hjust = 0.5),
        legend.text = element_text(size = 8))
connect_map



#### suppplemental figure 1
supp_fig1 <- sbdcombinedplot + connect_map +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = "bold"))
supp_fig1

###########
#### bring in the raw May, 2024 data
##########
may.2024 <- read.csv("~/Documents/Mobility/Zambia/Updated_CDR/Updated_May_2024.csv")

### take one unique instance of each district
may.2024.unqiue.districts <- may.2024[!duplicated(may.2024$sub_region),]

### create a district to province mapping dataframe
district_province_mapping <- may.2024.unqiue.districts %>% dplyr::select(province, sub_region) %>%
  mutate(
    sub_region_clean = recode(sub_region,
                              KAPIRI = "KAPIRI MPOSHI",
                              SHANGOMBO = "SHANG'OMBO",
                              MILENGE = "MILENGI",
                              "ITHEZI-TEZHI" = "ITEZHI-TEZHI",
                              MALOLE = "MUNGWI",
                              MUYOMBE = "ISOKA"))

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
             angle = 90, hjust = 1, size = 1.2) +

    annotate("text",
             y = origin_labels$pos,
             x = rep(0.11, nrow(origin_labels)),  # just to the left
             label = origin_labels$origin_province,
             hjust = 1, size = 1.2) +

    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_fill_viridis(option = 'D', trans = "log",
                       limits = c(1, 10000000),
                       breaks = c(1, 10, 1000, 100000, 1000000),
                       labels = c("1", "10", "1e3", "1e5", "1e6")) +
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


### make a OD matrix using average monthly trips
avg_mob_matrix_trips <- get_mob_matrix(orig = may_to_may_obs_grouped$origin_clean,
                                       dest = may_to_may_obs_grouped$destination_clean,
                                       value = may_to_may_obs_grouped$avg_trips)

### make plot using this OD matrix
avg_mob_matrix_trips_plot <- plot_mob_matrix_by_province(avg_mob_matrix_trips, district_province_mapping)
avg_mob_matrix_trips_plot

### make a OD matrix using average monthly proportion of origin travel
avg_mob_matrix_poot <- get_mob_matrix(orig = may_to_may_obs_grouped$origin_clean,
                                 dest = may_to_may_obs_grouped$destination_clean,
                                 value = may_to_may_obs_grouped$avg_poot)
### make plot using this OD matrix
avg_mob_matrix_poot_plot <- plot_mob_matrix_by_province(avg_mob_matrix_poot, district_province_mapping)


figure2 <- grid.arrange(connect_map, avg_mob_matrix_trips_plot, avg_mob_matrix_poot_plot,
                        layout_matrix = cbind(c(1, 1), c(2, 3)),
                        heights = c(1,1),
                        widths = c(1.5, 1))

fig2_layout <- "
  12
  13
"

quartz()
figure2 <- free(connect_map) + free(avg_mob_matrix_trips_plot) + free(avg_mob_matrix_poot_plot) +
  plot_layout(design = fig2_layout, widths = c(1.5, 1),
              heights = c(1.2,1)) +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = "bold"))  +
  theme(plot.margin = unit(c(0.1, 0.1, 0.2, 0.1), "cm"))
figure2




############
######## combining figures 1 and 2
############

### bring in objects from Cuebiq_data.R
mid_atl_OD_county_mob_matrix
monthly_users_map_data

## lower 48 map of monthly users
monthly_users_plot <- monthly_users_map_data %>%
  filter(!STATEFP %in% c("02", "15", "72")) %>%  # Remove AK (02), HI (15), PR (72)
  ggplot() +
  geom_sf(aes(fill = mean_monthly_users_log), color = NA) +
  geom_sf(data = state_outline_subset, fill = NA, color = "red", linewidth = 0.5) +  # Red outline
  scale_fill_viridis_c(na.value = "gray90", name = "Log average \nmonthly users") +
  coord_sf(
    xlim = c(-125, -66),  # longitude bounds (approx. mainland US)
    ylim = c(24, 50)      # latitude bounds (approx. mainland US)
  ) +
  theme_void() +
  theme(legend.position = "top",
        plot.margin = margin(t = 0, r = 0, b = 0, l = 40)
  )
monthly_users_plot

# Alaska inset
ak_monthly_users_plot <- monthly_users_map_data %>%
  filter(STATEFP == "02") %>%  # Alaska only
  ggplot() +
  geom_sf(aes(fill = mean_monthly_users_log), color = NA) +
  scale_fill_viridis_c(na.value = "gray90", guide = "none") +
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
  scale_fill_viridis_c(na.value = "gray90", guide = "none") +
  theme_void()
hi_monthly_users_plot

monthly_users_total_map <- ggdraw() +
  draw_plot(monthly_users_plot, 0, 0, 1, 1) +                        # Full base
  draw_plot(ak_monthly_users_plot, x = 0.02, y = 0.75, width = 0.2, height = 0.2) +  # AK inset
  draw_plot(hi_monthly_users_plot, x = 0.05, y = 0.2, width = 0.15, height = 0.15) +

  # Add arrow from state area (e.g., NY) to matrix region
  draw_line(
    x = c(0.84, 0.87),
    y = c(0.50, 0.35),
    arrow = arrow(length = unit(0.02, "npc")),
    color = "red",
    linewidth = 0.8)

### OD matrix for Cuebiq counties in mid atlantic states
avg_mob_county_US_trips_plot <- plot_mob_matrix_by_state(mid_atl_OD_county_mob_matrix)
avg_mob_county_US_trips_plot


fig1_layout <- "
  12
  34
"

figure1 <- free(zambia_included_map) + free(sbdcombinedplot) + plot_spacer() + plot_spacer() +
  plot_layout(design = fig1_layout, widths = c(1, 1),
              heights = c(1,0.8)) +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = "bold")) +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
figure1


fig2_layout <- "
  12
  13
"

quartz()
figure2 <- free(connect_map) + free(avg_mob_matrix_trips_plot) + free(avg_mob_matrix_poot_plot) +
  plot_layout(design = fig2_layout, widths = c(1.5, 1),
              heights = c(1.2,1)) +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = "bold"))  +
  theme(plot.margin = unit(c(0.1, 0.1, 0.2, 0.1), "cm"))
figure2



big_fig1_layout <- "
  123
  456
"
big_figure1 <- free(zambia_included_map) + free(connect_map) +
  free(monthly_users_total_map) + free(sbdcombinedplot) +
  free(avg_mob_matrix_trips_plot) + free(avg_mob_county_US_trips_plot) +
  plot_layout(design = big_fig1_layout, widths = c(1, 1.5, 1.5),
              heights = c(1,1)) +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = "bold"))
big_figure1

big_fig1_layout <- "
  12
  34
"
big_figure1 <- free(zambia_included_map) + free(sbdcombinedplot) +
  free(connect_map) + free(avg_mob_matrix_trips_plot) +
  free(monthly_users_total_map) + free(avg_mob_county_US_trips_plot) +
  plot_layout(design = big_fig1_layout, widths = c(0.32, 0.25),
              heights = c(1,1,1)) +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = "bold"))
big_figure1

ggsave(big_figure1, filename = "~/Desktop/test_fig1.png", width = 9, height = 10, units = "in")


quartz()
big_figure1 <- free(zambia_monthly_users_map) + (avg_mob_matrix_trips_plot) +
  free(monthly_users_total_map) + (avg_mob_county_US_trips_plot) +
  plot_layout(design = big_fig1_layout, widths = c(2, 1),
              heights = c(1,1)) +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = "bold"))
big_figure1




##############
##############
####### figure 3
##############
##############


#### take negative binomial only prediction models from Prediction_models.R
has.het.mod.results <- tidy(has_het_mod) |> filter(term != "(Intercept)") |> mutate(model = "Heterogeneity Indicator")
negbi.size.mod.results <- tidy(negbi.size.mod) |> filter(term != "(Intercept)") |> mutate(model = "NegBi Size")
negbi.mu.mod.results <- tidy(negbi.mu.mod) |> filter(term != "(Intercept)") |> mutate(model = "NegBi Mu")

### combine all three model results
prediction_results <- rbind(has.het.mod.results, negbi.size.mod.results, negbi.mu.mod.results)

prediction_results

## plot prediction model results
prediction_results_plot <- dwplot(prediction_results, dot_args = list(size = 3),
       model_order = c("Heterogeneity Indicator", "NegBi Size", "NegBi Mu")) %>%
  relabel_predictors(c("log(grouped.total)" = "Aggregate Trip Count",
                       "scaled_distance_km" = "Distance (km)",
                       "scaled_destination_pop_per100k" = "Destination Population (100k)",
                       "scaled_origin_pop_per100k" = "Origin Population (100k)",
                       "scaled_prop_of_origin_travel" = "Proportion of Origin Travel")) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.5) +
  annotate(geom = "text", size = 4, label = "Zamtel/Zambia", x = 1.25, y = 4.2) +
  scale_color_manual(values = c("Heterogeneity Indicator" = "#482677FF",
                                "NegBi Size" = "#29AF7FFF", "NegBi Mu" = "#B8DE29FF")) +

  theme_minimal(base_size = 12) +
  theme(legend.title = element_blank(),
        legend.position = "top",
        plot.title = element_text(hjust = 0),
        panel.grid.minor = element_blank()) +
  scale_x_continuous(limits = c(-1.5, 2), breaks = c(-1.5, -1, -0.5, 0.0, 0.5, 1.0, 1.5))
prediction_results_plot

library(forcats)

# Your renaming and ordering
term_labels <- c(
  "log(grouped.total)" = "Aggregate Trip Count",
  "scaled_distance_km" = "Distance (km)",
  "scaled_destination_pop_per100k" = "Destination Population (100k)",
  "scaled_origin_pop_per100k" = "Origin Population (100k)",
  "scaled_prop_of_origin_travel" = "Proportion of Origin Travel"
)

term_order <- rev(unname(term_labels))  # keep the order you want

# Apply recoding and reordering
prediction_results <- prediction_results %>%
  mutate(
    term = recode(term, !!!term_labels),
    term = fct_relevel(term, !!!term_order),
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )


# Basic coefficient plot
pred_new <- ggplot(prediction_results, aes(x = estimate, y = term, color = model)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.5),
                 height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
  annotate(geom = "text", size = 4, label = "Zamtel/Zambia", x = 1.25, y = 4.2) +
  scale_color_viridis_d(option = "B", begin = 0.3, end = 0.7) +
  labs(x = "Coefficient Estimate", y = NULL, color = NULL) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "top",
    axis.text.y = element_text(hjust = 0.5),
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )  +
  scale_x_continuous(limits = c(-1.5, 2), breaks = c(-1.5, -1, -0.5, 0.0, 0.5, 1.0, 1.5))
prediction_results_plot



##### prediction error table using predictions.errors.may.2024 from main script
predictions.errors.may.2024_plot <- as.ggplot(tableGrob(predictions.errors.may.2024,
                                              cols = c("Model", "Mean KL Divergence", "Mean Wasserstein Distance"),
                                              theme = ttheme_default(base_size = 14),
                                              rows = c()))
predictions.errors.may.2024_plot




#############
#############

#### using may.2024.just.negbi.preds from main script, add in indicator for presence of travel heterogeneity
may.2024.just.negbi.preds <- may.2024.just.negbi.preds %>% mutate(has_het = ifelse(percentage_ones == 100, 0, 1))

# make table to see specificity and sensitivity of prediction model for presence of travel heterogeneity
table(pred = may.2024.just.negbi.preds$has_het_pred, real = may.2024.just.negbi.preds$has_het)

### make dataframes for observed no travel het group and observed travel het group for plotting
real.no_het <- tibble(group = c("Predicted Heterogeneity", "Predicted No Heterogeneity"),
                    value = c(220, 1814),
                    label = c("220", "1814 (89.2%)"))

real.het <- tibble(group = c("Predicted Heterogeneity", "Predicted No Heterogeneity"),
                   value = c(1079, 153),
                   label = c("1079 (87.6%)", "153"))

#factor the group variable
real.no_het$group <- factor(real.no_het$group, levels = c("Predicted No Heterogeneity", "Predicted Heterogeneity"))
real.het$group <- factor(real.het$group, levels = c("Predicted No Heterogeneity", "Predicted Heterogeneity"))

### plot observed no travel het group (specificity)
real.no_het.plot <- ggplot(real.no_het, aes(x = "", y = value, fill = group)) +
  geom_col(color = "black") +
  geom_text(aes(label = label), size = 6,
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_brewer() +
  theme_void(base_size = 15) +
  labs(title = "OD pairs with \n no travel heterogeneity (n = 2034)") +
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 14))
real.no_het.plot

### plot observed travel het group (sensitivity)
real.het.plot <- ggplot(real.het, aes(x = "", y = value, fill = group)) +
  geom_col(color = "black") +
  geom_text(aes(label = label), size = 6,
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_brewer() +
  theme_void(base_size = 15) +
  labs(title = "OD pairs with \n travel heterogeneity (n = 1232)") +
  theme(legend.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 14))


########
########
### people error
########
########

### with may.2024.just.negbi.preds from main.script, take only the OD pairs with travel het that were predicted to
### to have travel het by the negative binomial model
may.2024.just.negbi.preds.just.pred.het <- filter(may.2024.just.negbi.preds, has_het == 1,
                                                  has_het_pred == 1)

### add in observed negative binomial parameters to compare to predicted ones
may.2024.just.negbi.preds.with.real.params <- add_pois_negbi_dists(may.2024.just.negbi.preds.just.pred.het)

###for each OD pair in May, 2024, calculate the error of negative binomial parameters and predicted number of people
### taking trips. Also add in indicator on whether to highlight people error or not
may.2024.just.negbi.preds.with.real.params <- may.2024.just.negbi.preds.with.real.params %>% rowwise() %>%
  mutate(negbi_size_error_percent = (abs(negbi_size - pred.neg.bi.size)/negbi_size)*100,
         negbi_size_error_rel = (abs(negbi_size - pred.neg.bi.size)),
         negbi_mu_error = abs(negbi_mu - pred.neg.bi.mu)/negbi_mu,
         predicted_people = grouped.total/(1+pred.neg.bi.mu),
         people_error_percent = (abs(predicted_people - total_trips)/total_trips)*100,
         highlight = ifelse(people_error_percent <=20, "Under 20% Error", "Over 20% Error"))

### repeat with all ODs, not just observed and predicted travel het ODs
may.2024.just.negbi.preds.all <- add_pois_negbi_dists(may.2024.just.negbi.preds)
may.2024.just.negbi.preds.all <- may.2024.just.negbi.preds.all %>% rowwise() %>%
  mutate(predicted_people = ifelse(has_het_pred == 1, grouped.total/(1+pred.neg.bi.mu), grouped.total),
         people_error_percent = (abs(predicted_people - total_trips)/total_trips)*100,
         highlight = ifelse(people_error_percent <=10, "Under 10% Error", "Over 10% Error"))


### find the proportion of OD pairs with less than 20%, 10%, etc. error
sum(may.2024.just.negbi.preds.with.real.params$people_error_percent <= 10)/nrow(may.2024.just.negbi.preds.with.real.params)
sum(may.2024.just.negbi.preds.all$people_error_percent <= 20)/nrow(may.2024.just.negbi.preds.all)
mean(may.2024.just.negbi.preds.all$people_error_percent)


### plot the error in # of people for just observed and predicted travel het ODs. Highlight bars under 20% error
people_error_plot <- ggplot(data = may.2024.just.negbi.preds.with.real.params) +
  geom_histogram(aes(people_error_percent, fill = highlight, color = "white"), binwidth = 1) +
  geom_text(label = "89% of OD pairs \n under 20% error", x = 35, y = 40, size = 5) +
  theme_minimal(base_size = 16) +
  scale_fill_manual(values = c("Under 20% Error" = "#29AF7FFF")) +
  scale_color_manual(values = "white") +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13)) +
  xlab("Error between predicted number of people and actual number of people") +
  ylab("OD pairs") +
  xlim(-1, 60) +
  ylim(0, 75)
people_error_plot

### plot the error in # of people for all ODs. Highlight bars under 10% error
people_error_all_plot <- ggplot(data = may.2024.just.negbi.preds.all) +
  geom_histogram(aes(people_error_percent, fill = highlight, color = "white"), binwidth = 1) +
  geom_text(label = "89% of OD pairs \n under 10% error", x = 35, y = 500, size = 5) +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("Under 10% Error" = "#29AF7FFF")) +
  scale_color_manual(values = "white") +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13)) +
  xlab("Error between predicted number of people and actual number of people") +
  ylab("OD pairs")
people_error_all_plot


library(patchwork)
library(cowplot)

quartz()
het_pie_plot <- real.no_het.plot + real.het.plot & theme(legend.position = "bottom")
het_pie_plot <- het_pie_plot + plot_layout(guides = "collect")
het_pie_plot


legend <- get_legend(pred_new)
legend <- as.ggplot(legend)
legend



p <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  theme(legend.position = "right")

legend <- get_legend(cuebiq_new)
legend

cbind(c(1, 3), c(1,4), c(2,5))

quartz()
grid.arrange(pred_new, cuebiq_new,
             legend, real.het.plot, people_error_plot,
             layout_matrix = cbind(c(1, 3), c(1,4), c(2,5)),
             widths = c(0.6, 0.6, 1))

fig3_layout <- "
  112
  345
"
figure3 <- ((pred_new) + (cuebiq_new)) +
    ((real.no_het.plot) + (real.het.plot)) +
    (people_error_plot) +
  plot_layout(design = fig3_layout, guides = "collect", widths = c(1, 1),
              heights = c(1, 1)) +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = "bold"))
figure3


quartz()
fig3_top <- (pred_new + cuebiq_new
             + plot_layout(guides = "collect") & theme(legend.position = "bottom")) +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = "bold"))
fig3_top

fig3_bottom <- ((real.no_het.plot | real.het.plot)
             + plot_layout(guides = "collect") & theme(legend.position = "bottom")) /
  people_error_plot +
  plot_annotation(tag_levels = 'A') & theme(plot.tag = element_text(face = "bold"))
fig3_bottom


test <- ((pred_new | cuebiq_new) +
           plot_layout(guides = "collect", tag_level = 'new') & theme(legend.position = "bottom")) /
  (((real.no_het.plot | real.het.plot) +
       plot_layout(guides = "collect", tag_level = 'new') & theme(legend.position = "bottom"))) +
  plot_annotation(tag_levels = c('A', '1'))
test

#####################
####################
####### simulation results plot
####################
#####################





flu_new_infections_plot <- ggplot(flu_infections_df) +
  geom_line(aes(x = step, y = new_infections, group = sim, color = group, alpha = highlight), linewidth = 1) +
  scale_y_continuous(position = "right", breaks = pretty_breaks(3)) +
  scale_x_continuous(breaks = c(0, 100, 200, 300), limits = c(0, 300)) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.05), guide = "none") +
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
flu_new_infections_plot

covid_new_infections_plot <- ggplot(covid_infections_df) +
  geom_line(aes(x = step, y = new_infections, group = sim, color = group, alpha = highlight), linewidth = 1) +
  scale_y_continuous(position = "right", breaks = pretty_breaks(3)) +
  scale_x_continuous(breaks = c(0, 50, 100), limits = c(0, 100)) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.05), guide = "none") +
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
covid_new_infections_plot

measles_new_infections_plot <- ggplot(measles_infections_df) +
  geom_line(aes(x = step, y = new_infections, group = sim, color = group, alpha = highlight), linewidth = 1) +
  scale_y_continuous(position = "right", breaks = pretty_breaks(n= 3)) +
  scale_x_continuous(breaks = c(0, 25, 50), limits = c(0, 50)) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.05), guide = "none") +
  scale_color_viridis(discrete = T, begin = 0.1, end = 0.8, option = "B") +
  facet_grid(group ~ start_loc, switch="y",
             labeller = labeller(group = c("Move_het" = "Aggregate +\nproportion travelled",
                                           "Travel_het" = "Aggregate +\ntrip frequency",
                                           "Both_het" = "Aggregate +\nproportion travelled +\ntrip frequency"))) +
  labs(title = "Measles",
       x = "Timestep", y = "New Infections") +
  theme_minimal() +
  theme(
    strip.text.x = element_text(face = "bold.italic"),
    strip.text.y = element_blank(),
    legend.position = "none",
    plot.title = element_text(hjust = 0.5))
measles_new_infections_plot


quartz()
simplot <- (flu_new_infections_plot) + covid_new_infections_plot + measles_new_infections_plot & theme(plot.tag = element_text(face = "bold")) +
  theme(plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
simplot
