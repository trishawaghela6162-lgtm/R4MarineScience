# Workshop 2 - Estuary Data Analysis

library(tidyverse)

# Import estuary water-quality data
estuary_sonde <- read_csv("data/workshop2/estuary_sonde_data.csv")

# Inspect the dataset
glimpse(estuary_sonde)
# Convert timestamps to date-time
estuary_sonde <- estuary_sonde %>%
  mutate(timestamp = lubridate::dmy_hm(timestamp))

# Check the result
glimpse(estuary_sonde)
# Count missing values in each column
colSums(is.na(estuary_sonde))
# Calculate average water quality at each site
site_summary <- estuary_sonde %>%
  group_by(site) %>%
  summarise(
    mean_temperature = mean(temperature),
    mean_salinity = mean(salinity),
    mean_turbidity = mean(turbidity),
    .groups = "drop"
  )

print(knitr::kable(site_summary))
summary(estuary_sonde$turbidity)
sum(estuary_sonde$turbidity == -999, na.rm = TRUE)
# Replace likely missing-data placeholder

estuary_sonde <- estuary_sonde %>%
  mutate(turbidity = ifelse(turbidity == -999, NA, turbidity))
# Recalculate averages using cleaned data
site_summary_clean <- estuary_clean %>%
  group_by(site) %>%
  summarise(
    mean_temperature = mean(temperature, na.rm = TRUE),
    mean_salinity = mean(salinity, na.rm = TRUE),
    mean_turbidity = mean(turbidity, na.rm = TRUE),
    .groups = "drop"
  )

print(site_summary_clean)

# Compare average salinity across estuary sites
ggplot(site_summary_clean,
       aes(x = site, y = mean_salinity)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Mean Salinity Across Ross River Estuary",
    x = "Sampling Site",
    y = "Mean Salinity"
  ) +
  theme_minimal()

# Import estuary site metadata
estuary_metadata <- read_csv(
  "data/workshop2/estuary_metadata.csv"
)

# Inspect the metadata
glimpse(estuary_metadata)

# Standardise site names
estuary_metadata <- estuary_metadata %>%
  mutate(
    site_name = site_name %>%
      str_to_lower() %>%
      str_replace_all(" ", "_")
  )

# Check cleaned names
print(estuary_metadata)

# Join cleaned water-quality data with site metadata
estuary_joined <- estuary_clean %>%
  left_join(
    estuary_metadata,
    by = c("site" = "site_name")
  )

# Check the joined dataset
glimpse(estuary_joined)

# Check for unmatched sampling sites
sum(is.na(estuary_joined$zone))

# Summarise water quality by estuary zone
zone_summary <- estuary_joined %>%
  group_by(zone) %>%
  summarise(
    mean_temperature = mean(temperature, na.rm = TRUE),
    mean_salinity = mean(salinity, na.rm = TRUE),
    mean_turbidity = mean(turbidity, na.rm = TRUE),
    .groups = "drop"
  )

print(zone_summary)

# Plot mean salinity across estuary zones
ggplot(
  zone_summary,
  aes(
    x = factor(zone,
               levels = c("Upstream", "Middle", "Downstream", "Marine")
    ),
    y = mean_salinity
  )
) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Salinity Gradient Across Ross River Estuary",
    x = "Estuary Zone",
    y = "Mean Salinity"
  ) +
  theme_minimal()

# Save salinity gradient graph
ggsave(
  "outputs/estuary_salinity_gradient.png",
  plot = last_plot(),
  width = 8,
  height = 5,
  dpi = 300
)

# Calculate daily mean salinity by estuary zone
daily_salinity <- estuary_joined %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(date, zone) %>%
  summarise(
    mean_salinity = mean(salinity, na.rm = TRUE),
    .groups = "drop"
  )

# Inspect the results
print(head(daily_salinity))

# Plot daily salinity changes by estuary zone
ggplot(daily_salinity,
       aes(x = date, y = mean_salinity, color = zone)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Daily Salinity Changes Across Ross River Estuary",
    x = "Date",
    y = "Mean Salinity",
    color = "Estuary Zone"
  ) +
  theme_minimal()

# Save daily salinity time-series graph
ggsave(
  "outputs/estuary_daily_salinity.png",
  plot = last_plot(),
  width = 8,
  height = 5,
  dpi = 300
)

# Calculate daily mean temperature by zone
daily_temperature <- estuary_joined %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(date, zone) %>%
  summarise(
    mean_temperature = mean(temperature, na.rm = TRUE),
    .groups = "drop"
  )

print(head(daily_temperature))

# Plot daily temperature by estuary zone
ggplot(daily_temperature,
       aes(x = date, y = mean_temperature, color = zone)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Daily Temperature Across Ross River Estuary",
    x = "Date",
    y = "Mean Temperature (°C)",
    color = "Estuary Zone"
  ) +
  theme_minimal()

# Save daily temperature graph
ggsave(
  "outputs/estuary_daily_temperature.png",
  plot = last_plot(),
  width = 8,
  height = 5,
  dpi = 300
)

# Calculate daily mean turbidity by estuary zone
daily_turbidity <- estuary_joined %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(date, zone) %>%
  summarise(
    mean_turbidity = mean(turbidity, na.rm = TRUE),
    .groups = "drop"
  )

print(head(daily_turbidity))

# Plot daily turbidity by estuary zone
ggplot(daily_turbidity,
       aes(x = date, y = mean_turbidity, color = zone)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Daily Turbidity Across Ross River Estuary",
    x = "Date",
    y = "Mean Turbidity",
    color = "Estuary Zone"
  ) +
  theme_minimal()

# Save daily turbidity graph
ggsave(
  "outputs/estuary_daily_turbidity.png",
  plot = last_plot(),
  width = 8,
  height = 5,
  dpi = 300
)

# Import estuary catch records
library(readxl)

catch_log <- read_excel(
  "data/workshop2/estuary_catch_log.xlsx"
)

# Inspect the dataset
glimpse(catch_log)

# Standardise site and species names
catch_clean <- catch_log %>%
  mutate(
    site = site %>%
      str_to_lower() %>%
      str_replace_all(" ", "_"),
    species = species %>%
      str_to_lower() %>%
      str_squish()
  )

# Check cleaned records
glimpse(catch_clean)

# Calculate total fish abundance by species
species_abundance <- catch_clean %>%
  group_by(species) %>%
  summarise(
    total_count = sum(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_count))

print(species_abundance)

# Plot total fish abundance by species
ggplot(species_abundance,
       aes(x = reorder(species, total_count),
           y = total_count)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Fish Abundance in Ross River Estuary",
    x = "Fish Species",
    y = "Total Individuals Recorded"
  ) +
  theme_minimal()

# Save fish abundance graph
ggsave(
  "outputs/estuary_fish_abundance.png",
  plot = last_plot(),
  width = 8,
  height = 5,
  dpi = 300
)

# Calculate total fish abundance at each site
site_abundance <- catch_clean %>%
  group_by(site) %>%
  summarise(
    total_fish = sum(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_fish))

print(site_abundance)

# Calculate daily fish abundance at Lower Ross
daily_catch <- catch_clean %>%
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarise(
    total_fish = sum(count, na.rm = TRUE),
    .groups = "drop"
  )

print(daily_catch)

# Visualise daily fish abundance
ggplot(daily_catch, aes(x = date, y = total_fish)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  labs(
    title = "Daily Fish Abundance at Lower Ross",
    x = "Sampling Date",
    y = "Total Fish Recorded"
  ) +
  theme_minimal()

# Save daily fish abundance graph
ggsave(
  "outputs/estuary_daily_fish_abundance.png",
  plot = last_plot(),
  width = 8,
  height = 5,
  dpi = 300
)

# Calculate daily species richness
daily_richness <- catch_clean %>%
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarise(
    species_richness = n_distinct(species),
    .groups = "drop"
  )

print(daily_richness)

print(daily_richness, n = 30)

ggplot(daily_richness,
       aes(x = date, y = species_richness)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  scale_y_continuous(breaks = 0:4) +
  labs(
    title = "Daily Fish Species Richness at Lower Ross",
    x = "Sampling Date",
    y = "Number of Species"
  ) +
  theme_minimal()

# Save daily fish species richness graph
ggsave(
  "outputs/estuary_daily_species_richness.png",
  plot = last_plot(),
  width = 8,
  height = 5,
  dpi = 300
)

# Calculate daily salinity at Lower Ross
lower_ross_salinity <- estuary_clean %>%
  filter(site == "lower_ross") %>%
  mutate(date = as.Date(timestamp)) %>%
  group_by(date) %>%
  summarise(
    mean_salinity = mean(salinity, na.rm = TRUE),
    .groups = "drop"
  )

# Join salinity with fish abundance
fish_environment <- daily_catch %>%
  left_join(lower_ross_salinity, by = "date")

print(head(fish_environment))


# Explore the relationship between salinity and fish abundance
ggplot(fish_environment,
       aes(x = mean_salinity, y = total_fish)) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Salinity vs Fish Abundance at Lower Ross",
    x = "Mean Daily Salinity",
    y = "Total Fish Recorded"
  ) +
  theme_minimal()

# Save salinity vs fish abundance graph
ggsave(
  "outputs/estuary_salinity_vs_fish_abundance.png",
  plot = last_plot(),
  width = 8,
  height = 5,
  dpi = 300
)
# Calculate Pearson correlation
salinity_correlation <- cor(
  fish_environment$mean_salinity,
  fish_environment$total_fish,
  use = "complete.obs",
  method = "pearson"
)

print(salinity_correlation)

# Test the salinity–fish abundance relationship
cor.test(
  fish_environment$mean_salinity,
  fish_environment$total_fish,
  method = "pearson"
)
# Interpretation:
# Pearson correlation: r = 0.0503
# p-value = 0.7917
# 95% CI = [-0.316, 0.403]
# No statistically significant linear relationship
# between daily salinity and fish abundance was detected.

# Join scientific names to fish catch records
catch_scientific <- catch_clean %>%
  left_join(
    species_dictionary,
    by = c("species" = "common_name")
  )

# Check the result
print(head(catch_scientific))

catch_with_zones <- catch_scientific %>%
  left_join(
    estuary_metadata,
    by = c("site" = "site_name")
  )

glimpse(catch_with_zones)

fish_salinity <- catch_with_zones %>%
  mutate(date = as.Date(date)) %>%
  left_join(
    daily_salinity,
    by = c("date", "zone")
  )

glimpse(fish_salinity)

fish_complete <- fish_salinity %>%
  tidyr::complete(
    nesting(site, date),
    species = unique(species)
  )

nrow(fish_complete)


fish_complete <- fish_complete %>%
  select(site, date, species, count) %>%
  left_join(
    species_dictionary,
    by = c("species" = "common_name")
  ) %>%
  left_join(
    estuary_metadata,
    by = c("site" = "site_name")
  ) %>%
  left_join(
    daily_salinity,
    by = c("date", "zone")
  )

glimpse(fish_complete)