# MB5370 Workshop 2 — finishing and verification script
# AI assistance disclosure: ChatGPT helped draft this data-validation,
# zero-catch completion, summary and visualisation script.
# Run AFTER the existing Workshop 2 import/cleaning script.

library(tidyverse)
library(lubridate)
library(palmerpenguins)

dir.create('outputs', showWarnings = FALSE, recursive = TRUE)

# PART A — Penguin metadata cleaning, join and wide maximum-mass table
island_metadata <- tibble(
  island_name = c(' biscoe', 'Dream ', 'Torgersen'),
  station_install = c('15/01/2003', '22-03-2004', '05/11/2001'),
  latitude = c(-64.81, -64.73, -64.76)
)
clean_metadata <- island_metadata |>
  mutate(
    island_name = str_to_title(str_trim(island_name)),
    island_name = if_else(island_name == 'Biscoe', 'Biscoe', island_name),
    station_install = dmy(station_install)
  )
penguins_spatial <- palmerpenguins::penguins |>
  left_join(clean_metadata, by = join_by(island == island_name))
penguin_max_mass <- penguins_spatial |>
  drop_na(body_mass_g) |>
  group_by(species, island) |>
  summarise(max_body_mass_g = max(body_mass_g), .groups = 'drop') |>
  pivot_wider(names_from = island, values_from = max_body_mass_g)
write_csv(penguin_max_mass, 'outputs/workshop2_penguin_max_mass.csv')

# PART B — Build a site-date-species zero-catch framework from ACTUAL sampling events
# This requires catch_clean, species_dictionary, estuary_metadata, daily_salinity
# from the earlier Workshop 2 code. It does not invent unsampled site-days.
required_objects <- c('catch_clean', 'species_dictionary', 'estuary_metadata', 'daily_salinity')
missing_objects <- required_objects[!vapply(required_objects, exists, logical(1), inherits = TRUE)]
if (length(missing_objects)) stop('Run the earlier Workshop 2 script first. Missing: ', paste(missing_objects, collapse = ', '))

catch_base <- catch_clean |>
  transmute(
    site = str_replace_all(str_to_lower(str_trim(site)), '\\s+', '_'),
    date = as.Date(date),
    species = str_replace_all(str_to_lower(str_trim(species)), '\\s+', '_'),
    count = as.numeric(count)
  )

# Stop rather than mistakenly interpreting unrecorded/invalid catches as zero.
if (any(is.na(catch_base$count))) {
  warning('Some original fish counts are NA: review before treating missing entries as zero.')
}
if (any(catch_base$count < 0, na.rm = TRUE)) stop('Negative fish counts found; inspect source data.')
if (anyDuplicated(catch_base[c('site', 'date', 'species')])) {
  stop('Duplicate site-date-species records: resolve before zero filling.')
}

sampled_site_dates <- catch_base |> distinct(site, date)
all_species <- species_dictionary |>
  transmute(species = str_replace_all(str_to_lower(str_trim(common_name)), '\\s+', '_')) |>
  distinct()

fish_complete <- sampled_site_dates |>
  tidyr::crossing(all_species) |>
  left_join(catch_base, by = c('site', 'date', 'species')) |>
  mutate(count = coalesce(count, 0)) |>
  left_join(
    species_dictionary |>
      mutate(common_name = str_replace_all(str_to_lower(str_trim(common_name)), '\\s+', '_')),
    by = c('species' = 'common_name')
  ) |>
  left_join(estuary_metadata, by = c('site' = 'site_name')) |>
  left_join(
    daily_salinity |> mutate(date = as.Date(date)),
    by = c('date', 'zone')
  ) |>
  mutate(zone = factor(zone, levels = c('Upstream', 'Middle', 'Downstream', 'Marine')))

# This is a hard check: don't silently publish unmatched records.
if (anyNA(fish_complete$scientific_name)) stop('Unmatched scientific names in fish_complete.')
if (anyNA(fish_complete$zone)) stop('Unmatched estuary zones in fish_complete.')
if (anyNA(fish_complete$mean_salinity)) stop('Unmatched salinity values in fish_complete.')

# Critical coverage report: source catch data may contain only lower_ross.
coverage <- sampled_site_dates |>
  count(site, name = 'sampled_days') |>
  arrange(site)
print(coverage)
write_csv(coverage, 'outputs/workshop2_sampling_coverage.csv')
if (n_distinct(fish_complete$site) < nrow(estuary_metadata)) {
  warning('NOT ALL ESTUARY SITES ARE IN THE FISH CATCH DATA. Review Excel tabs and original import before claiming full estuary coverage.')
}

# Phase 4: calculate both SD (deliverables) and SE (phase 4 wording)
# SE = sample SD / sqrt(number of observations).
fish_zone_summary <- fish_complete |>
  group_by(scientific_name, zone) |>
  summarise(
    n = n(),
    mean_fish = mean(count, na.rm = TRUE),
    sd_fish = sd(count, na.rm = TRUE),
    se_fish = sd_fish / sqrt(n),
    mean_salinity = mean(mean_salinity, na.rm = TRUE),
    sd_salinity = sd(mean_salinity, na.rm = TRUE),
    se_salinity = sd_salinity / sqrt(n),
    .groups = 'drop'
  ) |>
  arrange(scientific_name, zone)
print(fish_zone_summary, n = Inf)
write_csv(fish_complete, 'outputs/workshop2_master_fish.csv')
write_csv(fish_zone_summary, 'outputs/workshop2_species_zone_summary.csv')

# Phase 5: faceted species figure, scientific labels italicised
fish_plot <- ggplot(fish_complete, aes(x = mean_salinity, y = count)) +
  geom_point(aes(colour = zone), alpha = 0.7, size = 2) +
  geom_smooth(method = 'lm', se = TRUE, colour = 'grey25', linewidth = 0.6) +
  scale_colour_discrete(drop = FALSE) +
  labs(
    title = 'Fish abundance along the Ross River estuary salinity gradient',
    subtitle = 'Daily catches at sampled sites; unrecorded species coded as zero',
    x = 'Mean daily salinity (PSU)',
    y = 'Fish count per sampled site-day',
    colour = 'Estuary zone',
    caption = 'Descriptive relationships; correlation does not establish causation.'
  ) +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = 'italic'), plot.title.position = 'plot')
# facet_wrap needs syntactically parsed strings for label_parsed, so instead
# use a custom label function that wraps scientific names in plotmath italic().
fish_plot <- fish_plot + facet_wrap(
  ~scientific_name,
  labeller = as_labeller(function(x) paste0('italic("', x, '")'), label_parsed)
)
print(fish_plot)
ggsave('outputs/workshop2_fish_salinity_facets.png', fish_plot,
       width = 11, height = 7, dpi = 300)

cat('\nWORKSHOP 2 FINAL CHECK\n')
cat('Fish rows:', nrow(fish_complete), '\n')
cat('Zero-catch rows:', sum(fish_complete$count == 0), '\n')
cat('Fish sampling sites:', n_distinct(fish_complete$site), 'of', nrow(estuary_metadata), '\n')
cat('Saved 5 CSV/PNG outputs to outputs/.\n')

catch_log_all <- purrr::map_dfr(
  readxl::excel_sheets("data/workshop2/estuary_catch_log.xlsx"),
  ~ readxl::read_excel(
    "data/workshop2/estuary_catch_log.xlsx",
    sheet = .x
  ),
  .id = "sheet_id"
)

glimpse(catch_log_all)

catch_clean <- catch_log_all %>%
  mutate(
    site = stringr::str_to_lower(
      stringr::str_replace_all(
        stringr::str_trim(site), "[ _]+", "_"
      )
    ),
    species = stringr::str_to_lower(
      stringr::str_replace_all(
        stringr::str_trim(species), "[ _]+", "_"
      )
    ),
    date = as.Date(date),
    count = na_if(count, -999)
  ) %>%
  select(site, date, species, count)

unique(catch_clean$site)
