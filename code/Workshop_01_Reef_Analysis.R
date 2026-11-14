# MB5370 - Module 2 - Workshop 1
# Reef Cover Analysis

# Load packages
library(tidyverse)
library(readxl)
library(here)

# Import reef data
benthic_cover <- read_csv(
  here("data", "workshop1", "reef_cover_log.csv")
)

# Calculate average coral cover
coral_summary <- benthic_cover %>%
  group_by(site_id) %>%
  summarise(
    mean_coral_cover = mean(hard_coral_pct)
  )

# Plot average coral cover
ggplot(coral_summary,
       aes(x = site_id, y = mean_coral_cover)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Mean Hard Coral Cover Across Reef Sites",
    x = "Reef Site",
    y = "Mean Hard Coral Cover (%)"
  ) +
  theme_minimal()

# Save graph
ggsave(
  "outputs/mean_coral_cover.png",
  width = 8,
  height = 5,
  dpi = 300
)