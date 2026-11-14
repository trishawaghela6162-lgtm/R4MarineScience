# Workshop 1 - Penguin Data Analysis

library(tidyverse)
library(palmerpenguins)

# Load data
data("penguins")

# Select morphology variables
morphology_metrics <- penguins %>%
  select(species, bill_length_mm, bill_depth_mm, body_mass_g)

# Filter Adelie penguins
adelie_penguins <- penguins %>%
  filter(species == "Adelie")

# Convert body mass to kilograms
penguin_mass <- penguins %>%
  mutate(body_mass_kg = body_mass_g / 1000)

# Sort from heaviest to lightest
penguins_sorted <- penguin_mass %>%
  arrange(desc(body_mass_kg))

# Calculate average body mass by species
penguin_summary <- penguins %>%
  group_by(species) %>%
  summarise(
    mean_mass_g = mean(body_mass_g, na.rm = TRUE)
  )

# Create graph
ggplot(penguin_summary,
       aes(x = species, y = mean_mass_g)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Average Body Mass by Penguin Species",
    x = "Penguin Species",
    y = "Mean Body Mass (g)"
  ) +
  theme_minimal()

# Save graph
ggsave(
  "outputs/penguin_body_mass.png",
  plot = last_plot(),
  width = 8,
  height = 5,
  dpi = 300
)
