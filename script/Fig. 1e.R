library(ggplot2)
library(dplyr)
library(readxl)

# 1. Configuration & file paths
input_file <- "alpha_diversity.xlsx"
output_pdf <- "Shannon_diversity_boxplot.pdf"

# Mapping table for sample prefixes to species names
species_lookup <- c(
  "H" = "M. baibacina",
  "M" = "M. sibirica",
  "C" = "M. caudata",
  "X" = "M. himalayana"
)

palette_colors <- c(
  "M. baibacina"  = "#FF6A6A",
  "M. sibirica"   = "#FFA500",
  "M. caudata"    = "#4793AF",
  "M. himalayana" = "#8A2BE2"
)

# 2. Data loading and transformation
df <- read_excel(input_file, sheet = 2) %>%
  mutate(
    Prefix = substr(Species, 1, 1),
    Species = recode(Prefix, !!!species_lookup, .default = Prefix),
    Species = factor(Species, levels = names(palette_colors))
  )

# 3. Build Shannon diversity boxplot
p <- ggplot(df, aes(x = Species, y = Shannon, fill = Species)) +
  geom_boxplot(
    alpha = 0.7,
    outlier.shape = NA,
    median.linewidth = 0.5,
    linewidth = 0.2
  ) +
  geom_jitter(
    aes(color = Species),
    width = 0.2,
    size = 3,
    alpha = 0.8
  ) +
  scale_fill_manual(values = palette_colors) +
  scale_color_manual(values = palette_colors) +
  labs(
    x = "Species",
    y = "Shannon Diversity Index"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.text.x = element_text(angle = 25, hjust = 1, face = "italic"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.4)
  )

# 4. Export visualization
ggsave(
  filename = output_pdf,
  plot = p,
  device = "pdf",
  width = 9,
  height = 8,
  dpi = 300
)