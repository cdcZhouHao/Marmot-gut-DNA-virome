library(dplyr)
library(ggplot2)
library(readxl)
library(ggpubr)

# 1. Configuration & file paths
input_file  <- "lifestyle_result.xlsx"
output_pdf  <- "lifestyle_ratio.pdf"

palette_colors <- c(
  "virulent"  = "#FB8072",
  "temperate" = "#80B1D3"
)

lifestyle_levels <- c("virulent", "temperate")

# 2. Data loading and transformation
prop_data <- read_excel(input_file, sheet = 2) %>%
  count(sample, possible_lifestyle, name = "count") %>%
  group_by(sample) %>%
  mutate(
    ratio = count / sum(count) * 100,
    possible_lifestyle = factor(possible_lifestyle, levels = lifestyle_levels)
  ) %>%
  ungroup()

# 3. Visualization
p <- ggplot(prop_data, aes(x = possible_lifestyle, y = ratio, fill = possible_lifestyle)) +
  geom_violin(
    trim = FALSE,
    width = 0.32,
    alpha = 0.9,
    linewidth = 0.15
  ) +
  geom_boxplot(
    width = 0.18,
    linewidth = 0.15,
    outlier.shape = NA,
    fill = "white",
    alpha = 1
  ) +
  geom_jitter(
    shape = 21,
    stroke = 0.15,
    size = 2,
    alpha = 0.8,
    position = position_jitter(width = 0.15, seed = 123)
  ) +
  scale_fill_manual(values = palette_colors) +
  coord_cartesian(ylim = c(0, 115)) +
  labs(
    x = NULL,
    y = "Relative Proportion (%)"
  ) +
  stat_compare_means(
    method = "wilcox.test",
    label = "p.format",
    label.x = 1.3,
    label.y = 105,
    size = 4.5
  ) +
  theme_test() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 14),
    axis.ticks = element_line(linewidth = 0.3),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    panel.border = element_rect(linewidth = 0.35, fill = NA),
    legend.position = "none"
  )

# 4. Export visualization
ggsave(
  filename = output_pdf,
  plot = p,
  device = "pdf",
  width = 5.5,
  height = 5,
  units = "in",
  dpi = 300
)