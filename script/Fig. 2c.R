library(readxl)
library(ggplot2)

# 1. Configuration & file paths
input_file <- "lifestyle_result.xlsx"
output_pdf <- "lifestyle_distribution_stacked_bar.pdf"

# Taxonomy order and corresponding colors
tax_order <- c(
  "Mazoviaviridae", "Toyamaviridae", "Nipumfusiviridae", "Salasmaviridae",
  "Kanorauviridae", "Alisviridae", "Konodaiviridae", "Guelinviridae",
  "Inoviridae", "Ludisviridae", "Steigviridae", "Suoliviridae",
  "Intestiviridae", "Microviridae", "Clermontviridae"
)

palette_colors <- setNames(
  c(
    "#518DBE", "#D79D65", "#63C28D", "#A155E8", "#F1BD3E",
    "#F18282", "#85D7BB", "#CBCBCB", "#7DBDF1", "#F1AE6D",
    "#96D796", "#BC9BFF", "#FFDF33", "#FB998E", "#EDD9B4"
  ),
  tax_order
)

class_levels <- c("Temperate", "Virulent")

# 2. Data loading and transformation
data <- read_excel(input_file, sheet = 3)

data$tax <- factor(data$tax, levels = tax_order)
data$class <- factor(data$class, levels = class_levels)

# 3. Build stacked bar plot
p <- ggplot(data, aes(x = class, y = proportion, fill = tax)) +
  geom_col(
    color = "#696969",
    width = 0.48,
    linewidth = 0.13
  ) +
  scale_fill_manual(
    values = palette_colors,
    name = "Families"
  ) +
  labs(
    x = NULL,
    y = "Relative Proportion (%)"
  ) +
  theme_test() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 10),
    axis.ticks = element_line(linewidth = 0.23),
    axis.ticks.length = unit(0.04, "cm"),
    axis.text.x = element_text(hjust = 0.5, size = 10, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    panel.border = element_rect(fill = NA, linewidth = 0.35),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width = unit(0.4, "cm"),
    legend.background = element_rect(color = "white"),
    legend.key = element_blank()
  )

# 4. Export visualization
ggsave(
  filename = output_pdf,
  plot = p,
  device = "pdf",
  width = 6,
  height = 6,
  units = "in"
)