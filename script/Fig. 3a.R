library(tidyverse)
library(readxl)
library(RColorBrewer)

# 1. Configuration & file paths
input_file <- "viral_host.xlsx"
output_pdf <- "viral_host_abundance_correlation.pdf"

# 2. Data loading and transformation
df_scatter <- read_excel(input_file, sheet = "Sheet2") %>%
  mutate(
    Host_abundance_log10 = log10(Host_abundance + 1e-6),
    Virus_abundance_log10 = log10(Virus_abundance + 1e-6)
  )

# 3. Statistical computations (Linear regression & Pearson correlation)
fit <- lm(Virus_abundance_log10 ~ Host_abundance_log10, data = df_scatter)
summary_fit <- summary(fit)

slope <- round(coef(fit)[2], 2)
intercept <- round(coef(fit)[1], 2)
r_squared <- round(summary_fit$r.squared, 2)
p_value <- signif(summary_fit$coefficients[2, 4], 2)

cor_test <- cor.test(df_scatter$Host_abundance_log10, df_scatter$Virus_abundance_log10)
r_value <- round(cor_test$estimate, 2)

# Build annotation labels
eq_text <- sprintf("y = %.2fx + %.2f, R² = %.2f, P = %g", slope, intercept, r_squared, p_value)
pearson_text <- sprintf("Pearson r = %.2f", r_value)

# 4. Color palette mapping
phylum_levels <- unique(df_scatter$Phylum)
n_phyla <- length(phylum_levels)
palette_colors <- colorRampPalette(brewer.pal(min(max(3, n_phyla), 12), "Paired"))(n_phyla)

# Determine axis limits and annotation positions
x_min <- min(df_scatter$Host_abundance_log10, na.rm = TRUE)
x_max <- max(df_scatter$Host_abundance_log10, na.rm = TRUE)
y_min <- min(df_scatter$Virus_abundance_log10, na.rm = TRUE)
y_max <- max(df_scatter$Virus_abundance_log10, na.rm = TRUE)
y_span <- y_max - y_min

# 5. Build visualization
plot_B <- ggplot(df_scatter, aes(x = Host_abundance_log10, y = Virus_abundance_log10, color = Phylum)) +
  geom_point(size = 3.5, alpha = 0.85) +
  geom_smooth(
    method = "lm",
    color = "#0072a3",
    linetype = "solid",
    se = TRUE,
    fill = "#d0d9e3",
    fullrange = TRUE,
    aes(group = 1),
    linewidth = 0.5
  ) +
  annotate(
    "text",
    x = x_min,
    y = y_max,
    label = eq_text,
    hjust = 0,
    vjust = 1,
    size = 4.5
  ) +
  annotate(
    "text",
    x = x_min,
    y = y_max - (y_span * 0.08),
    label = pearson_text,
    hjust = 0,
    vjust = 1,
    size = 4.5
  ) +
  scale_x_continuous(limits = c(floor(x_min), ceiling(x_max))) +
  scale_y_continuous(limits = c(floor(y_min), ceiling(y_max))) +
  scale_color_manual(values = setNames(palette_colors, phylum_levels), name = "Phylum") +
  labs(
    x = expression(paste("Host abundance (", Log[10], " RPKM)")),
    y = expression(paste("Virus abundance (", Log[10], " RPKM)")),
    title = "B"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.line = element_line(linewidth = 0.5),
    plot.title = element_text(face = "bold", hjust = 0),
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

# 6. Export visualization
ggsave(
  filename = output_pdf,
  plot = plot_B,
  device = "pdf",
  width = 8,
  height = 6,
  units = "in",
  useDingbats = FALSE
)