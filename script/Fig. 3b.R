library(tidyverse)
library(readxl)

# 1. Configuration & file paths
input_file <- "viral_host.xlsx"
output_pdf <- "virus-to-host_abundance_ratio.pdf"

# 2. Data loading and transformation
df_vhr <- read_excel(input_file, sheet = "Sheet2") %>%
  mutate(
    Host_abundance_log = log10(Host_abundance + 1),
    VHR_Log10_Score    = log10((Virus_abundance / Host_abundance) + 1),
    Phylum             = reorder(Phylum, -VHR_Log10_Score)
  )

# 3. Dual-axis scaling factor
max_left       <- max(df_vhr$Host_abundance_log, na.rm = TRUE)
max_right      <- max(df_vhr$VHR_Log10_Score, na.rm = TRUE)
scale_factor_c <- max_left / max_right

# 4. Build dual-axis bar and line visualization
plot_C <- ggplot(df_vhr, aes(x = Phylum)) +
  # Left axis: host abundance
  geom_col(
    aes(y = Host_abundance_log),
    fill = "#319882",
    width = 0.6,
    alpha = 0.8
  ) +
  # Right axis: virus-to-host ratio (VHR)
  geom_line(
    aes(y = VHR_Log10_Score * scale_factor_c, group = 1),
    color = "#FB8072",
    alpha = 0.8,
    linewidth = 0.6
  ) +
  geom_point(
    aes(y = VHR_Log10_Score * scale_factor_c),
    color = "#FB8072",
    size = 2
  ) +
  scale_y_continuous(
    name = expression(atop("Host abundance", Log[10]~"RPKM")),
    limits = c(0, max_left * 1.1),
    sec.axis = sec_axis(
      transform = ~ . / scale_factor_c,
      name = expression(paste("VHR (", Log[10], " Score)")),
      breaks = seq(0, ceiling(max_right), by = 1)
    )
  ) +
  labs(title = "C", x = NULL) +
  theme_classic(base_size = 12) +
  theme(
    axis.line = element_line(linewidth = 0.5),
    plot.title = element_text(face = "bold", hjust = 0),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9, color = "black"),
    axis.title.y.left = element_text(color = "black", size = 12),
    axis.title.y.right = element_text(color = "black", size = 12),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

# 5. Export visualization
ggsave(
  filename = output_pdf,
  plot = plot_C,
  device = "pdf",
  width = 8,
  height = 5,
  units = "in",
  dpi = 300
)