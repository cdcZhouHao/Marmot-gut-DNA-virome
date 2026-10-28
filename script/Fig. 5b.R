library(linkET)
library(ggplot2)
library(dplyr)
library(readxl)
library(tibble)

# 1. Configuration & file paths
input_file <- "Mantel_test.xlsx"
output_csv <- "Table_SXX_Complete_Mantel_Statistical_Family.csv"
output_pdf <- "Mantel_plot_FDR_corrected.pdf"

# 2. Data loading and transformation
# Sheet 1: Environmental / ARG factors (samples as rows)
env <- read_excel(input_file, sheet = "Sheet1") %>%
  column_to_rownames("sample") %>%
  as.data.frame()

# Sheet 2: Species / MGE factors (samples as rows)
sp <- read_excel(input_file, sheet = "Sheet2") %>%
  column_to_rownames("sample") %>%
  as.data.frame()

# 3. Mantel test execution
# Treat each MGE column as an individual target group
spec_select_list <- as.list(setNames(seq_along(sp), colnames(sp)))

mantel_raw <- mantel_test(
  spec        = sp,
  env         = env,
  mantel_fun  = "mantel",
  spec_dist   = "bray",
  env_dist    = "euclidean",
  spec_select = spec_select_list
)

# 4. Multiple testing correction & statistical summary export
mantel_complete <- mantel_raw %>%
  as.data.frame() %>%
  mutate(
    n_samples        = nrow(env),
    permutations     = 9999,
    p_adj_fdr        = p.adjust(p, method = "BH"),
    p_adj_bonferroni = p.adjust(p, method = "bonferroni")
  ) %>%
  rename(
    MGE_category = spec,
    ARG_type     = env,
    Mantel_r     = r,
    p_raw        = p
  )

write.csv(mantel_complete, file = output_csv, row.names = FALSE)

# 5. Significance grading & visualization preparation
label_high_trend <- "italic(P)[raw] < 0.01 ~ '&' ~ italic(P)[adj] < 0.10"
label_nominal    <- "italic(P)[raw] < 0.05 ~ '&' ~ italic(P)[adj] >= 0.10"
label_not_sig    <- "italic(P)[raw] >= 0.05"

trend_colors <- setNames(
  c("#D385B7", "#8ECF89", "#D9D9D9"),
  c(label_high_trend, label_nominal, label_not_sig)
)

rd_levels <- c("-0.2 - 0.2", "0.2 - 0.4", ">= 0.4", "-0.4 - -0.2", "<= -0.4")
rd_sizes  <- setNames(c(0.7, 1.4, 2.1, 1.4, 2.1), rd_levels)

mantel <- mantel_raw %>%
  mutate(
    p_adj    = p.adjust(p, method = "BH"),
    rd       = cut(
      r,
      breaks = c(-Inf, -0.4, -0.2, 0.2, 0.4, Inf),
      labels = c("<= -0.4", "-0.4 - -0.2", "-0.2 - 0.2", "0.2 - 0.4", ">= 0.4")
    ),
    rd       = factor(rd, levels = rd_levels),
    linetype = cut(
      r,
      breaks = c(-Inf, 0, Inf),
      labels = c("Negative", "Positive")
    ),
    pd       = case_when(
      p < 0.01 & p_adj < 0.10 ~ label_high_trend,
      p < 0.05 & p_adj >= 0.10 ~ label_nominal,
      TRUE ~ label_not_sig
    ),
    pd       = factor(pd, levels = c(label_high_trend, label_nominal, label_not_sig))
  )

# 6. Build correlation and Mantel network plot
p1 <- qcorrplot(
  correlate(env, method = "pearson"),
  type = "upper",
  diag = TRUE
) +
  geom_square() +
  geom_mark(
    sep       = "\n",
    size      = 2.5,
    sig_level = c(0.05, 0.01, 0.001),
    mark      = c("*", "**", "***"),
    fontface  = 1
  ) +
  geom_couple(
    aes(color = pd, size = rd, linetype = linetype),
    data           = mantel,
    curvature      = nice_curvature(),
    label.size     = 4,
    label.fontface = 1
  ) +
  scale_linetype_manual(values = c("Negative" = "11", "Positive" = "solid")) +
  scale_fill_gradientn(
    colors = c("#F89095", "white", "#ADC9E0"),
    limits = c(-1, 1)
  ) +
  scale_size_manual(values = rd_sizes) +
  scale_color_manual(
    values = trend_colors,
    labels = scales::parse_format(),
    drop   = FALSE
  ) +
  guides(
    color    = guide_legend(title = "Mantel's significance", order = 1, title.theme = element_text(size = 10, face = "bold")),
    size     = guide_legend(title = "Mantel's r", order = 2, title.theme = element_text(size = 10, face = "bold")),
    linetype = guide_legend(title = "Linetype", order = 3, title.theme = element_text(size = 10, face = "bold")),
    fill     = guide_colorbar(title = "Pearson's r", order = 4, title.theme = element_text(size = 10, face = "bold"))
  ) +
  theme(
    axis.text.x.top = element_text(size = 10, angle = 45, hjust = 0, vjust = 0, face = 1),
    axis.text.y     = element_text(size = 10, angle = 0, face = 1),
    text            = element_text(size = 10, face = 1),
    legend.text     = element_text(size = 9, face = "plain", color = "black"),
    plot.margin     = margin(10, 30, 50, 50)
  ) +
  coord_cartesian(clip = "off")

# 7. Export visualization
pdf(output_pdf, width = 12.5, height = 8)
print(p1)
dev.off()