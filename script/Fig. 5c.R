library(tidyverse)
library(readxl)
library(patchwork)

# 1. Configuration & file paths
input_file <- "ARG-MGE.xlsx"
output_pdf <- "ARG_MGE_correlation.pdf"

# Mechanisms to analyze and their corresponding colors
mechanisms <- c(
  "integration/excision"              = "#78BB73",
  "phage"                             = "#F08968",
  "replication/recombination/repair" = "#9256A0",
  "stability/transfer/defense"        = "#4499C8",
  "transfer"                          = "#D53353"
)

# 2. Data loading and transformation
# Sheet 1: ARG abundance matrix
arg <- read_excel(input_file, sheet = 1)
colnames(arg)[1] <- "Type"

# Vectorized row-sum calculation for total ARG abundance
arg_total <- tibble(
  Type      = arg$Type,
  ARG_total = rowSums(select(arg, -Type), na.rm = TRUE)
)

# Sheet 2: MGE abundance matrix
mge <- read_excel(input_file, sheet = 2)
colnames(mge)[1] <- "Type"

plot_data <- mge %>%
  pivot_longer(
    cols      = -Type,
    names_to  = "MGE_mechanism",
    values_to = "MGE_abundance"
  ) %>%
  left_join(arg_total, by = "Type")

# 3. Scatter plot function with Spearman correlation & linear regression
plot_lm_spearman <- function(df, mech, color) {
  df_sub <- filter(df, MGE_mechanism == mech)

  # Non-parametric correlation
  cor_res <- cor.test(df_sub$MGE_abundance, df_sub$ARG_total, method = "spearman")
  r_val   <- cor_res$estimate
  p_val   <- cor_res$p.value

  # Linear model for trendline equation
  lm_fit    <- lm(ARG_total ~ MGE_abundance, data = df_sub)
  slope     <- coef(lm_fit)[2]
  intercept <- coef(lm_fit)[1]

  sign_char <- if (intercept >= 0) "+" else "-"
  label_text <- sprintf(
    "y = %.2gx %s %.2g\nr_s = %.2g, P = %.2g",
    slope, sign_char, abs(intercept), r_val, p_val
  )

  ggplot(df_sub, aes(x = MGE_abundance, y = ARG_total)) +
    geom_point(size = 3, alpha = 0.9, color = color) +
    geom_smooth(
      method    = "lm",
      se        = TRUE,
      linewidth = 1,
      color     = color,
      fill      = color,
      alpha     = 0.25
    ) +
    annotate(
      "text",
      x      = min(df_sub$MGE_abundance, na.rm = TRUE),
      y      = max(df_sub$ARG_total, na.rm = TRUE),
      label  = label_text,
      hjust  = 0,
      vjust  = 1,
      size   = 3.8,
      color  = "black"
    ) +
    labs(
      x = paste("Relative abundance of", mech, "(RPKM)"),
      y = "Relative abundance of ARGs (RPKM)"
    ) +
    theme_classic(base_size = 9) +
    theme(
      axis.title  = element_text(face = "bold"),
      axis.text   = element_text(color = "black"),
      plot.margin = margin(5.5, 18, 5.5, 5.5)
    )
}

# 4. Generate panels and assemble composite figure
plot_list <- imap(mechanisms, ~ plot_lm_spearman(plot_data, .y, .x))
final_plot <- wrap_plots(plot_list, nrow = 1)

# 5. Export visualization
ggsave(
  filename = output_pdf,
  plot     = final_plot,
  device   = "pdf",
  width    = 22,
  height   = 4.5,
  units    = "in",
  dpi      = 300
)