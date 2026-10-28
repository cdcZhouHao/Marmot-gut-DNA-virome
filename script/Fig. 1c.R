library(vegan)
library(ggplot2)
library(tibble)
library(purrr)

# 1. Define paths and color palette
input_file <- "reads_count_sum.txt"
output_pdf_base <- "Rarefaction_original.pdf"
output_pdf_gg <- "Rarefaction_ggplot.pdf"

palette_colors <- c("#FF6A6A", "#4793AF", "#8A2BE2", "#FFA500")
options(scipen = 200)

# 2. Load count table (taxa as rows, samples as columns)
counts <- read.table(
  input_file,
  header = TRUE,
  row.names = 1,
  sep = "\t",
  check.names = FALSE
)

# Transpose for vegan (samples as rows, taxa as columns)
count_matrix <- t(counts)
sample_colors <- rep(palette_colors, length.out = nrow(count_matrix))

# 3. Base R rarefaction curve
pdf(output_pdf_base, width = 10, height = 8)
rare_list <- rarecurve(
  count_matrix,
  step = 10000,
  col = sample_colors,
  tidy = FALSE
)
dev.off()

# 4. Tidy rarefaction results for ggplot2
names(rare_list) <- rownames(count_matrix)

rare_df <- imap_dfr(rare_list, function(curve_data, sample_name) {
  tibble(
    SampleSize = as.numeric(gsub("^N", "", names(curve_data))),
    SpeciesNumber = as.numeric(curve_data),
    Sample = sample_name
  )
})

# 5. Generate and export visualization
p <- ggplot(rare_df, aes(x = SampleSize, y = SpeciesNumber, color = Sample)) +
  geom_line(linewidth = 1, alpha = 0.5) +
  scale_color_manual(values = setNames(sample_colors, rownames(count_matrix))) +
  labs(
    x = "Number of Sequences",
    y = "Number of Species",
    color = NULL
  ) +
  theme_test()

ggsave(
  filename = output_pdf_gg,
  plot = p,
  width = 10,
  height = 8
)