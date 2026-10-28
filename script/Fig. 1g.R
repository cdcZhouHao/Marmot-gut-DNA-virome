library(ggplot2)
library(vegan)

# 1. Configuration & file paths
counts_file     <- "reads_count.txt"
metadata_file   <- "sample.txt"
output_matrix   <- "bray-curtis_matrix.txt"
output_pdf      <- "PCoA_plot.pdf"

palette_colors  <- c("#FF6A6A", "#4793AF", "#8A2BE2", "#FFA500")
shape_values    <- c(16, 17, 15, 18)

# 2. Data loading and normalization
# Load abundance matrix (taxa as rows, samples as columns)
counts <- read.table(
  counts_file,
  header = TRUE,
  sep = "\t",
  row.names = 1,
  check.names = FALSE
)

# Relative abundance transformation (TSS normalization)
col_sums <- colSums(counts)
if (any(col_sums == 0)) stop("One or more samples have zero total counts.")
rel_abundance <- sweep(counts, 2, col_sums, FUN = "/")

# Transpose for vegan (samples as rows, taxa as columns)
mat_t <- t(rel_abundance)

# Load sample metadata
metadata <- read.delim(metadata_file, sep = "\t", row.names = 1, stringsAsFactors = TRUE)

# Match and order samples across tables
common_samples <- intersect(rownames(mat_t), rownames(metadata))
if (length(common_samples) < 2) {
  stop("Insufficient overlapping samples between abundance matrix and metadata.")
}

mat_t    <- mat_t[common_samples, , drop = FALSE]
metadata <- metadata[common_samples, , drop = FALSE]

# 3. Distance calculation & PCoA ordination
bray_dist <- vegdist(mat_t, method = "bray")
write.table(as.matrix(bray_dist), output_matrix, sep = "\t", quote = FALSE)

pcoa <- cmdscale(bray_dist, k = 3, eig = TRUE)
pcoa_data <- as.data.frame(pcoa$points)
colnames(pcoa_data) <- c("PCoA1", "PCoA2", "PCoA3")

# Variance explained (accounting for positive eigenvalues)
pos_eig <- pcoa$eig[pcoa$eig > 0]
eig_percent <- round(pos_eig[1:2] / sum(pos_eig) * 100, 1)

# Merge coordinates with metadata
group_var <- colnames(metadata)[1]
pcoa_result <- cbind(pcoa_data, metadata)
colnames(pcoa_result)[which(colnames(pcoa_result) == group_var)] <- "Group"

# 4. Statistical hypothesis testing (PERMANOVA / adonis2)
set.seed(123)
adonis_res <- adonis2(mat_t ~ metadata[[1]], data = metadata, permutations = 999, method = "bray")
stat_caption <- sprintf("PERMANOVA R2 = %.3f, p = %.4f", adonis_res$R2[1], adonis_res$`Pr(>F)`[1])

# 5. Visualization
p <- ggplot(pcoa_result, aes(x = PCoA1, y = PCoA2, color = Group, shape = Group)) +
  geom_hline(yintercept = 0, color = "#BEBEBE", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "#BEBEBE", linetype = "dashed") +
  stat_ellipse(
    aes(fill = Group),
    geom = "polygon",
    level = 0.90,
    alpha = 0.1,
    linetype = 2,
    linewidth = 0.5
  ) +
  geom_point(size = 4, alpha = 0.85) +
  scale_color_manual(values = palette_colors) +
  scale_fill_manual(values = palette_colors) +
  scale_shape_manual(values = shape_values) +
  labs(
    x = paste0("PCoA 1 (", eig_percent[1], "%)"),
    y = paste0("PCoA 2 (", eig_percent[2], "%)"),
    caption = stat_caption
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.position = "right",
    axis.text = element_text(color = "black"),
    plot.caption = element_text(hjust = 0, size = 10, face = "italic")
  )

# 6. Save figure
ggsave(output_pdf, plot = p, width = 8, height = 6)