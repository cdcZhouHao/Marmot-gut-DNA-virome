library(tidyverse)
library(ComplexHeatmap)
library(circlize)
library(readxl)
library(grid)

# 1. Configuration & file paths
matrix_file <- "ARG_heatmap-V2.xlsx"
meta_file   <- "ARG_heatmap.xlsx"
output_pdf  <- "ARG_abundance_heatmap.pdf"

# 2. Load count matrix
df <- read_excel(matrix_file, sheet = 1)
count_mat <- df %>%
  column_to_rownames("HMM.category") %>%
  as.matrix()

# 3. Color mapping for expression values
col_fun <- colorRamp2(
  breaks = c(0, 1, 2, 3, 4, 5),
  colors = c("#FFFFFF", "#f8b3c3", "#f68da6", "#f0668e", "#e74b7e", "#d6356f")
)

# 4. Row annotation (ARG Type and Rank)
row_meta <- read_excel(meta_file, sheet = 4)
row_meta <- row_meta[match(rownames(count_mat), row_meta$HMM.category), ]
stopifnot(!any(is.na(row_meta$HMM.category)))

type_levels <- c(
  "tetracycline", "multidrug", "bacitracin", "MLS",
  "trimethoprim", "vancomycin", "pleuromutilin_tiamulin", "mupirocin",
  "beta_lactam", "polymyxin", "novobiocin", "chloramphenicol"
)
rank_levels <- c("IV", "III", "I")

row_meta$Type <- factor(row_meta$Type, levels = type_levels)
row_meta$rank <- factor(row_meta$rank, levels = rank_levels)

type_colors <- c(
  "tetracycline"          = "#8DD3C7",
  "multidrug"             = "#FFED6F",
  "bacitracin"            = "#A8A4CE",
  "MLS"                   = "#FB8072",
  "trimethoprim"          = "#80B1D3",
  "vancomycin"            = "#FDB462",
  "pleuromutilin_tiamulin" = "#B3DE69",
  "mupirocin"             = "#FCCDE5",
  "beta_lactam"           = "#D9D9D9",
  "polymyxin"             = "#C394EF",
  "novobiocin"            = "#CCEBC5",
  "chloramphenicol"       = "#FFFFB3"
)
rank_colors <- c("IV" = "#CFE0EE", "III" = "#5F8FB4", "I" = "#274B66")

row_ha <- rowAnnotation(
  ARG_Type = row_meta$Type,
  Rank = row_meta$rank,
  col = list(ARG_Type = type_colors, Rank = rank_colors),
  annotation_name_gp = gpar(fontsize = 8),
  simple_anno_size = unit(3, "mm"),
  gap = unit(2, "mm")
)

# 5. Column annotation (Host species)
col_meta <- read_excel(meta_file, sheet = 5)
col_meta <- col_meta[match(colnames(count_mat), col_meta$sample), ]
stopifnot(!any(is.na(col_meta$sample)))

species_levels <- c("M. caudata", "M. baibacina", "M. sibirica", "M. himalayana")
col_meta$species <- gsub("^M ", "M. ", col_meta$species)
col_meta$species <- factor(col_meta$species, levels = species_levels)

species_colors <- c(
  "M. caudata"    = "#4793AF",
  "M. baibacina"  = "#FF6A6A",
  "M. sibirica"   = "#FFA500",
  "M. himalayana" = "#8A2BE2"
)

column_ha <- HeatmapAnnotation(
  Species = col_meta$species,
  col = list(Species = species_colors),
  annotation_name_gp = gpar(fontsize = 8),
  simple_anno_size = unit(3, "mm"),
  gap = unit(2, "mm")
)

# 6. Build heatmap object
ht <- Heatmap(
  count_mat,
  name = "Counts",
  col = col_fun,
  cluster_rows = TRUE,
  cluster_columns = FALSE,
  row_split = row_meta$Type,
  row_title = NULL,
  column_split = col_meta$species,
  column_title = NULL,
  column_gap = unit(0.7, "mm"),
  row_gap = unit(0.7, "mm"),
  show_row_dend = FALSE,
  show_row_names = TRUE,
  show_column_names = TRUE,
  row_names_side = "left",
  row_names_gp = gpar(fontsize = 10),
  column_names_gp = gpar(fontsize = 10),
  column_names_rot = 90,
  rect_gp = gpar(col = "grey90", lwd = 0.05),
  top_annotation = column_ha,
  left_annotation = row_ha
)

# 7. Export visualization to PDF
pdf(output_pdf, width = 10, height = 8)
draw(ht)
dev.off()