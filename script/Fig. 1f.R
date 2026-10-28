library(readxl)
library(UpSetR)
library(tibble)

# 1. Configuration & file paths
input_file <- "Marmota_upset.xlsx"
output_pdf <- "upset_plot.pdf"

species_cols <- c("M. baibacina", "M. sibirica", "M. caudata", "M. himalayana")

set_colors <- c(
  "M. baibacina"  = "#FF6A6A",
  "M. sibirica"   = "#FFA500",
  "M. caudata"    = "#4793AF",
  "M. himalayana" = "#8A2BE2"
)

# 2. Data loading and transformation
data_raw <- read_excel(input_file, sheet = 1)

# Convert to standard binary data frame with row names
upset_data <- as.data.frame(column_to_rownames(data_raw, var = colnames(data_raw)[1]))
colnames(upset_data) <- species_cols

# Ensure all values are numeric (0/1)
upset_data[] <- lapply(upset_data, as.numeric)

# 3. Generate and export UpSet plot
cairo_pdf(output_pdf, width = 15, height = 7)

# Prevent potential leading blank page in UpSetR graphics device
plot.new()

upset(
  upset_data,
  nsets = length(species_cols),
  sets = species_cols,
  order.by = "freq",
  keep.order = TRUE,
  matrix.color = "#404040",
  point.size = 6,
  main.bar.color = "#878787",
  mainbar.y.label = "Intersection Size",
  sets.bar.color = adjustcolor(set_colors[species_cols], alpha.f = 0.7),
  sets.x.label = "Number of vOTUs",
  text.scale = c(2, 2, 1.5, 1.5, 1.5, 1.5)
)

dev.off()