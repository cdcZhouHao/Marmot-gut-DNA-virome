library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Configuration & file paths
input_file  <- "annotation_FPKM.xlsx"
output_pdf  <- "bubble_plot_annotated.pdf"

# Grouping and color configuration
group_lookup <- c(
  "H" = "M. baibacina",
  "M" = "M. sibirica",
  "C" = "M. caudata",
  "X" = "M. himalayana"
)

group_colors <- c(
  "M. baibacina"  = "#0073C2",
  "M. sibirica"   = "#c74732",
  "M. caudata"    = "#0f6657",
  "M. himalayana" = "#edae11"
)

realm_colors <- c(
  "Adnaviria"     = "#e41a1c",
  "Duplodnaviria" = "#377eb8",
  "Monodnaviria"  = "#4daf4a",
  "Riboviria"     = "#984ea3",
  "Varidnaviria"  = "#a65628",
  "Unassigned"    = "#ff7f00"
)

species_order <- c(
  "Ungulaviridae", "Lipothrixviridae", "Ahmunviridae", "Autonotataviridae", "Autotranscriptaviridae",
  "Autoscriptoviridae", "Steigviridae", "Suoliviridae", "Intestiviridae", "Crevaviridae", "Lakviridae",
  "Epsomviridae", "Orthoherpesviridae", "Alloherpesviridae", "Malacoherpesviridae", "Yangangviridae",
  "Krittikaviridae", "Armatusviridae", "Usuviridae", "Pigerviridae", "Kairosviridae", "Anaerodiviridae",
  "Leisingerviridae", "Ekchuahviridae", "Ahpuchviridae", "Soleiviridae", "Connertonviridae",
  "Mazoviaviridae", "Toyamaviridae", "Ludisviridae", "Clermontviridae", "Fuxiviridae", "Forsetiviridae",
  "Saffermanviridae", "Madridviridae", "Alisviridae", "Herelleviridae", "Felixviridae", "Trautnerviridae",
  "Speroviridae", "Vandenendeviridae", "Winoviridae", "Guelinviridae", "Nixviridae", "Helgolandviridae",
  "Salasmaviridae", "Peduoviridae", "Schitoviridae", "Pungoviridae", "Aliceevansviridae", "Madisaviridae",
  "Lutetiaviridae", "Konodaiviridae", "Zobellviridae", "Chimalliviridae", "Molycolviridae",
  "Drexlerviridae", "Kunpengviridae", "Umezonoviridae", "Casidaviridae", "Suolaviridae", "Lindbergviridae",
  "Orlajensenviridae", "Rountreeviridae", "Kleczkowskaviridae", "Naomviridae", "Duneviridae",
  "Arenbergviridae", "Ehrlichviridae", "Grimontviridae", "Casjensviridae", "Kruegerviridae", "Vertoviridae",
  "Mesyanzhinovviridae", "Demerecviridae", "Straboviridae", "Naryaviridae", "Kanorauviridae",
  "Draupnirviridae", "Adamaviridae", "Circoviridae", "Vilyaviridae", "Pecoviridae", "Oomyviridae",
  "Smacoviridae", "Kirkoviridae", "Thalassapleoviridae", "Inoviridae", "Plectroviridae", "Microviridae",
  "Parvoviridae", "Geplanaviridae", "Genomoviridae", "Geminiviridae", "Retroviridae", "Caulimoviridae",
  "Closteroviridae", "Nipumfusiviridae", "Itzamnaviridae", "Plasmaviridae", "Nudiviridae",
  "Spiraviridae", "Huangdiviridae", "Alphasatellitidae", "Bicaudaviridae", "Finnlakeviridae",
  "Autolykiviridae", "Parnassusviridae", "Mestraviridae", "Asemoviridae", "Phycodnaviridae",
  "Schizomimiviridae", "Mimiviridae", "Ascoviridae", "Pithoviridae", "Iridoviridae", "Poxviridae",
  "Tectiviridae", "Burtonviroviridae"
)

# 2. Data loading and transformation
raw_fpkm <- read_excel(input_file, sheet = "Sheet1")
colnames(raw_fpkm)[1] <- "Species"

realm_df <- read_excel(input_file, sheet = "Sheet3")
colnames(realm_df) <- c("Realm", "Species")

sample_cols <- c(
  paste0("X", 1:6),
  paste0("C", 1:6),
  paste0("M", 1:6),
  paste0("H", 1:6)
)

# Aggregate duplicate species, filter unassigned, and pivot to long format
data_long <- raw_fpkm %>%
  group_by(Species) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
  ungroup() %>%
  filter(Species != "Unassigned") %>%
  pivot_longer(
    cols = -Species,
    names_to = "Sample",
    values_to = "Abundance"
  ) %>%
  mutate(
    Prefix = substr(Sample, 1, 1),
    Group = recode(Prefix, !!!group_lookup),
    Group = factor(Group, levels = group_lookup),
    Sample = factor(Sample, levels = sample_cols),
    Species = factor(Species, levels = rev(species_order))
  ) %>%
  left_join(realm_df, by = "Species") %>%
  filter(Abundance > 0)

# Dynamic scale breaks for abundance sizes
max_abundance <- max(data_long$Abundance, na.rm = TRUE)
legend_breaks <- unique(ceiling(seq(0, max_abundance, length.out = 8)))

# 3. Generate faceted bubble chart
p <- ggplot(data_long, aes(x = Sample, y = Species, size = Abundance, color = Group)) +
  geom_point(alpha = 0.75) +
  scale_color_manual(values = group_colors) +
  scale_size(range = c(0.8, 7), breaks = legend_breaks) +
  facet_wrap(~Group, ncol = 4, scales = "free_x") +
  labs(
    x = "Samples",
    y = "Viral Family",
    size = "Abundance (FPKM)",
    color = "Host Species"
  ) +
  theme_bw(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
    axis.text.y = element_text(size = 5, color = "black"),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  ) +
  guides(color = guide_legend(override.aes = list(size = 4)))

# 4. Export visualization
ggsave(
  filename = output_pdf,
  plot = p,
  width = 8,
  height = 11,
  units = "in",
  device = "pdf"
)