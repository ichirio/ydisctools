library(ggplot2)
library(rlang)

# Load plotting function without requiring full package load.
.data <- rlang::.data
source("R/plot_sankey_polygon.R")
source("R/plot_sankey_subgroups_batch.R")

nodes <- data.frame(
  id = c(
    "L1_Chemo", "L1_IO", "L1_Target", "L1_No_Treatment",
    "L2_Chemo", "L2_IO", "L2_Target", "L2_BSC",
    "L3_Chemo", "L3_IO", "L3_Target", "L3_BSC",
    "L4_Chemo", "L4_IO", "L4_Target", "L4_BSC",
    "L5_Chemo", "L5_IO", "L5_Target", "L5_BSC"
  ),
  stage = c(
    rep("Line1", 4),
    rep("Line2", 4),
    rep("Line3", 4),
    rep("Line4", 4),
    rep("Line5", 4)
  ),
  line = c(
    rep("Line1", 4),
    rep("Line2", 4),
    rep("Line3", 4),
    rep("Line4", 4),
    rep("Line5", 4)
  ),
  treatment = rep(c("Chemo", "Immunotherapy", "Targeted", "No Treatment"), 5),
  label = c(
    "Line1: Chemo", "Line1: Immunotherapy", "Line1: Targeted", "Line1: No Treatment",
    "Line2: Chemo", "Line2: Immunotherapy", "Line2: Targeted", "Line2: No Treatment",
    "Line3: Chemo", "Line3: Immunotherapy", "Line3: Targeted", "Line3: No Treatment",
    "Line4: Chemo", "Line4: Immunotherapy", "Line4: Targeted", "Line4: No Treatment",
    "Line5: Chemo", "Line5: Immunotherapy", "Line5: Targeted", "Line5: No Treatment"
  ),
  node_n = c(
    NA, NA, NA, 20,
    NA, NA, NA, 35,
    NA, NA, NA, 40,
    NA, NA, NA, 34,
    NA, NA, NA, 21
  ),
  stringsAsFactors = FALSE
)

# Keep top alignment and force "No Treatment" to the bottom of each line.
line_levels <- paste0("Line", 1:5)
trt_order <- c("Chemo", "Immunotherapy", "Targeted", "No Treatment")
nodes$line <- factor(nodes$line, levels = line_levels)
nodes$treatment <- factor(nodes$treatment, levels = trt_order)
nodes <- nodes[order(nodes$line, nodes$treatment), , drop = FALSE]
nodes$line <- as.character(nodes$line)
nodes$treatment <- as.character(nodes$treatment)

links <- data.frame(
  source = c(
    "L1_Chemo", "L1_Chemo", "L1_Chemo", "L1_Chemo",
    "L1_IO", "L1_IO", "L1_IO", "L1_IO",
    "L1_Target", "L1_Target", "L1_Target", "L1_Target",

    "L2_Chemo", "L2_Chemo", "L2_Chemo", "L2_Chemo",
    "L2_IO", "L2_IO", "L2_IO", "L2_IO",
    "L2_Target", "L2_Target", "L2_Target", "L2_Target",

    "L3_Chemo", "L3_Chemo", "L3_Chemo", "L3_Chemo",
    "L3_IO", "L3_IO", "L3_IO", "L3_IO",
    "L3_Target", "L3_Target", "L3_Target", "L3_Target",

    "L4_Chemo", "L4_Chemo", "L4_Chemo", "L4_Chemo",
    "L4_IO", "L4_IO", "L4_IO", "L4_IO",
    "L4_Target", "L4_Target", "L4_Target", "L4_Target"
  ),
  target = c(
    "L2_Chemo", "L2_IO", "L2_Target", "L2_BSC",
    "L2_Chemo", "L2_IO", "L2_Target", "L2_BSC",
    "L2_Chemo", "L2_IO", "L2_Target", "L2_BSC",

    "L3_Chemo", "L3_IO", "L3_Target", "L3_BSC",
    "L3_Chemo", "L3_IO", "L3_Target", "L3_BSC",
    "L3_Chemo", "L3_IO", "L3_Target", "L3_BSC",

    "L4_Chemo", "L4_IO", "L4_Target", "L4_BSC",
    "L4_Chemo", "L4_IO", "L4_Target", "L4_BSC",
    "L4_Chemo", "L4_IO", "L4_Target", "L4_BSC",

    "L5_Chemo", "L5_IO", "L5_Target", "L5_BSC",
    "L5_Chemo", "L5_IO", "L5_Target", "L5_BSC",
    "L5_Chemo", "L5_IO", "L5_Target", "L5_BSC"
  ),
  value = c(
    70, 30, 10, 15,
    35, 30, 10, 15,
    15, 15, 15, 5,

    50, 20, 8, 20,
    18, 24, 8, 15,
    8, 8, 8, 5,

    30, 12, 5, 18,
    10, 15, 5, 12,
    5, 5, 6, 4,

    18, 8, 3, 10,
    6, 10, 3, 8,
    2, 3, 4, 3
  ),
  stringsAsFactors = FALSE
)

nodes_full <- nodes
nodes_full$scenario <- "full"

nodes_sub <- nodes
nodes_sub$scenario <- "subgroup"
nodes_sub$node_n <- ifelse(is.na(nodes_sub$node_n), NA, pmax(1, round(nodes_sub$node_n * 0.5)))

nodes_all <- rbind(nodes_full, nodes_sub)

# Build a long-format link table so subgroup filters can be passed as expressions.
links_full <- links
links_full$scenario <- "full"

links_sub <- links
links_sub$value <- pmax(1, round(links_sub$value * 0.5))
links_sub$scenario <- "subgroup"

links_all <- rbind(links_full, links_sub)

treatment_palette <- c(
  "Chemo" = "#2F6C8F",
  "Immunotherapy" = "#4B9F7A",
  "Targeted" = "#D08C3E",
  "No Treatment" = "#7F7F7F"
)

specs <- data.frame(
  subgroup = c("full", "subgroup"),
  node_filter = c("scenario == 'full'", "scenario == 'subgroup'"),
  link_filter = c("scenario == 'full'", "scenario == 'subgroup'"),
  title = c(
    "Cancer Therapy Transition: Line1 to Line5 (Sample) - Full cohort",
    "Cancer Therapy Transition: Line1 to Line5 (Sample) - Subgroup (~50% N)"
  ),
  file_name = c(
    "sankey_line1_line5_sample_full_shared_scale.png",
    "sankey_line1_line5_sample_subgroup_shared_scale.png"
  ),
  stringsAsFactors = FALSE
)

result <- plot_sankey_subgroups_batch(
  nodes = nodes_all,
  links = links_all,
  subgroup_specs = specs,
  node_id = "id",
  node_stage = "stage",
  node_label = "label",
  node_value = "node_n",
  link_source = "source",
  link_target = "target",
  link_value = "value",
  scale_strategy = "shared_first_stage",
  scale_reference_subgroup = "full",
  output_dir = "output",
  save_png = TRUE,
  width = 14,
  height = 8,
  dpi = 180,
  bg = "white",
  node_treatment = "treatment",
  node_line = "line",
  treatment_color_mode = "across_lines",
  treatment_palette = treatment_palette,
  orientation = "horizontal",
  baseline = "top",
  use_link_color_by_source = TRUE,
  node_width = 0.22,
  node_gap = 0.03,
  link_curvature = 0.48,
  link_alpha = 0.55,
  label_size = 3.1,
  label_color = "#1F2A30",
  label_nudge = 0.045
)

for (f in result$metadata$file_name) {
  cat(normalizePath(file.path("output", f), winslash = "/"), "\n")
}
