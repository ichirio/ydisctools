library(ggplot2)
library(rlang)

# Load plotting function without requiring full package load.
.data <- rlang::.data
source("R/plot_sankey_polygon.R")

# Scenario options to render.
scenario_names <- c("full", "subgroup")

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

treatment_palette <- c(
  "Chemo" = "#2F6C8F",
  "Immunotherapy" = "#4B9F7A",
  "Targeted" = "#D08C3E",
  "No Treatment" = "#7F7F7F"
)

compute_scale_max <- function(nodes_df, links_df, node_gap = 0.03) {
  out_sum <- aggregate(value ~ source, data = links_df, FUN = sum)
  names(out_sum) <- c("id", "outflow")
  in_sum <- aggregate(value ~ target, data = links_df, FUN = sum)
  names(in_sum) <- c("id", "inflow")

  tmp <- merge(nodes_df, out_sum, by = "id", all.x = TRUE)
  tmp <- merge(tmp, in_sum, by = "id", all.x = TRUE)
  tmp$outflow[is.na(tmp$outflow)] <- 0
  tmp$inflow[is.na(tmp$inflow)] <- 0

  base_value <- tmp$node_n
  base_value[is.na(base_value)] <- 0
  display_value <- pmax(base_value, tmp$inflow, tmp$outflow)

  stage_sum <- tapply(display_value, tmp$stage, sum)
  stage_n <- table(tmp$stage)
  stage_span <- as.numeric(stage_sum) + pmax(0, as.numeric(stage_n) - 1) * node_gap

  max(stage_span)
}

subgroup_ratio <- 0.5
nodes_sub <- nodes
links_sub <- links
links_sub$value <- pmax(1, round(links_sub$value * subgroup_ratio))
nodes_sub$node_n <- ifelse(is.na(nodes_sub$node_n), NA, pmax(1, round(nodes_sub$node_n * subgroup_ratio)))

shared_scale_max <- max(
  compute_scale_max(nodes, links, node_gap = 0.03),
  compute_scale_max(nodes_sub, links_sub, node_gap = 0.03)
)

scenario_data <- list(
  full = list(nodes = nodes, links = links, subtitle = "Full cohort"),
  subgroup = list(nodes = nodes_sub, links = links_sub, subtitle = "Subgroup (~50% N)")
)

for (scenario_name in scenario_names) {
  obj <- scenario_data[[scenario_name]]

  p <- plot_sankey_polygon(
    nodes = obj$nodes,
    links = obj$links,
    node_id = "id",
    node_stage = "stage",
    node_label = "label",
    node_value = "node_n",
    node_treatment = "treatment",
    node_line = "line",
    treatment_color_mode = "across_lines",
    treatment_palette = treatment_palette,
    link_source = "source",
    link_target = "target",
    link_value = "value",
    orientation = "horizontal",
    baseline = "top",
    scale_mode = "shared",
    shared_scale_max = shared_scale_max,
    use_link_color_by_source = TRUE,
    node_width = 0.22,
    node_gap = 0.03,
    link_curvature = 0.48,
    link_alpha = 0.55,
    label_size = 3.1,
    label_color = "#1F2A30",
    label_nudge = 0.045
  ) +
    ggplot2::labs(
      title = "Cancer Therapy Transition: Line1 to Line5 (Sample)",
      subtitle = paste0("Sankey-style plot with isolated no-treatment nodes (", obj$subtitle, ", shared scale)"),
      caption = "Sample synthetic data"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 15),
      plot.subtitle = ggplot2::element_text(size = 11),
      plot.caption = ggplot2::element_text(size = 9, colour = "#555555"),
      plot.margin = ggplot2::margin(15, 100, 15, 15)
    )

  out_png <- file.path("output", paste0("sankey_line1_line5_sample_", scenario_name, "_shared_scale.png"))

  ggplot2::ggsave(
    filename = out_png,
    plot = p,
    width = 14,
    height = 8,
    dpi = 180,
    bg = "white"
  )

  cat(normalizePath(out_png, winslash = "/"), "\n")
}
