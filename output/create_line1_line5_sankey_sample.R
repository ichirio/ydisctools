library(devtools)
library(ggplot2)

# Load current source (including plot_sankey_polygon added in this workspace)
devtools::load_all(".", quiet = TRUE)

# Color mode options to render.
color_modes <- c("across_lines", "by_line")

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

for (treatment_color_mode in color_modes) {
  p <- plot_sankey_polygon(
    nodes = nodes,
    links = links,
    node_id = "id",
    node_stage = "stage",
    node_label = "label",
    node_value = "node_n",
    node_treatment = "treatment",
    node_line = "line",
    treatment_color_mode = treatment_color_mode,
    treatment_palette = treatment_palette,
    link_source = "source",
    link_target = "target",
    link_value = "value",
    orientation = "horizontal",
    baseline = "top",
    scale_mode = "auto",
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
      subtitle = paste0("Sankey-style plot with isolated no-treatment nodes (color mode: ", treatment_color_mode, ")"),
      caption = "Sample synthetic data"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 15),
      plot.subtitle = ggplot2::element_text(size = 11),
      plot.caption = ggplot2::element_text(size = 9, colour = "#555555"),
      plot.margin = ggplot2::margin(15, 100, 15, 15)
    )

  out_png <- file.path("output", paste0("sankey_line1_line5_sample_", treatment_color_mode, ".png"))

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
