test_that("plot_sankey_polygon returns ggplot and keeps isolated nodes", {
  nodes <- data.frame(
    id = c("A", "B", "C", "No Treatment"),
    stage = c("L1", "L2", "L2", "L1"),
    label = c("A", "B", "C", "No Treatment"),
    stringsAsFactors = FALSE
  )

  links <- data.frame(
    source = c("A", "A"),
    target = c("B", "C"),
    value = c(10, 5),
    stringsAsFactors = FALSE
  )

  p <- plot_sankey_polygon(
    nodes = nodes,
    links = links,
    node_id = "id",
    node_stage = "stage",
    node_label = "label",
    orientation = "horizontal"
  )

  expect_s3_class(p, "ggplot")

  built_nodes <- ggplot2::layer_data(p, length(p$layers) - 1)
  expect_equal(nrow(built_nodes), 4)
  expect_false(any(is.na(built_nodes$xmin)))
  expect_false(any(is.na(built_nodes$ymin)))
})

test_that("plot_sankey_polygon supports vertical orientation and bottom baseline", {
  nodes <- data.frame(
    id = c("A", "B", "C"),
    stage = c("S1", "S2", "S2"),
    stringsAsFactors = FALSE
  )

  links <- data.frame(
    source = c("A", "A"),
    target = c("B", "C"),
    value = c(6, 4),
    stringsAsFactors = FALSE
  )

  p <- plot_sankey_polygon(
    nodes = nodes,
    links = links,
    orientation = "vertical",
    baseline = "bottom",
    scale_mode = "adaptive",
    shared_scale_max = 20,
    adaptive_max_multiplier = 2
  )

  expect_s3_class(p, "ggplot")
  expect_true(length(p$layers) >= 2)
})

test_that("plot_sankey_polygon aligns stage tops when baseline is top", {
  nodes <- data.frame(
    id = c("L1_A", "L1_No", "L2_A", "L2_No"),
    stage = c("L1", "L1", "L2", "L2"),
    node_n = c(80, 20, 60, 10),
    stringsAsFactors = FALSE
  )

  links <- data.frame(
    source = c("L1_A"),
    target = c("L2_A"),
    value = c(50),
    stringsAsFactors = FALSE
  )

  p <- plot_sankey_polygon(
    nodes = nodes,
    links = links,
    node_value = "node_n",
    baseline = "top",
    show_labels = FALSE
  )

  rect <- ggplot2::layer_data(p, 2)
  # x is stage axis in horizontal orientation; two unique stages around 0 and 1
  l1_top <- max(rect$ymax[rect$xmin < 0.5])
  l2_top <- max(rect$ymax[rect$xmin > 0.5])
  expect_equal(l1_top, l2_top, tolerance = 1e-8)
})

test_that("plot_sankey_polygon keeps input order as top-to-bottom when baseline is top", {
  nodes <- data.frame(
    id = c("L1_A", "L1_B", "L1_No", "L2_A"),
    stage = c("L1", "L1", "L1", "L2"),
    label = c("A", "B", "No Treatment", "A2"),
    node_n = c(40, 30, 20, 60),
    stringsAsFactors = FALSE
  )

  links <- data.frame(
    source = c("L1_A", "L1_B"),
    target = c("L2_A", "L2_A"),
    value = c(25, 15),
    stringsAsFactors = FALSE
  )

  p <- plot_sankey_polygon(
    nodes = nodes,
    links = links,
    node_label = "label",
    node_value = "node_n",
    baseline = "top",
    show_labels = FALSE
  )

  rect <- ggplot2::layer_data(p, 2)
  l1 <- rect[rect$xmin < 0.5, c("ymin", "ymax")]
  # Third node in input order (No Treatment) should be bottom-most in L1 stack.
  expect_equal(which.min(l1$ymin), 3)
})

test_that("plot_sankey_polygon validates shared scale input", {
  nodes <- data.frame(id = c("A", "B"), stage = c("S1", "S2"), stringsAsFactors = FALSE)
  links <- data.frame(source = "A", target = "B", value = 1, stringsAsFactors = FALSE)

  expect_error(
    plot_sankey_polygon(nodes, links, scale_mode = "shared"),
    "shared_scale_max"
  )
})

test_that("plot_sankey_polygon supports treatment color modes", {
  nodes <- data.frame(
    id = c("L1_Chemo", "L1_No", "L2_Chemo", "L2_No"),
    stage = c("L1", "L1", "L2", "L2"),
    line = c("L1", "L1", "L2", "L2"),
    treatment = c("Chemo", "No Treatment", "Chemo", "No Treatment"),
    stringsAsFactors = FALSE
  )

  links <- data.frame(
    source = c("L1_Chemo", "L1_Chemo"),
    target = c("L2_Chemo", "L2_No"),
    value = c(8, 2),
    stringsAsFactors = FALSE
  )

  p_global <- plot_sankey_polygon(
    nodes = nodes,
    links = links,
    node_treatment = "treatment",
    node_line = "line",
    treatment_color_mode = "across_lines",
    show_labels = FALSE
  )

  global_fill <- ggplot2::layer_data(p_global, 2)$fill
  expect_equal(global_fill[1], global_fill[3])

  p_by_line <- plot_sankey_polygon(
    nodes = nodes,
    links = links,
    node_treatment = "treatment",
    node_line = "line",
    treatment_color_mode = "by_line",
    show_labels = FALSE
  )

  by_line_fill <- ggplot2::layer_data(p_by_line, 2)$fill
  expect_false(identical(by_line_fill[1], by_line_fill[3]))
})