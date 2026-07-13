% Generated manually for ydisctools
\name{plot_sankey}
\alias{plot_sankey}
\alias{plot_sankey_polygon}
\title{Plot Sankey Diagram with Rectangular Nodes and Bezier Polygon Links}
\usage{
plot_sankey(
  nodes,
  links,
  node_id = "id",
  node_stage = "stage",
  node_label = NULL,
  node_value = NULL,
  link_source = "source",
  link_target = "target",
  link_value = "value",
  orientation = c("horizontal", "vertical"),
  baseline = c("top", "bottom"),
  scale_mode = c("auto", "shared", "adaptive"),
  shared_scale_max = NULL,
  adaptive_max_multiplier = 3,
  node_width = 0.18,
  node_gap = 0.03,
  node_min_size = 0.015,
  link_curvature = 0.45,
  bezier_n = 60,
  node_fill = "#4F6D7A",
  node_treatment = NULL,
  node_line = NULL,
  treatment_color_mode = c("across_lines", "by_line"),
  treatment_palette = NULL,
  node_color = "white",
  node_alpha = 1,
  link_fill = "#8AA1B1",
  link_color = NA,
  link_alpha = 0.6,
  show_labels = TRUE,
  label_size = 3,
  label_color = "#1F2A30",
  label_nudge = 0.03,
  label_position = c("right", "left"),
  use_link_color_by_source = FALSE,
  use_link_color_by_target = FALSE
)
}
\arguments{
\item{nodes}{A data frame of nodes.}
\item{links}{A data frame of links.}
\item{node_id}{Column name in \code{nodes} for unique node IDs.}
\item{node_stage}{Column name in \code{nodes} for stage/layer.}
\item{node_label}{Optional column name in \code{nodes} for labels.}
\item{node_value}{Optional column name in \code{nodes} for node values. If \code{NULL}, values are derived from links.}
\item{link_source}{Column name in \code{links} for source node IDs.}
\item{link_target}{Column name in \code{links} for target node IDs.}
\item{link_value}{Column name in \code{links} for link values.}
\item{orientation}{Plot direction: \code{"horizontal"} or \code{"vertical"}.}
\item{baseline}{Node stacking baseline: \code{"top"} or \code{"bottom"}.}
\item{scale_mode}{Scale behavior: \code{"auto"}, \code{"shared"}, or \code{"adaptive"}.}
\item{shared_scale_max}{Shared maximum span used in \code{"shared"} mode and as reference in \code{"adaptive"} mode.}
\item{adaptive_max_multiplier}{Maximum magnification in \code{"adaptive"} mode. Effective minimum span is \code{shared_scale_max / adaptive_max_multiplier}.}
\item{node_width}{Width of each node along stage axis.}
\item{node_gap}{Gap between nodes in the same stage.}
\item{node_min_size}{Minimum visible node size (keeps isolated zero-value nodes visible).}
\item{link_curvature}{Bezier curvature factor in \code{[0, 1]}.}
\item{bezier_n}{Number of points used to sample Bezier curves.}
\item{node_fill}{Node fill color, or a column name in \code{nodes} containing colors.}
\item{node_treatment}{Optional column name in \code{nodes} for treatment class.}
\item{node_line}{Optional column name in \code{nodes} for line identifier used when \code{treatment_color_mode = "by_line"}.}
\item{treatment_color_mode}{Treatment color mapping mode: \code{"across_lines"} or \code{"by_line"}.}
\item{treatment_palette}{Optional color palette (character vector). Named vectors are matched by treatment names.}
\item{node_color}{Node border color.}
\item{node_alpha}{Node alpha.}
\item{link_fill}{Link fill color, or a column name in \code{links} containing colors.}
\item{link_color}{Link border color.}
\item{link_alpha}{Link alpha.}
\item{show_labels}{Whether to draw labels.}
\item{label_size}{Label size.}
\item{label_color}{Label color.}
\item{label_nudge}{Label offset from node edge.}
\item{label_position}{Side of the node the labels are drawn on, along the
stage axis: \code{"right"} (default) or \code{"left"}. For
\code{orientation = "vertical"} this maps to above (\code{"right"}) / below
(\code{"left"}) the nodes. When labels are shown, the panel is extended on
the label side of the terminal stage by the span a link occupies between two
adjacent stages (\code{1 - node_width}) plus \code{label_nudge}, so
terminal-stage labels get the same room as the ones drawn between stages
instead of being clipped at the device edge.}
\item{use_link_color_by_source}{If \code{TRUE}, link fill uses source node color.}
\item{use_link_color_by_target}{If \code{TRUE}, link fill uses target node color.}
}
\value{A \code{ggplot} object.}
\description{
Draw a Sankey-style plot using \code{ggplot2} primitives (\code{geom_rect} for
nodes and \code{geom_polygon} for links).

A key feature of this implementation is that nodes are laid out from the node
table rather than inferred from the links, so a node with \strong{no links at
all} is still drawn, sized by its \code{node_value}. For example, a
\dQuote{L1: No Treatment} group that no patients enter or leave still
appears in the diagram as a standalone node (see the example below).
}
\examples{
# A five-line oncology treatment-sequence cohort (Line 1 -> Line 5).
#
# Highlighted feature: nodes are laid out from the node table, so a node with
# NO links at all is still drawn, sized by `node_value`. Here every
# "No Treatment" node (e.g. "L1: No Treatment") is such an isolated node --
# no patients flow into or out of it -- yet it appears in the diagram. Linked
# nodes are left as NA and are sized from the link values.
trt <- c("Chemo", "Immunotherapy", "Targeted", "No Treatment")
key <- c("Chemo", "IO", "Target", "NoTx")

nodes <- data.frame(
  id        = paste0("L", rep(1:5, each = 4), "_", rep(key, 5)),
  stage     = paste0("Line", rep(1:5, each = 4)),
  line      = paste0("Line", rep(1:5, each = 4)),
  treatment = rep(trt, 5),
  label     = paste0("L", rep(1:5, each = 4), ": ", rep(trt, 5)),
  node_n    = NA_real_,
  stringsAsFactors = FALSE
)
# Give the link-less "No Treatment" nodes an explicit size.
nodes$node_n[nodes$treatment == "No Treatment"] <- c(20, 18, 15, 12, 10)

# Patients flow only among the active treatments between consecutive lines;
# the "No Treatment" nodes deliberately have no links.
active <- c("Chemo", "IO", "Target")
flow <- c(45, 8, 4, 10, 28, 5, 6, 6, 16) # source-by-target, tapering per line
links <- do.call(rbind, lapply(1:4, function(k) {
  g <- expand.grid(s = active, t = active, stringsAsFactors = FALSE)
  data.frame(
    source = paste0("L", k, "_", g$s),
    target = paste0("L", k + 1, "_", g$t),
    value  = round(flow * 0.75^(k - 1)),
    stringsAsFactors = FALSE
  )
}))

treatment_palette <- c(
  "Chemo"         = "#2F6C8F",
  "Immunotherapy" = "#4B9F7A",
  "Targeted"      = "#D08C3E",
  "No Treatment"  = "#7F7F7F"
)

plot_sankey(
  nodes = nodes,
  links = links,
  node_id = "id",
  node_stage = "stage",
  node_label = "label",
  node_value = "node_n",
  link_source = "source",
  link_target = "target",
  link_value = "value",
  node_treatment = "treatment",
  node_line = "line",
  treatment_color_mode = "across_lines",
  treatment_palette = treatment_palette,
  baseline = "top",
  use_link_color_by_source = TRUE,
  link_alpha = 0.55,
  label_size = 3
)
}
