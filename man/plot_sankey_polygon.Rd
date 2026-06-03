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
\item{use_link_color_by_source}{If \code{TRUE}, link fill uses source node color.}
\item{use_link_color_by_target}{If \code{TRUE}, link fill uses target node color.}
}
\value{A \code{ggplot} object.}
\description{
Draw a Sankey-style plot using \code{ggplot2} primitives (\code{geom_rect} for nodes and
\code{geom_polygon} for links). This implementation can draw nodes that have no incoming
or outgoing links.
}
\examples{
nodes <- data.frame(
  id = c("L1_Chemo", "L1_No", "L2_Chemo", "L2_No"),
  stage = c("L1", "L1", "L2", "L2"),
  treatment = c("Chemo", "No Treatment", "Chemo", "No Treatment"),
  stringsAsFactors = FALSE
)

links <- data.frame(
  source = c("L1_Chemo", "L1_Chemo"),
  target = c("L2_Chemo", "L2_No"),
  value = c(8, 2),
  stringsAsFactors = FALSE
)

plot_sankey(
  nodes = nodes,
  links = links,
  node_treatment = "treatment",
  treatment_color_mode = "across_lines",
  baseline = "top"
)
}
