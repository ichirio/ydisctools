% Generated manually for ydisctools
\name{plot_sunburst}
\alias{plot_sunburst}
\title{Plot a Static Sunburst of Treatment Sequences}
\usage{
plot_sunburst(
  paths,
  path_cols = NULL,
  path_value = "n",
  levels = NULL,
  segment_labels = NULL,
  palette = NULL,
  segment_color = "white",
  ring_height = 0.92,
  inner_radius = 1,
  show_labels = TRUE,
  label_size = 2.6,
  label_color = "white",
  label_min_frac = 0.02,
  center_label = NULL,
  center_size = 4.2,
  center_color = "#1F2A30"
)
}
\arguments{
\item{paths}{A data frame of treatment sequences (one row per path).}
\item{path_cols}{Character vector naming the ring columns of \code{paths}, in
ring order (inner first). Default: every column except \code{path_value}, in
the order they appear.}
\item{path_value}{Column name in \code{paths} holding the patient count of
each path.}
\item{levels}{Optional character vector fixing the order of the segment
categories within every ring. Default: order of first appearance across the
ring columns.}
\item{segment_labels}{Optional named character vector mapping a category to
the display text used on the arcs and in the legend (e.g.
\code{c(IO = "Immunotherapy")}). Unmapped categories show verbatim.}
\item{palette}{Optional colour palette for the categories. A named vector is
matched by category name; an unnamed vector is recycled over \code{levels}.
Default: \code{grDevices::hcl.colors()} "Set 2".}
\item{segment_color}{Border colour of the arcs.}
\item{ring_height}{Radial thickness of each ring, in \code{(0, 1]} ring
units.}
\item{inner_radius}{Radius of the centre hole, in ring units.}
\item{show_labels}{Whether to draw the arc labels.}
\item{label_size}{Arc label size.}
\item{label_color}{Arc label colour.}
\item{label_min_frac}{Minimum share of the full circle an arc must span for
its label to be drawn (labels on thinner arcs are suppressed).}
\item{center_label}{Optional text drawn in the centre hole (e.g.
\code{"STUDY01\\nN = 200"}).}
\item{center_size}{Centre label size.}
\item{center_color}{Centre label colour.}
}
\value{A \code{ggplot} object.}
\description{
Draw a treatment-pattern sunburst (ring chart) using \code{ggplot2}
primitives only (\code{geom_rect} + \code{coord_polar} + \code{geom_text}) --
the output is a fully static \code{ggplot} object suitable for publications
and regulatory documents.

Each ring is one treatment line (the innermost ring is the first line) and
the arc length is the number of patients (one patient = one angular unit, so
no normalisation is applied). The child arcs of a segment start at the
parent's start angle and nest inside the parent's span; whatever remains of
the parent's span stays \strong{empty} -- those are the patients with no
further treatment line (attrition), the same idea as the unlinked part of a
\code{\link{plot_sankey}} node.

The input is a \strong{path table}: one row per treatment sequence, with one
column per ring (\code{path_cols}) and a count column (\code{path_value}).
\code{NA} (or \code{""}) in a ring column means the path stops before that
ring; once a path has stopped it must stay \code{NA} in all later columns.
Rows describing the same sequence are summed.
}
\examples{
# Treatment sequences over the first three lines of the same 200-patient
# cohort as the plot_sankey() example -- the ring totals of this path table
# reproduce that example's transition matrices exactly (L1 200 -> L2 120 ->
# L3 70, about 30\% "No Treatment" per line, no same-treatment transitions).
# A name like "C,IO" is a path that stops after the second line; the
# missing span outside an arc is the attrition.
seq_n <- c(
  "C,IO,C" = 11, "C,IO,T" = 2, "C,IO,N" = 4, "C,IO" = 3,
  "C,T,C" = 4, "C,T,IO" = 3, "C,T,N" = 2, "C,T" = 5,
  "C,N" = 24, "C" = 12,
  "IO,C,IO" = 10, "IO,C,T" = 7, "IO,C,N" = 9, "IO,C" = 2,
  "IO,T,C" = 1, "IO,T,IO" = 1, "IO,T,N" = 1, "IO,T" = 1,
  "IO,N" = 8, "IO" = 5,
  "T,C,IO" = 3, "T,C,T" = 2, "T,C,N" = 3, "T,C" = 2,
  "T,IO,C" = 4, "T,IO,T" = 1, "T,IO,N" = 2, "T,IO" = 1,
  "T,N" = 4, "T" = 3,
  "N" = 60
)
parts <- strsplit(names(seq_n), ",")
paths <- data.frame(
  line1 = vapply(parts, `[`, character(1), 1),
  line2 = vapply(parts, `[`, character(1), 2),
  line3 = vapply(parts, `[`, character(1), 3),
  n = unname(seq_n),
  stringsAsFactors = FALSE
)

treatment_palette <- c(
  "C"  = "#2F6C8F",
  "IO" = "#4B9F7A",
  "T"  = "#D08C3E",
  "N"  = "#7F7F7F"
)

plot_sunburst(
  paths,
  path_cols = c("line1", "line2", "line3"),
  path_value = "n",
  levels = c("C", "IO", "T", "N"),
  segment_labels = c(C = "Chemo", IO = "IO", T = "Target", N = "NoTx"),
  palette = treatment_palette,
  center_label = "STUDY01\nN = 200"
)
}
\seealso{\code{\link{plot_sankey}}}
