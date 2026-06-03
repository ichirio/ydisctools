# Sankey Guide

This guide describes how to use `plot_sankey()` for treatment transition plots.

## Key features

- Node: `geom_rect`
- Link: Bezier-based `geom_polygon`
- Supports isolated nodes (nodes with no links)
- Orientation: `horizontal` / `vertical`
- Baseline alignment: `top` / `bottom`
- Scale modes: `auto`, `shared`, `adaptive`
- Treatment color mode:
  - `across_lines` (default): same treatment keeps same color across stages
  - `by_line`: colors are reassigned by stage

## Example script

- `output/create_line1_line5_sankey_sample.R`

Run:

```r
Rscript output/create_line1_line5_sankey_sample.R
```

Output files:

- `output/sankey_line1_line5_sample_across_lines.png`
- `output/sankey_line1_line5_sample_by_line.png`

## Minimal call

```r
p <- plot_sankey(
  nodes = nodes,
  links = links,
  node_id = "id",
  node_stage = "stage",
  node_label = "label",
  node_value = "node_n",
  node_treatment = "treatment",
  treatment_color_mode = "across_lines",
  link_source = "source",
  link_target = "target",
  link_value = "value",
  baseline = "top"
)
```
