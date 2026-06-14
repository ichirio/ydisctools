# ydisctools (development version)

`ydisctools` is a Proof-of-Concept collection of small CDISC tools and is
not versioned for release; this changelog tracks notable changes only.

## Site & tooling

* Added a [pkgdown](https://pkgdown.r-lib.org/) documentation site published to
  GitHub Pages at <https://ichirio.github.io/ydisctools/>, replacing the earlier
  hand-built HTML pages. The site mirrors the
  [rtfreporter](https://github.com/ichirio/rtfreporter) design language with an
  indigo theme, hex logo, and favicons.

## Tools

* `plot_sankey()` — treatment-transition Sankey plotter (Bezier links,
  rectangle nodes, isolated-node support, per-line / across-line colouring),
  plus `plot_sankey_subgroups_batch()` for subgroup batches.
