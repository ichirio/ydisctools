# ARS method library (vendored from siera)

`method-library.json` is a verbatim copy of the analysis-method template
catalog shipped by the [siera](https://github.com/clymbclinical/siera) R
package (`inst/method-library/method-library.json`), Copyright (c) siera
authors (Clymb Clinical), distributed under the MIT license.

It provides, for each analysis method (`total_n`, `categorical_summary`,
`continuous_summary`, `risk_difference`, ...):

- the ARS `AnalysisMethods` metadata (name, label, description, operations),
- the `{cards}`/`{cardx}` R code template (`templateCode`), and
- the code-template parameters with their siera `valueSource` bindings.

`ydisctools::build_ars()` expands entries from this catalog into the
`AnalysisMethods` / `AnalysisMethodCodeTemplate` /
`AnalysisMethodCodeParameters` sheets of a generated ARS workbook, so that
users of the compact parameter format never have to author method template
code by hand, and so that `siera::readARS()` is guaranteed to understand the
result.

To refresh the catalog, copy the file from a newer siera release and re-run
the ydisctools test suite (`tests/testthat/test-ars-tools.R` exercises every
method key the compact format supports).

MIT license text of the upstream package:
<https://github.com/clymbclinical/siera/blob/main/LICENSE.md>
