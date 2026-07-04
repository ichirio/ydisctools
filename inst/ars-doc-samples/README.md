# Synthetic sample SAP / CSR template

Two Word documents used by the *From the SAP to the ARS* article and the
test suite to demonstrate the SAP -> TOC -> shell -> ARS chain:

| File | What it plays | Demo twist |
|------|---------------|------------|
| `sample_csr_template.docx` | a company CSR template | its display section is **15** (not ICH E3's 14), so `read_csr_map()` visibly drives sponsor-specific numbering |
| `sample_sap.docx` | a study SAP | the planned-display appendix table leaves the numbers **blank**, so `ars_from_toc()` demonstrates rule-based auto-numbering |

Regenerate with `Rscript make_doc_samples.R` from this folder (needs
{officer}).

## Provenance and licence

Both documents are **original synthetic samples authored for ydisctools**.
Publicly available real-world templates could not be bundled: the
TransCelerate Clinical Template Suite carries no redistribution licence,
SAPs posted on clinicaltrials.gov remain sponsor-owned, and ICH / PMDA
guideline PDFs are the respective organisations' text.  Only the
*section layout* here is informed by the public ICH E3 guideline structure
and the general outline of industry SAP templates -- document structure is
not subject to copyright, and every sentence in these files was written for
this package.  The files are therefore distributed under the package
licence (GPL >= 3).
