# ============================================================================
#  make_doc_samples.R -- regenerate the bundled synthetic SAP / CSR template
# ============================================================================
#
#  Run FROM THIS FOLDER (inst/ars-doc-samples of the source tree):
#
#      Rscript make_doc_samples.R
#
#  Both documents are ORIGINAL synthetic samples authored for ydisctools --
#  the section layout is informed by the public ICH E3 guideline structure
#  and the general outline of industry SAP templates, but every sentence is
#  our own, so the files can ship under the package licence (see README.md).
#
#  Deliberate demo choices:
#    * the company CSR template files its display section under section 15
#      (NOT ICH E3's 14), so read_csr_map() visibly overrides the ICH
#      defaults downstream;
#    * the SAP's planned-display appendix leaves the display numbers blank,
#      so ars_from_toc() demonstrates rule-based auto-numbering with the
#      CSR-derived section map.
# ============================================================================

if (basename(getwd()) != "ars-doc-samples") {
  stop("Run this script from inst/ars-doc-samples (its own folder).")
}
if (!requireNamespace("officer", quietly = TRUE)) {
  stop("The officer package is required to regenerate the samples.")
}
library(officer)

h1 <- function(doc, x) body_add_par(doc, x, style = "heading 1")
h2 <- function(doc, x) body_add_par(doc, x, style = "heading 2")
p  <- function(doc, x) body_add_par(doc, x, style = "Normal")

# -- 1. Company-style CSR template --------------------------------------------

doc <- read_docx()
doc <- p(doc, "ACME BIOPHARMA CLINICAL STUDY REPORT TEMPLATE (SAMPLE)")
doc <- p(doc, paste(
  "Synthetic sample bundled with the ydisctools R package. The section",
  "layout is informed by the public ICH E3 guideline; all text is original."))
doc <- h1(doc, "1 Title Page")
doc <- h1(doc, "2 Synopsis")
doc <- h1(doc, "3 Table of Contents")
doc <- h1(doc, "4 Ethics")
doc <- h1(doc, "5 Investigators and Study Administrative Structure")
doc <- h1(doc, "6 Introduction")
doc <- h1(doc, "7 Study Objectives")
doc <- h1(doc, "8 Investigational Plan")
doc <- h1(doc, "9 Study Subjects")
doc <- h1(doc, "10 Efficacy Evaluation")
doc <- h1(doc, "11 Safety Evaluation")
doc <- h2(doc, "11.2 Adverse Events")
doc <- p(doc, "Narrative discussion of adverse events is presented here.")
doc <- h1(doc, "12 Discussion and Overall Conclusions")
doc <- h1(doc, "13 Reference List")
doc <- h1(doc, "14 Appendices")
doc <- h1(doc, "15 Tables, Figures and Graphs Referred to but not Included in the Text")
doc <- h2(doc, "15.1 Demographic and Baseline Data")
doc <- h2(doc, "15.2 Efficacy Data")
doc <- h2(doc, "15.3 Safety Data")
doc <- body_add_par(doc, "15.3.1 Displays of Adverse Events",
                    style = "heading 3")
doc <- body_add_par(doc, "15.3.4 Laboratory Data", style = "heading 3")
print(doc, target = "sample_csr_template.docx")

# -- 2. Sample SAP --------------------------------------------------------------

doc <- read_docx()
doc <- p(doc, "STATISTICAL ANALYSIS PLAN (SAMPLE)")
doc <- p(doc, "Study STUDY01 - A Randomised Study of Xanomeline in Dementia")
doc <- p(doc, paste(
  "Synthetic sample bundled with the ydisctools R package; outline informed",
  "by common industry SAP templates, all text original."))
doc <- h1(doc, "1 Introduction")
doc <- p(doc, paste(
  "This statistical analysis plan describes the planned analyses for study",
  "STUDY01, a randomised, placebo-controlled study with three arms:",
  "Placebo, Xanomeline Low Dose and Xanomeline High Dose."))
doc <- h1(doc, "2 Analysis Sets")
doc <- p(doc, paste(
  "The Safety Analysis Set comprises all subjects who received at least one",
  "dose of study drug (ADSL.SAFFL = 'Y'). The Intent-to-Treat Analysis Set",
  "comprises all randomised subjects (ADSL.ITTFL = 'Y')."))
doc <- h1(doc, "3 Statistical Methods")
doc <- h2(doc, "3.1 Demographics and Baseline")
doc <- p(doc, paste(
  "Demographic and baseline characteristics, subject disposition and study",
  "drug exposure will be summarised by treatment group on the Safety",
  "Analysis Set using descriptive statistics."))
doc <- h2(doc, "3.2 Efficacy")
doc <- p(doc, paste(
  "The primary endpoint will be analysed on the Intent-to-Treat Analysis",
  "Set. Details are specified per protocol; the planned display is listed",
  "in the appendix."))
doc <- h2(doc, "3.3 Safety")
doc <- p(doc, paste(
  "Treatment-emergent adverse events (TRTEMFL = 'Y') will be summarised by",
  "treatment group: an overall summary, summaries by system organ class and",
  "by maximum severity."))
doc <- h1(doc, "4 Appendix: Planned Displays")
doc <- p(doc, paste(
  "Display numbers will be assigned per the Acme CSR template section",
  "structure at TFL specification time."))
displays <- data.frame(
  `Table No.` = rep("", 7),
  Title = c("Summary of Demographic Data",
            "Summary of Subject Disposition",
            "Summary of Study Drug Exposure",
            "Overall Summary of Treatment-Emergent Adverse Events",
            "Summary of Treatment-Emergent Adverse Events by System Organ Class",
            "Summary of Treatment-Emergent Adverse Events by Maximum Severity",
            "Primary Efficacy Analysis"),
  Population = c(rep("Safety", 6), "ITT"),
  check.names = FALSE
)
doc <- body_add_table(doc, displays)
print(doc, target = "sample_sap.docx")

cat("Regenerated: sample_csr_template.docx, sample_sap.docx\n")
