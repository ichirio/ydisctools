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
#
#  A decent-scale SAP (ydisctools issue #31): full front sections plus a
#  10-display planned-display appendix.  Everything in the appendix is
#  runnable downstream against the STUDY01 dummy ADaM in inst/sap-pipeline.

doc <- read_docx()
doc <- p(doc, "STATISTICAL ANALYSIS PLAN (SAMPLE)")
doc <- p(doc, "Study STUDY01 - A Randomised Study of Xanomeline in Dementia")
doc <- p(doc, paste(
  "Synthetic sample bundled with the ydisctools R package; outline informed",
  "by common industry SAP templates, all text original."))

doc <- h1(doc, "1 Introduction")
doc <- p(doc, paste(
  "This statistical analysis plan (SAP) describes the planned analyses for",
  "study STUDY01, a randomised, double-blind, placebo-controlled study with",
  "three arms: Placebo, Xanomeline Low Dose and Xanomeline High Dose.",
  "It elaborates the statistical section of the protocol and fixes the",
  "planned displays before database lock."))

doc <- h1(doc, "2 Study Objectives and Endpoints")
doc <- h2(doc, "2.1 Primary Objective and Endpoint")
doc <- p(doc, paste(
  "The primary objective is to evaluate the efficacy of Xanomeline compared",
  "with placebo. The primary endpoint (ADEFF.PARAMCD = 'PRIMEP') is the",
  "change from baseline to Week 24 in the cognition total score. A subject",
  "with an improvement of at least 4 points is a responder",
  "(ADEFF.CRIT1FL = 'Y')."))
doc <- h2(doc, "2.2 Key Secondary Endpoint")
doc <- p(doc, paste(
  "The key secondary endpoint (ADEFF.PARAMCD = 'SECEP') is the change from",
  "baseline to Week 24 in the clinician's global impression score, with a",
  "responder defined as any improvement from baseline",
  "(ADEFF.CRIT1FL = 'Y')."))

doc <- h1(doc, "3 Study Design")
doc <- p(doc, paste(
  "Subjects are randomised 1:1:1 to Placebo, Xanomeline Low Dose or",
  "Xanomeline High Dose (ADSL.TRT01A) and treated for 24 weeks. Visits are",
  "scheduled every 4 weeks; safety is monitored throughout the treatment",
  "and follow-up periods."))

doc <- h1(doc, "4 Analysis Sets")
doc <- p(doc, paste(
  "The Safety Analysis Set comprises all subjects who received at least one",
  "dose of study drug (ADSL.SAFFL = 'Y'); it is the default population for",
  "safety displays. The Intent-to-Treat (ITT) Analysis Set comprises all",
  "randomised subjects (ADSL.ITTFL = 'Y'); it is the population for the",
  "efficacy analyses."))

doc <- h1(doc, "5 Statistical Methods")
doc <- h2(doc, "5.1 General Considerations")
doc <- p(doc, paste(
  "Continuous variables will be described with n, mean, standard deviation,",
  "median, quartiles, minimum and maximum. Categorical variables will be",
  "described with counts and percentages; percentages use the number of",
  "subjects in the analysis set and treatment group as the denominator",
  "unless stated otherwise. No inferential multiplicity adjustment is",
  "planned for this sample document."))
doc <- h2(doc, "5.2 Demographics, Disposition and Exposure")
doc <- p(doc, paste(
  "Demographic and baseline characteristics will be summarised by treatment",
  "group on the Safety Analysis Set. Subject disposition (end-of-study",
  "status and reason for discontinuation) and study drug exposure (duration",
  "of treatment and cumulative dose) will be summarised likewise."))
doc <- h2(doc, "5.3 Efficacy Analyses")
doc <- p(doc, paste(
  "The primary endpoint will be analysed on the ITT Analysis Set: the",
  "Week 24 change from baseline will be summarised by treatment group, and",
  "the proportion of responders will be presented with counts and",
  "percentages. The key secondary endpoint will be analysed in the same",
  "way. No interim analysis is planned."))
doc <- h2(doc, "5.4 Safety Analyses")
doc <- p(doc, paste(
  "Treatment-emergent adverse events (ADAE.TRTEMFL = 'Y') will be",
  "summarised by treatment group on the Safety Analysis Set: an overall",
  "summary, summaries by system organ class, by preferred term and by",
  "maximum severity, and a summary of serious adverse events",
  "(ADAE.AESER = 'Y') by system organ class. Adverse events are coded with",
  "MedDRA."))

doc <- h1(doc, "6 Sample Size")
doc <- p(doc, paste(
  "The sample size is driven by the primary comparison of Xanomeline High",
  "Dose with placebo; the planned enrolment provides adequate power under",
  "the protocol assumptions. No re-estimation is planned."))

doc <- h1(doc, "7 Changes from the Protocol-Planned Analyses")
doc <- p(doc, "None.")

doc <- h1(doc, "8 Appendix: Planned Displays")
doc <- p(doc, paste(
  "Display numbers will be assigned per the Acme CSR template section",
  "structure at TFL specification time."))
displays <- data.frame(
  `Table No.` = rep("", 10),
  Title = c("Summary of Demographic Data",
            "Summary of Subject Disposition",
            "Summary of Study Drug Exposure",
            "Overall Summary of Treatment-Emergent Adverse Events",
            "Summary of Treatment-Emergent Adverse Events by System Organ Class",
            "Summary of Treatment-Emergent Adverse Events by Preferred Term",
            "Summary of Treatment-Emergent Adverse Events by Maximum Severity",
            "Summary of Serious Treatment-Emergent Adverse Events by System Organ Class",
            "Analysis of the Primary Efficacy Endpoint",
            "Analysis of the Key Secondary Efficacy Endpoint"),
  Population = c(rep("Safety", 8), "ITT", "ITT"),
  check.names = FALSE
)
doc <- body_add_table(doc, displays)
print(doc, target = "sample_sap.docx")

cat("Regenerated: sample_csr_template.docx, sample_sap.docx\n")
