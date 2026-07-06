
# Programme:    Generate code to produce ARD for Out_ae_ser
# Output:       Summary of Serious Treatment-Emergent Adverse Events by System Organ Class
# Date created: 2026-07-06 11:46:23

  # load libraries ----
    library(dplyr)
    library(readxl)
    library(readr)
    library(cards)
    library(cardx)
    library(broom)
    library(parameters)
    library(tidyr)
  
# Load ADaM -------
ADSL <- readr::read_csv('adam/ADSL.csv',
                                      show_col_types = FALSE,
                                      progress = FALSE) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ tidyr::replace_na(.x, '')))
ADAE <- readr::read_csv('adam/ADAE.csv',
                                      show_col_types = FALSE,
                                      progress = FALSE) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ tidyr::replace_na(.x, '')))


# Analysis An_26----
#Number of subjects
# Apply Analysis Set ---
overlap <- intersect(names(ADSL), names(ADAE))
overlapfin <- setdiff(overlap, 'USUBJID')
df_pop <- dplyr::filter(ADSL,
            SAFFL == 'Y') |>
            merge(ADAE |> dplyr::select(-dplyr::all_of(overlapfin)),
                  by = 'USUBJID',
                  all = FALSE)
df_poptot = dplyr::filter(ADSL,
            SAFFL == 'Y')

#Apply Data Subset ---
df2_An_26 <- df_poptot

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth_total_n
# Method name:            Big N denominator count by group
# Method description:     Distinct count of the analysis variable (typically USUBJID) per treatment group. Used as the N= header / denominator source. Verified against Common_Safety_Displays Mth01_CatVar_Count_ByGrp; current cards API.

if(nrow(df2_An_26) != 0) {
                              in_data = df2_An_26 |>
    dplyr::select(USUBJID, TRT01A) |>
    unique()
df3_An_26 <-
  cards::ard_tabulate(
    data = in_data
    , variables = 'TRT01A'
  ) |>
  dplyr::filter(stat_name == 'n') |>
  dplyr::mutate(operationid = 'opid1here')}
if(nrow(df2_An_26) != 0){
df3_An_26 <- df3_An_26 |>
        dplyr::mutate(AnalysisId = 'An_26',
               MethodId = 'Mth_total_n',
               OutputId = 'Out_ae_ser')
} else {
    df3_An_26 = data.frame(AnalysisId = 'An_26',
               MethodId = 'Mth_total_n',
               OutputId = 'Out_ae_ser')
}
    df3_An_26 <- df3_An_26 |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An_27----
#Subjects with at least one TEAE, n (%)# Apply Data Subset ---
# Data subset: TRTEMFL EQ Y; AESER EQ Y
df2_An_27 <- df_pop |>
        dplyr::filter(TRTEMFL == 'Y' & AESER == 'Y')
#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth_categorical_summary_flat
# Method name:            Subject count per group (n and %)
# Method description:     Distinct-subject n and percentage per group for a flat analysis (no category variable, e.g. 'subjects with at least one TEAE'), with a referenced denominator analysis. Tabulates a constant '.flag_' with the group as `by=`, so the percentage denominator is that group's big N. Selected automatically by build_ars() when a categorical_summary analysis has a single grouping and variable = USUBJID; the compact parameter format keeps the categorical_summary key. ydisctools overlay entry.

if(nrow(df2_An_27) != 0) {
                              denom_dataset = df2_An_26 |>
  dplyr::select(TRT01A)

# n (%) of subjects per group: tabulate a constant flag with the group as
# `by`, so the percentage denominator is that group's big N
in_data = df2_An_27 |>
    dplyr::distinct(TRT01A, USUBJID) |>
    dplyr::mutate(.flag_ = 'Y')

df3_An_27 <-
  cards::ard_tabulate(
    data = in_data
    , by = 'TRT01A', variables = '.flag_',
    denominator = denom_dataset
  )
df3_An_27 <- df3_An_27 |>
  dplyr::filter(stat_name %in% c('n', 'p')) |>
  dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'opid1here',
                                               stat_name == 'p' ~ 'opid2here'))}
if(nrow(df2_An_27) != 0){
df3_An_27 <- df3_An_27 |>
        dplyr::mutate(AnalysisId = 'An_27',
               MethodId = 'Mth_categorical_summary_flat',
               OutputId = 'Out_ae_ser')
} else {
    df3_An_27 = data.frame(AnalysisId = 'An_27',
               MethodId = 'Mth_categorical_summary_flat',
               OutputId = 'Out_ae_ser')
}
    if(nrow(df2_An_27) != 0){
df3_An_27 <- df3_An_27 |>
  dplyr::mutate(
      group1_groupingId = 'AnlsGrp_01_TRT01A',
      group1_groupId = dplyr::case_when(
        as.character(group1_level) == 'Placebo' ~ 'AnlsGrp_01_TRT01A_01',
        as.character(group1_level) == 'Xanomeline Low Dose' ~ 'AnlsGrp_01_TRT01A_02',
        as.character(group1_level) == 'Xanomeline High Dose' ~ 'AnlsGrp_01_TRT01A_03',
        TRUE ~ NA_character_
      )
  )
}
df3_An_27 <- df3_An_27 |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An_28----
#TEAE by System Organ Class, n (%)# Apply Data Subset ---
# Data subset: TRTEMFL EQ Y; AESER EQ Y
df2_An_28 <- df_pop |>
        dplyr::filter(TRTEMFL == 'Y' & AESER == 'Y')
#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth_categorical_summary
# Method name:            Summary of a categorical variable (n and %)
# Method description:     Distinct-subject n and percentage per category and group, with a referenced denominator analysis. Modern cards pattern: the category variable is passed as `variables=` (so it lands in the ARD's variable / variable_level columns) and the outer grouping(s) as `by=` (`strata=` when the innermost grouping is data-driven). Flat analyses (no category variable) are generated from the dedicated categorical_summary_flat template instead. ydisctools overlay entry; replaces the siera catalog's distinct+dummy template.

if(nrow(df2_An_28) != 0) {
                              denom_dataset = df2_An_26 |>
  dplyr::select(TRT01A)

in_data = df2_An_28 |>
    dplyr::distinct(TRT01A, AEBODSYS, USUBJID)

# strata= when the innermost grouping is data-driven, by= when pre-defined
dataDriven = TRUE
if(dataDriven == TRUE){
df3_An_28 <-
  cards::ard_tabulate(
    data = in_data
    , strata = 'TRT01A' , variables = 'AEBODSYS',
    denominator = denom_dataset
  ) } else {
df3_An_28 <-
 cards::ard_tabulate(
    data = in_data
    , by = 'TRT01A' , variables = 'AEBODSYS',
    denominator = denom_dataset
  ) }
df3_An_28 <- df3_An_28 |>
  dplyr::filter(stat_name %in% c('n', 'p')) |>
  dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'opid1here',
                                               stat_name == 'p' ~ 'opid2here'))}
if(nrow(df2_An_28) != 0){
df3_An_28 <- df3_An_28 |>
        dplyr::mutate(AnalysisId = 'An_28',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_ae_ser')
} else {
    df3_An_28 = data.frame(AnalysisId = 'An_28',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_ae_ser')
}
    if(nrow(df2_An_28) != 0){
df3_An_28 <- df3_An_28 |>
  dplyr::mutate(
      group1_groupingId = 'AnlsGrp_01_TRT01A',
      group1_groupId = dplyr::case_when(
        as.character(group1_level) == 'Placebo' ~ 'AnlsGrp_01_TRT01A_01',
        as.character(group1_level) == 'Xanomeline Low Dose' ~ 'AnlsGrp_01_TRT01A_02',
        as.character(group1_level) == 'Xanomeline High Dose' ~ 'AnlsGrp_01_TRT01A_03',
        TRUE ~ NA_character_
      )
  )
}
df3_An_28 <- df3_An_28 |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# combine analyses to create ARD ----
ARD <- dplyr::bind_rows(df3_An_26, 
df3_An_27, 
df3_An_28) 
