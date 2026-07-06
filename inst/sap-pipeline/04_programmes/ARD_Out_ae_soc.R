
# Programme:    Generate code to produce ARD for Out_ae_soc
# Output:       Summary of Treatment-Emergent Adverse Events by System Organ Class
# Date created: 2026-07-05 23:19:55

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


# Analysis An_17----
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
df2_An_17 <- df_poptot

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth_total_n
# Method name:            Big N denominator count by group
# Method description:     Distinct count of the analysis variable (typically USUBJID) per treatment group. Used as the N= header / denominator source. Verified against Common_Safety_Displays Mth01_CatVar_Count_ByGrp; current cards API.

if(nrow(df2_An_17) != 0) {
                              in_data = df2_An_17 |>
    dplyr::select(USUBJID, TRT01A) |>
    unique()
df3_An_17 <-
  cards::ard_tabulate(
    data = in_data
    , variables = 'TRT01A'
  ) |>
  dplyr::filter(stat_name == 'n') |>
  dplyr::mutate(operationid = 'opid1here')}
if(nrow(df2_An_17) != 0){
df3_An_17 <- df3_An_17 |>
        dplyr::mutate(AnalysisId = 'An_17',
               MethodId = 'Mth_total_n',
               OutputId = 'Out_ae_soc')
} else {
    df3_An_17 = data.frame(AnalysisId = 'An_17',
               MethodId = 'Mth_total_n',
               OutputId = 'Out_ae_soc')
}
    df3_An_17 <- df3_An_17 |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An_18----
#Subjects with at least one TEAE, n (%)# Apply Data Subset ---
# Data subset: TRTEMFL EQ Y
df2_An_18 <- df_pop |>
        dplyr::filter(TRTEMFL == 'Y')
#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth_categorical_summary
# Method name:            Summary of a categorical variable (n and %)
# Method description:     Distinct-subject n and percentage per category and group, with a referenced denominator analysis. Modern cards pattern: the category variable is passed as `variables=` (so it lands in the ARD's variable / variable_level columns) and the outer grouping(s) as `by=` (`strata=` when the innermost grouping is data-driven). A flat analysis (no category variable, e.g. 'subjects with at least one TEAE') tabulates a constant flag with the group as `by=`, so the percentage denominator is the per-group big N rather than the overall N. ydisctools overlay entry; replaces the siera catalog's distinct+dummy template.

if(nrow(df2_An_18) != 0) {
                              denom_dataset = df2_An_17 |>
  dplyr::select(TRT01A)

in_data = df2_An_18 |>
    dplyr::distinct(TRT01A, USUBJID)

dataDriven = FALSE
if(ncol(in_data) <= 2){
# flat analysis (no category variable): tabulate a constant flag with the
# group as `by`, so the percentage denominator is the per-group big N
df3_An_18 <-
  cards::ard_tabulate(
    data = in_data |> dplyr::mutate(.flag_ = 'Y')
    , by = 'TRT01A', variables = '.flag_',
    denominator = denom_dataset
  ) } else if(dataDriven == TRUE){
df3_An_18 <-
  cards::ard_tabulate(
    data = in_data
    , variables = 'TRT01A',
    denominator = denom_dataset
  ) } else {
df3_An_18 <-
 cards::ard_tabulate(
    data = in_data
    , variables = 'TRT01A',
    denominator = denom_dataset
  ) }
df3_An_18 <- df3_An_18 |>
  dplyr::filter(stat_name %in% c('n', 'p')) |>
  dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'opid1here',
                                               stat_name == 'p' ~ 'opid2here'))}
if(nrow(df2_An_18) != 0){
df3_An_18 <- df3_An_18 |>
        dplyr::mutate(AnalysisId = 'An_18',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_ae_soc')
} else {
    df3_An_18 = data.frame(AnalysisId = 'An_18',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_ae_soc')
}
    df3_An_18 <- df3_An_18 |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An_19----
#TEAE by System Organ Class, n (%)# Apply Data Subset ---
# Data subset: TRTEMFL EQ Y
df2_An_19 <- df_pop |>
        dplyr::filter(TRTEMFL == 'Y')
#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth_categorical_summary
# Method name:            Summary of a categorical variable (n and %)
# Method description:     Distinct-subject n and percentage per category and group, with a referenced denominator analysis. Modern cards pattern: the category variable is passed as `variables=` (so it lands in the ARD's variable / variable_level columns) and the outer grouping(s) as `by=` (`strata=` when the innermost grouping is data-driven). A flat analysis (no category variable, e.g. 'subjects with at least one TEAE') tabulates a constant flag with the group as `by=`, so the percentage denominator is the per-group big N rather than the overall N. ydisctools overlay entry; replaces the siera catalog's distinct+dummy template.

if(nrow(df2_An_19) != 0) {
                              denom_dataset = df2_An_17 |>
  dplyr::select(TRT01A)

in_data = df2_An_19 |>
    dplyr::distinct(TRT01A, AEBODSYS, USUBJID)

dataDriven = TRUE
if(ncol(in_data) <= 2){
# flat analysis (no category variable): tabulate a constant flag with the
# group as `by`, so the percentage denominator is the per-group big N
df3_An_19 <-
  cards::ard_tabulate(
    data = in_data |> dplyr::mutate(.flag_ = 'Y')
    , by = 'TRT01A', variables = '.flag_',
    denominator = denom_dataset
  ) } else if(dataDriven == TRUE){
df3_An_19 <-
  cards::ard_tabulate(
    data = in_data
    , strata = 'TRT01A' , variables = 'AEBODSYS',
    denominator = denom_dataset
  ) } else {
df3_An_19 <-
 cards::ard_tabulate(
    data = in_data
    , by = 'TRT01A' , variables = 'AEBODSYS',
    denominator = denom_dataset
  ) }
df3_An_19 <- df3_An_19 |>
  dplyr::filter(stat_name %in% c('n', 'p')) |>
  dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'opid1here',
                                               stat_name == 'p' ~ 'opid2here'))}
if(nrow(df2_An_19) != 0){
df3_An_19 <- df3_An_19 |>
        dplyr::mutate(AnalysisId = 'An_19',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_ae_soc')
} else {
    df3_An_19 = data.frame(AnalysisId = 'An_19',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_ae_soc')
}
    if(nrow(df2_An_19) != 0){
df3_An_19 <- df3_An_19 |>
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
df3_An_19 <- df3_An_19 |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# combine analyses to create ARD ----
ARD <- dplyr::bind_rows(df3_An_17, 
df3_An_18, 
df3_An_19) 
