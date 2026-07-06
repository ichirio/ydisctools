
# Programme:    Generate code to produce ARD for Out_ds
# Output:       Summary of Subject Disposition
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


# Analysis An_07----
#Number of subjects
# Apply Analysis Set ---
df_pop <- dplyr::filter(ADSL,
            SAFFL == 'Y')
df_poptot <- df_pop

#Apply Data Subset ---
df2_An_07 <- df_poptot

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth_total_n
# Method name:            Big N denominator count by group
# Method description:     Distinct count of the analysis variable (typically USUBJID) per treatment group. Used as the N= header / denominator source. Verified against Common_Safety_Displays Mth01_CatVar_Count_ByGrp; current cards API.

if(nrow(df2_An_07) != 0) {
                              in_data = df2_An_07 |>
    dplyr::select(USUBJID, TRT01A) |>
    unique()
df3_An_07 <-
  cards::ard_tabulate(
    data = in_data
    , variables = 'TRT01A'
  ) |>
  dplyr::filter(stat_name == 'n') |>
  dplyr::mutate(operationid = 'opid1here')}
if(nrow(df2_An_07) != 0){
df3_An_07 <- df3_An_07 |>
        dplyr::mutate(AnalysisId = 'An_07',
               MethodId = 'Mth_total_n',
               OutputId = 'Out_ds')
} else {
    df3_An_07 = data.frame(AnalysisId = 'An_07',
               MethodId = 'Mth_total_n',
               OutputId = 'Out_ds')
}
    df3_An_07 <- df3_An_07 |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An_08----
#End of study status, n (%)
#Apply Data Subset ---
df2_An_08 <- df_pop

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth_categorical_summary
# Method name:            Summary of a categorical variable (n and %)
# Method description:     Distinct-subject n and percentage per category and group, with a referenced denominator analysis. Modern cards pattern: the category variable is passed as `variables=` (so it lands in the ARD's variable / variable_level columns) and the outer grouping(s) as `by=` (`strata=` when the innermost grouping is data-driven). A flat analysis (no category variable, e.g. 'subjects with at least one TEAE') tabulates a constant flag with the group as `by=`, so the percentage denominator is the per-group big N rather than the overall N. ydisctools overlay entry; replaces the siera catalog's distinct+dummy template.

if(nrow(df2_An_08) != 0) {
                              denom_dataset = df2_An_07 |>
  dplyr::select(TRT01A)

in_data = df2_An_08 |>
    dplyr::distinct(TRT01A, EOSSTT, USUBJID)

dataDriven = TRUE
if(ncol(in_data) <= 2){
# flat analysis (no category variable): tabulate a constant flag with the
# group as `by`, so the percentage denominator is the per-group big N
df3_An_08 <-
  cards::ard_tabulate(
    data = in_data |> dplyr::mutate(.flag_ = 'Y')
    , by = 'TRT01A', variables = '.flag_',
    denominator = denom_dataset
  ) } else if(dataDriven == TRUE){
df3_An_08 <-
  cards::ard_tabulate(
    data = in_data
    , strata = 'TRT01A' , variables = 'EOSSTT',
    denominator = denom_dataset
  ) } else {
df3_An_08 <-
 cards::ard_tabulate(
    data = in_data
    , by = 'TRT01A' , variables = 'EOSSTT',
    denominator = denom_dataset
  ) }
df3_An_08 <- df3_An_08 |>
  dplyr::filter(stat_name %in% c('n', 'p')) |>
  dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'opid1here',
                                               stat_name == 'p' ~ 'opid2here'))}
if(nrow(df2_An_08) != 0){
df3_An_08 <- df3_An_08 |>
        dplyr::mutate(AnalysisId = 'An_08',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_ds')
} else {
    df3_An_08 = data.frame(AnalysisId = 'An_08',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_ds')
}
    if(nrow(df2_An_08) != 0){
df3_An_08 <- df3_An_08 |>
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
df3_An_08 <- df3_An_08 |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An_09----
#Reason for discontinuation, n (%)# Apply Data Subset ---
# Data subset: EOSSTT NE COMPLETED
df2_An_09 <- df_pop |>
        dplyr::filter(EOSSTT != 'COMPLETED')
#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth_categorical_summary
# Method name:            Summary of a categorical variable (n and %)
# Method description:     Distinct-subject n and percentage per category and group, with a referenced denominator analysis. Modern cards pattern: the category variable is passed as `variables=` (so it lands in the ARD's variable / variable_level columns) and the outer grouping(s) as `by=` (`strata=` when the innermost grouping is data-driven). A flat analysis (no category variable, e.g. 'subjects with at least one TEAE') tabulates a constant flag with the group as `by=`, so the percentage denominator is the per-group big N rather than the overall N. ydisctools overlay entry; replaces the siera catalog's distinct+dummy template.

if(nrow(df2_An_09) != 0) {
                              denom_dataset = df2_An_07 |>
  dplyr::select(TRT01A)

in_data = df2_An_09 |>
    dplyr::distinct(TRT01A, DCSREAS, USUBJID)

dataDriven = TRUE
if(ncol(in_data) <= 2){
# flat analysis (no category variable): tabulate a constant flag with the
# group as `by`, so the percentage denominator is the per-group big N
df3_An_09 <-
  cards::ard_tabulate(
    data = in_data |> dplyr::mutate(.flag_ = 'Y')
    , by = 'TRT01A', variables = '.flag_',
    denominator = denom_dataset
  ) } else if(dataDriven == TRUE){
df3_An_09 <-
  cards::ard_tabulate(
    data = in_data
    , strata = 'TRT01A' , variables = 'DCSREAS',
    denominator = denom_dataset
  ) } else {
df3_An_09 <-
 cards::ard_tabulate(
    data = in_data
    , by = 'TRT01A' , variables = 'DCSREAS',
    denominator = denom_dataset
  ) }
df3_An_09 <- df3_An_09 |>
  dplyr::filter(stat_name %in% c('n', 'p')) |>
  dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'opid1here',
                                               stat_name == 'p' ~ 'opid2here'))}
if(nrow(df2_An_09) != 0){
df3_An_09 <- df3_An_09 |>
        dplyr::mutate(AnalysisId = 'An_09',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_ds')
} else {
    df3_An_09 = data.frame(AnalysisId = 'An_09',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_ds')
}
    if(nrow(df2_An_09) != 0){
df3_An_09 <- df3_An_09 |>
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
df3_An_09 <- df3_An_09 |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# combine analyses to create ARD ----
ARD <- dplyr::bind_rows(df3_An_07, 
df3_An_08, 
df3_An_09) 
