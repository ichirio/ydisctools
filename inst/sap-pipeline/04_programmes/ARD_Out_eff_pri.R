
# Programme:    Generate code to produce ARD for Out_eff_pri
# Output:       Analysis of the Primary Efficacy Endpoint
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
ADEFF <- readr::read_csv('adam/ADEFF.csv',
                                      show_col_types = FALSE,
                                      progress = FALSE) |>
  dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ tidyr::replace_na(.x, '')))


# Analysis An_29----
#Number of subjects
# Apply Analysis Set ---
overlap <- intersect(names(ADSL), names(ADEFF))
overlapfin <- setdiff(overlap, 'USUBJID')
df_pop <- dplyr::filter(ADSL,
            ITTFL == 'Y') |>
            merge(ADEFF |> dplyr::select(-dplyr::all_of(overlapfin)),
                  by = 'USUBJID',
                  all = FALSE)
df_poptot = dplyr::filter(ADSL,
            ITTFL == 'Y')

#Apply Data Subset ---
df2_An_29 <- df_poptot

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth_total_n
# Method name:            Big N denominator count by group
# Method description:     Distinct count of the analysis variable (typically USUBJID) per treatment group. Used as the N= header / denominator source. Verified against Common_Safety_Displays Mth01_CatVar_Count_ByGrp; current cards API.

if(nrow(df2_An_29) != 0) {
                              in_data = df2_An_29 |>
    dplyr::select(USUBJID, TRT01A) |>
    unique()
df3_An_29 <-
  cards::ard_tabulate(
    data = in_data
    , variables = 'TRT01A'
  ) |>
  dplyr::filter(stat_name == 'n') |>
  dplyr::mutate(operationid = 'opid1here')}
if(nrow(df2_An_29) != 0){
df3_An_29 <- df3_An_29 |>
        dplyr::mutate(AnalysisId = 'An_29',
               MethodId = 'Mth_total_n',
               OutputId = 'Out_eff_pri')
} else {
    df3_An_29 = data.frame(AnalysisId = 'An_29',
               MethodId = 'Mth_total_n',
               OutputId = 'Out_eff_pri')
}
    df3_An_29 <- df3_An_29 |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An_30----
#Change from baseline at Week 24# Apply Data Subset ---
# Data subset: PARAMCD EQ PRIMEP
df2_An_30 <- df_pop |>
        dplyr::filter(PARAMCD == 'PRIMEP')
#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth_continuous_summary
# Method name:            Summary of a continuous variable
# Method description:     Descriptive statistics of a continuous analysis variable per group. Verified against Common_Safety_Displays Mth02_ContVar_Summ_ByGrp; current cards API (ard_summary, by_listc). Fixes the legacy sheet which used the deprecated ard_continuous + by_stmt (the latter stamps no group metadata).

if(nrow(df2_An_30) != 0) {
                              df3_An_30 <-
  cards::ard_summary(
    data = df2_An_30,
    by = c('TRT01A'),
    variables = AVAL
  ) |>
  dplyr::mutate(operationid = dplyr::case_when(stat_name == 'N'      ~ 'opid1here',
                                               stat_name == 'mean'   ~ 'opid2here',
                                               stat_name == 'sd'     ~ 'opid3here',
                                               stat_name == 'median' ~ 'opid4here',
                                               stat_name == 'p25'    ~ 'opid5here',
                                               stat_name == 'p75'    ~ 'opid6here',
                                               stat_name == 'min'    ~ 'opid7here',
                                               stat_name == 'max'    ~ 'opid8here'))}
if(nrow(df2_An_30) != 0){
df3_An_30 <- df3_An_30 |>
        dplyr::mutate(AnalysisId = 'An_30',
               MethodId = 'Mth_continuous_summary',
               OutputId = 'Out_eff_pri')
} else {
    df3_An_30 = data.frame(AnalysisId = 'An_30',
               MethodId = 'Mth_continuous_summary',
               OutputId = 'Out_eff_pri')
}
    if(nrow(df2_An_30) != 0){
df3_An_30 <- df3_An_30 |>
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
df3_An_30 <- df3_An_30 |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An_31----
#Responders, n (%)# Apply Data Subset ---
# Data subset: PARAMCD EQ PRIMEP
df2_An_31 <- df_pop |>
        dplyr::filter(PARAMCD == 'PRIMEP')
#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth_categorical_summary
# Method name:            Summary of a categorical variable (n and %)
# Method description:     Distinct-subject n and percentage per category and group, with a referenced denominator analysis. Modern cards pattern: the category variable is passed as `variables=` (so it lands in the ARD's variable / variable_level columns) and the outer grouping(s) as `by=` (`strata=` when the innermost grouping is data-driven). Flat analyses (no category variable) are generated from the dedicated categorical_summary_flat template instead. ydisctools overlay entry; replaces the siera catalog's distinct+dummy template.

if(nrow(df2_An_31) != 0) {
                              denom_dataset = df2_An_29 |>
  dplyr::select(TRT01A)

in_data = df2_An_31 |>
    dplyr::distinct(TRT01A, CRIT1FL, USUBJID)

# strata= when the innermost grouping is data-driven, by= when pre-defined
dataDriven = TRUE
if(dataDriven == TRUE){
df3_An_31 <-
  cards::ard_tabulate(
    data = in_data
    , strata = 'TRT01A' , variables = 'CRIT1FL',
    denominator = denom_dataset
  ) } else {
df3_An_31 <-
 cards::ard_tabulate(
    data = in_data
    , by = 'TRT01A' , variables = 'CRIT1FL',
    denominator = denom_dataset
  ) }
df3_An_31 <- df3_An_31 |>
  dplyr::filter(stat_name %in% c('n', 'p')) |>
  dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'opid1here',
                                               stat_name == 'p' ~ 'opid2here'))}
if(nrow(df2_An_31) != 0){
df3_An_31 <- df3_An_31 |>
        dplyr::mutate(AnalysisId = 'An_31',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_eff_pri')
} else {
    df3_An_31 = data.frame(AnalysisId = 'An_31',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_eff_pri')
}
    if(nrow(df2_An_31) != 0){
df3_An_31 <- df3_An_31 |>
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
df3_An_31 <- df3_An_31 |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# combine analyses to create ARD ----
ARD <- dplyr::bind_rows(df3_An_29, 
df3_An_30, 
df3_An_31) 
