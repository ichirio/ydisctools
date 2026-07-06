
# Programme:    Generate code to produce ARD for Out_ex
# Output:       Summary of Study Drug Exposure
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


# Analysis An_10----
#Number of subjects
# Apply Analysis Set ---
df_pop <- dplyr::filter(ADSL,
            SAFFL == 'Y')
df_poptot <- df_pop

#Apply Data Subset ---
df2_An_10 <- df_poptot

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth_total_n
# Method name:            Big N denominator count by group
# Method description:     Distinct count of the analysis variable (typically USUBJID) per treatment group. Used as the N= header / denominator source. Verified against Common_Safety_Displays Mth01_CatVar_Count_ByGrp; current cards API.

if(nrow(df2_An_10) != 0) {
                              in_data = df2_An_10 |>
    dplyr::select(USUBJID, TRT01A) |>
    unique()
df3_An_10 <-
  cards::ard_tabulate(
    data = in_data
    , variables = 'TRT01A'
  ) |>
  dplyr::filter(stat_name == 'n') |>
  dplyr::mutate(operationid = 'opid1here')}
if(nrow(df2_An_10) != 0){
df3_An_10 <- df3_An_10 |>
        dplyr::mutate(AnalysisId = 'An_10',
               MethodId = 'Mth_total_n',
               OutputId = 'Out_ex')
} else {
    df3_An_10 = data.frame(AnalysisId = 'An_10',
               MethodId = 'Mth_total_n',
               OutputId = 'Out_ex')
}
    df3_An_10 <- df3_An_10 |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An_11----
#Duration of treatment (days)
#Apply Data Subset ---
df2_An_11 <- df_pop

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth_continuous_summary
# Method name:            Summary of a continuous variable
# Method description:     Descriptive statistics of a continuous analysis variable per group. Verified against Common_Safety_Displays Mth02_ContVar_Summ_ByGrp; current cards API (ard_summary, by_listc). Fixes the legacy sheet which used the deprecated ard_continuous + by_stmt (the latter stamps no group metadata).

if(nrow(df2_An_11) != 0) {
                              df3_An_11 <-
  cards::ard_summary(
    data = df2_An_11,
    by = c('TRT01A'),
    variables = TRTDURD
  ) |>
  dplyr::mutate(operationid = dplyr::case_when(stat_name == 'N'      ~ 'opid1here',
                                               stat_name == 'mean'   ~ 'opid2here',
                                               stat_name == 'sd'     ~ 'opid3here',
                                               stat_name == 'median' ~ 'opid4here',
                                               stat_name == 'p25'    ~ 'opid5here',
                                               stat_name == 'p75'    ~ 'opid6here',
                                               stat_name == 'min'    ~ 'opid7here',
                                               stat_name == 'max'    ~ 'opid8here'))}
if(nrow(df2_An_11) != 0){
df3_An_11 <- df3_An_11 |>
        dplyr::mutate(AnalysisId = 'An_11',
               MethodId = 'Mth_continuous_summary',
               OutputId = 'Out_ex')
} else {
    df3_An_11 = data.frame(AnalysisId = 'An_11',
               MethodId = 'Mth_continuous_summary',
               OutputId = 'Out_ex')
}
    if(nrow(df2_An_11) != 0){
df3_An_11 <- df3_An_11 |>
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
df3_An_11 <- df3_An_11 |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An_12----
#Cumulative dose
#Apply Data Subset ---
df2_An_12 <- df_pop

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth_continuous_summary
# Method name:            Summary of a continuous variable
# Method description:     Descriptive statistics of a continuous analysis variable per group. Verified against Common_Safety_Displays Mth02_ContVar_Summ_ByGrp; current cards API (ard_summary, by_listc). Fixes the legacy sheet which used the deprecated ard_continuous + by_stmt (the latter stamps no group metadata).

if(nrow(df2_An_12) != 0) {
                              df3_An_12 <-
  cards::ard_summary(
    data = df2_An_12,
    by = c('TRT01A'),
    variables = CUMDOSE
  ) |>
  dplyr::mutate(operationid = dplyr::case_when(stat_name == 'N'      ~ 'opid1here',
                                               stat_name == 'mean'   ~ 'opid2here',
                                               stat_name == 'sd'     ~ 'opid3here',
                                               stat_name == 'median' ~ 'opid4here',
                                               stat_name == 'p25'    ~ 'opid5here',
                                               stat_name == 'p75'    ~ 'opid6here',
                                               stat_name == 'min'    ~ 'opid7here',
                                               stat_name == 'max'    ~ 'opid8here'))}
if(nrow(df2_An_12) != 0){
df3_An_12 <- df3_An_12 |>
        dplyr::mutate(AnalysisId = 'An_12',
               MethodId = 'Mth_continuous_summary',
               OutputId = 'Out_ex')
} else {
    df3_An_12 = data.frame(AnalysisId = 'An_12',
               MethodId = 'Mth_continuous_summary',
               OutputId = 'Out_ex')
}
    if(nrow(df2_An_12) != 0){
df3_An_12 <- df3_An_12 |>
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
df3_An_12 <- df3_An_12 |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# combine analyses to create ARD ----
ARD <- dplyr::bind_rows(df3_An_10, 
df3_An_11, 
df3_An_12) 
