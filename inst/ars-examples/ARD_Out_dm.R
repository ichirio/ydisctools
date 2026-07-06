
# Programme:    Generate code to produce ARD for Out_dm
# Output:       Summary of Demographic Data
# Date created: 2026-07-06 11:46:17

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


# Analysis An_01----
#Number of subjects
# Apply Analysis Set ---
df_pop <- dplyr::filter(ADSL,
            SAFFL == 'Y')
df_poptot <- df_pop

#Apply Data Subset ---
df2_An_01 <- df_poptot

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth_total_n
# Method name:            Big N denominator count by group
# Method description:     Distinct count of the analysis variable (typically USUBJID) per treatment group. Used as the N= header / denominator source. Verified against Common_Safety_Displays Mth01_CatVar_Count_ByGrp; current cards API.

if(nrow(df2_An_01) != 0) {
                              in_data = df2_An_01 |>
    dplyr::select(USUBJID, TRT01A) |>
    unique()
df3_An_01 <-
  cards::ard_tabulate(
    data = in_data
    , variables = 'TRT01A'
  ) |>
  dplyr::filter(stat_name == 'n') |>
  dplyr::mutate(operationid = 'opid1here')}
if(nrow(df2_An_01) != 0){
df3_An_01 <- df3_An_01 |>
        dplyr::mutate(AnalysisId = 'An_01',
               MethodId = 'Mth_total_n',
               OutputId = 'Out_dm')
} else {
    df3_An_01 = data.frame(AnalysisId = 'An_01',
               MethodId = 'Mth_total_n',
               OutputId = 'Out_dm')
}
    df3_An_01 <- df3_An_01 |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An_02----
#Age (years)
#Apply Data Subset ---
df2_An_02 <- df_pop

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth_continuous_summary
# Method name:            Summary of a continuous variable
# Method description:     Descriptive statistics of a continuous analysis variable per group. Verified against Common_Safety_Displays Mth02_ContVar_Summ_ByGrp; current cards API (ard_summary, by_listc). Fixes the legacy sheet which used the deprecated ard_continuous + by_stmt (the latter stamps no group metadata).

if(nrow(df2_An_02) != 0) {
                              df3_An_02 <-
  cards::ard_summary(
    data = df2_An_02,
    by = c('TRT01A'),
    variables = AGE
  ) |>
  dplyr::mutate(operationid = dplyr::case_when(stat_name == 'N'      ~ 'opid1here',
                                               stat_name == 'mean'   ~ 'opid2here',
                                               stat_name == 'sd'     ~ 'opid3here',
                                               stat_name == 'median' ~ 'opid4here',
                                               stat_name == 'p25'    ~ 'opid5here',
                                               stat_name == 'p75'    ~ 'opid6here',
                                               stat_name == 'min'    ~ 'opid7here',
                                               stat_name == 'max'    ~ 'opid8here'))}
if(nrow(df2_An_02) != 0){
df3_An_02 <- df3_An_02 |>
        dplyr::mutate(AnalysisId = 'An_02',
               MethodId = 'Mth_continuous_summary',
               OutputId = 'Out_dm')
} else {
    df3_An_02 = data.frame(AnalysisId = 'An_02',
               MethodId = 'Mth_continuous_summary',
               OutputId = 'Out_dm')
}
    if(nrow(df2_An_02) != 0){
df3_An_02 <- df3_An_02 |>
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
df3_An_02 <- df3_An_02 |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An_03----
#Age group 1, n (%)
#Apply Data Subset ---
df2_An_03 <- df_pop

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth_categorical_summary
# Method name:            Summary of a categorical variable (n and %)
# Method description:     Distinct-subject n and percentage per category and group, with a referenced denominator analysis. Modern cards pattern: the category variable is passed as `variables=` (so it lands in the ARD's variable / variable_level columns) and the outer grouping(s) as `by=` (`strata=` when the innermost grouping is data-driven). Flat analyses (no category variable) are generated from the dedicated categorical_summary_flat template instead. ydisctools overlay entry; replaces the siera catalog's distinct+dummy template.

if(nrow(df2_An_03) != 0) {
                              denom_dataset = df2_An_01 |>
  dplyr::select(TRT01A)

in_data = df2_An_03 |>
    dplyr::distinct(TRT01A, AGEGR1, USUBJID)

# strata= when the innermost grouping is data-driven, by= when pre-defined
dataDriven = TRUE
if(dataDriven == TRUE){
df3_An_03 <-
  cards::ard_tabulate(
    data = in_data
    , strata = 'TRT01A' , variables = 'AGEGR1',
    denominator = denom_dataset
  ) } else {
df3_An_03 <-
 cards::ard_tabulate(
    data = in_data
    , by = 'TRT01A' , variables = 'AGEGR1',
    denominator = denom_dataset
  ) }
df3_An_03 <- df3_An_03 |>
  dplyr::filter(stat_name %in% c('n', 'p')) |>
  dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'opid1here',
                                               stat_name == 'p' ~ 'opid2here'))}
if(nrow(df2_An_03) != 0){
df3_An_03 <- df3_An_03 |>
        dplyr::mutate(AnalysisId = 'An_03',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_dm')
} else {
    df3_An_03 = data.frame(AnalysisId = 'An_03',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_dm')
}
    if(nrow(df2_An_03) != 0){
df3_An_03 <- df3_An_03 |>
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
df3_An_03 <- df3_An_03 |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An_04----
#Age group 2, n (%)
#Apply Data Subset ---
df2_An_04 <- df_pop

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth_categorical_summary
# Method name:            Summary of a categorical variable (n and %)
# Method description:     Distinct-subject n and percentage per category and group, with a referenced denominator analysis. Modern cards pattern: the category variable is passed as `variables=` (so it lands in the ARD's variable / variable_level columns) and the outer grouping(s) as `by=` (`strata=` when the innermost grouping is data-driven). Flat analyses (no category variable) are generated from the dedicated categorical_summary_flat template instead. ydisctools overlay entry; replaces the siera catalog's distinct+dummy template.

if(nrow(df2_An_04) != 0) {
                              denom_dataset = df2_An_01 |>
  dplyr::select(TRT01A)

in_data = df2_An_04 |>
    dplyr::distinct(TRT01A, AGEGR2, USUBJID)

# strata= when the innermost grouping is data-driven, by= when pre-defined
dataDriven = TRUE
if(dataDriven == TRUE){
df3_An_04 <-
  cards::ard_tabulate(
    data = in_data
    , strata = 'TRT01A' , variables = 'AGEGR2',
    denominator = denom_dataset
  ) } else {
df3_An_04 <-
 cards::ard_tabulate(
    data = in_data
    , by = 'TRT01A' , variables = 'AGEGR2',
    denominator = denom_dataset
  ) }
df3_An_04 <- df3_An_04 |>
  dplyr::filter(stat_name %in% c('n', 'p')) |>
  dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'opid1here',
                                               stat_name == 'p' ~ 'opid2here'))}
if(nrow(df2_An_04) != 0){
df3_An_04 <- df3_An_04 |>
        dplyr::mutate(AnalysisId = 'An_04',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_dm')
} else {
    df3_An_04 = data.frame(AnalysisId = 'An_04',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_dm')
}
    if(nrow(df2_An_04) != 0){
df3_An_04 <- df3_An_04 |>
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
df3_An_04 <- df3_An_04 |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An_05----
#Sex, n (%)
#Apply Data Subset ---
df2_An_05 <- df_pop

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth_categorical_summary
# Method name:            Summary of a categorical variable (n and %)
# Method description:     Distinct-subject n and percentage per category and group, with a referenced denominator analysis. Modern cards pattern: the category variable is passed as `variables=` (so it lands in the ARD's variable / variable_level columns) and the outer grouping(s) as `by=` (`strata=` when the innermost grouping is data-driven). Flat analyses (no category variable) are generated from the dedicated categorical_summary_flat template instead. ydisctools overlay entry; replaces the siera catalog's distinct+dummy template.

if(nrow(df2_An_05) != 0) {
                              denom_dataset = df2_An_01 |>
  dplyr::select(TRT01A)

in_data = df2_An_05 |>
    dplyr::distinct(TRT01A, SEX, USUBJID)

# strata= when the innermost grouping is data-driven, by= when pre-defined
dataDriven = TRUE
if(dataDriven == TRUE){
df3_An_05 <-
  cards::ard_tabulate(
    data = in_data
    , strata = 'TRT01A' , variables = 'SEX',
    denominator = denom_dataset
  ) } else {
df3_An_05 <-
 cards::ard_tabulate(
    data = in_data
    , by = 'TRT01A' , variables = 'SEX',
    denominator = denom_dataset
  ) }
df3_An_05 <- df3_An_05 |>
  dplyr::filter(stat_name %in% c('n', 'p')) |>
  dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'opid1here',
                                               stat_name == 'p' ~ 'opid2here'))}
if(nrow(df2_An_05) != 0){
df3_An_05 <- df3_An_05 |>
        dplyr::mutate(AnalysisId = 'An_05',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_dm')
} else {
    df3_An_05 = data.frame(AnalysisId = 'An_05',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_dm')
}
    if(nrow(df2_An_05) != 0){
df3_An_05 <- df3_An_05 |>
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
df3_An_05 <- df3_An_05 |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An_06----
#Race, n (%)
#Apply Data Subset ---
df2_An_06 <- df_pop

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth_categorical_summary
# Method name:            Summary of a categorical variable (n and %)
# Method description:     Distinct-subject n and percentage per category and group, with a referenced denominator analysis. Modern cards pattern: the category variable is passed as `variables=` (so it lands in the ARD's variable / variable_level columns) and the outer grouping(s) as `by=` (`strata=` when the innermost grouping is data-driven). Flat analyses (no category variable) are generated from the dedicated categorical_summary_flat template instead. ydisctools overlay entry; replaces the siera catalog's distinct+dummy template.

if(nrow(df2_An_06) != 0) {
                              denom_dataset = df2_An_01 |>
  dplyr::select(TRT01A)

in_data = df2_An_06 |>
    dplyr::distinct(TRT01A, RACE, USUBJID)

# strata= when the innermost grouping is data-driven, by= when pre-defined
dataDriven = TRUE
if(dataDriven == TRUE){
df3_An_06 <-
  cards::ard_tabulate(
    data = in_data
    , strata = 'TRT01A' , variables = 'RACE',
    denominator = denom_dataset
  ) } else {
df3_An_06 <-
 cards::ard_tabulate(
    data = in_data
    , by = 'TRT01A' , variables = 'RACE',
    denominator = denom_dataset
  ) }
df3_An_06 <- df3_An_06 |>
  dplyr::filter(stat_name %in% c('n', 'p')) |>
  dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'opid1here',
                                               stat_name == 'p' ~ 'opid2here'))}
if(nrow(df2_An_06) != 0){
df3_An_06 <- df3_An_06 |>
        dplyr::mutate(AnalysisId = 'An_06',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_dm')
} else {
    df3_An_06 = data.frame(AnalysisId = 'An_06',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_dm')
}
    if(nrow(df2_An_06) != 0){
df3_An_06 <- df3_An_06 |>
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
df3_An_06 <- df3_An_06 |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An_07----
#Ethnicity, n (%)
#Apply Data Subset ---
df2_An_07 <- df_pop

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth_categorical_summary
# Method name:            Summary of a categorical variable (n and %)
# Method description:     Distinct-subject n and percentage per category and group, with a referenced denominator analysis. Modern cards pattern: the category variable is passed as `variables=` (so it lands in the ARD's variable / variable_level columns) and the outer grouping(s) as `by=` (`strata=` when the innermost grouping is data-driven). Flat analyses (no category variable) are generated from the dedicated categorical_summary_flat template instead. ydisctools overlay entry; replaces the siera catalog's distinct+dummy template.

if(nrow(df2_An_07) != 0) {
                              denom_dataset = df2_An_01 |>
  dplyr::select(TRT01A)

in_data = df2_An_07 |>
    dplyr::distinct(TRT01A, ETHNIC, USUBJID)

# strata= when the innermost grouping is data-driven, by= when pre-defined
dataDriven = TRUE
if(dataDriven == TRUE){
df3_An_07 <-
  cards::ard_tabulate(
    data = in_data
    , strata = 'TRT01A' , variables = 'ETHNIC',
    denominator = denom_dataset
  ) } else {
df3_An_07 <-
 cards::ard_tabulate(
    data = in_data
    , by = 'TRT01A' , variables = 'ETHNIC',
    denominator = denom_dataset
  ) }
df3_An_07 <- df3_An_07 |>
  dplyr::filter(stat_name %in% c('n', 'p')) |>
  dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'opid1here',
                                               stat_name == 'p' ~ 'opid2here'))}
if(nrow(df2_An_07) != 0){
df3_An_07 <- df3_An_07 |>
        dplyr::mutate(AnalysisId = 'An_07',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_dm')
} else {
    df3_An_07 = data.frame(AnalysisId = 'An_07',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_dm')
}
    if(nrow(df2_An_07) != 0){
df3_An_07 <- df3_An_07 |>
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
df3_An_07 <- df3_An_07 |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# combine analyses to create ARD ----
ARD <- dplyr::bind_rows(df3_An_01, 
df3_An_02, 
df3_An_03, 
df3_An_04, 
df3_An_05, 
df3_An_06, 
df3_An_07) 
