
# Programme:    Generate code to produce ARD for Out_ae
# Output:       Summary of Treatment-Emergent Adverse Events
# Date created: 2026-07-04 13:41:02

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


# Analysis An_06----
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
df2_An_06 <- df_poptot

#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth_total_n
# Method name:            Big N denominator count by group
# Method description:     Distinct count of the analysis variable (typically USUBJID) per treatment group. Used as the N= header / denominator source. Verified against Common_Safety_Displays Mth01_CatVar_Count_ByGrp; current cards API.

if(nrow(df2_An_06) != 0) {
                              in_data = df2_An_06 |>
    dplyr::select(USUBJID, TRT01A) |>
    unique()
df3_An_06 <-
  cards::ard_tabulate(
    data = in_data
    , variables = 'TRT01A'
  ) |>
  dplyr::filter(stat_name == 'n') |>
  dplyr::mutate(operationid = 'opid1here')}
if(nrow(df2_An_06) != 0){
df3_An_06 <- df3_An_06 |>
        dplyr::mutate(AnalysisId = 'An_06',
               MethodId = 'Mth_total_n',
               OutputId = 'Out_ae')
} else {
    df3_An_06 = data.frame(AnalysisId = 'An_06',
               MethodId = 'Mth_total_n',
               OutputId = 'Out_ae')
}
    df3_An_06 <- df3_An_06 |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An_07----
#Subjects with at least one TEAE, n (%)# Apply Data Subset ---
# Data subset: TRTEMFL EQ Y
df2_An_07 <- df_pop |>
        dplyr::filter(TRTEMFL == 'Y')
#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth_categorical_summary
# Method name:            Summary of a categorical variable (n and %)
# Method description:     n and percentage of a categorical analysis variable per group, with a referenced denominator analysis and a data-driven/pre-defined grouping branch. Verified against Common_Safety_Displays Mth01_CatVar_Summ_ByGrp; current cards API. Spurious by_vars/strata_vars params from the legacy sheet have been dropped (the template uses by_listc).

if(nrow(df2_An_07) != 0) {
                              denom_dataset = df2_An_06 |>
  dplyr::select(TRT01A)

in_data = df2_An_07 |>
    dplyr::distinct(TRT01A, USUBJID) |>
    dplyr::mutate(dummy = 'dummyvar')

dataDriven = FALSE
if(dataDriven == TRUE){
df3_An_07 <-
  cards::ard_tabulate(
    data = in_data,
    strata = c('TRT01A'),
    variables = 'dummy',
    denominator = denom_dataset
  ) } else {
df3_An_07 <-
 cards::ard_tabulate(
    data = in_data,
    by = c('TRT01A'),
    variables = 'dummy',
    denominator = denom_dataset
  ) }
df3_An_07 <- df3_An_07|>
  dplyr::filter(stat_name %in% c('n', 'p')) |>
  dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'opid1here',
                                               stat_name == 'p' ~ 'opid2here'))}
if(nrow(df2_An_07) != 0){
df3_An_07 <- df3_An_07 |>
        dplyr::mutate(AnalysisId = 'An_07',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_ae')
} else {
    df3_An_07 = data.frame(AnalysisId = 'An_07',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_ae')
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


# Analysis An_08----
#TEAE by System Organ Class, n (%)# Apply Data Subset ---
# Data subset: TRTEMFL EQ Y
df2_An_08 <- df_pop |>
        dplyr::filter(TRTEMFL == 'Y')
#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth_categorical_summary
# Method name:            Summary of a categorical variable (n and %)
# Method description:     n and percentage of a categorical analysis variable per group, with a referenced denominator analysis and a data-driven/pre-defined grouping branch. Verified against Common_Safety_Displays Mth01_CatVar_Summ_ByGrp; current cards API. Spurious by_vars/strata_vars params from the legacy sheet have been dropped (the template uses by_listc).

if(nrow(df2_An_08) != 0) {
                              denom_dataset = df2_An_06 |>
  dplyr::select(TRT01A)

in_data = df2_An_08 |>
    dplyr::distinct(TRT01A, AEBODSYS, USUBJID) |>
    dplyr::mutate(dummy = 'dummyvar')

dataDriven = TRUE
if(dataDriven == TRUE){
df3_An_08 <-
  cards::ard_tabulate(
    data = in_data,
    strata = c('TRT01A', 'AEBODSYS'),
    variables = 'dummy',
    denominator = denom_dataset
  ) } else {
df3_An_08 <-
 cards::ard_tabulate(
    data = in_data,
    by = c('TRT01A', 'AEBODSYS'),
    variables = 'dummy',
    denominator = denom_dataset
  ) }
df3_An_08 <- df3_An_08|>
  dplyr::filter(stat_name %in% c('n', 'p')) |>
  dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'opid1here',
                                               stat_name == 'p' ~ 'opid2here'))}
if(nrow(df2_An_08) != 0){
df3_An_08 <- df3_An_08 |>
        dplyr::mutate(AnalysisId = 'An_08',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_ae')
} else {
    df3_An_08 = data.frame(AnalysisId = 'An_08',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_ae')
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
      ),
      group2_groupingId = 'AnlsGrp_05_AEBODSYS',
      group2_groupId = NA_character_,
      group2_groupValue = as.character(group2_level)
  )
}
df3_An_08 <- df3_An_08 |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An_09----
#TEAE by Preferred Term, n (%)# Apply Data Subset ---
# Data subset: TRTEMFL EQ Y
df2_An_09 <- df_pop |>
        dplyr::filter(TRTEMFL == 'Y')
#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth_categorical_summary
# Method name:            Summary of a categorical variable (n and %)
# Method description:     n and percentage of a categorical analysis variable per group, with a referenced denominator analysis and a data-driven/pre-defined grouping branch. Verified against Common_Safety_Displays Mth01_CatVar_Summ_ByGrp; current cards API. Spurious by_vars/strata_vars params from the legacy sheet have been dropped (the template uses by_listc).

if(nrow(df2_An_09) != 0) {
                              denom_dataset = df2_An_06 |>
  dplyr::select(TRT01A)

in_data = df2_An_09 |>
    dplyr::distinct(TRT01A, AEDECOD, USUBJID) |>
    dplyr::mutate(dummy = 'dummyvar')

dataDriven = TRUE
if(dataDriven == TRUE){
df3_An_09 <-
  cards::ard_tabulate(
    data = in_data,
    strata = c('TRT01A', 'AEDECOD'),
    variables = 'dummy',
    denominator = denom_dataset
  ) } else {
df3_An_09 <-
 cards::ard_tabulate(
    data = in_data,
    by = c('TRT01A', 'AEDECOD'),
    variables = 'dummy',
    denominator = denom_dataset
  ) }
df3_An_09 <- df3_An_09|>
  dplyr::filter(stat_name %in% c('n', 'p')) |>
  dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'opid1here',
                                               stat_name == 'p' ~ 'opid2here'))}
if(nrow(df2_An_09) != 0){
df3_An_09 <- df3_An_09 |>
        dplyr::mutate(AnalysisId = 'An_09',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_ae')
} else {
    df3_An_09 = data.frame(AnalysisId = 'An_09',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_ae')
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
      ),
      group2_groupingId = 'AnlsGrp_06_AEDECOD',
      group2_groupId = NA_character_,
      group2_groupValue = as.character(group2_level)
  )
}
df3_An_09 <- df3_An_09 |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# Analysis An_10----
#TEAE by severity, n (%)# Apply Data Subset ---
# Data subset: TRTEMFL EQ Y
df2_An_10 <- df_pop |>
        dplyr::filter(TRTEMFL == 'Y')
#Apply Method --- 
#Apply Method --- 

# Method ID:              Mth_categorical_summary
# Method name:            Summary of a categorical variable (n and %)
# Method description:     n and percentage of a categorical analysis variable per group, with a referenced denominator analysis and a data-driven/pre-defined grouping branch. Verified against Common_Safety_Displays Mth01_CatVar_Summ_ByGrp; current cards API. Spurious by_vars/strata_vars params from the legacy sheet have been dropped (the template uses by_listc).

if(nrow(df2_An_10) != 0) {
                              denom_dataset = df2_An_06 |>
  dplyr::select(TRT01A)

in_data = df2_An_10 |>
    dplyr::distinct(TRT01A, AESEV, USUBJID) |>
    dplyr::mutate(dummy = 'dummyvar')

dataDriven = TRUE
if(dataDriven == TRUE){
df3_An_10 <-
  cards::ard_tabulate(
    data = in_data,
    strata = c('TRT01A', 'AESEV'),
    variables = 'dummy',
    denominator = denom_dataset
  ) } else {
df3_An_10 <-
 cards::ard_tabulate(
    data = in_data,
    by = c('TRT01A', 'AESEV'),
    variables = 'dummy',
    denominator = denom_dataset
  ) }
df3_An_10 <- df3_An_10|>
  dplyr::filter(stat_name %in% c('n', 'p')) |>
  dplyr::mutate(operationid = dplyr::case_when(stat_name == 'n' ~ 'opid1here',
                                               stat_name == 'p' ~ 'opid2here'))}
if(nrow(df2_An_10) != 0){
df3_An_10 <- df3_An_10 |>
        dplyr::mutate(AnalysisId = 'An_10',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_ae')
} else {
    df3_An_10 = data.frame(AnalysisId = 'An_10',
               MethodId = 'Mth_categorical_summary',
               OutputId = 'Out_ae')
}
    if(nrow(df2_An_10) != 0){
df3_An_10 <- df3_An_10 |>
  dplyr::mutate(
      group1_groupingId = 'AnlsGrp_01_TRT01A',
      group1_groupId = dplyr::case_when(
        as.character(group1_level) == 'Placebo' ~ 'AnlsGrp_01_TRT01A_01',
        as.character(group1_level) == 'Xanomeline Low Dose' ~ 'AnlsGrp_01_TRT01A_02',
        as.character(group1_level) == 'Xanomeline High Dose' ~ 'AnlsGrp_01_TRT01A_03',
        TRUE ~ NA_character_
      ),
      group2_groupingId = 'AnlsGrp_07_AESEV',
      group2_groupId = NA_character_,
      group2_groupValue = as.character(group2_level)
  )
}
df3_An_10 <- df3_An_10 |>
  dplyr::mutate(dplyr::across(
    dplyr::matches('_level$'),
    ~ vapply(.x, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1L))
  ))


# combine analyses to create ARD ----
ARD <- dplyr::bind_rows(df3_An_06, 
df3_An_07, 
df3_An_08, 
df3_An_09, 
df3_An_10) 
