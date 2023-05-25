# import the data
library(haven)
sp_provider_df <- read_dta('/Users/songshuyi/Documents/GitHub/SP-Meta-analysis/data-raw/nr_provider_sp_alldata_clean.dta')

#check data strorage type for analysis
library(labelled)
remove_val_labels("provider_male")

# combine village level and township level into rural level
sp_provider_df$sp_clinics_level <- ifelse(sp_provider_df$survey_clinics_level==1, "Rural",
                                     ifelse(sp_provider_df$survey_clinics_level==2, "Rural",
                                            ifelse(sp_provider_df$survey_clinics_level==3, "County",
                                                   ifelse(sp_provider_df$survey_clinics_level==4, "Migrant",
                                                          ifelse(sp_provider_df$survey_clinics_level==5, "CHC", "Online")))))   #为什么不写==6，online？

# get the summary statistics
sp_provider_df$Physician_age_group1 <- ifelse(sp_provider_df$provider_age_group ==0,
                                    c(1), c(0))
sp_provider_df$Physician_age_group2 <- ifelse(sp_provider_df$provider_age_group ==1,
                                     c(1), c(0))
sp_provider_df$Physician_age_group3 <- ifelse(sp_provider_df$provider_age_group ==2,
                                     c(1), c(0))
sp_provider_df$Chief_physician_associate <- ifelse(sp_provider_df$provider_title ==5,
                                             c(1), c(0))
sp_provider_df$Attending_physician <- ifelse(sp_provider_df$provider_title ==4,
                                             c(1), c(0))
sp_provider_df$Resident_physician <- ifelse(sp_provider_df$provider_title ==3,
                                            c(1), c(0))
sp_provider_df$Assistant_practicing_physician <- ifelse(sp_provider_df$provider_title ==2,
                                            c(1), c(0))
sp_provider_df$Rural_physician <- ifelse(sp_provider_df$provider_title ==1,
                                            c(1), c(0))
sp_provider_df$No_certificate <- ifelse(sp_provider_df$provider_title ==0,
                                            c(1), c(0))
sp_provider_df$Others <- ifelse(sp_provider_df$provider_title ==6,
                                            c(1), c(0))

# descriptive analysis

library(gtsummary)
library("dplyr")
sp_provider_df %>% filter(survey_vignette==0)
var <- sp_provider_df %>% select(sp_clinics_level, provider_male, Physician_age_group1, Physician_age_group2, Physician_age_group3, Chief_physician_associate, Attending_physician, Resident_physician, Assistant_practicing_physician, Rural_physician, No_certificate, Others)
var %>% tbl_summary( by = sp_clinics_level,
                    statistic = list(all_continuous() ~ "{mean} ({sd})"),
                    digits = all_continuous() ~ 3, type = list(provider_male ~ 'continuous',
                                                               Physician_age_group1 ~ 'continuous',
                                                               Physician_age_group2 ~ 'continuous',
                                                               Physician_age_group3 ~ 'continuous',
                                                               Chief_physician_associate ~ 'continuous',
                                                               Attending_physician ~ 'continuous',
                                                               Resident_physician ~ 'continuous',
                                                               Assistant_practicing_physician ~ 'continuous',
                                                               Rural_physician ~ 'continuous',
                                                               No_certificate ~ 'continuous',
                                                               Others ~ 'continuous'),
                    label = list(
                      provider_male ~ "Physician Gender (male physician)",
                      Physician_age_group1 ~ "Physician age group1 (<40 years)",
                      Physician_age_group2 ~ "Physician age group2 (40-50 years)",
                      Physician_age_group3 ~ "Physician age group3 (>50 years)",
                      Chief_physician_associate ~ "Chief physician and Associate chief physician",
                      Attending_physician ~ "Attending physician",
                      Assistant_practicing_physician ~ "With Assistant Practicing Physician Certificate ",
                      Resident_physician ~ "Resident physician / Physician with practicing Physician Certificate ",
                      Rural_physician ~ "With Rural Physician Certificate",
                      No_certificate ~ "No title / certificate",
                      Others ~ "Other title / certificate"
                    )) %>% add_overall() %>% modify_footnote(
                      all_stat_cols() ~ "Notes: Data are sample mean (SD). Other title/certificate means that the physician title was recorded as other title/certificate in the survey. Unknown are missing data"
                    ) %>% modify_caption("**Table 3. Physician characteristics (norepeated_sp)**")

