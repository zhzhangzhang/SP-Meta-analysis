# import the data
library(haven)
sp_all_df <- read_dta('/Users/songshuyi/Documents/GitHub/SP-Meta-analysis/data-raw/provider_sp_alldata_clean.dta')

# combine village level and township level into rural level
sp_all_df$sp_clinics_level <- ifelse(sp_all_df$survey_clinics_level==1, "Rural",
                                     ifelse(sp_all_df$survey_clinics_level==2, "Rural",
                                            ifelse(sp_all_df$survey_clinics_level==3, "County",
                                                   ifelse(sp_all_df$survey_clinics_level==4, "Migrant",
                                                          ifelse(sp_all_df$survey_clinics_level==5, "CHC", "Online")))))
sp_all_df$rural <- ifelse((sp_all_df$survey_clinics_level ==1 | sp_all_df$survey_clinics_level ==2),1, 0)
sp_all_df$county <- ifelse((sp_all_df$survey_clinics_level ==3),1,0)
sp_all_df$migrant <- ifelse((sp_all_df$survey_clinics_level ==4),1,0)
sp_all_df$city <- ifelse((sp_all_df$survey_clinics_level ==5),1,0)
sp_all_df$online <- ifelse((sp_all_df$survey_clinics_level ==6),1,0)

remove_val_labels("provider_male")

# Physician characteristics
sp_all_df$Physician_age_group1 <- ifelse(sp_all_df$provider_age_group ==0,
                                              c(1), c(0))
sp_all_df$Physician_age_group2 <- ifelse(sp_all_df$provider_age_group ==1,
                                              c(1), c(0))
sp_all_df$Physician_age_group3 <- ifelse(sp_all_df$provider_age_group ==2,
                                              c(1), c(0))
sp_all_df$Chief_physician_associate <- ifelse(sp_all_df$provider_title ==5,
                                                   c(1), c(0))
sp_all_df$Attending_physician <- ifelse(sp_all_df$provider_title ==4,
                                             c(1), c(0))
sp_all_df$Resident_physician <- ifelse(sp_all_df$provider_title ==3,
                                            c(1), c(0))
sp_all_df$Assistant_practicing_physician <- ifelse(sp_all_df$provider_title ==2,
                                                        c(1), c(0))
sp_all_df$Rural_physician <- ifelse(sp_all_df$provider_title ==1,
                                         c(1), c(0))
sp_all_df$No_certificate <- ifelse(sp_all_df$provider_title ==0,
                                        c(1), c(0))
sp_all_df$Others <- ifelse(sp_all_df$provider_title ==6,
                                c(1), c(0))
# outcome variables
sp_all_df$fee_all = rowSums(sp_all_df[,c("case1_totfee", "case2_totfee", "case3_totfee", "case4_totfee")], na.rm=TRUE)

sp_all_df$fee_all <- ifelse(sp_all_df$survey_project == 1,
                            rowSums(sp_all_df[,c("case1_clinfee", "case2_clinfee", "case3_clinfee", "case4_clinfee")], na.rm=TRUE),
                            sp_all_df$fee_all)

sp_all_df$ln_totfees <- log1p(fee_all)
sp_all_df$correct_diagnosis <- rowSums(sp_all_df[,c("case1_corrdiag", "case2_corrdiag", "case3_corrdiag", "case4_corrdiag")], na.rm=TRUE)

sp_all_df$referred_patients <- rowSums(sp_all_df[,c("case1_refer", "case2_refer", "case3_refer", "case4_refer")], na.rm=TRUE)

sp_all_df$medications_prescribed <- rowSums(sp_all_df[,c("case1_drug", "case2_drug", "case3_drug", "case4_drug")], na.rm=TRUE)

sp_all_df$correct_medications <- rowSums(sp_all_df[,c("case1_corrdrug", "case2_corrdrug", "case3_corrdrug", "case4_corrdrug")], na.rm=TRUE)

sp_all_df$antibiotics_prescribed <- rowSums(sp_all_df[,c("case1_antibiotic", "case2_antibiotic", "case3_antibiotic", "case4_antibiotic")], na.rm=TRUE)

sp_all_df$proportions_recommended <- rowSums(sp_all_df[,c("case1_r_qe_pct", "case2_r_qe_pct", "case3_r_qe_pct", "case4_r_qe_pct")], na.rm=TRUE)
sp_all_df$proportions_recommended <- ifelse(sp_all_df$survey_clinics_level==6, rowSums(sp_all_df[,c("case1_r_q_pct", "case2_r_q_pct", "case3_r_q_pct", "case4_r_q_pct")], na.rm=TRUE), sp_all_df$proportions_recommended)

sp_all_df$disease_type <- ifelse(sp_all_df$survey_disease_type==1, "Angina",
                                 ifelse(sp_all_df$survey_disease_type==2, "Bacterial Diarrhea",
                                        ifelse(sp_all_df$survey_disease_type==3, "Viral Diarrhea", "TB")))

sp_all_df$correct_treatment <- rowSums(sp_all_df[,c("case1_corrtreat", "case2_corrtreat", "case3_corrtreat", "case4_corrtreat")], na.rm=TRUE)

sp_all_df$correct_management <- ifelse((sp_all_df$disease_type=="TB" |
                                          sp_all_df$disease_type=="Angina") &
                                         (sp_all_df$referred_patients==1 |
                                            sp_all_df$correct_treatment==1 |
                                            sp_all_df$correct_medications==1), 1, ifelse((sp_all_df$disease_type=="Viral Diarrhea" |
                                                                                            sp_all_df$disease_type=="Bacterial Diarrhea") &
                                                                                           (sp_all_df$correct_treatment==1 |
                                                                                              sp_all_df$correct_medications==1), 1, 0))
sp_all_df$ln_totfees <- log1p(fee_all)

lm(proportions_recommended ~ county + migrant + city + online, data = sp_all_df)

fixed.dum <-lm(proportions_recommended ~ county + migrant + city + online + factor(disease_type) - 1, data = sp_all_df)
summary(fixed.dum)

lm(correct_diagnosis ~ county + migrant + city + online, data = sp_all_df)
lm(correct_management ~ county + migrant + city + online, data = sp_all_df)
lm(referred_patients ~ county + migrant + city + online, data = sp_all_df)
lm(medications_prescribed ~ county + migrant + city + online, data = sp_all_df)
lm(correct_medications ~ county + migrant + city + online, data = sp_all_df)
lm(proportions_recommended ~ county + migrant + city + online, data = sp_all_df)
lm(antibiotics_prescribed ~ county + migrant + city + online, data = sp_all_df)


library(gtsummary)
library("dplyr")
new_var <- sp_all_df %>% select(fee_all, proportions_recommended, sp_clinics_level, correct_diagnosis, correct_management, referred_patients, medications_prescribed, correct_medications, antibiotics_prescribed, disease_type)

new_var %>% tbl_strata(
  strata = disease_type,
  ~.x %>%
    tbl_summary(by = sp_clinics_level,
                statistic = list(all_continuous() ~ "{mean} ({sd})"),
                digits = all_continuous() ~ 3, type = list(correct_diagnosis ~ 'continuous',
                                                           correct_management ~ 'continuous',
                                                           referred_patients ~ 'continuous',
                                                           medications_prescribed ~ 'continuous',
                                                           correct_medications ~ 'continuous',
                                                           antibiotics_prescribed ~ 'continuous'),
                label = list(
                  fee_all ~ "Fees (Chinese Yuan)",
                  proportions_recommended ~ "Proportion of recommended items",
                  correct_diagnosis ~ "Correct diagnosis",
                  correct_management ~ "Correct case management",
                  referred_patients ~ "Referred patients",
                  medications_prescribed ~ "Medications prescribed",
                  correct_medications ~ "Correct medications, if any",
                  antibiotics_prescribed ~ "Antibiotics prescribed, if any"
                )) %>% modify_footnote(
                  all_stat_cols() ~ "Notes: Mean (SD). Process quality for non-online providers are measured as the proportion of recommended questions and examinations.
                                               Online process quality is measured as the proportion of recommended questions. Fees for non-online providers are fees of consultation and medicines combined. Fees for online platforms are consultation fees only."
                ) %>% modify_caption("**Table 5. SP outcomes across three disease cases**")
)

