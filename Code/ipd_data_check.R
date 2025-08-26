# Continuation file to check on IPD cases

# load packages from main file

condition.raw <- readxl::read_xlsx(here::here("Data", "NEPHUCaseLinelistDQReportVarName_20250811.xlsx")) %>% janitor::clean_names()

# Run cond_list chunk and condition.clean code

ipd_data <- condition.clean %>% 
  filter(condition=="Pneumococcal infection (IPD)")

key_missing_rmk <- ipd_data %>% 
  #filter(investigation_status=="Completed" ) %>% 
  #filter(!condition %in% excluded_conditions) %>% 
  filter(death_status=="Alive") %>% 
  filter(defn %in% c("Confirmed", "Probable", "Suspected")) %>% 
  mutate(
    # core data fields
    rmk_suburb = if_else(is.na(suburb), "Suburb is missing", NA_character_), 
    rmk_pc = if_else(is.na(postcode), "Postcode is missing", NA_character_), 
    rmk_lga = if_else(is.na(lga), "LGA is missing", NA_character_), 
    rmk_cob = if_else(!condition %in% cob_excl & is.na(cty_birth), "Country of birth is missing", NA_character_), 
    # rmk_cob_ns = if_else(!condition %in% cob_excl & cty_birth=="Not Stated", 
    #                   "Please review country of birth data to see if more details have been provided", NA_character_), 
    rmk_atsi = if_else(atsi=="Missing/Not Stated" & is.na(atsi_attempted), "Indigenous status marked Missing/Not Stated, please review if more details have been provided", NA_character_), 
    rmk_sex = if_else(is.na(sex), "Sex is missing", NA_character_), 
    rmk_dob = if_else(is.na(dob), "DOB is missing", NA_character_), 
    rmk_death = if_else(is.na(death_status), "Death due to notifiable condition is missing", NA_character_), 
    rmk_wsc = if_else(is.na(work_study_care_status), "Work/Study/Care status is missing", NA_character_), 
    rmk_cond = if_else(is.na(condition), "Condition is missing", NA_character_), 
    rmk_found = if_else(is.na(clin_pres), "Case found by is missing", NA_character_), 
    rmk_epiclass = if_else(is.na(epi_class), "Epi classification is missing", NA_character_), 
    rmk_organism = if_else(is.na(organism), "Organism/cause is missing", NA_character_), 
    rmk_case = if_else(event_type=="Contact/exposed person", "Case is left as Contact/exposed person", NA_character_), 
    rmk_eventdate = if_else(is.na(event_date), "Event date is missing", NA_character_), 
    #rmk_gp = if_else(is.na(local_family_treating_doctor), "GP detail is missing", NA_character_), 
    rmk_vac_source = if_else(vaccinated=="Yes" & is.na(vaccinated_source_of_info), 
                             "Vaccine source of information missing", NA_character_), 
    rmk_suspected = if_else(defn=="Suspected" & investigation_status=="Completed", 
                            "Case is still Suspected", NA_character_), 
    rmk_recent_travel_cty = if_else(recent_travel_overseas=="Yes" & is.na(cty_recent_travel), 
                                    "Country field is blank for overseas travel recently", NA_character_), 
    rmk_notifier_urgent = if_else(condition %in% urgent_cond & is.na(notifier), 
                                  "Notifier is missing", NA_character_), 
    # rmk_eform = if_else(condition %in% urgent_cond & is.na(eform), "Eform received is missing", NA_character_), 
    # risk factors
    primary_exp_count = if_else(disease_type != "Sexually Transmissible Infections" | 
                                  !condition %in% primary_risk_noncount, 
                                str_count(primary_exposure, "Primary"), NA_integer_), 
    rmk_risk = case_when(!condition %in% risk_factor_excl & is.na(risk_factors) 
                         ~ "Data on risk factors is missing", 
                         !condition %in% risk_factor_excl & primary_exp_count>1 
                         ~ "More than 1 primary risk factor provided", 
                         primary_exp_count==0 & !condition %in% c(primary_risk_noncount, "Hepatitis D") 
                         ~ "No primary risk factor provided"), 
    rmk_cty_travel = if_else(risk_factors=="Travel overseas" & is.na(risk_factor_os_cty), 
                             "Country missing for Travel overseas risk", NA_character_), 
    # hyperlink phess_id
    url = paste0('https://phess.dhhs.vic.gov.au/main.do?CaseID=', phess_id)) 

vpd_key_missing_rmk <- key_missing_rmk %>% 
  distinct(phess_id, .keep_all = TRUE) %>% 
  filter(disease_type=="Vaccine Preventable Diseases") %>% 
  # filter(! (condition=="Pertussis" & (healthcare_staff_member_maternity_neonatal_unit=="No" | is.na(healthcare_staff_member_maternity_neonatal_unit)))) %>% 
  #filter(! (condition=="Pertussis" & age>10)) %>% 
  mutate(
    rmk_pregnant = if_else((condition=="Mumps" | condition=="Meningococcal infection" ) & sex=="Female" & 
                             is.na(pregnant), 
                           "Pregnant at time of notification missing", NA_character_), 
    rmk_presented = if_else((condition=="Mumps" | 
                               condition=="Meningococcal infection" | 
                               condition=="Pneumococcal infection (IPD)") & 
                              is.na(hospital_presented_to), 
                            "Presented to field missing", NA_character_), 
    rmk_symp_onset = if_else((condition=="Mumps" | condition=="Meningococcal infection" ) & 
                               is.na(sympt_onset), 
                             "Symptom onset date missing", NA_character_), 
    # Pertussis specific checks
    rmk_pertuss_vacc = if_else(condition=="Pertussis" & age<10 & is.na(vaccinated), 
                               "Vaccine status is missing", NA_character_), 
    rmk_healthcare_worker = if_else(condition=="Pertussis" & 
                                      healthcare_staff_member_maternity_neonatal_unit=="Yes" & 
                                      is.na(vaccinated),
                                    "Vaccine status is missing", NA_character_),
    # rmk_ivw = if_else(condition=="Pertussis" & between(age, 0, 6) & is.na(ncov_interview), 
    #                   "Interview check is missing", NA_character_), 
    # Mumps
    rmk_vacc_date = if_else(condition=="Mumps" & vaccinated=="Yes" & 
                              is.na(vaccine_date), 
                            "Vaccine date missing", NA_character_), 
    rmk_vacc_source = if_else(condition=="Mumps" & vaccinated=="Yes" & 
                                is.na(vaccinated_source_of_info), 
                              "Vaccine source missing", NA_character_), 
    rmk_contact12_25d = if_else(condition=="Mumps" & 
                                  is.na(mumps_contact_with_case), 
                                "Case contact with person with similar illness 12-25 days prior missing", NA_character_), 
    # IMD
    rmk_vacc_status = if_else(condition=="Meningococcal infection" & 
                                is.na(vaccine_status), 
                              "Vaccination status missing", NA_character_), 
    rmk_imd_chronic = if_else(condition=="Meningococcal infection" & 
                                is.na(chronic_disease), 
                              "Chronic disease is blank", NA_character_), 
    rmk_imd_immunocomp = if_else(condition=="Meningococcal infection" & 
                                   is.na(immunocompromised), 
                                 "Immunocompromised is blank", NA_character_), 
    rmk_imd_anot_ill = if_else(condition=="Meningococcal infection" & 
                                 is.na(case_have_another_illness), 
                               "Another illness is blank", NA_character_), 
    rmk_asplenia = if_else(condition=="Meningococcal infection" & 
                             is.na(asplenia), 
                           "Asplenia is blank", NA_character_), 
    rmk_csf_leak = if_else(condition=="Meningococcal infection" & 
                             is.na(csf_leak), 
                           "CSF leak is blank", NA_character_), 
    rmk_imd_smoking = if_else(condition=="Meningococcal infection" & 
                                is.na(smoking_risk), 
                              "Smoking risk is blank", NA_character_), 
    rmk_imd_hh_smoking = if_else(condition=="Meningococcal infection" & 
                                   is.na(household_smoking_risk), 
                                 "Household smoking risk is blank", NA_character_), 
    rmk_travel_os = if_else(condition=="Meningococcal infection" &
                              is.na(recent_travel_overseas),
                            "Recent travel overseas is blank", NA_character_),
    rmk_travel_aus = if_else(condition=="Meningococcal infection" & 
                               is.na(recent_travel_aus), 
                             "Recent travel Australia is blank", NA_character_), 
    rmk_exp_site = if_else(condition=="Meningococcal infection" & 
                             is.na(exp_site), 
                           "Exposure site is blank", NA_character_), 
    # IPD
    rmk_manifest = if_else((condition=="Pneumococcal infection (IPD)" | condition=="Invasive Group A Streptococcus") & 
                             is.na(manifestation), 
                           "Manifestation missing", NA_character_), 
    rmk_vaccinated = if_else((condition=="Pneumococcal infection (IPD)" & is.na(vaccinated)), 
                             "Vaccinated missing", NA_character_), 
    rmk_vacc_type = if_else((condition=="Pneumococcal infection (IPD)" & vaccinated=="No" & vaccine!="No vaccine given"), 
                            "Vaccine is not marked as No vaccine given", NA_character_), 
    rmk_vacc_date = if_else((condition=="Pneumococcal infection (IPD)" & ! vaccine %in% vacc_comment_excl & is.na(vaccine_date)), 
                            "Vaccine date missing", NA_character_), 
    rmk_vacc_dose = if_else((condition=="Pneumococcal infection (IPD)" & ! vaccine %in% vacc_comment_excl & is.na(vaccine_dose)), 
                            "Vaccine dose missing", NA_character_), 
    rmk_ipd_anot_ill = if_else((condition=="Pneumococcal infection (IPD)" | condition=="Invasive Group A Streptococcus") & 
                                 is.na(case_have_another_illness), 
                               "Case have another illness is missing", NA_character_), 
    rmk_ipd_anot_ill_yes = if_else(case_have_another_illness=="Yes" & 
                                     is.na(case_have_another_illness_specify), 
                                   "Case have another illness to specify is missing", NA_character_), 
    rmk_ipd_chronic = if_else((condition=="Pneumococcal infection (IPD)" | condition=="Invasive Group A Streptococcus") & 
                                is.na(chronic_disease), 
                              "Chronic disease is blank", NA_character_), 
    rmk_ipd_chronic_yes = if_else(chronic_disease=="Yes" & 
                                    is.na(chronic_disease_specify), 
                                  "Chronic disease specify is missing", NA_character_), 
    rmk_ipd_immunocomp = if_else((condition=="Pneumococcal infection (IPD)" | condition=="Invasive Group A Streptococcus") & 
                                   is.na(immunocompromised), 
                                 "Immunocompromised is blank", NA_character_), 
    rmk_ipd_immunocomp_des = if_else(immunocompromised=="Yes" & condition=="Invasive Group A Streptococcus" & 
                                       is.na(immunocompromised_describe), 
                                     "Immunocompromised describe is missing", NA_character_), 
    rmk_ipd_immunocomp_spec = if_else(immunocompromised=="Yes" & condition=="Pneumococcal infection (IPD)" & 
                                        is.na(immunocompromised_specify),
                                      "Immunocompromised specify is missing", NA_character_),
    rmk_chromosome = if_else(condition=="Pneumococcal infection (IPD)" & 
                               is.na(congenital_chromosomal_abnormality), 
                             "Chromosomal abnormality is blank", NA_character_), 
    rmk_chromosome_yes = if_else(congenital_chromosomal_abnormality=="Yes" &
                                   is.na(congenital_chromosomal_abnormality_specify),
                                 "Chromosomal abnormality specify is blank", NA_character_),
    rmk_ipd_smoking = if_else(condition=="Pneumococcal infection (IPD)" & 
                                is.na(smoking_risk), 
                              "Smoking risk is blank", NA_character_), 
    rmk_ipd_hh_smoking = if_else(condition=="Pneumococcal infection (IPD)" & 
                                   is.na(household_smoking_risk), 
                                 "Household smoking risk is blank", NA_character_), 
    rmk_ipd_diagbefore = if_else(condition=="Pneumococcal infection (IPD)" & 
                                   is.na(previous_positive_for_same_condition), 
                                 "IPD has been diagnosed before missing", NA_character_), 
    rmk_ipd_diagbefore_yes = if_else(previous_positive_for_same_condition=="Yes" & is.na(previous_positive_for_same_condition_date), 
                                     "Previous IPD diagnosis date missing", NA_character_), 
    rmk_ipd_icu = if_else(condition=="Pneumococcal infection (IPD)" & 
                            hospital_presented_to %in% c("Hospital admission", "Hospital emergency") & is.na(icu), 
                          "IPD ICU status missing", NA_character_), 
    # rmk_ipd_pcr = if_else(condition=="Pneumococcal infection (IPD)" & is.na(pcr_diag_only), 
    #                       "IPD diagnosed by PCR missing", NA_character_), 
    # iGAS
    rmk_vacc_source = if_else(condition=="Invasive Group A Streptococcus" & vaccinated=="Yes" & is.na(vaccinated_source_of_info), 
                              "Vaccination source of info missing", NA_character_), 
    rmk_surgery = if_else(condition=="Invasive Group A Streptococcus" & 
                            is.na(surgical_intervention), 
                          "Surgical intervention required missing", NA_character_), 
    rmk_ecmo = if_else(condition=="Invasive Group A Streptococcus" & 
                         is.na(extracorporeal_membrane_oxygenation), 
                       "Received ECMO missing", NA_character_), 
    rmk_hosp_ipc = if_else(condition=="Invasive Group A Streptococcus" & 
                             is.na(infection_control_notified), 
                           "Hospital infection control notified missing", NA_character_), 
    rmk_treated = if_else(condition=="Invasive Group A Streptococcus" & 
                            is.na(treated), 
                          "Treated for this illness missing", NA_character_), 
    rmk_consent_contact = if_else(condition=="Invasive Group A Streptococcus" & 
                                    is.na(gp_consent_to_contact_case), 
                                  "doctor has provided consent to contact case missing", NA_character_), 
    rmk_igas_contact = if_else(condition=="Invasive Group A Streptococcus" & 
                                 is.na(igas_contact_confirmed_case), 
                               "Contact with iGAS case previous 3 months missing", NA_character_), 
    rmk_igas_risk = if_else(condition=="Invasive Group A Streptococcus" & 
                              is.na(igas_risk_factors), 
                            "iGAS risk factor missing", NA_character_), 
    rmk_igas_inst = if_else(condition=="Invasive Group A Streptococcus" & 
                              is.na(igas_contact_institutional_settings), 
                            "Any iGAS case in institutional settings previous 3 months missing", NA_character_), 
    rmk_igas_hh = if_else(condition=="Invasive Group A Streptococcus" & 
                            is.na(igas_contact_non_invasive), 
                          "Any non-invasive GAS case in household of institutional settings missing", NA_character_), 
    rmk_contact_mgt = if_else(condition=="Invasive Group A Streptococcus" & 
                                is.na(igas_contact_management), 
                              "Contact management undertaken with case missing", NA_character_), 
    rmk_give_birth = if_else(condition=="Invasive Group A Streptococcus" & 
                               sex=="Female" & 
                               is.na(gave_birth_28_days_prior_symptom_onset), 
                             "Give birth in 28 days prior missing", NA_character_), 
    rmk_neonate = if_else(condition=="Invasive Group A Streptococcus" & 
                            between(age, 0, 1) & 
                            is.na(neonate_birth_within_28_days_symptom_onset), 
                          "Give birth in 28 days prior missing", NA_character_), 
    rmk_edu_mat = if_else(condition=="Invasive Group A Streptococcus" & 
                            is.na(educational_materials), 
                          "Educational materials provided to case missing", NA_character_), 
    # IPD and iGAS
    rmk_summ_risk = if_else((condition=="Pneumococcal infection (IPD)" | condition=="Invasive Group A Streptococcus") & 
                              is.na(risk_factors_summary), 
                            "Summary risk factor is missing", NA_character_) 
  ) 

set_keymiss_table(vpd_key_missing_rmk)

