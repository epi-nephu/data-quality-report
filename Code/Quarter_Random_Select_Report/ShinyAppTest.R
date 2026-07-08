# Claude AI Prompt to generate code for Shiny App

# Here is a set of R code. I would like to convert this into a R Shiny app where user can enter values 
# under the column "Records" of the entry_table that will randomly select the number of records needed 
# from the case_raw table:



# load libraries
library(here)
library(tidyverse)
library(janitor)


# Load extract data
case_raw <- tibble::tribble(
       ~event_id,                     ~condition_type,                                 ~condition,  ~event_date, ~event_type, ~event_classification,   ~lphu,  ~assigned_lphu, ~investigation_status, ~investigation_completed_date,
  "320266296123",                  "Enteric Diseases",                              "Anaphylaxis", "2026-06-30",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-07-01",
  "320266295041",                  "Enteric Diseases",                              "Anaphylaxis", "2026-06-29",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-07-01",
  "320266295029", "Sexually Transmissible Infections",                    "Syphilis - Infectious", "2026-06-29",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-30",
  "320266294569",                  "Enteric Diseases",                              "Anaphylaxis", "2026-06-28",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-29",
  "320266294267",                  "Enteric Diseases",                              "Anaphylaxis", "2026-06-27",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-30",
  "320266294235",                  "Enteric Diseases",                              "Anaphylaxis", "2026-06-27",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-29",
  "320266294321",                  "Enteric Diseases",                              "Anaphylaxis", "2026-06-27",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-29",
  "320266293802",               "Blood Borne Viruses",                "Hepatitis B - Unspecified", "2026-06-26",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-30",
  "320266293492",      "Vaccine Preventable Diseases",  "Varicella zoster infection (Chickenpox)", "2026-06-26",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-26",
  "320266293619",                  "Other Conditions", "Carbapenemase producing enterobacterales", "2026-06-26",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-07-02",
  "320266293170",                  "Other Conditions",                            "Legionellosis", "2026-06-26",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-07-02",
  "320266293429",                  "Enteric Diseases",                              "Anaphylaxis", "2026-06-26",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-26",
  "320266292865",                  "Enteric Diseases",                              "Anaphylaxis", "2026-06-25",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-29",
  "320266292768", "Sexually Transmissible Infections",                          "Syphilis - Late", "2026-06-25",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-07-01",
  "320266292445", "Sexually Transmissible Infections",                     "Gonococcal infection", "2026-06-25",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-30",
  "320266292915",                  "Enteric Diseases",                              "Anaphylaxis", "2026-06-25",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-26",
  "320266291209",                  "Enteric Diseases",                              "Anaphylaxis", "2026-06-24",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-30",
  "320266291710",               "Blood Borne Viruses",                "Hepatitis B - Unspecified", "2026-06-24",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-07-02",
  "320266292074", "Sexually Transmissible Infections",                    "Syphilis - Infectious", "2026-06-24",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-29",
  "320266291738", "Sexually Transmissible Infections",                     "Gonococcal infection", "2026-06-24",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-26",
  "320266292245",                  "Enteric Diseases",                              "Anaphylaxis", "2026-06-24",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-30",
  "320266291018",                  "Enteric Diseases",                              "Anaphylaxis", "2026-06-23",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-24",
  "320266291179",                  "Enteric Diseases",                              "Anaphylaxis", "2026-06-23",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-29",
  "320266290088",      "Vaccine Preventable Diseases",                                "Influenza", "2026-06-23",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-23",
  "320266290575", "Sexually Transmissible Infections",                     "Gonococcal infection", "2026-06-23",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-29"
  )


tibble::tribble(
                        ~event_id,                     ~condition_type,                                 ~condition,  ~event_date, ~event_type, ~event_classification,   ~lphu,  ~assigned_lphu, ~investigation_status, ~investigation_completed_date,
                   "320266296123",                  "Enteric Diseases",                              "Anaphylaxis", "2026-06-30",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-07-01",
                   "320266295041",                  "Enteric Diseases",                              "Anaphylaxis", "2026-06-29",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-07-01",
                   "320266295029", "Sexually Transmissible Infections",                    "Syphilis - Infectious", "2026-06-29",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-30",
                   "320266294569",                  "Enteric Diseases",                              "Anaphylaxis", "2026-06-28",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-29",
                   "320266294267",                  "Enteric Diseases",                              "Anaphylaxis", "2026-06-27",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-30",
                   "320266294235",                  "Enteric Diseases",                              "Anaphylaxis", "2026-06-27",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-29",
                   "320266294321",                  "Enteric Diseases",                              "Anaphylaxis", "2026-06-27",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-29",
                   "320266293802",               "Blood Borne Viruses",                "Hepatitis B - Unspecified", "2026-06-26",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-30",
                   "320266293492",      "Vaccine Preventable Diseases",  "Varicella zoster infection (Chickenpox)", "2026-06-26",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-26",
                   "320266293619",                  "Other Conditions", "Carbapenemase producing enterobacterales", "2026-06-26",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-07-02",
                   "320266293170",                  "Other Conditions",                            "Legionellosis", "2026-06-26",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-07-02",
                   "320266293429",                  "Enteric Diseases",                              "Anaphylaxis", "2026-06-26",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-26",
                   "320266292865",                  "Enteric Diseases",                              "Anaphylaxis", "2026-06-25",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-29",
                   "320266292768", "Sexually Transmissible Infections",                          "Syphilis - Late", "2026-06-25",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-07-01",
                   "320266292445", "Sexually Transmissible Infections",                     "Gonococcal infection", "2026-06-25",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-30",
                   "320266292915",                  "Enteric Diseases",                              "Anaphylaxis", "2026-06-25",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-26",
                   "320266291209",                  "Enteric Diseases",                              "Anaphylaxis", "2026-06-24",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-30",
                   "320266291710",               "Blood Borne Viruses",                "Hepatitis B - Unspecified", "2026-06-24",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-07-02",
                   "320266292074", "Sexually Transmissible Infections",                    "Syphilis - Infectious", "2026-06-24",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-29",
                   "320266291738", "Sexually Transmissible Infections",                     "Gonococcal infection", "2026-06-24",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-26",
                   "320266292245",                  "Enteric Diseases",                              "Anaphylaxis", "2026-06-24",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-30",
                   "320266291018",                  "Enteric Diseases",                              "Anaphylaxis", "2026-06-23",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-24",
                   "320266291179",                  "Enteric Diseases",                              "Anaphylaxis", "2026-06-23",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-29",
                   "320266290088",      "Vaccine Preventable Diseases",                                "Influenza", "2026-06-23",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-23",
                   "320266290575", "Sexually Transmissible Infections",                     "Gonococcal infection", "2026-06-23",      "Case",           "Confirmed", "NEPHU", "North Eastern",           "Completed",                  "2026-06-29"
                   )
# Load base table
base_table <- tibble::tribble(
                                       ~Disease,
                                  "Shigellosis",
                                         "STEC",
                          "Rotavirus infection",
                                  "Hepatitis E",
                                "Salmonellosis",
                      "Campylobacter infection",
                            "Cryptosporidiosis",
                                  "Hepatitis A",
                                         "FBWB",
                                  "Listeriosis",
                                      "Typhoid",
                                  "Paratyphoid",
                                        "Covid",
                                          "RSV",
                                    "Influenza",
                            "Other Rare Urgent",
                                  "Hepatitis B",
                                  "Hepatitis C",
                                  "Hepatitis D",
                                          "HIV",
                                     "Syphilis",
                                    "Chlamydia",
                         "Gonococcal infection",
                                  "Donovanosis",
                                         "Mpox",
                                    "Pertussis",
                                          "IPD",
                                    "Varicella",
                                          "RHD",
                                        "Mumps",
                                      "Rubella",
                                      "Tetanus",
                                          "IMD",
                                         "iGAS",
                                      "Measles",
                                          "HIB",
                                   "Diphtheria",
                                  "Chikungunya",
                                       "Dengue",
                       "Kunjin virus infection",
                                      "Malaria",
                       "Mycobacterium ulcerans",
                                          "JEV",
                                 "Yellow fever",
                                          "MVE",
                                      "Q Fever",
                                  "Brucellosis",
                                "Leptospirosis",
                                  "Psittacosis",
                                       "Rabies",
                                   "Lyssavirus",
                                      "Anthrax",
                                "Legionellosis",
                                 "Rare Urgents",
                                          "VRE",
                                         "CPOs",
                                "Candida auris",
                                          "CJD",
                                       "Res OB",
                                       "Ent OB"
                      )

# Load records table
entry_table <- tibble::tribble(
                             ~disease_group,                                     ~Disease, ~Records,
                                  "BBV-STI",                                    "BBV-STI",        8,
                                  "BBV-STI",                                  "Chlamydia",       NA,
                                  "BBV-STI",                                "Donovanosis",       NA,
                                  "BBV-STI",                       "Gonococcal infection",       NA,
                                  "BBV-STI",                                "Hepatitis B",       NA,
                                  "BBV-STI",                                "Hepatitis C",       NA,
                                  "BBV-STI",                                "Hepatitis D",       NA,
                                  "BBV-STI",                                        "HIV",        3,
                                  "BBV-STI",                                       "Mpox",        3,
                                  "BBV-STI",                                   "Syphilis",       NA,
                                  "Enteric",                                    "Enteric",       15,
                                  "Enteric",                                   "Botulism",       NA,
                                  "Enteric",                    "Campylobacter infection",       NA,
                                  "Enteric",                          "Cryptosporidiosis",        3,
                                  "Enteric",          "Food-borne or water-borne illness",       NA,
                                  "Enteric",                "Haemolytic Uraemic Syndrome",       NA,
                                  "Enteric",                                "Hepatitis A",        3,
                                  "Enteric",                                "Hepatitis E",       NA,
                                  "Enteric",                                "Listeriosis",       NA,
                                  "Enteric",                                "Paratyphoid",       NA,
                                  "Enteric",                              "Salmonellosis",       NA,
                                  "Enteric",                                       "STEC",       NA,
                                  "Enteric",                                "Shigellosis",       NA,
                                  "Enteric",                                    "Typhoid",       NA,
                                  "Enteric",                                     "Vibrio",       NA,
                                     "VPRD",                                       "VPRD",       18,
                                     "VPRD",                                 "Diphtheria",       NA,
                                     "VPRD",                                        "HIB",       NA,
                                     "VPRD",                                  "Influenza",       NA,
                                     "VPRD",                                       "iGAS",        3,
                                     "VPRD",                                    "Measles",       NA,
                                     "VPRD",                                        "IMD",       NA,
                                     "VPRD",                                      "Mumps",       NA,
                                     "VPRD",                                      "Covid",       NA,
                                     "VPRD",                                  "Pertussis",       NA,
                                     "VPRD",                                        "IPD",        3,
                                     "VPRD",                                        "RSV",       NA,
                                     "VPRD",                    "Rheumatic Heart Disease",       NA,
                                     "VPRD",                        "Rotavirus infection",       NA,
                                     "VPRD",                                    "Rubella",       NA,
                                     "VPRD",                                    "Tetanus",       NA,
                                     "VPRD",                                  "Varicella",       10,
                                    "VB-ZO",                                      "VB-ZO",       NA,
                                    "VB-ZO",              "Barmah Forest virus infection",       NA,
                                    "VB-ZO",                                "Chikungunya",       NA,
                                    "VB-ZO",                                     "Dengue",        2,
                                    "VB-ZO",                                 "Flavivirus",       NA,
                                    "VB-ZO",                      "Japanese encephalitis",       NA,
                                    "VB-ZO",                     "Kunjin virus infection",       NA,
                                    "VB-ZO",                                    "Malaria",        2,
                                    "VB-ZO", "Murray Valley encephalitis virus infection",       NA,
                                    "VB-ZO",                     "Mycobacterium ulcerans",       NA,
                                    "VB-ZO",                 "Ross River virus infection",       NA,
                                    "VB-ZO",                                 "Tularaemia",       NA,
                                    "VB-ZO",                               "Yellow fever",       NA,
                                    "VB-ZO",                                    "Anthrax",       NA,
                                    "VB-ZO",                  "Avian Influenza in humans",       NA,
                                    "VB-ZO",                                "Brucellosis",       NA,
                                    "VB-ZO",                              "Leptospirosis",        2,
                                    "VB-ZO",                                 "Lyssavirus",       NA,
                                    "VB-ZO",                                     "Plague",       NA,
                                    "VB-ZO",                                "Psittacosis",       NA,
                                    "VB-ZO",                                    "Q Fever",       NA,
                                    "VB-ZO",                                     "Rabies",       NA,
                         "Other Conditions",                           "Other Conditions",        4,
                         "Other Conditions",                              "Legionellosis",       NA,
                         "Other Conditions",                                       "SARS",       NA,
                         "Other Conditions",                  "Viral haemorrhagic fevers",       NA,
                                      "AMR",                                        "AMR",        7,
                                      "AMR",                              "Candida auris",       NA,
                                      "AMR",      "Carbapenemase producing acinetobacter",       NA,
                                      "AMR",   "Carbapenemase producing enterobacterales",       NA,
                                      "AMR",        "Carbapenemase producing pseudomonas",       NA,
                                      "AMR",     "VanA Vancomycin resistant enterococcus",       NA
                         )
# Define configurations
nephu_lgas <- c("Banyule (C)", "Boroondara (C)", "Darebin (C)", "Hume (C)", "Knox (C)", "Manningham (C)", "Maroondah (C)", "Nillumbik (S)", 
                "Whitehorse (C)", "Whittlesea (C)", "Yarra (C)", "Yarra Ranges (S)")


cpo <- c("Carbapenemase producing acinetobacter", "Carbapenemase producing enterobacterales", "Carbapenemase producing pseudomonas")
hepb <- c("Hepatitis B - Newly acquired", "Hepatitis B - Unspecified")
hepc <- c("Hepatitis C - Newly acquired", "Hepatitis C - Unspecified", "Hepatitis C - <24 months of age")
hiv <- c("Human Immunodeficiency Virus Infection - Newly acquired", "Human Immunodeficiency Virus infection - Unspecified", "Human Immunodeficiency Virus Infection - Individual aged 18 months or older")
syph <- c("Syphilis - Congenital", "Syphilis - Infectious", "Syphilis - Late", "Syphilis - Not further specified")
vzv <- c("Varicella zoster infection (Chickenpox)", "Varicella zoster infection (Shingles)", "Varicella zoster infection (Unspecified)")

inclu_defn <- c("Confirmed", "Probable", "At risk")
resp_ob <- c("Influenza", "Influenza A", "Influenza B", "Respiratory Syncytial virus", "Coronavirus")


bbv_sti <- c("Chlamydia", "Donavanosis", "Gonococcal infection", hiv, syph, "Mpox", hepb, hepc, "Hepatitis D")
ent <- c("Botulism", "Campylobacter infection", "Cryptosporidiosis", "Haemolytic Uraemic Syndrome", "Hepatitis A", "Hepatitis E", "Listeriosis", "Paratyphoid", 
         "Salmonellosis", "STEC", "Shigellosis", "Typhoid", "Vibrio")
vprd <- c("Diphtheria", "HIB", "Influenza", "iGAS", "Measles", "IMD", "Mumps", "Covid", "Pertussis", "IPD", "RSV", "RHD", "Rotavirus infection", "Rubella", 
          "Tetanus", vzv)
vb_zo <- c("Barmah Forest virus infection", "Chikungunya", "Dengue", "Flavivirus", "JEV", "Kunjin virus infection", "Malaria", "MVEV", "Mycobacterium ulcerans", 
           "RRV", "Tularaemia", "Yellow Fever", "Anthrax", "Avian Influenza in humans", "Brucellosis", "Leptospirosis", "Lyssavirus", "Plague", "Psittacosis", 
           "Q Fever", "Rabies")
other <- c("Legionellosis", "SARS", "Viral haemorrhagic fevers")
amr <- c("Candida auris", cpo, "VRE")

disease_group <- c("AMR", "BBV-STI", "Enteric diseases", "Others", "VB-ZO", "VPRD")


# configure data to filter only for completed cases and outbreaks and rename for Diseases
condition.subset <- case_raw %>% 
  rename(defn = event_classification, 
         phess_id = event_id) %>% 
  distinct(phess_id, .keep_all=TRUE) %>% 
  #filter(between(date_completed, as.Date(start_date3mth), as.Date(start_date2mth))) %>% 
  #filter(investigation_status=="Completed") %>% 
  filter(event_type=="Case") %>% 
  filter(defn %in% inclu_defn) %>% 
  #filter(lga %in% nephu_lgas) %>% 
  #filter(lphu=="North Eastern") %>% 
  #filter(follow_up_required_by_last_iteration=="Local Public Health Unit") %>% 
  #filter(!is.na(acknowledged_by)) %>% 
  # adjust for MU since has long follow-up time (>6mths)
  mutate(investigation_outcome = if_else(condition=="Mycobacterium ulcerans" & investigation_status!="New", 
                                         "Completed", investigation_status)) %>% 
  filter(investigation_outcome == "Completed") %>% 
  mutate(Disease = case_when(
                             condition %in% hepb ~ "Hepatitis B", 
                             condition %in% hepc ~ "Hepatitis C", 
                             condition %in% hiv ~ "HIV", 
                             condition %in% syph ~ "Syphilis", 
                             condition %in% vzv ~ "Varicella", 
                             #condition %in% other_rare_urgents ~ "Other Rare Urgent", 
                             #condition %in% rare_urgents ~ "Rare Urgents", 
                             condition == "Chlamydia trachomatis infection" ~ "Chlamydia", 
                             condition == "Shiga-toxin and Vero-toxin producing Escherichia coli" ~ "STEC", 
                             condition == "Vibrio parahaemolyticus infection" ~ "Vibrio", 
                             condition == "Creutzfeldt-Jakob disease (CJD)" ~ "CJD", 
                             condition == "Severe Acute Respiratory Syndrome (SARS)" ~ "SARS", 
                             condition == "Meningococcal infection (IMD)" ~ "IMD", 
                             condition == "Pneumococcal infection (IPD)" ~ "IPD", 
                             condition == "Invasive Group A Streptococcus" ~ "iGAS", 
                             condition == "Haemophilus influenzae type B infection" ~ "HIB", 
                             condition == "Food-borne or water-borne illness" ~ "FBWB", 
                             condition == "Respiratory Syncytial virus" ~ "RSV", 
                             condition == "Novel Coronavirus (2019-nCoV)" ~ "Covid", 
                             condition == "Lyssavirus - Australian Bat Lyssavirus" ~ "Lyssavirus", 
                             condition == "Chikungunya virus infection" ~ "Chikungunya", 
                             condition == "Dengue virus infection" ~ "Dengue", 
                             TRUE ~ condition)) %>% 
  select(Disease, investigation_completed_date, condition, phess_id)


# Create disease groups variable in condition.subset data
extract.subset <- left_join(condition.subset, entry_table %>% select(-Records), by="Disease") %>% filter(Disease!="Anaphylaxis")

# 2 separate joins - first by disease_groups and second by Disease

# By disease_group
extract_diseasegrp <- extract.subset %>% 
  group_by(disease_group)

diseasegrp_vector <- extract_diseasegrp %>% 
  group_keys() %>% 
  pull(disease_group) %>% 
  data.frame() %>% 
  setNames("disease_group") %>% 
  na.omit()

record_numbers_diseasegrp <- left_join(diseasegrp_vector, entry_table %>% filter(Disease %in% disease_group), by="disease_group") %>% 
  filter(!is.na(Records))

select_diseasegrp_list <- extract_diseasegrp %>% 
  filter(disease_group %in% record_numbers_diseasegrp$disease_group) %>% 
  group_split() %>% 
  map2(.y = record_numbers_diseasegrp$Records, .f = random_cut) %>% 
  list_rbind()


# By disease
extract_disease <- extract.subset %>% 
  group_by(Disease)

disease_vector <- extract_disease %>% 
  group_keys() %>% 
  pull(Disease) %>% 
  data.frame() %>% 
  setNames("Disease")

record_numbers_disease <- left_join(disease_vector, entry_table %>% filter(!Disease %in% disease_group), by="Disease") %>% 
  filter(!is.na(Records))

select_disease_list <- extract_disease %>% 
  filter(Disease %in% record_numbers_disease$Disease) %>% 
  group_split() %>% 
  map2(.y = record_numbers_disease$Records, .f = random_cut) %>% 
  list_rbind()


# Combine both select_lists
combined_select_list <- bind_rows(select_diseasegrp_list, select_disease_list) %>% 
  # remove duplicates
  distinct(phess_id, .keep_all = TRUE)

# merge with entry table to produce final list table
final_select_list <- entry_table %>% 
  # remove rows corresponding to the disease group record request
  filter(!Disease %in% disease_group) %>% 
  left_join(combined_select_list %>% select(-disease_group), by = "Disease") %>% 
  # remove rows for which records was not requested for disease nor disease group
  filter(!(is.na(Records) & is.na(phess_id))) %>% 
  mutate(phess_id = if_else(is.na(phess_id), "No cases recently", phess_id)) %>% 
  arrange(disease_group, Disease) %>% 
  select(-Records)



# 
# random_select_table_preselect <- random_select_table_all |> 
#   group_by(Disease) |> 
#   group_split() |> 
#   slice_sample(n=Records)
#   purrr::map_dfr(slice_sample(n = Records))
#   purrr::map_dfr(~ slice_sample(.x, n = unique(.x$Records)))
#   select(disease, record_number) # Keeps only original columns from dataset A
#   slice_sample(n = first(Records)) |> 
#   ungroup()


  

# if(select_mth=="all") {
#   monthly_extract = random5_table
# } else {
#   monthly_extract <- random5_table %>% 
#     filter(Disease %in% !!sym(select_mth)) 
# }


# Export df to excel sheet
output_file_name <- paste0("Quarterly_review_cases_", select_qtr, "_", format(Sys.Date(), '%d%m%Y'), ".xlsx")
writexl::write_xlsx(request_df, here("Output", output_file_name))



#--TESTING GROUND with smaller datasets---

extract_list <- random_select_table_all |> 
  filter(Disease %in% c("Shigellosis", "STEC", "Cryptosporidiosis", "Hepatitis B"))


record_numbers_list <- data.frame(
  Disease = c("Shigellosis", "STEC", "Cryptosporidiosis", "Hepatitis B"), 
  records = c(3, 5, 2, 4)
)

random_cut <- function(x1, x2) {
  table <- slice_sample(x1, n=x2)
  return(table)
}

# Algorithm for merging and doing the random sampling
# tried to join all the tables first and then sample from the single table but doesn't seem to work
# Use the purrr::map2() function instead

# 1. create the groups
random_select_records_group <- extract_list |> 
  group_by(Disease) 

# 2. Get the vector of group names
group_vector <- random_select_records_group %>% group_keys() %>% pull(Disease) |> 
  data.frame() |> 
  setNames("Disease")

# 3. Rearrange the records table so that disease is in order of group_vector
record_numbers_list <- left_join(group_vector, record_numbers_list, by="Disease")

# 4. Split the groups
random_select_records <- random_select_records_group |> 
  group_split() |> 
  map2(.y = record_numbers_list$records, .f = random_cut) |> 
  list_rbind()


# --

condition.group <- condition.subset %>% 
  group_by(Disease)

group_vector <- condition.group %>% 
  group_keys() %>% 
  pull(Disease) %>% 
  data.frame() %>% 
  setNames("Disease")

record_numbers_list <- left_join(group_vector, entry_table, by="Disease") %>% 
  filter(!is.na(Records))

select_list <- condition.group %>% 
  filter(Disease %in% record_numbers_list$Disease) %>% 
  group_split() %>% 
  map2(.y = record_numbers_list$Records, .f = random_cut) %>% 
  list_rbind()

final_select_list <- entry_table %>% 
  filter(!is.na(Records)) %>% 
  left_join(select_list, by = "Disease") %>% 
  mutate(phess_id = if_else(is.na(phess_id), "No cases recently", phess_id))
