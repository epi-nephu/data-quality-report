# Script for randomly selecting x number of conditions a calendar quarter
# Number of cases and condition type will be specified by Asmara and sent through to us each quarter

# INSTRUCTIONS (HOW TO USE THIS R SCRIPT)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Define parameters:
# Select month by entering the first 3 letters of the month in small letters within quotation marks. Eg: "mar", "sep", "dec", etc.
# if you require all months, then enter "all"
select_qtr <- "Q3-2025"

# select the max number of cases you want to randomly select.
max_cases <- 5

# Now highlight the entire page and click "Run" to execute the entire code

# An Excel file will be generated and stored in the Output sub-folder in this main project folder

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# load libraries
library(here)
library(tidyverse)
library(lubridate)
library(janitor)


# set up connection to PHAR
con <- DBI::dbConnect(odbc::odbc(), "PHAR")


# extract data from PHAR
case_raw <- DBI::dbGetQuery(con, 
                           "
                           SELECT * 
                           FROM dh_public_health.phess_release.caseevents 
                           WHERE ASSIGNED_LPHU = 'North Eastern' 
                           AND EVENT_DATE BETWEEN '2025-07-01' AND '2025-09-30' 
                           ORDER BY EVENT_DATE DESC; 
                           "
) %>% 
  clean_names()

ob_raw <- DBI::dbGetQuery(con, 
                          "
                           SELECT * 
                           FROM dh_public_health.phess_release.outbreakevents
                           WHERE ASSIGNED_LPHU = 'North Eastern' 
                           AND EVENT_DATE BETWEEN '2025-07-01' AND '2025-09-30' 
                           ORDER BY EVENT_DATE DESC; 
                           "
) %>% 
  clean_names()


# Load base table
base_table <- readxl::read_xlsx(here("Data", "Monthly Review Base Table.xlsx"))


# Define configurations
nephu_lgas <- c("Banyule (C)", "Boroondara (C)", "Darebin (C)", "Hume (C)", "Knox (C)", "Manningham (C)", "Maroondah (C)", "Nillumbik (S)", 
                "Whitehorse (C)", "Whittlesea (C)", "Yarra (C)", "Yarra Ranges (S)")

urgent_cond <- c("Anthrax", "Botulism", "Candida auris", "Cholera", "COVID-19", "Diphtheria", "Food-borne or water-borne illness", 
                 "Haemolytic Uraemic Syndrome", "Haemophilus influenzae Type B (HiB)", "Hepatitis A", "Meningococcal infection", 
                 "Japanese encephalitis", "Legionellosis", "Listeriosis", "Lyssavirus - Australian Bat Lyssavirus", "Measles", 
                 "Middle East Respiratory Syndrome (MERS)", "Mpox", "Murray Valley Encephalitis Virus", "Paratyphoid", "Plague", 
                 "Poliomyelitis", "Rabies", "Severe Acute Respiratory Syndrome (SARS)", "Smallpox", "Tularaemia", "Typhoid", 
                 "Viral haemorrhagic fevers", "Yellow Fever")

cpo <- c("Carbapenemase producing acinetobacter", "Carbapenemase producing enterobacterales", "Carbapenemase producing pseudomonas")
hepb <- c("Hepatitis B - Newly acquired", "Hepatitis B - Unspecified")
hepc <- c("Hepatitis C - Newly acquired", "Hepatitis C - Unspecified", "Hepatitis C - <24 months of age")
hiv <- c("Human Immunodeficiency Virus Infection - Newly acquired", "Human Immunodeficiency Virus infection - Unspecified", "Human Immunodeficiency Virus Infection - Individual aged 18 months or older")
syph <- c("Syphilis - Congenital", "Syphilis - Infectious", "Syphilis - Late", "Syphilis - Not further specified")
vzv <- c("Varicella zoster infection (Chickenpox)", "Varicella zoster infection (Shingles)", "Varicella zoster infection (Unspecified)")
rare_urgents <- c("Middle East Respiratory Syndrome (MERS)", "Zika virus", "Barmah Forest virus infection", 
                  "Ross River virus infection", "Flavivirus", "Avian influenza", "Plague", "Tularaemia", 
                  "Viral haemorrhagic fevers")
other_rare_urgents <- c("Botulism", "Cholera", "Haemolytic Uraemic Syndrome", "Leprosy", 
                        "Severe Acute Respiratory Syndrome (SARS)", "Smallpox")
inclu_defn <- c("Confirmed", "Probable", "At risk")
resp_ob <- c("Influenza", "Influenza A", "Influenza B", "Respiratory Syncytial virus", "Coronavirus")



# Disease selection for each month
jan <- c("STEC", "Salmonellosis", "Cryptosporidiosis", "Hepatitis A", "RSV", "Other Rare Urgent", 
         "Hepatitis C", "Hepatitis D", "Chlamydia", "Mpox", "Pertussis", "Measles", "Diptheria", 
         "Malaria", "Mycobacterium ulcerans", "Rabies", "CJD")

feb <- c("Shigellosis", "Rotavirus infection", "Salmonellosis", "Cryptosporidiosis", "Listeriosis", "Typhoid", 
         "Hepatitis B", "Hepatitis C", "HIV", "Mpox", "Pertussis", "Varicella", "IMD", "Dengue", "Rabies", 
         "Legionellosis", "CPOs")

mar <- c("STEC", "Hepatitis E", "Campylobacter infection", "Hepatitis A", "Paratyphoid", "Influenza", 
         "Hepatitis C", "Hepatitis D", "Syphilis", "Mpox", "IPD", "RHD", "iGAS", "Rare Urgents", "Malaria", 
         "Mycobacterium ulcerans", "Candida auris")

apr <- c("Shigellosis", "Rotavirus infection", "Campylobacter infection", "FBWB", "Listeriosis", "Covid", 
         "Hepatitis B", "HIV", "Donovanosis", "Mpox", "Mumps", "Rubella", "Measles", "Chikungunya", 
         "Kunjin virus infection", "Legionellosis")

may <- c("STEC", "Salmonellosis", "Cryptosporidiosis", "Hepatitis A", "RSV", "Other Rare Urgent", "Hepatitis C", 
         "Hepatitis D", "Gonococcal infection", "Mpox", "Pertussis", "Tetanus", "iGAS", "Mycobacterium ulcerans", 
         "Q fever", "Rabies", "CJD")

jun <- c("Shigellosis", "Rotafivurs infection", "Salmonellosis", "Cryptosporidiosis", "Listeriosis", "Typhoid", 
         "Hepatitis B", "Hepatitis C", "HIV", "Mpox", "IMD", "iGAS", "HIB", "JEV", "Psittacosis", 
         "Legionellosis", "CPOs")

jul <- c("STEC", "Hepatitis E", "Campylobacter infection", "Hepatitis A", "Paratyphoid", "Influenza", 
         "Hepatitis C", "Hepatitis D", "Syphilis", "Mpox", "Pertussis", "Measles", "Diptheria", "Mycobacterium ulcerans", 
         "Rabies", "Lyssavirus", "Candida auris")

aug <- c("Shigellosis", "Rotavirus infection", "Campylobacter infection", "FBWB", "Listeriosis", "Covid", 
         "Hepatitis B", "HIV", "Donovanosis", "Mpox", "Pertussis", "Varicella", "IMD", "Yellow fever", "MVE", 
         "Legionellosis")

sep <- c("STEC", "Salmonellosis", "Cryptosporidiosis", "Hepatitis A", "RSV", "Other Rare Urgent", 
         "Hepatitis C", "Hepatitis D", "Chlamydia", "Mpox", "IPD", "RHD", "iGAS", "Mycobacterium ulcerans", 
         "Q Fever", "Brucellosis", "CJD")

oct <- c("Shigellosis", "Rotavirus infection", "Salmonellosis", "Cryptosporidiosis", "Listeriosis", "Typhoid", 
         "Hepatitis B", "Hepatitis C", "HIV", "Mpox", "Mumps", "Rubella", "Measles", "Leptospirosis", "Psittacosis", 
         "Legionellosis", "CPOs")

nov <- c("STEC", "Hepatitis E", "Campylobacter infection", "Hepatitis A", "Paratyphoid", "Influenza", 
         "Hepatitis C", "Hepatitis D", "Syphilis", "Mpox", "Pertussis", "Tetanus", "iGAS", "Mycobacterium ulcerans", 
         "Rabies", "Rare Urgents", "Candida auris")

dec <- c("Shigellosis", "Rotavirus infection", "Campylobacter infection", "FBWB", "Listeriosis", "Covid", 
         "Hepatitis B", "HIV", "Donovanosis", "Mpox", "IMD", "iGAS", "HIB", "Dengue", "Brucellosis", 
         "Legionellosis")



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
  mutate(Disease = case_when(condition %in% cpo ~ "CPOs", 
                             condition %in% hepb ~ "Hepatitis B", 
                             condition %in% hepc ~ "Hepatitis C", 
                             condition %in% hiv ~ "HIV", 
                             condition %in% syph ~ "Syphilis", 
                             condition %in% vzv ~ "Varicella", 
                             condition %in% other_rare_urgents ~ "Other Rare Urgent", 
                             condition %in% rare_urgents ~ "Rare Urgents", 
                             condition == "Chlamydia trachomatis infection" ~ "Chlamydia", 
                             condition == "Shiga-toxin and Vero-toxin producing Escherichia coli" ~ "STEC", 
                             condition == "VanA Vancomycin resistant enterococcus" ~ "VRE", 
                             condition == "Creutzfeldt-Jakob disease (CJD)" ~ "CJD", 
                             condition == "Meningococcal infection (IMD)" ~ "IMD", 
                             condition == "Pneumococcal infection (IPD)" ~ "IPD", 
                             condition == "Invasive Group A Streptococcus" ~ "iGAS", 
                             condition == "Haemophilus influenzae type B infection" ~ "HIB", 
                             condition == "Food-borne or water-borne illness" ~ "FBWB", 
                             condition == "Rheumatic Heart Disease" ~ "RHD", 
                             condition == "Respiratory Syncytial virus" ~ "RSV", 
                             condition == "Novel Coronavirus (2019-nCoV)" ~ "Covid", 
                             condition == "Lyssavirus - Australian Bat Lyssavirus" ~ "Lyssavirus", 
                             condition == "Murray Valley encephalitis virus infection" ~ "MVE", 
                             condition == "Chikungunya virus infection" ~ "Chikungunya", 
                             condition == "Japanese encephalitis" ~ "JEV", 
                             condition == "Dengue virus infection" ~ "Dengue", 
                             TRUE ~ condition)) %>% 
  select(Disease, investigation_completed_date, condition, phess_id)


ob.subset <- ob_raw %>% 
  rename(defn = event_classification, 
         phess_id = event_id) %>% 
  distinct(phess_id, .keep_all=TRUE) %>% 
  filter(investigation_status == "Completed") %>% 
  filter(event_type == "Outbreak") %>% 
  filter(defn == "Confirmed") %>% 
  mutate(OB_type = case_when(organism_cause %in% resp_ob ~ "Res OB", 
                             condition_type == "Enteric Diseases" ~ "Ent OB", 
                             TRUE ~ "Missing")) %>% 
  select(OB_type, create_date, organism_cause, phess_id)

# combined condition and ob subsets
combined <- condition.subset %>% 
  select(-investigation_completed_date) %>% 
  bind_rows(ob.subset %>% 
              select(-create_date) %>% 
              filter(OB_type %in% c("Res OB", "Ent OB")) %>% 
              rename(Disease = OB_type, 
                     condition = organism_cause)
            )


# Random select 5 cases on each disease
random5 <- combined %>% 
  group_by(Disease) %>% 
  slice_sample(n=5)

# Merge into base table (for layout)
random5_table <- left_join(base_table, random5, by="Disease") %>% 
  mutate(phess_id = if_else(is.na(phess_id), "No cases recently", phess_id))

# if(select_mth=="all") {
#   monthly_extract = random5_table
# } else {
#   monthly_extract <- random5_table %>% 
#     filter(Disease %in% !!sym(select_mth)) 
# }

request_selection <- c("iGAS", "Pertussis", "Hepatitis B", "Hepatitis C", "IPD", "Measles", "Mpox", 
                       "Typhoid", "STEC", "Dengue", "Mycobacterium ulcerans", "Res OB", "Ent OB")

request_df <- random5_table %>% 
  filter(Disease %in% request_selection)


# Export df to excel sheet
output_file_name <- paste0("Quarterly_review_cases_", select_qtr, "_", format(Sys.Date(), '%d%m%Y'), ".xlsx")
writexl::write_xlsx(request_df, here("Output", output_file_name))

tabyl(case_raw, condition, investigation_status)
typhoid <- case_raw %>% 
  filter(condition=="Typhoid")

mu <- case_raw %>% 
  filter(condition=="Mycobacterium ulcerans")
