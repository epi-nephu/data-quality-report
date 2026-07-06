# Script for randomly selecting x number of conditions a calendar quarter from a pre-specified table
# Testing ground for converting to Shiny app
# Number of cases and condition type will be specified by Asmara and sent through to us each quarter

# INSTRUCTIONS (HOW TO USE THIS R SCRIPT)
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Define parameters:
# Select month by entering the first 3 letters of the month in small letters within quotation marks. Eg: "mar", "sep", "dec", etc.
# if you require all months, then enter "all"
select_qtr <- "Q2-2026"

# select the max number of cases you want to randomly select.
max_cases <- 15

# Now highlight the entire page and click "Run" to execute the entire code

# An Excel file will be generated and stored in the Output sub-folder in this main project folder

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Version Control Notes:
# - include code to determine start and end dates for use in dbGetQuery function to extract data from PHESS

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# load libraries
library(here)
library(tidyverse)
library(janitor)

# determine start and end dates for data extract
year <- str_extract(select_qtr, "20\\d\\d")
quarter <- str_extract(select_qtr, "Q\\d")
if(quarter=="Q1") {
  start_date = paste0(year, "-01-01")
  end_date = paste0(year, "-03-31")
}
if(quarter=="Q2") {
  start_date = paste0(year, "-04-01")
  end_date = paste0(year, "-06-30")
}
if(quarter=="Q3") {
  start_date = paste0(year, "-07-01")
  end_date = paste0(year, "-09-30")
}
if(quarter=="Q4") {
  start_date = paste0(year, "-10-01")
  end_date = paste0(year, "-12-31")
}
start_date <- ymd(start_date)
end_date <- ymd(end_date)


# Load extract data
case_raw <- readxl::read_xlsx(here("Data", "Extract_Q2_2026.xlsx")) |> 
  clean_names() |> 
  mutate(investigation_completed_date = if_else(investigation_completed_date=="null", NA_Date_, 
                                                janitor::excel_numeric_to_date(as.numeric(investigation_completed_date))))

# Load base table
base_table <- readxl::read_xlsx(here("Data", "Monthly Review Base Table.xlsx"))

# Load records table
entry_table <- readxl::read_xlsx(here("Data", "Records Entry Table.xlsx"), sheet=2) %>% rename(disease_group = "Disease Group")


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
