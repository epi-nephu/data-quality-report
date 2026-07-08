# ============================================================
# Random Case Selection Shiny App — numeric input box version
#
# Same sampling logic as app.R, but the "Records" column is entered
# via one numeric input box per disease (grouped under its
# disease_group), instead of an editable DT table.
# ============================================================

library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(tibble)

# ------------------------------------------------------------
# 1. Static lookup vectors (condition -> standardised Disease name)
# ------------------------------------------------------------

cpo  <- c("Carbapenemase producing acinetobacter", "Carbapenemase producing enterobacterales",
          "Carbapenemase producing pseudomonas")
hepb <- c("Hepatitis B - Newly acquired", "Hepatitis B - Unspecified")
hepc <- c("Hepatitis C - Newly acquired", "Hepatitis C - Unspecified", "Hepatitis C - <24 months of age")
hiv  <- c("Human Immunodeficiency Virus Infection - Newly acquired",
          "Human Immunodeficiency Virus infection - Unspecified",
          "Human Immunodeficiency Virus Infection - Individual aged 18 months or older")
syph <- c("Syphilis - Congenital", "Syphilis - Infectious", "Syphilis - Late",
          "Syphilis - Not further specified")
vzv  <- c("Varicella zoster infection (Chickenpox)", "Varicella zoster infection (Shingles)",
          "Varicella zoster infection (Unspecified)")

inclu_defn <- c("Confirmed", "Probable", "At risk")

# ------------------------------------------------------------
# 2. entry_table: Disease <-> disease_group map + default Records.
#    Rows where Disease == disease_group are "whole group" requests.
# ------------------------------------------------------------

entry_table_default <- tribble(
  ~disease_group,      ~Disease,                                     ~Records,
  "BBV-STI",           "BBV-STI",                                    8,
  "BBV-STI",           "Chlamydia",                                  NA,
  "BBV-STI",           "Donovanosis",                                NA,
  "BBV-STI",           "Gonococcal infection",                       NA,
  "BBV-STI",           "Hepatitis B",                                NA,
  "BBV-STI",           "Hepatitis C",                                NA,
  "BBV-STI",           "Hepatitis D",                                NA,
  "BBV-STI",           "HIV",                                        3,
  "BBV-STI",           "Mpox",                                       3,
  "BBV-STI",           "Syphilis",                                   NA,
  "Enteric",           "Enteric",                                    15,
  "Enteric",           "Botulism",                                   NA,
  "Enteric",           "Campylobacter infection",                    NA,
  "Enteric",           "Cryptosporidiosis",                          3,
  "Enteric",           "Food-borne or water-borne illness",          NA,
  "Enteric",           "Haemolytic Uraemic Syndrome",                NA,
  "Enteric",           "Hepatitis A",                                3,
  "Enteric",           "Hepatitis E",                                NA,
  "Enteric",           "Listeriosis",                                NA,
  "Enteric",           "Paratyphoid",                                NA,
  "Enteric",           "Salmonellosis",                               NA,
  "Enteric",           "STEC",                                       NA,
  "Enteric",           "Shigellosis",                                NA,
  "Enteric",           "Typhoid",                                    NA,
  "Enteric",           "Vibrio",                                     NA,
  "VPRD",              "VPRD",                                       18,
  "VPRD",              "Diphtheria",                                 NA,
  "VPRD",              "HIB",                                        NA,
  "VPRD",              "Influenza",                                  NA,
  "VPRD",              "iGAS",                                       3,
  "VPRD",              "Measles",                                    NA,
  "VPRD",              "IMD",                                        NA,
  "VPRD",              "Mumps",                                      NA,
  "VPRD",              "Covid",                                      NA,
  "VPRD",              "Pertussis",                                  NA,
  "VPRD",              "IPD",                                        3,
  "VPRD",              "RSV",                                        NA,
  "VPRD",              "Rheumatic Heart Disease",                    NA,
  "VPRD",              "Rotavirus infection",                        NA,
  "VPRD",              "Rubella",                                    NA,
  "VPRD",              "Tetanus",                                    NA,
  "VPRD",              "Varicella",                                  10,
  "VB-ZO",             "VB-ZO",                                      NA,
  "VB-ZO",             "Barmah Forest virus infection",               NA,
  "VB-ZO",             "Chikungunya",                                NA,
  "VB-ZO",             "Dengue",                                     2,
  "VB-ZO",             "Flavivirus",                                 NA,
  "VB-ZO",             "Japanese encephalitis",                      NA,
  "VB-ZO",             "Kunjin virus infection",                     NA,
  "VB-ZO",             "Malaria",                                    2,
  "VB-ZO",             "Murray Valley encephalitis virus infection", NA,
  "VB-ZO",             "Mycobacterium ulcerans",                     NA,
  "VB-ZO",             "Ross River virus infection",                 NA,
  "VB-ZO",             "Tularaemia",                                 NA,
  "VB-ZO",             "Yellow fever",                               NA,
  "VB-ZO",             "Anthrax",                                    NA,
  "VB-ZO",             "Avian Influenza in humans",                  NA,
  "VB-ZO",             "Brucellosis",                                NA,
  "VB-ZO",             "Leptospirosis",                               2,
  "VB-ZO",             "Lyssavirus",                                 NA,
  "VB-ZO",             "Plague",                                     NA,
  "VB-ZO",             "Psittacosis",                                NA,
  "VB-ZO",             "Q Fever",                                    NA,
  "VB-ZO",             "Rabies",                                     NA,
  "Other Conditions",  "Other Conditions",                           4,
  "Other Conditions",  "Legionellosis",                              NA,
  "Other Conditions",  "SARS",                                       NA,
  "Other Conditions",  "Viral haemorrhagic fevers",                  NA,
  "AMR",               "AMR",                                        7,
  "AMR",               "Candida auris",                              NA,
  "AMR",               "Carbapenemase producing acinetobacter",      NA,
  "AMR",               "Carbapenemase producing enterobacterales",   NA,
  "AMR",               "Carbapenemase producing pseudomonas",        NA,
  "AMR",               "VanA Vancomycin resistant enterococcus",     NA
)

# ------------------------------------------------------------
# 3. Sample case_raw (used until the user uploads their own extract)
# ------------------------------------------------------------

case_raw_sample <- tribble(
  ~event_id,        ~condition_type,                      ~condition,                                 ~event_date,   ~event_type, ~event_classification, ~lphu,   ~assigned_lphu,   ~investigation_status, ~investigation_completed_date,
  "320266296123", "Enteric Diseases",                   "Anaphylaxis",                              "2026-06-30", "Case",       "Confirmed",           "NEPHU", "North Eastern", "Completed",            "2026-07-01",
  "320266295041", "Enteric Diseases",                   "Anaphylaxis",                              "2026-06-29", "Case",       "Confirmed",           "NEPHU", "North Eastern", "Completed",            "2026-07-01",
  "320266295029", "Sexually Transmissible Infections",  "Syphilis - Infectious",                    "2026-06-29", "Case",       "Confirmed",           "NEPHU", "North Eastern", "Completed",            "2026-06-30",
  "320266294569", "Enteric Diseases",                   "Anaphylaxis",                              "2026-06-28", "Case",       "Confirmed",           "NEPHU", "North Eastern", "Completed",            "2026-06-29",
  "320266294267", "Enteric Diseases",                   "Anaphylaxis",                              "2026-06-27", "Case",       "Confirmed",           "NEPHU", "North Eastern", "Completed",            "2026-06-30",
  "320266294235", "Enteric Diseases",                   "Anaphylaxis",                              "2026-06-27", "Case",       "Confirmed",           "NEPHU", "North Eastern", "Completed",            "2026-06-29",
  "320266294321", "Enteric Diseases",                   "Anaphylaxis",                              "2026-06-27", "Case",       "Confirmed",           "NEPHU", "North Eastern", "Completed",            "2026-06-29",
  "320266293802", "Blood Borne Viruses",                "Hepatitis B - Unspecified",                "2026-06-26", "Case",       "Confirmed",           "NEPHU", "North Eastern", "Completed",            "2026-06-30",
  "320266293492", "Vaccine Preventable Diseases",       "Varicella zoster infection (Chickenpox)",  "2026-06-26", "Case",       "Confirmed",           "NEPHU", "North Eastern", "Completed",            "2026-06-26",
  "320266293619", "Other Conditions",                   "Carbapenemase producing enterobacterales", "2026-06-26", "Case",       "Confirmed",           "NEPHU", "North Eastern", "Completed",            "2026-07-02",
  "320266293170", "Other Conditions",                   "Legionellosis",                            "2026-06-26", "Case",       "Confirmed",           "NEPHU", "North Eastern", "Completed",            "2026-07-02",
  "320266293429", "Enteric Diseases",                   "Anaphylaxis",                              "2026-06-26", "Case",       "Confirmed",           "NEPHU", "North Eastern", "Completed",            "2026-06-26",
  "320266292865", "Enteric Diseases",                   "Anaphylaxis",                              "2026-06-25", "Case",       "Confirmed",           "NEPHU", "North Eastern", "Completed",            "2026-06-29",
  "320266292768", "Sexually Transmissible Infections",  "Syphilis - Late",                          "2026-06-25", "Case",       "Confirmed",           "NEPHU", "North Eastern", "Completed",            "2026-07-01",
  "320266292445", "Sexually Transmissible Infections",  "Gonococcal infection",                     "2026-06-25", "Case",       "Confirmed",           "NEPHU", "North Eastern", "Completed",            "2026-06-30",
  "320266292915", "Enteric Diseases",                   "Anaphylaxis",                              "2026-06-25", "Case",       "Confirmed",           "NEPHU", "North Eastern", "Completed",            "2026-06-26",
  "320266291209", "Enteric Diseases",                   "Anaphylaxis",                              "2026-06-24", "Case",       "Confirmed",           "NEPHU", "North Eastern", "Completed",            "2026-06-30",
  "320266291710", "Blood Borne Viruses",                "Hepatitis B - Unspecified",                "2026-06-24", "Case",       "Confirmed",           "NEPHU", "North Eastern", "Completed",            "2026-07-02",
  "320266292074", "Sexually Transmissible Infections",  "Syphilis - Infectious",                    "2026-06-24", "Case",       "Confirmed",           "NEPHU", "North Eastern", "Completed",            "2026-06-29",
  "320266291738", "Sexually Transmissible Infections",  "Gonococcal infection",                     "2026-06-24", "Case",       "Confirmed",           "NEPHU", "North Eastern", "Completed",            "2026-06-26",
  "320266292245", "Enteric Diseases",                   "Anaphylaxis",                              "2026-06-24", "Case",       "Confirmed",           "NEPHU", "North Eastern", "Completed",            "2026-06-30",
  "320266291018", "Enteric Diseases",                   "Anaphylaxis",                              "2026-06-23", "Case",       "Confirmed",           "NEPHU", "North Eastern", "Completed",            "2026-06-24",
  "320266291179", "Enteric Diseases",                   "Anaphylaxis",                              "2026-06-23", "Case",       "Confirmed",           "NEPHU", "North Eastern", "Completed",            "2026-06-29",
  "320266290088", "Vaccine Preventable Diseases",       "Influenza",                                "2026-06-23", "Case",       "Confirmed",           "NEPHU", "North Eastern", "Completed",            "2026-06-23",
  "320266290575", "Sexually Transmissible Infections",  "Gonococcal infection",                     "2026-06-23", "Case",       "Confirmed",           "NEPHU", "North Eastern", "Completed",            "2026-06-29"
)

# ------------------------------------------------------------
# 4. Core logic (identical to app.R)
# ------------------------------------------------------------

map_conditions_to_disease <- function(case_raw) {
  case_raw %>%
    rename(defn = event_classification, phess_id = event_id) %>%
    distinct(phess_id, .keep_all = TRUE) %>%
    filter(event_type == "Case") %>%
    filter(defn %in% inclu_defn) %>%
    mutate(investigation_outcome = if_else(
      condition == "Mycobacterium ulcerans" & investigation_status != "New",
      "Completed", investigation_status
    )) %>%
    filter(investigation_outcome == "Completed") %>%
    mutate(Disease = case_when(
      condition %in% hepb ~ "Hepatitis B",
      condition %in% hepc ~ "Hepatitis C",
      condition %in% hiv  ~ "HIV",
      condition %in% syph ~ "Syphilis",
      condition %in% vzv  ~ "Varicella",
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
      TRUE ~ condition
    )) %>%
    select(Disease, investigation_completed_date, condition, phess_id)
}

random_cut <- function(df, n) {
  n <- suppressWarnings(as.numeric(n))
  if (length(n) == 0 || is.na(n) || n <= 0) return(df[0, , drop = FALSE])
  if (nrow(df) <= n) return(df)
  dplyr::slice_sample(df, n = n)
}

build_extract_subset <- function(case_raw, group_map) {
  map_conditions_to_disease(case_raw) %>%
    left_join(group_map %>% select(disease_group, Disease) %>% distinct(),
              by = "Disease") %>%
    filter(Disease != "Anaphylaxis")
}

generate_selection <- function(extract.subset, records_tbl) {

  disease_group_names <- unique(records_tbl$disease_group)

  ## ---- rows where Disease == disease_group ("whole group" requests) ----
  extract_diseasegrp <- extract.subset %>% group_by(disease_group)

  diseasegrp_vector <- extract_diseasegrp %>%
    group_keys() %>%
    pull(disease_group) %>%
    data.frame() %>%
    setNames("disease_group") %>%
    na.omit()

  record_numbers_diseasegrp <- left_join(
    diseasegrp_vector,
    records_tbl %>% filter(Disease %in% disease_group_names),
    by = "disease_group"
  ) %>%
    filter(!is.na(Records))

  select_diseasegrp_list <- extract_diseasegrp %>%
    filter(disease_group %in% record_numbers_diseasegrp$disease_group) %>%
    group_split() %>%
    map2(.y = record_numbers_diseasegrp$Records, .f = random_cut) %>%
    list_rbind()

  ## ---- rows for a specific disease ----
  extract_disease <- extract.subset %>% group_by(Disease)

  disease_vector <- extract_disease %>%
    group_keys() %>%
    pull(Disease) %>%
    data.frame() %>%
    setNames("Disease")

  record_numbers_disease <- left_join(
    disease_vector,
    records_tbl %>% filter(!Disease %in% disease_group_names),
    by = "Disease"
  ) %>%
    filter(!is.na(Records))

  select_disease_list <- extract_disease %>%
    filter(Disease %in% record_numbers_disease$Disease) %>%
    group_split() %>%
    map2(.y = record_numbers_disease$Records, .f = random_cut) %>%
    list_rbind()

  ## ---- combine + build final table ----
  combined_select_list <- bind_rows(select_diseasegrp_list, select_disease_list) %>%
    distinct(phess_id, .keep_all = TRUE)

  records_tbl %>%
    filter(!Disease %in% disease_group_names) %>%
    left_join(combined_select_list %>% select(-disease_group), by = "Disease") %>%
    filter(!(is.na(Records) & is.na(phess_id))) %>%
    mutate(phess_id = if_else(is.na(phess_id), "No cases recently", phess_id)) %>%
    arrange(disease_group, Disease) %>%
    select(-Records)
}

# ------------------------------------------------------------
# 5. Build one numeric input per entry_table row, grouped under
#    a heading for each disease_group. Input ids are "rec_<row>",
#    where <row> is the row number in entry_table_default.
# ------------------------------------------------------------

build_records_inputs <- function(tbl) {
  tbl <- tbl %>% mutate(.row = row_number())

  tagList(
    lapply(unique(tbl$disease_group), function(grp) {
      grp_rows <- tbl %>% filter(disease_group == grp)

      tagList(
        h4(grp, style = "margin-top: 18px; border-bottom: 1px solid #ddd; padding-bottom: 4px;"),
        lapply(seq_len(nrow(grp_rows)), function(k) {
          row <- grp_rows[k, ]
          is_group_row <- identical(row$Disease, row$disease_group)
          fluidRow(
            style = "margin-bottom: 4px;",
            column(
              7,
              tags$div(
                style = if (is_group_row) "font-weight: 600;" else "padding-left: 14px;",
                if (is_group_row) paste0(row$Disease, " (whole group)") else row$Disease
              )
            ),
            column(
              5,
              numericInput(
                inputId = paste0("rec_", row$.row),
                label = NULL,
                value = row$Records,
                min = 0,
                step = 1,
                width = "100px"
              )
            )
          )
        })
      )
    })
  )
}

# ------------------------------------------------------------
# 6. Shiny UI
# ------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Random Case Selection Tool"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("1. Case data"),
      fileInput("case_file", "Upload case extract (CSV)",
                accept = c(".csv"),
                placeholder = "Using built-in sample data"),
      helpText("Needs the same columns as your PHESS extract: event_id, ",
               "condition_type, condition, event_date, event_type, ",
               "event_classification, lphu, assigned_lphu, ",
               "investigation_status, investigation_completed_date. ",
               "If nothing is uploaded, a small built-in sample is used."),
      hr(),
      h4("2. Run"),
      actionButton("run", "Generate random selection", icon = icon("dice"), class = "btn-primary"),
      br(), br(),
      downloadButton("download", "Download results (CSV)")
    ),
    mainPanel(
      width = 8,
      fluidRow(
        column(
          12,
          h4("How many records?"),
          helpText("Enter a number next to each disease. Bold rows draw that ",
                   "many records from ANY disease within the group. Leave a ",
                   "box empty to skip that row."),
          actionButton("reset_table", "Reset to defaults", icon = icon("rotate-left")),
          div(style = "margin-top: 10px;", uiOutput("entry_inputs"))
        )
      ),
      hr(),
      h4("Selected records"),
      DTOutput("results_table")
    )
  )
)

# ------------------------------------------------------------
# 7. Shiny server
# ------------------------------------------------------------

server <- function(input, output, session) {

  output$entry_inputs <- renderUI({
    build_records_inputs(entry_table_default)
  })

  # reset every numeric input back to its default value
  observeEvent(input$reset_table, {
    for (i in seq_len(nrow(entry_table_default))) {
      updateNumericInput(session, paste0("rec_", i), value = entry_table_default$Records[i])
    }
  })

  # read the current value of every numeric input into a records table
  current_records_tbl <- reactive({
    vals <- vapply(seq_len(nrow(entry_table_default)), function(i) {
      v <- input[[paste0("rec_", i)]]
      if (is.null(v)) NA_real_ else as.numeric(v)
    }, numeric(1))

    entry_table_default %>% mutate(Records = vals)
  })

  # case_raw: uploaded file if present, otherwise the built-in sample
  case_data <- reactive({
    if (!is.null(input$case_file)) {
      read_csv(input$case_file$datapath, col_types = cols(.default = "c"))
    } else {
      case_raw_sample
    }
  })

  result <- eventReactive(input$run, {
    records_tbl <- current_records_tbl()
    extract.subset <- build_extract_subset(case_data(), records_tbl)
    generate_selection(extract.subset, records_tbl)
  })

  output$results_table <- renderDT({
    datatable(result(), rownames = FALSE, options = list(pageLength = 20, dom = "ftip"))
  })

  output$download <- downloadHandler(
    filename = function() paste0("selected_records_", Sys.Date(), ".csv"),
    content = function(file) {
      write_csv(result(), file)
    }
  )
}

shinyApp(ui, server)
