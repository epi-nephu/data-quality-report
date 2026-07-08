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

entry_table_default <- readxl::read_xlsx(here("Data", "Records Entry Table.xlsx"), sheet=2) %>% 
  rename(disease_group = "Disease Group")

# ------------------------------------------------------------
# 3. case_raw (used until the user uploads their own extract)
# ------------------------------------------------------------

case_raw <- readxl::read_xlsx(here("Data", "Extract_Q2_2026.xlsx")) |> 
  clean_names() |> 
  mutate(investigation_completed_date = if_else(investigation_completed_date=="null", NA_Date_, 
                                                janitor::excel_numeric_to_date(as.numeric(investigation_completed_date))))


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
  titlePanel("Quarterly Case Selection Tool"),
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
      downloadButton("download", "Download results (Excel)")
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
      case_raw
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
    filename = function() paste0("selected_records_", Sys.Date(), ".xlsx"),
    content = function(file) {
      writexl::write_xlsx(result(), file)
    }
  )
}

shinyApp(ui, server)
