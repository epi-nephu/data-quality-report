# Interchangeable code chunks to configure code files to run from Austin VM or from own device

# changes to be made to R chunks - define files and load phess data

# For Austin VM - for VPD conditions

```{r define files}
nephu_linelist_file <- "NEPHUCaseLinelist.xlsx"
viclinelist_file <- "VictoriaVPDLinelist.xlsx"
population_file <- "Data/LGAPopulationData.xlsx"

wd <- getwd()
main_dir <- str_remove(wd, "CD Surveillance Reporting Rmarkdown/Code")
phess_dir_component <- "PHESS Reporting/PHESS Reports for Power BI/"
phess_data_dir <- paste0(main_dir, phess_dir_component)

```

``` {r load phess data}
viclinelist.raw <- readxl::read_xlsx(paste0(phess_data_dir, viclinelist_file), sheet=1, guess_max=min(50000, Inf)) %>% clean_names()

epirisk.raw <- readxl::read_xlsx(paste0(phess_data_dir, nephu_linelist_file), sheet=1, guess_max=min(100000, Inf)) %>% clean_names() 

```

# For Austin VM - for non-VPD conditions

```{r define files}
nephu_linelist_file <- "NEPHUCaseLinelist.xlsx"
viclinelist_file <- "VictorianonVPDLinelist.xlsx"
population_file <- "Data/LGAPopulationData.xlsx"

wd <- getwd()
main_dir <- str_remove(wd, "CD Surveillance Reporting Rmarkdown/Code")
phess_dir_component <- "PHESS Reporting/PHESS Reports for Power BI/"
phess_data_dir <- paste0(main_dir, phess_dir_component)

```

``` {r load phess data}
viclinelist.raw <- readxl::read_xlsx(paste0(phess_data_dir, viclinelist_file), sheet=1, guess_max=min(50000, Inf)) %>% clean_names()

epirisk.raw <- readxl::read_xlsx(paste0(phess_data_dir, nephu_linelist_file), sheet=1, guess_max=min(100000, Inf)) %>% clean_names() 

```

# For own device - for VPD conditions

```{r define files}
nephu_linelist_file <- "Data/NEPHUCaseLinelist.xlsx"
viclinelist_file <- "Data/VictoriaVPDLinelist.xlsx"
population_file <- "Data/LGAPopulationData.xlsx"
```

``` {r load phess data}
viclinelist.raw <- readxl::read_xlsx(here(viclinelist_file), sheet=1, guess_max=min(50000, Inf)) %>% clean_names()

epirisk.raw <- readxl::read_xlsx(here(nephu_linelist_file), sheet=1, guess_max=min(50000, Inf)) %>% clean_names() %>% remove_empty(which="cols")

```


# For own device - for non-VPD conditions

```{r define files}
nephu_linelist_file <- "Data/NEPHUCaseLinelist.xlsx"
viclinelist_file <- "Data/VictorianonVPDLinelist.xlsx"
population_file <- "Data/LGAPopulationData.xlsx"
```

``` {r load phess data}
viclinelist.raw <- readxl::read_xlsx(here(viclinelist_file), sheet=1, guess_max=min(50000, Inf)) %>% clean_names()

epirisk.raw <- readxl::read_xlsx(here(nephu_linelist_file), sheet=1, guess_max=min(50000, Inf)) %>% clean_names() %>% remove_empty(which="cols")

```

