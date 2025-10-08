#map of victoria - nephu LGAs 
# Packages ---------------------------------------------------------------------
library(pacman)

pacman::p_load(tidyverse,
               here,
               readxl,
               janitor,
               lubridate,
               scales,
               knitr,
               kableExtra,
               patchwork,
               apyramid,
               sf,
               ggthemes)

# LGA names --------------------------------------------------------------------
lga_name_long <- c("Banyule (C)", "Boroondara (C)", "Darebin (C)", 
                   "Hume (C)", "Knox (C)", "Manningham (C)", 
                   "Maroondah (C)", "Nillumbik (S)", "Whitehorse (C)", 
                   "Whittlesea (C)", "Yarra (C)", "Yarra Ranges (S)")

lga_name_short <- c("Banyule", "Boroondara", "Darebin", 
                    "Hume", "Knox", "Manningham", 
                    "Maroondah", "Nillumbik", "Whitehorse", 
                    "Whittlesea", "Yarra", "Yarra Ranges")

lga_nudge <- c("Boroondara", "Darebin", "Yarra", "Maroondah")

# Load and wrangle case data ---------------------------------------------------
# Read in the NEPHU PHESS case linelist (current year)
cases_allnephu <- readxl::read_xlsx(paste0(here::here(), "/Data", "/LGA_COVID_CASES_2020_2021", "2020", ".xlsx"),
                                    sheet     = 1,
                                    guess_max = min(100000, Inf)) %>% 
  #
  janitor::clean_names()

# Load and wrangle population data ---------------------------------------------
# Read in population data
reference_population <- readxl::read_xlsx(paste0(here::here(), "/Data", "/Reference", "/LGAAllocationLong.xlsx"),
                                          sheet     = 1,
                                          guess_max = min(100000, Inf)) %>% 
  janitor::clean_names()
install.packages("readxl")
reference_population <-readxl(LGAAllocationLong)
# Calculate NEPHU and statewide totals
population_nephu <- reference_population %>% 
  dplyr::filter(lphu == "NEPHU") %>%
  #
  dplyr::summarise(x2021_population = sum(x2021_population)) %>% 
  #
  dplyr::mutate(lga = "Total NEPHU")

population_vic <- reference_population %>% 
  dplyr::summarise(x2021_population = sum(x2021_population)) %>% 
  #
  dplyr::mutate(lga = "Total Victoria")

reference_population <- reference_population %>% 
  dplyr::filter(lphu == "NEPHU") %>% 
  #
  dplyr::select(-lphu) %>% 
  #
  dplyr::bind_rows(population_nephu) %>% 
  dplyr::bind_rows(population_vic)

# Load and wrangle shape file for maps -----------------------------------------
nephu_lga.sf <- sf::st_read(here::here("Data/LGA_2022_AUST_GDA2020_SHP/LGA_2022_AUST_GDA2020.shp"),
                            quiet = TRUE) %>% 
  dplyr::select(-STE_CODE21, 
                -AUS_CODE21, 
                -AUS_NAME21, 
                -LOCI_URI21) %>% 
  #
  dplyr::rename(lga_code   = LGA_CODE22,
                lga_name   = LGA_NAME22,
                ste_name   = STE_NAME21,
                areasqkm   = AREASQKM,
                shp_length = SHAPE_Leng, 
                shp_area   = SHAPE_Area) %>% 
  #
  dplyr::filter(lga_name %in% lga_name_short) %>% 
  #
  dplyr::arrange(lga_name) %>% 
  #
  dplyr::mutate(lga = c("Banyule (C)", "Boroondara (C)", "Darebin (C)", 
                        "Hume (C)", "Knox (C)", "Manningham (C)", 
                        "Maroondah (C)", "Nillumbik (S)", "Whitehorse (C)", 
                        "Whittlesea (C)", "Yarra (C)", "Yarra Ranges (S)"))

# Calculate notification rate by LGA -------------------------------------------
cases_rate <- cases_allnephu %>% 
  dplyr::filter(condition == "Cryptosporidiosis") %>%
  #
  dplyr::group_by(local_government_area) %>% 
  dplyr::summarise(n_cases = n()) %>% 
  dplyr::ungroup() %>% 
  #
  dplyr::left_join(reference_population, by = c("local_government_area" = "lga")) %>% 
  #
  dplyr::mutate(rate = (n_cases / (x2021_population / 12)) * 100000)

# Draw awesome map -------------------------------------------------------------
nephu_lga.sf %>% 
  dplyr::left_join(cases_rate, by = c("lga" = "local_government_area")) %>% 
  #
  ggplot() +
  #
  geom_sf(aes(fill = rate), color = "black") +
  #
  scale_fill_gradient_tableau(palette = "Blue", name = "Rate (per 100,000 person-years)") +
  #
  geom_sf_text(data = nephu_lga.sf %>% filter(!(lga_name %in% lga_nudge)), 
               aes(label = lga_name), 
               size = 3) + 
  #
  geom_sf_text(data = nephu_lga.sf %>% filter(lga_name == "Darebin"),
               aes(label = lga_name), 
               size    = 3, 
               nudge_x = -0.07, 
               nudge_y = -0.01) +
  #
  geom_sf_text(data = nephu_lga.sf %>% filter(lga_name == "Yarra"), 
               aes(label = lga_name), 
               size    = 3, 
               nudge_x = -0.03, 
               nudge_y = -0.01) +
  #
  geom_sf_text(data = nephu_lga.sf %>% filter(lga_name == "Boroondara"), 
               aes(label = lga_name), 
               size    = 3, 
               nudge_x = -0.07, 
               nudge_y = -0.025) +
  #
  geom_sf_text(data = nephu_lga.sf %>% filter(lga_name == "Maroondah"), 
               aes(label = lga_name), 
               size    = 3, 
               nudge_x = 0.03, 
               nudge_y = -0.01) + 
  #
  labs(title = paste0("Cryptosporidiosis notification rate by LGA, NEPHU, ",
                      "January 2024")) +
  #
  theme_void() +
  theme(plot.title.position = "plot",
        plot.title          = element_text(size  = 10,
                                           face  = "bold",
                                           hjust = 0.5),
        #
        legend.position = "bottom",
        legend.title    = element_text(size = 9)) + 
  #
  guides(fill = guide_colorbar(barwidth = 10))