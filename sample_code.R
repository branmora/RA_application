# Run script for Road Safety project

# This research project uses data from the National Census of Police Stations,
# international oil prices and local gasoline prices to study the effect of
# fuel prices on road safety in the Cusco Region using a two-way fixed effects reg

# CONTENTS
  # Set folder globals
  # Clean world bank data
  # Clean osinergmin data
  # Construct main dataset
  # Regression Analysis
  # Additional tables and plots

# User path --------------------------------------------------------------------
datawork  <- file.path("")

# Packages ---- ----------------------------------------------------------------
packages   <- 0
if (packages) lapply(c("tidyverse",
                       "haven",
                       "readxl",
                       "janitor",
                       "lubridate",
                       "broom",
                       "estimatr",
                       "sandwich",
                       "lmtest",
                       "fixest",
                       "flextable",
                       "modelsummary"),
                     library,
                     character.only = TRUE)

# Folder globals ---------------------------------------------------------------

data_source1    <- file.path(datawork, "datasets", "data_source1")
data_source2    <- file.path(datawork, "datasets", "data_source2")
data_source3    <- file.path(datawork, "datasets", "data_source3")
data_construct  <- file.path(datawork, "datasets", "data_constructed")
scripts         <- file.path(datawork, "scripts")
results         <- file.path(datawork, "results")


# Clean world bank data --------------------------------------------------------

# Load monhtly prices dataset
wb_data <-read_xlsx(file.path(data_source2, "raw","wb_data.xlsx"), sheet = "Monthly Prices", range = "A5:B744") %>% clean_names()

# Tidy data
# We keep crude oil data from 2010 to 2018
wb_data <- wb_data %>% 
  mutate(date = ym(x1)) %>%
  filter(!is.na(x1))%>% 
  filter(date > as.Date("2010-12-01") & date < as.Date("2018-01-01")) %>%
  select(crude_oil_average, date) %>%
  mutate(crude_oil_average = as.numeric(crude_oil_average))

# Save data
write_csv(wb_data, file.path(data_source2, "clean", "data_wb_clean.csv"))
write_rds(wb_data, file.path(data_source2, "clean", "data_wb_clean.rds"))

# Clean osinergmin data --------------------------------------------------------

# Load dataset
osinergmin_data <-read_xlsx(file.path(data_source3, "raw","osinergmin_data.xlsx"), sheet = "Hoja1", range = "A1:H785") %>% clean_names()

# Generate date variable
osinergmin_data <- osinergmin_data %>%
  mutate(date = make_date(ano,mes)) 

# Tidy data
# We keep local gasoline data (84,90,95,97) from osinergmin data
osinergmin_data <- osinergmin_data %>%
  filter(!is.na(date)) %>% 
  rowwise() %>%
  mutate(osinergmin_average = mean(c(gasolina_97,gasolina_95,gasolina_90,gasolina_84))) %>%
  select(osinergmin_average, date)

# Save data
write_csv(osinergmin_data, file.path(data_source3, "clean", "data_osinergmin_clean.csv"))
write_rds(osinergmin_data, file.path(data_source3, "clean", "data_osinergmin_clean.rds"))

# Construct main dataset -------------------------------------------------------

# Read transito data
data_transito_clean <- read_rds(file.path(data_source1, "clean", "data_transito_clean.rds"))

# Read oil prices data
data_wb_clean <- read_rds(file.path(data_source2, "clean", "data_wb_clean.rds"))

# Read osinergmin data
data_osinergmin_clean <- read_rds(file.path(data_source3, "clean", "data_osinergmin_clean.rds"))

## Merge datasets
data_constructed <- data_transito_clean %>%
  left_join(data_wb_clean, by = "date") %>%
  left_join(data_osinergmin_clean, by = "date")

# Save merged dataset
write_csv(data_constructed, file.path(data_construct, "data_constructed.csv"))
write_rds(data_constructed, file.path(data_construct, "data_constructed.rds"))

# Regression Analysis ----------------------------------------------------------

# Load dataset
data_analysis <- read_rds(file.path(data_construct, "data_constructed.rds"))

## We run the regression for Crude oil prices
models_crude_oil <- list(
  "Accidentes (total)" = feols(accidentes ~ l(crude_oil_average, 1) | comisaria + year, data_analysis, panel.id = c("comisaria","date")), # Fixed effects model (all accidents)
  "Accidentes por velocidad" = feols(AT109_1 ~ l(crude_oil_average, 1) | comisaria + year, data_analysis, panel.id = c("comisaria","date")), # Fixed effects model (exceso de velocidad)
  "Accidentes por cambio de carril" = feols(AT109_6 ~ l(crude_oil_average, 1) | comisaria + year, data_analysis, panel.id = c("comisaria","date")), # Fixed effects model (cambio de carril)
  "Accidentes por imprudencia del peaton" = feols(AT109_9 ~ l(crude_oil_average, 1) | comisaria + year, data_analysis, panel.id = c("comisaria","date")), # Fixed effects model (imprudencia peaton)
  "Accidentes por estado de ebriedad" = feols(AT109_5 ~ l(crude_oil_average, 1) | comisaria + year, data_analysis, panel.id = c("comisaria","date")) # Fixed effects model (estado de ebriedad conductor)
)

# Export regression results

# We change coefficients names for the regression table
names_crude_oil <- c('l(crude_oil_average, 1)'    = 'Precio de petróleo',
                     '(Intercept)' = 'Constante')

# Add stats (number of observations)
statistics_crude_oil <- tribble(~raw,~clean,~fmt,
                                "nobs","Observaciones",0)

# We add fixed effects rows for the regression table
fixed_effects_crude_oil <- tribble(~term, ~"Accidentes (total)",  ~"Accidentes por velocidad", ~"Accidentes por cambio de carril",~"Accidentes por imprudencia del peaton",~"Accidentes por estado de ebriedad",
                                   'Efectos fijos Comisaría', 'Si','Si', 'Si', 'Si','Si',
                                   'Efectos fijos de Año','Si', 'Si', 'Si', 'Si','Si')
attr(fixed_effects_crude_oil, 'position') <- c(3,4)

# We add notes in the regression table
notes_crude_oil <- list('Nota. Errores clusterizados al nivel de Comisaria. Todos los modelos incluyen efectos fijos a nivel comisaría y año.')

# Export model as docx
modelsummary(models_crude_oil,
             stars = TRUE,
             coef_map = names_crude_oil,
             gof_map = statistics_crude_oil,
             add_rows = fixed_effects_crude_oil,
             notes = notes_crude_oil,
             output = file.path(results, "tables", "results_oil.docx")
)


## We run the regression for Osinergmin prices

models_osinergmin <- list(
  "Accidentes (total)" = feols(accidentes ~ l(osinergmin_average, 1) | comisaria + year, data_analysis, panel.id = c("comisaria","date")), # Fixed effects model (all accidents)
  "Accidentes por velocidad" = feols(AT109_1 ~ l(osinergmin_average, 1) | comisaria + year, data_analysis, panel.id = c("comisaria","date")), # Fixed effects model (exceso de velocidad)
  "Accidentes por cambio de carril" = feols(AT109_6 ~ l(osinergmin_average, 1) | comisaria + year, data_analysis, panel.id = c("comisaria","date")), # Fixed effects model (cambio de carril)
  "Accidentes por imprudencia del peaton" = feols(AT109_9 ~ l(osinergmin_average, 1) | comisaria + year, data_analysis, panel.id = c("comisaria","date")), # Fixed effects model (imprudencia peaton)
  "Accidentes por estado de ebriedad" = feols(AT109_5 ~ l(osinergmin_average, 1) | comisaria + year, data_analysis, panel.id = c("comisaria","date")) # Fixed effects model (estado de ebriedad conductor)
)

# Export regression results

# We change coefficients names for the regression table
names_osinergmin <- c('l(osinergmin_average, 1)'    = 'Precio de gasolina',
                      '(Intercept)' = 'Constante')

# Add stats (number of observations)
statistics_osinergmin <- tribble(~raw,~clean,~fmt,
                                 "nobs","Observaciones",0)

# We add fixed effects rows for the regression table
fixed_effects_osinergmin <- tribble(~term, ~"Accidentes (total)",  ~"Accidentes por velocidad", ~"Accidentes por cambio de carril",~"Accidentes por imprudencia del peaton",~"Accidentes por estado de ebriedad",
                                    'Efectos fijos Comisaría', 'Si','Si', 'Si', 'Si','Si',
                                    'Efectos fijos de Año','Si', 'Si', 'Si', 'Si','Si')
attr(fixed_effects_osinergmin, 'position') <- c(3,4)

# We add notes in the regression table
notes_osinergmin <- list('Nota. Errores clusterizados al nivel de Comisaria. Todos los modelos incluyen efectos fijos a nivel comisaría y año.')

# We save the model as docx
modelsummary(models_osinergmin,
             stars = TRUE,
             coef_map = names_osinergmin,
             gof_map = statistics_osinergmin,
             add_rows = fixed_effects_osinergmin,
             notes = notes_osinergmin,
             output = file.path(results, "tables", "results_osinergmin.docx")
)

# Additional tables and plots --------------------------------------------------

## Descriptive stats table
data_analysis %>%
  select(accidentes,AT109_1,AT109_9,AT109_6,AT109_5,crude_oil_average,osinergmin_average) %>%
  datasummary_skim(output = file.path(results, "tables", "descriptive_stats.docx"))

## Coef plots for our models
modelplot(models_crude_oil) 
modelplot(models_osinergmin) 
