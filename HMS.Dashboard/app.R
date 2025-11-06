# A Shiny app for investigating U.S. Seafood trade, landings, and processing
  # data through time for Highly Migratory Species
# Contact: Cameron Van Horn
#          cameron.vanhorn@noaa.gov

# A note on the general data formatting:
  # all 'value' data are calculated in Real 2024 U.S. Dollars (USD) to account
  # for inflation by default
# Packages Sources, and Data ---------------------------------------------------
if(!require("googledrive")) install.packages("googledrive")
if(!require("shiny"))       install.packages("shiny")
if(!require("bslib"))       install.packages("bslib")
if(!require("tidyverse"))   install.packages("tidyverse")
if(!require("scales"))      install.packages("scales")
if(!require("ggh4x"))       install.packages("ggh4x")
if(!require("shinycssloaders")) install.packages("shinycssloaders")
if(!require("bsicons"))     install.packages("bsicons")
# Due to some limitations in downloading nmfspalette on devices, use source
  # file located in app directory for nmfspalette colors
source("nmfs_cols.R")

# Add Resource Path for Quarto documents
addResourcePath("tmpuser", getwd())

# Load most recent data file (manually taken from Seafood Dashboard)
load('hms_data_munge_10_28_25.RData')


# filter out confidential data (no data contained therein)
landings <- landings %>%
  filter(CONFIDENTIALITY == 'Public')

# create matrix of all categorization terms available in the data
categorization_matrix <- bind_rows(trade_data, 
                                   landings %>%
                                     filter(CONFIDENTIALITY != 'Confidential'), 
                                   pp_data %>%
                                     filter(CONFIDENTIAL == 0)) %>%
  select(SPECIES_NAME, SPECIES_GROUP, 
         SPECIES_CATEGORY, COAST) %>%
  group_by(SPECIES_NAME, SPECIES_GROUP, 
           SPECIES_CATEGORY, COAST) %>%
  distinct() %>%
  ungroup()

# create matrix of all trade categorization terms available in the data
trade_categorization_matrix <- trade_data %>%
  select(SPECIES_NAME, SPECIES_GROUP, 
         SPECIES_CATEGORY) %>%
  group_by(SPECIES_NAME, SPECIES_GROUP, 
           SPECIES_CATEGORY) %>%
  distinct() %>%
  ungroup()

# create matrix of all landings categorization terms available in the data
landings_categorization_matrix <- landings %>%
  select(SPECIES_NAME, SPECIES_GROUP,
         SPECIES_CATEGORY) %>%
  group_by(SPECIES_NAME, SPECIES_GROUP,
           SPECIES_CATEGORY) %>%
  distinct() %>%
  ungroup()

# create matrix of all products categorization terms available in the data
products_categorization_matrix <- pp_data %>%
  select(SPECIES_NAME, SPECIES_GROUP,
         SPECIES_CATEGORY) %>%
  group_by(SPECIES_NAME, SPECIES_GROUP,
           SPECIES_CATEGORY) %>%
  distinct() %>%
  ungroup()

# Create list of terms for each level of organization hierarchy
# these lists will be used to determine where a provided species input is 
# found in the hierarchy
scat_list <- unique(categorization_matrix %>%
                      select(SPECIES_CATEGORY) %>%
                      distinct() %>%
                      filter(!is.na(SPECIES_CATEGORY)) %>%
                      mutate(SPECIES_CATEGORY = 
                               str_to_title(SPECIES_CATEGORY)) %>%
                      pull())

sgrp_list <- unique(categorization_matrix %>%
                      select(SPECIES_GROUP) %>%
                      distinct() %>%
                      filter(!is.na(SPECIES_GROUP)) %>%
                      mutate(SPECIES_GROUP = str_to_title(SPECIES_GROUP)) %>%
                      pull())

sname_list <- unique(categorization_matrix %>%
                       select(SPECIES_NAME) %>%
                       distinct() %>%
                       filter(!is.na(SPECIES_NAME)) %>%
                       mutate(SPECIES_NAME = str_to_title(SPECIES_NAME)) %>%
                       pull())

tooltip_aes <- paste0(
  "position: absolute; ",
  "background-color: rgba(255, 255, 255, 0.95); ",
  "border: 1px solid #ccc; ",
  "border-radius: 10px; ",
  "padding: 10px; ",
  "padding-right: 25px; ",
  "box-shadow: 0 2px 4px rgba(0, 0, 0, 0.2); ",
  "z-index: 1000; ",
  "max-width: 250px; ",
  "min-width: 250px; "
)

close_button_aes <- paste0(
  "position: absolute; ",
  "top: 2px; ",
  "right: 5px; ",
  "background: transparent; ",
  "border: none; ",
  "color: #666; ",
  "font-size: 18px; ",
  "font-weight: bold; ",
  "cursor: pointer; ",
  "padding: 0; ",
  "width: 20px; ",
  "height: 20px; ",
  "line-height: 18px; ",
  "text-align: center; ")

tooltip_heading <- paste0(
  "</span><span style = 'font-size: 22px; font-weight: bold; text-decoration: underline;'>")

tooltip_subheading <- paste0(
  "</span><span style = 'font-size: 18px; font-style: italic; text-decoration: underline;'>")

###

# Custom Functions -------------------------------------------------------------
# stop functions without outputting error message
stop_quietly <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}
### filter species
filter_species <- function(data, species) {
  # data is a formatted data frame created from 2_data_munge.R (see GitHub)
  # species is a character vector of a species of interest 
  # (e.g., 'Albacore Tuna')
  
  # if All Species is entered, no filtering occurs, original data returned
  if(species == 'All Species') {
    return(data)
  }
  
  # species are organized in a hierarhcy of three levels:
  # species category (e.g., 'Tunas')
  # species group (e.g., 'Hawaii Tuna')
  # species name (e.g., 'Yellowfin Tuna')
  
  # store unique values in each species hierarchy level
  species_categories <- unique(data$SPECIES_CATEGORY)
  species_groups <- unique(data$SPECIES_GROUP)
  species_names <- unique(data$SPECIES_NAME)
  
  # coerce species input to upper case to align with data frame formatting
  species <- toupper(species)
  
  # ifelse loop to find which hierarchy level the input species is stored
  locate_level <- 
    # first search highest level 'Species Category'
    ifelse(species %in% species_categories, 
           'SPECIES_CATEGORY',
           ifelse(species %in% species_groups, 
                  'SPECIES_GROUP',
                  ifelse(species %in% species_names, 
                         'SPECIES_NAME',
                         # if the species is not found in the data, 
                         # return 'UNAVAILABLE'
                         'UNAVAILABLE')))
  
  # if species was not found, stop function with message to try a different
  # species input or search for available entries
  if (locate_level == 'UNAVAILABLE') {
    return(data[-c(1:nrow(data)), ])
  } 
  
  # only runs if species is found
  # store the hierarchy level as symbol, then as object of type quosure
  # (see RLang package for more information on quosures)
  # this enables the object to be called in a dplyr pipe via bang-bang (!!)
  level <- as.symbol(locate_level)
  level <- rlang::enquo(level)
  
  # filter the input data frame for the species of interest using the hierarchy
  # level column in which the entry was found
  new_data <- data %>%
    filter(!!level == species)
  
  return(new_data)
  
  # a note on the hierarchy level conventions:
  # including multiple levels of species classification to each product 
  # enables more data to be used in the event that a product does not contain
  # a specific species on the label (e.g., 'tuna'). Also, it enables us to 
  # investigate the data at different resolutions (e.g., all tunas compared
  # to just Yellowfin Tuna)
}
filter_coast <- function(data, coast) {
  # This filter is used in all summary functions to filter for selected coasts
  # Data is any data frame with a field specifying the data's coast of origin
  # coast is a character vector meant to match how coast is specified in data
  if (coast == '' | is.null(coast) | coast == 'ALL') {
    return(data)
  }
  
  new_data <- data %>%
    filter(COAST == coast)
  return(new_data)
}

### summary + calculation functions
summarize_trade_yr_spp <- function(trade_table, species, coast, output.format, 
                                   units = NULL, nominal = F) {
  # this function summarizes trade data by year and species of interest
  # trade_table is a formatted data frame of FOSS trade data (see 2_data_munge.R)
  # species is a character vector of a species of interest
  
  # coerce species to upper case
  # IF NOT COERCED TO UPPER CASE: app would not display data as species input
  # is sourced from a selected user input of pre-determined values, which are 
  # provided in lower case (as title; e.g., 'Tuna' instead of 'tuna')
  species <- toupper(species)
  
  # if a species is selected, find the level of the categorization hierarchy in
  # which the species input resides
  # see filter_species function for info on why we store as symbol and quosure
  if (species != 'ALL SPECIES') {
    which_level <- as.symbol(
      ifelse(species %in% unique(trade_table$SPECIES_CATEGORY), 
             'SPECIES_CATEGORY',
             ifelse(species %in% unique(trade_table$SPECIES_GROUP), 
                    'SPECIES_GROUP',
                    'SPECIES_NAME')))
    # store level as object of quosure to work in dplyr pipe (via !!)
    level <- rlang::enquo(which_level)
  } else {
    level <- NULL
    species <- str_to_title(species)
  }
  
  if (coast == 'ALL') {
    field <- as.symbol('COAST')
    field <- rlang::enquo(field)
  } else {field <- NULL}
  
  summarized_data <- trade_table %>%
    filter_species(species) %>%
    filter_coast(coast) %>%
    select(YEAR, !!level, !!field, EXP_VALUE_2024USD, EXP_VOLUME_KG, EXP_CONVERTED_VOLUME,
           IMP_VALUE_2024USD, IMP_VOLUME_KG, IMP_CONVERTED_VOLUME, EXP_VALUE_USD, 
           IMP_VALUE_USD) %>%
    mutate(EXP_VALUE_2024USD = ifelse(is.na(EXP_VALUE_2024USD), 0,
                                      EXP_VALUE_2024USD),
           IMP_VALUE_2024USD = ifelse(is.na(IMP_VALUE_2024USD), 0,
                                      IMP_VALUE_2024USD),
           EXP_VOLUME_KG = ifelse(is.na(EXP_VOLUME_KG), 0,
                                  EXP_VOLUME_KG),
           IMP_VOLUME_KG = ifelse(is.na(IMP_VOLUME_KG), 0,
                                  IMP_VOLUME_KG),
           EXP_CONVERTED_VOLUME = ifelse(is.na(EXP_CONVERTED_VOLUME), 0,
                                         EXP_CONVERTED_VOLUME),
           IMP_CONVERTED_VOLUME = ifelse(is.na(IMP_CONVERTED_VOLUME), 0,
                                         IMP_CONVERTED_VOLUME),
           EXP_VALUE_USD = ifelse(is.na(EXP_VALUE_USD), 0,
                                  EXP_VALUE_USD),
           IMP_VALUE_USD = ifelse(is.na(IMP_VALUE_USD), 0,
                                  IMP_VALUE_USD)) %>%
    group_by(YEAR, !!level, !!field) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop')
  
  if (output.format == 'FULL') {
    new_data <- summarized_data %>%
      mutate(EXP_VOLUME_LB = EXP_VOLUME_KG * 2.20462,
             IMP_VOLUME_LB = IMP_VOLUME_KG * 2.20462,
             EXP_ROUND_VOLUME_LB = EXP_CONVERTED_VOLUME * 2.20462,
             IMP_ROUND_VOLUME_LB = IMP_CONVERTED_VOLUME * 2.20462,
             EXP_PRICE_USD_PER_KG = EXP_VALUE_2024USD / EXP_VOLUME_KG,
             IMP_PRICE_USD_PER_KG = IMP_VALUE_2024USD / IMP_VOLUME_KG,
             EXP_PRICE_USD_PER_LB = EXP_VALUE_2024USD / EXP_VOLUME_LB,
             IMP_PRICE_USD_PER_LB = IMP_VALUE_2024USD / IMP_VOLUME_LB,
             EXP_PRICE_NOMINAL_PER_KG = EXP_VALUE_USD / EXP_VOLUME_KG,
             EXP_PRICE_NOMINAL_PER_LB = EXP_VALUE_USD / EXP_VOLUME_LB,
             IMP_PRICE_NOMINAL_PER_KG = IMP_VALUE_USD / IMP_VOLUME_KG,
             IMP_PRICE_NOMINAL_PER_LB = IMP_VALUE_USD / IMP_VOLUME_LB,
             EXP_VALUE_2024USD_MILLIONS = EXP_VALUE_2024USD / 1000000,
             IMP_VALUE_2024USD_MILLIONS = IMP_VALUE_2024USD / 1000000,
             EXP_VALUE_2024USD_BILLIONS = EXP_VALUE_2024USD / 1000000000,
             IMP_VALUE_2024USD_BILLIONS = IMP_VALUE_2024USD / 1000000000,
             EXP_VALUE_MILLIONS = EXP_VALUE_USD / 1000000,
             IMP_VALUE_MILLIONS = IMP_VALUE_USD / 1000000,
             EXP_VALUE_BILLIONS = EXP_VALUE_USD / 1000000000,
             IMP_VALUE_BILLIONS = IMP_VALUE_USD / 1000000000,
             EXP_VOLUME_MT = EXP_VOLUME_KG / 1000,
             IMP_VOLUME_MT = IMP_VOLUME_KG / 1000,
             EXP_ROUND_VOLUME_MT = EXP_CONVERTED_VOLUME / 1000,
             IMP_ROUND_VOLUME_MT = IMP_CONVERTED_VOLUME / 1000,
             EXP_VOLUME_ST = EXP_VOLUME_LB / 2000,
             IMP_VOLUME_ST = IMP_VOLUME_LB / 2000,
             EXP_ROUND_VOLUME_ST = EXP_ROUND_VOLUME_LB / 2000,
             IMP_ROUND_VOLUME_ST = IMP_ROUND_VOLUME_LB / 2000)
    return(new_data)
  }
  
  if (units == 'METRIC') {
    new_data <- summarized_data %>%
      rename(EXP_VOLUME = EXP_VOLUME_KG,
             IMP_VOLUME = IMP_VOLUME_KG,
             EXP_ROUND_VOLUME = EXP_CONVERTED_VOLUME,
             IMP_ROUND_VOLUME = IMP_CONVERTED_VOLUME) %>%
      mutate(EXP_VOLUME_T = EXP_VOLUME / 1000,
             IMP_VOLUME_T = IMP_VOLUME / 1000,
             EXP_ROUND_VOLUME_T = EXP_ROUND_VOLUME / 1000,
             IMP_ROUND_VOLUME_T = IMP_ROUND_VOLUME / 1000)
  } else if (units == 'IMPERIAL') {
    new_data <- summarized_data %>%
      mutate(EXP_VOLUME = EXP_VOLUME_KG * 2.20462, # convert kg to lbs
             IMP_VOLUME = IMP_VOLUME_KG * 2.20462,
             EXP_ROUND_VOLUME = EXP_CONVERTED_VOLUME * 2.20462,
             IMP_ROUND_VOLUME = IMP_CONVERTED_VOLUME * 2.20462,
             EXP_VOLUME_T = EXP_VOLUME / 2000, # calculate short tons
             IMP_VOLUME_T = IMP_VOLUME / 2000,
             EXP_ROUND_VOLUME_T = EXP_ROUND_VOLUME / 2000,
             IMP_ROUND_VOLUME_T = IMP_ROUND_VOLUME / 2000) %>%
      select(!c(EXP_VOLUME_KG, IMP_VOLUME_KG))
  }
  
  if (nominal == F) {
    new_data <- new_data %>%
      select(!c(EXP_VALUE_USD, IMP_VALUE_USD)) %>%
      rename(EXP_VALUE = EXP_VALUE_2024USD,
             IMP_VALUE = IMP_VALUE_2024USD)
    
  } else if (nominal == T) {
    new_data <- new_data %>%
      select(!c(EXP_VALUE_2024USD, IMP_VALUE_2024USD)) %>%
      rename(EXP_VALUE = EXP_VALUE_USD,
             IMP_VALUE = IMP_VALUE_USD)
  }
  
  new_data <- new_data %>%
    mutate(EXP_VALUE_MILLIONS = EXP_VALUE / 1000000,
           EXP_VALUE_BILLIONS = EXP_VALUE / 1000000000,
           IMP_VALUE_MILLIONS = IMP_VALUE / 1000000,
           IMP_VALUE_BILLIONS = IMP_VALUE / 1000000000,
           EXP_PRICE = EXP_VALUE / EXP_VOLUME,
           IMP_PRICE = IMP_VALUE / IMP_VOLUME) 
  
  if (output.format == 'BALANCE') {
    balance_data <- new_data %>%
      rename(EXPORTS = EXP_VALUE_MILLIONS,
             IMPORTS = IMP_VALUE_MILLIONS) %>%
      select(YEAR, EXPORTS, IMPORTS) %>%
      mutate(TRADE_BALANCE = EXPORTS - IMPORTS) %>%
      pivot_longer(cols = c(EXPORTS, IMPORTS, TRADE_BALANCE)) %>%
      mutate(name = ifelse(name == 'TRADE_BALANCE', 'TRADE BALANCE', name),
             name = as.factor(str_to_title(name))) %>%
      rename(VALUE_MILLIONS = value,
             TRADE = name)
    
    return(balance_data)
  } else if (output.format %in% c('VALUE', 'VOLUME')) {
    trade_data <- new_data %>%
      select(YEAR, EXP_VALUE, IMP_VALUE, EXP_VALUE_MILLIONS, IMP_VALUE_MILLIONS, 
             EXP_PRICE, IMP_PRICE, EXP_VOLUME_T, IMP_VOLUME_T, EXP_VOLUME,
             IMP_VOLUME, EXP_ROUND_VOLUME, IMP_ROUND_VOLUME, EXP_ROUND_VOLUME_T,
             IMP_ROUND_VOLUME_T, !!field) %>%
      mutate(RATIO = EXP_VOLUME_T / IMP_VOLUME_T)
    
    return(trade_data)
  }
}
summarize_trade_ctry_yr_spp <- function(trade_table, species, coast, output.format,
                                        time.frame, nominal = F) {
  # this function summarizes trade data by year and species of interest
  # within the top 5 trading partners of the U.S. for that species during
  # the specified period of time
  # trade_table is a formatted data frame of FOSS trade data (see 2_data_munge.R)
  # species is a character vector of a species of interest
  # time.frame is a vector of two years that bookend a desired time period
  # value is logical that specifies if the function should output summaries by 
  # trade value, set to FALSE by default
  # volume is logical that specifies if the function should output summaries by
  # trade volume, set to FALSE by default
  
  # Function only proceeds if either value OR volume are T
  # store which column ('field') to summarize by as object of type symbol, then 
  # as type quosure to function within dplyr pipe 
  # (see RLang package for more details)
  if (nominal == T) {
    field <- as.symbol('TOTAL_NOMINAL_TRADE_VALUE')
    field <- rlang::enquo(field)
  } else {
    field <- as.symbol('TOTAL_REAL_TRADE_VALUE')
    field <- rlang::enquo(field)
  }
  
  # coerce species to upper case to match data formatting
  species <- ifelse(species == 'All Species', 'All Species', toupper(species))
  
  # dplyr pipe to summarize exports and imports by year and country
  summarized_data <- trade_table %>%
    filter_species(species) %>%
    filter_coast(coast) %>%
    # select only columns of interest: year, country, exports and imports
    select(YEAR, COUNTRY_NAME, EXP_VALUE_2024USD, EXP_VOLUME_KG,
           IMP_VALUE_2024USD, IMP_VOLUME_KG, EXP_VALUE_USD, IMP_VALUE_USD) %>%
    # filter data to be within the specified time frame
    filter(YEAR >= time.frame[1],
           YEAR <= time.frame[2]) %>%
    # set NAs to 0 so sums and averages can be calculated without outputting NA
    mutate(EXP_VALUE_2024USD = ifelse(is.na(EXP_VALUE_2024USD), 0,
                                      EXP_VALUE_2024USD),
           IMP_VALUE_2024USD = ifelse(is.na(IMP_VALUE_2024USD), 0,
                                      IMP_VALUE_2024USD),
           EXP_VOLUME_KG = ifelse(is.na(EXP_VOLUME_KG), 0,
                                  EXP_VOLUME_KG),
           IMP_VOLUME_KG = ifelse(is.na(IMP_VOLUME_KG), 0,
                                  IMP_VOLUME_KG),
           EXP_VALUE_USD = ifelse(is.na(EXP_VALUE_USD), 0,
                                  EXP_VALUE_USD),
           IMP_VALUE_USD = ifelse(is.na(IMP_VALUE_USD), 0,
                                  IMP_VALUE_USD)) %>%
    # group_by year and country
    group_by(YEAR, COUNTRY_NAME) %>%
    # sum all numeric columns, drop groups
    summarise(across(where(is.numeric), sum),
              .groups = 'drop')
  
  # dplyr pipe to find top 5 trading nations during time frame for input species
  top5 <- summarized_data %>%
    # remove YEAR so it does not get summed
    select(!YEAR) %>%
    # group by country
    group_by(COUNTRY_NAME) %>%
    # sum all numeric columns (i.e., export and import value and volume)
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    # calculate total real trade value by summing export and import values
    # calculate total real trade volume by summing export and import volumes 
    mutate(TOTAL_REAL_TRADE_VALUE = EXP_VALUE_2024USD + IMP_VALUE_2024USD,
           TOTAL_NOMINAL_TRADE_VALUE = EXP_VALUE_USD + IMP_VALUE_USD,
           TOTAL_TRADE_VOLUME = EXP_VOLUME_KG + IMP_VOLUME_KG) %>%
    # filter for the top 5 countries based on the field specified from 
    # the logical value and volume function inputs
    top_n(5, !!field) %>%
    # pull() outputs the values in the specified field as a vector
    pull(COUNTRY_NAME)
  
  if (output.format == 'FULL') {
    # summarize trade data by top five countries during time period
    # summarized_data is already filtered for the time period
    final_data <- summarized_data %>%
      # filter for the top 5 countries
      filter(COUNTRY_NAME %in% top5) %>%
      # calculate export and import values in millions/billions,
      # calculate export and import volumes in metric tons,
      # calculate net value and net volume by subtracting imports from exports
      mutate(EXP_VALUE_2024USD_BILLIONS = EXP_VALUE_2024USD / 1000000000,
             IMP_VALUE_2024USD_BILLIONS = IMP_VALUE_2024USD / 1000000000,
             EXP_VALUE_BILLIONS = EXP_VALUE_USD / 1000000000,
             IMP_VALUE_BILLIONS = IMP_VALUE_USD / 1000000000,
             NET_VALUE_2024USD_BILLIONS = 
               EXP_VALUE_2024USD_BILLIONS - IMP_VALUE_2024USD_BILLIONS,
             NET_VALUE_NOMINAL_BILLIONS = 
               EXP_VALUE_BILLIONS - IMP_VALUE_BILLIONS,
             EXP_VALUE_2024USD_MILLIONS = EXP_VALUE_2024USD / 1000000,
             IMP_VALUE_2024USD_MILLIONS = IMP_VALUE_2024USD / 1000000,
             EXP_VALUE_MILLIONS = EXP_VALUE_USD / 1000000,
             IMP_VALUE_MILLIONS = IMP_VALUE_USD / 1000000,
             NET_VALUE_2024USD_MILLIONS =
               EXP_VALUE_2024USD_MILLIONS - IMP_VALUE_2024USD_MILLIONS,
             NET_VALUE_NOMINAL_MILLIONS = 
               EXP_VALUE_MILLIONS - IMP_VALUE_MILLIONS,
             EXP_VOLUME_LB = EXP_VOLUME_KG * 2.20462,
             IMP_VOLUME_LB = IMP_VOLUME_KG * 2.20462,
             EXP_VOLUME_ST = EXP_VOLUME_LB / 2000,
             IMP_VOLUME_ST = IMP_VOLUME_LB / 2000,
             EXP_VOLUME_MT = EXP_VOLUME_KG / 1000,
             IMP_VOLUME_MT = IMP_VOLUME_KG / 1000,
             NET_VOLUME_MT = EXP_VOLUME_MT - IMP_VOLUME_MT,
             NET_VOLUME_ST = EXP_VOLUME_ST - IMP_VOLUME_ST,
             NET_PRICE_2024USD_PER_KG = 
               (EXP_VALUE_2024USD - IMP_VALUE_2024USD) / 
               (EXP_VOLUME_KG - IMP_VOLUME_KG))
    
    return(final_data)
  }
  
  if (output.format == 'VALUE') {
    final_data <- summarized_data %>%
      filter(COUNTRY_NAME %in% top5)
    
    if (nominal == F) {
      final_data <- final_data %>%
        rename(EXP_VALUE = EXP_VALUE_2024USD,
               IMP_VALUE = IMP_VALUE_2024USD)
    } else if (nominal == T) {
      final_data <- final_data %>%
        rename(EXP_VALUE = EXP_VALUE_USD,
               IMP_VALUE = IMP_VALUE_USD)
    }
    
    final_data <- final_data %>%
      select(YEAR, COUNTRY_NAME, EXP_VALUE, IMP_VALUE) %>%
      mutate(EXP_VALUE_MILLIONS = EXP_VALUE / 1000000,
             IMP_VALUE_MILLIONS = IMP_VALUE / 1000000,
             NET_VALUE_MILLIONS = EXP_VALUE_MILLIONS - IMP_VALUE_MILLIONS)
    
    return(final_data)
  }
  
}
summarize_pp_yr_spp <- function(product_data, species, coast, full_data = F, 
                                units = NULL, nominal = F) {
  # this function summarizes processed product data by year and species of 
  # interest
  # product_data is a formatted data frame of FOSS processed product data
  # (see 2_data_munge.R)
  # species is a character vector of a species of interest
  
  # coerce species to upper case to match data formatting
  species <- ifelse(species == 'All Species', 'All Species', toupper(species))
  
  if (coast == 'ALL') {
    field <- as.symbol('COAST')
    field <- rlang::enquo(field)
  } else {field <- NULL}
  
  summarized_data <- product_data %>%
    filter_species(species) %>%
    filter_coast(coast) %>%
    select(YEAR, PRODUCT_FORM, !!field, KG, DOLLARS_2024, DOLLARS, POUNDS) %>%
    mutate(DOLLARS = ifelse(is.na(DOLLARS), 0, DOLLARS),
           DOLLARS_2024 = ifelse(is.na(DOLLARS_2024), 0, DOLLARS_2024),
           KG = ifelse(is.na(KG), 0, KG),
           POUNDS = ifelse(is.na(POUNDS), 0, POUNDS)) %>%
    group_by(YEAR, !!field, PRODUCT_FORM) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop')
  
  if (full_data == 'FULL') {
    summarized_data <- summarized_data %>%
      mutate(MT = KG / 1000,
             ST = POUNDS / 2000,
             MILLIONS_2024USD = DOLLARS_2024 / 1000000,
             BILLIONS_2024USD = DOLLARS_2024 / 1000000000,
             PP_PRICE_2024USD_PER_KG = DOLLARS_2024 / KG,
             PP_PRICE_2024USD_PER_LB = DOLLARS_2024 / POUNDS,
             MILLIONS = DOLLARS / 1000000,
             BILLIONS = DOLLARS / 1000000000,
             PP_PRICE_NOMINAL_PER_KG = DOLLARS / KG,
             PP_PRICE_NOMINAL_PER_LB = DOLLARS / POUNDS) %>%
      rename(PP_VALUE_2024USD = DOLLARS_2024,
             PP_VOLUME_MT = MT,
             PP_VOLUME_LB = POUNDS,
             PP_VOLUME_ST = ST,
             PP_VALUE_MILLIONS_2024USD = MILLIONS_2024USD,
             PP_VALUE_BILLIONS_2024USD = BILLIONS_2024USD,
             PP_VOLUME_KG = KG,
             PP_NOMINAL_VALUE = DOLLARS,
             PP_VALUE_MILLIONS = MILLIONS,
             PP_VALUE_BILLIONS = BILLIONS)
    
    return(summarized_data)
  }
  
  if (nominal == F) {
    summarized_data <- summarized_data %>%
      select(!c(DOLLARS)) %>%
      rename(PP_VALUE = DOLLARS_2024)
  } else {
    summarized_data <- summarized_data %>%
      select(!c(DOLLARS_2024)) %>%
      rename(PP_VALUE = DOLLARS)
  }
  
  if (units == 'METRIC') {
    summarized_data <- summarized_data %>%
      rename(PP_VOLUME = KG) %>%
      mutate(PP_VOLUME_T = PP_VOLUME / 1000)
  } else if (units == 'IMPERIAL') {
    summarized_data <- summarized_data %>%
      mutate(PP_VOLUME = KG * 2.20462, # convert kilgorams to pounds
             PP_VOLUME_T = PP_VOLUME / 2000) # short tons are 2000 pounds
  }
  
  low_prop_types <- summarized_data %>% 
    select(PP_VALUE, PRODUCT_FORM) %>%
    group_by(PRODUCT_FORM) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    mutate(TOTAL_VALUE = sum(PP_VALUE),
           VALUE_SHARE = PP_VALUE / TOTAL_VALUE) %>%
    filter(VALUE_SHARE < 0.02) %>%
    select(PRODUCT_FORM) %>%
    distinct() %>%
    pull()
  
  # rename these low proportion types as 'OTHER*' and re-summarise
  new_data <- summarized_data %>%
    mutate(PRODUCT_FORM = ifelse(PRODUCT_FORM %in% c('OTHER', low_prop_types),
                                 'OTHER*', PRODUCT_FORM)) %>%
    group_by(YEAR, !!field, PRODUCT_FORM) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    mutate(PP_VALUE_MILLIONS = PP_VALUE / 1000000,
           PP_PRICE = PP_VALUE / PP_VOLUME,
           PRODUCT_FORM = factor(str_to_title(PRODUCT_FORM),
                                 levels = c(
                                   'Fillets', 'Surimi', 'Steaks', 'Meat',
                                   'Breaded Product', 'Meal', 'Cakes & Patties', 'Ready-To-Eat',
                                   'Dressed', 'Smoked', 'Whole', 'Unaltered',
                                   'Canned', 'Roe / Caviar', 'Oil', 'Dried',
                                   'Shucked Meat', 'Peeled', 'Headless', 'Sections',
                                   'Tails', 'Body Parts', 'Claws', 'Fins',
                                   'Not For Human Consumption', 'Not Specified', 'Other*')))
  
  return(new_data)
}
summarize_landings_yr_spp <- function(landings_data, species, coast, full_data = F,
                                      units = NULL, nominal = F) {
  # this function summarizes landings data (not exclusively commercial) by 
  # year and species of interest
  # landings_data is a formatted data frame of FOSS landings data 
  # (see 2_data_munge.R)
  # species is a character vector of a species of interest
  
  # coerce species to upper case to match data formatting
  species <- toupper(species)
  
  # if species is provided, find the level of the categorization hierarchy in 
  # which it exists
  if (species != 'ALL SPECIES') {
    which_level <- as.symbol(
      ifelse(species %in% unique(landings_data$SPECIES_CATEGORY), 
             'SPECIES_CATEGORY',
             ifelse(species %in% unique(landings_data$SPECIES_GROUP), 
                    'SPECIES_GROUP',
                    'SPECIES_NAME')))
    
    level <- rlang::enquo(which_level)
  } else {
    level <- NULL
    species <- str_to_title(species)
  }
  
  if (coast == 'ALL') {
    field <- as.symbol('COAST')
    field <- rlang::enquo(field)
  } else {field <- NULL}
  
  summarized_data <- landings_data %>%
    filter_species(species) %>%
    filter_coast(coast) %>%
    filter(CONFIDENTIALITY != 'Confidential',
           !is.na(DOLLARS),
           !is.na(KG)) %>%
    select(YEAR, !!level, !!field, KG, DOLLARS_2024, DOLLARS) %>%
    group_by(YEAR, !!level, !!field) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop')
  
  if (full_data == T) {
    summarized_data <- summarized_data %>%
      mutate(MT = KG / 1000,
             LB = KG * 2.20462,
             ST = LB / 2000,
             MILLIONS_DOLLARS_2024 = DOLLARS_2024 / 1000000,
             BILLIONS_DOLLARS_2024 = DOLLARS_2024 / 1000000000,
             COM_PRICE_2024USD_PER_KG = DOLLARS_2024 / KG,
             COM_PRICE_2024USD_PER_LB = DOLLARS_2024 / LB,
             MILLIONS = DOLLARS / 1000000,
             BILLIONS = DOLLARS / 1000000000,
             COM_PRICE_NOMINAL_PER_KG = DOLLARS / KG,
             COM_PRICE_NOMINAL_PER_LB = DOLLARS / LB) %>%
      rename(COM_VOLUME_KG = KG,
             COM_VOLUME_MT = MT,
             COM_VOLUME_LB = LB,
             COM_VOLUME_ST = ST,
             COM_VALUE_MILLIONS_2024USD = MILLIONS_DOLLARS_2024,
             COM_VALUE_BILLIONS_2024USD = BILLIONS_DOLLARS_2024,
             COM_VALUE_MILLIONS = MILLIONS,
             COM_VALUE_BILLIONS = BILLIONS) 
    
    return(summarized_data)
  }
  
  if (nominal == F) {
    summarized_data <- summarized_data %>%
      select(!c(DOLLARS)) %>%
      rename(COM_VALUE = DOLLARS_2024)
  } else {
    summarized_data <- summarized_data %>%
      select(!c(DOLLARS_2024)) %>%
      rename(COM_VALUE = DOLLARS)
  }
  
  if (units == 'METRIC') {
    summarized_data <- summarized_data %>%
      rename(COM_VOLUME = KG) %>%
      mutate(COM_VOLUME_T = COM_VOLUME / 1000)
  } else if (units == 'IMPERIAL') {
    summarized_data <- summarized_data %>%
      mutate(COM_VOLUME = KG * 2.20462, # convert kilgorams to pounds
             COM_VOLUME_T = COM_VOLUME / 2000) # short tons are 2000 pounds
  }
  
  summarized_data <- summarized_data %>%
    mutate(COM_VALUE_MILLIONS = COM_VALUE / 1000000,
           COM_PRICE = COM_VALUE / COM_VOLUME) 
  
  return(summarized_data)
}
summarize_yr_spp <- function(species, coast, units = NULL,  nominal = F) {
  # this function utilizes the summary functions for trade, processed products,
  # and landings by year and species of interest and joins the data sets
  # produced by these functions
  # this enables more complex visualizations and calculations of these data
  # for species of interest, specifically for the function calculate_supply_metrics
  # species is a character vector of a species of interest
  
  # coerce species to uppercase to match data formatting
  species <- ifelse(species == 'All Species', 'All Species', toupper(species))
  
  combined_data <- 
    # the order of joining is fairly irrelevant
    left_join(left_join(summarize_trade_yr_spp(trade_data, species, coast, 'VALUE',
                                               units = units, nominal = nominal),
                        # for processed produccts, we must perform an additional
                        # step by removing the product name (condition) from
                        # the data to prevent duplicated data from subsequent
                        # joins
                        summarize_pp_yr_spp(pp_data, species, coast, units = units,
                                            nominal = nominal) %>%
                          select(!PRODUCT_FORM) %>%
                          # regroup by Year and sum value and volume columns
                          group_by(YEAR) %>%
                          summarise(across(where(is.numeric), sum),
                                    .groups = 'drop')),
              summarize_landings_yr_spp(landings, species, coast, units = units,
                                        nominal = nominal)) 
  
  return(combined_data)
}
calculate_mlti <- function(species, coast, exports = F, imports = F, nominal = F) {
  # this function calculates the multi-lateral Lowe trade index (MLTI) among
  # the top 5 trading countries for a given species, either for imports
  # or exports
  # species is a character vector of a species of interest
  # exports is logical that specifies if the MLTI is an export index
  # imports is logical that specifies if the MLTI is an import index
  
  # stop function if exports or imports are not specified
  if (exports == F & imports == F) {
    stop('Please set either "exports" or "imports" to "T"')
  }
  
  # coerce species to uppercase to match data formatting
  species <- toupper(species)
  
  # set value and volume to class of type symbol, specify if the value and 
  # volume are export or import
  if (nominal == T) {
    which_value <- as.symbol(ifelse(exports == T, 'EXP_VALUE_USD',
                                    'IMP_VALUE_USD'))
  } else {
    which_value <- as.symbol(ifelse(exports == T, 'EXP_VALUE_2024USD',
                                    'IMP_VALUE_2024USD')) 
  }
  which_volume <- as.symbol(ifelse(exports == T, 'EXP_VOLUME_KG',
                                   'IMP_VOLUME_KG'))
  # set value and volume to type quosure (see RLang package for details)
  which_value <- rlang::enquo(which_value)
  which_volume <- rlang::enquo(which_volume)
  
  # if a species is specified, find the level of the classification hierarchy
  # in which it resides
  if (species != 'ALL SPECIES') {
    which_level <- as.symbol(
      ifelse(species %in% unique(landings_data$SPECIES_CATEGORY), 
             'SPECIES_CATEGORY',
             ifelse(species %in% unique(landings_data$SPECIES_GROUP), 
                    'SPECIES_GROUP',
                    'SPECIES_NAME'))
    )
    # coerce the level to be quosure
    which_level <- rlang::enquo(which_level)
    
    # step 1: filter trade data for species of interest
    spp_data <- trade_data %>%
      filter_species(species) %>%
      filter_coast(coast) %>%
      # do not include absent values
      filter(is.na(!!which_value) == F)
    
    # step 2: calculate the average price per year per country
    summary_spp_data <- spp_data %>%
      select(YEAR, COUNTRY_NAME, !!which_level, !!which_value,
             !!which_volume) %>%
      group_by(YEAR, COUNTRY_NAME, !!which_level) %>%
      summarise(across(where(is.numeric), sum),
                .groups = 'drop') %>%
      filter(!!which_volume > 0) %>%
      mutate(PRICE = !!which_value / !!which_volume)
    
  } else if (species == 'ALL SPECIES') {
    # alternative: if no species is selected
    # same steps as before except no species is selected
    spp_data <- trade_data %>%
      filter(is.na(!!which_value) == F)
    
    summary_spp_data <- spp_data %>%
      select(YEAR, COUNTRY_NAME, !!which_value, !!which_volume) %>%
      group_by(YEAR, COUNTRY_NAME) %>%
      summarise(across(where(is.numeric), sum),
                .groups = 'drop') %>%
      filter(!!which_volume > 0) %>%
      mutate(PRICE = !!which_value / !!which_volume)
  }
  
  # step 3: count the number of years and the number of countries represented
  total_years <- length(unique(summary_spp_data$YEAR))
  total_countries <- length(unique(summary_spp_data$COUNTRY_NAME))
  
  # step 4: sum the average prices across all countries
  average_price <- summary_spp_data %>%
    select(!c(YEAR, !!which_value, !!which_volume)) %>%
    group_by(COUNTRY_NAME) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    summarise(across(where(is.numeric), sum)) 
  
  # step 5: calculate the overall average price by dividing step 4's output
  # by the product of the number of years and the number of countries
  average_price <- average_price$PRICE / (total_years * total_countries)
  
  # step 6: find top 5 trading partners by value during most recent year (2024)
  top5 <- summary_spp_data %>%
    filter(YEAR == 2024) %>%
    group_by(COUNTRY_NAME) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    arrange(-!!which_value) %>%
    top_n(5, !!which_value)
  
  # step 7: set base country as the middle (third) country in the list
  # the list is arranged by value
  base_country <- top5$COUNTRY_NAME[3]
  # output trading partners from first year of period (2004)
  trade_nations <- summary_spp_data %>%
    filter(YEAR == 2004) %>%
    select(COUNTRY_NAME) %>%
    distinct() 
  
  # make sure that the base country was a trade partner in 2004
  # if it is not, set base country as the second listed country
  # this is a band-aid solution
  if (base_country %in% trade_nations$COUNTRY_NAME) {} else {
    base_country <- top5$COUNTRY_NAME[2]
  } 
  
  # step 8: calculate the Q-index of the base country in 2004
  # the Q-index is the base country's trade volume in the base year multiplied
  # by the average price calculated in step 5; in other words, it is the
  # normalized value of the traded volume determined by the average price
  # of the traded product during the time period by all trading partners
  base_country_q <- summary_spp_data %>%
    filter(YEAR == 2004,
           COUNTRY_NAME == base_country) %>%
    mutate(Q_INDEX = !!which_volume * average_price)
  
  # set this value as the index base
  index_base <- base_country_q$Q_INDEX
  
  # step 9: calculate the MLTI for the top 9 countries throughout the time period
  # the MLTI is each country's Q-index divided by the index base, or the base
  # country's Q-index during the base year
  mlti_data <- summary_spp_data %>%
    filter(COUNTRY_NAME %in% top5$COUNTRY_NAME) %>%
    mutate(Q_INDEX = !!which_volume * average_price) %>%
    select(YEAR, COUNTRY_NAME, Q_INDEX) %>%
    mutate(MLTI = Q_INDEX / index_base)
  
  return(mlti_data)
}
calculate_mlti_table <- function(species, exports = F, imports = F) {
  # this function is identical to calculate_mlti save for one major difference:
  # it calculates the top 5 countries rather than the top 9; this enables
  # a more concise table to be outputted for the app
  # see calculate_mlti for notes on this function
  if (exports == F & imports == F) {
    stop('Please set either "exports" or "imports" to "T"')
  }
  
  species <- toupper(species)
  
  which_value <- as.symbol(ifelse(exports == T, 'EXP_VALUE_2024USD',
                                  'IMP_VALUE_2024USD'))
  which_volume <- as.symbol(ifelse(exports == T, 'EXP_VOLUME_KG',
                                   'IMP_VOLUME_KG'))
  which_value <- rlang::enquo(which_value)
  which_volume <- rlang::enquo(which_volume)
  
  if (species != 'ALL SPECIES') {
    which_group <- as.symbol(
      ifelse(species %in% unique(landings_data$SPECIES_CATEGORY), 
             'SPECIES_CATEGORY',
             ifelse(species %in% unique(landings_data$SPECIES_GROUP), 
                    'SPECIES_GROUP',
                    'SPECIES_NAME')))

    which_group <- rlang::enquo(which_group)
    
    spp_data <- trade_data %>%
      filter_species(species) %>%
      filter(is.na(!!which_value) == F) 
    
    summary_spp_data <- spp_data %>%
      select(YEAR, COUNTRY_NAME, !!which_group, !!which_value,
             !!which_volume) %>%
      group_by(YEAR, COUNTRY_NAME, !!which_group) %>%
      summarise(across(where(is.numeric), sum),
                .groups = 'drop') %>%
      filter(!!which_volume > 0) %>%
      mutate(PRICE = !!which_value / !!which_volume)
    
  } else if (species == 'ALL SPECIES') {
    spp_data <- trade_data %>%
      filter(is.na(!!which_value) == F)
    
    summary_spp_data <- spp_data %>%
      select(YEAR, COUNTRY_NAME, !!which_value, !!which_volume) %>%
      group_by(YEAR, COUNTRY_NAME) %>%
      summarise(across(where(is.numeric), sum),
                .groups = 'drop') %>%
      filter(!!which_volume > 0) %>%
      mutate(PRICE = !!which_value / !!which_volume)
  }
  
  total_years <- length(unique(summary_spp_data$YEAR))
  total_countries <- length(unique(summary_spp_data$COUNTRY_NAME))
  
  
  average_price <- summary_spp_data %>%
    select(!c(YEAR, !!which_value, !!which_volume)) %>%
    group_by(COUNTRY_NAME) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    summarise(across(where(is.numeric), sum)) 
  
  average_price <- average_price$PRICE / (total_years * total_countries)
  
  top9 <- summary_spp_data %>%
    filter(YEAR == 2024) %>%
    group_by(COUNTRY_NAME) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    arrange(-!!which_value) %>%
    top_n(5, !!which_value)
  
  base_country <- top9$COUNTRY_NAME[3]
  trade_nations <- summary_spp_data %>%
    filter(YEAR == 2004) %>%
    select(COUNTRY_NAME) %>%
    distinct() 
  
  base_country_q <- summary_spp_data %>%
    filter(YEAR == 2004,
           COUNTRY_NAME == base_country) %>%
    mutate(Q_INDEX = !!which_volume * average_price)
  
  index_base <- base_country_q$Q_INDEX
  
  mlti_data <- summary_spp_data %>%
    filter(COUNTRY_NAME %in% top9$COUNTRY_NAME) %>%
    mutate(Q_INDEX = !!which_volume * average_price) %>%
    select(YEAR, COUNTRY_NAME, Q_INDEX) %>%
    mutate(MLTI = Q_INDEX / index_base) %>%
    select(!Q_INDEX) %>%
    mutate(COUNTRY_NAME = str_to_title(COUNTRY_NAME)) %>%
    rename(Year = YEAR) %>%
    pivot_wider(names_from = COUNTRY_NAME,
                values_from = MLTI) %>%
    mutate(Year = as.character(Year))
  
  return(mlti_data)
}
calculate_hi <- function(species, coast, nominal = F) {
  # this function calculates the herfindahl trade index for a species of interest
  # species is a character vector of a species of interest
  
  if (nominal == T) {
    exp_value <- as.symbol('EXP_VALUE_USD')
    imp_value <- as.symbol('IMP_VALUE_USD')
    exp_value <- rlang::enquo(exp_value)
    imp_value <- rlang::enquo(imp_value)
  } else {
    exp_value <- as.symbol('EXP_VALUE_2024USD')
    imp_value <- as.symbol('IMP_VALUE_2024USD')
    exp_value <- rlang::enquo(exp_value)
    imp_value <- rlang::enquo(imp_value)
  }
  
  # calculate index from trade data
  hi_data <- trade_data %>%
    filter_species(species) %>%
    filter_coast(coast) %>%
    # select only columns of interest
    select(YEAR, COUNTRY_NAME, EXP_VALUE_2024USD, IMP_VALUE_2024USD,
           EXP_VALUE_USD, IMP_VALUE_USD) %>%
    # set export and import NAs to 0 to prevent NA as sum values
    mutate(EXP_VALUE_2024USD = ifelse(is.na(EXP_VALUE_2024USD) == T,
                                      0, EXP_VALUE_2024USD),
           IMP_VALUE_2024USD = ifelse(is.na(IMP_VALUE_2024USD) == T,
                                      0, IMP_VALUE_2024USD),
           EXP_VALUE_USD = ifelse(is.na(EXP_VALUE_USD) == T,
                                  0, EXP_VALUE_USD),
           IMP_VALUE_USD = ifelse(is.na(IMP_VALUE_USD) == T,
                                  0, IMP_VALUE_USD)) %>%
    # sum the total value by each country in each year
    group_by(YEAR, COUNTRY_NAME) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    group_by(YEAR) %>%
    # for each year,
    # step 1: sum the export and import value
    # step 2: calculate the proportion of export and import value for each
    # country
    # step 3: square the proportion of export and import value
    # step 4: sum the squares to calculate the HI for exports and imports
    mutate(TOTAL_EXP_VALUE_YR = sum(!!exp_value),
           TOTAL_IMP_VALUE_YR = sum(!!imp_value),
           PROPORT_EXP_VALUE = !!exp_value / TOTAL_EXP_VALUE_YR,
           PROPORT_IMP_VALUE = !!imp_value / TOTAL_IMP_VALUE_YR,
           PROPORT_EXP_SQUARED = PROPORT_EXP_VALUE^2,
           PROPORT_IMP_SQUARED = PROPORT_IMP_VALUE^2,
           EXP_HI = sum(PROPORT_EXP_SQUARED),
           IMP_HI = sum(PROPORT_IMP_SQUARED)) %>%
    # retain year and HI's of exports and imports
    select(YEAR, EXP_HI, IMP_HI) %>%
    # remove duplicate columns so there is one of each per year
    distinct()
  
  return(hi_data)
}
calculate_supply_metrics <- function(species, coast, units = NULL, nominal = F) {
  # this function calculates three metrics that we visualize:
  # apparent supply, apparent supply relative to domestic production, and
  # unexported domestic production relative to apparent supply
  # the function relies on summarize_yr_spp for data formatting
  # species is a character vector of a species of interest
  
  data <- summarize_yr_spp(species, coast, units = units, nominal = nominal) %>%
    # calculate apparent supply by summing domestic production and imports 
    # and subtracting export volume
    # calculate apparent supply relative to domestic production by dividing
    # apparent supply by domestic production
    # calculate unexported domestic production relative to apparent supply by
    # dividing the absolute value of the difference of domestic production and
    # export volume by apparent supply
    mutate(APPARENT_SUPPLY = (PP_VOLUME_T - EXP_ROUND_VOLUME_T) + IMP_ROUND_VOLUME_T,
           APPARENT_SUPPLY_REL_US_PROD = APPARENT_SUPPLY / PP_VOLUME_T,
           UNEXPORTED_US_PROD_REL_APPARENT_SUPPLY = 
             abs(PP_VOLUME_T - EXP_ROUND_VOLUME_T) / APPARENT_SUPPLY,
           SPECIES = species) 
  
  return(data)
}

# plot functions
plot_trade <- function(data, coast, plot_format, units = NULL, export = F, import = F, species, nominal = F) {
  # this function has the power to generate multiple plot types of trade data
  # data is formatted trade data from summarize_trade_yr_spp
  # plot_format is a character vector that currently accepts these inputs:
  # 'VALUE', 'VOLUME', 'PRICE', 'BALANCE', 'RATIO'
  # export is logical that specifies if the output should be for export data
  # import is logical that specifies if the output should be for import data
  if (species == 'All Species') {
    species <- 'Highly Migratory Species'
  }
  
  # set shortform and longform values for plot labeling if export
  if (export == T & import == F) {
    shortform <- 'EXP'
    longform <- 'Exports'
    color <- export_color
    if (!(coast %in% c('', 'ALL'))) {
      coast_text <- paste0(' from the ', coast)
    } else {coast_text <- ''}
  }
  # set shortform and longform values for plot labeling if import
  if (import == T & export == F) {
    shortform <- 'IMP'
    longform <- 'Imports'
    color <- import_color
    if (!(coast %in% c('', 'ALL'))) {
      coast_text <- paste0(' to the ', coast)
    } else {coast_text <- ''}
  }
  # coerce plot_format to uppercase to work within function
  plot_format <- toupper(plot_format)
  
  if (coast == 'ALL') {
    data <- data %>%
      filter(!is.na(COAST))
  }
  
  # set labels and y values for plots of VALUE
  if (plot_format == 'VALUE') {
    y <- as.symbol(paste0(shortform, '_VALUE_MILLIONS'))
    y <- rlang::enquo(y)
    
    # label <- label_currency(suffix = 'B')
    label <- label_currency(suffix = 'M')
    
    y2 <- as.symbol(paste0(shortform, '_PRICE'))
    y2 <- rlang::enquo(y2)
    
    max_exp_price <- max(data$EXP_PRICE, na.rm = T)
    max_imp_price <- max(data$IMP_PRICE, na.rm = T)
    
    # the rate for price will depend on specified units
    if (units == 'METRIC') {
      label2 <- label_currency(suffix = '/kg')
      unit <- ' per kilogram'
    } else if (units == 'IMPERIAL') {
      label2 <- label_currency(suffix = '/lb')
      unit <- ' per pound'
    }
    
    if (nominal == T) {
      ylab <- 'Millions (Nominal USD)'
      ylab2 <- 'Average Price (Nominal USD)'
    } else {
      ylab <- 'Millions (Real 2024 USD)'
      ylab2 <- 'Average Price (Real 2024 USD)' 
    }
    
    max_exp <- max(data$EXP_VALUE_MILLIONS, na.rm = T)
    max_imp <- max(data$IMP_VALUE_MILLIONS, na.rm = T)
    
    y_max <- ifelse(max_exp > max_imp, max_exp, max_imp)
    
    y2_max <- ifelse(max_exp_price > max_imp_price, 
                     max_exp_price, max_imp_price)
  }
  
  # set labels and y values for plots of VOLUME
  if (plot_format == 'VOLUME') {
    y <- as.symbol(paste0(shortform, '_VOLUME_T'))
    y <- rlang::enquo(y)
    label <- comma
    
    max_exp <- max(data$EXP_VOLUME_T, na.rm = T)
    max_imp <- max(data$IMP_VOLUME_T, na.rm = T)
    
    ylab <- ifelse(units == 'METRIC', 'Metric Tons', 'Short Tons')
    
    y_max <- ifelse(max_exp > max_imp, max_exp, max_imp)
    tlab <- 'Volume'
  }
  
  # plots of VALUE and VOLUME
  if (plot_format %in% c('VOLUME')) {
    plot <- 
      ggplot(data = data,
             aes(x = factor(YEAR),
                 # call for unique y value set earlier (see RLang)
                 y = !!y)) + 
      geom_col(color = 'black',
               fill = color) +
      scale_x_discrete(breaks = seq(2006, 2022, by = 4),
                       limits = factor(2004:2024)) +
      scale_y_continuous(labels = label,
                         limits = c(0, y_max)) +
      labs(x = '',
           y = ylab,
           title = paste0(species, ' ', longform, coast_text)) +
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            plot.title = element_text(size = 18),
            axis.title = element_text(size = 15))
  } else if (plot_format == 'VALUE') {
    # plot of Value with Price overlayed as a line chart
    # because we have a line chart, we need a column to group by
    data$GROUP <- 'group'
    
    # for the two axes to work in ggplot, we need a scaling factor to apply
    # The scaling factor will coerce the price values to work against the 
    # main y axis of value while retaining price trends across years
    # To ensure the plots of exports and imports are coerced to the same axis,
    # use the same scale factor for all plots by using the max value and 
    # max price, which may not necessarily be both exports or imports
    scale_factor <- y_max / y2_max
    
    plot <- 
      ggplot(data = data,
             aes(x = factor(YEAR))) +
      geom_col(aes(y = !!y),
               fill = color,
               color = 'black') +
      geom_line(aes(y = !!y2 * scale_factor,
                    group = GROUP),
                color = trade_price_color,
                linewidth = 1.5) +
      geom_point(aes(y = !!y2 * scale_factor),
                 color = 'black',
                 size = 2) +
      scale_x_discrete(breaks = seq(2006, 2022, by = 4),
                       limits = factor(2004:2024)) +
      scale_y_continuous(name = ylab, 
                         labels = label,
                         limits = c(0, y_max + y_max*0.1),
                         sec.axis = sec_axis(~./scale_factor, name = ylab2,
                                             labels = label2)) +
      labs(x = '',
           title = paste0(species, ' ', longform, coast_text)) +
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            plot.title = element_text(size = 18),
            axis.title = element_text(size = 15))
  } else if (plot_format == 'RATIO') {
    # plot of RATIO
    # RATIO is a line chart, so we need a column to group by
    data$GROUP <- 'group'
    if (!(coast %in% c('', 'ALL'))) {
      coast_text <- paste0(' traded in the ', coast)
    } else {coast_text <- ''}
    
    plot <- 
      ggplot(data = data, 
             aes(x = factor(YEAR),
                 # calculate export / import volume ratio here
                 y = RATIO)) +
      geom_line(aes(group = GROUP),
                color = 'black',
                linewidth = 1.5) +
      geom_point(color = 'black',
                 size = 2) +
      scale_x_discrete(breaks = seq(2006, 2022, by = 4),
                       limits = factor(2004:2024)) +
      labs(x = '', 
           y = 'Export / Import',
           title = paste0('Volume Ratio of ', species, coast_text)) +
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            plot.title = element_text(size = 18),
            axis.title = element_text(size = 15))
  } else {
    if (!(coast %in% c('', 'ALL'))) {
      coast_text <- paste0(' traded in the ', coast)
    } else {coast_text <- ''}
    
    plot <- 
      ggplot(data = data,
             aes(x = factor(YEAR),
                 y = VALUE_MILLIONS)) +
      geom_bar(aes(fill = TRADE),
               stat = 'identity',
               position = 'dodge',
               color = 'black') +
      labs(x = '',
           # y = 'Billions (Real 2024 USD)',
           y = 'Millions (Real 2024 USD)',
           fill = '',
           title = paste0('Value Balance of ', species, coast_text)) +
      scale_fill_manual(values = balance_colors) +
      coord_axes_inside(labels_inside = T) +
      scale_x_discrete(limits = factor(2004:2024)) +
      scale_y_continuous(labels = label_currency()) +
      geom_hline(yintercept = 0, color = 'black') +
      theme_minimal() +
      theme(legend.position = 'top',
            axis.line.y = element_line(color = 'black'),
            axis.text.x = element_text(hjust = 0.8,
                                       size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.y = element_text(vjust = 23,
                                        size = 15),
            legend.text = element_text(size = 15),
            plot.title = element_text(size = 18),
            plot.background = element_rect(fill = 'white',
                                           color = 'white'),
            panel.grid = element_blank(),
            plot.margin = margin(5.5, 5.5, 5.5, 75.5, 'points'))
  }
  
  return(plot)
}
plot_trade_ctry_yr_spp <- function(data, species, coast, nominal = F) {
  # this function plots trade among the top five trading partners for a species
  # using data generated by summarize_trade_ctry_yr_spp
  # value is logical that specifies if the data is formatted for value
  # volume is logical that specifies if the data is formatted for volume
  if (species == 'All Species') {
    species <- 'Highly Migratory Species'
  }
  
  if (nominal == T) {
    ylab <- 'Millions (Nominal USD)'
  } else {
    ylab <- 'Millions (Real 2024 USD)'
  }
  
  if (coast != '') {
    coast_text <- paste0(' with the ', coast)
  } else {coast_text <- ''}
  
  ggplot(data = data,
         aes(x = factor(gsub(' ', '\n', str_to_title(COUNTRY_NAME))),
             y = NET_VALUE_MILLIONS, 
             fill = factor(YEAR))) +
    geom_col(position = 'dodge',
             color = 'black') +
    scale_fill_manual(values = top5_colors) +
    labs(x = '',
         y = ylab,
         fill = 'Year',
         title = paste0('Net Export Value of ', species, 
                        ' for the \nTop 5 Trading Partners', coast_text)) +
    scale_y_continuous(labels = label_currency(suffix = 'M')) +
    theme_bw() +
    geom_hline(yintercept = 0, 'black') +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 15),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 12),
          plot.title = element_text(size = 18))
}
plot_spp_pp <- function(processed_product_data, coast, plot.format, units = NULL, species, nominal = F) {
  # function that plots processed product data 
  # processed_product_data is data formatted by summarize_pp_yr_spp
  # plot.format is a character vector of three inputs:
  # VALUE, VOLUME, and PRICE
  if (species == 'All Species') {
    species <- 'Highly Migratory Species'
  }
  
  if (coast == 'ALL') {
    field <- as.symbol('COAST')
    field <- rlang::enquo(field)
    
    processed_product_data <- processed_product_data %>%
      filter(!is.na(COAST))
  } else {field <- NULL}
  
  # coerce plot.format to uppercase to work within function
  plot.format <- toupper(plot.format)
  
  if (!(coast %in% c('', 'ALL'))) {
    coast_text <- paste0(coast, ' ')
  } else {coast_text <- ''}
  
  # set labels for VALUE plots
  if (plot.format == 'VALUE') {
    y <- as.symbol('PP_VALUE_MILLIONS') 
    y <- rlang::enquo(y)
    
    if (nominal == T) {
      ylab <- 'Millions (Nominal USD)'
    } else {
      ylab <- 'Millions (2024 Real USD)'
    }
    
    label <- label_currency(suffix = 'M')
    tlab <- 'Production Value of '
  }
  
  if (plot.format == 'VOLUME') {
    y <- as.symbol('PP_VOLUME_T')
    y <- rlang::enquo(y)
    
    if (units == 'METRIC') {
      ylab <- 'Metric Tons'
      label <- comma
      tlab <- 'Production Volume of '
    }
    
    if (units == 'IMPERIAL') {
      ylab <- 'Short Tons'
      label <- comma
      tlab <- 'Production Volume of '
    }
  }
  
  if (plot.format == 'PRICE') {
    # because price is a line chart rather than a bar (as VALUE and VOLUME are),
    # just create plot for PRICE instead of setting label definitions
    
    # specify whether price is reported in per kg or per lb
    if (units == 'METRIC') {
      if (nominal == T) {
        ylab <- 'Average Price (Nominal USD)'
      } else {
        ylab <- 'Average Price (Real 2024 USD)'
      }
      
      label <- label_currency(suffix = '/kg')
    }
    
    if (units == 'IMPERIAL') {
      if (nominal == T) {
        ylab <- 'Average Price (Nominal USD)'
      } else {
        ylab <- 'Average Price (Real 2024 USD)'
      }
      
      label <- label_currency(suffix = '/lb')
    }
    
    ymax <- max(processed_product_data$PP_PRICE) + 0.05*(max(processed_product_data$PP_PRICE))
    
    plot <- ggplot(data = processed_product_data,
                   aes(x = factor(YEAR),
                       y = PP_PRICE,
                       color = PRODUCT_FORM)) +
      geom_line(aes(group = PRODUCT_FORM),
                linewidth = 1.5) +
      geom_point(color = 'black',
                 size = 1.5) +
      scale_color_manual(values = pp_colors,
                         name = 'Product Condition') +
      labs(x = '',
           y = ylab,
           fill = 'Product Condition',
           title = paste0(coast_text, 'Production Price of ', species)) +
      scale_x_discrete(breaks = seq(2006, 2022, by = 4)) +
      scale_y_continuous(limits = c(0, ymax),
                         expand = c(0, 0),
                         labels = label) +
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 15),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 15),
            plot.title = element_text(size = 18))
    
    return(plot)
  }
  
  # find upper limit for value/volume plots
  upper_limit <- processed_product_data %>%
    select(YEAR, !!field, !!y) %>%
    group_by(YEAR, !!field) %>%
    summarise(across(where(is.numeric), sum),
              .groups = 'drop') %>%
    filter(!!y == max(!!y)) %>%
    pull(!!y)
  
  ylim <- upper_limit + 0.05*upper_limit
  
  # plot for VALUE or VOLUME depending on plot.format
  plot <- ggplot(data = processed_product_data,
                 aes(x = factor(YEAR),
                     y = !!y,
                     fill = PRODUCT_FORM)) +
    geom_col(position = 'stack',
             color = 'black') +
    scale_fill_manual(values = pp_colors,
                      name = 'Product Condition') +
    labs(x = '',
         y = ylab,
         fill = 'Product Condition',
         title = paste0(coast_text, tlab, species)) +
    scale_x_discrete(breaks = seq(2006, 2022, by = 4)) +
    scale_y_continuous(limits = c(0, ylim), 
                       expand = c(0, 0),
                       labels = label) +
    theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 15),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 15),
          plot.title = element_text(size = 18))
  
  return(plot)
}
plot_landings <- function(data, coast, plot.format, units = NULL, species, nominal = F) {
  # this function plots landings data formatted by summarize_landings_yr_spp
  # plot.format is a character vector that accepts inputs of VALUE, VOLUME
  # and PRICE
  if (species == 'All Species') {
    species <- 'Highly Migratory Species'
  }
  
  # coerce plot.format to uppercase to work within function
  plot.format <- toupper(plot.format)
  
  if (!(coast %in% c('', 'ALL'))) {
    coast_text <- paste0(coast, ' ')
  } else {coast_text <- ''}
  
  if (coast == 'ALL') {
    data <- data %>%
      filter(!is.na(COAST))
  }
  
  # set labels for VALUE plot
  if (plot.format == 'VALUE') {
    if (units == 'METRIC') {
      label2 <- label_currency(suffix = '/kg')
      unit <- 'per kilogram'
    } else if (units == 'IMPERIAL') {
      label2 <- label_currency(suffix = '/lb')
      unit <- 'per pound'
    }
    
    if (nominal == F) {
      ylab <- 'Millions (Real 2024 USD)'
      ylab2 <- 'Average Price (Real 2024 USD)'
    } else {
      ylab <- 'Millions (Nominal USD)'
      ylab2 <- 'Average Price (Nominal USD)'
    }
    
    label <- label_currency(suffix = 'M')
    tlab <- 'Ex-Vessel Value of '
  }
  
  # set labels for VOLUME plot
  if (plot.format == 'VOLUME') {
    if (units == 'METRIC') {
      ylab <- 'Metric Tons'
    } else if (units == 'IMPERIAL') {
      ylab <- 'Short Tons'
    }
    
    label <- comma
    tlab <- 'Landed Volume of '
  }
  
  # create plot for VALUE
  # This plot has two y-axes to display value and price in the same chart
  if (plot.format == 'VALUE') {
    # create GROUP column for the line chart to GROUP by
    data$GROUP <- 'group'
    
    # calculate scale factor (see plot_trade for details)
    max_value <- data %>%
      slice_max(COM_VALUE_MILLIONS, n = 1) %>%
      select(COM_VALUE_MILLIONS) %>%
      pull()
    
    max_price <- data %>%
      slice_max(COM_PRICE, n = 1) %>%
      select(COM_PRICE) %>%
      pull()
    
    scale_factor <- max_value / max_price
    
    plot <- 
      ggplot(data = data,
             aes(x = factor(YEAR))) +
      geom_col(aes(y = COM_VALUE_MILLIONS),
               fill = landings_colors[1],
               color = 'black') +
      geom_line(aes(y = COM_PRICE * scale_factor,
                    group = GROUP),
                color = landings_colors[2],
                linewidth = 1.5) +
      geom_point(aes(y = COM_PRICE * scale_factor),
                 color = 'black',
                 size = 2) +
      scale_x_discrete(breaks = seq(2006, 2022, by = 4),
                       limits = factor(2004:2023)) +
      scale_y_continuous(name = ylab, 
                         labels = label,
                         sec.axis = sec_axis(~./scale_factor, name = ylab2,
                                             labels = label2)) +
      labs(x = '',
           title = paste0(coast_text, tlab, species)) +
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 15),
            plot.title = element_text(size = 18))
    
    return(plot)
  }
  
  # output plot of VOLUME
  plot <- 
    ggplot(data = data,
           aes(x = factor(YEAR),
               y = COM_VOLUME_T)) +
    geom_col(color = 'black',
             fill = landings_colors[1]) +
    scale_x_discrete(breaks = seq(2006, 2022, by = 4),
                     limits = factor(2004:2023)) +
    scale_y_continuous(labels = label) +
    labs(x = '',
         y = ylab,
         title = paste0(coast_text, tlab, species)) +
    theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 15),
          plot.title = element_text(size = 18))
  
  return(plot)
}
plot_mlti <- function(mlti_data, coast, exports = F, imports = F, species) {
  # this function generates a plot of MLTI data with countries distinct by
  # color and point shape
  # mlti_data is a data set formatted by calculate_mlti
  # exports is logical that reflects if the data input is for exports
  # imports is logical that reflects if the data input is for imports
  if (species == 'All Species') {
    species <- 'Highly Migratory Species'
  }
  
  # stop function if neither exports nor imports were specified
  if (exports == F & imports == F) {
    stop('Please set "exports" or "imports" to "T"')
  }
  
  if (coast != '') {
    coast_text <- paste0(ifelse(exports == T, ' from the ', ' to the '),
                          coast)
  } else {coast_text <- ''}
  
  # set label for plot based on exports logical
  label <- ifelse(exports == T, 'Export', 'Import')
  
  ggplot(data = mlti_data %>%
           mutate(COUNTRY_NAME = str_to_title(COUNTRY_NAME)),
         aes(x = factor(YEAR),
             y = MLTI,
             color = COUNTRY_NAME,
             shape = COUNTRY_NAME)) +
    geom_line(aes(group = COUNTRY_NAME),
              linewidth = 1.25) +
    geom_point(color = 'black',
               size = 2.5) +
    scale_x_discrete(breaks = seq(2006, 2022, by = 4)) +
    scale_color_manual(values = mlti_colors) +
    # hline sets baseline to compare points from base index for all plots
    geom_hline(yintercept = 1, color = 'black') +
    labs(x = '',
         y = 'Multilateral Trade Index',
         title = paste0(label, 's of ', species, coast_text),
         color = '',
         shape = '') +
    theme_bw() +
    theme(axis.text = element_text(size = 15),
          axis.title.y = element_text(size = 18),
          plot.title = element_text(size = 20),
          strip.text = element_text(size = 15,
                                    color = 'white'),
          legend.text = element_text(size = 15),
          legend.key.size = unit(2, 'line'),
          strip.background = element_rect(fill = 'black'))
}
plot_hi <- function(hi_data, coast, species) {
  # this function generates a line plot that compares HI for exports and imports
  # hi_data is a data set formatted by calculate_hi
  if (species == 'All Species') {
    species <- 'Highly Migratory Species'
  }
  
  if (coast != '') {
    coast_text <- paste0(' traded in the ', coast)
  } else {coast_text <- ''}
  
  # format the data by renaming columns for plot labels
  format_hi_data <- hi_data %>%
    rename(Exports = EXP_HI,
           Imports = IMP_HI) %>%
    # pivot the plot longer to create a grouping column by export or import
    pivot_longer(cols = c(Exports, Imports))
  
  ggplot(data = format_hi_data,
         aes(x = as.factor(YEAR),
             y = value)) +
    # group lines by the pivoted longer column 'name'
    geom_line(aes(group = name, 
                  colour = name),
              linewidth = 1.5) +
    geom_point(size = 2,
               color = 'black') +
    scale_color_discrete(name = NULL, 
                         type = c(export_color, import_color)) +
    labs(x = '',
         y = 'Index',
         title = paste0('Herfindahl Index of \n', species, coast_text)) +
    scale_x_discrete(breaks = seq(2006, 2022, by = 4)) +
    theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 15),
          legend.text = element_text(size = 15),
          legend.position = 'inside',
          legend.position.inside = c(0.87, 0.93),
          legend.box.background = element_rect(color = 'black', linetype = 'solid', linewidth = 1),
          plot.title = element_text(size = 16))
  
}
plot_supply_metrics <- function(supply_data, coast, metric, units = NULL, species) {
  # this function generates three types of plots 
  # supply_data is data formatted by calculate_supply_metrics in tandem with
  # summarize_yr_spp
  # metric is a character vector of three accepted inputs: 
  # SUPPLY, RATIO, AND SHARE
  # SUPPLY outputs plots of apparent supply 
  # RATIO outputs plots of apparent supply relative to domestic production
  # SHARE outputs plots of Unexported domestic production relative to 
  # apparent supply
  if (species == 'All Species') {
    species <- 'Highly Migratory Species'
  }
  
  if (coast != '') {
    coast_text <- paste0(' in the ', coast)
  } else {coast_text <- ''}
  if (metric == 'SUPPLY') {
    # units are embedded in the calculation function
    # here, we only need to specify how the figure is labeled
    ylab <- ifelse(units == 'METRIC', 'Metric Tons',
                   'Short Tons')
    plot <- 
      ggplot(data = supply_data %>%
               # we do not have landings or processing data for 2024 despite
               # having so for trade data
               filter(YEAR < 2024),
             aes(x = factor(YEAR),
                 y = APPARENT_SUPPLY)) +
      geom_col(color = 'black',
               fill = c(supply_color)) +
      labs(x = '',
           y = ylab,
           title = paste0('Apparent Supply of \n', species, coast_text)) +
      scale_x_discrete(limits = factor(c(2004:2023)),
                       breaks = seq(2006, 2022, by = 4)) +
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 15),
            plot.title = element_text(size = 18))
  }
  
  if (metric == 'RATIO') {
    plot <- 
      ggplot(data = supply_data %>%
               filter(YEAR < 2024),
             aes(x = factor(YEAR),
                 y = APPARENT_SUPPLY_REL_US_PROD,
                 group = SPECIES)) +
      geom_point(color = 'black', 
                 size = 3) +
      geom_line(color = 'black',
                linewidth = 1) +
      labs(x = '',
           y = 'Ratio',
           title = paste0('Apparent Supply of \n', species, 
                          '\nRelative to Domestic \nProduction', coast_text)) +
      scale_x_discrete(limits = factor(c(2004:2023)),
                       breaks = seq(2006, 2022, by = 4)) +
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 15),
            plot.title = element_text(size = 18))
  }
  
  if (metric == 'SHARE') {
    plot <- 
      ggplot(data = supply_data %>%
               filter(YEAR < 2024),
             aes(x = factor(YEAR),
                 y = UNEXPORTED_US_PROD_REL_APPARENT_SUPPLY)) +
      geom_col(color = 'black',
               fill = share_color) +
      labs(x = '',
           y = 'Share of Apparent Supply',
           title = paste0('Unexported Domestic \nProduction Relative \nto Apparent Supply of \n', 
                          species, coast_text)) +
      scale_x_discrete(limits = factor(c(2004:2023)),
                       breaks = seq(2006, 2022, by = 4)) +
      scale_y_continuous(labels = label_percent()) +
      theme_bw() +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 15),
            plot.title = element_text(size = 18))
  }
  
  return(plot)
}

# tooltip function
tooltip_line_icon <- function(line, point) {
  # Create temporary PNG
  temp_png <- tempfile(fileext = '.png')
  
  # Create larger plot for bigger icon (change icon size here)
  png(temp_png, width = 40, height = 20, bg = 'transparent')
  par(mar = c(0, 0, 0, 0))
  plot(c(0, 1), c(0, 1), type = 'n', axes = F, xlab = '', ylab = '')
  
  # Draw the line (color specific)
  lines(c(0.1, 0.9), c(0.5, 0.5), col = line, lwd = 4)
  
  # Draw point (shape specific)
  points(0.5, 0.5, pch = point, col = 'black', cex = 1.5)
  
  dev.off()
  
  # Convert to base64 
  icon_data <- base64enc::base64encode(temp_png)
  unlink(temp_png) # cleans the file
  
  icon <- paste0("data:image/png;base64,", icon_data)
  
  return(paste0('</span><img src = "', icon, 
                '" class = "tooltip-icon" alt = "legend icon"/>'))
}
tooltip_color_icon <- function(color) {
  paste0("</span><span class = 'color-swatch' style = 'background-color: ", 
         color, ";'>")
}
# Colors -----------------------------------------------------------------------
# Balance plot colors
balance_colors <- c('#B3EDEF', '#1ECAD3', '#005761')
names(balance_colors) <- levels(factor(levels = c('Exports', 'Imports', 'Trade Balance')))
top5_colors <- c('#C6E6F0', '#5EB6D9', '#0085CA', '#003087', '#002364')
names(top5_colors) <- levels(factor(levels = c(2020:2024)))
export_color <- c('#003087')
import_color <- c('#0085CA')
trade_price_color <- c('#A6D4EC')
landings_colors <- c('#853B00', '#FFAB38')
supply_color <- c('#008DA8')
share_color <- c('#005E5E')

# colors designed primarily for processed products at the moment
pp_colors <- c('#853B00', '#DB6015', '#FF8400', '#FFAB38', 
               '#A8821B', '#DDBB25', '#F0D302', '#FFFF65', 
               '#B1DC6B', '#76BC21', '#4B8320', '#365E17',
               '#005E5E', '#00797F', '#1EBEC7', '#90DFE3',
               '#5EB6D9', '#0085CA', '#003087', '#002364',
               '#001743', '#3B469A', '#5761C0', '#737BE6',
               '#9A9A9A', '#646464', '#323C46')

# They are organized here in the same order and line as 'colors' above
names(pp_colors) <- levels(factor(levels = c(
  'Fillets', 'Surimi', 'Steaks', 'Meat',
  'Breaded Product', 'Meal', 'Cakes & Patties', 'Ready-To-Eat',
  'Dressed', 'Smoked', 'Whole', 'Unaltered',
  'Canned', 'Roe / Caviar', 'Oil', 'Dried',
  'Shucked Meat', 'Peeled', 'Headless', 'Sections',
  'Tails', 'Body Parts', 'Claws', 'Fins',
  'Not For Human Consumption', 'Not Specified', 'Other*')))

# Because the countries will change based on the selected species,
# the colors have no mapping
mlti_colors <- c('#A6D4EC', '#54ADDB', '#B3EDEF', '#6DDBE1', '#005761')
# App --------------------------------------------------------------------------
# Define UI --------------------------------------------------------------------
ui <- page_sidebar(
  # custom CSS for wider tooltips
  tags$head(
    tags$style(HTML("
                    .tooltip-inner {
                    max-width: 600px !important;
                    width: auto !important;
                    font-size: 16px !important;
                    background-color: #283A38 !important;
                    }")),
    tags$style(HTML("g.hovertext > path {opacity: .9;}")),
    tags$style(HTML(".color-swatch {
                      display: inline-block;
                      width: 18px;
                      height: 15px; 
                      border: 1px solid #000; 
                      border-radius: 3px;
                      vertical-align: middle;
                      margin-right: 8px;
                    }"))
  ),
  
  sidebar = sidebar(
    title = 'Species Selection',
    actionButton('reset_button', 'Reset All Filters',
                 class = 'btn-warning',
                 style = 'margin-bottom: 15px; width: 100%'),
    # search bar that outputs directions for how to filter for the searched 
    # species (if available)
    # selectizeInput(inputId = 'search_term',
    #                label = 'Search for a Species',
    #                choices = NULL),
    uiOutput('filter_0'),
    uiOutput('filter_1'),
    # these outputs only appear once a selection is made for the prior input
    # this means filter_3 only appears once filter_2 has input, which only
    # appears once filter_1 has input, etc.
    uiOutput('filter_2'),
    uiOutput('filter_3'),
    selectizeInput(inputId = 'coast',
                   label = 'Alternatively, select a Coast',
                   choices = c('', 'West Coast + Alaska', 'Atlantic', 
                               'Pacific Islands', 'Gulf + Territories'),
                   options = list(
                     placeholder = 'Type here...'
                   )),
    input_switch('units', 'Imperial Units'),
    input_switch('nominal', 'Nominal Values'),
    uiOutput('trade_unfilter_button'),
    uiOutput('product_unfilter_button'),
    uiOutput('landings_unfilter_button'),
    downloadButton('download_trade',
                   'Download raw trade data'),
    downloadButton('download_landings',
                   'Download raw landings data'),
    downloadButton('download_products',
                   'Download raw processed products data')
  ),
  
  # Banner
  div(
    style = 'background: linear-gradient(155deg, #001743 0%, #0085CA 100%);
             color: white;
             padding: 30px 20px;
             margin-bottom: 10px;
             min-width: 800px;
             border-radius: 10px;
             align-items: center;
             text-align: left;
             display: flex;
             box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);',
    
    # Image
    div(
      style = 'flex-shrink: 0;',
      img(src = 'NOAA_FISHERIES_H.png',
          align = 'left',
          style = 'width: 200px; height: 200px;'
      )
    ),
    
    # Text
    div(
      style = 'flex: 1; text-align: left;',
      h1('NOAA Fisheries HMS Seafood Dashboard',
         style = 'font-family: "Gill Sans MT", sans-serif; font-size: clamp(2.5rem, 4vw, 4.5rem); margin-bottom: 0px;'),
      
      p('Investigate 20 Years of U.S. Fisheries Data for Highly Migratory Species',
        style = 'font-family: "Gill Sans MT", sans-serif; font-size: clamp(1.5rem, 2.6vw, 2.6rem); margin-top: 0px; margin-bottom: 0px; opacity: 0.9;')
    )),
  page_fluid(
    navset_tab(
      nav_panel(
        title = 'The Dashboard',
        icon = bsicons::bs_icon("layout-wtf"),
        fluidRow(
          div(
            style = 'border: 3px solid #005761; border-radius: 12px;
               min-width: 800px; width: 100%; display: flex; flex-direction: column;',
            navset_card_pill(title = 'Trade',
                             nav_panel(title = 'Market Summary',
                                       div(
                                         style = "position: relative; min-width: 1200px;",
                                         withSpinner(
                                           plotOutput('balance',
                                                      click = clickOpts('balance_plot_click'),
                                                      height = "400px", width = "100%"), 
                                           type = 7),
                                         # textOutput('balance_tooltip'),
                                         uiOutput('balance_click_overlay'),
                                         div(
                                           style = "position: absolute; top: 0px; left: 5px",
                                           tooltip(
                                             icon("info-circle"),
                                             "Trade balance reflects the net value of product traded between the U.S. and all trading partners. Balance values in the negative indicate more product is imported than exported. Balance values in the positive indicate more product is exported than imported."
                                           ))),
                                       div(
                                         style = "flex: 1; display: flex; gap: 15px;",
                                         div(
                                           style = "position: relative; min-width: 600px; width: 100%;",
                                           withSpinner(
                                             plotOutput('trade_ratio',
                                                        click = clickOpts('ratio_plot_click'),
                                                        height = '400px'), 
                                             type = 7),
                                           # textOutput('ratio_tooltip'),
                                           uiOutput('ratio_click_overlay'),
                                           div(
                                             style = "position: absolute; top: 0px; left: 5px",
                                             tooltip(
                                               icon("info-circle"),
                                               "The ratio of the volume of exported product to the volume of imported product. Values less than one indicate a greater volume of product is imported than exported. Values greater than one indicate a greater volume of product is exported than imported."
                                             ))),
                                         div(
                                           style = "position: relative; min-width: 600px; width: 100%;",
                                           withSpinner(
                                             plotOutput('top5_trade',
                                                        click = clickOpts('top5_plot_click'),
                                                        height = '400px'), 
                                             type = 7),
                                           # textOutput('top5_tooltip'),
                                           uiOutput('top5_click_overlay'),
                                           div(
                                             style = "position: absolute; top: 0px; left: 5px",
                                             tooltip(
                                               icon("info-circle"),
                                               "Trade balance reflects the net value of product traded between the U.S. and the given trading partner. The top 5 countries displayed are those with the greatest sum of value traded (exports + imports). Balance values in the negative indicate more product is imported than exported. Balance values in the positive indicate more product is exported than imported. Countries display in alphabetical order."
                                             )))),
                                       downloadButton('download_page1',
                                                      'Download these plots and their data')),
                             nav_panel(title = 'Value',
                                       div(
                                         style = "flex: 1; display: flex; gap: 15px;",
                                         div(
                                           style = "position: relative; min-width: 600px; width: 100%;",
                                           withSpinner(
                                             plotOutput('exp_value', 
                                                        click = clickOpts("exp_value_plot_click"),
                                                        height = "500px"), 
                                             type = 7),
                                           # textOutput('expval_tooltip'),
                                           uiOutput("exp_value_click_overlay"),
                                           div(
                                             style = "position: absolute; top: 0px; left: 5px",
                                             tooltip(
                                               icon("info-circle"),
                                               "Export value reflects the total value of product traded out of the U.S. into other countries. The left y-axis reflects the total value of exports and applies to the bars. The right y-axis reflects the average price of exported product per kilogram or pound and applies to the line and points."
                                             ))),
                                         div(
                                           style = "position: relative; min-width: 600px; width: 100%;",
                                           withSpinner(
                                             plotOutput('imp_value',
                                                        click = clickOpts("imp_value_plot_click"),
                                                        height = "500px"), 
                                             type = 7),
                                           # textOutput('impval_tooltip'),
                                           uiOutput("imp_value_click_overlay"),
                                           div(
                                             style = "position: absolute; top: 0px; left: 5px",
                                             tooltip(
                                               icon("info-circle"),
                                               "Import value reflects the total value of product traded into the U.S. from other countries. The left y-axis reflects the total value of imports and applies to the bars. The right y-axis reflects the average price of imported product per kilogram or pound and applies to the line and points."
                                             )))),
                                       downloadButton('download_page2',
                                                      'Download these plots and their data')),
                             nav_panel(title = 'Volume',
                                       div(
                                         style = "flex: 1; display: flex; gap: 15px;",
                                         div(
                                           style = "position: relative; min-width: 600px; width: 100%;",
                                           withSpinner(
                                             plotOutput('exp_volume',
                                                        click = clickOpts("exp_volume_plot_click"),
                                                        height = "500px"), 
                                             type = 7),
                                           # textOutput('expvol_tooltip'),
                                           uiOutput("exp_volume_click_overlay"),
                                           div(
                                             style = "position: absolute; top: 0px; left: 5px",
                                             tooltip(
                                               icon("info-circle"),
                                               "Export volume reflects the total volume of product traded out of the U.S. into other countries."
                                             ))),
                                         div(
                                           style = "position: relative; min-width: 600px; width: 100%;",
                                           withSpinner(
                                             plotOutput('imp_volume',
                                                        click = clickOpts("imp_volume_plot_click"),
                                                        height = "500px"), 
                                             type = 7),
                                           # textOutput('impvol_tooltip')
                                           uiOutput('imp_volume_click_overlay'),
                                           div(
                                             style = "position: absolute; top: 0px; left: 5px",
                                             tooltip(
                                               icon("info-circle"),
                                               "Import volume reflects the total volume of product traded into the U.S. from other countries."
                                             )))),
                                       downloadButton('download_page3',
                                                      'Download these plots and their data')),
                             nav_panel(title = 'Advanced Metrics',
                                       div(
                                         style = "flex: 1; display: flex; gap: 15px;",
                                         div(
                                           # style argument keeps overlay positioned within the container
                                           style = "position: relative; min-width: 600px; width: 100%;",
                                           withSpinner(
                                             # tableOutput('exp_mlti_table'),
                                             plotOutput('exp_mlti',
                                                        click = clickOpts(id = 'exp_mlti_plot_click'),
                                                        height = "500px"),
                                             type = 7),
                                           # textOutput('expmlti_tooltip'),
                                           uiOutput('exp_mlti_click_overlay'),
                                           div(
                                             style = "position: absolute; top: 0px; left: 5px",
                                             tooltip(
                                               icon("info-circle"),
                                               "The multilateral trade index (MLTI) measures relative densities of exported product volumes to individual nations. The index subsets the top five trading partners by total export value over the time period. The base of the index is the export value of the country with the third most cumulative export value (middle of the top five selected countries) in the initial year of the time period (MLTI = 1 for the base country in the base year). MLTI above 1 reflects a greater density of traded volume than the base. MLTI below 1 reflects a lower density of traded volume than the base."
                                             ))),
                                         div(
                                           style = "position: relative; min-width: 600px; width: 100%;",
                                           withSpinner(
                                             # tableOutput('imp_mlti_table'), 
                                             plotOutput('imp_mlti',
                                                        click = clickOpts(id = 'imp_mlti_plot_click'),
                                                        height = "500px"),
                                             type = 7),
                                           uiOutput('imp_mlti_click_overlay'),
                                           div(
                                             style = "position: absolute; top: 0px; left: 5px",
                                             tooltip(
                                               icon("info-circle"),
                                               "The multilateral trade index (MLTI) measures relative densities of imported product volumes from individual nations. The index subsets the top five trading partners by total import value over the time period. The base of the index is the import value of the country with the third most cumulative import value (middle of the top five selected countries) in the initial year of the time period (MLTI = 1 for the base country in the base year). MLTI above 1 reflects a greater density of traded volume than the base. MLTI below 1 reflects a lower density of traded volume than the base."
                                             )))),
                                       br(),
                                       div(
                                         style = "flex: 1; display: flex; gap: 15px",
                                         div(
                                           style = "position: relative; min-width: 300px; width: 100%",
                                           withSpinner(
                                             plotOutput('hi',
                                                        click = clickOpts(id = 'hi_plot_click'),
                                                        height = "500px"), 
                                             type = 7),
                                           # textOutput('hi_tooltip'),
                                           uiOutput('hi_click_overlay'),
                                           div(
                                             style = "position: absolute; top: 0px; left: 5px",
                                             tooltip(
                                               icon("info-circle"),
                                               "The Herfindahl index (HI) measures the relative distribution of traded product value (exports and imports individually) among trading partners; it cannot be greater than 1. The HI communicates potential trading dependencies for given products. An HI closer to 1 indicates more trade value concentrated among fewer trading partners. An HI closer to 0 indicates trade value is spread out among more trading partners."
                                             ))),
                                         div(
                                           style = "position: relative; min-width: 300px; width: 100%",
                                           withSpinner(
                                             plotOutput('supply',
                                                        click = clickOpts(id = "supply_plot_click"),
                                                        height = "500px"), 
                                             type = 7),
                                           # textOutput('supply_tooltip')
                                           uiOutput("supply_click_overlay"),
                                           div(
                                             style = "position: absolute; top: 0px; left: 5px",
                                             tooltip(
                                               icon("info-circle"),
                                               "Apparent supply indicates the volume of given product available for domestic consumption that relates domestic landings and production with trade."
                                             ))),
                                         div(
                                           style = "position: relative; min-width: 300px; width: 100%",
                                           withSpinner(
                                             plotOutput('supply_ratio',
                                                        click = clickOpts(id = 'supply_ratio_plot_click'),
                                                        height = "500px"), 
                                             type = 7),
                                           # textOutput('supplyratio_tooltip')
                                           uiOutput('supply_ratio_click_overlay'),
                                           div(
                                             style = "position: absolute; top: 0px; left: 5px",
                                             tooltip(
                                               icon("info-circle"),
                                               "Apparent supply relative to domestic production reflects the ratio of apparent supply to domestic production (processed products) volume. Ratios greater than 1 indicate the U.S. must import product to meet domestic demand. Ratios less than 1 indicate the U.S. produces more of the product than is domestically available."
                                             ))),
                                         div(
                                           style = "position: relative; min-width: 300px; width: 100%",
                                           withSpinner(
                                             plotOutput('supply_share',
                                                        click = clickOpts(id = 'supply_share_plot_click'),
                                                        height = "500px"), 
                                             type = 7),
                                           # textOutput('supplyshare_tooltip')
                                           uiOutput('supply_share_click_overlay'),
                                           div(
                                             style = "position: absolute; top: 0px; left: 5px",
                                             tooltip(
                                               icon("info-circle"),
                                               "Unexported domestic production relative to apparently supply reflects the share of apparent supply that derives from retained processed products (i.e., processed product volume less export volume). High percentages indicate most apparent supply is domestically produced and retained. Low percentages indicate most apparent supply is due to imports."
                                             )))),
                                       downloadButton('download_page4',
                                                      'Download these plots and their data'))))
        ),
        fluidRow(
          div(style = 'display: flex; gap: 15px; min-width: 800px; width: 100%;',
              div(
                style = 'border: 3px solid #234515; border-radius: 12px;
                 min-width: 400px; width: 100%; display: flex; flex-direction: column;',
                navset_card_pill(title = 'Commercial Landings',
                                 nav_panel(title = 'Value',
                                           div(
                                             style = "position: relative; min-width: 600px; width: 100%",
                                             withSpinner(
                                               plotOutput('landings_value',
                                                          click = clickOpts(id = 'landings_value_plot_click'),
                                                          height = "500px"),
                                               type = 7),
                                             # textOutput('comvalue_tooltip')
                                             uiOutput('landings_value_click_overlay'),
                                             div(
                                               style = "position: absolute; top: 0px; left: 5px",
                                               tooltip(
                                                 icon("info-circle"),
                                                 "Ex-vessel value reflects the amount paid to fishers for raw product (i.e., landed catch) in the U.S. The left y-axis reflects the total value of landed catch and applies to the bars. The right y-axis reflects the average price of landed catch per kilogram or pound and applies to the line and points."
                                               ))),
                                           downloadButton('download_landings_page1',
                                                          'Download this plot and the data')),
                                 nav_panel(title = 'Volume',
                                           div(
                                             style = "position: relative; min-width: 600px; width: 100%",
                                             withSpinner(
                                               plotOutput('landings_volume',
                                                          click = clickOpts(id = 'landings_volume_plot_click'),
                                                          height = "500px"),
                                               type = 7),
                                             # textOutput('comvolume_tooltip')
                                             uiOutput('landings_volume_click_overlay'),
                                             div(
                                               style = "position: absolute; top: 0px; left: 5px",
                                               tooltip(
                                                 icon("info-circle"),
                                                 "Ex-vessel volume reflects the weight of raw product landed by fishers in the U.S."
                                               ))),
                                           downloadButton('download_landings_page2',
                                                          'Download this plot and the data')))),
              div(
                style = 'border: 3px solid #681617; border-radius: 12px;
                 min-width: 400px; width: 100%; display: flex; flex-direction: column;',
                navset_card_pill(title = 'Processed Products',
                                 nav_panel(title = 'Value',
                                           div(
                                             style = "position: relative; min-width: 600px; width: 100%",
                                             withSpinner(
                                               plotOutput('pp_value',
                                                          click = clickOpts('pp_value_plot_click'),
                                                          height = "500px"),
                                               type = 7),
                                             # textOutput('ppvalue_tooltip')
                                             uiOutput('pp_value_click_overlay'),
                                             div(
                                               style = "position: absolute; top: 0px; left: 5px",
                                               tooltip(
                                                 icon("info-circle"),
                                                 "Processed products are divided by the condition of their processing (i.e., canned, fillets, surimi, etc.). The category Other* includes conditions marked as 'Other' as well as those that comprise 2% or less of total processed product value."
                                               ))),
                                           downloadButton('download_products_page1',
                                                          'Download this plot and the data')),
                                 nav_panel(title = 'Volume',
                                           div(
                                             style = "position: relative; min-width: 600px; width: 100%",
                                             withSpinner(
                                               plotOutput('pp_volume',
                                                          click = clickOpts('pp_volume_plot_click'),
                                                          height = "500px"),
                                               type = 7),
                                             # textOutput('ppvolume_tooltip')
                                             uiOutput('pp_volume_click_overlay'),
                                             div(
                                               style = "position: absolute; top: 0px; left: 5px",
                                               tooltip(
                                                 icon("info-circle"),
                                                 "Processed products are divided by the condition of their processing (i.e., canned, fillets, surimi, etc.). The category Other* includes conditions marked as 'Other' as well as those that comprise 2% or less of total processed product value."
                                               ))),
                                           downloadButton('download_products_page2',
                                                          'Download this plot and the data')),
                                 nav_panel(title = 'Price',
                                           div(
                                             style = "position: relative; min-width: 600px; width: 100%",
                                             withSpinner(
                                               plotOutput('pp_price',
                                                          click = clickOpts('pp_price_plot_click'),
                                                          height = "500px"),
                                               type = 7),
                                             # textOutput('ppprice_tooltip')
                                             uiOutput('pp_price_click_overlay'),
                                             div(
                                               style = "position: absolute; top: 0px; left: 5px",
                                               tooltip(
                                                 icon("info-circle"),
                                                 "Processed products are divided by the condition of their processing (i.e., canned, fillets, surimi, etc.). The category Other* includes conditions marked as 'Other' as well as those that comprise 2% or less of total processed product value."
                                               ))),
                                           downloadButton('download_products_page3',
                                                          'Download this plot and the data')))))) 
      ),
      nav_panel(
        title = 'Information & Methods',
        icon = bsicons::bs_icon("text-paragraph"),
        navset_card_pill(
          nav_panel(title = 'Introduction',
                    htmlOutput('intro')),
          nav_panel(title = 'Data Collection',
                    htmlOutput('collection')),
          nav_panel(title = 'Data Sourcing',
                    htmlOutput('sourcing')),
          nav_panel(title = 'Data Management',
                    htmlOutput('management')),
          nav_panel(title = 'Regional Consolidation',
                    htmlOutput('coast')),
          nav_panel(title = 'Species Classification',
                    htmlOutput('classification')),
          nav_panel(title = 'Metrics',
                    htmlOutput('metrics')),
          nav_panel(title = 'Contact Us',
                    htmlOutput('contact')),
          nav_panel(title = 'Resources',
                    htmlOutput('resource'))
        )
      ),
      nav_panel(
        title = 'Coast Analysis',
        icon = bsicons::bs_icon("tsunami"),
        fluidRow(
          div(
            style = 'border: 3px solid #005761; border-radius: 12px;
               min-width: 800px; width: 100%; display: flex; flex-direction: column;',
            navset_card_pill(title = 'Trade',
                             nav_panel(title = 'Value',
                                       div(
                                         style = "position: relative; min-width: 1200px;",
                                         withSpinner(
                                           plotOutput('exp_coast_value',
                                                      click = clickOpts('exp_coast_value_click'),
                                                      height = "400px", width = "100%"), 
                                           type = 7),
                                         # textOutput('balance_tooltip'),
                                         uiOutput('exp_coast_value_click_overlay'),
                                         div(
                                           style = "position: absolute; top: 0px; left: 5px",
                                           tooltip(
                                             icon("info-circle"),
                                             "Export value reflects the total value of product traded out of the U.S. into other countries. The left y-axis reflects the total value of exports and applies to the bars. The right y-axis reflects the average price of exported product per kilogram or pound and applies to the line and points."))),
                                       div(
                                         style = "position: relative; min-width: 1200px;",
                                         withSpinner(
                                           plotOutput('imp_coast_value',
                                                      click = clickOpts('imp_coast_value_click'),
                                                      height = "400px", width = "100%"), 
                                           type = 7),
                                         # textOutput('balance_tooltip'),
                                         uiOutput('imp_coast_value_click_overlay'),
                                         div(
                                           style = "position: absolute; top: 0px; left: 5px",
                                           tooltip(
                                             icon("info-circle"),
                                             "Import value reflects the total value of product traded into the U.S. from other countries. The left y-axis reflects the total value of imports and applies to the bars. The right y-axis reflects the average price of imported product per kilogram or pound and applies to the line and points.")))),
                             nav_panel(title = 'Volume',
                                       div(
                                         style = "position: relative; min-width: 1200px;",
                                         withSpinner(
                                           plotOutput('exp_coast_volume',
                                                      click = clickOpts('exp_coast_volume_click'),
                                                      height = '400px', width = '100%'),
                                           type = 7),
                                         uiOutput('exp_coast_volume_click_overlay'),
                                         div(style = 'position: absolute; top: 0px; left: 5px',
                                             tooltip(
                                               icon('info-circle'),
                                               'Export volume reflects the total volume of product traded out of the U.S. into other countries.'))),
                                       div(
                                         style = 'position: relative; min-width: 1200px;',
                                         withSpinner(
                                           plotOutput('imp_coast_volume',
                                                      click = clickOpts('imp_coast_volume_click'),
                                                      height = '400px', width = '100%'),
                                           type = 7),
                                         uiOutput('imp_coast_volume_click_overlay'),
                                         div(
                                           style = 'position: absolute; top: 0px; left: 5px',
                                           tooltip(
                                             icon('info-circle'),
                                             'Import volume reflects the total volume of product traded into the U.S. from other countries.')))))
                             )),
        fluidRow(
          div(
            style = 'display: flex; gap: 15px; min-width: 800px; width: 100%;',
            div(
              style = 'border: 3px solid #234515; border-radius: 12px;
                 min-width: 400px; width: 100%; display: flex; flex-direction: column;',
              navset_card_pill(title = 'Commercial Landings',
                               nav_panel(title = 'Value',
                                         div(
                                           style = "position: relative; min-width: 600px; width: 100%",
                                           withSpinner(
                                             plotOutput('coast_landings_value',
                                                        click = clickOpts(id = 'coast_landings_value_plot_click'),
                                                        height = "500px"),
                                             type = 7),
                                           # textOutput('comvalue_tooltip')
                                           uiOutput('coast_landings_value_click_overlay'),
                                           div(
                                             style = "position: absolute; top: 0px; left: 5px",
                                             tooltip(
                                               icon("info-circle"),
                                               "Ex-vessel value reflects the amount paid to fishers for raw product (i.e., landed catch) in the U.S. The left y-axis reflects the total value of landed catch and applies to the bars. The right y-axis reflects the average price of landed catch per kilogram or pound and applies to the line and points."
                                             ))),
                                         downloadButton('download_landings_page1',
                                                        'Download this plot and the data')),
                               nav_panel(title = 'Volume',
                                         div(
                                           style = "position: relative; min-width: 600px; width: 100%",
                                           withSpinner(
                                             plotOutput('coast_landings_volume',
                                                        click = clickOpts(id = 'coast_landings_volume_plot_click'),
                                                        height = "500px"),
                                             type = 7),
                                           # textOutput('comvolume_tooltip')
                                           uiOutput('coast_landings_volume_click_overlay'),
                                           div(
                                             style = "position: absolute; top: 0px; left: 5px",
                                             tooltip(
                                               icon("info-circle"),
                                               "Ex-vessel volume reflects the weight of raw product landed by fishers in the U.S."
                                             ))),
                                         downloadButton('download_landings_page2',
                                                        'Download this plot and the data')))),
            div(
              style = 'border: 3px solid #681617; border-radius: 12px;
                 min-width: 400px; width: 100%; display: flex; flex-direction: column;',
              navset_card_pill(title = 'Processed Products',
                               nav_panel(title = 'Value',
                                         div(
                                           style = "position: relative; min-width: 600px; width: 100%",
                                           withSpinner(
                                             plotOutput('coast_pp_value',
                                                        click = clickOpts('coast_pp_value_plot_click'),
                                                        height = "500px"),
                                             type = 7),
                                           # textOutput('ppvalue_tooltip')
                                           uiOutput('coast_pp_value_click_overlay'),
                                           div(
                                             style = "position: absolute; top: 0px; left: 5px",
                                             tooltip(
                                               icon("info-circle"),
                                               "Processed products are divided by the condition of their processing (i.e., canned, fillets, surimi, etc.). The category Other* includes conditions marked as 'Other' as well as those that comprise 2% or less of total processed product value."
                                             ))),
                                         downloadButton('download_products_page1',
                                                        'Download this plot and the data')),
                               nav_panel(title = 'Volume',
                                         div(
                                           style = "position: relative; min-width: 600px; width: 100%",
                                           withSpinner(
                                             plotOutput('coast_pp_volume',
                                                        click = clickOpts('coast_pp_volume_plot_click'),
                                                        height = "500px"),
                                             type = 7),
                                           # textOutput('ppvolume_tooltip')
                                           uiOutput('coast_pp_volume_click_overlay'),
                                           div(
                                             style = "position: absolute; top: 0px; left: 5px",
                                             tooltip(
                                               icon("info-circle"),
                                               "Processed products are divided by the condition of their processing (i.e., canned, fillets, surimi, etc.). The category Other* includes conditions marked as 'Other' as well as those that comprise 2% or less of total processed product value."
                                             ))),
                                         downloadButton('download_products_page2',
                                                        'Download this plot and the data')),
                               nav_panel(title = 'Price',
                                         div(
                                           style = "position: relative; min-width: 600px; width: 100%",
                                           withSpinner(
                                             plotOutput('coast_pp_price',
                                                        click = clickOpts('coast_pp_price_plot_click'),
                                                        height = "500px"),
                                             type = 7),
                                           # textOutput('ppprice_tooltip')
                                           uiOutput('coast_pp_price_click_overlay'),
                                           div(
                                             style = "position: absolute; top: 0px; left: 5px",
                                             tooltip(
                                               icon("info-circle"),
                                               "Processed products are divided by the condition of their processing (i.e., canned, fillets, surimi, etc.). The category Other* includes conditions marked as 'Other' as well as those that comprise 2% or less of total processed product value."
                                             ))),
                                         downloadButton('download_products_page3',
                                                        'Download this plot and the data'))))))
        ))
        
      ))


# Define server logic ----------------------------------------------------------
server <- function(input, output, session) {
  # Reset Button ---------------------------------------------------------------
  observeEvent(input$reset_button, {
    updateSelectizeInput(session, 'search_term', selected = '')
    updateSelectInput(session, 'species_cat', selected = 'All Species')
    updateSelectInput(session, 'species_grp', selected = 'All Species')
    updateSelectInput(session, 'species_name', selected = 'All Species')
    updateSelectizeInput(session, 'coast', selected = '')
    updateCheckboxInput(session, 'trade_button', value = F)
    updateCheckboxInput(session, 'landings_button', value = F)
    updateCheckboxInput(session, 'products_button', value = F)
    update_switch(session = session, 'units', value = F)
    update_switch(session = session, 'nominal', value = F)
    balance_clicked_point(NULL)
    ratio_clicked_point(NULL)
    top5_clicked_point(NULL)
    exp_value_clicked_point(NULL)
    imp_value_clicked_point(NULL)
    exp_volume_clicked_point(NULL)
    imp_volume_clicked_point(NULL)
    exp_mlti_clicked_point(NULL)
    imp_mlti_clicked_point(NULL)
    hi_clicked_point(NULL)
    supply_clicked_point(NULL)
    supply_ratio_clicked_point(NULL)
    supply_share_clicked_point(NULL)
    landings_value_clicked_point(NULL)
    landings_volume_clicked_point(NULL)
    pp_value_clicked_point(NULL)
    pp_volume_clicked_point(NULL)
    pp_price_clicked_point(NULL)
    exp_coast_value_clicked_point(FALSE)
    imp_coast_value_clicked_point(FALSE)
    exp_coast_volume_clicked_point(FALSE)
    imp_coast_volume_clicked_point(FALSE)
    coast_landings_value_clicked_point(FALSE)
    coast_landings_volume_clicked_point(FALSE)
    coast_pp_value_clicked_point(FALSE)
    coast_pp_volume_clicked_point(FALSE)
    coast_pp_price_clicked_point(FALSE)
  })
  # Download buttons -----------------------------------------------------------
  # The following are a series of download buttons that provide the user the
  # ability to download pieces of the dashboard, organized by page and tab
  # For instance, the user can download landings price, or trade advanced metrics,
  # or processed products volume. Each button will download the plots associated
  # with that page as well as the data used to generate the figures
  
  # download the raw trade data
  output$download_trade <- downloadHandler(
    filename = 'trade_data.csv',
    content = function(con) {
      # Show Modal presents to the user that the download is happening
      showModal(modalDialog('Downloading data...', footer = NULL))
      on.exit(removeModal())
      
      write.csv(trade_data, con)
    }
  )
  
  # download the raw landings data
  output$download_landings <- downloadHandler(
    filename = 'landings_data.csv',
    content = function(con) {
      showModal(modalDialog('Downloading data...', footer = NULL))
      Sys.sleep(1)
      on.exit(removeModal())
      
      write.csv(landings, con)
    }
  )
  
  # download the raw products data
  output$download_products <- downloadHandler(
    filename = 'processed_products_data.csv',
    content = function(con) {
      showModal(modalDialog('Downloading data...', footer = NULL))
      # because the products data is so small, the download happens too quickly
      # for showModal to understand that the data is ready for download
      # without Sys.sleep, the modalDialog box will not go away and render the
      # dashboard unusable
      Sys.sleep(1)
      on.exit(removeModal())
      
      write.csv(pp_data, con)
    }
  )
  
  # download page 1 of trade data (Aggregate tab)
  output$download_page1 <- downloadHandler(
    filename = 'trade_aggregate_page.zip',
    content = function(fname) {
      showModal(modalDialog('Downloading plots and data...', footer = NULL))
      Sys.sleep(1)
      on.exit(removeModal())
      # fname is a placeholder, I'm not sure if it needs to be called that
      # the code was derived from stackoverflow answers
      
      # set up a temporary working directory for the data and plots to save to
      tmpdir <- tempdir()
      setwd(tempdir())
      
      # list of names that will be saved, these MUST match the csv's and
      # ggsave items listed below
      fs <- c('balance_plot.png', 'ratio_plot.png', 'top5_trade_plot.png',
              'trade_plots_data.csv', 'top5_trade_plot_data.csv')
      ggsave('balance_plot.png', balance_plot(),
             width = 15,
             height = 8,
             device = 'png')
      ggsave('ratio_plot.png', ratio_plot(),
             width = 10,
             height = 8,
             device = 'png')
      ggsave('top5_trade_plot.png', top5_trade_plot(),
             width = 10,
             height = 8,
             device = 'png')
      write.csv(trade_df_full(), 'trade_plots_data.csv')
      write.csv(top5_trade_df_full(), 'top5_trade_plot_data.csv')
      
      # we are saving multiple files so they must be in a zip file
      # fname is filename that derives from the start of the function
      zip(zipfile = fname, files = fs)
    },
    # specify the content saved is a zip
    contentType = 'application/zip'
  )
  
  # download page 2 of trade data (value)
  output$download_page2 <- downloadHandler(
    filename = 'trade_value_page.zip',
    content = function(fname) {
      showModal(modalDialog('Downloading plots and data...', footer = NULL))
      Sys.sleep(1)
      on.exit(removeModal())
      
      tmpdir <- tempdir()
      setwd(tempdir())
      
      fs <- c('export_value_plot.png', 'import_value_plot.png', 
              'trade_plots_data.csv')
      ggsave('export_value_plot.png', exp_value_plot(),
             width = 10,
             height = 8,
             device = 'png')
      ggsave('import_value_plot.png', imp_value_plot(),
             width = 10,
             height = 8,
             device = 'png')
      write.csv(trade_df_full(), 'trade_plots_data.csv')
      
      zip(zipfile = fname, files = fs)
    },
    contentType = 'application/zip'
  )
  
  # download page 3 of trade data (volume)
  output$download_page3 <- downloadHandler(
    filename = 'trade_volume_page.zip',
    content = function(fname) {
      showModal(modalDialog('Downloading plots and data...', footer = NULL))
      Sys.sleep(1)
      on.exit(removeModal())
      
      tmpdir <- tempdir()
      setwd(tempdir())
      
      fs <- c('export_volume_plot.png', 'import_volume_plot.png',
              'trade_plots_data.csv')
      ggsave('export_volume_plot.png', exp_volume_plot(),
             width = 10,
             height = 8,
             device = 'png')
      ggsave('import_volume_plot.png', imp_volume_plot(),
             width = 10,
             height = 8,
             device = 'png')
      write.csv(trade_df_full(), 'trade_plots_data.csv')
      
      zip(zipfile = fname, files = fs)
    },
    contentType = 'application/zip'
  )
  
  # download page 4 of trade data (advanced metrics)
  output$download_page4 <- downloadHandler(
    filename = 'trade_advanced_metrics_page.zip',
    content = function(fname) {
      showModal(modalDialog('Downloading plots and data...', footer = NULL))
      Sys.sleep(1)
      on.exit(removeModal())
      
      tmpdir <- tempdir()
      setwd(tempdir())
      
      fs <- c('export_MLTI_table.csv', 'import_MLTI_table.csv', 
              'supply_plots_data.csv', 'HI_plot.png', 'supply_plot.png', 
              'supply_production_ratio.png', 
              'unexported_production_supply_rate.png')
      write.csv(exp_mlti_table_df(), 'export_MLTI_table.csv')
      write.csv(imp_mlti_table_df(), 'import_MLTI_table.csv')
      write.csv(supply_df(), 'supply_plots_data.csv')
      ggsave('HI_plot.png', hi_plot(),
             width = 10,
             height = 8,
             device = 'png')
      ggsave('supply_plot.png', supply_plot(),
             width = 10,
             height = 8,
             device = 'png')
      ggsave('supply_production_ratio.png', supply_ratio_plot(),
             width = 10,
             height = 8,
             device = 'png')
      ggsave('unexported_production_supply_rate.png', supply_share_plot(),
             width = 10,
             height = 8,
             device = 'png')
      
      zip(zipfile = fname, files = fs)
    },
    contentType = 'application/zip'
  )
  
  # download page 1 of landings data (value)
  # initially this was meant to download all landings plots and its data,
  # however it is difficult to place the download icon such that it will not
  # disappear once the user selects different tabs (value, volume, etc.)
  output$download_landings_page1 <- downloadHandler(
    filename = 'commercial_landings_value.zip',
    content = function(fname) {
      showModal(modalDialog('Downloading plots and data...', footer = NULL))
      Sys.sleep(1)
      on.exit(removeModal())
      
      tmpdir <- tempdir()
      setwd(tempdir())
      
      fs <- c('commercial_landings_plots_data.csv', 'landings_value.png')
      write.csv(landings_df_full(), 'commercial_landings_plots_data.csv')
      ggsave('landings_value.png', landings_value_plot(),
             width = 10,
             height = 8,
             device = 'png')
      
      zip(zipfile = fname, files = fs)
    },
    contentType = 'application/zip'
  )
  
  # download page 2 of landings data (volume)
  output$download_landings_page2 <- downloadHandler(
    filename = 'commercial_landings_volume.zip',
    content = function(fname) {
      showModal(modalDialog('Downloading plots and data...', footer = NULL))
      Sys.sleep(1)
      on.exit(removeModal())
      
      tmpdir <- tempdir()
      setwd(tempdir())
      
      fs <- c('commercial_landings_plots_data.csv', 'landings_volume.png')
      write.csv(landings_df_full(), 'commercial_landings_plots_data.csv')
      ggsave('landings_volume.png', landings_volume_plot(),
             width = 10,
             height = 8,
             device = 'png')
      
      zip(zipfile = fname, files = fs)
    },
    contentType = 'application/zip'
  )
  
  # download page 3 of landings data (price)
  output$download_landings_page3 <- downloadHandler(
    filename = 'commercial_landings_price.zip',
    content = function(fname) {
      showModal(modalDialog('Downloading plots and data...', footer = NULL))
      Sys.sleep(1)
      on.exit(removeModal())
      
      tmpdir <- tempdir()
      setwd(tempdir())
      
      fs <- c('commercial_landings_plots_data.csv', 'landings_price.png')
      write.csv(landings_df_full(), 'commercial_landings_plots_data.csv')
      ggsave('landings_price.png', landings_price_plot(),
             width = 10,
             height = 8,
             device = 'png')
      
      zip(zipfile = fname, files = fs)
    },
    contentType = 'application/zip'
  )
  
  # download page 1 of processed products data (value)
  output$download_products_page1 <- downloadHandler(
    filename = 'processed_products_value.zip',
    content = function(fname) {
      showModal(modalDialog('Downloading plots and data...', footer = NULL))
      Sys.sleep(1)
      on.exit(removeModal())
      
      tmpdir <- tempdir()
      setwd(tempdir())
      
      fs <- c('products_plots_data.csv', 'products_value.png')
      write.csv(pp_df_full(), 'products_plots_data.csv')
      ggsave('products_value.png', pp_value_plot(),
             width = 10,
             height = 8,
             device = 'png')
      
      zip(zipfile = fname, files = fs)
    },
    contentType = 'application/zip'
  )
  
  # download page 2 of processed products data (volume)
  output$download_products_page2 <- downloadHandler(
    filename = 'processed_products_volume.zip',
    content = function(fname) {
      showModal(modalDialog('Downloading plots and data...', footer = NULL))
      Sys.sleep(1)
      on.exit(removeModal())
      
      tmpdir <- tempdir()
      setwd(tempdir())
      
      fs <- c('products_plots_data.csv', 'products_volume.png')
      write.csv(pp_df_full(), 'products_plots_data.csv')
      ggsave('products_volume.png', pp_volume_plot(),
             width = 10,
             height = 8,
             device = 'png')
      
      zip(zipfile = fname, files = fs)
    },
    contentType = 'application/zip'
  )
  
  # download page 3 of processed products data (price)
  output$download_products_page3 <- downloadHandler(
    filename = 'processed_products_price.zip',
    content = function(fname) {
      showModal(modalDialog('Downloading plots and data...', footer = NULL))
      Sys.sleep(1)
      on.exit(removeModal())
      
      tmpdir <- tempdir()
      setwd(tempdir())
      
      fs <- c('products_plots_data.csv', 'products_price.png')
      write.csv(pp_df_full(), 'products_plots_data.csv')
      ggsave('products_price.png', pp_price_plot(),
             width = 10,
             height = 8,
             device = 'png')
      
      zip(zipfile = fname, files = fs)
    },
    contentType = 'application/zip'
  )
  
  # species filter and search inputs -------------------------------------------
  # define search bar terms
  
  output$filter_0 <- renderUI({
    species_list <- c('', sort(c(categorization_matrix %>%
                                   filter_coast(input$coast) %>%
                                   select(SPECIES_NAME) %>%
                                   distinct() %>%
                                   filter(!is.na(SPECIES_NAME)) %>%
                                   mutate(SPECIES_NAME = 
                                            str_to_title(SPECIES_NAME)) %>%
                                   pull())))
    
    selectizeInput('search_term', 
                   'Search for a Species',
                   species_list,
                   options = list(
                     placeholder = 'Type here...'
                   ))
  })
  # updateSelectizeInput(session = session,
  #                      'search_term',
  #                      choices = 
  #                        c('', sort(c(categorization_matrix %>%
  #                                       select(SPECIES_NAME) %>%
  #                                       distinct() %>%
  #                                       filter(!is.na(SPECIES_NAME)) %>%
  #                                       mutate(SPECIES_NAME = 
  #                                                str_to_title(SPECIES_NAME)) %>%
  #                                       pull()))),
  #                      options = list(
  #                        placeholder = 'Type here...'
  #                      ),
  #                      server = T)
  
  search_cats <- reactive({
    categorization_matrix %>%
      filter(SPECIES_NAME == toupper(input$search_term)) %>%
      pivot_longer(c(SPECIES_NAME, SPECIES_GROUP, SPECIES_CATEGORY)) %>%
      select(value) %>%
      mutate(value = str_to_title(value)) %>%
      pull()
  }) 
  
  
  # creates input: species_cat
  output$filter_1 <- renderUI({
    species_cats <- c('All Species', categorization_matrix %>%
                        select(SPECIES_CATEGORY) %>%
                        distinct() %>%
                        # remove NA category
                        filter(!is.na(SPECIES_CATEGORY)) %>%
                        # display strings as titles (first letter capitalized)
                        mutate(SPECIES_CATEGORY = 
                                 str_to_title(SPECIES_CATEGORY)) %>%
                        pull())
    
    if (input$search_term == '') {
      selectInput('species_cat', 'Choose a Category', species_cats)
    } else {
      selectInput('species_cat', 'Choose a Category', species_cats,
                  selected = search_cats()[3])
    }
  })
  
  # creates input: species_grp
  # filter_2 appears once a species category (species_cat) is selected
  output$filter_2 <- renderUI({
    # req prevents anything from being run if species_cat is not specified
    req(input$species_cat != 'All Species')
    # grab all species groups for the selected species category
    species_groups <- c('All Species', categorization_matrix %>%
                          # filter for previous input to only show terms
                          # related to previous filtering
                          filter_species(input$species_cat) %>%
                          select(SPECIES_GROUP) %>%
                          distinct() %>%
                          # remove NA category
                          filter(!is.na(SPECIES_GROUP)) %>%
                          # display strings as titles (first letter capitalized)
                          mutate(SPECIES_GROUP =
                                   str_to_title(SPECIES_GROUP)) %>%
                          pull())
    
    if (input$search_term == '') {
      selectInput('species_grp', 'Choose a Group', species_groups)
    } else {
      selectInput('species_grp', 'Choose a Group', species_groups,
                  selected = search_cats()[2])
    }
  })
  
  # creates input: species_name
  # filter_3 appears once a species group (species_grp) is selected
  output$filter_3 <- renderUI({
    # req prevents anything from being run if species_cat and 
    # species_grp are not selected
    req(input$species_grp != 'All Species' & 
          input$species_cat != 'All Species')
    # grab all species names for the selected species group
    species_names <- c('All Species', categorization_matrix %>%
                         # filter for all previous inputs so presented terms 
                         # only reflect those within selected filters
                         filter_species(input$species_cat) %>%
                         filter_species(input$species_grp) %>%
                         select(SPECIES_NAME) %>%
                         distinct() %>%
                         # remove NA category
                         filter(!is.na(SPECIES_NAME)) %>%
                         # display strings as titles (first letter capitalized)
                         mutate(SPECIES_NAME = str_to_title(SPECIES_NAME)) %>%
                         pull())
    if (input$search_term == '') {
      selectInput('species_name', 'Choose a Species', species_names)
    } else {
      selectInput('species_name', 'Choose a Species', species_names,
                  selected = search_cats()[1])
    }
  })
  
  # creates checkbox to unfilter trade up one level
  # requires the selected species to NOT be available in trade categories
  # the validate prevents an error from being displayed in the side bar
  output$trade_unfilter_button <- renderUI({
    validate(need(try(!(species_selected() %in% trade_terms())),
                  ''))
    req(!(species_selected() %in% trade_terms()))
    
    checkboxInput('trade_button', 'Revert trade data to last available selection')
  })
  
  # creates checkbox to unfilter production up one level
  # requires the selected species to NOT be available in production categories
  # the validate prevents an error from being displayed in the side bar
  output$product_unfilter_button <- renderUI({
    validate(need(try(!(species_selected() %in% pp_terms())),
                  ''))
    req(!(species_selected() %in% pp_terms()))
    
    checkboxInput('products_button', 'Revert products data to last available selection')
  })
  
  # creates checkbox to unfilter landings up one level
  # requires the selected species to NOT be available in landings categories
  # the validate prevents an error from being displayed in the side bar
  output$landings_unfilter_button <- renderUI({
    validate(need(try(!(species_selected() %in% landings_terms())),
                  ''))
    req(!(species_selected() %in% landings_terms()))
    
    checkboxInput('landings_button', 'Revert landings data to last available selection')
  })
  
  # sets aside species selected by the user
  species_selected <- reactive({
    ifelse(input$species_cat == 'All Species', 'All Species',
           ifelse(input$species_grp == 'All Species', input$species_cat,
                  ifelse(input$species_name == 'All Species', input$species_grp,
                         input$species_name)))
  })
  
  # specify the units
  selected_units <- reactive({
    ifelse(input$units == F, 'METRIC', 'IMPERIAL')
  })
  
  selected_value <- reactive({
    ifelse(input$nominal == F, 'FALSE', 'TRUE')
  })
  
  # trade ----------------------------------------------------------------------
  
  # create list of trade categories based on selected filters
  trade_terms <- reactive({
    
    # determine which category the user selected last
    # is.null(input$ecol_cat) is the default because, despite the app displaying
    # 'All Species' on startup, the input is NULL because no selection has 
    # technically been made by the user. If the user later selects 'All Species'
    # for ecol_cat, then the input is not NULL
    # whichever category lists 'All Species', the prior category contains the
    # most recent specific selection
    cat_index <- 
      ifelse(is.null(input$species_cat) | input$species_cat == 'All Species', 'DEFAULT',
             ifelse(input$species_grp == 'All Species', 'scat',
                    ifelse(input$species_name == 'All Species', 'sgrp',
                           'sname')))
    
    # trade_terms() will return the list of terms for the selected category that
    # exist in the trade data
    # this is so that we can test to see if the selected term exists in the
    # trade data based on the selected filters
    # if the user has selected an ecological category, or if the ecological 
    # category is 'All Species', will return the list of ecological categories
    if(cat_index == 'scat' | cat_index == 'DEFAULT') {
      result <- c('All Species', trade_categorization_matrix %>%
                    select(SPECIES_CATEGORY) %>%
                    mutate(SPECIES_CATEGORY = str_to_title(SPECIES_CATEGORY)) %>%
                    pull())
    }
    
    # if a species category was selected, will return all species categories
    # for the selected ecological category in the trade data
    # We also need to include the previously selected ecological category,
    # but only if that category exists in the trade data
    if(cat_index == 'sgrp') {
      if(toupper(input$species_cat) %in% 
         trade_categorization_matrix$SPECIES_CATEGORY) {
        terms <- input$species_cat
      } else {
        # if the category does not exist, create empty vector for functionality
        terms <- vector()
      }
      
      # the output will include the ecological category term (if it exists) and
      # all species categories that exist within that ecological category
      # Should the e_cat NOT exist in the trade data, filter_species will 
      # return an empty data frame, thus trade_terms will be empty, and
      # 'All Species' will be returned later in unfilter_species_trade()
      result <- c(terms,
                  trade_categorization_matrix %>%
                    filter_species(input$species_cat) %>%
                    select(SPECIES_GROUP) %>%
                    mutate(SPECIES_GROUP = str_to_title(SPECIES_GROUP)) %>%
                    pull())
    }
    
    # if a species group was selected, will return all species groups for
    # that species category AND that ecological category in the trade data
    # We also need to include the previously selected ecological and species
    # categories but only if they exist in the trade data
    if(cat_index == 'sname') {
      if(toupper(input$species_grp) %in% (trade_categorization_matrix %>%
                                          filter_species(input$species_cat) %>%
                                          select(SPECIES_GROUP) %>%
                                          pull())) {
        terms <- c(input$species_cat, input$species_grp)
        # if they don't exist, then check if ecol_cat exists in the trade data
      } else if(toupper(input$species_cat) %in%
                trade_categorization_matrix$SPECIES_CATEGORY) {
        terms <- input$species_cat
        # if neither the selected species or e_cat terms exist, returns empty
        # vector
      } else {
        terms <- vector()
      }
      
      result <- c(terms, 
                  trade_categorization_matrix %>%
                    filter_species(input$species_cat) %>%
                    filter_species(input$species_grp) %>%
                    select(SPECIES_NAME) %>%
                    mutate(SPECIES_NAME = str_to_title(SPECIES_NAME)) %>%
                    pull())
    }
    
    # result will thus include any selected terms made by the user ONLY if those
    # terms exist within their prior filters, and all terms of the last 
    # selected input. This creates a vector of terms that subsequent reactive
    # expressions will search through to determine if the last selected input
    # exists in the trade data. This enables functionality of the unfilter 
    # button such that, when checked, it will default back to the last 
    # available term within this result vector
    result
  })
  
  # identifies which species is the next highest level based on if the 
  # selected category is not available from available trade data
  # operates by determining which level contains 'All Species'. For that level,
  # it will unfilter to the next two levels up (because, if species name is
  # 'All Species', species group was last selected, which means there is no
  # data for that species group, thus we must unfilter back to species cat,
  # which is two levels up from species name).
  # If species group is 'All Species', then there is no data for species cat, which
  # means we must unfilter back up to All Species (i.e. no filter)
  # If species name has a selection, then we only unfilter to species group, 
  # because there must have been data for that species group to have selected
  # a species name
  unfilter_species_trade <- reactive({
    req(input$trade_button == T)
    
    # determine what the last selected input was. We start from the earliest
    # possible filter (in this case, species_grp since if species_cat == 'All
    # Species' then no selection was made)
    if(input$species_grp == 'All Species') {
      # if species_grp == 'All Species', then the user has only inputted 
      # species_cat, which means we only have to check that input
      # trade_terms, as explained above, includes all previously selected terms
      # ONLY IF they exist in the trade data, and all available terms within
      # the previously selected filters 
      ifelse(input$species_cat %in% trade_terms(), input$species_cat,
             'All Species')
      # Subsequent ifelse terms work backwards from the last selected input
      # to determine which last selected term exists in the trade data based
      # on the selected filters. This is such that if a user selected down
      # to species name, but the last available trade data was from the 
      # selected species_cat term, then selecting the unfilter_button would
      # display the species_cat filtered data for trade plots
    } else if(input$species_name == 'All Species') {
      ifelse(input$species_grp %in% trade_terms(), input$species_grp,
             ifelse(input$species_cat %in% trade_terms(), input$species_cat,
                    'All Species'))
    } else if(input$species_name != 'All Species') {
      ifelse(input$species_name %in% trade_terms(), input$species_name,
             ifelse(input$species_grp %in% trade_terms(), input$species_grp,
                    ifelse(input$species_cat %in% trade_terms(), input$species_cat,
                           'All Species')))
    } 
    
    
  })
  
  # determines if the selected species OR the next highest level of categorization
  # (unfilter_species_trade) should be used for trade data visualization based
  # on whether the selected species (based on all selected filters) exists
  # in the trade data
  # because unfilter_species_trade requires the trade_button to be checked by
  # the user, this will only switch to unfilter_species_trade once the user
  # checks the box
  species_selection_trade <- reactive({
    ifelse(species_selected() %in% trade_terms(), species_selected(),
           unfilter_species_trade())
  })
  
  # progressive filter applied to data such that plots accurately reflect all
  # inputted species selections
  # operates with several if statements that evaluate which filters should be
  # applied to the data. There are instances where several of the conditions
  # will be met, thus filtering the data several times. This is fine, because
  # each if statement overwrites 'new_data', thus only the last met condition
  # will apply to the outputted data
  trade_filtered <- reactive({
    # first filter data for the selected species_cat
    # (this will work if 'all species' is selected (default) bc filter_species
    # accounts for that input)
    new_data <- trade_data %>%
      filter_species(input$species_cat)
    
    # It will apply the next inputted filter only if there is an input for that
    # filter AND if the selected filter is present in the trade data at that
    # level (this protects against the ifelse breaking)
    if(species_selection_trade() %in% 
       trade_categorization_matrix$SPECIES_GROUP &
       !(is.null(input$species_grp))) {
      
      # apply filter for both selected inputs
      new_data <- trade_data %>%
        filter_species(input$species_cat) %>%
        filter_species(input$species_grp)
      
      # if the trade button is inputted, replace data with only one filter
      # applied
      if(input$trade_button == T) {
        new_data <- trade_data %>%
          filter_species(input$species_cat)
      }
    }
    
    if(species_selection_trade() %in% 
       trade_categorization_matrix$SPECIES_NAME &
       !(is.null(input$species_name))) {
      
      new_data <- trade_data %>%
        filter_species(input$species_cat) %>%
        filter_species(input$species_grp) %>%
        filter_species(input$species_name)
      
      if(input$trade_button == T) {
        new_data <- trade_button %>%
          filter_species(input$species_cat) %>%
          filter_species(input$species_grp)
      }
    }
    
    new_data
  })
  
  # creates full trade data
  trade_df_full <- reactive({
    summarize_trade_yr_spp(
      trade_filtered(),
      species_selection_trade(),
      coast = '',
      'FULL')
  })
  
  # creates trade balance data
  balance_df <- reactive({
    summarize_trade_yr_spp(
      trade_filtered(),
      species_selection_trade(),
      input$coast,
      'BALANCE',
      units = selected_units(),
      nominal = selected_value())
  })
  
  # validation reactive; outputs message if species is not available in trade data
  trade_data_validation <- reactive({
    validate(need(try(species_selection_trade() %in% trade_terms()),
                  '      There is no available trade data for the selected species'))
  })
  
  # creates trade balance plot (value)
  balance_plot <- reactive({
    plot_trade(balance_df(), input$coast, 'BALANCE', 
               species = species_selection_trade(), nominal = selected_value())
  })
  
  # outputs trade balance plot (value)
  output$balance <- renderPlot({
    trade_data_validation()
    validate(need(try(!is.na(balance_plot())),
                  '      Data for this species is insufficient to produce this plot'))
    balance_plot()
  })
  
  # creates trade data for plots
  trade_df <- reactive({
    summarize_trade_yr_spp(
      trade_filtered(),
      species_selection_trade(),
      input$coast,
      'VALUE',
      units = selected_units(),
      nominal = selected_value())
  })
  
  # creates export/import ratio plot
  ratio_plot <- reactive({
    plot_trade(trade_df(), input$coast, 'RATIO', export = T, import = T, 
               species = species_selection_trade())
  })
  
  # outputs export/import ratio plot
  output$trade_ratio <- renderPlot({
    trade_data_validation()
    validate(need(try(!is.na(ratio_plot())),
                  '      Data for this species is insufficient to produce this plot'))
    ratio_plot()
  })
  
  # creates top 5 net export data for download
  top5_trade_df_full <- reactive({
    summarize_trade_ctry_yr_spp(
      trade_filtered(),
      species_selection_trade(),
      coast = '',
      output.format = 'FULL',
      time.frame = c(2020, 2024),
      nominal = selected_value())
  })
  
  # creates top 5 net export data for plots
  top5_trade_df <- reactive({
    summarize_trade_ctry_yr_spp(
      trade_filtered(),
      species_selection_trade(),
      input$coast,
      output.format = 'VALUE',
      time.frame = c(2020, 2024),
      nominal = selected_value())
  })
  
  # creates top 5 net export plot
  top5_trade_plot <- reactive({
    plot_trade_ctry_yr_spp(top5_trade_df(), 
                           species = species_selection_trade(), 
                           input$coast, nominal = selected_value())
  })
  
  # outputs top 5 net export plot
  output$top5_trade <- renderPlot({
    trade_data_validation()
    validate(need(try(!is.na(top5_trade_plot())),
                  '      Data for this species is insufficient to produce this plot'))
    top5_trade_plot()
  })
  
  # creates export value plot
  exp_value_plot <- reactive({
    plot_trade(trade_df(), input$coast, 'VALUE', units = selected_units(), export = T, 
               species = species_selection_trade(), nominal = selected_value())
  })
  
  # outputs export value plot
  output$exp_value <- renderPlot({
    trade_data_validation()
    validate(need(try(!is.na(exp_value_plot())),
                  '      Data for this species is insufficient to produce this plot'))
    exp_value_plot()
  })
  
  # creates import value plot
  imp_value_plot <- reactive({
    plot_trade(trade_df(), input$coast, 'VALUE', units = selected_units(), import = T, 
               species = species_selection_trade(), nominal = selected_value())
  })
  
  # outputs import value plot
  output$imp_value <- renderPlot({
    trade_data_validation()
    validate(need(try(!is.na(imp_value_plot())),
                  '      Data for this species is insufficient to produce this plot'))
    imp_value_plot()
  })
  
  # creates export volume plot
  exp_volume_plot <- reactive({
    plot_trade(trade_df(), input$coast, 'VOLUME', units = selected_units(), export = T, 
               species = species_selection_trade())
  })
  
  # outputs export volume plot
  output$exp_volume <- renderPlot({
    trade_data_validation()
    validate(need(try(!is.na(exp_volume_plot())),
                  '      Data for this species is insufficient to produce this plot'))
    exp_volume_plot()
  })
  
  # creates import volume plot
  imp_volume_plot <- reactive({
    plot_trade(trade_df(), input$coast, 'VOLUME', units = selected_units(), import = T, 
               species = species_selection_trade())
  })
  
  # outputs import volume plot
  output$imp_volume <- renderPlot({
    trade_data_validation()
    validate(need(try(!is.na(imp_volume_plot())),
                  '      Data for this species is insufficient to produce this plot'))
    imp_volume_plot()
  })
  
  # creates export price plot
  # exp_price_plot <- reactive({
  #   plot_trade(trade_df(), 'PRICE', units = selected_units(), export = T, 
  #              species = species_selection_trade())
  # })
  # 
  # # outputs export price plot
  # output$exp_price <- renderPlot({
  #   trade_data_validation()
  #   validate(need(try(!is.na(exp_price_plot())),
  #                 'Data for this species is insufficient to produce this plot'))
  #   exp_price_plot()
  # })
  # 
  # # creates import price plot
  # imp_price_plot <- reactive({
  #   plot_trade(trade_df(), 'PRICE', units = selected_units(), import = T, 
  #              species = species_selection_trade())
  # })
  # 
  # # outputs import price plot
  # output$imp_price <- renderPlot({
  #   trade_data_validation()
  #   validate(need(try(!is.na(imp_price_plot())),
  #                 'Data for this species is insufficient to produce this plot'))
  #   imp_price_plot()
  # })
  
  # landings -------------------------------------------------------------------
  
  # create list of landings categories based on selected filters
  # see trade_terms() notes 
  landings_terms <- reactive({
    
    cat_index <- 
      ifelse(is.null(input$species_cat) | input$species_cat == 'All Species', 'DEFAULT',
             ifelse(input$species_grp == 'All Species', 'scat',
                    ifelse(input$species_name == 'All Species', 'sgrp',
                           'sname')))
    
    if(cat_index == 'scat' | cat_index == 'DEFAULT') {
      result <- c('All Species', landings_categorization_matrix %>%
                    select(SPECIES_CATEGORY) %>%
                    mutate(SPECIES_CATEGORY = str_to_title(SPECIES_CATEGORY)) %>%
                    pull())
    }
    
    if(cat_index == 'sgrp') {
      if(toupper(input$species_cat) %in% 
         landings_categorization_matrix$SPECIES_CATEGORY) {
        terms <- input$species_cat
      } else {
        terms <- vector()
      }
      
      result <- c(terms,
                  landings_categorization_matrix %>%
                    filter_species(input$species_cat) %>%
                    select(SPECIES_GROUP) %>%
                    mutate(SPECIES_GROUP = str_to_title(SPECIES_GROUP)) %>%
                    pull())
    }
    
    if(cat_index == 'sname') {
      if(toupper(input$species_grp) %in% (landings_categorization_matrix %>%
                                          filter_species(input$species_cat) %>%
                                          select(SPECIES_GROUP) %>%
                                          pull())) {
        terms <- c(input$species_cat, input$species_grp)
      } else if(toupper(input$species_cat) %in%
                landings_categorization_matrix$SPECIES_CATEGORY) {
        terms <- input$species_cat
      } else {
        terms <- vector()
      }
      
      result <- c(terms, 
                  landings_categorization_matrix %>%
                    filter_species(input$species_cat) %>%
                    filter_species(input$species_grp) %>%
                    select(SPECIES_NAME) %>%
                    mutate(SPECIES_NAME = str_to_title(SPECIES_NAME)) %>%
                    pull())
    }
    
    result
  })
  
  # identifies which species is the next highest level based on if the 
  # selected category is not available from available landings data 
  # see notes above 'unfilter_species_trade'
  unfilter_species_landings <- reactive({
    req(input$landings_button == T)
    
    if(input$species_grp == 'All Species') {
      ifelse(input$species_cat %in% landings_terms(), input$species_cat,
             'All Species')
    } else if(input$species_name == 'All Species') {
      ifelse(input$species_grp %in% landings_terms(), input$species_grp,
             ifelse(input$species_cat %in% landings_terms(), input$species_cat,
                    'All Species'))
    } else if(input$species_name != 'All Species') {
      ifelse(input$species_name %in% landings_terms(), input$species_name,
             ifelse(input$species_grp %in% landings_terms(), input$species_grp,
                    ifelse(input$species_cat %in% landings_terms(), input$species_cat,
                           'All Species')))
    } 
  })
  
  # determines if the selected species OR the next highest level of categorization
  # (unfilter_species_landings) should be used for landings data visualization 
  # based on whether the selected species (and all selected filters) exists in
  # the landings data
  # because unfilter_species_landings requires the landings_button to be checked by
  # the user, this will only switch to unfilter_species_landings once the user
  # checks the box
  species_selection_landings <- reactive({
    ifelse(species_selected() %in% landings_terms(), species_selected(),
           unfilter_species_landings())
  })
  
  # see notes above and within trade_filtered
  landings_filtered <- reactive({
    new_data <- landings %>%
      filter_species(input$species_cat)
    
    if(species_selection_landings() %in% 
       landings_categorization_matrix$SPECIES_GROUP &
       !(is.null(input$species_grp))) {
      
      new_data <- landings %>%
        filter_species(input$species_cat) %>%
        filter_species(input$species_grp)
      
      if(input$landings_button == T) {
        new_data <- landings %>%
          filter_species(input$species_cat)
      }
    }
    
    if(species_selection_landings() %in% 
       landings_categorization_matrix$SPECIES_NAME &
       !(is.null(input$species_name))) {
      
      new_data <- landings %>%
        filter_species(input$species_cat) %>%
        filter_species(input$species_grp) %>%
        filter_species(input$species_name)
      
      if(input$landings_button == T) {
        new_data <- landings %>%
          filter_species(input$species_cat) %>%
          filter_species(input$species_grp)
      }
      
    }
    
    new_data
  })
  
  # validation reactive; displays message if species is not found in landings data
  landings_data_validation <- reactive({
    validate(need(try(species_selection_landings() %in% landings_terms()),
                  '      There is no available landings data for this species'))
  })
  
  # creates landings data for download
  landings_df_full <- reactive({
    summarize_landings_yr_spp(
      landings_filtered(),
      species_selection_landings(),
      coast = '',
      full_data = T)
  })
  
  # creates landings data for plots
  landings_df <- reactive({
    summarize_landings_yr_spp(
      landings_filtered(),
      species_selection_landings(),
      input$coast,
      units = selected_units(),
      nominal = selected_value())
  })
  
  # creates landings value plot
  landings_value_plot <- reactive({
    plot_landings(landings_df(), input$coast, 'VALUE', units = selected_units(),
                  species = species_selection_landings(),
                  nominal = selected_value())
  })
  
  # outputs landings value plot
  output$landings_value <- renderPlot({
    landings_data_validation()
    validate(need(try(!is.na(landings_value_plot())),
                  '      Data for this species is insufficient to produce this plot'))
    landings_value_plot()
  })
  
  # creates landings volume plot
  landings_volume_plot <- reactive({
    plot_landings(landings_df(), input$coast, 'VOLUME', units = selected_units(),
                  species = species_selection_landings())
  })
  
  # outputs landings volume plot
  output$landings_volume <- renderPlot({
    landings_data_validation()
    validate(need(try(!is.na(landings_volume_plot())),
                  '      Data for this species is insufficient to produce this plot'))
    landings_volume_plot()
  })
  
  # creates landings price plot
  landings_price_plot <- reactive({
    plot_landings(landings_df(), input$coast, 'PRICE', units = selected_units(), 
                  species = species_selection_landings())
  })
  
  # outputs landings price plot
  output$landings_price <- renderPlot({
    landings_data_validation()
    validate(need(try(!is.na(landings_price_plot())),
                  '      Data for this species is insufficient to produce this plot'))
    landings_price_plot()
  })
  
  # products -------------------------------------------------------------------
  
  # create list of production categories based on selected filters
  # see trade_terms() notes
  pp_terms <- reactive({
    
    cat_index <- 
      ifelse(is.null(input$species_cat) | input$species_cat == 'All Species', 'DEFAULT',
             ifelse(input$species_grp == 'All Species', 'scat',
                    ifelse(input$species_name == 'All Species', 'sgrp',
                           'sname')))
    
    if(cat_index == 'scat' | cat_index == 'DEFAULT') {
      result <- c('All Species', products_categorization_matrix %>%
                    select(SPECIES_CATEGORY) %>%
                    mutate(SPECIES_CATEGORY = str_to_title(SPECIES_CATEGORY)) %>%
                    pull())
    }
    
    if(cat_index == 'sgrp') {
      if(toupper(input$species_cat) %in% 
         products_categorization_matrix$SPECIES_CATEGORY) {
        terms <- input$species_cat
      } else {
        terms <- vector()
      }
      
      result <- c(terms,
                  products_categorization_matrix %>%
                    filter_species(input$species_cat) %>%
                    select(SPECIES_GROUP) %>%
                    mutate(SPECIES_GROUP = str_to_title(SPECIES_GROUP)) %>%
                    pull())
    }
    
    if(cat_index == 'sname') {
      if(toupper(input$species_name) %in% (products_categorization_matrix %>%
                                          filter_species(input$species_cat) %>%
                                          select(SPECIES_GROUP) %>%
                                          pull())) {
        terms <- c(input$species_cat, input$species_grp)
      } else if(toupper(input$species_cat) %in%
                products_categorization_matrix$SPECIES_CATEGORY) {
        terms <- input$species_cat
      } else {
        terms <- vector()
      }
      
      result <- c(terms, 
                  products_categorization_matrix %>%
                    filter_species(input$species_cat) %>%
                    filter_species(input$species_grp) %>%
                    select(SPECIES_NAME) %>%
                    mutate(SPECIES_NAME = str_to_title(SPECIES_NAME)) %>%
                    pull())
    }
    
    result
  })
  
  # identifies which species is the next highest level based on if the 
  # selected category is not available from available production data 
  # see notes above 'unfilter_species_trade'
  unfilter_species_products <- reactive({
    req(input$products_button == T)
    
    if(input$species_grp == 'All Species') {
      ifelse(input$species_cat %in% pp_terms(), input$species_cat,
             'All Species')
    } else if(input$species_name == 'All Species') {
      ifelse(input$species_grp %in% pp_terms(), input$species_grp,
             ifelse(input$species_cat %in% pp_terms(), input$species_cat,
                    'All Species'))
    } else if(input$species_name != 'All Species') {
      ifelse(input$species_name %in% pp_terms(), input$species_name,
             ifelse(input$species_grp %in% pp_terms(), input$species_grp,
                    ifelse(input$species_cat %in% pp_terms(), input$species_cat,
                           'All Species')))
    } 
  })
  
  # determines if the selected species OR the next highest level of categorization
  # (unfilter_species_products) should be used for production data visualization 
  # based on whether the selected species (and all selected filters) exists in
  # the production data
  # because unfilter_species_products requires the products_button to be checked by
  # the user, this will only switch to unfilter_species_products once the user
  # checks the box
  species_selection_products <- reactive({
    ifelse(species_selected() %in% pp_terms(), species_selected(),
           unfilter_species_products())
  })
  
  # see notes above and within trade_filtered
  products_filtered <- reactive({
    new_data <- pp_data %>%
      filter_species(input$species_cat)
    
    if(species_selection_products() %in% 
       products_categorization_matrix$SPECIES_GROUP &
       !(is.null(input$species_grp))) {
      
      new_data <- pp_data %>%
        filter_species(input$species_cat) %>%
        filter_species(input$species_grp)
      
      if(input$products_button == T) {
        new_data <- pp_data %>%
          filter_species(input$species_cat)
      }
    }
    
    if(species_selection_products() %in% 
       products_categorization_matrix$SPECIES_NAME &
       !(is.null(input$species_name))) {
      
      new_data <- pp_data %>%
        filter_species(input$species_cat) %>%
        filter_species(input$species_grp) %>%
        filter_species(input$species_name)
      
      if(input$products_button == T) {
        new_data <- pp_data %>%
          filter_species(input$species_cat) %>%
          filter_species(input$species_grp)
      }
    }
    
    new_data
  })
  
  # validation reactive; outputs message if species is not found in production data
  pp_data_validation <- reactive({
    validate(need(try(species_selection_products() %in% pp_terms()),
                  '      There is no available production data for this species'))
  })
  
  # creates processed products data for download
  pp_df_full <- reactive({
    summarize_pp_yr_spp(
      products_filtered(),
      species_selection_products(),
      coast = '',
      full_data = T)
  })
  
  # creates processed products data for plots
  pp_df <- reactive({
    summarize_pp_yr_spp(
      products_filtered(),
      species_selection_products(),
      input$coast,
      units = selected_units(),
      nominal = selected_value())
  })
  
  # creates processed products value plot
  pp_value_plot <- reactive({
    plot_spp_pp(pp_df(), input$coast, 'VALUE', 
                units = selected_units(),
                species = species_selection_products(),
                nominal = selected_value())
  })
  
  # outputs processed products value plot
  output$pp_value <- renderPlot({
    pp_data_validation()
    validate(need(try(!is.na(pp_value_plot())),
                  '      Data for this species is insufficient to produce this plot'))
    pp_value_plot()
  })
  
  # creates processed products volume plot
  pp_volume_plot <- reactive({
    plot_spp_pp(pp_df(), input$coast, 'VOLUME', 
                units = selected_units(),
                species = species_selection_products())
  })
  
  # outputs processed products volume plot
  output$pp_volume <- renderPlot({
    pp_data_validation()
    validate(need(try(!is.na(pp_volume_plot())),
                  '      Data for this species is insufficient to produce this plot'))
    pp_volume_plot()
  })
  
  # creates processed products price plot
  pp_price_plot <- reactive({
    plot_spp_pp(pp_df(), input$coast, 'PRICE', 
                units = selected_units(),
                species = species_selection_products(),
                nominal = selected_value())
  })
  
  # outputs processed products price plot
  output$pp_price <- renderPlot({
    pp_data_validation()
    validate(need(try(!is.na(pp_price_plot())),
                  '      Data for this species is insufficient to produce this plot'))
    pp_price_plot()
  })
  
  # advanced metrics -----------------------------------------------------------
  
  # creates MLTI export table
  exp_mlti_table_df <- reactive({
    calculate_mlti(species_selection_trade(), input$coast,
                   exports = T, nominal = selected_value())
  })
  
  # outputs MLTI export table
  output$exp_mlti_table <- renderTable({
    trade_data_validation()
    validate(need(try(!is.na(exp_mlti_table_df())),
                  '      Data for this species is insufficient to produce this table'))
    exp_mlti_table_df()
  })
  
  # creates MLTI export plot
  exp_mlti_plot <- reactive({
    plot_mlti(exp_mlti_table_df(), input$coast,
              exports = T, species = species_selection_trade())
  })
  
  # outputs MLTI export plot
  output$exp_mlti <- renderPlot({
    trade_data_validation()
    validate(need(try(!is.na(exp_mlti_plot())),
                  '      Data for this species is insufficient to produce this plot'))
    exp_mlti_plot()
  })
  
  # creates MLTI import table
  imp_mlti_table_df <- reactive({
    calculate_mlti(species_selection_trade(), input$coast,
                   imports = T, nominal = selected_value())
  })
  
  # outputs MLTI import table
  output$imp_mlti_table <- renderTable({
    trade_data_validation()
    validate(need(try(!is.na(imp_mlti_table_df())),
                  '      Data for this species is insufficient to produce this table'))
    imp_mlti_table_df()
  })
  
  # creates MLTI import plot
  imp_mlti_plot <- reactive({
    plot_mlti(imp_mlti_table_df(), input$coast,
              imports = T, species = species_selection_trade())
  })
  
  # outputs MLTI import plot
  output$imp_mlti <- renderPlot({
    trade_data_validation()
    validate(need(try(!is.na(imp_mlti_plot())),
                  '      Data for this species is insufficient to produce this plot'))
    imp_mlti_plot()
  })
  
  # creates HI plot
  hi_plot <- reactive({
    plot_hi(calculate_hi(species_selection_trade(), input$coast,
                         nominal = selected_value()), 
            input$coast, species = species_selection_trade())
  })
  
  # outputs HI plot
  output$hi <- renderPlot({
    trade_data_validation()
    validate(need(try(!is.na(hi_plot())),
                  '      Data for this species is insufficient to produce this table'))
    hi_plot()
  })
  
  # creates supply metric data
  supply_df <- reactive({
    calculate_supply_metrics(
      species_selection_trade(), input$coast, 
      units = selected_units(), nominal = selected_value())
  })
  
  # creates apparent supply plot
  supply_plot <- reactive({
    plot_supply_metrics(supply_df(), input$coast, 'SUPPLY', units = selected_units(),
                        species = species_selection_trade())
  })
  
  # outputs apparent supply plot
  output$supply <- renderPlot({
    trade_data_validation()
    validate(need(try(supply_plot()),
                  '      Data for this species is insufficient to produce this table'))
    supply_plot()
  })
  
  # creates apparent supply (ratio) plot
  supply_ratio_plot <- reactive({
    plot_supply_metrics(supply_df(), input$coast, 'RATIO', 
                        species = species_selection_trade())
  })
  
  # outputs apparent supply (ratio) plot
  output$supply_ratio <- renderPlot({
    trade_data_validation()
    validate(need(try(supply_ratio_plot()),
                  '      Data for this species is insufficient to produce this table'))
    supply_ratio_plot()
  })
  
  # creates apparent supply (share) plot
  supply_share_plot <- reactive({
    plot_supply_metrics(supply_df(), input$coast, 'SHARE', 
                        species = species_selection_trade())
  })
  
  # outputs apparent supply (share) plot
  output$supply_share <- renderPlot({
    trade_data_validation()
    validate(need(try(supply_share_plot()),
                  '      Data for this species is insufficient to produce this table'))
    supply_share_plot()
  })
  
  # Plot-clicking tooltips -----------------------------------------------------
  #' *Trade Balance Plot*
  # Stores data point information and pixel coordinates where user clicked
  balance_clicked_point <- reactiveVal(NULL)
  
  # Handle click events and store clicked point
  observeEvent(input$balance_plot_click, {
    
    click_x <- input$balance_plot_click$x
    
    balance_data <- balance_df()
    
    year_levels <- levels(factor(sort(unique(balance_data$YEAR))))
    clicked_year <- round(click_x)
    
    if (clicked_year >= 1 && clicked_year <= length(year_levels)) {
      clicked_factor_year <- year_levels[clicked_year]
      
      click_point <- balance_data[balance_data$YEAR == clicked_factor_year, ]
      
      balance_clicked_point(list(
        data = click_point,
        coords_css = input$balance_plot_click$coords_css))
    } else {
      balance_clicked_point(NULL)
    }
  })
  
  # allow user to exit out of tooltip
  observeEvent(input$close_balance_tooltip, {
    balance_clicked_point(NULL)
  })
  
  # overlay the clicked point
  output$balance_click_overlay <- renderUI({
    click_info <- balance_clicked_point()
    
    if (is.null(click_info)) {
      return(NULL)
    }
    
    left_pos <- click_info$coords_css$x + 10 # offset to right of point
    top_pos <- click_info$coords_css$y - 10 # offset above point
    
    left_pos <- max(10, left_pos)
    top_pos <- max(10, top_pos)
    
    div(
      style = paste0(
        "left: ", left_pos, "px;",
        "top: ", top_pos, "px;",
        tooltip_aes),
      # tooltip close button
      tags$button(
        "x",
        id = "close_balance_tooltip",
        onclick = "Shiny.setInputValue('close_balance_tooltip', Math.random());",
        style = close_button_aes),
      HTML(paste0(
        tooltip_heading, click_info$data$YEAR[1], "</span><br>",
        tooltip_color_icon(balance_colors[1]), tooltip_subheading, "Exports</span>:<br>", 
        dollar(click_info$data$VALUE_MILLIONS[1]), " Million <br>",
        tooltip_color_icon(balance_colors[2]), tooltip_subheading, "Imports</span>:<br>",
        dollar(click_info$data$VALUE_MILLIONS[2]), " Million <br>",
        tooltip_color_icon(balance_colors[3]), tooltip_subheading, "Trade Balance</span>:<br>",
        dollar(click_info$data$VALUE_MILLIONS[3]), " Million <br>"
      ))
    )
  })
  
  
  
  #' *Volume Ratio Plot*
  ratio_clicked_point <- reactiveVal(NULL)
  
  observeEvent(input$ratio_plot_click, {
    click_x <- input$ratio_plot_click$x
    
    ratio_data <- trade_df()
    
    year_levels <- levels(factor(sort(unique(ratio_data$YEAR))))
    clicked_year <- round(click_x)
    
    if (clicked_year >= 1 && clicked_year <= length(year_levels)) {
      clicked_factor_year <- year_levels[clicked_year]
      
      click_point <- ratio_data[ratio_data$YEAR == clicked_factor_year, ]
      
      ratio_clicked_point(list(
        data = click_point,
        coords_css = input$ratio_plot_click$coords_css))
    } else {
      ratio_clicked_point(NULL)
    }
  })
  
  observeEvent(input$close_ratio_tooltip, {
    ratio_clicked_point(NULL)
  })
  
  output$ratio_click_overlay <- renderUI({
    click_info <- ratio_clicked_point()
    
    if (is.null(click_info)) {
      return(NULL)
    }
    
    left_pos <- click_info$coords_css$x + 10
    top_pos <- click_info$coords_css$y - 10
    
    left_pos <- max(10, left_pos)
    top_pos <- max(10, top_pos)
    
    div(
      style = paste0(
        "left: ", left_pos, "px;",
        "top: ", top_pos, "px;",
        tooltip_aes),
      tags$button(
        "x",
        id = "close_ratio_tooltip",
        onclick = "Shiny.setInputValue('close_ratio_tooltip', Math.random());",
        style = close_button_aes),
      HTML(paste0(
        tooltip_heading, click_info$data$YEAR[1], "<br>",
        tooltip_subheading, "Export Volume</span>:<br>",  
        comma(click_info$data$EXP_VOLUME_T), ifelse(selected_units() == 'METRIC', 
                                                    " Metric Tons <br>", 
                                                    " Short Tons <br>"),
        tooltip_subheading, "Imports</span>:<br>",
        comma(click_info$data$IMP_VOLUME_T), ifelse(selected_units() == 'METRIC', 
                                                    " Metric Tons <br>", 
                                                    " Short Tons <br>"),
        tooltip_line_icon('black', 16), tooltip_subheading, "Ratio</span>:<br>",
        round(click_info$data$RATIO, digits = 3))))
  })
  
  
  
  #' *Top 5 Trade Plot*
  top5_clicked_point <- reactiveVal(NULL)
  
  observeEvent(input$top5_plot_click, {
    click_x <- round(input$top5_plot_click$x)
    
    top5_data <- top5_trade_df()
    top5_data <- top5_data %>%
      mutate(COUNTRY_NAME = gsub(' ', '\n', str_to_title(COUNTRY_NAME)))
    
    country_levels <- factor(sort(unique(top5_data$COUNTRY_NAME)))
    clicked_country <- click_x
    
    if (click_x >= 1 && click_x <= 5) {
      clicked_factor_country <- country_levels[clicked_country]
      
      click_point <- top5_data[top5_data$COUNTRY_NAME == clicked_factor_country, ]
      
      top5_clicked_point(list(
        data = click_point,
        coords_css = input$top5_plot_click$coords_css))
    } else {
      top5_clicked_point(NULL)
    }
  })
  
  observeEvent(input$close_top5_tooltip, {
    top5_clicked_point(NULL)
  })
  
  output$top5_click_overlay <- renderUI({
    click_info <- top5_clicked_point()
    
    if (is.null(click_info)) {
      return(NULL)
    }
    
    left_pos <- click_info$coords_css$x + 10
    top_pos <- click_info$coords_css$y - 10
    
    left_pos <- max(10, left_pos)
    top_pos <- max(10, top_pos)
    
    div(
      style = paste0(
        "left: ", left_pos, "px;",
        "top: ", top_pos, "px;",
        tooltip_aes),
      tags$button(
        "x",
        id = "close_top5_tooltip",
        onclick = "Shiny.setInputValue('close_top5_tooltip', Math.random());",
        style = close_button_aes),
      HTML(paste0(
        tooltip_heading, 
        click_info$data$COUNTRY_NAME[1], "<br>",
        tooltip_color_icon(top5_colors[1]), tooltip_subheading, "2020</span>: ",
        dollar(click_info$data$NET_VALUE_MILLIONS[1]), " Million <br>",
        tooltip_color_icon(top5_colors[2]), tooltip_subheading, "2021</span>: ",
        dollar(click_info$data$NET_VALUE_MILLIONS[2]), " Million <br>",
        tooltip_color_icon(top5_colors[3]), tooltip_subheading, "2022</span>: ",
        dollar(click_info$data$NET_VALUE_MILLIONS[3]), " Million <br>",
        tooltip_color_icon(top5_colors[4]), tooltip_subheading, "2023</span>: ",
        dollar(click_info$data$NET_VALUE_MILLIONS[4]), " Million <br>",
        tooltip_color_icon(top5_colors[5]), tooltip_subheading, "2024</span>: ",
        dollar(click_info$data$NET_VALUE_MILLIONS[5]), " Million <br>")))
  })
  
  
  
  #' *Export Value*
  exp_value_clicked_point <- reactiveVal(NULL)
  
  observeEvent(input$exp_value_plot_click, {
    click_x <- input$exp_value_plot_click$x
    
    # store current data
    trade_data <- trade_df()
    
    # require clicked year to be within range
    year_levels <- levels(factor(trade_data$YEAR))
    clicked_year <- round(click_x)
    
    if (clicked_year >= 1 && clicked_year <= length(year_levels)) {
      # Get actual factor value
      clicked_factor_year <- year_levels[clicked_year]
      
      # Get data for clicked bar
      click_point <- trade_data[trade_data$YEAR == clicked_factor_year, ]
      
      # store coords of clicked point
      exp_value_clicked_point(list(
        data = click_point,
        coords_css = input$exp_value_plot_click$coords_css))
    } else {
      exp_value_clicked_point(NULL)
    }
  })
  
  observeEvent(input$close_exp_value_tooltip, {
    exp_value_clicked_point(NULL)
  })
  
  output$exp_value_click_overlay <- renderUI({
    click_info <- exp_value_clicked_point()
    
    if (is.null(click_info)) {
      return(NULL)
    }
    
    # position tooltip near clicked point
    left_pos <- click_info$coords_css$x + 10
    top_pos <- click_info$coords_css$y - 10
    
    # keep tooltip within plot bounds
    
    left_pos <- max(10, left_pos)
    top_pos <- max(10, top_pos)
    
    # tooltip style
    div(
      style = paste0(
        "left: ", left_pos, "px;",
        "top: ", top_pos, "px;",
        tooltip_aes
      ),
      
      # Close button
      tags$button(
        "x",
        id = "close_exp_value_tooltip",
        onclick = "Shiny.setInputValue('close_exp_value_tooltip', Math.random());",
        style = close_button_aes
      ), 
      
      # Tooltip info
      HTML(paste0(
        tooltip_heading, click_info$data$YEAR, "<br>",
        tooltip_color_icon(export_color), tooltip_subheading, "Export Value</span>:<br>",
        dollar(click_info$data$EXP_VALUE_MILLIONS), " Million<br>",
        tooltip_line_icon(trade_price_color, 16), tooltip_subheading, "Export Price</span>:<br>",
        dollar(click_info$data$EXP_PRICE), ifelse(selected_units() == 'METRIC', 
                                                  " per kilogram", 
                                                  " per pound"))))
  })
  
  
  
  #' *Import Value*
  imp_value_clicked_point <- reactiveVal(NULL)
  
  observeEvent(input$imp_value_plot_click, {
    click_x <- input$imp_value_plot_click$x
    
    trade_data <- trade_df()
    
    year_levels <- levels(factor(trade_data$YEAR))
    clicked_year <- round(click_x)
    
    if (clicked_year >= 1 && clicked_year <= length(year_levels)) {
      clicked_factor_year <- year_levels[clicked_year]
      
      click_point <- trade_data[trade_data$YEAR == clicked_factor_year, ]
      
      imp_value_clicked_point(list(
        data = click_point,
        coords_css = input$imp_value_plot_click$coords_css))
    } else {
      imp_value_clicked_point(NULL)
    }
  })
  
  observeEvent(input$close_imp_value_tooltip, {
    imp_value_clicked_point(NULL)
  })
  
  output$imp_value_click_overlay <- renderUI({
    click_info <- imp_value_clicked_point()
    
    if (is.null(click_info)) {
      return(NULL)
    }
    
    left_pos <- click_info$coords_css$x + 10
    top_pos <- click_info$coords_css$y - 10
    
    left_pos <- max(10, left_pos)
    top_pos <- max(10, top_pos)
    
    div(
      style = paste0(
        "left: ", left_pos, "px;",
        "top: ", top_pos, "px;",
        tooltip_aes),
      
      tags$button(
        "x",
        id = "close_imp_value_tooltip",
        onclick = "Shiny.setInputValue('close_imp_value_tooltip', Math.random());",
        style = close_button_aes),
      
      HTML(paste0(
        tooltip_heading, click_info$data$YEAR, "<br>",
        tooltip_color_icon(import_color), tooltip_subheading, "Import Value</span>:<br>",
        dollar(click_info$data$IMP_VALUE_MILLIONS), " Million<br>",
        tooltip_line_icon(trade_price_color, 16), tooltip_subheading, "Import Price</span>:<br>",
        dollar(click_info$data$IMP_PRICE), ifelse(selected_units() == 'METRIC', 
                                                  " per kilogram", 
                                                  " per pound"))))
  })
  
  
  
  #' *Export Volume*
  exp_volume_clicked_point <- reactiveVal(NULL)
  
  observeEvent(input$exp_volume_plot_click, {
    click_x <- input$exp_volume_plot_click$x
    
    # store current data
    trade_data <- trade_df()
    
    # require clicked year to be within range
    year_levels <- levels(factor(trade_data$YEAR))
    clicked_year <- round(click_x)
    
    if (clicked_year >= 1 && clicked_year <= length(year_levels)) {
      # Get actual factor value
      clicked_factor_year <- year_levels[clicked_year]
      
      # Get data for clicked bar
      click_point <- trade_data[trade_data$YEAR == clicked_factor_year, ]
      
      # store coords of clicked point
      exp_volume_clicked_point(list(
        data = click_point,
        coords_css = input$exp_volume_plot_click$coords_css))
    } else {
      exp_volume_clicked_point(NULL)
    }
  })
  
  observeEvent(input$close_exp_volume_tooltip, {
    exp_volume_clicked_point(NULL)
  })
  
  output$exp_volume_click_overlay <- renderUI({
    click_info <- exp_volume_clicked_point()
    
    if (is.null(click_info)) {
      return(NULL)
    }
    
    # position tooltip near clicked point
    left_pos <- click_info$coords_css$x + 10
    top_pos <- click_info$coords_css$y - 10
    
    # keep tooltip within plot bounds
    
    left_pos <- max(10, left_pos)
    top_pos <- max(10, top_pos)
    
    # tooltip style
    div(
      style = paste0(
        "left: ", left_pos, "px;",
        "top: ", top_pos, "px;",
        tooltip_aes
      ),
      
      # Close button
      tags$button(
        "x",
        id = "close_exp_volume_tooltip",
        onclick = "Shiny.setInputValue('close_exp_volume_tooltip', Math.random());",
        style = close_button_aes
      ), 
      
      # Tooltip info
      HTML(paste0(
        tooltip_heading, click_info$data$YEAR, "<br>",
        tooltip_color_icon(export_color), tooltip_subheading, "Export Volume</span>:<br>",
        comma(click_info$data$EXP_VOLUME_T), ifelse(selected_units() == 'METRIC', 
                                                    " Metric Tons", 
                                                    " Short Tons"))))
  })
  
  
  
  #' *Import Volume*
  imp_volume_clicked_point <- reactiveVal(NULL)
  
  observeEvent(input$imp_volume_plot_click, {
    click_x <- input$imp_volume_plot_click$x
    
    # store current data
    trade_data <- trade_df()
    
    # require clicked year to be within range
    year_levels <- levels(factor(trade_data$YEAR))
    clicked_year <- round(click_x)
    
    if (clicked_year >= 1 && clicked_year <= length(year_levels)) {
      # Get actual factor value
      clicked_factor_year <- year_levels[clicked_year]
      
      # Get data for clicked bar
      click_point <- trade_data[trade_data$YEAR == clicked_factor_year, ]
      
      # store coords of clicked point
      imp_volume_clicked_point(list(
        data = click_point,
        coords_css = input$imp_volume_plot_click$coords_css))
    } else {
      imp_volume_clicked_point(NULL)
    }
  })
  
  observeEvent(input$close_imp_volume_tooltip, {
    imp_volume_clicked_point(NULL)
  })
  
  output$imp_volume_click_overlay <- renderUI({
    click_info <- imp_volume_clicked_point()
    
    if (is.null(click_info)) {
      return(NULL)
    }
    
    # position tooltip near clicked point
    left_pos <- click_info$coords_css$x + 10
    top_pos <- click_info$coords_css$y - 10
    
    # keep tooltip within plot bounds
    
    left_pos <- max(10, left_pos)
    top_pos <- max(10, top_pos)
    
    # tooltip style
    div(
      style = paste0(
        "left: ", left_pos, "px;",
        "top: ", top_pos, "px;",
        tooltip_aes
      ),
      
      # Close button
      tags$button(
        "x",
        id = "close_imp_volume_tooltip",
        onclick = "Shiny.setInputValue('close_imp_volume_tooltip', Math.random());",
        style = close_button_aes
      ), 
      
      # Tooltip info
      HTML(paste0(
        tooltip_heading, click_info$data$YEAR, "<br>",
        tooltip_color_icon(import_color), tooltip_subheading, "Import Volume</span>:<br>",
        comma(click_info$data$IMP_VOLUME_T), ifelse(selected_units() == 'METRIC', 
                                                    " Metric Tons", 
                                                    " Short Tons"))))
  })
  
  
  
  #' *MLTI Exports*
  # Stores data point information and pixel coordinates where user clicked
  exp_mlti_clicked_point <- reactiveVal(NULL)
  
  # Handle click events and store clicked point
  observeEvent(input$exp_mlti_plot_click, {
    
    click_x <- input$exp_mlti_plot_click$x
    
    mlti_data <- exp_mlti_table_df()
    
    year_levels <- levels(factor(sort(unique(mlti_data$YEAR))))
    clicked_year <- round(click_x)
    
    # require clicked year to be within range
    if (clicked_year >= 1 && clicked_year <= length(year_levels)) {
      clicked_factor_year <- year_levels[clicked_year]
      
      click_point <- mlti_data[mlti_data$YEAR == clicked_factor_year, ]
      
      exp_mlti_clicked_point(list(
        data = click_point,
        coords_css = input$exp_mlti_plot_click$coords_css))
    } else {
      exp_mlti_clicked_point(NULL)
    }
  })
  
  # allow user to exit out of tooltip
  observeEvent(input$close_exp_mlti_tooltip, {
    exp_mlti_clicked_point(NULL)
  })
  
  # overlay the clicked point
  output$exp_mlti_click_overlay <- renderUI({
    click_info <- exp_mlti_clicked_point()
    
    if (is.null(click_info)) {
      return(NULL)
    }
    
    # SPECIFY POINT SHAPES
    # First, get data
    mlti_data <- exp_mlti_table_df()
    # Next, get countries
    countries <- unique(mlti_data$COUNTRY_NAME)
    # Assign the shapes to these countries
    # the countries are already sorted alphabetically in data
    point_shapes <- c(16, 17, 15, 3, 7)
    names(point_shapes) <- c(countries)
    # set names for mlti_colors
    new_mlti_colors <- mlti_colors
    names(new_mlti_colors) <- c(countries)
    # create all 5 icons
    icon_1 <- tooltip_line_icon(new_mlti_colors[1], point_shapes[1])
    icon_2 <- tooltip_line_icon(new_mlti_colors[2], point_shapes[2])
    icon_3 <- tooltip_line_icon(new_mlti_colors[3], point_shapes[3])
    icon_4 <- tooltip_line_icon(new_mlti_colors[4], point_shapes[4])
    icon_5 <- tooltip_line_icon(new_mlti_colors[5], point_shapes[5])
    
    # Position tooltip near clicked point
    left_pos <- click_info$coords_css$x + 10 # Offset to right of point
    top_pos <- click_info$coords_css$y - 10 # Offset above point
    
    # Prevent tooltip from being off screen
    left_pos <- max(10, left_pos)
    top_pos <- max(10, top_pos)
    
    # style the tooltip for clicked point
    div(
      style = paste0(
        "left: ", left_pos, "px;",
        "top: ", top_pos, "px;",
        tooltip_aes),
      # tooltip close button
      tags$button(
        "x",
        id = 'close_exp_mlti_tooltip',
        onclick = "Shiny.setInputValue('close_exp_mlti_tooltip', Math.random());",
        style = close_button_aes), 
      HTML(paste0(
        tooltip_heading, 
        click_info$data$YEAR[1], "<br>",
        icon_1, tooltip_subheading, str_to_title(click_info$data$COUNTRY_NAME[1]), 
        "</span>: ", round(click_info$data$MLTI[1], digits = 3), "<br>",
        icon_2, tooltip_subheading, str_to_title(click_info$data$COUNTRY_NAME[2]), 
        "</span>: ", round(click_info$data$MLTI[2], digits = 3), "<br>",
        icon_3, tooltip_subheading, str_to_title(click_info$data$COUNTRY_NAME[3]), 
        "</span>: ", round(click_info$data$MLTI[3], digits = 3), "<br>",
        icon_4, tooltip_subheading, str_to_title(click_info$data$COUNTRY_NAME[4]), 
        "</span>: ", round(click_info$data$MLTI[4], digits = 3), "<br>",
        icon_5, tooltip_subheading, str_to_title(click_info$data$COUNTRY_NAME[5]), 
        "</span>: ", round(click_info$data$MLTI[5], digits = 3)
      ))
    )
  })
  
  
  
  #' *MLTI Imports*
  imp_mlti_clicked_point <- reactiveVal(NULL)
  
  observeEvent(input$imp_mlti_plot_click, {
    
    click_x <- input$imp_mlti_plot_click$x
    
    mlti_data <- imp_mlti_table_df()
    
    year_levels <- levels(factor(sort(unique(mlti_data$YEAR))))
    clicked_year <- round(click_x)
    
    if (clicked_year >= 1 && clicked_year <= length(year_levels)) {
      clicked_factor_year <- year_levels[clicked_year]
      
      click_point <- mlti_data[mlti_data$YEAR == clicked_factor_year, ]
      
      imp_mlti_clicked_point(list(
        data = click_point,
        coords_css = input$imp_mlti_plot_click$coords_css))
    } else {
      imp_mlti_clicked_point(NULL)
    }
  })
  
  observeEvent(input$close_imp_mlti_tooltip, {
    imp_mlti_clicked_point(NULL)
  })
  
  output$imp_mlti_click_overlay <- renderUI({
    click_info <- imp_mlti_clicked_point()
    
    if(is.null(click_info)) {
      return(NULL)
    }
    
    # SPECIFY POINT SHAPES
    mlti_data <- imp_mlti_table_df()
    
    countries <- unique(mlti_data$COUNTRY_NAME)
    
    point_shapes <- c(16, 17, 15, 3, 7)
    names(point_shapes) <- c(countries)
    
    new_mlti_colors <- mlti_colors
    names(new_mlti_colors) <- c(countries)
    
    icon_1 <- tooltip_line_icon(new_mlti_colors[1], point_shapes[1])
    icon_2 <- tooltip_line_icon(new_mlti_colors[2], point_shapes[2])
    icon_3 <- tooltip_line_icon(new_mlti_colors[3], point_shapes[3])
    icon_4 <- tooltip_line_icon(new_mlti_colors[4], point_shapes[4])
    icon_5 <- tooltip_line_icon(new_mlti_colors[5], point_shapes[5])
    
    
    left_pos <- click_info$coords_css$x + 10 # Offset to right of point
    top_pos <- click_info$coords_css$y - 10 # Offset above point
    
    left_pos <- max(10, left_pos)
    top_pos <- max(10, top_pos)
    
    
    div(
      style = paste0(
        "left: ", left_pos, "px;",
        "top: ", top_pos, "px;",
        tooltip_aes),
      # tooltip close button
      tags$button(
        "x",
        id = 'close_imp_mlti_tooltip',
        onclick = "Shiny.setInputValue('close_imp_mlti_tooltip', Math.random());",
        style = close_button_aes
      ), 
      HTML(paste0(
        tooltip_heading, click_info$data$YEAR[1], "<br>",
        icon_1, tooltip_subheading, str_to_title(click_info$data$COUNTRY_NAME[1]), 
        "</span>: ", round(click_info$data$MLTI[1], digits = 3), "<br>",
        icon_2, tooltip_subheading, str_to_title(click_info$data$COUNTRY_NAME[2]), 
        "</span>: ", round(click_info$data$MLTI[2], digits = 3), "<br>",
        icon_3, tooltip_subheading, str_to_title(click_info$data$COUNTRY_NAME[3]), 
        "</span>: ", round(click_info$data$MLTI[3], digits = 3), "<br>",
        icon_4, tooltip_subheading, str_to_title(click_info$data$COUNTRY_NAME[4]), 
        "</span>: ", round(click_info$data$MLTI[4], digits = 3), "<br>",
        icon_5, tooltip_subheading, str_to_title(click_info$data$COUNTRY_NAME[5]), 
        "</span>: ", round(click_info$data$MLTI[5], digits = 3)
      ))
    )
  })
  
  
  
  #' *Herfindahl Index*
  hi_clicked_point <- reactiveVal(NULL)
  
  observeEvent(input$hi_plot_click, {
    click_x <- input$hi_plot_click$x
    
    hi_data <- calculate_hi(species_selection_trade(), nominal = selected_value())
    
    year_levels <- levels(factor(sort(unique(hi_data$YEAR))))
    clicked_year <- round(click_x)
    
    if (clicked_year >= 1 && clicked_year <= length(year_levels)) {
      clicked_factor_year <- year_levels[clicked_year]
      
      click_point <- hi_data[hi_data$YEAR == clicked_factor_year, ]
      
      hi_clicked_point(list(
        data = click_point,
        coords_css = input$hi_plot_click$coords_css))
    } else {
      hi_clicked_point(NULL)
    }
  })
  
  observeEvent(input$close_hi_tooltip, {
    hi_clicked_point(NULL)
  })
  
  output$hi_click_overlay <- renderUI({
    click_info <- hi_clicked_point()
    
    if (is.null(click_info)) {
      return(NULL)
    }
    
    # create legend icons
    exp_icon <- tooltip_line_icon('#003087', 16)
    imp_icon <- tooltip_line_icon('#0085CA', 16)
    
    left_pos <- click_info$coords_css$x + 10 # Offset to right of point
    top_pos <- click_info$coords_css$y - 10 # Offset above point
    
    left_pos <- max(10, left_pos)
    top_pos <- max(10, top_pos)
    
    div(
      style = paste0(
        "left: ", left_pos, "px;",
        "top: ", top_pos, "px;",
        tooltip_aes),
      tags$button(
        "x",
        id = 'close_hi_tooltip',
        onclick = "Shiny.setInputValue('close_hi_tooltip', Math.random());",
        style = close_button_aes), 
      HTML(paste0(
        tooltip_heading, click_info$data$YEAR[1], "<br>",
        exp_icon, tooltip_subheading, "Exports</span>: ", round(click_info$data$EXP_HI, digits = 3), "<br>",
        imp_icon, tooltip_subheading, "Imports</span>: ", round(click_info$data$IMP_HI, digits = 3)
      ))
    )
  })
  
  
  
  #' *Apparent Supply*
  supply_clicked_point <- reactiveVal(NULL)
  
  observe({
    if (!is.null(input$supply_plot_click)) {
      # Get clicked coordinates (bc it's a bar chart, different from exp_mlti)
      click_x <- input$supply_plot_click$x
      click_y <- input$supply_plot_click$y
      
      # Store current data
      supply_data <- supply_df()
      
      # Factored x-axis for years requires extra wrangling
      year_levels <- levels(factor(supply_data$YEAR))
      clicked_year <- round(click_x)
      
      # require clicked year to be within range
      if (clicked_year >= 1 && clicked_year <= length(year_levels)) {
        # Get actual factor value
        clicked_factor_year <- year_levels[clicked_year]
        
        # Get data for clicked bar
        click_point <- supply_data[supply_data$YEAR == clicked_factor_year, ]
        
        # tooltip only shows when bar is clicked (y < supply)
        if (click_y >= 0 && click_y <= click_point$APPARENT_SUPPLY) {
          # store coords of clicked point
          supply_clicked_point(list(
            data = click_point,
            coords_css = input$supply_plot_click$coords_css
          ))
        } else {
          supply_clicked_point(NULL)
        }
      } else {
        supply_clicked_point(NULL)
      }
    }
  })
  
  observeEvent(input$close_supply_tooltip, {
    supply_clicked_point(NULL)
  })
  
  output$supply_click_overlay <- renderUI({
    click_info <- supply_clicked_point()
    
    if (is.null(click_info)) {
      return(NULL)
    }
    
    # position tooltip near clicked point
    left_pos <- click_info$coords_css$x + 10
    top_pos <- click_info$coords_css$y - 10
    
    # keep tooltip within plot bounds
    left_pos <- max(10, left_pos)
    top_pos <- max(10, top_pos)
    
    # tooltip style
    div(
      style = paste0(
        "left: ", left_pos, "px;",
        "top: ", top_pos, "px;",
        tooltip_aes),
      
      # Close button
      tags$button(
        "x",
        id = "close_supply_tooltip",
        onclick = "Shiny.setInputValue('close_supply_tooltip', Math.random());",
        style = close_button_aes), 
      
      # Tooltip info
      HTML(paste0(
        tooltip_heading, click_info$data$YEAR, "<br>",
        tooltip_color_icon(supply_color), tooltip_subheading, "Apparent Supply</span>: ",
        comma(click_info$data$APPARENT_SUPPLY), ifelse(input$units == F, 
                                                       " Metric Tons", 
                                                       " Short Tons")
      ))
    )
  })
  
  
  
  #' *Supply Ratio*
  supply_ratio_clicked_point <- reactiveVal(NULL)
  
  observeEvent(input$supply_ratio_plot_click, {
    click_x <- input$supply_ratio_plot_click$x
    
    supply_data <- supply_df()
    
    year_levels <- levels(factor(sort(unique(supply_data$YEAR))))
    clicked_year <- round(click_x)
    
    if (clicked_year >= 1 && clicked_year <= length(year_levels)) {
      clicked_factor_year <- year_levels[clicked_year]
      
      click_point <- supply_data[supply_data$YEAR == clicked_factor_year, ]
      
      supply_ratio_clicked_point(list(
        data = click_point,
        coords_css = input$supply_ratio_plot_click$coords_css))
    } else {
      supply_ratio_clicked_point(NULL)
    }
  })
  
  observeEvent(input$close_supply_ratio_tooltip, {
    supply_ratio_clicked_point(NULL)
  })
  
  output$supply_ratio_click_overlay <- renderUI({
    click_info <- supply_ratio_clicked_point()
    
    if (is.null(click_info)) {
      return(NULL)
    }
    
    left_pos <- click_info$coords_css$x + 10
    top_pos <- click_info$coords_css$y - 10
    
    left_pos <- max(10, left_pos)
    top_pos <- max(10, top_pos)
    
    div(
      style = paste0(
        "left: ", left_pos, "px;",
        "top: ", top_pos, "px;",
        tooltip_aes),
      tags$button(
        "x",
        id = 'close_supply_ratio_tooltip',
        onclick = "Shiny.setInputValue('close_supply_ratio_tooltip', Math.random());",
        style = close_button_aes),
      HTML(paste0(
        tooltip_heading, click_info$data$YEAR[1], "<br>",
        tooltip_subheading, "Apparent Supply</span>:<br>",
        comma(click_info$data$APPARENT_SUPPLY), ifelse(selected_units() == 'METRIC',
                                                       " Metric Tons <br>",
                                                       " Short Tons <br>"),
        tooltip_subheading, "Domestic Production Volume</span>:<br>",
        comma(click_info$data$PP_VOLUME_T), ifelse(selected_units() == 'METRIC',
                                                   " Metric Tons <br>",
                                                   " Short Tons <br>"),
        tooltip_line_icon('black', 16), tooltip_subheading, "Ratio</span>:<br>",
        round(click_info$data$APPARENT_SUPPLY_REL_US_PROD, digits = 3))))
  })
  
  
  
  #' *Supply Share*
  supply_share_clicked_point <- reactiveVal(NULL)
  
  observeEvent(input$supply_share_plot_click, {
    click_x <- input$supply_share_plot_click$x
    
    supply_data <- supply_df()
    
    year_levels <- levels(factor(sort(unique(supply_data$YEAR))))
    clicked_year <- round(click_x)
    
    if (clicked_year >= 1 && clicked_year <= length(year_levels)) {
      clicked_factor_year <- year_levels[clicked_year]
      
      click_point <- supply_data[supply_data$YEAR == clicked_factor_year, ]
      
      supply_share_clicked_point(list(
        data = click_point,
        coords_css = input$supply_share_plot_click$coords_css))
    } else {
      supply_share_clicked_point(NULL)
    }
  })
  
  observeEvent(input$close_supply_share_tooltip, {
    supply_share_clicked_point(NULL)
  })
  
  output$supply_share_click_overlay <- renderUI({
    click_info <- supply_share_clicked_point()
    
    if (is.null(click_info)) {
      return(NULL)
    }
    
    left_pos <- click_info$coords_css$x + 10
    top_pos <- click_info$coords_css$y - 10
    
    left_pos <- max(10, left_pos)
    top_pos <- max(10, top_pos)
    
    div(
      style = paste0(
        "left: ", left_pos, "px;",
        "top: ", top_pos, "px;",
        tooltip_aes),
      tags$button(
        "x",
        id = 'close_supply_share_tooltip',
        onclick = "Shiny.setInputValue('close_supply_share_tooltip', Math.random());",
        style = close_button_aes),
      HTML(paste0(
        tooltip_heading, click_info$data$YEAR[1], "<br>",
        tooltip_subheading, "Unexported Domestic Production</span>:<br>",
        comma(abs(click_info$data$PP_VOLUME_T - click_info$data$EXP_VOLUME_T)), 
        ifelse(selected_units() == 'METRIC', 
               " Metric Tons <br>", " Short Tons <br>"),
        tooltip_subheading, "Apparent Supply</span>:<br>",
        comma(click_info$data$APPARENT_SUPPLY), ifelse(selected_units() == 'METRIC',
                                                       " Metric Tons <br>",
                                                       " Short Tons <br>"),
        tooltip_color_icon(share_color), tooltip_subheading, "Share</span>:<br>",
        round(click_info$data$UNEXPORTED_US_PROD_REL_APPARENT_SUPPLY * 100, digits = 3), 
        "% of Apparent Supply")))
  })
  
  
  
  #' *Landings Value*
  landings_value_clicked_point <- reactiveVal(NULL)
  
  observeEvent(input$landings_value_plot_click, {
    click_x <- input$landings_value_plot_click$x
    
    landings_data <- landings_df()
    
    year_levels <- levels(factor(sort(unique(landings_data$YEAR))))
    clicked_year <- round(click_x)
    
    if (clicked_year >= 1 && clicked_year <= length(year_levels)) {
      clicked_factor_year <- year_levels[clicked_year]
      
      click_point <- landings_data[landings_data$YEAR == clicked_factor_year, ]
      
      landings_value_clicked_point(list(
        data = click_point,
        coords_css = input$landings_value_plot_click$coords_css))
    } else {
      landings_value_clicked_point(NULL)
    }
  })
  
  observeEvent(input$close_landings_value_tooltip, {
    landings_value_clicked_point(NULL)
  })
  
  output$landings_value_click_overlay <- renderUI({
    click_info <- landings_value_clicked_point()
    
    if (is.null(click_info)) {
      return(NULL)
    }
    
    left_pos <- click_info$coords_css$x + 10
    top_pos <- click_info$coords_css$y - 10
    
    left_pos <- max(10, left_pos)
    top_pos <- max(10, top_pos)
    
    div(
      style = paste0(
        "left: ", left_pos, "px;",
        "top: ", top_pos, "px;",
        tooltip_aes),
      tags$button(
        "x",
        id = 'close_landings_value_tooltip',
        onclick = "Shiny.setInputValue('close_landings_value_tooltip', Math.random());",
        style = close_button_aes),
      HTML(paste0(
        tooltip_heading, click_info$data$YEAR, "<br>", 
        tooltip_color_icon(landings_colors[1]), tooltip_subheading, "Ex-Vessel Value</span>:<br>",
        dollar(click_info$data$COM_VALUE_MILLIONS), " Million<br>",
        tooltip_line_icon(landings_colors[2], 16), tooltip_subheading, "Ex-Vessel Price</span>:<br>",
        dollar(click_info$data$COM_PRICE), ifelse(selected_units() == 'METRIC', 
                                                  " per kilogram", 
                                                  " per pound"))))
  })
  
  
  
  #' *Landings Volume*
  landings_volume_clicked_point <- reactiveVal(NULL)
  
  observeEvent(input$landings_volume_plot_click, {
    click_x <- input$landings_volume_plot_click$x
    
    landings_data <- landings_df()
    
    year_levels <- levels(factor(sort(unique(landings_data$YEAR))))
    clicked_year <- round(click_x)
    
    if (clicked_year >= 1 && clicked_year <= length(year_levels)) {
      clicked_factor_year <- year_levels[clicked_year]
      
      click_point <- landings_data[landings_data$YEAR == clicked_factor_year, ]
      
      landings_volume_clicked_point(list(
        data = click_point,
        coords_css = input$landings_volume_plot_click$coords_css))
    } else {
      landings_volume_clicked_point(NULL)
    }
  })
  
  observeEvent(input$close_landings_volume_tooltip, {
    landings_volume_clicked_point(NULL)
  })
  
  output$landings_volume_click_overlay <- renderUI({
    click_info <- landings_volume_clicked_point()
    
    if (is.null(click_info)) {
      return(NULL)
    }
    
    left_pos <- click_info$coords_css$x + 10
    top_pos <- click_info$coords_css$y - 10
    
    left_pos <- max(10, left_pos)
    top_pos <- max(10, top_pos)
    
    div(
      style = paste0(
        "left: ", left_pos, "px;",
        "top: ", top_pos, "px;",
        tooltip_aes),
      tags$button(
        "x",
        id = 'close_landings_volume_tooltip',
        onclick = "Shiny.setInputValue('close_landings_volume_tooltip', Math.random());",
        style = close_button_aes),
      HTML(paste0(
        tooltip_heading, click_info$data$YEAR, "<br>",
        tooltip_color_icon(landings_colors[1]), tooltip_subheading, "Landed Volume</span>:<br>",
        comma(click_info$data$COM_VOLUME_T), ifelse(selected_units() == 'METRIC', 
                                                    " Metric Tons", 
                                                    " Short Tons"))))
  })
  
  
  
  #' *Processed Product Value*
  pp_value_clicked_point <- reactiveVal(NULL)
  
  observeEvent(input$pp_value_plot_click, {
    click_x <- input$pp_value_plot_click$x
    
    pp_data <- pp_df()
    
    year_levels <- levels(factor(sort(unique(pp_data$YEAR))))
    clicked_year <- round(click_x)
    
    if (clicked_year >= 1 && clicked_year <= length(year_levels)) {
      clicked_factor_year <- year_levels[clicked_year]
      
      click_point <- pp_data[pp_data$YEAR == clicked_factor_year, ]
      
      pp_value_clicked_point(list(
        data = click_point,
        coords_css = input$pp_value_plot_click$coords_css))
    } else {
      pp_value_clicked_point(NULL)
    }
  })
  
  observeEvent(input$close_pp_value_tooltip, {
    pp_value_clicked_point(NULL)
  })
  
  output$pp_value_click_overlay <- renderUI({
    click_info <- pp_value_clicked_point()
    
    if (is.null(click_info)) {
      return(NULL)
    }
    
    # SPECIFY GROUPS
    # First, get data
    pp_data <- pp_df()
    
    # Next, get product forms
    products <- unique(str_to_title(pp_data$PRODUCT_FORM))
    # Subset colors for these products
    pp_colors <- pp_colors[names(pp_colors) %in% products]
    pp_colors <- pp_colors[names(pp_colors)]
    
    # extract first year for only one row per product, arrange alphabetically
    filtered_data <- pp_data %>% 
      filter(YEAR == click_info$data$YEAR[1])
    
    # create icons (number will vary)
    # begin with empty vector that will ultimately contain the full HTML code
    pp_val_tooltip <- vector()
    for (i in 1:length(pp_colors)) {
      tooltip_color <- paste0(tooltip_color_icon(pp_colors[i]))
      
      tooltip_text <- paste0(tooltip_subheading, names(pp_colors)[i], "</span>: ")
      
      tooltip_data <- paste0(
        dollar(filtered_data$PP_VALUE_MILLIONS[i]), " Million <br>")
      
      pp_val_tooltip <- paste0(pp_val_tooltip, tooltip_color, tooltip_text, tooltip_data)
    }
    
    # Position tooltip near clicked point
    left_pos <- click_info$coords_css$x + 20 # Offset to right of point
    top_pos <- click_info$coords_css$y - 150 # Offset above point
    
    # Prevent tooltip from being off screen
    left_pos <- max(10, left_pos)
    top_pos <- max(10, top_pos)
    
    # style the tooltip for clicked point
    div(
      style = paste0(
        "left: ", left_pos, "px;",
        "top: ", top_pos, "px;",
        tooltip_aes,
        "max-width: 350px; ",
        "min-width: 350px; "),
      # tooltip close button
      tags$button(
        "x",
        id = 'close_pp_value_tooltip',
        onclick = "Shiny.setInputValue('close_pp_value_tooltip', Math.random());",
        style = close_button_aes), 
      HTML(paste0(tooltip_heading, click_info$data$YEAR[1], "<br>", 
                  pp_val_tooltip)))
  })
  
  
  
  #' *Processed Product Volume*
  pp_volume_clicked_point <- reactiveVal(NULL)
  
  observeEvent(input$pp_volume_plot_click, {
    click_x <- input$pp_volume_plot_click$x
    
    pp_data <- pp_df()
    
    year_levels <- levels(factor(sort(unique(pp_data$YEAR))))
    clicked_year <- round(click_x)
    
    if (clicked_year >= 1 && clicked_year <= length(year_levels)) {
      clicked_factor_year <- year_levels[clicked_year]
      
      click_point <- pp_data[pp_data$YEAR == clicked_factor_year, ]
      
      pp_volume_clicked_point(list(
        data = click_point,
        coords_css = input$pp_volume_plot_click$coords_css))
    } else {
      pp_volume_clicked_point(NULL)
    }
  })
  
  observeEvent(input$close_pp_volume_tooltip, {
    pp_volume_clicked_point(NULL)
  })
  
  output$pp_volume_click_overlay <- renderUI({
    click_info <- pp_volume_clicked_point()
    
    if (is.null(click_info)) {
      return(NULL)
    }
    
    # SPECIFY GROUPS
    # First, get data
    pp_data <- pp_df()
    
    # Next, get product forms
    products <- unique(str_to_title(pp_data$PRODUCT_FORM))
    # Subset colors for these products
    pp_colors <- pp_colors[names(pp_colors) %in% products]
    pp_colors <- pp_colors[names(pp_colors)]
    
    filtered_data <- pp_data %>% 
      filter(YEAR == click_info$data$YEAR[1])
    
    # create icons (number will vary)
    # begin with empty vector that will ultimately contain the full HTML code
    pp_vol_tooltip <- vector()
    for (i in 1:length(pp_colors)) {
      tooltip_color <- paste0(tooltip_color_icon(pp_colors[i]))
      
      tooltip_text <- paste0(tooltip_subheading, names(pp_colors)[i], "</span>: ")
      
      tooltip_data <- paste0(
        comma(filtered_data$PP_VOLUME_T[i]), ifelse(selected_units() == 'METRIC',
                                                    " Metric Tons <br>",
                                                    " Short Tons <br>"))
      
      pp_vol_tooltip <- paste0(pp_vol_tooltip, tooltip_color, tooltip_text, tooltip_data)
    }
    
    # Position tooltip near clicked point
    left_pos <- click_info$coords_css$x + 20 # Offset to right of point
    top_pos <- click_info$coords_css$y - 150 # Offset above point
    
    # Prevent tooltip from being off screen
    left_pos <- max(10, left_pos)
    top_pos <- max(10, top_pos)
    
    # style the tooltip for clicked point
    div(
      style = paste0(
        "left: ", left_pos, "px;",
        "top: ", top_pos, "px;",
        tooltip_aes,
        "max-width: 350px; ",
        "min-width: 350px; "),
      # tooltip close button
      tags$button(
        "x",
        id = 'close_pp_volume_tooltip',
        onclick = "Shiny.setInputValue('close_pp_volume_tooltip', Math.random());",
        style = close_button_aes), 
      HTML(paste0(tooltip_heading, click_info$data$YEAR[1], "<br>",
                  pp_vol_tooltip)))
  })
  
  
  
  #' *Processed Product Price*
  pp_price_clicked_point <- reactiveVal(NULL)
  
  observeEvent(input$pp_price_plot_click, {
    click_x <- input$pp_price_plot_click$x
    
    pp_data <- pp_df()
    
    year_levels <- levels(factor(sort(unique(pp_data$YEAR))))
    clicked_year <- round(click_x)
    
    if (clicked_year >= 1 && clicked_year <= length(year_levels)) {
      clicked_factor_year <- year_levels[clicked_year]
      
      click_point <- pp_data[pp_data$YEAR == clicked_factor_year, ]
      
      pp_price_clicked_point(list(
        data = click_point,
        coords_css = input$pp_price_plot_click$coords_css))
    } else {
      pp_price_clicked_point(NULL)
    }
  })
  
  observeEvent(input$close_pp_price_tooltip, {
    pp_price_clicked_point(NULL)
  })
  
  output$pp_price_click_overlay <- renderUI({
    click_info <- pp_price_clicked_point()
    
    if (is.null(click_info)) {
      return(NULL)
    }
    
    # SPECIFY GROUPS
    # First, get data
    pp_data <- pp_df()
    
    # Next, get product forms
    products <- unique(str_to_title(pp_data$PRODUCT_FORM))
    # Subset colors for these products
    pp_colors <- pp_colors[names(pp_colors) %in% products]
    pp_colors <- pp_colors[names(pp_colors)]
    
    filtered_data <- pp_data %>% 
      filter(YEAR == click_info$data$YEAR[1])
    
    # create icons (number will vary)
    # begin with empty vector that will ultimately contain the full HTML code
    pp_price_tooltip <- vector()
    for (i in 1:length(pp_colors)) {
      tooltip_icon <- paste0(tooltip_line_icon(pp_colors[i], 16))
      
      tooltip_text <- paste0(tooltip_subheading, names(pp_colors)[i], "</span>: ")
      
      tooltip_data <- paste0(
        dollar(filtered_data$PP_PRICE[i]), ifelse(selected_units() == 'METRIC',
                                                  " per kilogram <br>",
                                                  " per pound <br>"))
      
      pp_price_tooltip <- paste0(pp_price_tooltip, tooltip_icon, tooltip_text, tooltip_data)
    }
    
    # Position tooltip near clicked point
    left_pos <- click_info$coords_css$x + 20 # Offset to right of point
    top_pos <- click_info$coords_css$y - 150 # Offset above point
    
    # Prevent tooltip from being off screen
    left_pos <- max(10, left_pos)
    top_pos <- max(10, top_pos)
    
    # style the tooltip for clicked point
    div(
      style = paste0(
        "left: ", left_pos, "px;",
        "top: ", top_pos, "px;",
        tooltip_aes,
        "max-width: 375px; ",
        "min-width: 375px; "),
      # tooltip close button
      tags$button(
        "x",
        id = 'close_pp_price_tooltip',
        onclick = "Shiny.setInputValue('close_pp_price_tooltip', Math.random());",
        style = close_button_aes), 
      HTML(paste0(tooltip_heading, click_info$data$YEAR[1], "<br>",
                  pp_price_tooltip)))
  })
  # Quarto Documents -----------------------------------------------------------
  output$intro <- renderUI({
    tags$iframe(seamless = "seamless",
                src = "tmpuser/dashboard_intro.html",
                height = 800)
  })
  
  output$collection <- renderUI({
    tags$iframe(seamless = "seamless",
                src = "tmpuser/dashboard_data_collection.html",
                height = 800)
  })
  
  output$sourcing <- renderUI({
    tags$iframe(seamless = "seamless",
                src = "tmpuser/dashboard_data_sourcing.html",
                height = 800)
  })
  
  output$management <- renderUI({
    tags$iframe(seamless = "seamless",
                src = "tmpuser/dashboard_data_management.html",
                height = 800)
  })
  
  output$coast <- renderUI({
    tags$iframe(seamless = "seamless",
                src = "tmpuser/dashboard_coast_doc.html",
                height = 800)
  })
  
  output$classification <- renderUI({
    tags$iframe(seamless = "seamless",
                src = "tmpuser/dashboard_species_classification.html",
                height = 800)
  })
  
  output$metrics <- renderUI({
    tags$iframe(seamless = "seamless",
                src = "tmpuser/dashboard_calc_metrics.html",
                height = 800)
  })
  
  output$contact <- renderUI({
    tags$iframe(seamless = "seamless",
                src = "tmpuser/dashboard_contact_us.html",
                height = 800)
  })
  
  output$resource <- renderUI({
    tags$iframe(seamless = "seamless",
                src = "tmpuser/dashboard_resources.html",
                height = 800)
  })
  # Coast Analysis plots -------------------------------------------------------
  # creates trade data for plots
  coast_trade_df <- reactive({
    summarize_trade_yr_spp(
      trade_filtered(),
      species_selection_trade(),
      coast = 'ALL',
      'VALUE',
      units = selected_units(),
      nominal = selected_value())
  })
  
  # creates export value plot
  exp_coast_value_plot <- reactive({
    plot_trade(coast_trade_df(), 'ALL', 'VALUE', units = selected_units(), export = T, 
               species = species_selection_trade(), nominal = selected_value()) +
      facet_grid(cols = vars(COAST))
  })
  
  # outputs export value plot
  output$exp_coast_value <- renderPlot({
    trade_data_validation()
    validate(need(try(!is.na(exp_coast_value_plot())),
                  '      Data for this species is insufficient to produce this plot'))
    exp_coast_value_plot()
  })
  
  # creates import value plot
  imp_coast_value_plot <- reactive({
    plot_trade(coast_trade_df(), 'ALL', 'VALUE', units = selected_units(), import = T, 
               species = species_selection_trade(), nominal = selected_value()) +
      facet_grid(cols = vars(COAST))
  })
  
  # outputs import value plot
  output$imp_coast_value <- renderPlot({
    trade_data_validation()
    validate(need(try(!is.na(imp_coast_value_plot())),
                  '      Data for this species is insufficient to produce this plot'))
    imp_coast_value_plot()
  })
  
  # creates export volume plot
  exp_coast_volume_plot <- reactive({
    plot_trade(coast_trade_df(), 'ALL', 'VOLUME', units = selected_units(), export = T, 
               species = species_selection_trade()) +
      facet_grid(cols = vars(COAST))
  })
  
  # outputs export volume plot
  output$exp_coast_volume <- renderPlot({
    trade_data_validation()
    validate(need(try(!is.na(exp_coast_volume_plot())),
                  '      Data for this species is insufficient to produce this plot'))
    exp_coast_volume_plot()
  })
  
  # creates import volume plot
  imp_coast_volume_plot <- reactive({
    plot_trade(coast_trade_df(), 'ALL', 'VOLUME', units = selected_units(), import = T, 
               species = species_selection_trade()) +
      facet_grid(cols = vars(COAST))
  })
  
  # outputs import volume plot
  output$imp_coast_volume <- renderPlot({
    trade_data_validation()
    validate(need(try(!is.na(imp_coast_volume_plot())),
                  '      Data for this species is insufficient to produce this plot'))
    imp_coast_volume_plot()
  })
  
  
  
  
  # creates landings data for plots
  coast_landings_df <- reactive({
    summarize_landings_yr_spp(
      landings_filtered(),
      species_selection_landings(),
      coast = 'ALL',
      units = selected_units(),
      nominal = selected_value())
  })
  
  # creates landings value plot
  coast_landings_value_plot <- reactive({
    plot_landings(coast_landings_df(), 'ALL', 'VALUE', units = selected_units(),
                  species = species_selection_landings(),
                  nominal = selected_value()) +
      facet_grid(cols = vars(COAST))
  })
  
  # outputs landings value plot
  output$coast_landings_value <- renderPlot({
    landings_data_validation()
    validate(need(try(!is.na(coast_landings_value_plot())),
                  '      Data for this species is insufficient to produce this plot'))
    coast_landings_value_plot()
  })
  
  # creates landings volume plot
  coast_landings_volume_plot <- reactive({
    plot_landings(coast_landings_df(), 'ALL', 'VOLUME', units = selected_units(),
                  species = species_selection_landings()) +
      facet_grid(cols = vars(COAST))
  })
  
  # outputs landings volume plot
  output$coast_landings_volume <- renderPlot({
    landings_data_validation()
    validate(need(try(!is.na(coast_landings_volume_plot())),
                  '      Data for this species is insufficient to produce this plot'))
    coast_landings_volume_plot()
  })
  
  
  
  # creates processed products data for plots
  coast_pp_df <- reactive({
    summarize_pp_yr_spp(
      products_filtered(),
      species_selection_products(),
      'ALL',
      units = selected_units(),
      nominal = selected_value())
  })
  
  # creates processed products value plot
  coast_pp_value_plot <- reactive({
    plot_spp_pp(coast_pp_df(), 'ALL', 'VALUE', 
                units = selected_units(),
                species = species_selection_products(),
                nominal = selected_value()) +
      facet_grid(cols = vars(COAST))
  })
  
  # outputs processed products value plot
  output$coast_pp_value <- renderPlot({
    pp_data_validation()
    validate(need(try(!is.na(coast_pp_value_plot())),
                  '      Data for this species is insufficient to produce this plot'))
    coast_pp_value_plot()
  })
  
  # creates processed products volume plot
  coast_pp_volume_plot <- reactive({
    plot_spp_pp(coast_pp_df(), 'ALL', 'VOLUME', 
                units = selected_units(),
                species = species_selection_products()) +
      facet_grid(cols = vars(COAST))
  })
  
  # outputs processed products volume plot
  output$coast_pp_volume <- renderPlot({
    pp_data_validation()
    validate(need(try(!is.na(coast_pp_volume_plot())),
                  '      Data for this species is insufficient to produce this plot'))
    coast_pp_volume_plot()
  })
  
  # creates processed products price plot
  coast_pp_price_plot <- reactive({
    plot_spp_pp(coast_pp_df(), 'ALL', 'PRICE', 
                units = selected_units(),
                species = species_selection_products(),
                nominal = selected_value()) +
      facet_grid(cols = vars(COAST))
  })
  
  # outputs processed products price plot
  output$coast_pp_price <- renderPlot({
    pp_data_validation()
    validate(need(try(!is.na(coast_pp_price_plot())),
                  '      Data for this species is insufficient to produce this plot'))
    coast_pp_price_plot()
  })
  # Coast Analysis Tooltips ----------------------------------------------------
  #' *Export Value*
  # Set clicked point as reactive value, allows us to update later (T/F)
  exp_coast_value_clicked_point <- reactiveVal(FALSE)
  
  # when the close button (X) is clicked, make tooltip disappear
  observeEvent(input$close_exp_coast_value_tooltip, {
    exp_coast_value_clicked_point(FALSE)
  })
  
  # when the plot is clicked, make tooltip appear
  observeEvent(input$exp_coast_value_click, {
    exp_coast_value_clicked_point(TRUE)
  })

  # create tooltip
  output$exp_coast_value_click_overlay <- renderUI({
    # if reactive value is false, nothing shows
    if (!exp_coast_value_clicked_point()) {
      return(NULL)
    }
    
    # To get the data from the click, first store the data
    data <- coast_trade_df()
    
    # get year (round the x-click to get a whole number)
    year <- round(input$exp_coast_value_click$x)
    # year is stored as factor in plot so get the years as a factor
    year_levels <- levels(factor(data$YEAR))
    # get the value for the clicked year as the factored value
    factored_year <- year_levels[year]
    
    # get the facet panel clicked by the user
    panel <- input$exp_coast_value_click$panelvar1
    
    # get the data from the click by factoring for YEAR and COAST (panel)
    click_info <- data %>%
      filter(YEAR == factored_year,
             COAST == panel)
    
    # output a NULL if no click info data
    if (nrow(click_info) == 0) {
      return(NULL)
    }

    # position tooltip near clicked point
    left_pos <- input$exp_coast_value_click$coords_css$x + 10
    top_pos <- input$exp_coast_value_click$coords_css$y - 10

    # keep tooltip within plot bounds

    left_pos <- max(10, left_pos)
    top_pos <- max(10, top_pos)

    # tooltip style
    div(
      style = paste0(
        "left: ", left_pos, "px;",
        "top: ", top_pos, "px;",
        tooltip_aes
      ),

      # Close button
      tags$button(
        "x",
        id = "close_exp_coast_value_tooltip",
        onclick = "Shiny.setInputValue('close_exp_coast_value_tooltip', Math.random());",
        style = close_button_aes
      ),

      # Tooltip info
      HTML(paste0(
        tooltip_heading, click_info$YEAR, ": ", click_info$COAST, "<br>",
        tooltip_color_icon(export_color), tooltip_subheading, "Export Value</span>:<br>",
        dollar(click_info$EXP_VALUE_MILLIONS), " Million<br>",
        tooltip_line_icon(trade_price_color, 16), tooltip_subheading, "Export Price</span>:<br>",
        dollar(click_info$EXP_PRICE), ifelse(selected_units() == 'METRIC',
                                                  " per kilogram",
                                                  " per pound"))))
  })
  
  
  #' *Import Value*
  imp_coast_value_clicked_point <- reactiveVal(FALSE)
  
  observeEvent(input$close_imp_coast_value_tooltip, {
    imp_coast_value_clicked_point(FALSE)
  })
  
  observeEvent(input$imp_coast_value_click, {
    imp_coast_value_clicked_point(TRUE)
  })
  
  output$imp_coast_value_click_overlay <- renderUI({
    if (!imp_coast_value_clicked_point()) {
      return(NULL)
    }
    
    data <- coast_trade_df()
    
    year <- round(input$imp_coast_value_click$x)
    year_levels <- levels(factor(data$YEAR))
    factored_year <- year_levels[year]
    
    panel <- input$imp_coast_value_click$panelvar1
    
    click_info <- data %>%
      filter(YEAR == factored_year,
             COAST == panel)
    
    if (nrow(click_info) == 0) {
      return(NULL)
    }
    
    left_pos <- input$imp_coast_value_click$coords_css$x + 10
    top_pos <- input$imp_coast_value_click$coords_css$y - 100
    
    left_pos <- max(10, left_pos)
    top_pos <- max(10, top_pos)
    
    div(
      style = paste0(
        "left: ", left_pos, "px;",
        "top: ", top_pos, "px;",
        tooltip_aes
      ),
      
      tags$button(
        "x",
        id = "close_imp_coast_value_tooltip",
        onclick = "Shiny.setInputValue('close_imp_coast_value_tooltip', Math.random());",
        style = close_button_aes
      ),
      
      HTML(paste0(
        tooltip_heading, click_info$YEAR, ": ", click_info$COAST, "<br>",
        tooltip_color_icon(import_color), tooltip_subheading, "Import Value</span>:<br>",
        dollar(click_info$IMP_VALUE_MILLIONS), " Million<br>",
        tooltip_line_icon(trade_price_color, 16), tooltip_subheading, "Import Price</span>:<br>",
        dollar(click_info$IMP_PRICE), ifelse(selected_units() == 'METRIC',
                                             " per kilogram",
                                             " per pound"))))
  })
  
  
  
  #' *Export Volume*
  exp_coast_volume_clicked_point <- reactiveVal(FALSE)
  
  observeEvent(input$close_exp_coast_volume_tooltip, {
    exp_coast_volume_clicked_point(FALSE)
  })
  
  observeEvent(input$exp_coast_volume_click, {
    exp_coast_volume_clicked_point(TRUE)
  })
  
  output$exp_coast_volume_click_overlay <- renderUI({
    if (!exp_coast_volume_clicked_point()) {
      return(NULL)
    }
    
    data <- coast_trade_df()
    
    year <- round(input$exp_coast_volume_click$x)
    year_levels <- levels(factor(data$YEAR))
    factored_year <- year_levels[year]
    
    panel <- input$exp_coast_volume_click$panelvar1
    
    click_info <- data %>%
      filter(YEAR == factored_year,
             COAST == panel)
    
    if (nrow(click_info) == 0) {
      return(NULL)
    }
    
    left_pos <- input$exp_coast_volume_click$coords_css$x + 10
    top_pos <- input$exp_coast_volume_click$coords_css$y - 10
    
    left_pos <- max(10, left_pos)
    top_pos <- max(10, top_pos)
    
    div(
      style = paste0(
        "left: ", left_pos, "px;",
        "top: ", top_pos, "px;",
        tooltip_aes
      ),
      
      tags$button(
        "x",
        id = "close_exp_coast_volume_tooltip",
        onclick = "Shiny.setInputValue('close_exp_coast_volume_tooltip', Math.random());",
        style = close_button_aes
      ),
      
      HTML(paste0(
        tooltip_heading, click_info$YEAR, ": ", click_info$COAST, "<br>",
        tooltip_color_icon(import_color), tooltip_subheading, "Export Volume</span>:<br>",
        comma(click_info$EXP_VOLUME_T), ifelse(selected_units() == 'METRIC', 
                                                    " Metric Tons", 
                                                    " Short Tons"))))
  })
  
  
  
  #' *Import Volume*
  imp_coast_volume_clicked_point <- reactiveVal(FALSE)
  
  observeEvent(input$close_imp_coast_volume_tooltip, {
    imp_coast_volume_clicked_point(FALSE)
  })
  
  observeEvent(input$imp_coast_volume_click, {
    imp_coast_volume_clicked_point(TRUE)
  })
  
  output$imp_coast_volume_click_overlay <- renderUI({
    if (!imp_coast_volume_clicked_point()) {
      return(NULL)
    }
    
    data <- coast_trade_df()
    
    year <- round(input$imp_coast_volume_click$x)
    year_levels <- levels(factor(data$YEAR))
    factored_year <- year_levels[year]
    
    panel <- input$imp_coast_volume_click$panelvar1
    
    click_info <- data %>%
      filter(YEAR == factored_year,
             COAST == panel)
    
    if (nrow(click_info) == 0) {
      return(NULL)
    }
    
    left_pos <- input$imp_coast_volume_click$coords_css$x + 10
    top_pos <- input$imp_coast_volume_click$coords_css$y - 100
    
    left_pos <- max(10, left_pos)
    top_pos <- max(10, top_pos)
    
    div(
      style = paste0(
        "left: ", left_pos, "px;",
        "top: ", top_pos, "px;",
        tooltip_aes
      ),
      
      tags$button(
        "x",
        id = "close_imp_coast_volume_tooltip",
        onclick = "Shiny.setInputValue('close_imp_coast_volume_tooltip', Math.random());",
        style = close_button_aes
      ),
      
      HTML(paste0(
        tooltip_heading, click_info$YEAR,  ": ", click_info$COAST, "<br>",
        tooltip_color_icon(import_color), tooltip_subheading, "Export Volume</span>:<br>",
        comma(click_info$EXP_VOLUME_T), ifelse(selected_units() == 'METRIC', 
                                               " Metric Tons", 
                                               " Short Tons"))))
  })
  
  
  
  #' *Landings Value*
  coast_landings_value_clicked_point <- reactiveVal(FALSE)
  
  observeEvent(input$close_coast_landings_value_tooltip, {
    coast_landings_value_clicked_point(FALSE)
  })
  
  observeEvent(input$coast_landings_value_plot_click, {
    coast_landings_value_clicked_point(TRUE)
  })
  
  output$coast_landings_value_click_overlay <- renderUI({
    if (!coast_landings_value_clicked_point()) {
      return(NULL)
    }
    
    data <- coast_landings_df()
    
    year <- round(input$coast_landings_value_plot_click$x)
    year_levels <- levels(factor(data$YEAR))
    factored_year <- year_levels[year]
    
    panel <- input$coast_landings_value_plot_click$panelvar1
    
    click_info <- data %>%
      filter(YEAR == factored_year,
             COAST == panel)
    
    if (nrow(click_info) == 0) {
      return(NULL)
    }
    
    left_pos <- input$coast_landings_value_plot_click$coords_css$x + 10
    top_pos <- input$coast_landings_value_plot_click$coords_css$y - 100
    
    left_pos <- max(10, left_pos)
    top_pos <- max(10, top_pos)
    
    div(
      style = paste0(
        "left: ", left_pos, "px;",
        "top: ", top_pos, "px;",
        tooltip_aes
      ),
      
      tags$button(
        "x",
        id = "close_coast_landings_value_tooltip",
        onclick = "Shiny.setInputValue('close_coast_landings_value_tooltip', Math.random());",
        style = close_button_aes
      ),
      
      HTML(paste0(
        tooltip_heading, click_info$YEAR,  ": ", click_info$COAST, "<br>", 
        tooltip_color_icon(landings_colors[1]), tooltip_subheading, "Ex-Vessel Value</span>:<br>",
        dollar(click_info$COM_VALUE_MILLIONS), " Million<br>",
        tooltip_line_icon(landings_colors[2], 16), tooltip_subheading, "Ex-Vessel Price</span>:<br>",
        dollar(click_info$COM_PRICE), ifelse(selected_units() == 'METRIC', 
                                                  " per kilogram", 
                                                  " per pound"))))
  })
  
  
  
  #' *Landings Volume*
  coast_landings_volume_clicked_point <- reactiveVal(FALSE)
  
  observeEvent(input$close_coast_landings_volume_tooltip, {
    coast_landings_volume_clicked_point(FALSE)
  })
  
  observeEvent(input$coast_landings_volume_plot_click, {
    coast_landings_volume_clicked_point(TRUE)
  })
  
  output$coast_landings_volume_click_overlay <- renderUI({
    if (!coast_landings_volume_clicked_point()) {
      return(NULL)
    }
    
    data <- coast_landings_df()
    
    year <- round(input$coast_landings_volume_plot_click$x)
    year_levels <- levels(factor(data$YEAR))
    factored_year <- year_levels[year]
    
    panel <- input$coast_landings_volume_plot_click$panelvar1
    
    click_info <- data %>%
      filter(YEAR == factored_year,
             COAST == panel)
    
    if (nrow(click_info) == 0) {
      return(NULL)
    }
    
    left_pos <- input$coast_landings_volume_plot_click$coords_css$x + 10
    top_pos <- input$coast_landings_volume_plot_click$coords_css$y - 100
    
    left_pos <- max(10, left_pos)
    top_pos <- max(10, top_pos)
    
    div(
      style = paste0(
        "left: ", left_pos, "px;",
        "top: ", top_pos, "px;",
        tooltip_aes
      ),
      
      tags$button(
        "x",
        id = "close_coast_landings_volume_tooltip",
        onclick = "Shiny.setInputValue('close_coast_landings_volume_tooltip', Math.random());",
        style = close_button_aes
      ),
      
      HTML(paste0(
        tooltip_heading, click_info$YEAR, ": ", click_info$COAST, "<br>",
        tooltip_color_icon(landings_colors[1]), tooltip_subheading, "Landed Volume</span>:<br>",
        comma(click_info$COM_VOLUME_T), ifelse(selected_units() == 'METRIC', 
                                                    " Metric Tons", 
                                                    " Short Tons"))))
  })
  
  
  
  #' *Processed Product Value*
  coast_pp_value_clicked_point <- reactiveVal(FALSE)
  
  observeEvent(input$close_coast_pp_value_tooltip, {
    coast_pp_value_clicked_point(FALSE)
  })
  
  observeEvent(input$coast_pp_value_plot_click, {
    coast_pp_value_clicked_point(TRUE)
  })
  
  output$coast_pp_value_click_overlay <- renderUI({
    if (!coast_pp_value_clicked_point()) {
      return(NULL)
    }
    
    panel <- input$coast_pp_value_plot_click$panelvar1
    
    # SPECIFY GROUPS
    # First, get data
    pp_data <- coast_pp_df()
    
    year <- round(input$coast_pp_value_plot_click$x)
    year_levels <- levels(factor(pp_data$YEAR))
    factored_year <- year_levels[year]
    
    # Next, get product forms
    products <- unique(str_to_title(pp_data$PRODUCT_FORM))
    # Subset colors for these products
    pp_colors <- pp_colors[names(pp_colors) %in% products]
    pp_colors <- pp_colors[names(pp_colors)]
    
    # extract first year for only one row per product, arrange alphabetically
    filtered_data <- pp_data %>% 
      filter(YEAR == factored_year,
             COAST == panel)
    
    # create icons (number will vary)
    # begin with empty vector that will ultimately contain the full HTML code
    coast_pp_val_tooltip <- vector()
    for (i in 1:length(pp_colors)) {
      tooltip_color <- paste0(tooltip_color_icon(pp_colors[i]))
      
      tooltip_text <- paste0(tooltip_subheading, names(pp_colors)[i], "</span>: ")
      
      tooltip_data <- paste0(
        dollar(filtered_data$PP_VALUE_MILLIONS[i]), " Million <br>")
      
      coast_pp_val_tooltip <- paste0(coast_pp_val_tooltip, tooltip_color, tooltip_text, tooltip_data)
    }
    
    if (nrow(filtered_data) == 0) {
      return(NULL)
    }
    
    # Position tooltip near clicked point
    left_pos <- input$coast_pp_value_plot_click$coords_css$x + 20 # Offset to right of point
    top_pos <- input$coast_pp_value_plot_click$coords_css$y - 150 # Offset above point
    
    # Prevent tooltip from being off screen
    left_pos <- max(10, left_pos)
    top_pos <- max(10, top_pos)
    
    # style the tooltip for clicked point
    div(
      style = paste0(
        "left: ", left_pos, "px;",
        "top: ", top_pos, "px;",
        tooltip_aes,
        "max-width: 350px; ",
        "min-width: 350px; "),
      # tooltip close button
      tags$button(
        "x",
        id = 'close_coast_pp_value_tooltip',
        onclick = "Shiny.setInputValue('close_coast_pp_value_tooltip', Math.random());",
        style = close_button_aes), 
      HTML(paste0(tooltip_heading, filtered_data$YEAR[1], ": ", filtered_data$COAST[1], "<br>", 
                  coast_pp_val_tooltip)))
  })
  
  

  #' *Processed Product Volume*
  coast_pp_volume_clicked_point <- reactiveVal(FALSE)
  
  observeEvent(input$close_coast_pp_volume_tooltip, {
    coast_pp_volume_clicked_point(FALSE)
  })
  
  observeEvent(input$coast_pp_volume_plot_click, {
    coast_pp_volume_clicked_point(TRUE)
  })
  
  output$coast_pp_volume_click_overlay <- renderUI({
    if (!coast_pp_volume_clicked_point()) {
      return(NULL)
    }
    
    panel <- input$coast_pp_volume_plot_click$panelvar1
    
    # SPECIFY GROUPS
    # First, get data
    pp_data <- coast_pp_df()
    
    year <- round(input$coast_pp_volume_plot_click$x)
    year_levels <- levels(factor(pp_data$YEAR))
    factored_year <- year_levels[year]
    
    # Next, get product forms
    products <- unique(str_to_title(pp_data$PRODUCT_FORM))
    # Subset colors for these products
    pp_colors <- pp_colors[names(pp_colors) %in% products]
    pp_colors <- pp_colors[names(pp_colors)]
    
    filtered_data <- pp_data %>% 
      filter(YEAR == factored_year,
             COAST == panel)
    
    # create icons (number will vary)
    # begin with empty vector that will ultimately contain the full HTML code
    coast_pp_vol_tooltip <- vector()
    for (i in 1:length(pp_colors)) {
      tooltip_color <- paste0(tooltip_color_icon(pp_colors[i]))
      
      tooltip_text <- paste0(tooltip_subheading, names(pp_colors)[i], "</span>: ")
      
      tooltip_data <- paste0(
        comma(filtered_data$PP_VOLUME_T[i]), ifelse(selected_units() == 'METRIC',
                                                    " Metric Tons <br>",
                                                    " Short Tons <br>"))
      
      coast_pp_vol_tooltip <- paste0(coast_pp_vol_tooltip, tooltip_color, tooltip_text, tooltip_data)
    }
    
    # Position tooltip near clicked point
    left_pos <- input$coast_pp_volume_plot_click$coords_css$x + 20 # Offset to right of point
    top_pos <- input$coast_pp_volume_plot_click$coords_css$y - 150 # Offset above point
    
    # Prevent tooltip from being off screen
    left_pos <- max(10, left_pos)
    top_pos <- max(10, top_pos)
    
    # style the tooltip for clicked point
    div(
      style = paste0(
        "left: ", left_pos, "px;",
        "top: ", top_pos, "px;",
        tooltip_aes,
        "max-width: 350px; ",
        "min-width: 350px; "),
      # tooltip close button
      tags$button(
        "x",
        id = 'close_coast_pp_volume_tooltip',
        onclick = "Shiny.setInputValue('close_coast_pp_volume_tooltip', Math.random());",
        style = close_button_aes), 
      HTML(paste0(tooltip_heading, filtered_data$YEAR[1], ": ", filtered_data$COAST[1], "<br>",
                  coast_pp_vol_tooltip)))
  })
  
  
  
  #' *Processed Product Price*
  coast_pp_price_clicked_point <- reactiveVal(FALSE)
  
  observeEvent(input$close_coast_pp_price_tooltip, {
    coast_pp_price_clicked_point(FALSE)
  })
  
  observeEvent(input$coast_pp_price_plot_click, {
    coast_pp_price_clicked_point(TRUE)
  })
  
  output$coast_pp_price_click_overlay <- renderUI({
    if (!coast_pp_price_clicked_point()) {
      return(NULL)
    }
    
    panel <- input$coast_pp_price_plot_click$panelvar1
    
    # SPECIFY GROUPS
    # First, get data
    pp_data <- coast_pp_df()
    
    year <- round(input$coast_pp_price_plot_click$x)
    year_levels <- levels(factor(pp_data$YEAR))
    factored_year <- year_levels[year]
    
    # Next, get product forms
    products <- unique(str_to_title(pp_data$PRODUCT_FORM))
    # Subset colors for these products
    pp_colors <- pp_colors[names(pp_colors) %in% products]
    pp_colors <- pp_colors[names(pp_colors)]
    
    filtered_data <- pp_data %>% 
      filter(YEAR == factored_year,
             COAST == panel)
    
    # create icons (number will vary)
    # begin with empty vector that will ultimately contain the full HTML code
    coast_pp_price_tooltip <- vector()
    for (i in 1:length(pp_colors)) {
      tooltip_icon <- paste0(tooltip_line_icon(pp_colors[i], 16))
      
      tooltip_text <- paste0(tooltip_subheading, names(pp_colors)[i], "</span>: ")
      
      tooltip_data <- paste0(
        dollar(filtered_data$PP_PRICE[i]), ifelse(selected_units() == 'METRIC',
                                                  " per kilogram <br>",
                                                  " per pound <br>"))
      
      coast_pp_price_tooltip <- paste0(coast_pp_price_tooltip, tooltip_icon, tooltip_text, tooltip_data)
    }
    
    # Position tooltip near clicked point
    left_pos <- input$coast_pp_price_plot_click$coords_css$x + 20 # Offset to right of point
    top_pos <- input$coast_pp_price_plot_click$coords_css$y - 150 # Offset above point
    
    # Prevent tooltip from being off screen
    left_pos <- max(10, left_pos)
    top_pos <- max(10, top_pos)
    
    # style the tooltip for clicked point
    div(
      style = paste0(
        "left: ", left_pos, "px;",
        "top: ", top_pos, "px;",
        tooltip_aes,
        "max-width: 375px; ",
        "min-width: 375px; "),
      # tooltip close button
      tags$button(
        "x",
        id = 'close_coast_pp_price_tooltip',
        onclick = "Shiny.setInputValue('close_coast_pp_price_tooltip', Math.random());",
        style = close_button_aes), 
      HTML(paste0(tooltip_heading, filtered_data$YEAR[1], ": ", filtered_data$COAST[1], "<br>",
                  coast_pp_price_tooltip)))
  })
}

# Run the app
shinyApp(ui = ui, server = server)