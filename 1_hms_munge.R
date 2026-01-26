# Munge Seafood Dashboard data for HMS Analysis

if(!require("tidyverse")) install.packages("tidyverse")


# Load in data -----------------------------------------------------------------
# load in seafood data
load('seafood_trade_data_munge_01_15_26.RData')

# get inflationary index data
  # this will allow values per year to be calculated as inflation-adjusted
def_index <- read.csv('GDPDEF_2024_index.csv') %>%
  rename_with( ~ toupper(.x)) %>%
  rename(DEFLATOR_VALUE = GDPDEF_NBD20240101,
         YEAR = OBSERVATION_DATE) %>%
  # remove -01-01 from year as it is negligible (the index is averaged per year)
  mutate(YEAR = as.numeric(gsub('-01-01', '', YEAR)),
         INDEX = (100/DEFLATOR_VALUE))

# Get territorial landings from FOSS
foss_terr_landings <- read.csv('foss_terr_landings.csv') %>%
  setNames(.[1, ]) %>%
  rename_with( ~ toupper(gsub(' ', '_', .x, fixed = T))) %>%
  .[-1, ] %>%
  mutate(COAST = ifelse(TERRITORY %in% c('AMERICAN SAMOA', 'GUAM',
                                         'NORTHERN MARIANA IS.'),
                        'PACIFIC', 'ATLANTIC'),
         REGION = ifelse(COAST == 'PACIFIC', 'PACIFIC ISLANDS',
                         'SOUTHEAST'))

# read territorial landings map data
  # this map attaches the species classification hierarchy
foss_terr_landings_map <- read.csv('foss_terr_landings_map.csv')

# We have landings data for HMS species from Pacific Islands territories that
  # are not included in FOSS
# this data is aggregated for Samoa, Guam, and the Marianas and is grouped by
  # HMS species. It needs to replace all FOSS landings data for these territories
pacisl_terr_landings <- read.csv('PIR_HMS_AS_GU_CNMI_totals2011_2024.csv') %>%
  select(!X) %>%
  rename(DOLLARS = REVENUE_NOMINAL,
         COMMON_NAME = SPECIES_NAME) %>%
  left_join(def_index %>% select(YEAR, INDEX)) %>%
  mutate(DOLLARS_2024 = DOLLARS * INDEX,
         STATE = 'PACIFIC_ISLAND_TERRITORIES') %>%
  select(-INDEX) %>%
  mutate(COMMON_NAME = ifelse(COMMON_NAME == 'ALBACORE TUNA', 'TUNA, ALBACORE',
                              COMMON_NAME),
         COMMON_NAME = ifelse(COMMON_NAME == 'BIGEYE TUNA', 'TUNA, BIGEYE',
                              COMMON_NAME),
         COMMON_NAME = ifelse(COMMON_NAME == 'KAWAKAWA', 'TUNA, KAWAKAWA',
                              COMMON_NAME),
         COMMON_NAME = ifelse(COMMON_NAME == 'SKIPJACK TUNA', 'TUNA, SKIPJACK',
                              COMMON_NAME),
         COMMON_NAME = ifelse(COMMON_NAME == 'YELLOWFIN TUNA', 'TUNA, YELLOWFIN',
                              COMMON_NAME),
         COMMON_NAME = ifelse(COMMON_NAME == 'TUNAS (MISC)', 'TUNAS',
                              COMMON_NAME)) %>%
  left_join(foss_terr_landings_map %>%
              # an erroneous skipjack scientific name
              filter(SCIENTIFIC_NAME != 'Euthynnus pelamis')) %>%
  mutate(CONFIDENTIALITY = 'Public',
         COAST = 'PACIFIC',
         REGION = 'PACIFIC ISLANDS')

# new territorial landings data (combine foss and other data)
terr_landings <- left_join(foss_terr_landings, foss_terr_landings_map) %>%
  # duplicates caused by this join are confids or shrimps and are not relevant
    # to HMS, can ignore UNLESS looking to investigate shrimp data
  mutate(SPECIES_CATEGORY = ifelse(SPECIES_NAME %in% c('SWORDFISH', 
                                                       'BLUE MARLIN', 
                                                       'BLACK MARLIN',
                                                       'STRIPED MARLIN',
                                                       'SAILFISH', 
                                                       'SHORTBILL SPEARFISH'), 
                                   'BILLFISHES', SPECIES_CATEGORY),
         SPECIES_CATEGORY = ifelse(SPECIES_GROUP == 'SWORDFISH', 
                                   'BILLFISHES', SPECIES_CATEGORY),
         YEAR = as.numeric(YEAR),
         POUNDS = as.numeric(gsub(',', '', POUNDS)),
         METRIC_TONS = as.numeric(gsub(',', '', METRIC_TONS)),
         DOLLARS = as.numeric(gsub(',', '', DOLLARS))) %>%
  rename(STATE = TERRITORY) %>%
  left_join(def_index %>% select(YEAR, INDEX)) %>%
  mutate(DOLLARS_2024 = DOLLARS * INDEX,
         KG = POUNDS * 0.45359237) %>%
  select(-INDEX) %>%
  # remove territorial landings for American Samoa, Guam, and Northern Marianas
  # or, just keep USVI and PR
  filter(STATE %in% c('U.S. VIRGIN IS.', 'PUERTO RICO')) %>%
  bind_rows(pacisl_terr_landings)

# Get pp data manually ---------------------------------------------------------
# pp data in saved file does not have state resolution so attaching coasts is
  # not presently possible. Need to re summarize pp data
pp_address <- read.csv('pp_address.csv') %>%
  mutate(STATE = STATE_ABRV,
         STATE = ifelse(STATE %in% c('CM', 'MP'), 'NORTHERN MARIANA IS.',
                        ifelse(STATE == 'GU', 'GUAM',
                               ifelse(STATE == 'PR', 'PUERTO RICO',
                                      ifelse(STATE == 'AS', 'AMERICAN SAMOA',
                                             STATE)))),
         PLANT_CITY = ifelse(PLANT_CITY == 'BOZEMEN', 'BOZEMAN', PLANT_CITY),
         STATE = ifelse(PLANT_CITY == 'SAN FRANCISCO', 'CA', STATE),
         PLANT_CITY = ifelse(PLANT_CITY == 'EAST QUOQUE', 'EAST QUOGUE', PLANT_CITY),
         PLANT_CITY = ifelse(PLANT_CITY == 'EAST SEATAUKUT', 'EAST SETAUKET', PLANT_CITY),
         PLANT_CITY = ifelse(PLANT_CITY == 'GLEN FALLS', 'GLENS FALLS', PLANT_CITY),
         PLANT_CITY = ifelse(PLANT_CITY == 'Hodgkins', 'HODGKINS', PLANT_CITY),
         PLANT_CITY = ifelse(PLANT_CITY == 'APALCHICOLA', 'APALACHICOLA',
                             PLANT_CITY),
         PLANT_CITY = ifelse(PLANT_CITY == 'FT LAUDERDALE', 'FORT LAUDERDALE',
                             PLANT_CITY),
         PLANT_CITY = ifelse(PLANT_CITY == 'FT PIERCE', 'FORT PIERCE',
                             PLANT_CITY),
         PLANT_CITY = ifelse(PLANT_CITY == 'GODDLAND', 'GOODLAND',
                             PLANT_CITY),
         PLANT_CITY = ifelse(PLANT_CITY == 'Jacksonville', 'JACKSONVILLE',
                             PLANT_CITY),
         PLANT_STATE_ABRV = ifelse(PLANT_CITY == 'IRVINGTON', 'AL', PLANT_STATE_ABRV))

# Merge address data with pp_processed csv
pp_processed <- read.csv('pp_processed.csv') %>%
  filter(YEAR >= 2004) %>%
  left_join(pp_address %>%
              select(PP_IDNUM, PLANT_CITY, PLANT_STATE_ABRV, PLANT_STREET) %>%
              rename(STATE = PLANT_STATE_ABRV,
                     CITY = PLANT_CITY))

# remove confidential data
# get pp map data
pp_form_map <- read.csv('pp_form_map.csv')
pp_map <- read.csv('pp_db_map.csv') %>%
  left_join(pp_form_map)

# get florida coast map
florida_coast_map <- read.csv('florida_city_map.csv')

# get great lakes map
great_lakes_cities <- read.csv('gl_border_state_cities.csv')

# assign regions
norpac <- c('AK', 'ALASKA')
pac <- c('CA', 'CALIFORNIA', 'OR', 'OREGON', 'WA', 'WASHINGTON')
pacisl <- c('HI', 'HAWAII', 'AS', 'CM', 'MP', 'GU')
neweng <- c('CT', 'CONNECTICUT', 'ME', 'MAINE', 'MA', 'MASSACHUSETTS', 'NH', 
            'NEW HAMPSHIRE', 'RI', 'RHODE ISLAND')
midatl <- c('DE', 'DELAWARE', 'MD', 'MARYLAND', 'NJ', 'NEW JERSEY', 'NY',
            'NEW YORK', 'VA', 'VIRGINIA', 'PA', 'PENNSYLVANIA', 'DC')
souatl <- c('GA', 'GEORGIA', 'NC', 'NORTH CAROLINA', 'SC', 'SOUTH CAROLINA',
            'FL-E', 'FLORIDA', 'PR', 'PUERTO RICO', 'VI', 'U.S. VIRGIN ISLANDS')
gulf <- c('AL', 'ALABAMA', 'LA', 'LOUISIANA', 'MS', 'MISSISSIPPI', 'TX', 'TEXAS',
          'FL-W')
# We are adding a Great Lakes region that is city-based, not state-based like
# the FEUS. State exceptions include OH and MI, which are considered great
# lake states
grlake <- c('OH', 'OHIO', 'MI', 'MICHIGAN')
# great lakes cities are defined as cities within 75 miles of the nearest great
# lake
grlake_cities <- great_lakes_cities %>%
  mutate(MILES_TO_LAKE = as.numeric(MILES_TO_LAKE)) %>%
  filter(MILES_TO_LAKE <= 75)

products <- left_join(pp_processed, pp_map) %>%
  # split florida by east and west
  left_join(florida_coast_map %>%
              rename(CITY = PLANT_CITY,
                     FLORIDA_STATE = PLANT_STATE_ABRV) %>%
              select(!c(PLANT_COAST_GEMINI, PLANT_COAST))) %>%
  mutate(STATE = ifelse(!is.na(FLORIDA_STATE), FLORIDA_STATE, STATE)) %>%
  select(!FLORIDA_STATE) %>%
  # add regions
  mutate(REGION = ifelse(STATE %in% norpac, 'North Pacific', NA),
         REGION = ifelse(STATE %in% pac, 'Pacific', REGION),
         REGION = ifelse(STATE %in% pacisl, 'West Pacific', REGION),
         REGION = ifelse(STATE %in% neweng, 'New England', REGION),
         REGION = ifelse(STATE %in% midatl, 'Mid-Atlantic', REGION),
         REGION = ifelse(STATE %in% souatl, 'South Atlantic', REGION),
         REGION = ifelse(STATE %in% gulf, 'Gulf', REGION),
         REGION = ifelse(STATE %in% grlake, 'Great Lakes', REGION),
         REGION = ifelse(STATE %in% grlake_cities$PLANT_STATE_ABRV &
                           CITY %in% grlake_cities$PLANT_CITY,
                         'Great Lakes', REGION),
         # NEW_PRODUCT_FORM will store updated product conditions so that
         # PRODUCT_FORM can retain prior, more specific data
         NEW_PRODUCT_FORM = PRODUCT_FORM,
         # OLD_ levels of the classification hierarchy will store initial
          # species classifications that may be set to NA for confidentiality
         OLD_SPECIES_NAME = SPECIES_NAME,
         OLD_SPECIES_GROUP = SPECIES_GROUP,
         OLD_SPECIES_CATEGORY = SPECIES_CATEGORY,
         OLD_ECOLOGICAL_CATEGORY = ECOLOGICAL_CATEGORY,
         # CONFIDENTIAL is a placeholder for labeling data as confidential
         CONFIDENTIAL = NA)

# Filter for only highly migratory species -------------------------------------
# set billfish
billfishes <- c('BLACK MARLIN', 'BLUE MARLIN', 'WHITE MARLIN', 'STRIPED MARLIN', 
                'SAILFISH', 'SWORDFISH', 'SHORTBILL SPEARFISH')
com_landings <- com_landings %>%
  mutate(SPECIES_CATEGORY = ifelse(SPECIES_NAME %in% billfishes, 'BILLFISHES',
                                   SPECIES_CATEGORY))
trade_data <- trade_data %>%
  mutate(SPECIES_CATEGORY = ifelse(SPECIES_NAME %in% billfishes, 'BILLFISHES',
                                   SPECIES_CATEGORY))
products <- products %>%
  mutate(SPECIES_CATEGORY = ifelse(SPECIES_NAME %in% billfishes, 'BILLFISHES',
                                   SPECIES_CATEGORY))
terr_landings <- terr_landings %>%
  mutate(SPECIES_CATEGORY = ifelse(SPECIES_NAME %in% billfishes, 'BILLFISHES',
                                   SPECIES_CATEGORY))

# no need to set tunas, already a category

# set sharks
sharks <- c('BLACKNOSE SHARK', 'BONNETHEAD SHARK', 'FINETOOTH SHARK', 
            'OCEANIC WHITETIP SHARK', 'THRESHER SHARK', 'SHORTFIN MAKO SHARK',
            'SANDBAR SHARK', 'SCALLOPED HAMMERHEAD', 'WHITE SHARK')

com_landings <- com_landings %>%
  mutate(ECOLOGICAL_CATEGORY = ifelse(SPECIES_NAME %in% sharks, 'HIGHLY MIGRATORY SPECIES',
                                      ECOLOGICAL_CATEGORY))

products <- products %>%
  mutate(ECOLOGICAL_CATEGORY = ifelse(SPECIES_NAME %in% sharks, 'HIGHLY MIGRATORY SPECIES',
                                      ECOLOGICAL_CATEGORY))

trade_data <- trade_data %>%
  mutate(ECOLOGICAL_CATEGORY = ifelse(SPECIES_NAME %in% sharks, 'HIGHLY MIGRATORY SPECIES',
                                      ECOLOGICAL_CATEGORY))

terr_landings <- terr_landings %>%
  mutate(ECOLOGICAL_CATEGORY = ifelse(SPECIES_NAME %in% sharks, 'HIGHLY MIGRATORY SPECIES',
                                      ECOLOGICAL_CATEGORY))

# include mahi mahi
com_landings <- com_landings %>%
  mutate(ECOLOGICAL_CATEGORY = ifelse(SPECIES_GROUP == 'DOLPHINFISH', 'HIGHLY MIGRATORY SPECIES',
                                      ECOLOGICAL_CATEGORY))

products <- products %>%
  mutate(ECOLOGICAL_CATEGORY = ifelse(SPECIES_GROUP == 'DOLPHINFISH', 'HIGHLY MIGRATORY SPECIES',
                                      ECOLOGICAL_CATEGORY))

trade_data <- trade_data %>%
  mutate(ECOLOGICAL_CATEGORY = ifelse(SPECIES_GROUP == 'DOLPHINFISH', 'HIGHLY MIGRATORY SPECIES',
                                      ECOLOGICAL_CATEGORY))

terr_landings <- terr_landings %>%
  mutate(ECOLOGICAL_CATEGORY = ifelse(SPECIES_GROUP == 'DOLPHINFISH', 'HIGHLY MIGRATORY SPECIES',
                                      ECOLOGICAL_CATEGORY))


# Filter for the HMS
com_landings <- com_landings %>%
  filter(SPECIES_CATEGORY %in% c('TUNAS', 'BILLFISHES') | 
           ECOLOGICAL_CATEGORY == 'HIGHLY MIGRATORY SPECIES') %>%
  select(!ECOLOGICAL_CATEGORY)

products <- products %>%
  filter(SPECIES_CATEGORY %in% c('TUNAS', 'BILLFISHES') |
           ECOLOGICAL_CATEGORY == 'HIGHLY MIGRATORY SPECIES') %>%
  select(!ECOLOGICAL_CATEGORY)

trade_data <- trade_data %>%
  filter(SPECIES_CATEGORY %in% c('TUNAS', 'BILLFISHES') |
           ECOLOGICAL_CATEGORY == 'HIGHLY MIGRATORY SPECIES') %>%
  select(!ECOLOGICAL_CATEGORY)

terr_landings <- terr_landings %>%
  filter(SPECIES_CATEGORY %in% c('TUNAS', 'BILLFISHES') |
           ECOLOGICAL_CATEGORY == 'HIGHLY MIGRATORY SPECIES') %>%
  select(!ECOLOGICAL_CATEGORY)

# Attach coasts ----------------------------------------------------------------
# Assign coasts by state abbreviation
pacific <- c('CA', 'OR', 'WA', 'AK')
atlantic <- c('ME', 'NH', 'MA', 'RI', 
              'CT', 'DE', 'MD', 'NY', 
              'VA', 'NC', 'SC', 'GA', 
              'NJ', 'PA', 'DC', 'FL-E')
gulf <- c('AL', 'MS', 'LA', 'TX', 'PR', 'VI', 'FL-W', 'FL')
hawaii <- c('HI', 'CM', 'MP', 'GU', 'AS')

products <- products %>%
  # add coasts
  mutate(COAST = ifelse(STATE %in% pacific, 'WEST COAST + ALASKA',
                        ifelse(STATE %in% atlantic, 'ATLANTIC',
                               ifelse(STATE %in% gulf, 'GULF + TERRITORIES',
                                      ifelse(STATE %in% hawaii, 'PACIFIC ISLANDS',
                                             NA)))))
  

# Remove confidential data -----------------------------------------------------
# NOTE: There is no product data from Alaska here, so the effort is less complex
  # than in the main U.S. Seafood Dashboard
overwrite_prodform <- function(data, cols = '', coast = '') {
  # here, data should be processed product data as formatted above 
  # cols is a vector of columns to group by and is defaulted as an empty string;
  # acceptable inputs are columns that exist in the inputted data
  # ORDER MATTERS: the last column included in the vector should be the finest
  # level of resolution within the species hierarchy
  # This is because we need to remove any NAs in that column such that the
  # data only includes products with 
  # coast is defaulted as an empty string; acceptable inputs are specific
  # coasts
  
  # if cols is empty (i.e., no desired columns to group by), set cols to be 
  # NEW_PRODUCT_FORM 
  if ('' %in% cols) {
    cols <- c('NEW_PRODUCT_FORM')
  }
  # Identify what the last column in cols is (this should be the lowest level
  # of the classification hierarchy) and set it as object of type quosure
  # to work in dplyr pipe
  level_filter <- cols[length(cols)]
  level_filter <- as.symbol(level_filter)
  level_filter <- rlang::enquo(level_filter)
  
  # there are two possible ways to munge the data:
  # 1) if no coast is provided
  if (coast == '') {
    data %>%
      # all inputted columns and non-negotiable columns
      # all_of() allows us to use a vector of strings in a dplyr pipe
      select(YEAR, NEW_PRODUCT_FORM, all_of(cols), PLANT_STREET) %>%
      # Some plant addresses are blank or NA, so we don't worry about those for 
      # confidentiality
      # We also don't want any NAs of the finest level of classification - this
      # ensures that we aren't accidentally marking confidential data that
      # is nonspecific to the desired classification level
      # In the event that no columns are provided, this will be NEW_PRODUCT_FORM
      # which will ensure that we leave out products without a provided condition
      filter(PLANT_STREET != '',
             !is.na(PLANT_STREET),
             !is.na(!!level_filter)) %>%
      # remove duplicates from the data so plants are NOT double-counted
      distinct() %>%
      # group by all columns except plant street so we count the number of plants
      # for our desired group of columns
      group_by(across(c(-PLANT_STREET))) %>%
      count() %>%
      # filter for combos that have fewer than 3 plants 
      filter(n < 3) %>%
      ungroup() %>%
      # join back to the original data so now there is an extra column of 'n'
      # this column will have a number for any product found to have less than
      # 3 plants which process said product, and NA for any others
      right_join(data) %>%
      # we now want to overwrite NEW_PRODUCT_FORM to be OTHER for any products
      # listed at less than 3 plants
      # In the event a product was listed at less than 3 plants for one group
      # and more than 3 for another, we do not want to lose that work and overwrite
      # back to the previous product form, so we also check to see if CONFIDENTIAL
      # is NA or not to preserve prior confidentiality checks
      mutate(NEW_PRODUCT_FORM = ifelse(!is.na(n), 'OTHER', 
                                       ifelse(!is.na(CONFIDENTIAL), 'OTHER', PRODUCT_FORM)),
             # we update CONFIDENTIAL to 1 if the product was found to be
             # confidential in the current grouping
             CONFIDENTIAL = ifelse(!is.na(n), 1, CONFIDENTIAL)) %>%
      # remove the 'n' column so now data is same structure as inputted
      select(!n)
    
    # 2) if coast is provided
  } else {
    # we filter for the inputted coast and add the column for coast later
    # for joining
    data %>%
      filter(COAST == coast) %>%
      select(YEAR, NEW_PRODUCT_FORM, all_of(cols), PLANT_STREET) %>%
      filter(PLANT_STREET != '',
             !is.na(PLANT_STREET),
             !is.na(!!level_filter)) %>%
      distinct() %>%
      group_by(across(c(-PLANT_STREET))) %>%
      count() %>%
      filter(n < 3) %>%
      ungroup() %>%
      mutate(COAST = coast) %>%
      right_join(data) %>%
      mutate(NEW_PRODUCT_FORM = ifelse(!is.na(n), 'OTHER', 
                                       ifelse(!is.na(CONFIDENTIAL), 'OTHER', PRODUCT_FORM)),
             CONFIDENTIAL = ifelse(!is.na(n), 1, CONFIDENTIAL)) %>%
      select(!n)
  }
}
# the end result of the above function is a dataset identical in structure to
# the inputted data, with some changes made to NEW_PRODUCT_FORM and CONFIDENTIAL
# the function is designed where it can be used in a pipe and no prior effort
# is lost

# pipe for overwriting product forms to OTHER
overwritten_products <- products %>%
  # first identify data that is confidential without aggregation (i.e., 
  # product conditions that are only processed at 1 or 2 plants)
  overwrite_prodform() %>%
  # FIRST SECTION: Each level of the classification hierarchy without coast
  overwrite_prodform(c('SPECIES_CATEGORY')) %>%
  overwrite_prodform(c('SPECIES_CATEGORY', 'SPECIES_GROUP')) %>%
  overwrite_prodform(c('SPECIES_CATEGORY', 'SPECIES_GROUP', 'SPECIES_NAME')) %>%
  # SECOND SECTION: Each level of the classification hierarchy for EACH coast
  # WEST COAST + ALASKA
  overwrite_prodform(coast = 'WEST COAST + ALASKA') %>%
  overwrite_prodform(c('SPECIES_CATEGORY'), 
                     coast = 'WEST COAST + ALASKA') %>%
  overwrite_prodform(c('SPECIES_CATEGORY', 
                       'SPECIES_GROUP'), coast = 'WEST COAST + ALASKA') %>%
  overwrite_prodform(c('SPECIES_CATEGORY',
                       'SPECIES_GROUP', 'SPECIES_NAME'), coast = 'WEST COAST + ALASKA') %>%
  # ATLANTIC
  overwrite_prodform(coast = 'ATLANTIC') %>%
  overwrite_prodform(c('SPECIES_CATEGORY'), 
                     coast = 'ATLANTIC') %>%
  overwrite_prodform(c('SPECIES_CATEGORY', 
                       'SPECIES_GROUP'), coast = 'ATLANTIC') %>%
  overwrite_prodform(c('SPECIES_CATEGORY',
                       'SPECIES_GROUP', 'SPECIES_NAME'), coast = 'ATLANTIC') %>%
  # PACIFIC ISLANDS
  overwrite_prodform(coast = 'PACIFIC ISLANDS') %>%
  overwrite_prodform(c('SPECIES_CATEGORY'), 
                     coast = 'PACIFIC ISLANDS') %>%
  overwrite_prodform(c('SPECIES_CATEGORY', 
                       'SPECIES_GROUP'), coast = 'PACIFIC ISLANDS') %>%
  overwrite_prodform(c('SPECIES_CATEGORY',
                       'SPECIES_GROUP', 'SPECIES_NAME'), coast = 'PACIFIC ISLANDS') %>%
  # GULF + TERRITORIES
  overwrite_prodform(coast = 'GULF + TERRITORIES') %>%
  overwrite_prodform(c('SPECIES_CATEGORY'), 
                     coast = 'GULF + TERRITORIES') %>%
  overwrite_prodform(c('SPECIES_CATEGORY', 
                       'SPECIES_GROUP'), coast = 'GULF + TERRITORIES') %>%
  overwrite_prodform(c('SPECIES_CATEGORY',
                       'SPECIES_GROUP', 'SPECIES_NAME'), coast = 'GULF + TERRITORIES')

test <- overwritten_products %>%
  mutate(POUNDS = ifelse(is.na(POUNDS), 0, POUNDS))
sum(test$POUNDS[which(test$CONFIDENTIAL == 1)]) / sum(test$POUNDS)

changed_product_forms <- overwritten_products %>%
  filter(CONFIDENTIAL == 1) %>%
  select(!CONFIDENTIAL)

# remove confidential markers
overwritten_products <- overwritten_products %>%
  mutate(CONFIDENTIAL = NA)

# The next step is to identify which products, after attempting to consolidate
# into less specific product conditions, are confidential. For these, we will
# attempt to consolidate into less specific species classifications
declassify_species <- function(data, coast = '') {
  # data is a dataset of processed products
  # coast is an empty character string by default that accepts a string of a 
  # desired coast formatted as is in 'data'
  
  # The process is four steps, one for each level of the species hierarchy
  # Each step consists of isolating necessary columns for aggregation, counting
  # how many plants process the product, and overwriting species classifications
  # The distinction in each step lies in the specific level getting isolated
  # and then overwritten. 
  if (coast == '') {
    step1 <- data %>%
      select(YEAR, NEW_PRODUCT_FORM, SPECIES_CATEGORY,
             SPECIES_GROUP, SPECIES_NAME, PLANT_STREET) %>%
      # Because users can't select NA species name, only take products with 
      # a provided species name
      filter(!is.na(SPECIES_NAME)) %>%
      distinct() %>%
      group_by(YEAR, NEW_PRODUCT_FORM, SPECIES_CATEGORY,
               SPECIES_GROUP, SPECIES_NAME) %>%
      count() %>%
      ungroup() %>%
      filter(n < 3) %>%
      mutate(CONFIDENTIAL = 1) %>%
      # rejoin back to original data with CONFIDENTIAL removed (clean join)
      right_join(data %>% select(!CONFIDENTIAL)) %>%
      # STATE OF ALASKA represnts many processors, so they are not confidential
      # For any confidentially marked products, remove the assigned species name
      mutate(CONFIDENTIAL = ifelse(CITY == 'STATE OF ALASKA', NA, CONFIDENTIAL),
             SPECIES_NAME = ifelse(!is.na(CONFIDENTIAL), NA, SPECIES_NAME)) %>%
      # remove n
      select(!n)
    
    step2 <- step1 %>%
      select(YEAR, NEW_PRODUCT_FORM, SPECIES_CATEGORY,
             SPECIES_GROUP, SPECIES_NAME, PLANT_STREET) %>%
      # Now we only want products for which there is no species name but a 
      # species group
      filter(is.na(SPECIES_NAME),
             !is.na(SPECIES_GROUP)) %>%
      distinct() %>%
      group_by(YEAR, NEW_PRODUCT_FORM, SPECIES_CATEGORY,
               SPECIES_GROUP, SPECIES_NAME) %>%
      count() %>%
      ungroup() %>%
      filter(n < 3) %>%
      mutate(CONFIDENTIAL = 1) %>%
      right_join(step1 %>% select(!CONFIDENTIAL)) %>%
      mutate(CONFIDENTIAL = ifelse(CITY == 'STATE OF ALASKA', NA, CONFIDENTIAL),
             SPECIES_GROUP = ifelse(!is.na(CONFIDENTIAL), NA, SPECIES_GROUP)) %>%
      select(!n)
    
    step3 <- step2 %>%
      select(YEAR, NEW_PRODUCT_FORM, SPECIES_CATEGORY,
             SPECIES_GROUP, SPECIES_NAME, PLANT_STREET) %>%
      # only products for which there is no species name nor group, but a 
      # species category
      filter(is.na(SPECIES_NAME),
             is.na(SPECIES_GROUP),
             !is.na(SPECIES_CATEGORY)) %>%
      distinct() %>%
      group_by(YEAR, NEW_PRODUCT_FORM, SPECIES_CATEGORY,
               SPECIES_GROUP, SPECIES_NAME) %>%
      count() %>%
      ungroup() %>%
      filter(n < 3) %>%
      mutate(CONFIDENTIAL = 1) %>%
      right_join(step2 %>% select(!CONFIDENTIAL)) %>%
      mutate(CONFIDENTIAL = ifelse(CITY == 'STATE OF ALASKA', NA, CONFIDENTIAL),
             SPECIES_CATEGORY = ifelse(!is.na(CONFIDENTIAL), NA, SPECIES_CATEGORY)) %>%
      select(!n)
    
  } else {
    # Here, the only differences from the steps above are filtering for the
    # desired region in the data
    
    step1 <- data %>%
      filter(COAST == coast) %>%
      select(YEAR, NEW_PRODUCT_FORM, SPECIES_CATEGORY,
             SPECIES_GROUP, SPECIES_NAME, PLANT_STREET) %>%
      filter(!is.na(SPECIES_NAME)) %>%
      distinct() %>%
      group_by(YEAR, NEW_PRODUCT_FORM, SPECIES_CATEGORY,
               SPECIES_GROUP, SPECIES_NAME) %>%
      count() %>%
      ungroup() %>%
      filter(n < 3) %>%
      mutate(CONFIDENTIAL = 1,
             COAST = coast) %>%
      right_join(data %>% select(!CONFIDENTIAL)) %>%
      mutate(SPECIES_NAME = ifelse(!is.na(CONFIDENTIAL), NA, SPECIES_NAME)) %>%
      select(!n)
    
    step2 <- step1 %>%
      filter(COAST == coast) %>%
      select(YEAR, NEW_PRODUCT_FORM, SPECIES_CATEGORY,
             SPECIES_GROUP, SPECIES_NAME, PLANT_STREET) %>%
      filter(is.na(SPECIES_NAME),
             !is.na(SPECIES_GROUP)) %>%
      distinct() %>%
      group_by(YEAR, NEW_PRODUCT_FORM, SPECIES_CATEGORY,
               SPECIES_GROUP, SPECIES_NAME) %>%
      count() %>%
      ungroup() %>%
      filter(n < 3) %>%
      mutate(CONFIDENTIAL = 1,
             COAST = coast) %>%
      right_join(step1 %>% select(!CONFIDENTIAL)) %>%
      mutate(SPECIES_GROUP = ifelse(!is.na(CONFIDENTIAL), NA, SPECIES_GROUP)) %>%
      select(!n)
    
    step3 <- step2 %>%
      filter(COAST == coast) %>%
      select(YEAR, NEW_PRODUCT_FORM, SPECIES_CATEGORY,
             SPECIES_GROUP, SPECIES_NAME, PLANT_STREET) %>%
      filter(is.na(SPECIES_NAME),
             is.na(SPECIES_GROUP),
             !is.na(SPECIES_CATEGORY)) %>%
      distinct() %>%
      group_by(YEAR, NEW_PRODUCT_FORM, SPECIES_CATEGORY,
               SPECIES_GROUP, SPECIES_NAME) %>%
      count() %>%
      ungroup() %>%
      filter(n < 3) %>%
      mutate(CONFIDENTIAL = 1,
             COAST = coast) %>%
      right_join(step2 %>% select(!CONFIDENTIAL)) %>%
      mutate(SPECIES_CATEGORY = ifelse(!is.na(CONFIDENTIAL), NA, SPECIES_CATEGORY)) %>%
      select(!n)
    
  }
  
  return(step3)
}

# pipe for declassifying species
declassified_products <- declassify_species(overwritten_products) %>%
  declassify_species('ATLANTIC') %>%
  declassify_species('GULF + TERRITORIES') %>%
  declassify_species('WEST COAST + ALASKA') %>%
  declassify_species('PACIFIC ISLANDS')

# store declassified species in separate object
species_declassified_products <- declassified_products %>%
  filter((is.na(SPECIES_NAME) & !is.na(OLD_SPECIES_NAME)) |
           (is.na(SPECIES_GROUP) & !is.na(OLD_SPECIES_GROUP)) |
           (is.na(SPECIES_CATEGORY) & !is.na(OLD_SPECIES_CATEGORY)))

# remove confidential markers
declassified_products <- declassified_products %>%
  mutate(CONFIDENTIAL = NA)

# The next step is to identify which products, after attempting to consolidate
# into less specific product conditions, are confidential (with a function)
set_confids <- function(data, cols = '', coast = '') {
  # this function is nearly identical to overwrite_prodforms
  # data can be raw products data or that formatted by overwrite_prodforms
  # cols are columns to group the data by
  # coast is an empty string that accepts specific coasts as strings
  
  # the only difference between set_confids and overwrite_prodforms is the 
  # absence of changing product forms to other. Instead, any products 
  # identified to less than 3 plants will have CONFIDENTIAL as 1
  
  if ('' %in% cols) {
    cols <- c('NEW_PRODUCT_FORM')
  }
  
  level_filter <- cols[length(cols)]
  level_filter <- as.symbol(level_filter)
  level_filter <- rlang::enquo(level_filter)
  
  if (coast == '') {
    data %>%
      select(YEAR, NEW_PRODUCT_FORM, all_of(cols), PLANT_STREET) %>%
      filter(PLANT_STREET != '',
             !is.na(PLANT_STREET),
             !is.na(!!level_filter)) %>%
      distinct() %>%
      group_by(across(c(-PLANT_STREET))) %>%
      count() %>%
      filter(n < 3) %>%
      ungroup() %>%
      right_join(data) %>%
      mutate(CONFIDENTIAL = ifelse(!is.na(n), 1, CONFIDENTIAL)) %>%
      select(!n)
  } else {
    data %>%
      filter(COAST == coast) %>%
      select(YEAR, NEW_PRODUCT_FORM, all_of(cols), PLANT_STREET) %>%
      filter(PLANT_STREET != '',
             !is.na(PLANT_STREET),
             !is.na(!!level_filter)) %>%
      distinct() %>%
      group_by(across(c(-PLANT_STREET))) %>%
      count() %>%
      filter(n < 3) %>%
      ungroup() %>%
      mutate(COAST = coast) %>%
      right_join(data) %>%
      mutate(CONFIDENTIAL = ifelse(!is.na(n), 1, CONFIDENTIAL)) %>%
      select(!n)
  }
}

# pipe for identifying confidential products
products_marked <- declassified_products %>%
  set_confids() %>%
  # FIRST SECTION: Each level of the classification hierarchy without coast
  set_confids(c('SPECIES_CATEGORY')) %>%
  set_confids(c('SPECIES_CATEGORY', 
                'SPECIES_GROUP')) %>%
  set_confids(c('SPECIES_CATEGORY',
                'SPECIES_GROUP', 'SPECIES_NAME')) %>%
# SECOND SECTION: Each level of the classification hierarchy for EACH coast
# WEST COAST + ALASKA
set_confids(coast = 'WEST COAST + ALASKA') %>%
  set_confids(c('SPECIES_CATEGORY'), 
              coast = 'WEST COAST + ALASKA') %>%
  set_confids(c('SPECIES_CATEGORY', 
                'SPECIES_GROUP'), coast = 'WEST COAST + ALASKA') %>%
  set_confids(c('SPECIES_CATEGORY',
                'SPECIES_GROUP', 'SPECIES_NAME'), coast = 'WEST COAST + ALASKA') %>%
  # ATLANTIC
  set_confids(coast = 'ATLANTIC') %>%
  set_confids(c('SPECIES_CATEGORY'), 
              coast = 'ATLANTIC') %>%
  set_confids(c('SPECIES_CATEGORY', 
                'SPECIES_GROUP'), coast = 'ATLANTIC') %>%
  set_confids(c('SPECIES_CATEGORY',
                'SPECIES_GROUP', 'SPECIES_NAME'), coast = 'ATLANTIC') %>%
  # PACIFIC ISLANDS
  set_confids(coast = 'PACIFIC ISLANDS') %>%
  set_confids(c('SPECIES_CATEGORY'), 
              coast = 'PACIFIC ISLANDS') %>%
  set_confids(c('SPECIES_CATEGORY', 
                'SPECIES_GROUP'), coast = 'PACIFIC ISLANDS') %>%
  set_confids(c('SPECIES_CATEGORY',
                'SPECIES_GROUP', 'SPECIES_NAME'), coast = 'PACIFIC ISLANDS') %>%
  # GULF + TERRITORIES
  set_confids(coast = 'GULF + TERRITORIES') %>%
  set_confids(c('SPECIES_CATEGORY'), 
              coast = 'GULF + TERRITORIES') %>%
  set_confids(c('SPECIES_CATEGORY', 
                'SPECIES_GROUP'), coast = 'GULF + TERRITORIES') %>%
  set_confids(c('SPECIES_CATEGORY',
                'SPECIES_GROUP', 'SPECIES_NAME'), coast = 'GULF + TERRITORIES')

test <- products_marked %>%
  mutate(POUNDS = ifelse(is.na(POUNDS), 0, POUNDS))

sum(test$POUNDS[which(test$CONFIDENTIAL == 1)]) / sum(test$POUNDS)

# store confidential products in separate objects
confidential_products <- products_marked %>%
  filter(CONFIDENTIAL == 1)

# Attach coasts ----------------------------------------------------------------
# Assign coasts by state abbreviation
pacific <- c('CA', 'OR', 'WA', 'AK')
atlantic <- c('ME', 'NH', 'MA', 'RI', 
              'CT', 'DE', 'MD', 'NY', 
              'VA', 'NC', 'SC', 'GA', 
              'NJ', 'PA', 'DC', 'FL-E')
gulf <- c('AL', 'MS', 'LA', 'TX', 'PR', 'VI', 'FL-W', 'FL')
hawaii <- c('HI', 'CM', 'MP', 'GU', 'AS')

trade_data <- trade_data %>%
  mutate(STATE = ifelse(US_CUSTOMS_DISTRICT %in% c('LOW VALUE SHIPMENT',
                                                   'U.S. VIRGIN ISLANDS',
                                                   'MAIL SHIPMENT'),
                        NA, STATE),
         STATE = ifelse(US_CUSTOMS_DISTRICT == 'U.S. VIRGIN ISLANDS', 'VI',
                        STATE),
         COAST = ifelse(STATE %in% pacific, 'WEST COAST + ALASKA',
                        ifelse(STATE %in% atlantic, 'ATLANTIC',
                               ifelse(STATE %in% gulf, 'GULF + TERRITORIES', 
                                      ifelse(STATE %in% hawaii, 'HAWAII', NA)))))

# Recreate pp data
pp_data <- products_marked %>%
  mutate(POUNDS = ifelse(is.na(POUNDS), 0, POUNDS),
         KG = POUNDS * 0.45359237,
         KG = ifelse(is.na(KG), 0, KG),
         DOLLARS = ifelse(is.na(DOLLARS), 0, DOLLARS)) %>%
  arrange(YEAR, SPECIES_NAME, NEW_PRODUCT_FORM, PRODUCT_FORM) %>%
  # reorder columns so species is left of PRODUCT_FORM for ease of viewing
  select(YEAR, SPECIES_NAME, SPECIES_GROUP, SPECIES_CATEGORY, STATE, CITY,
         NEW_PRODUCT_FORM, PRODUCT_FORM, POUNDS, DOLLARS, KG, COAST,
         CONFIDENTIAL) %>%
  left_join(def_index %>% select(YEAR, INDEX)) %>%
  mutate(DOLLARS_2024 = DOLLARS * INDEX,
         DOLLARS_PER_LB = DOLLARS / POUNDS,
         DOLLARS_PER_KG = DOLLARS / KG,
         DOLLARS_2024_PER_LB = DOLLARS_2024 / POUNDS,
         DOLLARS_2024_PER_KG = DOLLARS_2024 / KG) %>%
  select(-INDEX) %>%
  # mark confidential records' values as 0
  mutate(CONFIDENTIAL = ifelse(is.na(CONFIDENTIAL), 0, CONFIDENTIAL),
         POUNDS = ifelse(CONFIDENTIAL == 1, 0, POUNDS),
         DOLLARS = ifelse(CONFIDENTIAL == 1, 0, DOLLARS),
         KG = ifelse(CONFIDENTIAL == 1, 0, KG),
         DOLLARS_2024 = ifelse(CONFIDENTIAL == 1, 0, DOLLARS_2024),
         DOLLARS_PER_LB = ifelse(CONFIDENTIAL == 1, 0, DOLLARS_PER_LB),
         DOLLARS_PER_KG = ifelse(CONFIDENTIAL == 1, 0, DOLLARS_PER_KG),
         DOLLARS_2024_PER_LB = ifelse(CONFIDENTIAL == 1, 0, DOLLARS_2024_PER_LB),
         DOLLARS_2024_PER_KG = ifelse(CONFIDENTIAL == 1, 0, DOLLARS_2024_PER_KG)) %>%
  select(!c(CITY, STATE, DOLLARS_PER_LB, DOLLARS_PER_KG, DOLLARS_2024_PER_LB,
            DOLLARS_2024_PER_KG)) %>%
  group_by(YEAR, SPECIES_NAME, SPECIES_GROUP, SPECIES_CATEGORY, COAST,
           PRODUCT_FORM, CONFIDENTIAL) %>%
  summarise(across(where(is.numeric), sum),
            .groups ='drop') %>%
  mutate(DOLLARS_PER_LB = DOLLARS / POUNDS,
         DOLLARS_PER_KG = DOLLARS / KG,
         DOLLARS_2024_PER_LB = DOLLARS_2024 / POUNDS,
         DOLLARS_2024_PER_KG = DOLLARS_2024 / KG,
         DOLLARS_PER_LB = ifelse(CONFIDENTIAL == 1, 0, DOLLARS_PER_LB),
         DOLLARS_PER_KG = ifelse(CONFIDENTIAL == 1, 0, DOLLARS_PER_KG),
         DOLLARS_2024_PER_LB = ifelse(CONFIDENTIAL == 1, 0, DOLLARS_2024_PER_LB),
         DOLLARS_2024_PER_KG = ifelse(CONFIDENTIAL == 1, 0, DOLLARS_2024_PER_KG))

pacific <- c('CALIFORNIA', 'OREGON', 'WASHINGTON', 'ALASKA')
hawaii <- c('HAWAII', 'AMERICAN SAMOA', 'NORTHERN MARIANA IS.', 'GUAM',
            'PACIFIC_ISLAND_TERRITORIES')
atlantic <- c('MAINE', 'NEW HAMPSHIRE', 'MASSACHUSETTS', 'RHODE ISLAND', 
              'CONNECTICUT', 'DELAWARE', 'MARYLAND', 'NEW YORK', 
              'VIRGINIA', 'NORTH CAROLINA', 'SOUTH CAROLINA', 'GEORGIA', 
              'FLORIDA-EAST', 'NEW JERSEY', 'PENNSYLVANIA')
gulf <- c('ALABAMA', 'MISSISSIPPI', 'LOUISIANA', 'TEXAS', 'FLORIDA-WEST',
          'U.S. VIRGIN IS.', 'PUERTO RICO')

com_landings <- com_landings %>%
  bind_rows(terr_landings) %>%
  mutate(COAST = ifelse(STATE %in% pacific, 'WEST COAST + ALASKA',
                        ifelse(STATE %in% atlantic, 'ATLANTIC',
                               ifelse(STATE %in% gulf, 'GULF + TERRITORIES', 
                                      ifelse(STATE %in% hawaii, 'PACIFIC ISLANDS',
                                             NA)))))



# Attach territorial and commercial landings -----------------------------------
# terr landings do not have columns of COLLECTION, DOLLARS_2024_PER_KG, 
  # DOLLARS_2024_PER_LB, SOURCE, TSN; we only need the price columns here
com_landings <- com_landings %>%
  select(!c(COLLECTION, TSN, SOURCE))

terr_landings <- terr_landings %>%
  mutate(DOLLARS = ifelse(is.na(DOLLARS), 0, DOLLARS),
         POUNDS = ifelse(is.na(POUNDS), 0, POUNDS),
         METRIC_TONS = ifelse(is.na(METRIC_TONS), 0, METRIC_TONS),
         DOLLARS_2024 = ifelse(is.na(DOLLARS_2024), 0, DOLLARS_2024),
         KG = ifelse(is.na(KG), 0, KG),
         DOLLARS_2024_PER_KG = DOLLARS_2024 / KG,
         DOLLARS_2024_PER_LB = DOLLARS_2024 / POUNDS)

landings <- bind_rows(com_landings, terr_landings)
# Collection, Dollars 2024 per kg, dollars 2024 per lb, source, tsn
# Rename Coasts ----------------------------------------------------------------
landings <- landings %>%
  mutate(COAST = ifelse(COAST == 'PACIFIC', 'PACIFIC ISLANDS', COAST),
         COAST = str_to_title(COAST))

trade_data <- trade_data %>%
  mutate(COAST = ifelse(COAST == 'HAWAII', 'PACIFIC ISLANDS', COAST),
         COAST = str_to_title(COAST))

pp_data <- pp_data %>%
  mutate(COAST = str_to_title(COAST))

# Export Data ------------------------------------------------------------------
# Remove ECOLOGICAL_CATEGORY due to lack of info (they are all HMS)
  # also change NAs to Species Name Not Provided
trade_data <- trade_data %>%
  mutate(SPECIES_GROUP = ifelse(is.na(SPECIES_GROUP), 'SPECIES GROUP NOT PROVIDED',
                                SPECIES_GROUP),
         SPECIES_NAME = ifelse(is.na(SPECIES_NAME), 'SPECIES NAME NOT PROVIDED',
                               SPECIES_NAME))
pp_data <- pp_data %>%
  mutate(SPECIES_GROUP = ifelse(is.na(SPECIES_GROUP), 'SPECIES GROUP NOT PROVIDED',
                                SPECIES_GROUP),
         SPECIES_NAME = ifelse(is.na(SPECIES_NAME), 'SPECIES NAME NOT PROVIDED',
                               SPECIES_NAME))
landings <- landings %>%
  mutate(SPECIES_GROUP = ifelse(is.na(SPECIES_GROUP), 'SPECIES GROUP NOT PROVIDED',
                                SPECIES_GROUP),
         SPECIES_NAME = ifelse(is.na(SPECIES_NAME), 'SPECIES NAME NOT PROVIDED',
                               SPECIES_NAME))


file_name <- paste0('hms_data_munge_',
                    format(Sys.Date(), '%m_%d_%y'),
                    '.RData')

save(list = c('trade_data', 'landings', 'pp_data'),
     file = file_name)
