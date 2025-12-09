# Munge Seafood Dashboard data for HMS Analysis

if(!require("tidyverse")) install.packages("tidyverse")


# Load in data -----------------------------------------------------------------
# load in seafood data
load('seafood_trade_data_munge_11_24_25.RData')

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

processed_confids <- left_join(pp_processed, pp_map) %>%
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
                         'Great Lakes', REGION))
# Pacific Confidentials
confid_pacific <- processed_confids %>%
  filter(REGION == 'Pacific') %>%
  select(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
         PRODUCT_FORM, PLANT_STREET) %>%
  group_by(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
           PRODUCT_FORM) %>%
  count() %>%
  mutate(REGION = 'Pacific')
# West Pacific Confidentials
confid_westpacific <- processed_confids %>%
  filter(REGION == 'West Pacific') %>%
  select(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
         PRODUCT_FORM, PLANT_STREET) %>%
  group_by(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
           PRODUCT_FORM) %>%
  count() %>%
  mutate(REGION = 'West Pacific')
# North Pacific Confidentials
confid_norpac <- processed_confids %>%
  filter(REGION == 'North Pacific') %>%
  select(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
         PRODUCT_FORM, PLANT_STREET) %>%
  group_by(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
           PRODUCT_FORM) %>%
  count() %>%
  mutate(REGION = 'North Pacific')
# New England Confidentials
confid_newengland <- processed_confids %>%
  filter(REGION == 'New England') %>%
  select(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
         PRODUCT_FORM, PLANT_STREET) %>%
  group_by(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
           PRODUCT_FORM) %>%
  count() %>%
  mutate(REGION = 'New England')
# Mid-Atlantic Confidentials
confid_midatlantic <- processed_confids %>%
  filter(REGION == 'Mid-Atlantic') %>%
  select(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
         PRODUCT_FORM, PLANT_STREET) %>%
  group_by(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
           PRODUCT_FORM) %>%
  count() %>%
  mutate(REGION = 'Mid-Atlantic')
# South Atlantic Confidentials
confid_southatlantic <- processed_confids %>%
  filter(REGION == 'South Atlantic') %>%
  select(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
         PRODUCT_FORM, PLANT_STREET) %>%
  group_by(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
           PRODUCT_FORM) %>%
  count() %>%
  mutate(REGION = 'South Atlantic')
# Gulf Confidentials
confid_gulf <- processed_confids %>%
  filter(REGION == 'Gulf') %>%
  select(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
         PRODUCT_FORM, PLANT_STREET) %>%
  group_by(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
           PRODUCT_FORM) %>%
  count() %>%
  mutate(REGION = 'Gulf')
# Great Lakes Confidentials
confid_greatlakes <- processed_confids %>%
  filter(REGION == 'Great Lakes') %>%
  select(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
         PRODUCT_FORM, PLANT_STREET) %>%
  group_by(ECOLOGICAL_CATEGORY, SPECIES_CATEGORY, SPECIES_GROUP, SPECIES_NAME,
           PRODUCT_FORM) %>%
  count() %>%
  mutate(REGION = 'Great Lakes')
# combine
confid_products <- rbind(confid_pacific, confid_westpacific, confid_norpac,
                         confid_newengland, confid_midatlantic, confid_southatlantic,
                         confid_gulf, confid_greatlakes) %>%
  ungroup() %>%
  filter(n < 3) %>%
  select(!n) %>%
  mutate(CONFIDENTIAL = 1)

  
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
pp_data <- pp_processed %>%
  # connect groups from map
  left_join(pp_map) %>%
  mutate(YEAR = as.numeric(YEAR),
         POUNDS = as.numeric(gsub(',', '', POUNDS)),
         DOLLARS = as.numeric(gsub(',', '', DOLLARS)),
         # convert pounds to kilograms in separate column
         KG = POUNDS * 0.45359237) %>%
  arrange(YEAR, SPECIES_NAME, PRODUCT_FORM) %>%
  # reorder columns so species is left of PRODUCT_FORM for ease of viewing
  select(YEAR, SPECIES_NAME, SPECIES_GROUP, SPECIES_CATEGORY, STATE, CITY,
         ECOLOGICAL_CATEGORY, PRODUCT_FORM, POUNDS, DOLLARS, KG) %>%
  left_join(def_index %>% select(YEAR, INDEX)) %>%
  mutate(DOLLARS_2024 = DOLLARS * INDEX,
         DOLLARS_PER_LB = DOLLARS / POUNDS,
         DOLLARS_PER_KG = DOLLARS / KG,
         DOLLARS_2024_PER_LB = DOLLARS_2024 / POUNDS,
         DOLLARS_2024_PER_KG = DOLLARS_2024 / KG) %>%
  select(-INDEX) %>%
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
                         'Great Lakes', REGION)) %>%
  # add coasts
  mutate(COAST = ifelse(STATE %in% pacific, 'WEST COAST + ALASKA',
                        ifelse(STATE %in% atlantic, 'ATLANTIC',
                               ifelse(STATE %in% gulf, 'GULF + TERRITORIES',
                                      ifelse(STATE %in% hawaii, 'PACIFIC ISLANDS',
                                             NA))))) %>%
  left_join(confid_products) %>%
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
           ECOLOGICAL_CATEGORY, PRODUCT_FORM, REGION, CONFIDENTIAL) %>%
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
pp_data <- pp_data %>%
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

pp_data <- pp_data %>%
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

pp_data <- pp_data %>%
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
  mutate(ECOLOGICAL_CATEGORY = 'HIGHLY MIGRATORY SPECIES')

pp_data <- pp_data %>%
  filter(SPECIES_CATEGORY %in% c('TUNAS', 'BILLFISHES') |
           ECOLOGICAL_CATEGORY == 'HIGHLY MIGRATORY SPECIES') %>%
  mutate(ECOLOGICAL_CATEGORY = 'HIGHLY MIGRATORY SPECIES')

trade_data <- trade_data %>%
  filter(SPECIES_CATEGORY %in% c('TUNAS', 'BILLFISHES') |
           ECOLOGICAL_CATEGORY == 'HIGHLY MIGRATORY SPECIES') %>%
  mutate(ECOLOGICAL_CATEGORY = 'HIGHLY MIGRATORY SPECIES')

terr_landings <- terr_landings %>%
  filter(SPECIES_CATEGORY %in% c('TUNAS', 'BILLFISHES') |
           ECOLOGICAL_CATEGORY == 'HIGHLY MIGRATORY SPECIES') %>%
  mutate(ECOLOGICAL_CATEGORY = 'HIGHLY MIGRATORY SPECIES')


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
  select(!ECOLOGICAL_CATEGORY) %>%
  mutate(SPECIES_GROUP = ifelse(is.na(SPECIES_GROUP), 'SPECIES GROUP NOT PROVIDED',
                                SPECIES_GROUP),
         SPECIES_NAME = ifelse(is.na(SPECIES_NAME), 'SPECIES NAME NOT PROVIDED',
                               SPECIES_NAME))
pp_data <- pp_data %>%
  select(!ECOLOGICAL_CATEGORY) %>%
  mutate(SPECIES_GROUP = ifelse(is.na(SPECIES_GROUP), 'SPECIES GROUP NOT PROVIDED',
                                SPECIES_GROUP),
         SPECIES_NAME = ifelse(is.na(SPECIES_NAME), 'SPECIES NAME NOT PROVIDED',
                               SPECIES_NAME))
landings <- landings %>%
  select(!ECOLOGICAL_CATEGORY) %>%
  mutate(SPECIES_GROUP = ifelse(is.na(SPECIES_GROUP), 'SPECIES GROUP NOT PROVIDED',
                                SPECIES_GROUP),
         SPECIES_NAME = ifelse(is.na(SPECIES_NAME), 'SPECIES NAME NOT PROVIDED',
                               SPECIES_NAME))


file_name <- paste0('hms_data_munge_',
                    format(Sys.Date(), '%m_%d_%y'),
                    '.RData')

save(list = c('trade_data', 'landings', 'pp_data'),
     file = file_name)
