rm(list = ls())

library(plm)
library(tidyverse)
library(readxl)
library(countrycode)
library(WDI)
library(stargazer)
library(car) #for vif

# Steven working directory
# setwd("C:/Users/steve/Dropbox/Tax Havens and Conflict/Paper 3 v2")


# DATA LOADING ----

## IMF: DYADIC FDI DATA ---- 

# fdi_dat_full <- data.frame("investor" = NULL, "Total Investment" = NULL, "investee" = NULL, "dyad_investment" = NULL, "year" = NULL)
# 
# # Using a for loop to read in each separate Excel file from CDIS
# for(i in 2009:2022) {
# 
#   excelname <-  paste0("Data/Raw/CDIS_Table_6_Direct_Investment_Position_", as.character(i), ".xlsx")
#   fdi_dat <- read_excel(excelname, skip = 3, n_max = 246, col_types = "text")
#   fdi_dat_gather <- fdi_dat %>% pivot_longer(2:ncol(fdi_dat), names_to = "investor", values_to = "dyad_investment")
#   fdi_dat_gather$year <- as.numeric(i)
#   fdi_dat_full <- rbind.data.frame(fdi_dat_full, fdi_dat_gather)
#   print(i) # track progress
# 
# }
# 
# # Some values marked as confidential; treating these as missing for this analysis:
# fdi_dat_full$dyad_investment <- with(fdi_dat_full, ifelse(dyad_investment == "C", NA, dyad_investment))
# 
# fdi_dat_full$dyad_investment <- as.numeric(fdi_dat_full$dyad_investment)
#  
# colnames(fdi_dat_full)[1] <- "investee"
# 
# # # Renaming some countries to avoid double matching
# fdi_dat_full[fdi_dat_full$investor == "Aruba, Kingdom of the Netherlands", "investor"] <- "Aruba"
# fdi_dat_full[fdi_dat_full$investor == "Curaçao, Kingdom of the Netherlands", "investor"] <- "Curaçao"
# fdi_dat_full[fdi_dat_full$investor == "Guiana, French", "investor"] <- "French Guyana"
# fdi_dat_full[fdi_dat_full$investor == "Sint Maarten, Kingdom of the Netherlands", "investor"] <- "Sin Maarten"
# fdi_dat_full[fdi_dat_full$investor == "Türkiye, Rep. of", "investor"] <- "Turkey"
# 
# 
# fdi_dat_full[fdi_dat_full$investee == "Aruba, Kingdom of the Netherlands", "investee"] <- "Aruba"
# fdi_dat_full[fdi_dat_full$investee == "Curaçao, Kingdom of the Netherlands", "investee"] <- "Cura?ao"
# fdi_dat_full[fdi_dat_full$investee == "Guiana, French", "investee"] <- "French Guyana"
# fdi_dat_full[fdi_dat_full$investee == "Sint Maarten, Kingdom of the Netherlands", "investee"] <- "Sin Maarten"
# fdi_dat_full[fdi_dat_full$investee == "Türkiye, Rep. of", "investee"] <- "Turkey"
# 
# # NOTE TO STEVEN:
# # Noting some mismatches with countrycode; these countries have few, if any, non-confidential entries in the
# # data as it is. Leaving them out.
# # In countrycode_convert(sourcevar = sourcevar, origin = origin, destination = dest,  :
# # Some values were not matched unambiguously: Kosovo, Rep. of, Netherlands Antilles, US Pacific Islands
# fdi_dat_full |>  filter(investor == "Netherlands Antilles") |>
#   arrange(year, desc = T)
# # # Netherlands Antilles is an aggregate reporting for a single year 2009 that gets broken out into 
# # # individual jurisdictions later. 
# fdi_dat_full |>  filter(investor == "US Pacific Islands" |
#                           investor == "Kosovo, Rep. of") |>
#   arrange(desc(dyad_investment))
# # Generally, the amounts for these jurisdictions are fairly small.
# # Excluding them from the analysis.
# # 
# # # Labeling with iso3c for indexing
# fdi_dat_full$investor.iso3c <- countrycode(fdi_dat_full$investor, "country.name", "iso3c")
# fdi_dat_full$investee.iso3c  <- countrycode(fdi_dat_full$investee, "country.name", "iso3c")
# fdi_dat_full <- fdi_dat_full |> filter(is.na(investee.iso3c) == F)
# 
# rm(list = c("fdi_dat", "fdi_dat_gather"))

# saveRDS(fdi_dat_full, "Data/Cleaned/fdi_dat_full.R")
fdi_dat_full <- readRDS("Data/Cleaned/fdi_dat_full.R")


## ZOROME/IMF: TAX HAVEN INDICATOR  ----
# zorome <- read_excel("Data/Raw/Zorome_haven_list.xlsx")
# zorome$investor.iso3c <- countrycode(zorome$jurisdiction, "country.name", "iso3c")
# zorome <- zorome %>% dplyr::select(-jurisdiction)

# saveRDS(zorome, "Data/Cleaned/zorome.R")
zorome <- readRDS("Data/Cleaned/zorome.R")
zorome <- zorome |> 
  add_row(haven = 1, investor.iso3c = "NLD")


## BINARY CONFLICT INDICATORS ----
# UcdpPrioConflict_v23_1 <- read.csv("Data/Raw/UcdpPrioConflict_v23_1.csv")
# prio_dat <- UcdpPrioConflict_v23_1 %>% filter(year >= 2008)
# prio_dat_cw <-prio_dat %>% filter(type_of_conflict == '3' | type_of_conflict == '4')
# prio_dat_cw <- prio_dat_cw %>% dplyr::select(location, side_a_id, year, intensity_level, type_of_conflict)
# prio_dat_cw <- prio_dat_cw %>% unique()
# prio_dat_cw$investee.iso3c <- countrycode(prio_dat_cw$location, "country.name", "iso3c")
# Drops North Yemen
# prio_dat_cw <- prio_dat_cw %>% select(-c(location))
# prio_dat_cw <- prio_dat_cw |> select(side_a_id, investee.iso3c, year, type_of_conflict, intensity_level)
 # There are duplicate codings because of coding at conflict level
 
# # Getting rid of duplicates at country/year -- taking highest level of intensity
# prio_dat_cw_typ3 <- prio_dat_cw |> filter(type_of_conflict == 3)
# prio_test_plm3 <- pdata.frame(prio_dat_cw_typ3, index = c("investee.iso3c", "year"), stringsAsFactors = FALSE)
# dubbles3 <- data.frame(table(index(prio_test_plm3), useNA = "ifany"))
# dubbles3 <- dubbles3 |> filter(Freq > 1)
 # These are the doubles to be removed by keeping only highest intensity 
# dubbles3$year <- as.numeric(as.character(dubbles3$year))
# prio_dat_cw_typ3 <- left_join(prio_dat_cw_typ3, dubbles3, by = c("investee.iso3c", "year"))
# prio_dat_cw_typ3$intensity_level <- with(prio_dat_cw_typ3, ifelse(is.na(Freq) == F, 2, intensity_level))
# prio_dat_cw_typ3 <- prio_dat_cw_typ3 |> select(-c(Freq))
# prio_dat_cw_typ3 <- unique(prio_dat_cw_typ3)
#                        
# 
# prio_dat_cw_typ4 <- prio_dat_cw |> filter(type_of_conflict == 4)
# prio_test_plm4 <- pdata.frame(prio_dat_cw_typ4, index = c("investee.iso3c", "year"), stringsAsFactors = FALSE)
# dubbles4 <- data.frame(table(index(prio_test_plm4), useNA = "ifany"))
# dubbles4 <- dubbles4 |> filter(Freq > 1)
# dubbles4$year <- as.numeric(as.character(dubbles4$year))
# prio_dat_cw_typ4 <- left_join(prio_dat_cw_typ4, dubbles4, by = c("investee.iso3c", "year"))
# prio_dat_cw_typ4$intensity_level <- with(prio_dat_cw_typ4, ifelse(is.na(Freq) == F, 2, intensity_level))
# prio_dat_cw_typ4 <- prio_dat_cw_typ4 |> select(-c(Freq))
# prio_dat_cw_typ4 <- unique(prio_dat_cw_typ4)
# 
# # Bringing back together with country-year unit level
# prio_full <- full_join(prio_dat_cw_typ3, prio_dat_cw_typ4, by = c("investee.iso3c", "year"))
# colnames(prio_full) <- c("type3_side_a_id","investee.iso3c", "year", "internal", "internal_intensity", "type4_side_a_id", "internationalized", "internationalized_intensity")
# prio_full <- prio_full |> filter(is.na(investee.iso3c) == F)

# rm(list = c("dubbles3", 'dubbles4', 'UcdpPrioConflict_v23_1', 'prio_dat', 
 #     'prio_dat_cw', 'prio_dat_cw_typ3', 'prio_dat_cw_typ4', 
  #    "prio_test_plm4", "prio_test_plm3"))

# prio_full <- prio_full |> filter(is.na(investee.iso3c) == F)
 
# Merge type3_side_a_id and type4_side_a_id into single column, then clean
#prio_full$side_a_id <- with(prio_full, ifelse(is.na(type3_side_a_id) == F, type3_side_a_id, type4_side_a_id))
#prio_full <- prio_full |> select(-c(type3_side_a_id,type4_side_a_id))

# saveRDS(prio_full, "Data/Cleaned/prio_full.R")
prio_full <- readRDS("Data/Cleaned/prio_full.R")

## V-DEM ----
# vdem <- read_rds("Data/Raw/V-Dem-CY-Full+Others-v13.rds")
# vdem <- vdem %>% filter(country_name != "Palestine/Gaza") # dropping to avoid duplication with West Bank
# vdem$iso3c <- countrycode(vdem$country_name, "country.name", "iso3c")
# 
# # NOTE TO STEVEN: The warnings here largely return countries that are
# # either sub-national entities (Bavaria) or are in a civil war
# # (Republic of Vietnam). None of this should affect our analysis,
# # because our panel is so recent.
# 
# vdem <- vdem |> filter(is.na(iso3c) == F)
# 
# # Selecting some variables:
# vdem_select <- vdem %>% dplyr::select(iso3c, year, v2x_polyarchy, v2x_libdem, v2x_partipdem, v2x_delibdem, v2x_egaldem,
#                                       v2x_civlib, v2x_clpol, v2x_clpriv)
# 
# saveRDS(vdem_select, "Data/Cleaned/vdem_select.R")
vdem_select <- readRDS("Data/Cleaned/vdem_select.R")

## WDI ----
# wdi_dat <- WDI(indicator = c("NY.GDP.PCAP.KD", # GDP per capita constant USD
#                              "NY.GDP.MKTP.KD.ZG",   # GDP growth, annual %
#                              "NY.GDP.TOTL.RT.ZS",  # Natural resource rents
#                              "BN.KLT.DINV.CD", # Net FDI stocks, current USD
#                              "NY.GDP.MKTP.CD", # GDP current USD
#                              "NY.GDP.MKTP.KD", # GDP constant USD
#                              "NE.TRD.GNFS.ZS", # Trade % GDP
#                              "SP.POP.TOTL",  # Total population
#                              "BX.KLT.DINV.WD.GD.ZS", # Net FDI inflow,  % GDP
#                              "GC.XPN.TOTL.GD.ZS", # Government expense, % GDP
#                              "DT.ODA.ODAT.GN.ZS", # Net ODA received, % GNI
#                              "GC.TAX.TOTL.GD.ZS" # Tax revenue, % GDP
#                              ),
# extra = T) # include extra information
# 
# wdi_dat <- filter(wdi_dat, region != "Aggregates") # removes aggregate regions from countries
# wdi_dat$iso3c <- countrycode(wdi_dat$iso3c, "iso3c", "iso3c") # will use ISO3C code as base, but WB coding is slightly different (according to countrycode package)
# # NOTE TO STEVEN: Per usual, you can ignore these warnings. One is Kosovo,
# # the other is... I think the Channel Islands.
# 
# # Renaming WDI variables for intuition
# names(wdi_dat)[which(names(wdi_dat) == "NY.GDP.PCAP.KD")] <- "wdi_gdppc_cusd"
# names(wdi_dat)[which(names(wdi_dat) == "NY.GDP.MKTP.KD.ZG")] <- "wdi_gdp_growth_perc"
# names(wdi_dat)[which(names(wdi_dat) == "NY.GDP.TOTL.RT.ZS")] <- "wdi_resource_rents"
# names(wdi_dat)[which(names(wdi_dat) == "BN.KLT.DINV.CD")] <- "wdi_fdi_curr"
# names(wdi_dat)[which(names(wdi_dat) == "NY.GDP.MKTP.CD")] <- "wdi_gdp_curr"
# names(wdi_dat)[which(names(wdi_dat) == "NY.GDP.MKTP.KD")] <- "wdi_gdp_cusd"
# names(wdi_dat)[which(names(wdi_dat) == "NE.TRD.GNFS.ZS")] <- "wdi_trade_gdp"
# names(wdi_dat)[which(names(wdi_dat) == "SP.POP.TOTL")] <- "wdi_population"
# names(wdi_dat)[which(names(wdi_dat) == "BX.KLT.DINV.WD.GD.ZS")] <- "wdi_net_fdi_gdp"
# names(wdi_dat)[which(names(wdi_dat) == "GC.XPN.TOTL.GD.ZS")] <- "wdi_govt_spend"
# names(wdi_dat)[which(names(wdi_dat) == "DT.ODA.ODAT.GN.ZS")] <- "wdi_oda_net_gni"
# names(wdi_dat)[which(names(wdi_dat) == "GC.TAX.TOTL.GD.ZS")] <- "wdi_taxrev_gdp"
# 
# 
# wdi_dat <- wdi_dat %>% dplyr::select(-c("country", 'capital', 'iso2c', "lending", "income")) # removing unnecessary variables
# wdi_dat$year <- as.numeric(wdi_dat$year) # reclassifying for merge
# 
# wdi_dat <- wdi_dat |> filter(is.na(iso3c) == F)
# 
# saveRDS(wdi_dat, "Data/Cleaned/wdi_dat.R")
wdi_dat <- readRDS("Data/Cleaned/wdi_dat.R")

## UN Voting Similarity ----
# un_ideal <- read.csv("Data/Raw/IdealpointestimatesALL_Jun2022.csv")
# un_ideal$year <- un_ideal$session + 1945 # Sessions don't respond perfectly according to years, but will be lagging in analysis
# un_ideal_select <- un_ideal %>% dplyr::select(iso3c, year, IdealPointAll)
# 
# # Setting aside US ideal points in separate variable
# un_ideal_select_us <- un_ideal_select %>% filter(year >= 2008 & iso3c == "USA")
# un_ideal_select_us$IdealPointUS <- un_ideal_select_us$IdealPointAll
# un_ideal_select_us <- un_ideal_select_us %>% dplyr::select(year, IdealPointUS)
# 
# # Re-merging and creating differenced variable
# un_ideal_select <- left_join(un_ideal_select %>% filter(year >=2008), un_ideal_select_us, by = c("year"))
# un_ideal_select$IdealPointDiff <- with(un_ideal_select, abs(IdealPointAll - IdealPointUS)*-1)

# un_ideal_select <- un_ideal_select %>% dplyr::select(iso3c, year, IdealPointDiff, IdealPointAll)
# saveRDS(un_ideal_select, "Data/Cleaned/un_ideal_select.R")
un_ideal_select <- readRDS("Data/Cleaned/un_ideal_select.R")


##  OECD membership ----
oecd <- read_xlsx(("Data/Raw/OECD_members.xlsx"))
oecd$iso3c <- countrycode(oecd$country.name, "country.name", "iso3c")
oecd <- oecd |> 
  mutate(oecd = 1) |> 
  select(iso3c, oecd)

## EUSANCT data ---- 
# eusanct <- rio::import("Data/Raw/EUSANCT_Dataset_Dyadic_Excel.xls")
# eusanct_eu <- eusanct |> 
#   select(sender, cname, year, threat_dyad, sanction_dyad, potential_sanction, democracy_sanction) |> 
#   pivot_wider(names_from = sender, values_from = threat_dyad:democracy_sanction) |> 
#   distinct()
# eusanct_eu$iso3c <- countrycode(eusanct_eu$cname, "country.name", "iso3c")
# eusanct_eu <- eusanct_eu |> 
#   filter(is.na(iso3c) == F)
# eusanct_eu <- eusanct_eu |> 
#   mutate(
#   threat = case_when(threat_dyad_UN == 1 ~ 1, 
#                           threat_dyad_EU == 1 ~ 1, 
#                           threat_dyad_US == 1 ~ 1), 
#   sanction = case_when(sanction_dyad_EU == 1 ~ 1, 
#                        sanction_dyad_US == 1 ~ 1, 
#                        sanction_dyad_UN == 1 ~ 1), 
#   potential_sanction = case_when(potential_sanction_EU == 1 ~ 1, 
#                                  potential_sanction_US == 1 ~ 1, 
#                                  potential_sanction_UN == 1 ~ 1), 
#   democracy_sanction = case_when(democracy_sanction_EU == 1 ~ 1, 
#                                  democracy_sanction_US == 1 ~ 1, 
#                                  democracy_sanction_UN == 1 ~ 1), 
#   threat = coalesce(threat, 0), 
#   sanction = coalesce(sanction, 0), 
#   potential_sanction = coalesce(potential_sanction, 0), 
#   democracy_sanction = coalesce(democracy_sanction, 0)
# )
# saveRDS(eusanct_eu, "Data/Cleaned/eusanct_eu.R")
eusanct_eu <- readRDS("Data/Cleaned/eusanct_eu.R")


## MERGING DATA ----

# Across the board: removing observations where the 
# index variables are NA

merge_1 <- left_join(fdi_dat_full, zorome, by = "investor.iso3c") # adding binary indicators for investors as tax havens
merge_2 <- left_join(merge_1, prio_full, by = c("investee.iso3c", "year")) # adding conflict variables for investees
merge_3 <- left_join(merge_2, vdem_select, by = c("investee.iso3c" = "iso3c", "year")) # adding conflict variables for investees
merge_4 <- left_join(merge_3, wdi_dat, by = c("investee.iso3c" = "iso3c", "year")) # adding conflict variables for investees
merge_5 <- left_join(merge_4, un_ideal_select, by = c("investee.iso3c" = "iso3c", "year"))
merge_6 <- left_join(merge_5, oecd, by = c("investee.iso3c" = "iso3c"))
merge_7 <- left_join(merge_6, eusanct_eu, by = c("investee.iso3c" = "iso3c", "year"))

merge_final <- merge_7
saveRDS(merge_final, "Data/Cleaned/merge_final.R")


## LOADING DATA ----
merge_final <- readRDS("Data/Cleaned/merge_final.R")
OSV_raw <- readRDS("Data/Cleaned/OneSided_v23_1.rds")

# Binary indicator for civil conflict
merge_final <- merge_final %>% mutate("civ.conflict.binary" = ifelse(is.na(internal) == T & is.na(internationalized) == T, 0, 1))


# Filling in data for civil conflict: 
merge_final <- merge_final |> mutate("internal" = ifelse(is.na(internal) == T, 0, 1), 
                                     "internationalized" = ifelse(is.na(internationalized) == T, 0, 1))

# Filtering OSV data for gov violence, then merging in
# this merge works bc OSV_gov$actor_id is the same ID as prio_dat_cw$side_a_id
OSV_gov <- OSV_raw %>% filter(is_government_actor == 1) %>% filter(year >= 2008)
OSV_gov <- OSV_gov %>% select(conflict_id,dyad_id,actor_id,actor_name,year,best_fatality_estimate,low_fatality_estimate,high_fatality_estimate,location)
OSV_gov$actor_id <- as.character(OSV_gov$actor_id)
merge_final <- left_join(merge_final,OSV_gov,by = c("side_a_id" = "actor_id","year"))
#make binary indicator for OSV, and casualty variable
#fatality estimates return NA if no fatalities that year.
merge_final$gov.OSV.binary <- with(merge_final, ifelse(is.na(best_fatality_estimate) == F, 1, 0))
merge_final$gov.OSV.cas <- with(merge_final, ifelse(is.na(best_fatality_estimate) == F, best_fatality_estimate, 0))



# Creating high and low intensity measures 
merge_final <- merge_final |> 
  mutate("internal.hi" = ifelse(internal == 1 & internal_intensity == 2, 1, 0), 
         "internal.lo" = ifelse(internal == 1 & internal_intensity == 1, 1, 0), 
         "internationalized.hi" = ifelse(internationalized == 1 & internationalized_intensity == 2, 1, 0),
         "internationalized.lo" = ifelse(internationalized == 1 & internationalized_intensity == 1, 1, 0),
         "civ.conflict.binary.hi" = ifelse(internal.hi == 1 | internationalized.hi == 1, 1, 0), 
         "civ.conflict.binary.lo" = ifelse(internal.lo == 1 | internationalized.lo == 1, 1, 0))

#Creating measure for amount of FDI coming from tax havens
# Total investment from all dyads
merge_final <- merge_final %>% 
  group_by(year, investee) %>% 
  mutate("investee.total" = sum(dyad_investment, na.rm = T)) |> 
  ungroup()

# Investment only from tax havens 
merge_final_haven <- merge_final |> 
  group_by(year, investee) |> 
  filter(haven == 1) |> 
  mutate("haven.investee.total" = sum(dyad_investment, na.rm = T)) |> 
  select(year, investee, haven.investee.total) |> 
  ungroup() |> 
  distinct()

# Remerging and dropping dyadic setup
merge_final_flat <- merge_final |> 
  select(-c(dyad_investment, investor, investor.iso3c, haven)) |> 
  distinct()
merge_final_flat <- left_join(merge_final_flat, merge_final_haven, by = c("investee", "year"))
merge_final_flat <- left_join(merge_final_flat, zorome, by = c("investee.iso3c" = "investor.iso3c"))


# Checking on number of negative values
haven_neg <- merge_final_flat[merge_final_flat$haven.investee.total < 0,] 

haven_neg |> 
  count()

haven_neg |> 
  ggplot() + 
  aes(x = haven.investee.total) + 
  geom_histogram()+ 
  theme_bw()


# Non-haven investment total
merge_final_flat$nonhaven.investee.total <- with(merge_final_flat, investee.total- haven.investee.total)

# Standardizing by GDP, current USD
merge_final_flat <-  merge_final_flat |> 
  mutate(haven.investee.total.gdp = haven.investee.total/wdi_gdp_curr, 
         investee.total.gdp = investee.total/wdi_gdp_curr, 
         nonhaven.investee.total.gdp = nonhaven.investee.total/wdi_gdp_curr)

# Making relative to IMF data and to WDI totals
merge_final_flat <-  merge_final_flat |> 
  mutate(relative.investee.total.imf = haven.investee.total/investee.total, 
         relative.investee.total.wdi = haven.investee.total/(wdi_fdi_curr/1000), 
         relative.investee.total.imf.gdp = haven.investee.total.gdp/investee.total.gdp)

# Adding minimum to each to ensure observations below 0 don't get dropped out in logs
merge_final_flat <- merge_final_flat |> 
  mutate(haven.investee.total.shift = haven.investee.total + abs(min(haven.investee.total, na.rm = T)), 
         investee.total.shift = investee.total + abs(min(investee.total, na.rm = T)), 
         nonhaven.investee.total.shift = nonhaven.investee.total + abs(min(nonhaven.investee.total, na.rm = T)), 
         relative.investee.total.shift = haven.investee.total.shift/investee.total.shift)

# Making shift share relative to GDP  
merge_final_flat <- merge_final_flat |> 
  mutate(haven.investee.total.shift.gdp = haven.investee.total.shift/wdi_gdp_curr, 
         investee.total.shift.gdp = investee.total.shift/wdi_gdp_curr, 
         nonhaven.investee.total.shift.gdp = nonhaven.investee.total/wdi_gdp_curr)

# New relative ratios 
merge_final_flat <- merge_final_flat |> 
  mutate(relative.investee.total.shift.gdp = haven.investee.total.shift.gdp/investee.total.shift.gdp, 
         nonh.relative.investee.total.shift.gdp = nonhaven.investee.total.shift.gdp/investee.total.shift.gdp)

# Selecting final variables and ensuring each observation is unique
merge_final_select <- merge_final_flat |> 
  select(investee, investee.iso3c, year, region, 
         investee.total, haven.investee.total, nonhaven.investee.total,  # totals
         investee.total.gdp, haven.investee.total.gdp, nonhaven.investee.total.gdp, # GDP standardized
         haven.investee.total.shift, investee.total.shift, nonhaven.investee.total.shift, # shifted
         haven.investee.total.shift.gdp, investee.total.shift.gdp, nonhaven.investee.total.shift.gdp, # shift standardized
         relative.investee.total.imf, relative.investee.total.wdi, relative.investee.total.imf.gdp, relative.investee.total.shift.gdp, relative.investee.total.shift,  # relative ratios
         civ.conflict.binary, internal, internal_intensity, 
         IdealPointDiff, IdealPointAll, oecd,
         threat, sanction, potential_sanction, democracy_sanction, 
         internationalized, internationalized_intensity,
         internationalized.hi, internal.hi, civ.conflict.binary.hi,
         internationalized.lo, internal.lo, civ.conflict.binary.lo,
         wdi_gdppc_cusd, wdi_gdp_growth_perc, wdi_resource_rents, wdi_trade_gdp, wdi_population, wdi_gdp_curr, wdi_fdi_curr,
         v2x_polyarchy, v2x_libdem, v2x_partipdem, v2x_delibdem, v2x_egaldem, v2x_civlib, v2x_clpriv, v2x_clpol,gov.OSV.binary,gov.OSV.cas) |> 
  distinct()

# Saving as a Pdataframe, enabling panel manipulations
merge_final_pdataframe <- pdata.frame(merge_final_select, index = c("investee", "year"), stringsAsFactors = FALSE)

# Creating lags of key variables 
merge_final_pdataframe$civ.conflict.binary.l1 <- plm::lag(merge_final_pdataframe$civ.conflict.binary, 1, shift = "time")
merge_final_pdataframe$civ.conflict.binary.hi.l1 <- plm::lag(merge_final_pdataframe$civ.conflict.binary.hi, 1, shift = "time")
merge_final_pdataframe$civ.conflict.binary.lo.l1 <- plm::lag(merge_final_pdataframe$civ.conflict.binary.lo, 1, shift = "time")
merge_final_pdataframe$internal.l1 <- plm::lag(merge_final_pdataframe$internal, 1, shift = "time")
merge_final_pdataframe$internationalized.l1 <- plm::lag(merge_final_pdataframe$internationalized, 1, shift = "time")
merge_final_pdataframe$wdi_gdppc_cusd.l1 <- plm::lag(merge_final_pdataframe$wdi_gdppc_cusd, 1, shift = "time")
merge_final_pdataframe$wdi_gdp_growth_perc.l1 <- plm::lag(merge_final_pdataframe$wdi_gdp_growth_perc, 1, shift = "time")
merge_final_pdataframe$wdi_resource_rents.l1 <- plm::lag(merge_final_pdataframe$wdi_resource_rents, 1, shift = "time")
merge_final_pdataframe$wdi_trade_gdp.l1 <- plm::lag(merge_final_pdataframe$wdi_trade_gdp, 1, shift = "time")
merge_final_pdataframe$wdi_population.l1 <- plm::lag(merge_final_pdataframe$wdi_population, 1, shift = "time")
merge_final_pdataframe$v2x_polyarchy.l1 <- plm::lag(merge_final_pdataframe$v2x_polyarchy, 1, shift = "time")
merge_final_pdataframe$v2x_libdem.l1 <- plm::lag(merge_final_pdataframe$v2x_libdem, 1, shift = "time")
merge_final_pdataframe$v2x_partipdem.l1 <- plm::lag(merge_final_pdataframe$v2x_partipdem, 1, shift = "time")
merge_final_pdataframe$v2x_delibdem.l1 <- plm::lag(merge_final_pdataframe$v2x_delibdem, 1, shift = "time")
merge_final_pdataframe$v2x_egaldem.l1 <- plm::lag(merge_final_pdataframe$v2x_egaldem, 1, shift = "time")
merge_final_pdataframe$IdealPointAll.l1 <- plm::lag(merge_final_pdataframe$IdealPointAll, 1, shift = "time")
merge_final_pdataframe$IdealPointDiff.l1 <- plm::lag(merge_final_pdataframe$IdealPointDiff, 1, shift = "time")
merge_final_pdataframe$democracy_sanction.l1 <- plm::lag(merge_final_pdataframe$democracy_sanction, 1, shift = "time")
merge_final_pdataframe$threat.l1 <- plm::lag(merge_final_pdataframe$threat, 1, shift = "time")
merge_final_pdataframe$sanction.l1 <- plm::lag(merge_final_pdataframe$sanction, 1, shift = "time")
merge_final_pdataframe$v2x_civlib.l1 <- plm::lag(merge_final_pdataframe$v2x_civlib, 1, shift = "time")
merge_final_pdataframe$v2x_clpol.l1 <- plm::lag(merge_final_pdataframe$v2x_clpol, 1, shift = "time")
merge_final_pdataframe$v2x_clpriv.l1 <- plm::lag(merge_final_pdataframe$v2x_clpriv, 1, shift = "time")
merge_final_pdataframe$gov.OSV.binary.l1 <- plm::lag(merge_final_pdataframe$gov.OSV.binary, 1, shift = "time")
merge_final_pdataframe$gov.OSV.cas.l1 <- plm::lag(merge_final_pdataframe$gov.OSV.cas, 1, shift = "time")
merge_final_pdataframe$gov.OSV.cas.log <- with(merge_final_pdataframe, ifelse(gov.OSV.cas == 0, 1, log(gov.OSV.cas)))
merge_final_pdataframe$gov.OSV.cas.log.l1 <- plm::lag(merge_final_pdataframe$gov.OSV.cas.log, 1, shift = "time")




# Graphical Exploration  ----
# Graphical depictions of key variables
ggplot(as_tibble(merge_final_pdataframe), aes(x = relative.investee.total.imf/wdi_gdp_curr)) + 
  geom_histogram() 

#this produces figure dv1histo
ggplot(as_tibble(merge_final_pdataframe), aes(x = relative.investee.total.imf.gdp)) + 
  geom_histogram() + xlim(0,2)

#this produces figure dv2histo
ggplot(as_tibble(merge_final_pdataframe), aes(x = log(haven.investee.total + 1))) + 
  geom_histogram()
# Becomes much more normalized with a log in place. 

#this produces figure dv3histo
ggplot(as_tibble(merge_final_pdataframe), aes(x = log(nonhaven.investee.total + 1))) + 
  geom_histogram()

#this produces figure dv4histo
ggplot(as_tibble(merge_final_pdataframe), aes(x = log(investee.total + 1))) + 
  geom_histogram()


## shifted versions, which add the absolute min value
#note that none of these are normally distributed
ggplot(as_tibble(merge_final_pdataframe), aes(x = log(haven.investee.total.shift + 1))) + 
  geom_histogram()

ggplot(as_tibble(merge_final_pdataframe), aes(x = log(nonhaven.investee.total.shift + 1))) + 
  geom_histogram()

ggplot(as_tibble(merge_final_pdataframe), aes(x = log(investee.total.shift + 1))) + 
  geom_histogram()


# Simple bivariate plot: 

merge_final_pdataframe |> 
  as_tibble() |> 
  group_by(civ.conflict.binary.lo) |> 
  summarise(dvmean = mean(relative.investee.total.shift, na.rm = T))


merge_final_pdataframe |> 
  as_tibble() |> 
  ggplot() +
  aes(x = relative.investee.total.shift, color = factor(civ.conflict.binary.lo)) + 
  geom_density() + 
  geom_vline(xintercept = 0.613, color = "red") + 
    geom_vline(xintercept = 0.648, color = "blue") + 
  xlim(0, 1) + 
  theme_bw()


# Time trends in region and/or country: 
merge_final_pdataframe |> 
  as_tibble() |> 
  mutate(year = as.numeric(as.character(year))) |> 
  # filter(region == "Middle East & North Africa") |>
  filter(investee.iso3c == "SYR") |>
  ggplot(aes(x = year)) +
  geom_line(aes(y = relative.investee.total.imf.gdp)) +
  # ylim(0,1) +
  # geom_line(aes(y = haven.investee.total.gdp, color = "red")) +
  # geom_line(aes(y = investee.total.gdp)) +
  # geom_line(aes(y = nonhaven.investee.total.gdp, color = "blue")) +
  
  # geom_line(aes(y = haven.investee.total.shift.gdp/investee.total.shift.gdp, color = "blue")) +
  # geom_line(aes(y = log(wdi_gdp_ppp), color = "green")) +
  geom_rect(aes(xmin = year - 0.5, 
                xmax = year + 0.5,
                ymin = -Inf, ymax = Inf,
                fill = factor(civ.conflict.binary)),
            alpha = 0.2) +
  scale_fill_manual("Civil Conflict", values = c("0" = "white", "1" = "grey50")) + 
  facet_wrap(~ investee.iso3c) + 
  ggthemes::theme_tufte()

# Well if I'm being honest -- this isn't the best evidence, is it? 
merge_final_pdataframe |> 
  as_tibble() |> 
  mutate(year = as.numeric(as.character(year))) |> 
  filter(region == "Sub-Saharan Africa") |> 
  ggplot(aes(x = year, y = log(haven.investee.total + 1))) + 
  geom_line() + 
  geom_rect(aes(xmin = year - 0.5, 
                xmax = year + 0.5,
                ymin = -Inf, ymax = Inf,
                fill = factor(internal)),
            alpha = 0.2) +
  scale_fill_manual(values = c("0" = "white", "1" = "grey50")) + 
  facet_wrap(~ investee.iso3c) + 
  theme_minimal()


# iraq Data Case ----
merge_final_pdataframe |> 
  as_tibble() |> 
  mutate(year = as.numeric(as.character(year))) |> 
  filter(investee.iso3c == "IRQ") |>
  ggplot(aes(x = year)) +
  geom_line(aes(y = relative.investee.total.imf.gdp)) +
  geom_rect(aes(xmin = year - 0.5, 
                xmax = year + 0.5,
                ymin = -Inf, ymax = Inf,
                fill = factor(civ.conflict.binary.hi)),
            alpha = 0.2) +
  scale_fill_manual("Civil War", values = c("0" = "white", "1" = "grey50")) + 
  ylab("Haven FDI as Share of Total FDI") + xlab("Year") + 
  ggthemes::theme_tufte()

# Looks like there is a trendline starting when the conflict starts. 
# It remains durably higher during the conflict, though it varies over time within 
# it. 

# Let's see... what would be a data story we could tell based on Iraq's 
# dyadic FDI partners over this time? 

fdi_dat_full <- readRDS("Data/Cleaned/fdi_dat_full.R")
fdi_dat_dyadic_iraq <- fdi_dat_full |> 
  filter(investee.iso3c == "IRQ") |> 
  group_by(investee, investor) |> 
  mutate(investor_total = sum(dyad_investment, na.rm = T))
fdi_dat_dyadic_iraq_top <- fdi_dat_dyadic_iraq |> 
  select(investee, investor, investor_total) |> 
  distinct() |> 
  ungroup() |> 
  slice_max(investor_total, n = 21) |>
  filter(investor != "Total Investment") 
# These are the top 20 investors in Iraq over the course of the panel

fdi_dat_dyadic_iraq_top$top_investor <- 1
fdi_dat_dyadic_iraq_top <- fdi_dat_dyadic_iraq_top |> 
  select(-investor_total)

# Re-merging to create a time series
fdi_dat_dyadic_iraq <- left_join(fdi_dat_dyadic_iraq, fdi_dat_dyadic_iraq_top, 
                                  by = c("investee", "investor"))
fdi_dat_dyadic_iraq <- fdi_dat_dyadic_iraq |> 
  filter(top_investor == 1)
fdi_dat_dyadic_iraq <- left_join(fdi_dat_dyadic_iraq, zorome, by = "investor.iso3c") 
fdi_dat_dyadic_iraq$haven <- coalesce(fdi_dat_dyadic_iraq$haven, 0)

fdi_dat_dyadic_iraq |> 
  ggplot(aes(x = as.numeric(as.character(year)), y = dyad_investment, color = factor(haven))) +
  geom_line() + 
  facet_wrap(~investor) +
  xlab("Year") + ylab("FDI Inflows") +
  ggthemes::theme_tufte()
  

# Regressions: ----
## Relative haven investment inflows as DV ----

# Dependent variable: relative haven investment as a share of total investment 
# Denominator purely the investment reported by the IMF CDIS
# No shift for any of the components of the DV
# Standardized by GDP in current USD 

reg1 <- lm(relative.investee.total.imf.gdp ~ gov.OSV.binary.l1, 
           data = merge_final_pdataframe |> filter(as.numeric(as.character(year)) < 2020 ))
summary(reg1) # neg not sig

reg2<- lm(relative.investee.total.imf.gdp~ gov.OSV.binary.l1 + factor(region) + factor(year), 
          data = merge_final_pdataframe |> filter(as.numeric(as.character(year)) < 2020 ))
summary(reg2) #neg not sig 
vif(reg2) #fine
reg3 <- lm(relative.investee.total.imf.gdp ~ gov.OSV.binary.l1 + civ.conflict.binary.l1 + wdi_gdppc_cusd.l1 + wdi_gdp_growth_perc.l1 + wdi_population.l1 
            + wdi_trade_gdp.l1 + wdi_resource_rents.l1 + v2x_polyarchy.l1 + v2x_libdem.l1 + v2x_partipdem.l1 + v2x_delibdem.l1 + v2x_egaldem.l1,
            data = merge_final_pdataframe |> filter(as.numeric(as.character(year)) < 2020))
summary(reg3) # neg not sig
vif(reg3) #massive values for all VDEM 
reg4 <- lm(relative.investee.total.imf.gdp ~ gov.OSV.binary.l1 + civ.conflict.binary.l1 + wdi_gdppc_cusd.l1 + wdi_gdp_growth_perc.l1 + wdi_population.l1 
           + wdi_trade_gdp.l1 + wdi_resource_rents.l1 + v2x_polyarchy.l1 + v2x_libdem.l1 + v2x_partipdem.l1 + v2x_delibdem.l1 + v2x_egaldem.l1 
           + factor(region) + factor(year), 
           data = merge_final_pdataframe |> filter(as.numeric(as.character(year)) < 2020 ))
summary(reg4) # neg not sig
vif(reg4) #nothing above 2 except for massive VDEM values


## leaving this code in for future marginal effects plots
#library(marginaleffects)
#plot_slopes(reg4, "civ.conflict.binary.l1", condition = "sanction.l1", rug = T)

## STARGAZER TABLE 1
stargazer(reg1,reg2,reg3,reg4,
          title="Regression Results with Relative Haven Investment Inflows as DV", 
          dep.var.labels = c("Relative Haven Investment Inflows"), 
          covariate.labels = c("Lagged State OSV Indicator",
                               "Lagged Civil Conflict Indicator",
                               "Lagged GDP Per Capita (Constant 2015 Dollars)",
                               "Lagged Annual GDP Growth Percentage",
                               "Lagged Population", 
                               "Lagged Trade as Percentage of GDP",
                               "Lagged Natural Resource Rents as Percentage of GDP",
                               "Lagged Electoral Democracy Index", 
                               "Lagged Liberal Democracy Index", 
                               "Lagged Participatory Democracy Index",
                               "Lagged Deliberative Democracy Index",
                               "Lagged Egalitarian Democracy Index"),
          omit = c("investee", "region", "year"),
          header = F,
          font.size = "small",
          omit.stat = c("f", "ser"),
          star.char = c("*"),
          star.cutoffs = c(.05),
          notes = c("* p<0.05"),
          notes.append = FALSE)


## Haven FDI stocks as DV ---- 
reg5 <- lm(haven.investee.total.gdp ~ gov.OSV.binary.l1, 
           data = merge_final_pdataframe |> filter(as.numeric(as.character(year)) < 2020))
summary(reg5) #neg not sig

reg6<- lm(haven.investee.total.gdp ~ gov.OSV.binary.l1 + factor(region) + factor(year), 
          data = merge_final_pdataframe |> filter(as.numeric(as.character(year)) < 2020))
summary(reg6) #neg not sig
vif(reg6) #fine
reg7 <- lm(haven.investee.total.gdp ~ gov.OSV.binary.l1 + civ.conflict.binary.l1 + wdi_gdppc_cusd.l1 + wdi_gdp_growth_perc.l1 + wdi_population.l1 
            + wdi_trade_gdp.l1 + wdi_resource_rents.l1 + v2x_polyarchy.l1  + v2x_libdem.l1 + v2x_partipdem.l1 + v2x_delibdem.l1 + v2x_egaldem.l1,
           data = merge_final_pdataframe |> filter(as.numeric(as.character(year)) < 2020))
summary(reg7) #pos not sig 
vif(reg7) #VDEM massive, all else =<2
reg8 <- lm(haven.investee.total.gdp ~ gov.OSV.binary.l1 + civ.conflict.binary.l1 + wdi_gdppc_cusd.l1 + wdi_gdp_growth_perc.l1 + wdi_population.l1 
           + wdi_trade_gdp.l1 + wdi_resource_rents.l1 + v2x_polyarchy.l1  + v2x_libdem.l1 + v2x_partipdem.l1 + v2x_delibdem.l1 + v2x_egaldem.l1
           + factor(region) + factor(year), 
           data = merge_final_pdataframe |> filter(as.numeric(as.character(year)) < 2020))
summary(reg8) #pos not sig
vif(reg8) #VDEM massive, factor(region) at 4, all else fine

## STARGAZER TABLE 2

stargazer(reg5,reg6,reg7,reg8,
          title="Regression Results with Haven FDI Stocks as DV", 
          dep.var.labels = c("Logged Haven FDI Stocks"), 
          covariate.labels = c("Lagged State OSV Indicator",
                               "Lagged Civil Conflict Indicator",
                               "Lagged GDP Per Capita (Constant 2015 Dollars)",
                               "Lagged Annual GDP Growth Percentage",
                               "Lagged Population",
                               "Lagged Trade as Percentage of GDP",
                               "Lagged Natural Resource Rents as Percentage of GDP",
                               "Lagged Electoral Democracy Index", 
                               "Lagged Liberal Democracy Index", 
                               "Lagged Participatory Democracy Index", 
                               "Lagged Deliberative Democracy Index", 
                               "Lagged Egalitarian Democracy Index"),
          omit = c("investee","region","year"),
          header = F,
          font.size = "small",
          omit.stat = c("f", "ser"),
          star.char = c("*"),
          star.cutoffs = c(.05),
          notes = c("* p<0.05"),
          notes.append = FALSE,
          digits = 7)

## FDI Stocks from Non-Havens ---- 
reg9 <- lm(log(nonhaven.investee.total + 1) ~ gov.OSV.binary.l1, 
            data = merge_final_pdataframe |> filter(as.numeric(as.character(year)) < 2020))
summary(reg9) # neg not sig
reg10<- lm(log(nonhaven.investee.total + 1) ~ gov.OSV.binary.l1 + 
             factor(region) + factor(year), 
           data = merge_final_pdataframe |> filter(as.numeric(as.character(year)) < 2020))
summary(reg10) # neg not sig
vif(reg10) #fine
reg11 <- lm(log(nonhaven.investee.total + 1) ~ gov.OSV.binary.l1 + civ.conflict.binary.l1 + wdi_gdppc_cusd.l1 + wdi_gdp_growth_perc.l1 + wdi_population.l1 
            + wdi_trade_gdp.l1 + wdi_resource_rents.l1 + v2x_polyarchy.l1 + v2x_libdem.l1 + v2x_partipdem.l1 + v2x_delibdem.l1 + v2x_egaldem.l1, 
            data = merge_final_pdataframe |> filter(as.numeric(as.character(year)) < 2020))
summary(reg11) # neg sig
vif(reg11) #vdem massive, all else fine
reg12 <- lm(log(nonhaven.investee.total + 1) ~ gov.OSV.binary.l1 + civ.conflict.binary.l1 + wdi_gdppc_cusd.l1 + wdi_gdp_growth_perc.l1 + wdi_population.l1 
            + wdi_trade_gdp.l1 + wdi_resource_rents.l1 + v2x_polyarchy.l1 + v2x_libdem.l1 + v2x_partipdem.l1 + v2x_delibdem.l1 + v2x_egaldem.l1
            + factor(region) + factor(year), 
            data = merge_final_pdataframe |> filter(as.numeric(as.character(year)) < 2020))
summary(reg12) #neg not sig
vif(reg12) #vdem massive, factor(region) at 4, all else fine

## STARGAZER TABLE 3

stargazer(reg9,reg10,reg11,reg12,
          title="Regression Results with FDI Stocks from Non-Havens as DV", 
          dep.var.labels = c("Logged FDI Stocks from Non-Havens"), 
          covariate.labels = c("Lagged State OSV Indicator",
                               "Lagged Civil Conflict Indicator",
                               "Lagged GDP Per Capita (Constant 2015 Dollars)",
                               "Lagged Annual GDP Growth Percentage",
                               "Lagged Population",
                               "Lagged Trade as Percentage of GDP",
                               "Lagged Natural Resource Rents as Percentage of GDP",
                               "Lagged Electoral Democracy Index",
                               "Lagged Liberal Democracy Index", 
                               "Lagged Participatory Democracy Index",
                               "Lagged Deliberative Democracy Index",
                               "Lagged Egalitarian Democracy Index"),
          omit = c("investee","region" ,"year"),
          header = F,
          font.size = "small",
          omit.stat = c("f", "ser"),
          star.char = c("*"),
          star.cutoffs = c(.05),
          notes = c("* p<0.05"),
          notes.append = FALSE)



## Total FDI Stocks ---- 
reg13 <- lm(log(investee.total + 1) ~ gov.OSV.binary.l1, 
            data = merge_final_pdataframe |> filter(as.numeric(as.character(year)) < 2020))
summary(reg13) #neg not sig
reg14<- lm(log(investee.total + 1) ~ gov.OSV.binary.l1 + factor(region) + factor(year), 
           data = merge_final_pdataframe |> filter(as.numeric(as.character(year)) < 2020))
summary(reg14) # pos not sig
vif(reg14) #fine
reg15 <- lm(log(investee.total + 1) ~ gov.OSV.binary.l1 + civ.conflict.binary.l1 + wdi_gdppc_cusd.l1 + wdi_gdp_growth_perc.l1 + wdi_population.l1 
           + wdi_trade_gdp.l1 + wdi_resource_rents.l1 + v2x_polyarchy.l1 + v2x_libdem.l1 + v2x_partipdem.l1 + v2x_delibdem.l1 + v2x_egaldem.l1, 
           data = merge_final_pdataframe |> filter(as.numeric(as.character(year)) < 2020))
summary(reg15) # neg and sig
vif(reg15) #vdem massive, everything else fine
reg16 <- lm(log(investee.total + 1) ~ gov.OSV.binary.l1 + civ.conflict.binary.l1 + wdi_gdppc_cusd.l1 + wdi_gdp_growth_perc.l1 + wdi_population.l1 
           + wdi_trade_gdp.l1 + wdi_resource_rents.l1 + v2x_polyarchy.l1 + v2x_libdem.l1 + v2x_partipdem.l1 + v2x_delibdem.l1 + v2x_egaldem.l1
           + factor(region) + factor(year), 
           data = merge_final_pdataframe |> filter(as.numeric(as.character(year)) < 2020))
summary(reg16) # neg not sig
vif(reg16) #vdem massive, factor(region) at 4, all else fine

## STARGAZER TABLE 4

stargazer(reg13,reg14,reg15,reg16,
          title="Regression Results with Total FDI Stocks as DV", 
          dep.var.labels = c("Total Logged FDI Stocks"), 
          covariate.labels = c("Lagged State OSV Indicator",
                               "Lagged Civil Conflict Indicator",
                               "Lagged GDP Per Capita (Constant 2015 Dollars)",
                               "Lagged Annual GDP Growth Percentage",
                               "Lagged Population", 
                               "Lagged Trade as Percentage of GDP", 
                               "Lagged Natural Resource Rents as Percentage of GDP",
                               "Lagged Electoral Democracy Index", 
                               "Lagged Liberal Democracy Index", 
                               "Lagged Participatory Democracy Index", 
                               "Lagged Deliberative Democracy Index", 
                               "Lagged Egalitarian Democracy Index"),
          omit = c("investee", "region" ,"year"),
          header = F,
          font.size = "small",
          omit.stat = c("f", "ser"),
          star.char = c("*"),
          star.cutoffs = c(.05),
          notes = c("* p<0.05"),
          notes.append = FALSE)


#### Robustness Tests
### Table 5: Regression Results with Confidential Reporters Ratio as DV
# run imf_confidential v2.R to reproduce table

### Appendix Tables: Regressions excluding V-Dem due to VIF scores

reg3a <- lm(relative.investee.total.imf.gdp ~ gov.OSV.binary.l1 + civ.conflict.binary.l1 + wdi_gdppc_cusd.l1 + wdi_gdp_growth_perc.l1 + wdi_population.l1 
           + wdi_trade_gdp.l1 + wdi_resource_rents.l1,
           data = merge_final_pdataframe |> filter(as.numeric(as.character(year)) < 2020))
summary(reg3a) # no change
reg4a <- lm(relative.investee.total.imf.gdp ~ gov.OSV.binary.l1 + civ.conflict.binary.l1 + wdi_gdppc_cusd.l1 + wdi_gdp_growth_perc.l1 + wdi_population.l1 
           + wdi_trade_gdp.l1 + wdi_resource_rents.l1 + factor(region) + factor(year), 
           data = merge_final_pdataframe |> filter(as.numeric(as.character(year)) < 2020 ))
summary(reg4a) # no change

## APPENDIX TABLE 1
stargazer(reg3a,reg4a,
          title="Regression Results with Relative Haven Investment Inflows as DV", 
          dep.var.labels = c("Relative Haven Investment Inflows"), 
          covariate.labels = c("Lagged State OSV Indicator",
                               "Lagged Civil Conflict Indicator",
                               "Lagged GDP Per Capita (Constant 2015 Dollars)",
                               "Lagged Annual GDP Growth Percentage",
                               "Lagged Population", 
                               "Lagged Trade as Percentage of GDP",
                               "Lagged Natural Resource Rents as Percentage of GDP"),
          omit = c("investee", "region", "year"),
          header = F,
          font.size = "small",
          omit.stat = c("f", "ser"),
          star.char = c("*"),
          star.cutoffs = c(.05),
          notes = c("* p<0.05"),
          notes.append = FALSE)



reg7a <- lm(haven.investee.total.gdp ~ gov.OSV.binary.l1 + civ.conflict.binary.l1 + wdi_gdppc_cusd.l1 + wdi_gdp_growth_perc.l1 + wdi_population.l1 
           + wdi_trade_gdp.l1 + wdi_resource_rents.l1,
           data = merge_final_pdataframe |> filter(as.numeric(as.character(year)) < 2020))
summary(reg7a) #same 
reg8a <- lm(haven.investee.total.gdp ~ gov.OSV.binary.l1 + civ.conflict.binary.l1 + wdi_gdppc_cusd.l1 + wdi_gdp_growth_perc.l1 + wdi_population.l1 
           + wdi_trade_gdp.l1 + wdi_resource_rents.l1 + factor(region) + factor(year), 
           data = merge_final_pdataframe |> filter(as.numeric(as.character(year)) < 2020))
summary(reg8a) #same

## APPENDIX TABLE 2

stargazer(reg7a,reg8a,
          title="Regression Results with Haven FDI Stocks as DV", 
          dep.var.labels = c("Logged Haven FDI Stocks"), 
          covariate.labels = c("Lagged State OSV Indicator",
                               "Lagged Civil Conflict Indicator",
                               "Lagged GDP Per Capita (Constant 2015 Dollars)",
                               "Lagged Annual GDP Growth Percentage",
                               "Lagged Population",
                               "Lagged Trade as Percentage of GDP",
                               "Lagged Natural Resource Rents as Percentage of GDP"),
          omit = c("investee","region","year"),
          header = F,
          font.size = "small",
          omit.stat = c("f", "ser"),
          star.char = c("*"),
          star.cutoffs = c(.05),
          notes = c("* p<0.05"),
          notes.append = FALSE,
          digits = 7)

reg11a <- lm(log(nonhaven.investee.total + 1) ~ gov.OSV.binary.l1 + civ.conflict.binary.l1 + wdi_gdppc_cusd.l1 + wdi_gdp_growth_perc.l1 + wdi_population.l1 
            + wdi_trade_gdp.l1 + wdi_resource_rents.l1, 
            data = merge_final_pdataframe |> filter(as.numeric(as.character(year)) < 2020))
summary(reg11a) # same
reg12a <- lm(log(nonhaven.investee.total + 1) ~ gov.OSV.binary.l1 + civ.conflict.binary.l1 + wdi_gdppc_cusd.l1 + wdi_gdp_growth_perc.l1 + wdi_population.l1 
            + wdi_trade_gdp.l1 + wdi_resource_rents.l1 + factor(region) + factor(year), 
            data = merge_final_pdataframe |> filter(as.numeric(as.character(year)) < 2020))
summary(reg12a) #same

## APPENDIX TABLE 3

stargazer(reg11a,reg12a,
          title="Regression Results with FDI Stocks from Non-Havens as DV", 
          dep.var.labels = c("Logged FDI Stocks from Non-Havens"), 
          covariate.labels = c("Lagged State OSV Indicator",
                               "Lagged Civil Conflict Indicator",
                               "Lagged GDP Per Capita (Constant 2015 Dollars)",
                               "Lagged Annual GDP Growth Percentage",
                               "Lagged Population",
                               "Lagged Trade as Percentage of GDP",
                               "Lagged Natural Resource Rents as Percentage of GDP"),
          omit = c("investee","region" ,"year"),
          header = F,
          font.size = "small",
          omit.stat = c("f", "ser"),
          star.char = c("*"),
          star.cutoffs = c(.05),
          notes = c("* p<0.05"),
          notes.append = FALSE)

reg15a <- lm(log(investee.total + 1) ~ gov.OSV.binary.l1 + civ.conflict.binary.l1 + wdi_gdppc_cusd.l1 + wdi_gdp_growth_perc.l1 + wdi_population.l1 
            + wdi_trade_gdp.l1 + wdi_resource_rents.l1, 
            data = merge_final_pdataframe |> filter(as.numeric(as.character(year)) < 2020))
summary(reg15a) # same

reg16a <- lm(log(investee.total + 1) ~ gov.OSV.binary.l1 + civ.conflict.binary.l1 + wdi_gdppc_cusd.l1 + wdi_gdp_growth_perc.l1 + wdi_population.l1 
            + wdi_trade_gdp.l1 + wdi_resource_rents.l1 + factor(region) + factor(year), 
            data = merge_final_pdataframe |> filter(as.numeric(as.character(year)) < 2020))
summary(reg16a) # still neg, now sig

## STARGAZER TABLE 4

stargazer(reg15a,reg16a,
          title="Regression Results with Total FDI Stocks as DV", 
          dep.var.labels = c("Total Logged FDI Stocks"), 
          covariate.labels = c("Lagged State OSV Indicator",
                               "Lagged Civil Conflict Indicator",
                               "Lagged GDP Per Capita (Constant 2015 Dollars)",
                               "Lagged Annual GDP Growth Percentage",
                               "Lagged Population", 
                               "Lagged Trade as Percentage of GDP", 
                               "Lagged Natural Resource Rents as Percentage of GDP"),
          omit = c("investee", "region" ,"year"),
          header = F,
          font.size = "small",
          omit.stat = c("f", "ser"),
          star.char = c("*"),
          star.cutoffs = c(.05),
          notes = c("* p<0.05"),
          notes.append = FALSE)


