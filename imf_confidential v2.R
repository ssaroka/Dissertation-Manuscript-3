rm(list = ls())

library(plm)
library(tidyverse)
library(readxl)
library(countrycode)
library(WDI)
library(stargazer)

# DATA LOADING ----

## IMF CDIS ----
# 
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
# # For this analysis: treat confidential as the dependent variable 
# fdi_dat_full <- fdi_dat_full |> 
#   mutate(confidential = if_else(dyad_investment == "C", 1, 
#                                 if_else(is.na(dyad_investment == T), NA, 0)))
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
# 
# saveRDS(fdi_dat_full, "Data/Cleaned/fdi_dat_confidential.R")
fdi_dat_confidential <- readRDS("Data/Cleaned/fdi_dat_confidential.R")

## Zorome Tax Havens List ----
zorome <- readRDS("Data/Cleaned/zorome.R")
zorome <- zorome |> 
  add_row(haven = 1, investor.iso3c = "NLD")

## PRIO ----
prio_full <- readRDS("Data/Cleaned/prio_full.R")

## V-Dem ----
vdem_select <- readRDS("Data/Cleaned/vdem_select.R")


## WDI ----
wdi_dat <- readRDS("Data/Cleaned/wdi_dat.R")


## Voeten ideal points ----
un_ideal_select <- readRDS("Data/Cleaned/un_ideal_select.R")

##  OECD membership ----
oecd <- read_xlsx(("Data/Raw/OECD_members.xlsx"))
oecd$iso3c <- countrycode(oecd$country.name, "country.name", "iso3c")
oecd <- oecd |> 
  mutate(oecd = 1) |> 
  select(iso3c, oecd)

## EUSANCT data ---- 
eusanct_eu <- readRDS("Data/Cleaned/eusanct_eu.R")



## Merging ----
merge_1 <- left_join(fdi_dat_confidential, zorome, by = "investor.iso3c") # adding binary indicators for investors as tax havens
merge_2 <- left_join(merge_1, prio_full, by = c("investee.iso3c", "year")) # adding conflict variables for investees
merge_3 <- left_join(merge_2, vdem_select, by = c("investee.iso3c" = "iso3c", "year")) # adding conflict variables for investees
merge_4 <- left_join(merge_3, wdi_dat, by = c("investee.iso3c" = "iso3c", "year")) # adding conflict variables for investees
merge_5 <- left_join(merge_4, un_ideal_select, by = c("investee.iso3c" = "iso3c", "year"))
merge_6 <- left_join(merge_5, oecd, by = c("investee.iso3c" = "iso3c"))
merge_7 <- left_join(merge_6, eusanct_eu, by = c("investee.iso3c" = "iso3c", "year"))

merge_final <- merge_7


# DATA CLEANING ----

## LOADING DATA ----
OSV_raw <- readRDS("Data/Cleaned/OneSided_v23_1.rds")

# Binary indicator for civil conflict
merge_final <- merge_final %>% mutate("civ.conflict.binary" = ifelse(is.na(internal) == T & is.na(internationalized) == T, 0, 1))


# Filling in data for civil conflict: 
merge_final <- merge_final |> mutate("internal" = ifelse(is.na(internal) == T, 0, 1), 
                                     "internationalized" = ifelse(is.na(internationalized) == T, 0, 1))

# Filtering OSV data for gov violence, then merging in
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


# Count of confidential observations, total observations, and ratio
merge_final <- merge_final %>% 
  group_by(year, investee) %>% 
  mutate(confidential.total = sum(confidential, na.rm = T), 
         report.total = sum(is.na(dyad_investment) == F), 
         report.nonzero = if_else(dyad_investment < 1, 0, 1),
         nonzero.total = sum(report.nonzero, na.rm = T),
         confidential.report.ratio = confidential.total/report.total, 
         confidential.nonzero.ratio = confidential.total/nonzero.total) |> 
  ungroup()

# Confidential only from tax havens 
merge_final_haven <- merge_final |> 
  group_by(year, investee) |> 
  filter(haven == 1) |> 
  mutate("haven.confidential.total" = sum(confidential, na.rm = T)) |> 
  select(year, investee, haven.confidential.total) |> 
  ungroup() |> 
  distinct()

# Remerging and dropping dyadic setup
merge_final_flat <- merge_final |> 
  select(-c(dyad_investment, investor, investor.iso3c, haven, report.nonzero)) |> 
  distinct()
merge_final_flat <- left_join(merge_final_flat, merge_final_haven, by = c("investee", "year"))


# Non-haven confidential total
merge_final_flat$nonhaven.confidential.total <- with(merge_final_flat, confidential.total- haven.confidential.total)

merge_final_select <- merge_final_flat |> 
  select(investee, investee.iso3c, year, region, 
         confidential.total, report.total, confidential.report.ratio,
         nonzero.total, confidential.nonzero.ratio,
         haven.confidential.total, nonhaven.confidential.total,  # totals
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


# EXPLORATORY DATA ANALYSIS ----
merge_final_pdataframe |> 
  as_tibble() |> 
  ggplot() + 
  aes(x = confidential.total) + 
  geom_histogram() + 
  theme_minimal()
# Incredibly normally distributed, despite being a count. 

merge_final_pdataframe |> 
  as_tibble() |> 
  ggplot() + 
  aes(x = confidential.total, fill = factor(civ.conflict.binary)) + 
  geom_histogram() + 
  theme_minimal()
# Honestly, these look really, really similar as well. 

merge_final_pdataframe |> 
  as_tibble() |> 
  ggplot() + 
  aes(x = confidential.total, fill = factor(civ.conflict.binary.hi)) + 
  geom_histogram() + 
  theme_minimal()
# As do the de-aggregated versions. 

# Do they look different by tax havens? 
merge_final_pdataframe |> 
  as_tibble() |> 
  ggplot() + 
  aes(x = haven.confidential.total) + 
  geom_histogram() + 
  theme_minimal()
# Again, normally distributed. 

merge_final_pdataframe |> 
  as_tibble() |> 
  ggplot() + 
  aes(x = haven.confidential.total, fill = factor(civ.conflict.binary)) + 
  geom_histogram() + 
  theme_minimal()
# And again, pretty normally distributed. 

merge_final_pdataframe |> 
  as_tibble() |> 
  ggplot() + 
  aes(x = haven.confidential.total, fill = factor(civ.conflict.binary.lo)) + 
  geom_histogram() + 
  theme_minimal()

# Looking purely at total reports
merge_final_pdataframe |> 
  as_tibble() |> 
  ggplot() + 
  aes(x = report.total) + 
  geom_histogram() + 
  theme_minimal()
# Skewed farther to the right

merge_final_pdataframe |> 
  as_tibble() |> 
  ggplot() + 
  aes(x = confidential.report.ratio) + 
  geom_histogram() + 
  theme_minimal()
# And confidential makes up a fairly small amount. 

merge_final_pdataframe |> 
  as_tibble() |> 
  ggplot() + 
  aes(x = confidential.report.ratio, fill = factor(civ.conflict.binary.l1)) + 
  geom_histogram() + 
  theme_minimal()
# And this sure doesn't look any different, yet again. 


# Looking only at non-zero reports
merge_final_pdataframe |> 
  as_tibble() |> 
  ggplot() + 
  aes(x = nonzero.total) + 
  geom_histogram() + 
  theme_minimal()

merge_final_pdataframe |> 
  as_tibble() |> 
  ggplot() + 
  aes(x = nonzero.total, fill = factor(civ.conflict.binary)) + 
  geom_histogram() + 
  theme_minimal()


#### This code produces plot of confidential reporter ratios
merge_final_pdataframe |> 
  as_tibble() |> 
  ggplot() + 
  aes(x = haven.confidential.total/report.total, fill = factor(civ.conflict.binary)) + 
  #facet_wrap(~sanction) + 
  geom_histogram() + 
  xlab("Count of Observations") + 
  ylab("Ratio of Confidential Reporters") + 
  labs(fill = "Civil Conflict") + 
  theme_minimal() 
# And again -- this broadly looks the exact same. 



# REGRESSION ANALYSIS ---- 
reg1 <- lm(haven.confidential.total/nonzero.total ~ gov.OSV.binary.l1 , 
           data = merge_final_pdataframe |> filter(as.numeric(as.character(year)) < 2020))
summary(reg1) # No relationship. 
reg2<- lm(haven.confidential.total/nonzero.total ~ gov.OSV.binary.l1  + factor(region) + factor(year), 
          data = merge_final_pdataframe |> filter(as.numeric(as.character(year)) < 2020))
summary(reg2) # No relationship. 
reg3 <- lm(haven.confidential.total/nonzero.total ~ gov.OSV.binary.l1 + civ.conflict.binary.l1 + wdi_gdppc_cusd.l1 + wdi_gdp_growth_perc.l1 + wdi_population.l1 
           + wdi_trade_gdp.l1 + wdi_resource_rents.l1 + v2x_polyarchy.l1 + v2x_libdem.l1 + v2x_partipdem.l1 + v2x_delibdem.l1 + v2x_egaldem.l1,
           data = merge_final_pdataframe |> filter(as.numeric(as.character(year)) < 2020))
summary(reg3) # no relationship 
reg4 <- lm(haven.confidential.total/nonzero.total ~ gov.OSV.binary.l1 + civ.conflict.binary.l1 + wdi_gdppc_cusd.l1 + wdi_gdp_growth_perc.l1 + wdi_population.l1 
           + wdi_trade_gdp.l1 + wdi_resource_rents.l1 + v2x_polyarchy.l1 + v2x_libdem.l1 + v2x_partipdem.l1 + v2x_delibdem.l1 + v2x_egaldem.l1
           + factor(region) + factor(year), 
           data = merge_final_pdataframe |> filter(as.numeric(as.character(year)) < 2020))
summary(reg4) # no relationship again. 




library(stargazer)

texreg(list(reg1,reg2,reg3,reg4), 
       # custom.header = list("Regression Results with Relative Confidential Reporters as DV"), 
       # custom.model.names = "Relative Confidential Reporters",
       custom.coef.map = list("civ.conflict.binary.l1" = "Lagged Civil Conflict Indicator",
                            "wdi_gdppc_cusd.l1" = "Lagged GDP Per Capita (Constant 2015 Dollars)",
                            "wdi_gdp_growth_perc.l1" = "Lagged Annual GDP Growth Percentage",
                            "wdi_population.l1" = "Lagged Population", 
                            "wdi_trade_gdp.l1" = "Lagged Trade as Percentage of GDP",
                            "wdi_resource_rents.l1" = "Lagged Natural Resource Rents as Percentage of GDP",
                            "v2x_polyarchy.l1" = "Lagged Electoral Democracy Index", 
                            "v2x_libdem.l1" = "Lagged Liberal Democracy Index", 
                            "v2x_partipdem.l1" = "Lagged Participatory Democracy Index",
                            "v2x_delibdem.l1" = "Lagged Deliberative Democracy Index",
                            "v2x_egaldem.l1" = "Lagged Egalitarian Democracy Idex"), 
       include.rsquared = F,  include.adjrs = F, include.missings = F,
       stars = c(.05),
       sideways = F, center = T, no.margin = T,  booktabs = T, 
       label = "table:confidential",
       caption = "Alternative DV: Confidential Reporters"
       
       
       
       )

## STARGAZER TABLE 1
stargazer(reg1,reg2,reg3,reg4,
          title="Regression Results with Confidential Reporters Ratio as DV", 
          dep.var.labels = c("Confidential Reporters Ratio"), 
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
       
