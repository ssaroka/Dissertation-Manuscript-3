library(readxl)
library(countrycode)
library(dplyr)
library(tidyr)
library(tinytable)

cdis_reporters <- read_excel("Data/Raw/CDIS_reporters.xlsx", 
                             sheet = 2, 
                             n_max = 129)

reporters <- apply(cdis_reporters[,-1], 2, FUN = function(x) {
  sum(is.na(x) == F)
})

mean(reporters)
min(reporters)
max(reporters)



colnames(cdis_reporters)[1] <- "countryname"
cdis_reporters[cdis_reporters$countryname == "Aruba, Kingdom of the Netherlands", "countryname"] <- "Aruba" 
cdis_reporters[cdis_reporters$countryname == "Curaçao, Kingdom of the Netherlands", "countryname"] <- "Curaçao" 
cdis_reporters[cdis_reporters$countryname == "Sint Maarten, Kingdom of the Netherlands", "countryname"] <- "Sint Maarten" 
cdis_reporters[cdis_reporters$countryname == "Türkiye, Rep. of", "countryname"] <- "Turkey" 
cdis_reporters$iso3c <- countrycode(cdis_reporters$countryname, "country.name", "iso3c")

zorome <- read_xlsx("Data/Raw/Zorome_haven_list.xlsx")
zorome$iso3c <- countrycode(zorome$jurisdiction, "country.name", "iso3c")
zorome <- zorome |> 
  add_row(haven = 1, iso3c = "NLD")

merge <- left_join(cdis_reporters, zorome, by = "iso3c")


cdis_reporters_2 <- merge |> filter(haven == 1)

haven_reporters <- apply(cdis_reporters_2[,-1], 2, FUN = function(x) {
  sum(is.na(x) == F)
})

mean(haven_reporters)
min(haven_reporters)
max(haven_reporters)

# Creating a table of CDIS tax haven reporters: 

reporter_table <- cdis_reporters_2 |> 
  select(countryname, 2:15) |> 
  pivot_longer(cols = 2:15, names_to = "Year") |> 
  filter(is.na(value) == F) |> 
  select(-value) |> 
  group_by(countryname) |> 
  mutate("Min" = min(Year), 
         "Max" = max(Year), 
         "Range" = paste(Min, "-", Max)) |> 
  select("Tax Haven" = countryname, "Reporting Years" = Range) |> 
  distinct()

tt(reporter_table, theme = "void", 
   caption = "Tax Havens Reporting to IMF CDIS") |> 
  save_tt("Output/cdis_haven_reporters.tex", 
          overwrite = T) 
