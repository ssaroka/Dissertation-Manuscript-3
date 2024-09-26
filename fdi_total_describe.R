library(WDI)
library(dplyr)
library(tinytable)


wdi_fdi <- WDI(indicator = c("BM.KLT.DINV.CD.WD", 
                             "NY.GDP.MKTP.CD"), extra = T)

wdi_fdi <- filter(wdi_fdi, region != "Aggregates")
names(wdi_fdi)[which(names(wdi_fdi) == "BM.KLT.DINV.CD.WD")] <- "wdi_fdi_cusd"
names(wdi_fdi)[which(names(wdi_fdi) == "NY.GDP.MKTP.CD")] <- "wdi_gdp_cusd"
wdi_fdi$year <- as.numeric(wdi_fdi$year) # reclassifying for merge


wdi_fdi_2022_top <- wdi_fdi |>
  filter(year == 2022) |>
  slice_max(wdi_fdi_cusd, n = 20) |> 
  arrange(desc(wdi_fdi_cusd)) |> 
  rownames_to_column() |> 
  mutate("fdi_rank" = rowname) |> 
  select("country", "fdi_rank")

wdi_gdp_2022_top <- wdi_fdi |> 
  filter(year == 2022) |> 
  slice_max(wdi_gdp_cusd, n = 20) |> 
  arrange(desc(wdi_gdp_cusd)) |> 
  rownames_to_column() |> 
  mutate("gdp_rank" = rowname) |> 
  select("country", "gdp_rank")

wdi_2022_top <- full_join(wdi_fdi_2022_top, wdi_gdp_2022_top, by = "country")
wdi_2022_top <-  wdi_2022_top |> 
  mutate(fdi_new = ifelse(is.na(gdp_rank) == T, 1, 0))
wdi_2022_top <- wdi_2022_top |> 
  pivot_longer(2:3, names_to = "Indicator", values_to = "Rank")

wdi_2022_top <- wdi_2022_top |> 
  mutate(xaxis = if_else(Indicator == "fdi_rank", 2, 1), 
         xpath = if_else(Indicator == "fdi_rank", 1.85, 1.15))

fdi_plot_2022 <- ggplot(wdi_2022_top, aes(x = xaxis, y = (21 - as.numeric(as.character(Rank))), 
                         group = country, label = country, fill = factor(fdi_new))) + 
  geom_path(aes(x = xpath), size=.3, color="grey50") +
  geom_label(aes(fill = factor(fdi_new)), color = "black", size = 8) +
  scale_fill_manual(values = c("white", "grey50")) + 
  theme_minimal() + 
  # ggtitle("Global Rank in 2022 (Current USD)") +  
  scale_x_continuous(limits = c(.5, 2.5), breaks = c(1, 2), labels = c("GDP", "FDI")) +
  scale_y_continuous(breaks = c(1:20), labels = c(20:1)) +
  theme(axis.title = element_blank(),
        # axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(), 
        legend.position = "none")

ggsave("Output/fdi_plot_2022.png", fdi_plot_2022, 
       height = 4, width = 4, units = "in", scale = 3)
