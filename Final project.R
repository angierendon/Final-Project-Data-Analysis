library(tidyverse)
library(sf)
library(janitor)
install.packages("ggplot2")
library(ggplot2)
library(dplyr)

bins_and_income <- read.csv("~/Desktop/QGIS/Assignments /Final Project/income and bins normalized.csv")


##bin_and_income data was prepared in QGIS and excel 
##scatter plot of litter basket density and median income 
ggplot(bins_and_income, aes(x = mhhi, y = binsper100k)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Litter Basket Density vs. Median Household Income",
    x = "Median Household Income ($)",
    y = "Litter Basket Density (bins per 100k people)"
  ) +
  theme_minimal()
##I ended up making this scatter plot on Tableau for the official publication 

##finding the correlation 
correlation <- cor(bins_and_income$mhhi, bins_and_income$binsper100k, method = "pearson")
print(correlation)



##preparing data set for high-end trash baskets analysis in QGIS

binsdata <- read.csv("~/Desktop/QGIS/Assignments /Final Project/DSNY_Litter_Basket_Inventory_20241204.csv")

##filter for only baskets that are high density 
high_end_bins <- binsdata[binsdata$BASKETTYPE == "H", ]

##export as a csv for use in QGIS 
write.csv(high_end_bins,"~/Desktop/QGIS/Assignments /Final Project/high end bins.csv")



##rat inspection data was prepared in QGIS and excel 
rat_inspections <- read.csv("~/Desktop/QGIS/Assignments /Final Project/Rat Inspection Data Upper Manhattan.csv")

##calculate the percent of inspections that recieved "Rat Activity" and "Failed Other Reasons" 
percent_failed <- rat_inspections %>%
  mutate(percent_failed = ((rat_activity + failed) / all_inspections) *100)%>%
  select(boro_cd, percent_failed)%>%
  arrange(factor(boro_cd, levels = c('7','9','8','11'))) 
##arrange in order to group UWS with West Harlem and UES with East Harlem 


##create a bar graph
ggplot(percent_failed, aes(x = boro_cd, y = percent_failed)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.6) +
  labs(
    title = "Failed Rat Inspections per Community District",
  ) +
  theme_linedraw() +
  scale_x_discrete(breaks = c("10")) +  # Specify breaks correctly here
  theme(
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor = element_blank()
  )
##export as an image in the plot window to edit in Canva
