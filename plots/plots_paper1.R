library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(bbplot)
library(reshape2)
library(tibble)
library(stringr)
library(forcats)
library(rgdal)
library(leaflet)
library(sf)
library(terra)
library(spData)
library(tmap)
library(tigris)
library(tidyverse)
library(readxl)
library(tmaptools) 
library(shinyjs)

# This script produces the graphs to compare different scenarios. The datasets to be compared can be modified in this section.
available_results_files <- list.dirs(paste0(getwd(), "/outputs/air_quality/"), full.names = FALSE, recursive = FALSE)
results_files <- available_results_files
#results_files <- available_results_files[1:3]


# Creation of the graphs
  # 1 - Fleet composition
path <- "D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/08_Data_for_graphs/"
fleet_composition_default <- read.csv(paste0(path, "fleet_composition_default.csv"))
fleet_composition_state_default <- read.csv(paste0(path, "fleet_composition_by_state_default.csv"))
fleet_compositionzev_all <- read.csv(paste0(path, "fleet_composition_ZEV-CA_All.csv"))
fleet_composition_state_zev_all <- read.csv(paste0(path, "fleet_composition_by_state_ZEV-CA_All.csv"))
fleet_composition_zev_zev <- read.csv(paste0(path, "fleet_composition_ZEV-CA_zev.csv"))
fleet_composition_zev_zev <- read.csv(paste0(path, "fleet_composition_by_state_ZEV-CA_zev.csv"))
scenarios <- c("Standard", "ZEV-CA_All", "ZEV-CA_zev")
  # 1.1 - Plot the fraction of electric vehicles in the total fleet for each scenario
years <- unique(fleet_composition_default$Year)
fraction_electric_vehicles <- data.frame(years)
fraction_electric_vehicles_sales <- data.frame(years)
for (i in 1:length(years)) {
    fraction_electric_vehicles[i,scenarios[1]] <- sum(fleet_composition_default$Value[which(fleet_composition_default$Year == fraction_electric_vehicles$years[i] & (fleet_composition_default$Technology == "BEV100" | fleet_composition_default$Technology == "BEV300"))])/sum(fleet_composition_default$Value[which(fleet_composition_default$Year == fraction_electric_vehicles$years[i])])
    fraction_electric_vehicles[i,scenarios[2]] <- sum(fleet_compositionzev_all$Value[which(fleet_compositionzev_all$Year == fraction_electric_vehicles$years[i] & (fleet_compositionzev_all$Technology == "BEV100" | fleet_compositionzev_all$Technology == "BEV300"))])/sum(fleet_compositionzev_all$Value[which(fleet_compositionzev_all$Year == fraction_electric_vehicles$years[i])])
    fraction_electric_vehicles[i,scenarios[3]] <- sum(fleet_composition_zev_zev$Value[which(fleet_composition_zev_zev$Year == fraction_electric_vehicles$years[i] & (fleet_composition_zev_zev$Technology == "BEV100" | fleet_composition_zev_zev$Technology == "BEV300"))])/sum(fleet_composition_zev_zev$Value[which(fleet_composition_zev_zev$Year == fraction_electric_vehicles$years[i])])
    fraction_electric_vehicles_sales[i,scenarios[1]] <- sum(fleet_composition_default$Value[which(fleet_composition_default$Year == fraction_electric_vehicles$years[i] & (fleet_composition_default$Technology == "BEV100" | fleet_composition_default$Technology == "BEV300") & fleet_composition_default$Age == 0)])/sum(fleet_composition_default$Value[which(fleet_composition_default$Year == fraction_electric_vehicles$years[i] & fleet_composition_default$Age == 0)])
    fraction_electric_vehicles_sales[i,scenarios[2]] <- sum(fleet_compositionzev_all$Value[which(fleet_compositionzev_all$Year == fraction_electric_vehicles$years[i] & (fleet_compositionzev_all$Technology == "BEV100" | fleet_compositionzev_all$Technology == "BEV300") & fleet_compositionzev_all$Age == 0)])/sum(fleet_compositionzev_all$Value[which(fleet_compositionzev_all$Year == fraction_electric_vehicles$years[i] & fleet_compositionzev_all$Age == 0)])
    fraction_electric_vehicles_sales[i,scenarios[3]] <- sum(fleet_composition_zev_zev$Value[which(fleet_composition_zev_zev$Year == fraction_electric_vehicles$years[i] & (fleet_composition_zev_zev$Technology == "BEV100" | fleet_composition_zev_zev$Technology == "BEV300") & fleet_composition_zev_zev$Age == 0)])/sum(fleet_composition_zev_zev$Value[which(fleet_composition_zev_zev$Year == fraction_electric_vehicles$years[i] & fleet_composition_zev_zev$Age == 0)])
}
fraction_electric_vehicles_graph <- melt(filter(fraction_electric_vehicles, fraction_electric_vehicles$years >= 2020), id = c("years"))
fraction_electric_vehicles_graph <- mutate(fraction_electric_vehicles_graph, variable = fct_relevel(variable, 
                          "Standard", "ZEV-CA_zev", "ZEV-CA_All"))
  # 1.1.1 - Fraction of electric vehicles in the total fleet
colors <- c("#004c6d", "#6d95b3", "#c7e6ff")
p <- ggplot(fraction_electric_vehicles_graph, aes(x=years, y=value, group=variable, color=variable)) +
  geom_line(size= 2) +
  labs(title = "Share of electric vehicles in the U.S. fleet") +
  scale_color_manual(values = colors, labels = c("AEO forecast", "ZEV", "ZEV Nationwide")) +
  bbc_style() +
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1)) +
  ylim(0, 1) +
  theme(plot.title = element_text(color = "#002554"),
        #legend.position = c(0.38, 0.95),
        legend.direction = "horizontal")
finalise_plot(p, source_name = "ZEV = Zero-Emissions Vehicles policy implemented by California ", 
              save_filepath = "D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/08_Data_for_graphs/BEVs_in_fleet.png",
              width_pixels = 640, height_pixels = 450)

  # 1.1.2 Fraction of electric vehicles in selected U.S. states
states <- c("California", "Texas", "Florida", "New York", "Pennsylvania", "Illinois", "Ohio", "Georgia", "North Carolina", "Michigan")
states_id <- c("6", "48", "12", "36", "42", "17", "39", "13", "37", "26")

  
  # 1.1.2 - Fraction of electric vehicles in new sales
ggplot(fraction_electric_vehicles_sales, aes(x=years)) + 
  geom_line(aes(y=Standard, color="Standard")) + 
  geom_line(aes(y=`ZEV-CA_All`, color="ZEV-CA_All")) + 
  geom_line(aes(y=`ZEV-CA_zev`, color="ZEV-CA_zev"))

# Evolution of the number of electric vehicles in the fleet
path <- "D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_FLAME-AQ/outputs/air_quality/"
dataset_template <- data.frame(years)
dataset_template <- dataset_template %>%
  add_column("Scenario" = NA) %>%
  add_column("Number_of_EVs" = NA)
scenarios <- c("baseline", "ZEV_CA_CG", "ZEV_All_CG")
paths <- c("2023-02-20_baseline_zev_current_grid_results/", "2023-02-20_ZEV_CA_zev_current_grid_results/", "2023-02-17_ZEV_All_zev_current_grid_results/")
years <- 2023:2050
m <- 0
for (i in 1:length(scenarios)) {
  data <- read.csv(paste0(path, paths[i], "fleet_composition.csv"))
  temp_dataset <- dataset_template
  for (j in years) {
    row <- which(temp_dataset$years == j)
    temp_dataset$Scenario[row] <- scenarios[i]
    temp_dataset$Number_of_EVs[row] <- sum(data$Value[which(data$Year == j & (data$Technology == "BEV100" | data$Technology == "BEV300"))])
  }
  if (m == 0) {
    dataset <- temp_dataset
  } else {
    dataset <- rbind(dataset, temp_dataset)
  }
  m <- m+1
}


  # 2 - Pollutants emissions
path <- "D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_FLAME-AQ/outputs/air_quality/"
years <- 2023:2050
dataset_template <- data.frame(years)
dataset_template <- dataset_template %>%
  add_column("Scenario" = NA) %>%
  add_column("Emission_process" = NA) %>%
  add_column("Pollutant" = NA) %>%
  add_column("Value" = NA)
scenarios <- c("baseline", "ZEV_CA_CG", "ZEV_All_CG", "ZEV_CA_Ren", "ZEV_All_Ren")
paths <- c("2023-02-20_baseline_zev_current_grid_results/", "2023-02-20_ZEV_CA_zev_current_grid_results/", "2023-02-17_ZEV_All_zev_current_grid_results/", "2023-02-21_ZEV_CA_zev_renewables_results/", "2023-02-20_ZEV_All_zev_renewables_results/")
pollutants <- c("NOx", "SO2", "PM25")
emission_process <- c("Fleet", "Electricity")
m <- 0
for (i in 1:length(scenarios)) {
  for (j in 1:length(emission_process)) {
    if (emission_process[j] == "Fleet") {
      data <- read.csv(paste0(path, paths[i], "total_yearly_emissions.csv"))
      data <- data %>%
        add_column("PM25" = 0)
      data$PM25 <- data$Total_PM25+data$Brake_PM25+data$Tire_PM25
    } else if (emission_process[j] == "Electricity") {
      data <- read.csv(paste0(path, paths[i], "fleet_emissions_elec_state_breakdown.csv"))
      data <- select(data, c("State", "Year", "SO2", "NOx", "PM25"))
      data$SO2 <- data$SO2*1E6
      data$NOx <- data$NOx*1E6
      data$PM25 <- data$PM25*1E6
    }
    for (k in pollutants) {
      temp_dataset <- dataset_template
      for (l in years) {
        row <- which(temp_dataset$years == l)
        temp_dataset$Scenario[row] <- scenarios[i]
        temp_dataset$Emission_process[row] <- emission_process[j]
        temp_dataset$Pollutant[row] <- k
        if (j == 1) {
          temp_dataset$Value[row] <- data[which(data$Year == l), which(colnames(data) == k)]
        } else if (j == 2) {
          temp_dataset$Value[row] <- sum(data[which(data$Year == l), which(colnames(data) == k)])
        }
      }
      if (m == 0) {
        dataset <- temp_dataset
      } else {
        dataset <- rbind(dataset, temp_dataset)
      }
      m <- m+1
    }
  }
}
dataset <- filter(dataset, !grepl("Ren", dataset$Scenario))
#dataset <- add_column(dataset, "Scen" = NA)
#dataset$Scen <- paste0(dataset$Scenario, "_", dataset$Emission_process)
#dataset <- select(dataset, -c("Scenario", "Emission_process"))
#dataset_electricity <- filter(dataset, dataset$Emission_process == "Electricity")
#dataset_fleet <- filter(dataset, dataset$Emission_process == "Fleet")
#dataset_total <- dataset_electricity
#for (i in 1:dim(dataset_total)[1]) {
#  dataset_total$Value[i] <- dataset_electricity$Value[i]+dataset_fleet$Value[i]
#}
#dataset_total <- select(dataset_total, -c(Emission_process))
dataset$Value <- dataset$Value/1E6


# 2.1 - Evolution of NOx in the different scenarios
#colors <- c("#a80000", "#0086a8", "#76c2d1", "#d68e5e", "#fff2e3", "#cbffff")
Nox_plot <- ggplot(filter(dataset, dataset$Pollutant == "PM25"), aes(x=years, y=Value, color=Scenario, linetype = Emission_process)) +
  geom_line(size= 2) +
  labs(title = "PM2.5 emissions") +
  labs(subtitle = "in thousand tons per year") +
  #scale_color_manual(values = colors, labels = c("baseline_fleet", "baseline_elec", "ZEV_fleet", "ZEV_elec", "ZEV Nationwide_fleet", "ZEV Nationawide_elec")) +
  bbc_style() +
  #scale_linetype_manual(values=c("solid", "dotted"))+
  #scale_y_continuous(breaks = c(0, 200, 400, 600, 800)) +
  #scale_x_continuous(breaks = c(2022, 2030, 2040, 2050)) +
  #ylim(0, 800) +
  theme(plot.title = element_text(color = "#002554"),
        #legend.position = c(0.38, 0.95),
        legend.direction = "horizontal")
finalise_plot(Nox_plot, source_name = "ZEV = Zero-Emissions Vehicles policy implemented by California ", 
              save_filepath = "D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/08_Data_for_graphs/PM25_emissions_fleet_vs_electricity.png",
              width_pixels = 640, height_pixels = 450)




# Fleet emissions
fleet_baseline <- read.csv(paste0(baseline, "total_yearly_emissions.csv"))
fleet_baseline <- add_column(fleet_baseline, "PM25")


fleet_ZEV_CA_CG <- read.csv(paste0(ZEV_CA_CG, "total_yearly_emissions.csv"))
fleet_ZEV_All_CG <- read.csv(paste0(ZEV_All_CG, "total_yearly_emissions.csv"))
fleet_ZEV_CA_Ren <- read.csv(paste0(ZEV_CA_Ren, "total_yearly_emissions.csv"))
fleet_ZEV_All_Ren <- read.csv(paste0(ZEV_All_Ren, "total_yearly_emissions.csv"))

# Electricity emissions
elec_baseline <- read.csv(paste0(baseline, "fleet_emissions_elec_state_breakdown.csv")) 
elec_ZEV_CA_CG <- read.csv(paste0(ZEV_CA_CG, "fleet_emissions_elec_state_breakdown.csv"))
elec_ZEV_All_CG <- read.csv(paste0(ZEV_All_CG, "fleet_emissions_elec_state_breakdown.csv"))
elec_ZEV_CA_Ren <- read.csv(paste0(ZEV_CA_Ren, "fleet_emissions_elec_state_breakdown.csv"))
elec_ZEV_All_Ren <- read.csv(paste0(ZEV_All_Ren, "fleet_emissions_elec_state_breakdown.csv"))




pollutants_emissions_default <- read.csv(paste0(path, "total_yearly_emissions_Default.csv")) %>%
  select(-X)
pollutants_emissions_zev_ca <- read.csv(paste0(path, "total_yearly_emissions_ZEV-CA_zev.csv")) %>%
  select(-X)
pollutants_emissions_zev_all <- read.csv(paste0(path, "total_yearly_emissions_ZEV-CA_All.csv")) %>%
  select(-X)
years <- unique(pollutants_emissions_default$Year)
NOx_emissions <- data.frame(years)
NOx_emissions <- cbind(NOx_emissions, pollutants_emissions_default$NOx)
NOx_emissions <- cbind(NOx_emissions, pollutants_emissions_zev_ca$NOx)
NOx_emissions <- cbind(NOx_emissions, pollutants_emissions_zev_all$NOx)
colnames(NOx_emissions) <- c("Years", "Standard", "ZEV-CA", "ZEV-CA_all")
NOx_emissions_graph <- melt(NOx_emissions, id = c("Years"))
NOx_emissions_graph <- mutate(NOx_emissions_graph, variable = fct_relevel(variable, 
                                                                "Standard", "ZEV-CA", "ZEV-CA_all"))
PM25_emissions <- data.frame(years)
PM25_emissions <- cbind(PM25_emissions, pollutants_emissions_default$Total_PM25)
PM25_emissions <- cbind(PM25_emissions, pollutants_emissions_zev_ca$Total_PM25)
PM25_emissions <- cbind(PM25_emissions, pollutants_emissions_zev_all$Total_PM25)
PM25_emissions <- cbind(PM25_emissions, pollutants_emissions_default$Brake_PM25)
PM25_emissions <- cbind(PM25_emissions, pollutants_emissions_zev_ca$Brake_PM25)
PM25_emissions <- cbind(PM25_emissions, pollutants_emissions_zev_all$Brake_PM25)
PM25_emissions <- cbind(PM25_emissions, pollutants_emissions_default$Tire_PM25)
PM25_emissions <- cbind(PM25_emissions, pollutants_emissions_zev_ca$Tire_PM25)
PM25_emissions <- cbind(PM25_emissions, pollutants_emissions_zev_all$Tire_PM25)
colnames(PM25_emissions) <- c("Years", "Standard_Total", "ZEV-CA_Total", "ZEV-CA_all_Total", "Standard_Brake", "ZEV-CA_Brake", "ZEV-CA_all_Brake", "Standard_Tire", "ZEV-CA_Tire", "ZEV-CA_all_Tire")
PM25_emissions <- PM25_emissions %>%
  add_column("Standard_Sum" = PM25_emissions$Standard_Total + PM25_emissions$Standard_Brake + PM25_emissions$Standard_Tire) %>%
  add_column("ZEV-CA_Sum" = PM25_emissions$`ZEV-CA_Total` + PM25_emissions$`ZEV-CA_Brake` + PM25_emissions$`ZEV-CA_Tire`) %>%
  add_column("ZEV-CA_all_Sum" = PM25_emissions$`ZEV-CA_all_Total` + PM25_emissions$`ZEV-CA_all_Brake` + PM25_emissions$`ZEV-CA_all_Tire`)
PM25_emissions_graph <- melt(PM25_emissions, id = c("Years"))
PM25_emissions_graph <- mutate(PM25_emissions_graph, variable = fct_relevel(variable, 
                                                                          "Standard_Total", "Standard_Brake", "Standard_Tire", "Standard_Sum", "ZEV-CA_Total", "ZEV-CA_Brake", "ZEV-CA_Tire", "ZEV-CA_Sum", "ZEV-CA_all_Total", "ZEV-CA_all_Brake", "ZEV-CA_all_Tire", "ZEV-CA_all_Sum"))

SO2_emissions <- data.frame(years)
SO2_emissions <- cbind(SO2_emissions, pollutants_emissions_default$SO2)
SO2_emissions <- cbind(SO2_emissions, pollutants_emissions_zev_ca$SO2)
SO2_emissions <- cbind(SO2_emissions, pollutants_emissions_zev_all$SO2)
colnames(SO2_emissions) <- c("Years", "Standard", "ZEV-CA", "ZEV-CA_all")
SO2_emissions_graph <- melt(SO2_emissions, id = c("Years"))
SO2_emissions_graph <- mutate(SO2_emissions_graph, variable = fct_relevel(variable, 
                                                                          "Standard", "ZEV-CA", "ZEV-CA_all"))
  # 2.1 - Evolution of NOx in the different scenarios
colors <- c("#d10000", "#e4765c", "#e3beb5")
Nox_plot <- ggplot(NOx_emissions_graph, aes(x=Years, y=value/1e9, group=variable, color=variable)) +
  geom_line(size= 2) +
  labs(title = "NOx emissions") +
  labs(subtitle = "in thousand tons per year") +
  scale_color_manual(values = colors, labels = c("AEO forecast", "ZEV", "ZEV Nationwide")) +
  bbc_style() +
  scale_y_continuous(breaks = c(0, 200, 400, 600, 800)) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050)) +
  ylim(0, 800) +
  theme(plot.title = element_text(color = "#002554"),
        #legend.position = c(0.38, 0.95),
        legend.direction = "horizontal")
finalise_plot(Nox_plot, source_name = "ZEV = Zero-Emissions Vehicles policy implemented by California ", 
              save_filepath = "D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/08_Data_for_graphs/NOx_emissions.png",
              width_pixels = 640, height_pixels = 450)

  # 2.2 - Evolution of SO2 in the different scenarios
colors <- c("#098200", "#72a762", "#baccb3")
So2_plot <- ggplot(SO2_emissions_graph, aes(x=Years, y=value/1e9, group=variable, color=variable)) +
  geom_line(size= 2) +
  labs(title = "SO2 emissions") +
  labs(subtitle = "in thousand tons per year") +
  scale_color_manual(values = colors, labels = c("AEO forecast", "ZEV", "ZEV Nationwide")) +
  bbc_style() +
  scale_y_continuous(breaks = c(0, 2, 4, 6, 8)) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050)) +
  ylim(0, 8) +
  theme(plot.title = element_text(color = "#002554"),
        #legend.position = c(0.38, 0.95),
        legend.direction = "horizontal")
finalise_plot(So2_plot, source_name = "ZEV = Zero-Emissions Vehicles policy implemented by California ", 
              save_filepath = "D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/08_Data_for_graphs/So2_emissions.png",
              width_pixels = 640, height_pixels = 450)

# 2.2 - Evolution of PM2.5 in the different scenarios
temp <- c("Standard_Sum", "ZEV-CA_all_Sum", "ZEV-CA_Sum")
PM25_emissions_graph_sum <- PM25_emissions_graph[which(PM25_emissions_graph$variable%in%temp),]

colors <- c("#ffbb00", "#efbf6b", "#d6c4ab")
PM25_plot_sum <- ggplot(PM25_emissions_graph_sum, aes(x=Years, y=value/1e9, group=variable, color=variable)) +
  geom_line(size= 2) +
  labs(title = "Total PM2.5 emissions") +
  labs(subtitle = "in thousand tons per year") +
  scale_color_manual(values = colors, labels = c("AEO forecast", "ZEV", "ZEV Nationwide")) +
  bbc_style() +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050)) +
  theme(plot.title = element_text(color = "#002554"),
        #legend.position = c(0.38, 0.95),
        legend.direction = "horizontal")
finalise_plot(PM25_plot_sum, source_name = "ZEV = Zero-Emissions Vehicles policy implemented by California ", 
              save_filepath = "D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/08_Data_for_graphs/Total_PM25_emissions.png",
              width_pixels = 640, height_pixels = 450)

# 2.3 - Evolution of the ratio of combustion PM25 over non-combustion sources
PM25_combustion_ratio <- data.frame(years) %>%
  add_column("Standard_Ratio" = (PM25_emissions$Standard_Brake + PM25_emissions$Standard_Tire)/PM25_emissions$Standard_Total) %>%
  add_column("ZEV-CA_Ratio" = (PM25_emissions$`ZEV-CA_Brake` + PM25_emissions$`ZEV-CA_Tire`)/PM25_emissions$`ZEV-CA_Total`) %>%
  add_column("ZEV-CA_all_Ratio" = (PM25_emissions$`ZEV-CA_all_Brake` + PM25_emissions$`ZEV-CA_all_Tire`)/PM25_emissions$`ZEV-CA_all_Total`)
PM25_combustion_ratio <- melt(PM25_combustion_ratio, id = c("years"))
PM25_combustion_ratio <- mutate(PM25_combustion_ratio, variable = fct_relevel(variable, 
                                                                            "Standard_Ratio", "ZEV-CA_Ratio", "ZEV-CA_all_Ratio"))
colors <- c("#ab335f", "#cf8096", "#eec8d1")
PM25_ratio <- ggplot(PM25_combustion_ratio, aes(x=years, y=value, group=variable, color=variable)) +
  geom_line(size= 2) +
  labs(title = "PM2.5 from non-combustion sources") +
  labs(subtitle = "ratio between non-combustion and combustion sources") +
  scale_color_manual(values = colors, labels = c("AEO forecast", "ZEV", "ZEV Nationwide")) +
  bbc_style() +
  scale_y_continuous(breaks = c(0, 4, 8, 12)) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050)) +
  ylim(0, 12) +
  theme(plot.title = element_text(color = "#002554"),
        #legend.position = c(0.38, 0.95),
        legend.direction = "horizontal")
finalise_plot(PM25_ratio, source_name = "ZEV = Zero-Emissions Vehicles policy implemented by California ", 
              save_filepath = "D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/08_Data_for_graphs/Total_PM25_emissions_ratio.png",
              width_pixels = 640, height_pixels = 450)

# 2.3 - Evolution of non-combustion PM2.5
PM25_NC <- data.frame(years) %>%
  add_column("Standard_NC_Tire" = PM25_emissions$Standard_Tire) %>%
  add_column("Standard_NC_Brake" = PM25_emissions$Standard_Brake) %>%
  add_column("ZEV-CA_NC_Tire" = PM25_emissions$`ZEV-CA_Tire`) %>%
  add_column("ZEV-CA_NC_Brake" = PM25_emissions$`ZEV-CA_Brake`) %>%
  add_column("ZEV-CA_all_NC_Tire" = PM25_emissions$`ZEV-CA_all_Tire`) %>%
  add_column("ZEV-CA_all_NC_Brake" = PM25_emissions$`ZEV-CA_all_Brake`)
PM25_NC <- melt(PM25_NC, id = c("years"))
PM25_NC <- mutate(PM25_NC, variable = fct_relevel(variable,
                                                  "Standard_NC_Tire", "ZEV-CA_NC_Tire", "ZEV-CA_all_NC_Tire", "Standard_NC_Brake", "ZEV-CA_NC_Brake", "ZEV-CA_all_NC_Brake"))

colors <- c("#6b0000", "#b1695c", "#efc9c1", "#00196b", "#766da9", "#d4ceea")
PM25_non_combustion <- ggplot(PM25_NC, aes(x=years, y=value/1e9, group=variable, color=variable)) +
  geom_line(size= 2) +
  labs(title = "PM2.5 from non-combustion sources") +
  labs(subtitle = "in thousand tons per year") +
  scale_color_manual(values = colors, labels = c("Tire AEO forecast", "Tire ZEV", "Tire ZEV Nationwide", "Brake AEO forecast", "Brake ZEV", "Brake ZEV Nationwide")) +
  bbc_style() +
  scale_y_continuous(breaks = c(4, 6, 8, 10)) +
  ylim(3.5, 10) +
  annotate("text", x=2022, y=9.5, label= "Brake", col="#000000", size=8, parse=TRUE, hjust = 0) +
  annotate("text", x=2022, y=5, label= "Tire", col="#000000", size=8, parse=TRUE, hjust = 0) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050)) +
  theme(plot.title = element_text(color = "#002554"),
        #legend.position = c(0.38, 0.95),
        legend.direction = "horizontal", 
        axis.line.x = element_line(size = 1, colour = "#cbcbcb", linetype=1))
finalise_plot(PM25_non_combustion, source_name = "ZEV = Zero-Emissions Vehicles policy implemented by California ", 
              save_filepath = "D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/08_Data_for_graphs/Brake_tire_PM25.png",
              width_pixels = 640, height_pixels = 450)
  # 3 - Monetized health impacts
health_impacts_standard <- read.csv(paste0(path, "health_benefits_default.csv")) %>%
  select(-X)
health_impacts_zev_ca <- read.csv(paste0(path, "health_benefits_ZEV-CA_zev.csv")) %>%
  select(-X)
health_impacts_zev_all <- read.csv(paste0(path, "health_benefits_ZEV-CA_All.csv")) %>%
  select(-X)


# 3.1 - Total benefits
temp <- c("Standard", "ZEV-CA_CG", "ZEV-all_CG", "ZEV-CA_RE", "ZEV_all_RE")
total_benefits <- data.frame(temp) %>%
  add_column("Benefits" = NA)
#total_benefits$Benefits[which(total_benefits$temp == "Standard")] <- health_impacts_standard$Total_health_benefits[which(health_impacts_standard$Year == 0)]
#total_benefits$Benefits[which(total_benefits$temp == "ZEV-CA")] <- health_impacts_zev_ca$Total_health_benefits[which(health_impacts_zev_ca$Year == 0)]
#total_benefits$Benefits[which(total_benefits$temp == "ZEV-CA_all")] <- health_impacts_zev_all$Total_health_benefits[which(health_impacts_zev_all$Year == 0)]
total_benefits$Benefits <- c("38.3", "29", "25", "44.8", "64")
temp_graph <- c("AEO forecast", "ZEV Current Grid", "ZEV Nationwide Current Grid", "ZEV Renewables", "ZEV Nationwide Renewables")

colors <- c("#5c0e30", "#912539", "#c14737", "#e77329", "#ffa600")
benefits_total <- ggplot(total_benefits, aes(x=temp_graph, y=Benefits, color = temp)) +
  geom_bar(stat = "identity", color = colors, fill = colors) +
  labs(title = "Monetized health benefits 2022-2050") +
  labs(subtitle = "in billions USD - cumulated") +
  bbc_style() +
  #scale_y_continuous(breaks = c(0, 4, 8, 12)) +
  #scale_x_continuous(breaks = c(2022, 2030, 2040, 2050)) +
  #ylim(0, 12) +
  theme(legend.position="none",
        plot.title = element_text(color = "#002554"),
        #legend.position = c(0.38, 0.95),
        legend.direction = "horizontal")
finalise_plot(benefits_total, source_name = "ZEV policy applied to different states - Health benefits calculated for the entire U.S.", 
              save_filepath = "D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/08_Data_for_graphs/Total_health_benefits.png",
              width_pixels = 1500, height_pixels = 900)

# 3.2 - Cumulated benefits relative to the baseline scenario

relative_benefits <- total_benefits
for (i in 1:length(relative_benefits$temp)) {
  relative_benefits$Benefits[i] <- total_benefits$Benefits[i]-total_benefits$Benefits[1]
}
relative_benefits <- relative_benefits[2:3,]

colors <- c("#ab335f", "#cf8096", "#eec8d1")
benefits_relative <- ggplot(relative_benefits, aes(x=temp_graph, y=Benefits/1e9, color = temp)) +
  geom_bar(stat = "identity", color = colors, fill = colors) +
  labs(title = "Monetized health benefits 2022-2050") +
  labs(subtitle = "in billions USD - cumulated - relative to baseline scenario") +
  bbc_style() +
  #scale_y_continuous(breaks = c(0, 4, 8, 12)) +
  #scale_x_continuous(breaks = c(2022, 2030, 2040, 2050)) +
  #ylim(0, 12) +
  theme(legend.position="none",
        plot.title = element_text(color = "#002554"),
        #legend.position = c(0.38, 0.95),
        legend.direction = "horizontal")
finalise_plot(relative_benefits, source_name = "ZEV policy applied to different states - Health benefits calculated for the entire U.S.", 
              save_filepath = "D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/08_Data_for_graphs/Total_health_benefits.png",
              width_pixels = 640, height_pixels = 450)


# 3.3 - Yearly evolution of the cumulated benefits

years <- unique(health_impacts_standard$Year)
yearly_health_benefits <- data.frame(years) %>%
  add_column("Standard" = health_impacts_standard$Total_health_benefits) %>%
  add_column("ZEV-CA" = health_impacts_zev_ca$Total_health_benefits) %>%
  add_column("ZEC-CA_all" = health_impacts_zev_all$Total_health_benefits) %>%
  add_column("Standard_Sum" = NA) %>%
  add_column("ZEC-CA_Sum" = NA) %>%
  add_column("ZEV-CA_all_Sum" = NA)
yearly_health_benefits <- yearly_health_benefits[which(yearly_health_benefits$years !=0),]
for (i in 1:length(yearly_health_benefits$years)) {
  if (i == 1) {
    yearly_health_benefits$Standard_Sum[i] <- yearly_health_benefits$Standard[i]
    yearly_health_benefits$`ZEC-CA_Sum`[i] <- yearly_health_benefits$`ZEV-CA`[i]
    yearly_health_benefits$`ZEV-CA_all_Sum`[i] <- yearly_health_benefits$`ZEC-CA_all`[i] 
  } else {
    yearly_health_benefits$Standard_Sum[i] <- yearly_health_benefits$Standard_Sum[i-1] + yearly_health_benefits$Standard[i]
    yearly_health_benefits$`ZEC-CA_Sum`[i] <- yearly_health_benefits$`ZEC-CA_Sum`[i-1] + yearly_health_benefits$`ZEV-CA`[i]
    yearly_health_benefits$`ZEV-CA_all_Sum`[i] <- yearly_health_benefits$`ZEV-CA_all_Sum`[i-1] + yearly_health_benefits$`ZEC-CA_all`[i] 
  }
}
yearly_health_benefits <- select(yearly_health_benefits, c(years, Standard_Sum, `ZEC-CA_Sum`, `ZEV-CA_all_Sum`))
yearly_health_benefits[nrow(yearly_health_benefits)+1,] <- c(2022, 0, 0, 0)
yearly_health_benefits <- melt(yearly_health_benefits, id = c("years"))
colors <- c("#004aab", "#767db5", "#b5b5be")
yearly_health <- ggplot(yearly_health_benefits, aes(x=years, y=value/1e9, group=variable, color=variable)) +
  geom_line(size= 2) +
  labs(title = "Health benefits") +
  labs(subtitle = "in billion USD") +
  scale_color_manual(values = colors, labels = c("AEO forecast", "ZEV", "ZEV Nationwide")) +
  bbc_style() +
  scale_y_continuous(breaks = c(0, 20, 40, 60)) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050)) +
  ylim(0,65) +
  theme(plot.title = element_text(color = "#002554"),
        #legend.position = c(0.38, 0.95),
        legend.direction = "horizontal")
finalise_plot(yearly_health, source_name = "ZEV = Zero-Emissions Vehicles policy implemented by California ", 
              save_filepath = "D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/08_Data_for_graphs/yearly_health_benefits.png",
              width_pixels = 640, height_pixels = 450)

# 4. Impact of adoption timeline on emissions of GHG and on the health benefits
health_impacts_zev_all <- read.csv(paste0(path, "health_benefits_ZEV-CA_All.csv")) %>%
  select(-X) %>%
  add_column("Sum" = NA)
health_impacts_zev_all[nrow(health_impacts_zev_all)+1,] <- c(2022, 0, 0)
health_impacts_zev_all <- health_impacts_zev_all[which(health_impacts_zev_all$Year != 0),] %>%
  arrange(Year)
health_impacts_zev_all_P2 <- read.csv(paste0(path, "health_benefits_ZEV-CA_All_P2.csv")) %>%
  select(-X) %>%
  add_column("Sum" = NA)
health_impacts_zev_all_P2[nrow(health_impacts_zev_all_P2)+1,] <- c(2022, 0, 0)
health_impacts_zev_all_P2 <- health_impacts_zev_all_P2[which(health_impacts_zev_all_P2$Year != 0),] %>%
  arrange(Year)
health_impacts_zev_all_P5 <- read.csv(paste0(path, "health_benefits_ZEV-CA_All_P5.csv")) %>%
  select(-X) %>%
  add_column("Sum" = NA)
health_impacts_zev_all_P5[nrow(health_impacts_zev_all_P5)+1,] <- c(2022, 0, 0)
health_impacts_zev_all_P5 <- health_impacts_zev_all_P5[which(health_impacts_zev_all_P5$Year != 0),] %>%
  arrange(Year)
health_impacts_zev_all_P10 <- read.csv(paste0(path, "health_benefits_ZEV-CA_All_P10.csv")) %>%
  select(-X) %>%
  add_column("Sum" = NA)
health_impacts_zev_all_P10[nrow(health_impacts_zev_all_P10)+1,] <- c(2022, 0, 0)
health_impacts_zev_all_P10 <- health_impacts_zev_all_P10[which(health_impacts_zev_all_P10$Year != 0),] %>%
  arrange(Year)
health_impacts_zev_all_P15 <- read.csv(paste0(path, "health_benefits_ZEV-CA_All_P15.csv")) %>%
  select(-X) %>%
  add_column("Sum" = NA)
health_impacts_zev_all_P15[nrow(health_impacts_zev_all_P15)+1,] <- c(2022, 0, 0)
health_impacts_zev_all_P15 <- health_impacts_zev_all_P15[which(health_impacts_zev_all_P15$Year != 0),] %>%
  arrange(Year)

for (i in 2:length(health_impacts_zev_all$Year)) {
  if (i == 2) {
    health_impacts_zev_all$Sum[2] <- health_impacts_zev_all$Total_health_benefits[2]
    health_impacts_zev_all_P2$Sum[2] <- health_impacts_zev_all_P2$Total_health_benefits[2]
    health_impacts_zev_all_P5$Sum[2] <- health_impacts_zev_all_P5$Total_health_benefits[2]
    health_impacts_zev_all_P10$Sum[2] <- health_impacts_zev_all_P10$Total_health_benefits[2]
    health_impacts_zev_all_P15$Sum[2] <- health_impacts_zev_all_P15$Total_health_benefits[2]
  } else {
    health_impacts_zev_all$Sum[i] <- health_impacts_zev_all$Sum[i-1]+health_impacts_zev_all$Total_health_benefits[i]
    health_impacts_zev_all_P2$Sum[i] <- health_impacts_zev_all_P2$Sum[i-1]+health_impacts_zev_all_P2$Total_health_benefits[i]
    health_impacts_zev_all_P5$Sum[i] <- health_impacts_zev_all_P5$Sum[i-1]+health_impacts_zev_all_P5$Total_health_benefits[i]
    health_impacts_zev_all_P10$Sum[i] <- health_impacts_zev_all_P10$Sum[i-1]+health_impacts_zev_all_P10$Total_health_benefits[i]
    health_impacts_zev_all_P15$Sum[i] <- health_impacts_zev_all_P15$Sum[i-1]+health_impacts_zev_all_P15$Total_health_benefits[i]
  }
}

health_impact_timeline <- data.frame(health_impacts_zev_all$Year) %>%
  cbind(health_impacts_zev_all$Sum) %>%
  cbind(health_impacts_zev_all_P2$Sum) %>%
  cbind(health_impacts_zev_all_P5$Sum) %>%
  cbind(health_impacts_zev_all_P10$Sum) %>%
  cbind(health_impacts_zev_all_P15$Sum)
colnames(health_impact_timeline) <- c("Year", "ZEV", "ZEV2", "ZEV5", "ZEV10", "ZEV15")
relative_health_impact_timeline <- health_impact_timeline
health_impact_timeline <- melt(health_impact_timeline, id = c("Year"))
health_impact_timeline <- mutate(health_impact_timeline, variable = fct_relevel(variable,
                                                  "ZEV", "ZEV2", "ZEV5", "ZEV10", "ZEV15"))

colors <- c("#239400", "#5ca942", "#87be70", "#aed39d", "#d5e8cb")
zev_timeline <- ggplot(health_impact_timeline, aes(x=Year, y=value/1e9, group=variable, color=variable)) +
  geom_line(size= 2) +
  labs(title = "Health benefits") +
  labs(subtitle = "in billion USD") +
  scale_color_manual(values = colors, labels = c("Original timeline", "2 years", "5 years", "10 years", "15 years")) +
  bbc_style() +
  scale_y_continuous(breaks = c(0, 20, 40, 60)) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050)) +
  ylim(0,65) +
  theme(plot.title = element_text(color = "#002554"),
        #legend.position = c(0.38, 0.95),
        legend.direction = "horizontal")
finalise_plot(zev_timeline, source_name = "ZEV = Zero-Emissions Vehicles policy implemented by California ", 
              save_filepath = "D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/08_Data_for_graphs/benefits_impact_timeline.png",
              width_pixels = 640, height_pixels = 450)

# Relative health impact by changes in the timeline
relative_health_impact_timeline$ZEV2 <- relative_health_impact_timeline$ZEV2 - relative_health_impact_timeline$ZEV
relative_health_impact_timeline$ZEV5 <- relative_health_impact_timeline$ZEV5 - relative_health_impact_timeline$ZEV
relative_health_impact_timeline$ZEV10 <- relative_health_impact_timeline$ZEV10 - relative_health_impact_timeline$ZEV
relative_health_impact_timeline$ZEV15 <- relative_health_impact_timeline$ZEV15 - relative_health_impact_timeline$ZEV
relative_health_impact_timeline <- select(relative_health_impact_timeline, -c("ZEV"))
relative_health_impact_timeline <- melt(relative_health_impact_timeline, id = c("Year"))
relative_health_impact_timeline <- mutate(relative_health_impact_timeline, variable = fct_relevel(variable,
                                                                                "ZEV", "ZEV2", "ZEV5", "ZEV10", "ZEV15"))

colors <- c("#002554", "#364a7c", "#6174a6", "#8ea0d2", "#bcceff")
zev_timeline_rel <- ggplot(relative_health_impact_timeline, aes(x=Year, y=value/1e9, group=variable, color=variable)) +
  geom_line(size= 2) +
  labs(title = "Impact of policy delays on health benefits") +
  labs(subtitle = "in billion USD - original timeline as reference") +
  scale_color_manual(values = colors, labels = c("2 years late", "5 years late", "10 years late", "15 years late")) +
  bbc_style() +
  scale_y_continuous(breaks = c(-20, -10, 0)) +
  scale_x_continuous(breaks = c(2022, 2030, 2040, 2050)) +
  ylim(-20,0) +
  theme(plot.title = element_text(color = "#002554"),
        #legend.position = c(0.38, 0.95),
        legend.direction = "horizontal")
finalise_plot(zev_timeline_rel, source_name = "ZEV policy applied to all states - Electricity scenario: 100% renewables", 
              save_filepath = "D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/08_Data_for_graphs/relative_impact_timeline.png",
              width_pixels = 640, height_pixels = 450)



# Map of benefits per inhabitant by state and by county
#results_folder <- "2023-04-28_noEVs_NREL_intermediate_agg_results"
#results_folder <- "2023-04-28_default_NREL_intermediate_agg_results"
#results_folder <- "2023-04-28_ZEV_CA_NREL_intermediate_agg_results"
#results_folder <- "2023-04-28_ZEV_All_NREL_intermediate_agg_results"
#results_folder <- "2023-04-28_ZEV_All_current_mix_agg_results"
#results_folder <- "2023-04-28_ZEV_All_NREL_low_case_agg_results"
#results_folder <- "2023-04-28_ZEV_All_NREL_high_decarb_results"
#results_folder <- "2023-04-26_elec100truck100_current_mix_agg_results"
#results_folder <- "2023-04-26_elec100car100_current_mix_agg_results"
#results_folder <- "2023-05-17_ZEV_All_NREL_intermediate_agg_results"
results_folder <-  "2023-05-20_ZEV_All_NREL_high_decarb_results"

#default_folder <- "2023-04-26_elec0truck100_current_mix_agg_results"
#default_folder <- "2023-04-26_elec0car100_current_mix_agg_results"
#default_folder <- "2023-04-28_noEVs_NREL_intermediate_agg_results"
default_folder <- "2023-05-19_noEVs_NREL_intermediate_results"

default <- read.csv(paste0("D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_FLAME-AQ/outputs/air_quality/", default_folder, "/health_benefits_by_county.csv"))
scenario <- read.csv(paste0("D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_FLAME-AQ/outputs/air_quality/", results_folder, "/health_benefits_by_county.csv"))
#ZEV_all_renewables <- read.csv("D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_FLAME-AQ/outputs/air_quality/2023-02-20_ZEV_All_zev_renewables_results/health_benefits_by_county.csv")
data_for_graph <- scenario
data_for_graph$Total_health_benefits <-  data_for_graph$Total_health_benefits - default$Total_health_benefits
#data_for_graph <- ZEV_all_current_grid

us_geo <- tigris::counties(class = "sf") 
contiguous_states <- us_geo %>% 
  filter(STATEFP < 60 & STATEFP != "02" & STATEFP != "15") %>% 
  shift_geometry()
contiguous_states <- contiguous_states %>%
  add_column("HEALTH_BENEFITS" = NA) %>%
  add_column("POPULATION" = NA) %>%
  add_column("BENEFITS_PER_POP" = NA)
#Load population files also used in COBRA
filenames <- list.files("D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_Updated_FLAME/inputs/air_quality/cobra_population", pattern="*.csv", full.names=TRUE)
for (i in 1:length(filenames)) {
  temp <- read.csv(filenames[i])
  if (i == 1) {
    population_by_county <- temp
  } else {
    population_by_county <- rbind(population_by_county, temp)
  }
}
population_by_county <- add_column(population_by_county, "Total_pop" = NA)
population_by_county$Total_pop <- rowSums(population_by_county[,4:(length(colnames(population_by_county))-1)])
population_by_county <- select(population_by_county, c("Year", "FIPS", "Total_pop"))
population_by_county$FIPS[which(population_by_county$FIPS<10000)] <- paste0("0", population_by_county$FIPS[which(population_by_county$FIPS<10000)])
# Add data to the mapping dataset
health_benefits_by_county <- data_for_graph
#health_benefits_by_county <- filter(health_benefits_by_county, health_benefits_by_county$Year == 0)
health_benefits_by_county$FIPS[which(health_benefits_by_county$FIPS < 10000)] <- paste0("0", health_benefits_by_county$FIPS[which(health_benefits_by_county$FIPS < 10000)])
for (i in unique(health_benefits_by_county$FIPS)) {
  temp <- which(contiguous_states$GEOID == i)
  contiguous_states$HEALTH_BENEFITS[temp] <- health_benefits_by_county$Total_health_benefits[which(health_benefits_by_county$FIPS == i)]
  if (length(which(population_by_county$FIPS == i)) != 0) {
    contiguous_states$POPULATION[temp] <- population_by_county$Total_pop[which(population_by_county$Year == 2035 & population_by_county$FIPS == i)]
  } else {
    contiguous_states$POPULATION[temp] <- NA
  }
}
plant_location <- read.csv("D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_FLAME-AQ/inputs/air_quality/eGRID_plant_data.csv")
plant_location <- plant_location[!is.na(plant_location$cnt_id),]
fossil_fuels <- c("GAS", "OIL", "COAL")
plant_location <- plant_location[plant_location$Fuel%in%fossil_fuels,]
contiguous_states <- contiguous_states %>%
  add_column("Fuel" = NA) %>%
  add_column("Annual_Generation" = NA) %>%
  add_column("NOx_emissions" = NA) %>%
  add_column("SO2_emissions" = NA)
for (i in 1:dim(plant_location)[1]) {
  if (as.numeric(plant_location$st_id[i]) < 10) {
    plant_location$st_id[i] <- paste0("0", plant_location$st_id[i])
  }
  if (as.numeric(plant_location$cnt_id[i]) < 10) {
    plant_location$cnt_id[i] <- paste0("00", plant_location$cnt_id[i])
  } else if (as.numeric(plant_location$cnt_id[i]) < 100) {
    plant_location$cnt_id[i] <- paste0("0", plant_location$cnt_id[i])
  }
  temp <- which(contiguous_states$STATEFP == plant_location$st_id[i] & contiguous_states$COUNTYFP == plant_location$cnt_id[i])
  contiguous_states$Fuel[temp] <- plant_location$Fuel[i]
  contiguous_states$Annual_Generation[temp] <- plant_location$Annual_generation[i]
  contiguous_states$NOx_emissions[temp] <- plant_location$Nox_emissions[i]
  contiguous_states$SO2_emissions[temp] <- plant_location$SO2_emissions[i]
}

contiguous_states$BENEFITS_PER_POP <- contiguous_states$HEALTH_BENEFITS/contiguous_states$POPULATION

bbox_new <- st_bbox(contiguous_states)
xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
bbox_new[1] <- bbox_new[1] - (0.2 * xrange) # xmin - left
#bbox_new[3] <- bbox_new[3] + (0.25 * xrange) # xmax - right
#bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
bbox_new[4] <- bbox_new[4] + (0.1 * yrange) # ymax - top
bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

tmap_mode("plot")
map <- tm_shape(contiguous_states, bbox = bbox_new) +
  tm_polygons("BENEFITS_PER_POP",
              breaks=c(-500, -400, -300, -200, -100, -50, -25, 0, 25, 50, 100, 200, 300, 400, 500),
              #breaks=c(-1000, 0, 1000),
              id = "NAME",
              palette = "RdYlBu",
              #n = 10,
              title = "Benefits per capita [USD]",
              legend.is.portrait = TRUE, 
              border.col = "black",
              lwd = 0.5) +
  #tm_bubbles("Annual_Generation", 
             #size = "Annual_Generation",
  #           col = "Fuel",
  #           alpha = 1,
  #           legend.size.show = FALSE,
  #           legend.col.show = TRUE,
  #           legend.shape.show = FALSE,
  #           border.lwd = 0,
  #           palette = c("#000000", "#365d89", "#656565"))+
  tm_layout(legend.title.size = 2,
            legend.text.size = 1.2,
            legend.position = c("left","top"),
            frame = FALSE)+
  #tm_add_legend("symbol", col = c("green", "black", "blue", "grey"), size = c(.1,.5), labels = c("Biomass", "Coal", "Natural gas", "Oil"))+
  tm_view(
    #frame = FALSE,  
    #view.legend.position = c("right","bottom")
  )
#map_name <- paste0(Sys.Date(), "_benefits_per_capita_county.bmp")
map_name <- results_folder
path_map <- "D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_FLAME-AQ/plots/"
tmap_save(map, paste0(path_map, map_name, ".bmp"), dpi=600)


# Fraction of counties with negative benefits
perc_negative <- length(which(health_benefits_by_county$Total_health_benefits <0))/length(health_benefits_by_county$Total_health_benefits)*100
max <- max(contiguous_states$BENEFITS_PER_POP, na.rm = TRUE)
min <- min(contiguous_states$BENEFITS_PER_POP, na.rm = TRUE)
mean <- mean(contiguous_states$BENEFITS_PER_POP, na.rm = TRUE)
stddev <- sd(contiguous_states$BENEFITS_PER_POP, na.rm = TRUE)
# Total popultion facing positive and negative health outcomes
pop_total <- sum(contiguous_states$POPULATION, na.rm = TRUE)
pop_positive <- sum(contiguous_states$POPULATION[which(contiguous_states$BENEFITS_PER_POP >= 0)])
pop_negative <- sum(contiguous_states$POPULATION[which(contiguous_states$BENEFITS_PER_POP <= 0)])


# Map the benefits by state / given in dollars per inhabitant
us_geo_states <- tigris::states(class = "sf") 
contiguous_states_st <- us_geo_states %>% 
  filter(STATEFP < 60 & STATEFP != "02" & STATEFP != "15")
contiguous_states_st <- contiguous_states_st %>%
  add_column("HEALTH_BENEFITS" = NA) %>%
  add_column("POPULATION" = NA) %>%
  add_column("BENEFITS_PER_POP" = NA)
#Load population files also used in COBRA
filenames <- list.files("D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_Updated_FLAME/inputs/air_quality/cobra_population", pattern="*.csv", full.names=TRUE)
for (i in 1:length(filenames)) {
  temp <- read.csv(filenames[i])
  if (i == 1) {
    population_by_state <- temp
  } else {
    population_by_state <- rbind(population_by_state, temp)
  }
}
population_by_state$FIPS[which(population_by_state$FIPS<10000)] <- paste0("0", population_by_state$FIPS[which(population_by_state$FIPS<10000)])
population_by_state <- add_column(population_by_state, "Total_pop" = NA)
population_by_state$Total_pop <- rowSums(population_by_state[,4:(length(colnames(population_by_state))-1)])
population_by_state <- population_by_state %>%
  add_column("STATE_ID" = substr(population_by_state$FIPS,1,2)) %>%
  select(c("Year", "STATE_ID", "Total_pop")) %>%
  aggregate(Total_pop ~ Year + STATE_ID, sum)

# Add data to the mapping dataset
health_benefits_by_state <- read.csv("D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_FLAME-AQ/outputs/air_quality/2023-02-17_ZEV_All_zev_current_grid_results/health_benefits_by_state.csv")
health_benefits_by_state <- filter(health_benefits_by_state, health_benefits_by_state$Year == 0)
health_benefits_by_state$State <- toupper(health_benefits_by_state$State)
for (i in unique(health_benefits_by_state$State)) {
  temp <- which(toupper(contiguous_states_st$NAME) == i)
  contiguous_states_st$HEALTH_BENEFITS[temp] <- health_benefits_by_state$Total_health_benefits[which(health_benefits_by_state$Year == "0" & health_benefits_by_state$State == i)]
  if (length(which(population_by_state$Year == "2050" & population_by_state$STATE_ID == contiguous_states_st$GEOID[temp])) != 0) {
    contiguous_states_st$POPULATION[temp] <- population_by_state$Total_pop[which(population_by_state$Year == "2050" & population_by_state$STATE_ID == contiguous_states_st$GEOID[temp])]
  } else {
    contiguous_states_st$POPULATION[temp] <- NA
  }
}
contiguous_states_st$BENEFITS_PER_POP <- contiguous_states_st$HEALTH_BENEFITS/contiguous_states_st$POPULATION
tmap_mode("view")
tm_shape(contiguous_states_st) +
  tm_polygons("BENEFITS_PER_POP",
              id = "NAME",
              n = 5,
              title = "Air quality benefits of fleet electrification - 2050 [USD per inhabitant]",
              legend.is.portrait = FALSE) +
  tm_layout(
    frame = FALSE,
    legend.outside = TRUE,
    legend.outside.position = "bottom"
  )

class(contiguous_states_st)

# Evolution
results_path <- "D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_FLAME-AQ/outputs/air_quality/"
lifes_results <- list.files(path = results_path, pattern = "results")


# Changes in pollutants emissions in each county - comparing baseline and full electrification scenarios
path_default <-"D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_FLAME-AQ/outputs/air_quality/2023-02-20_baseline_zev_current_grid_results/"
path_electrification <- "D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_FLAME-AQ/outputs/air_quality/2023-02-17_ZEV_All_zev_current_grid_results/"
baseline_default <- read.csv(paste0(path_default, "COBRA_baseline_files/COBRA_emissions_baseline_2023.csv"))
final_default <- read.csv(paste0(path_default, "COBRA_scenario_files/COBRA_emissions_scenario_2050.csv"))
baseline_electrification <- read.csv(paste0(path_electrification, "COBRA_baseline_files/COBRA_emissions_baseline_2023.csv"))
final_electrification <- read.csv(paste0(path_electrification, "COBRA_scenario_files/COBRA_emissions_scenario_2050.csv"))
differences_default <- final_default
differences_electrification <- final_electrification
for (i in 9:length(colnames(differences_default))) {
  differences_default[,i] <- final_default[,i]-baseline_default[,i]
  differences_electrification[,i] <- final_electrification[,i]-baseline_electrification[,i]
}
# Add data to the mapping dataset
differences_default$stid <- paste0("00", differences_default$stid)
differences_default$stid <- substr(differences_default$stid, nchar(differences_default$stid)-1, nchar(differences_default$stid))
differences_default$cyid <- paste0("00", differences_default$cyid)
differences_default$cyid <- substr(differences_default$cyid, nchar(differences_default$cyid)-2, nchar(differences_default$cyid))
differences_electrification$stid <- paste0("00", differences_electrification$stid)
differences_electrification$stid <- substr(differences_electrification$stid, nchar(differences_electrification$stid)-1, nchar(differences_electrification$stid))
differences_electrification$cyid <- paste0("00", differences_electrification$cyid)
differences_electrification$cyid <- substr(differences_electrification$cyid, nchar(differences_electrification$cyid)-2, nchar(differences_electrification$cyid))
differences_default <- add_column(differences_default, "GEOID" = NA)
differences_default$GEOID <- paste0(differences_default$stid, differences_default$cyid)
differences_electrification <- add_column(differences_electrification, "GEOID" = NA)
differences_electrification$GEOID <- paste0(differences_electrification$stid, differences_electrification$cyid)

delta <- differences_electrification
for (i in 9:(length(colnames(differences_default))-1)) {
  delta[,i] <- differences_electrification[,i]-differences_default[,i]
}

us_geo <- tigris::counties(class = "sf") 
contiguous_states <- us_geo %>% 
  filter(STATEFP < 60 & STATEFP != "02" & STATEFP != "15")
contiguous_states <- contiguous_states %>%
  add_column("NOx" = NA) %>%
  add_column("SO2" = NA) %>%
  add_column("PM25" = NA)
for (i in 1:dim(contiguous_states)[1]) {
  temp <- which(delta$GEOID == contiguous_states$GEOID[i])
  contiguous_states$NOx[i] <- sum(delta$NO2[temp])
  contiguous_states$SO2[i] <- sum(delta$SO2[temp])
  contiguous_states$PM25[i] <- sum(delta$PM25[temp])
}

tmap_mode("view")
tm_shape(contiguous_states) +
  tm_polygons("PM25",
              id = "NAME",
              palette = "seq",
              n = 5,
              title = "Changes in PM2.5 emissions 2023-2050 in the high electrification scenario compared to the baseline scenario [tons]",
              legend.is.portrait = FALSE) +
  tm_layout(
    aes.palette = list(seq = "-RdYlGn"),
    frame = FALSE,
    legend.outside = TRUE,
    legend.outside.position = "bottom"
  )
