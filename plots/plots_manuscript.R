# Generation of the graphs for the manuscript
# Libraries
#install.packages("rlang")
#install.packages("cli")
#install.packages("utf8")
#install.packages("vctrs")
#install.packages("fansi")
#install.packages("tibble")
#install.packages("gridExtra")
devtools::install_github("hughjonesd/ggmagnify", force = TRUE)
install.packages(c("ggfx", "ggforce"))
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(reshape2)
library(ggthemes)
library(ggmagnify)
library(bbplot)
library(gridExtra)
library(tibble)

path <- "D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_FLAME-AQ/"
inputs <- "outputs/air_quality/"
output <- "D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_FLAME-AQ/plots/"

## Section 1 - Total benefits for different electrification and electrical grid scenarios
# Graph 1A - Evolution of the total benefits of the Renewal only scenario and full electrification with intermediate grid
renewal_scenario <- "2023-05-19_noEVs_NREL_intermediate_results"
#electrification_scenario <- "2023-05-10_ZEV_All_NREL_intermediate_results"
renewal_benefits <- read.csv(paste0(path, inputs, renewal_scenario, "/health_benefits.csv"))
#electrification_benefits <- read.csv(paste0(path, inputs, electrification_scenario, "/health_benefits.csv"))
#graph_data <- data.frame(cbind(Year = renewal_benefits$Year, Renewal = renewal_benefits$Total_benefits, Electrification = electrification_benefits$Total_benefits))
graph_data <- data.frame(cbind(Year = renewal_benefits$Year, Renewal = renewal_benefits$Total_benefits))
graph_data <- filter(graph_data, graph_data$Year != 0)
graph_data_long <- melt(graph_data, id.vars=c("Year"))

# Plot
cols_points <- c("#9d3926")
cols_line <- c("#e9af9e")
theme_set(theme_bw())
from <- c(2026, 2030, 25, 125)
to <- c(2024, 2032, 250, 650)
plot1 <- ggplot(graph_data_long, aes(x = Year, y = value/1e9, color = variable)) +
  geom_line(lwd = 1.5, aes(col = variable)) +
  geom_point(size = 3, stroke = 0, shape = 21, aes(fill = variable)) +
  scale_fill_manual(values = cols_points) +
  scale_color_manual(values = cols_line) +
  labs(x = "Year") +
  labs(y = "Health benefits [bn. USD]") +
  xlim(c(2022, 2050)) +
  #bbc_style() +
  scale_x_continuous(breaks=seq(2022,2050,by=4)) +
  scale_y_continuous(breaks = seq(0, 600, 100), 
                     limits = c(-10,600), 
                     expand = c(0,0)) +
  labs(title = "a.",
       subtitle = "Fleet renewal \n Reference: 2021 fleet") +
  #theme(plot.title = element_text(color = "#002554"),
  #      legend.position = c(0.38, 0.95),
  #      legend.direction = "horizontal") +
  #annotate("text", x=2022, y=690, label= "a.") +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15, margin=margin(r=15)),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black", size = 1),
        axis.line.x = element_line(colour = "black", size = 1),
        axis.ticks.y = element_line(size = 1),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.length=unit(0.1,"inch"),
        legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 20, vjust = -10),
        plot.subtitle = element_text(size = 15, hjust = 0.5, vjust = -15))
  #geom_magnify(from = from, 
  #             to = to, 
  #             shadow = FALSE)


plot1
#finalise_plot(plot, source_name = "Figure 1a", 
#              save_filepath = paste0(output, Sys.Date(), "_renewal_vs_current_fleet.png"),
#              width_pixels = 640, height_pixels = 450)

# Graph 1B - Display of fleet electrification compared to fleet renewal 
renewal_scenario <- "2023-05-19_noEVs_NREL_intermediate_results"
electrification_scenario <- "2023-05-19_ZEV_All_NREL_intermediate_results"
renewal_benefits <- read.csv(paste0(path, inputs, renewal_scenario, "/health_benefits.csv"))
electrification_benefits <- read.csv(paste0(path, inputs, electrification_scenario, "/health_benefits.csv"))
#graph_data <- data.frame(cbind(Year = renewal_benefits$Year, Renewal = renewal_benefits$Total_benefits, Electrification = electrification_benefits$Total_benefits))
graph_data_plot2 <- data.frame(cbind(Year = renewal_benefits$Year, Electrification = -(renewal_benefits$Total_benefits-electrification_benefits$Total_benefits)))
graph_data_plot2 <- filter(graph_data_plot2, graph_data_plot2$Year != 0)
graph_data_long_plot2 <- melt(graph_data_plot2, id.vars=c("Year"))

# Plot
cols_points <- c("#004c6d")
cols_line <- c("#a2cde6")
theme_set(theme_bw())
from <- c(2026, 2030, 25, 125)
to <- c(2024, 2032, 250, 650)
plot2 <- ggplot(graph_data_long_plot2, aes(x = Year, y = value/1e9, color = variable)) +
  geom_line(lwd = 1.5, aes(col = variable)) +
  geom_point(size = 3, stroke = 0, shape = 21, aes(fill = variable)) +
  scale_fill_manual(values=cols_points) +
  scale_color_manual(values = cols_line) +
  labs(x = "Year") +
  labs(y = "Health benefits [bn. USD]") +
  xlim(c(2022, 2050)) +
  #bbc_style() +
  scale_x_continuous(breaks=seq(2022,2050,by=4)) +
  scale_y_continuous(breaks = seq(0, 160, 20), 
                     limits = c(-10,160), 
                     expand = c(0,0)) +
  labs(title = "b.",
       subtitle = "Fleet electrification \n Reference: fleet renewal") +
  theme(axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15, margin=margin(r=10)),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black", size = 1),
        axis.line.x = element_line(colour = "black", size = 1),
        axis.ticks.y = element_line(size = 1),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.length=unit(0.1,"inch"),
        legend.position = "none",
        plot.title.position = "plot",
        plot.title = element_text(size = 20, vjust = -10),
        plot.subtitle = element_text(size = 15, hjust = 0.5, vjust = -15))
  #geom_magnify(from = from, 
  #             to = to, 
  #             shadow = FALSE)
plot2
#finalise_plot(plot, source_name = "Figure 1a", 
#              save_filepath = paste0(output, Sys.Date(), "_renewal_vs_electrification.png"),
              #width_pixels = 640, height_pixels = 450)

# Graph 1C - Barplot of the health benefits in the different scenarios
renewal_only_scenario <- "2023-05-19_noEVs_NREL_intermediate_results"
AEO_scenario <- "2023-05-23_default_NREL_intermediate_results"
zev_ca_scenario <- "2023-05-20_ZEV_CA_NREL_intermediate_results"
zev_inter_scenario <- "2023-05-19_ZEV_All_NREL_intermediate_results"
zev_current_scenario <- "2023-05-20_ZEV_All_current_mix_results"
zev_low_case_scenario <- "2023-05-20_ZEV_All_NREL_low_case_results"
zev_high_decarb_scenario <- "2023-05-20_ZEV_All_NREL_high_decarb_results"

#Load the input data
renewal_data <- read.csv(paste0(path, inputs, renewal_only_scenario, "/health_benefits.csv"))
aeo_data <- read.csv(paste0(path, inputs, AEO_scenario, "/health_benefits.csv"))
zev_ca_data <- read.csv(paste0(path, inputs, zev_ca_scenario, "/health_benefits.csv"))
zev_inter_data <- read.csv(paste0(path, inputs, zev_inter_scenario, "/health_benefits.csv"))
zev_current_data <- read.csv(paste0(path, inputs, zev_current_scenario, "/health_benefits.csv"))
zev_low_data <- read.csv(paste0(path, inputs, zev_low_case_scenario, "/health_benefits.csv"))
zev_high_data <- read.csv(paste0(path, inputs, zev_high_decarb_scenario, "/health_benefits.csv"))

# Creation of the data
barplot_data_electrification <- data.frame(
  scenario = c("AEO", "Limited ZEV", "Extended ZEV"),
  #value = c(renewal_data$Total_benefits[28], aeo_data$Total_benefits[28], zev_ca_data$Total_benefits[28], zev_inter_data$Total_benefits[28], zev_current_data$Total_benefits[28], zev_low_data$Total_benefits[28], zev_high_data$Total_benefits[28])
  value = c(aeo_data$Total_benefits[28]-renewal_data$Total_benefits[28], zev_ca_data$Total_benefits[28]-renewal_data$Total_benefits[28], zev_inter_data$Total_benefits[28]-renewal_data$Total_benefits[28])
)
barplot_data_grid <- data.frame(
  scenario = c("Current grid", "Low renewables", "Intermediate", "High renewables"),
  value = c(zev_current_data$Total_benefits[28]-renewal_data$Total_benefits[28], zev_low_data$Total_benefits[28]-renewal_data$Total_benefits[28], zev_inter_data$Total_benefits[28]-renewal_data$Total_benefits[28], zev_high_data$Total_benefits[28]-renewal_data$Total_benefits[28])
)
barplot_data_electrification$scenario <- factor(barplot_data_electrification$scenario, levels = barplot_data_electrification$scenario)
barplot_data_grid$scenario <- factor(barplot_data_grid$scenario, levels = barplot_data_grid$scenario)
# Absolute benefits
plot3 <- ggplot(barplot_data_electrification, aes(x=scenario, y=value/1e9, width = 0.75)) + 
  geom_col(colour = "black", size = 1, fill = "#a2a2a2") +
  geom_text(aes(label = round(value/1e9, digits = 0)), vjust = -0.5, colour = "black", size = 6) +
  scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 7)) +
  scale_y_continuous(breaks = seq(0, 200, 50), 
                     limits = c(0,200), 
                     expand = c(0,0)) +
  labs(title = "a.") +
  labs(y="Health benefits [bn. USD]", x=element_blank()) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.line.y = element_line(colour = "black", size = 1),
        axis.line.x = element_line(colour = "black", size = 1),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(size = 1),
        axis.ticks.length=unit(0.1,"inch"),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 18, margin=margin(r=10)),
        axis.text.x = element_text(size = 12),
        plot.title.position = "plot",
        plot.title = element_text(size = 20, vjust = 0),
        panel.background = element_blank())
plot3

plot4 <- ggplot(barplot_data_grid, aes(x=scenario, y=value/1e9, width = 0.75)) + 
  geom_col(colour = "black", size = 1, fill = "#a2a2a2") +
  geom_text(aes(y = round(value/1e9, digits = 0)+sign(value), label = round(value/1e9, digits = 0),
            vjust = ifelse(value >= 0, -1, 1.5),
            hjust = ifelse(value>= 0, 0.5, 0.5)),
            colour = "black", size = 6) +
  scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 7)) +
  scale_y_continuous(breaks = seq(-250, 300, 50), 
                     limits = c(-250,300), 
                     expand = c(0,0)) +
  labs(title = "b.") +
  labs(y="Health benefits [bn. USD]", x=element_blank()) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        axis.line.y = element_line(colour = "black", size = 1),
        axis.line.x = element_line(colour = "black", size = 1),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(size = 1),
        axis.ticks.length=unit(0.1,"inch"),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 18, margin=margin(r=10)),
        axis.text.x = element_text(size = 12),
        plot.title.position = "plot",
        plot.title = element_text(size = 20, vjust = 0),
        panel.background = element_blank())
plot4

# Paste all graphs in the same plot
plot3 <- plot3 + theme(plot.margin=unit(c(5,20,5,20),"pt"))
plot4 <- plot4 + theme(plot.margin=unit(c(5,20,5,20),"pt"))
grid.arrange(plot3, plot4, nrow = 1)
lay <- rbind(c(1,1,1,2,2,2,2))
figure1 <- grid.arrange(plot3, plot4, layout_matrix = lay)
ggsave(
  paste0(Sys.Date(), "_Fig1_scenario_comparison.png"),
  plot = figure1,
  path = output,
  width = 1200,
  height = 1200,
  unit = "px"
)
figure1



# Alternative figure 1
renewal_scenario <- "2023-05-19_noEVs_NREL_intermediate_results"
electrification_scenario <- "2023-05-19_ZEV_All_NREL_intermediate_results"
worst_case <- "2023-05-20_ZEV_All_current_mix_results"
best_case <- "2023-05-20_ZEV_All_NREL_high_decarb_results"
low_case <- "2023-05-20_ZEV_All_NREL_low_case_results"
renewal_benefits <- read.csv(paste0(path, inputs, renewal_scenario, "/health_benefits.csv"))
electrification_benefits <- read.csv(paste0(path, inputs, electrification_scenario, "/health_benefits.csv"))
worst_case_benefits <- read.csv(paste0(path, inputs, worst_case, "/health_benefits.csv"))
best_case_benefits <- read.csv(paste0(path, inputs, best_case, "/health_benefits.csv"))
low_case_benefits <- read.csv(paste0(path, inputs, low_case, "/health_benefits.csv"))

#graph_data <- data.frame(cbind(Year = renewal_benefits$Year, Renewal = renewal_benefits$Total_benefits, Electrification = electrification_benefits$Total_benefits))
graph_data_plot2 <- data.frame(cbind(Year = renewal_benefits$Year, Electrification = electrification_benefits$Total_benefits, Renewal = renewal_benefits$Total_benefits, Worst_Case = worst_case_benefits$Total_benefits, Best_Case = best_case_benefits$Total_benefits, Low_Case = low_case_benefits$Total_benefits))
graph_data_plot2 <- filter(graph_data_plot2, graph_data_plot2$Year != 0)
graph_data_long_plot2 <- melt(graph_data_plot2, id.vars=c("Year"))

# Plot
cols_points <- c("#004c6d", "black", "#940000", "#4bb23c")
cols_line <- c("#a2cde6", "grey", "#e7b1a3", "#b1dca5")
theme_set(theme_bw())
from <- c(2026, 2030, 25, 125)
to <- c(2024, 2032, 250, 650)
plot2 <- ggplot(graph_data_long_plot2, aes(x = Year, y = value/1e9, color = variable, shape = variable)) +
  geom_line(lwd = 1.5, aes(col = variable)) +
  geom_point(size = 3, stroke = 0, shape = 21, aes(fill = variable)) +
  scale_fill_manual(values=cols_points) +
  scale_color_manual(values = cols_line) +
  labs(x = "Year") +
  labs(y = "Health benefits [bn. USD]") +
  xlim(c(2022, 2050)) +
  
  scale_x_continuous(breaks=seq(2022,2050,by=4)) +
  scale_y_continuous(breaks = seq(0, 800, 100), 
                     limits = c(-10,800), 
                     expand = c(0,0)) +
  #labs(title = "Fleet electrification \n Reference: fleet renewal") +
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18, margin=margin(r=15)),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black", size = 1),
        axis.line.x = element_line(colour = "black", size = 1),
        axis.ticks.y = element_line(size = 1),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.length=unit(0.1,"inch"),
        #plot.title.position = "plot",
        #plot.title = element_text(size = 15, hjust = 0.5, vjust = -15),
        legend.title = element_blank(),
        legend.position = c(0.35,0.7),
        legend.text = element_text(size = 20),
        legend.spacing.y = unit(0, "inch"),
        legend.key.size = unit(0.4, "inch")) +
  guides(fill = guide_legend(byrow = TRUE))
plot2

# Impact of delays in the implementation of the electrification policy
renewal_scenario <- "2023-05-19_noEVs_NREL_intermediate_results"
electrification_scenario <- "2023-05-19_ZEV_All_NREL_intermediate_results"
delay1 <- "2023-05-22_ZEV_All_P1_NREL_intermediate_results"
delay2 <- "2023-05-22_ZEV_All_P2_NREL_intermediate_results"
delay5 <- "2023-05-23_ZEV_All_P5_NREL_intermediate_results"
delay10 <- "2023-05-23_ZEV_All_P10_NREL_intermediate_results"
delay15 <- "2023-05-23_ZEV_All_P15_NREL_intermediate_results"
renewal_benefits <- read.csv(paste0(path, inputs, renewal_scenario, "/health_benefits.csv"))
electrification_benefits <- read.csv(paste0(path, inputs, electrification_scenario, "/health_benefits.csv"))
delay1_benefits <- read.csv(paste0(path, inputs, delay1, "/health_benefits.csv"))
delay2_benefits <- read.csv(paste0(path, inputs, delay2, "/health_benefits.csv"))
delay5_benefits <- read.csv(paste0(path, inputs, delay5, "/health_benefits.csv"))
delay10_benefits <- read.csv(paste0(path, inputs, delay10, "/health_benefits.csv"))
delay15_benefits <- read.csv(paste0(path, inputs, delay15, "/health_benefits.csv"))

graph_data_plot3 <- data.frame(cbind(Year = renewal_benefits$Year, Electrification = electrification_benefits$Total_benefits-renewal_benefits$Total_benefits, Renewal = renewal_benefits$Total_benefits-renewal_benefits$Total_benefits, Delay1 = delay1_benefits$Total_benefits-renewal_benefits$Total_benefits, Delay2 = delay2_benefits$Total_benefits-renewal_benefits$Total_benefits, Delay5 = delay5_benefits$Total_benefits-renewal_benefits$Total_benefits, Delay10 = delay10_benefits$Total_benefits-renewal_benefits$Total_benefits, Delay15 = delay15_benefits$Total_benefits-renewal_benefits$Total_benefits))
graph_data_plot3 <- filter(graph_data_plot3, graph_data_plot3$Year != 0)
graph_data_long_plot3 <- melt(graph_data_plot3, id.vars=c("Year"))

# Plot
cols_points <- c("#004c6d", "black", "#255e7e", "#3d708f", "#5383a1", "#6996b3", "#7faac6")
cols_line <- c("#5383a1", "grey", "#6996b3", "#7faac6", "#94bed9", "#abd2ec", "#c1e7ff")
theme_set(theme_bw())
plot3 <- ggplot(graph_data_long_plot3, aes(x = Year, y = value/1e9, color = variable, shape = variable)) +
  geom_line(lwd = 1.5, aes(col = variable)) +
  geom_point(size = 3, stroke = 0, shape = 21, aes(fill = variable)) +
  scale_fill_manual(values=cols_points) +
  scale_color_manual(values = cols_line) +
  labs(x = "Year") +
  labs(y = "Health benefits [bn. USD]") +
  xlim(c(2022, 2050)) +
  
  scale_x_continuous(breaks=seq(2022,2050,by=4)) +
  scale_y_continuous(breaks = seq(0, 200, 50), 
                     limits = c(-10,200), 
                     expand = c(0,0)) +
  #labs(title = "Fleet electrification \n Reference: fleet renewal") +
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18, margin=margin(r=15)),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black", size = 1),
        axis.line.x = element_line(colour = "black", size = 1),
        axis.ticks.y = element_line(size = 1),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.length=unit(0.1,"inch"),
        #plot.title.position = "plot",
        #plot.title = element_text(size = 15, hjust = 0.5, vjust = -15),
        legend.title = element_blank(),
        legend.position = c(0.35,0.7),
        legend.text = element_text(size = 20),
        legend.spacing.y = unit(0, "inch"),
        legend.key.size = unit(0.4, "inch")) +
  guides(fill = guide_legend(byrow = TRUE))
plot3

# Calculation of the health benefits of a single EV over the years (ZEV scenarios)
fleet_composition <- read.csv(paste0(path, inputs, zev_inter_scenario, "/fleet_composition.csv"))
benefits_per_EV_dataset <- data.frame(unique(fleet_composition$Year[fleet_composition$Year >= 2023])) %>%
  add_column("Number_EVs" = NA) %>%
  add_column("Electrification" = NA) %>%
  add_column("Worst_case" = NA) %>%
  add_column("Best_case" = NA) %>%
  add_column("Low_Case" = NA) %>%
  add_column("Cumulative_Elec" = NA) %>%
  add_column("Cumulative_Worst" = NA) %>%
  add_column("Cumulative_Best" = NA) %>%
  add_column("Cumulative_Low" = NA)
colnames(benefits_per_EV_dataset)[1] <- c("Years")
for (i in 1:dim(benefits_per_EV_dataset)[1]) {
  benefits_per_EV_dataset$Number_EVs[i] <- sum(fleet_composition$Value[which(grepl("BEV", fleet_composition$Technology) & fleet_composition$Year == benefits_per_EV_dataset$Years[i])])
  benefits_per_EV_dataset$Electrification[i] <- (graph_data_plot2$Electrification[which(graph_data_plot2$Year == benefits_per_EV_dataset$Years[i])]-graph_data_plot2$Renewal[which(graph_data_plot2$Year == benefits_per_EV_dataset$Years[i])])/benefits_per_EV_dataset$Number_EVs[i]
  benefits_per_EV_dataset$Worst_case[i] <- (graph_data_plot2$Worst_Case[which(graph_data_plot2$Year == benefits_per_EV_dataset$Years[i])]-graph_data_plot2$Renewal[which(graph_data_plot2$Year == benefits_per_EV_dataset$Years[i])])/benefits_per_EV_dataset$Number_EVs[i]
  benefits_per_EV_dataset$Best_case[i] <- (graph_data_plot2$Best_Case[which(graph_data_plot2$Year == benefits_per_EV_dataset$Years[i])]-graph_data_plot2$Renewal[which(graph_data_plot2$Year == benefits_per_EV_dataset$Years[i])])/benefits_per_EV_dataset$Number_EVs[i]
  benefits_per_EV_dataset$Low_Case[i] <- (graph_data_plot2$Low_Case[which(graph_data_plot2$Year == benefits_per_EV_dataset$Years[i])]-graph_data_plot2$Renewal[which(graph_data_plot2$Year == benefits_per_EV_dataset$Years[i])])/benefits_per_EV_dataset$Number_EVs[i]
  if (i == 1) {
    benefits_per_EV_dataset$Cumulative_Elec[i] <- benefits_per_EV_dataset$Electrification[i]
    benefits_per_EV_dataset$Cumulative_Worst[i] <- benefits_per_EV_dataset$Worst_case[i]
    benefits_per_EV_dataset$Cumulative_Best[i] <- benefits_per_EV_dataset$Best_case[i]
    benefits_per_EV_dataset$Cumulative_Low[i] <- benefits_per_EV_dataset$Low_Case[i]
  } else {
    benefits_per_EV_dataset$Cumulative_Elec[i] <- benefits_per_EV_dataset$Cumulative_Elec[i-1]+benefits_per_EV_dataset$Electrification[i]
    benefits_per_EV_dataset$Cumulative_Worst[i] <- benefits_per_EV_dataset$Cumulative_Worst[i-1]+benefits_per_EV_dataset$Worst_case[i]
    benefits_per_EV_dataset$Cumulative_Best[i] <- benefits_per_EV_dataset$Cumulative_Best[i-1]+benefits_per_EV_dataset$Best_case[i]
    benefits_per_EV_dataset$Cumulative_Low[i] <- benefits_per_EV_dataset$Cumulative_Low[i-1]+benefits_per_EV_dataset$Low_Case[i]
  }
}
#benefits_per_EV_dataset <- select(benefits_per_EV_dataset, c("Years", "Cumulative_Elec", "Cumulative_Worst", "Cumulative_Best", "Cumulative_Low"))
benefits_per_EV_dataset <- select(benefits_per_EV_dataset, c("Years", "Cumulative_Elec", "Cumulative_Best", "Cumulative_Low"))
benefits_per_EV_dataset_long <- melt(benefits_per_EV_dataset, id.vars=c("Years"))
# Plot
cols_points <- c("#004c6d", "#4bb23c", "#940000", "black")
cols_line <- c("#a2cde6", "#b1dca5", "#e7b1a3", "grey")
theme_set(theme_bw())
plot4 <- ggplot(benefits_per_EV_dataset_long, aes(x = Years, y = value, color = variable)) +
  geom_line(lwd = 1.5, aes(col = variable)) +
  geom_point(size = 3, stroke = 0, shape = 21, aes(fill = variable)) +
  scale_fill_manual(values=cols_points) +
  scale_color_manual(values = cols_line) +
  labs(x = "Year") +
  labs(y = "Health impact [bn. USD]") +
  xlim(c(2022, 2050)) +
  
  scale_x_continuous(breaks=seq(2022,2050,by=4)) +
  scale_y_continuous(breaks = seq(-2000, 10000, 2000), 
                     limits = c(-3000,10000), 
                     expand = c(0,0)) +
  #labs(title = "Fleet electrification \n Reference: fleet renewal") +
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18, margin=margin(r=15)),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.y = element_line(colour = "black", size = 1),
        axis.line.x = element_line(colour = "black", size = 1),
        axis.ticks.y = element_line(size = 1),
        axis.ticks.x = element_line(size = 1),
        axis.ticks.length=unit(0.1,"inch"),
        #plot.title.position = "plot",
        #plot.title = element_text(size = 15, hjust = 0.5, vjust = -15),
        legend.title = element_blank(),
        legend.position = c(0.35,0.7),
        legend.text = element_text(size = 20),
        legend.spacing.y = unit(0, "inch"),
        legend.key.size = unit(0.4, "inch")) +
  guides(fill = guide_legend(byrow = TRUE))
plot4
