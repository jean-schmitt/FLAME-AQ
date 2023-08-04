electricity_emission_factors_NEI_f <- function() {
  NEI_emissions_inventory <- get_input_f(input_name = 'NEI_plant_emissions')
  NEI_emissions_inventory <- NEI_emissions_inventory[order(-NEI_emissions_inventory$Generation),]
  GIS_matching_matrix <- read.csv(paste0(getwd(), "/inputs/air_quality/GIS_matching_matrix.csv"))
  Plant_database <- get_input_f(input_name = 'EIA_860_Plant_Database')
  fossil_technologies <- c("Petroleum Liquids", "Natural Gas Steam Turbine", "Conventional Steam Coal", "Natural Gas Fired Combined Cycle", "Natural Gas Fired Combustion Turbine", "Natural Gas Internal Combustion Engine",
                           "Coal Integrated Gasification Combined Cycle", "Other Gases", "Petroleum Coke", "Landfill Gas", "Natural Gas with Compressed Air Storage", "Other Natural Gas")
  Plant_database <- filter(Plant_database, Plant_database$Technology%in%fossil_technologies)
  NEI_emissions_inventory <- NEI_emissions_inventory %>%
    select(c("Generation", "Heat Input", "ORSPL", "State", "County", "PM2.5", "VOCs", "NH3")) %>%
    add_column("FIPS" = NA) %>%
    add_column("eGRID_Region" = NA) %>%
    add_column("Fuel" = NA)
  for (i in 1:dim(NEI_emissions_inventory)[1]) {
    NEI_emissions_inventory$County[i] <- str_split(NEI_emissions_inventory$County, " County")[[i]][1]
    NEI_emissions_inventory$County[i] <- str_split(NEI_emissions_inventory$County, " Parish")[[i]][1]
    NEI_emissions_inventory$FIPS[i] <- GIS_matching_matrix$X5_digit_FIPS[which(tolower(GIS_matching_matrix$CY_NAME) == tolower(NEI_emissions_inventory$County[i]) & GIS_matching_matrix$States_Abbreviation == NEI_emissions_inventory$State[i])]
    NEI_emissions_inventory$eGRID_Region[i] <- GIS_matching_matrix$eGRID_subregion[which(tolower(GIS_matching_matrix$CY_NAME) == tolower(NEI_emissions_inventory$County[i]) & GIS_matching_matrix$States_Abbreviation == NEI_emissions_inventory$State[i])]
    if (length(which(Plant_database$`Plant Code` == NEI_emissions_inventory$ORSPL[i])) != 0) {
      NEI_emissions_inventory$Fuel[i] <- Plant_database$Technology[which(Plant_database$`Plant Code` == NEI_emissions_inventory$ORSPL[i])[1]]
    }
    if (grepl("Petroleum", NEI_emissions_inventory$Fuel[i])) {
      NEI_emissions_inventory$Fuel[i] <- "oil"
    } else if (grepl("Gas", NEI_emissions_inventory$Fuel[i])) {
      NEI_emissions_inventory$Fuel[i] <- "ng"
    } else if (grepl("Coal", NEI_emissions_inventory$Fuel[i])) {
      NEI_emissions_inventory$Fuel[i] <- "coal"
    }
  }
  NEI_emissions_inventory <- filter(NEI_emissions_inventory, !is.na(NEI_emissions_inventory$Fuel) & NEI_emissions_inventory$Generation != 0)
  NEI_emissions_inventory <- add_column(NEI_emissions_inventory, "Unit" = "lb/MMBtu")
  NEI_emissions_inventory$PM2.5 <- NEI_emissions_inventory$PM2.5*453.592
  NEI_emissions_inventory$VOCs <- NEI_emissions_inventory$VOCs*453.592
  NEI_emissions_inventory$NH3 <- NEI_emissions_inventory$NH3*453.592
  NEI_emissions_inventory$Unit <- "g/MMBtu"
  for (i in 1:dim(NEI_emissions_inventory)[1]) {
    NEI_emissions_inventory$PM2.5[i] <- NEI_emissions_inventory$PM2.5[i]*NEI_emissions_inventory$`Heat Input`[i]#*3.41214
    NEI_emissions_inventory$VOCs[i] <- NEI_emissions_inventory$VOCs[i]*NEI_emissions_inventory$`Heat Input`[i]#*3.41214
    NEI_emissions_inventory$NH3[i] <- NEI_emissions_inventory$NH3[i]*NEI_emissions_inventory$`Heat Input`[i]#*3.41214
  }
  #NEI_emissions_inventory$PM2.5 <- NEI_emissions_inventory$PM2.5*NEI_emissions_inventory$Generation
  #NEI_emissions_inventory$VOCs <- NEI_emissions_inventory$VOCs*NEI_emissions_inventory$Generation
  #NEI_emissions_inventory$NH3 <- NEI_emissions_inventory$NH3*NEI_emissions_inventory$Generation
  NEI_emissions_inventory$Unit <- "g"
  NEI_emissions_inventory <- select(NEI_emissions_inventory, c("Generation", "PM2.5", "VOCs", "NH3", "eGRID_Region", "Fuel", "Unit"))
  NEI_emissions_inventory <- aggregate(.~ eGRID_Region + Fuel + Unit, data = NEI_emissions_inventory, FUN = sum, na.rm = TRUE)
  NEI_emissions_inventory$PM2.5 <- NEI_emissions_inventory$PM2.5/(NEI_emissions_inventory$Generation*1e3)
  NEI_emissions_inventory$VOCs <- NEI_emissions_inventory$VOCs/(NEI_emissions_inventory$Generation*1e3)
  NEI_emissions_inventory$NH3 <- NEI_emissions_inventory$NH3/(NEI_emissions_inventory$Generation*1e3)
  NEI_emissions_inventory$Unit <- "g/kWh"
  NEI_emissions_inventory$PM2.5 <- NEI_emissions_inventory$PM2.5*1000/453.592
  NEI_emissions_inventory$VOCs <- NEI_emissions_inventory$VOCs*1000/453.592
  NEI_emissions_inventory$NH3 <- NEI_emissions_inventory$NH3*1000/453.592
  NEI_emissions_inventory$Unit <- "lb/MWh"
  NEI_emissions_inventory <- select(NEI_emissions_inventory, -c("Generation"))
  NEI_emissions_inventory[nrow(NEI_emissions_inventory)+1,] <- data.frame("Average", "ng", "lb/MWh", mean(NEI_emissions_inventory$PM2.5[which(NEI_emissions_inventory$Fuel == "ng")], na.rm = TRUE), mean(NEI_emissions_inventory$VOCs[which(NEI_emissions_inventory$Fuel == "ng")], na.rm = TRUE), mean(NEI_emissions_inventory$NH3[which(NEI_emissions_inventory$Fuel == "ng")], na.rm = TRUE))
  NEI_emissions_inventory[nrow(NEI_emissions_inventory)+1,] <- data.frame("Average", "oil", "lb/MWh", mean(NEI_emissions_inventory$PM2.5[which(NEI_emissions_inventory$Fuel == "oil")], na.rm = TRUE), mean(NEI_emissions_inventory$VOCs[which(NEI_emissions_inventory$Fuel == "oil")], na.rm = TRUE), mean(NEI_emissions_inventory$NH3[which(NEI_emissions_inventory$Fuel == "oil")], na.rm = TRUE))
  NEI_emissions_inventory[nrow(NEI_emissions_inventory)+1,] <- data.frame("Average", "coal", "lb/MWh", mean(NEI_emissions_inventory$PM2.5[which(NEI_emissions_inventory$Fuel == "coal")], na.rm = TRUE), mean(NEI_emissions_inventory$VOCs[which(NEI_emissions_inventory$Fuel == "coal")], na.rm = TRUE), mean(NEI_emissions_inventory$NH3[which(NEI_emissions_inventory$Fuel == "coal")], na.rm = TRUE))
}
