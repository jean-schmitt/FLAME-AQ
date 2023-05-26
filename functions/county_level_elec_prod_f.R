# Calculates the electricity production at the county level based on information from the Energy Information Administration.
# The output table contains the fraction of the total electricity production in the GEA area taking place in each county
county_level_elec_prod_f <- function() {
  GIS_matching_matrix <- read.csv(paste0(getwd(), "/inputs/air_quality/GIS_matching_matrix.csv"))
  plant_data_EIA <- get_input_f(input_name = "EIA_860_Plant_Database")
  plant_data_EIA <- filter(plant_data_EIA, plant_data_EIA$State != "AK" & plant_data_EIA$State != "HI")
  plant_production_data_EIA <- get_input_f(input_name = "EIA_923_Plant_Level_Production")
  plant_data_NEI <- get_input_f(input_name = "NEI_plant_elec_inventory")
  plant_data_eGRID <- get_input_f(input_name = "eGRID_plant_elec_inventory")
  plant_data_eGRID <- filter(plant_data_eGRID, plant_data_eGRID$`Plant state abbreviation` != "AK" & plant_data_eGRID$`Plant state abbreviation` != "HI" & plant_data_eGRID$`Plant state abbreviation` != "PR")
  plant_data <- data.frame(unique(c(plant_data_EIA$`Plant Code`, plant_data_NEI$ORSPL, plant_data_eGRID$`DOE/EIA ORIS plant or facility code`)))
  colnames(plant_data)[1] <- c("Plant_ID")
  plant_data <- plant_data %>%
    add_column("FIPS" = NA) %>%
    add_column("Generation" = NA) %>%
    add_column("GEA_region" = NA)
  plant_data_EIA$County <- tolower(gsub("\\-|'| ","",plant_data_EIA$County))
  plant_data_eGRID$`Plant county name` <- tolower(gsub("\\-|'| ","",plant_data_eGRID$`Plant county name`))
  for (i in 1:dim(plant_data_NEI)[1]) {
    if (length(str_split(plant_data_NEI$County[i], " ")[[1]]) == 2) {
      plant_data_NEI$County[i] <- str_split(plant_data_NEI$County[i], " ")[[1]][1]
    } else if (length(str_split(plant_data_NEI$County[i], " ")[[1]]) == 3) {
      plant_data_NEI$County[i] <- paste0(str_split(plant_data_NEI$County[i], " ")[[1]][1], " ", str_split(plant_data_NEI$County[i], " ")[[1]][2]) 
    }
  }
  plant_data_NEI$County <- tolower(gsub("\\-|'| ","",plant_data_NEI$County))
  GIS_matching_matrix$CY_NAME <- tolower(gsub("\\-|'| ","",GIS_matching_matrix$CY_NAME))
  plant_data_eGRID$`Plant county name`[which(plant_data_eGRID$`Plant county name` == "virginiabeach")] <- "virginiabeachcity"
  for (i in 1:dim(plant_data)[1]) {
    # Search for the plant in the EIA file
     if (length(which(plant_data_eGRID$`DOE/EIA ORIS plant or facility code` == plant_data$Plant_ID[i])) != 0) {
      # Search for the plant in the eGRID file
      state <- unique(plant_data_eGRID$`Plant state abbreviation`[which(plant_data_eGRID$`DOE/EIA ORIS plant or facility code` == plant_data$Plant_ID[i])])
      county <- unique(plant_data_eGRID$`Plant county name`[which(plant_data_eGRID$`DOE/EIA ORIS plant or facility code` == plant_data$Plant_ID[i])])
      generation <- sum(plant_data_eGRID$`Plant annual net generation (MWh)`[which(plant_data_eGRID$`DOE/EIA ORIS plant or facility code` == plant_data$Plant_ID[i])])
     } else if (length(which(plant_data_EIA$`Plant Code` == plant_data$Plant_ID[i])) != 0) {
       state <- unique(plant_data_EIA$State[which(plant_data_EIA$`Plant Code` == plant_data$Plant_ID[i])])
       county <- unique(plant_data_EIA$County[which(plant_data_EIA$`Plant Code` == plant_data$Plant_ID[i])])
       generation <- sum(plant_production_data_EIA$Yearly_generation[which(plant_production_data_EIA$`Plant Id` == plant_data$Plant_ID[i])])
     } else if (length(which(plant_data_NEI$ORSPL == plant_data$Plant_ID[i])) != 0) {
       # Search for the plant in the NEI file
       state <- unique(plant_data_NEI$State[which(plant_data_NEI$ORSPL == plant_data$Plant_ID[i])])
       county <- unique(str_split(plant_data_NEI$County[which(plant_data_NEI$ORSPL == plant_data$Plant_ID[i])], " ")[[1]][1])
       generation <- sum(plant_data_NEI$Generation[which(plant_data_NEI$ORSPL == plant_data$Plant_ID[i])])
     }
    if (county == "notinfile" | is.na(county) | length(county) == 0 | county == "") {
      plant_data$FIPS[i] <- 0
      plant_data$GEA_region[i] <- 0
      plant_data$Generation[i] <- 0
    } else if (length(which(GIS_matching_matrix$States_Abbreviation == state & tolower(GIS_matching_matrix$CY_NAME) == tolower(county))) != 0) {
      FIPS <- GIS_matching_matrix$FIPS[which(GIS_matching_matrix$States_Abbreviation == state & tolower(GIS_matching_matrix$CY_NAME) == tolower(county))]
      plant_data$GEA_region[i] <- GIS_matching_matrix$CAMBIUM_GEA[which(GIS_matching_matrix$States_Abbreviation == state & tolower(GIS_matching_matrix$CY_NAME) == tolower(county))]
      plant_data$FIPS[i] <- FIPS
      plant_data$Generation[i] <- generation
    }
  }
  plant_data <- filter(plant_data, plant_data$FIPS != 0 & !is.na(plant_data$FIPS))
  plant_data$Generation[which(plant_data$Generation < 0)] <- 0
  plant_data <- select(plant_data, -c("Plant_ID"))
  plant_data <- aggregate(. ~ FIPS + GEA_region, data = plant_data, FUN = sum)
  plant_data <- add_column(plant_data, "Fraction" = NA)
  for (i in 1:dim(plant_data)[1]) {
    plant_data$Fraction[i] <- plant_data$Generation[i]/sum(plant_data$Generation[which(plant_data$GEA_region == plant_data$GEA_region[i])])
  }
  write.csv(plant_data, paste0(getwd(), "/inputs/air_quality/electricity_production_county.csv"))
}
