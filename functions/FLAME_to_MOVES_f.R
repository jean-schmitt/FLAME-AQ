FLAME_to_MOVES_f <- function(fleet_composition, scenario_id = NA, fuel_matching_option = NA) {
  attribute_f("FLAME_to_MOVES_f")
  results_path <- paste0(getwd(), "/outputs/air_quality/", Sys.Date(), "_", scenario_id, "_results")
  #fleet_dataset <- read.csv("D:/03_University_of_Toronto/11_Health_Co-Benefits_CO2_Mitigation_Scenarios/03_FLAME-AQ/outputs/air_quality/2023-02-17_ZEV_All_zev_current_grid_results/fleet_composition.csv")
  #fleet_dataset <- select(fleet_dataset, -c("X"))
  fleet_dataset <- fleet_composition[["fleet_vint_stock"]]
  fleet_size <- total_fleet_size_f(fleet_dataset)
  # Adapt the fuel and engine types according to the chosen matching option
  if (fuel_matching_option == "hybrid") {
    correspondence_file <- read.delim(paste0(getwd(), "/inputs/data/FLAME_to_MOVES_correspondance_files/technologies_", fuel_matching_option, ".txt"), header = TRUE, sep = ";", dec = ".")
  } else if (fuel_matching_option == "FLAME") {
    
  } else if (fuel_matching_option == "MOVES") {
    
  }
  for (i in 1:length(correspondence_file$ID)) {
    fleet_dataset$Technology[which(fleet_dataset$Technology == correspondence_file$Technology[i])] <- correspondence_file$ID[i]
  }
  
  # Adapt the naming of vehicles sizes
  vehicles_sizes_file <- read.delim(paste0(getwd(), "/inputs/data/FLAME_to_MOVES_correspondance_files/vehicles_sizes.txt"), header = TRUE, sep = ";", dec = ".")
  for (i in 1:length(vehicles_sizes_file$Sizes)) {
    fleet_dataset$Size[which(fleet_dataset$Size == vehicles_sizes_file$Sizes[i])] <- vehicles_sizes_file$ID[i]
    colnames(fleet_size)[which(colnames(fleet_size) == vehicles_sizes_file$Sizes[i])] <- vehicles_sizes_file$ID[i]
  }
  
  # Adapt the file structure
  fleet_dataset <- fleet_dataset %>%
    add_column("Model_Year" = fleet_dataset$Year - fleet_dataset$Age) %>%
    relocate("Year", .after = "Age") %>%
    relocate("Size", .after = "Year") %>%
    relocate("Size", .after = "Year") %>%
    relocate("Technology", .after = "Size") %>%
    relocate("Model_Year", .after = "Technology")
  names <- colnames(fleet_dataset)
  fleet_dataset  <- aggregate(fleet_dataset$Value, by = list(fleet_dataset$Age, fleet_dataset$Year, fleet_dataset$Size, fleet_dataset$Technology, fleet_dataset$Model_Year), FUN = sum)
  colnames(fleet_dataset) <- names
  
  # Build the three files necessary for the MOVES input database
  # Build the total fleet size dataset and export the file
  fleet_size <- add_column(fleet_size, "yearID" = rownames(fleet_size))
  m <- 0
  for (i in 1:(dim(fleet_size)[2]-2)) {
    m <- m+1
    fleet_size_temp <- data.frame(fleet_size$yearID) %>%
      add_column("sourceTypeID" = colnames(fleet_size)[i]) %>%
      add_column("salesGrowthFactor" = 0) %>%
      add_column("sourceTypePopulation" = fleet_size[,i]) %>%
      add_column("migrationrate" = 1) %>%
      rename("yearID" = "fleet_size.yearID")
    if (m == 1) {
      output <- fleet_size_temp
    } else {
      output <- bind_rows(output, fleet_size_temp)
    }
  }
  fleet_size <- output
  years <- unique(fleet_size$yearID)
  MOVES_min_year <- 1990
  MOVES_max_year <- 2050
  if (min(years)<MOVES_min_year) {
    fleet_size <- filter(fleet_size, fleet_size$yearID>=MOVES_min_year)
  }
  if (max(years)<MOVES_max_year) {
    for (i in 1:(MOVES_max_year-max(years))) {
      fleet_size[nrow(fleet_size)+1,] <- fleet_size[nrow(fleet_size),]
      fleet_size$yearID[nrow(fleet_size)] <- max(years)+i 
    }
  }
  #write.csv(fleet_size, paste0(getwd(), "/outputs/moves_files/sourceTypeYear_",scenario_id,".csv"))
# Total fleet size can be imported into MOVES
  
  # Build the age distribution file
  output_age_distribution <- fleet_dataset %>%
    select(c("Age", "Year", "Size", "Value")) %>%
    add_column("ageFraction" = 0) %>%
    rename("ageID" = "Age") %>%
    rename("yearID" = "Year") %>%
    rename("sourceTypeID" = "Size") %>%
    relocate("ageID", .after = "yearID") %>%
    relocate("sourceTypeID", .before = "yearID")
  output_age_distribution <- output_age_distribution %>%
    add_column("temp" = paste0(output_age_distribution$sourceTypeID, output_age_distribution$yearID, output_age_distribution$ageID)) %>%
    group_by(temp) %>%
    summarise(Total = sum(Value), across()) %>%
    ungroup() %>%
    select(-Value) %>%
    rename("Value" = "Total") %>%
    relocate()
  output_age_distribution <- output_age_distribution[!duplicated(output_age_distribution),]
  years <- unique(output_age_distribution$yearID)
  sizes <- unique(output_age_distribution$sourceTypeID)
  for (i in 1:length(years)) {
    for (j in 1:length(sizes)) {
      temp <- which(output_age_distribution$yearID == years[i] & output_age_distribution$sourceTypeID == sizes[j])
      sum_year <- sum(output_age_distribution$Value[temp])
      output_age_distribution$ageFraction[temp] <- output_age_distribution$Value[temp]/sum_year
      if (sum(output_age_distribution$ageFraction[temp]) !=1) {
        output_age_distribution$ageFraction[temp] <- output_age_distribution$ageFraction[temp]/sum(output_age_distribution$ageFraction[temp])
      }
    }
  }
  output_age_distribution <- select(output_age_distribution, -c("Value"))
  if (min(years)<MOVES_min_year) {
    output_age_distribution <- filter(output_age_distribution, output_age_distribution$yearID>=MOVES_min_year)
  }
  if (max(years)<MOVES_max_year) {
    for (i in 1:(MOVES_max_year-max(years))) {
      temp <- output_age_distribution[which(output_age_distribution$yearID == max(years)),]
      temp$yearID <- max(years)+1
      output_age_distribution <- bind_rows(output_age_distribution, temp)
    }
  }
  output_age_distribution <- select(output_age_distribution, -temp)
  #write.csv(output_age_distribution, paste0(getwd(), "/outputs/moves_files/sourcetypeagedistribution_",scenario_id,".csv"))
  
  # Build the engine tech distribution file
  output_enginetech_distribution <- fleet_dataset %>%
    select(c("Size", "Technology", "Model_Year", "Value")) %>%
    rename("sourceTypeID" = "Size") %>%
    rename("modelYearID" = "Model_Year") %>%
    rename("fuelTypeID" = "Technology") %>%
    add_column("sourceTypeModelYearID" = 0) %>%
    add_column("engTechID" = 0) %>%
    add_column("regClassID" = 0) %>%
    add_column("stmyFuelEngFraction" = 0) %>%
    add_column("stmyFraction" = 0) %>%
    relocate("sourceTypeModelYearID", .before = "sourceTypeID") %>%
    relocate("modelYearID", .before = "fuelTypeID") %>%
    relocate("Value", .after = "stmyFraction")
  output_enginetech_distribution$sourceTypeModelYearID <- paste0(output_enginetech_distribution$sourceTypeID, output_enginetech_distribution$modelYearID)
  temp <- t(data.frame(str_split(output_enginetech_distribution$fuelTypeID, "", 2)))
  output_enginetech_distribution$fuelTypeID <- temp[,1]
  output_enginetech_distribution$engTechID <- temp[,2]
  output_enginetech_distribution <- add_column(output_enginetech_distribution, "temp" = paste0(output_enginetech_distribution$sourceTypeModelYearID, output_enginetech_distribution$fuelTypeID, output_enginetech_distribution$engTechID))
  #output_enginetech_distribution <- aggregate(output_enginetech_distribution, by = list(output_enginetech_distribution$sourceTypeModelYearID, output_enginetech_distribution$sourceTypeID, output_enginetech_distribution$modelYearID, output_enginetech_distribution$fuelTypeID, output_enginetech_distribution$engTechID, output_enginetech_distribution$regClassID, output_enginetech_distribution$stmyFuelEngFraction, output_enginetech_distribution$stmyFraction, output_enginetech_distribution$temp), FUN = mean)
  output_enginetech_distribution <- output_enginetech_distribution %>%
    group_by(temp) %>%
    summarise(mean = mean(Value), across()) %>%
    ungroup() %>%
    select(-Value)
  output_enginetech_distribution <- output_enginetech_distribution[!duplicated(output_enginetech_distribution),]
  #filter(output_enginetech_distribution, !duplicated(output_enginetech_distribution))
  years <- unique(output_enginetech_distribution$modelYearID)
  sizes <- unique(output_enginetech_distribution$sourceTypeID)
  for (i in 1:length(years)) {
    for (j in 1:length(sizes)) {
      temp <- which(output_enginetech_distribution$modelYearID == years[i] & output_enginetech_distribution$sourceTypeID == sizes[j])
      number_vehicles <- sum(output_enginetech_distribution$mean[temp])
      if (number_vehicles == 0) {
        output_enginetech_distribution$stmyFraction[temp] <- 0
      } else {
        output_enginetech_distribution$stmyFraction[temp] <- output_enginetech_distribution$mean[temp]/number_vehicles
      }
    }
  }
  output_enginetech_distribution$stmyFuelEngFraction <- output_enginetech_distribution$stmyFraction
  output_enginetech_distribution <- select(output_enginetech_distribution, -c("temp", "mean"))
  avft <- output_enginetech_distribution %>%
    select(-c(sourceTypeModelYearID, regClassID, stmyFuelEngFraction)) %>%
    rename("fuelEngFraction" = "stmyFraction")
  #write.csv(avft, paste0(getwd(), "/outputs/moves_files/AVFT_",scenario_id,".csv"))
  dir.create(paste0(results_path, "/MOVES_fleet_files"))
  write.csv(fleet_size, paste0(results_path, "/MOVES_fleet_files/sourceTypeYear.csv"))
  write.csv(output_age_distribution, paste0(results_path, "/MOVES_fleet_files/sourcetypeagedistribution.csv"))
  write.csv(avft, paste0(results_path, "/MOVES_fleet_files/AVFT.csv"))
  return(list(sourceTypeYear = fleet_size, sourcetypeagedistribution = output_age_distribution, avft = avft))
}
