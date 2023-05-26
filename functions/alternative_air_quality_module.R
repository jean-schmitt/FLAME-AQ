attribute_f("air_quality_module_f")
normalized_total_emissions <- get_input_f(input_name = 'normalized_total_emissions')
fleet_dataset <- fleet_composition_state_breakdown[["fleet_vint_stock_scenario_state_breakdown"]]
fleet_size <- total_fleet_size_f(fleet_dataset)
normalized_emissions_by_state <- read.csv(paste0(getwd(), "/inputs/air_quality/normalized_emissions_by_state.csv"))
total_emissions_fleet <- normalized_total_emissions
print("Modification of brake and tire wear coefficients")
m <- 1
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
#fleet_dataset <- fleet_dataset %>%
#  #add_column("Model_Year" = fleet_dataset$Year - fleet_dataset$Age) %>%
#  relocate("Year", .after = "Age") %>%
#  relocate("Size", .after = "Year") %>%
#  relocate("Size", .after = "Year") %>%
#  relocate("Technology", .after = "Size") %>%
#  relocate("Model_Year", .after = "Technology") %>%
#  relocate("State", .before = "Age")
#names <- colnames(fleet_dataset)
#fleet_dataset  <- aggregate(fleet_dataset$Value, by = list(fleet_dataset$State, fleet_dataset$Age, fleet_dataset$Year, fleet_dataset$Size, fleet_dataset$Technology, fleet_dataset$Model_Year), FUN = sum)
#colnames(fleet_dataset) <- names
# Original code to insert ino a lapply loop
evaluation <- 1:dim(normalized_total_emissions)[1]

normalized_total_emissions <- add_column(normalized_total_emissions, "Vehicles_population" = 0)
normalized_emissions_by_state <- add_column(normalized_emissions_by_state, "Vehicles_population" = 0)
fleet_dataset <- filter(fleet_dataset, fleet_dataset$Year >= 2021)

#normalized_emissions_by_state <- filter(normalized_emissions_by_state, normalized_emissions_by_state$State == 1)
fleet_dataset$Technology <- substr(fleet_dataset$Technology,1,1)
fleet_dataset <- aggregate(Value ~ Age+Year+Size+Technology+State+Model_Year, data = fleet_dataset, FUN = sum)
data <- future_lapply(evaluation, function(i) {
  temp <- which(normalized_emissions_by_state$Year == normalized_total_emissions$Year[i] &
                   normalized_emissions_by_state$Source == normalized_total_emissions$Source[i] &
                   normalized_emissions_by_state$Fuel == normalized_total_emissions$Fuel[i] &
                   normalized_emissions_by_state$ModelYr == normalized_total_emissions$ModelYr[i])
  
  population <- fleet_dataset$Value[Reduce(intersect, list(which(fleet_dataset$Year == normalized_total_emissions$Year[i]),
                                                           which(fleet_dataset$Size == normalized_total_emissions$Source[i]), 
                                                           which(fleet_dataset$Technology == normalized_total_emissions$Fuel[i]), 
                                                           which(fleet_dataset$Model_Year == normalized_total_emissions$ModelYr[i])))]
  return (list(temp = temp, population = population))
})
for (i in 1:dim(normalized_total_emissions)[1]) {
  index <- data[[i]]$temp
  population <- data[[i]]$population
  normalized_emissions_by_state$Vehicles_population[index] <- population
}
fleet_emissions_by_state <- normalized_emissions_by_state
fleet_emissions_by_state[,7:17] <- fleet_emissions_by_state[,7:17]*fleet_emissions_by_state$Vehicles_population/1e6
fleet_emissions_by_state <- select(fleet_emissions_by_state, -c(Source, Fuel, ModelYr, Run, Vehicles_population))
fleet_emissions_by_state <- aggregate(.~Year+State, data = fleet_emissions_by_state, FUN = sum)
fleet_emissions_by_state <- add_column(fleet_emissions_by_state, "Unit" = "tons")
fleet_emissions_national <- aggregate(.~Year+Unit, data = select(fleet_emissions_by_state, -c(State)), FUN = sum)







normalized_emissions_by_state$Vehicles_population[which(normalized_emissions_by_state$Year == normalized_total_emissions$Year[i] &
      normalized_emissions_by_state$Source == normalized_total_emissions$Source[i] &
      normalized_emissions_by_state$Fuel == normalized_total_emissions$Fuel[i] &
      normalized_emissions_by_state$ModelYr == normalized_total_emissions$ModelYr[i])]


# Code using lapply
normalized_emissions_by_state[i] <- future_lapply(evaluation, function(i) {
  population <- fleet_dataset$Value[Reduce(intersect, list(which(fleet_dataset$Year == normalized_emissions_by_state$Year[i]),
                                  which(fleet_dataset$Size == normalized_emissions_by_state$Source[i]), 
                                  which(fleet_dataset$Technology == normalized_emissions_by_state$Fuel[i]), 
                                  which(fleet_dataset$Model_Year == normalized_emissions_by_state$ModelYr[i]),
                                  which(fleet_dataset$State == normalized_emissions_by_state$State[i])))]
  
  
})
  population <- 
  
  population <- fleet_dataset$Value[which(fleet_dataset$Year == normalized_emissions_by_state$Year[i] &
                                                    fleet_dataset$Size == normalized_emissions_by_state$Source[i] & 
                                                    fleet_dataset$Technology == normalized_emissions_by_state$Fuel[i] &
                                                    fleet_dataset$Model_Year == normalized_emissions_by_state$ModelYr[i] &
                                                    fleet_dataset$State == normalized_emissions_by_state$State[i])]
  population
})

# Code using local
for (i in evaluation) {
  normalized_emissions_by_state$Vehicles_population[i] <- local({
    vehicle_population <- fleet_dataset$Value[which(fleet_dataset$Year == normalized_emissions_by_state$Year[i] &
                                                      fleet_dataset$Size == normalized_emissions_by_state$Source[i] & 
                                                      substr(fleet_dataset$Technology,1,1) == normalized_emissions_by_state$Fuel[i] &
                                                      fleet_dataset$Model_Year == normalized_emissions_by_state$ModelYr[i] &
                                                      fleet_dataset$State == normalized_emissions_by_state$State[i])]
    if (length(vehicle_population) == 0) {
      vehicle_population <- 0
    }
    return(vehicle_population)
  })
}

#Original code
for (i in 1:dim(normalized_total_emissions)[1]) {
  temp_emissions <- normalized_emissions_by_state[which(normalized_emissions_by_state$Year == normalized_total_emissions$Year[i] & 
                                                          normalized_emissions_by_state$Source == normalized_total_emissions$Source[i] &
                                                          normalized_emissions_by_state$Fuel == normalized_total_emissions$Fuel[i] &
                                                          normalized_emissions_by_state$ModelYr == normalized_total_emissions$ModelYr[i]),]
  temp_vehicle_population <- fleet_dataset[which(fleet_dataset$Year == normalized_total_emissions$Year[i] &
                                                   fleet_dataset$Size == normalized_total_emissions$Source[i] & 
                                                   substr(fleet_dataset$Technology,1,1) == normalized_total_emissions$Fuel[i] &
                                                   fleet_dataset$Model_Year == normalized_total_emissions$ModelYr[i]),]
  temp_emissions <- arrange(temp_emissions, temp_emissions$State)
  temp_state_emissions <- temp_emissions
  temp_vehicle_population <- arrange(temp_vehicle_population, temp_vehicle_population$State)
  for (j in 8:length(colnames(temp_state_emissions))) {
    temp_state_emissions[,j] <- temp_emissions[,j]*temp_vehicle_population$Value
    total_emissions_fleet[i,j-2] <- normalized_total_emissions[i,j-2]*sum(temp_vehicle_population$Value)
  }
  if (m == 1) {
    total_emissions_fleet_state_breakdown <- temp_state_emissions
  } else {
    total_emissions_fleet_state_breakdown <- rbind(total_emissions_fleet_state_breakdown, temp_state_emissions)
  }
  m <- m+1
}
