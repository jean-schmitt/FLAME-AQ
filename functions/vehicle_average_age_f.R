vehicle_average_age_f <- function(fleet, first_proj_yr = NA) {
  fleet_vint_scrap <- fleet$get_list_dataframe()[["fleet_vint_scrap"]]
  fleet_vint_stock <- fleet$get_list_dataframe()[["fleet_vint_stock"]]
  fleet_vint_stock <- add_column(fleet_vint_stock, "Model_Year" = NA)
  fleet_vint_stock$Model_Year <- fleet_vint_stock$Year-fleet_vint_stock$Age
  fleet_vint_stock <- filter(fleet_vint_stock, fleet_vint_stock$Year >= 2010 & (fleet_vint_stock$Technology == "BEV100" | fleet_vint_stock$Technology == "BEV300"))
  fleet_data <- filter(fleet_vint_scrap, fleet_vint_scrap$Year >= 2010 & (fleet_vint_scrap$Technology == "BEV100" | fleet_vint_scrap$Technology == "BEV300")) %>%
    add_column("Product" = NA) %>%
    add_column("Model_Year" = NA)
  fleet_data$Product <- fleet_data$Value*fleet_data$Age
  fleet_data$Model_Year <- fleet_data$Year - fleet_data$Age
  vehicle_age_matrix <- data.frame(Model_Year = NA, Average_Age = NA)
  model_years <- unique(fleet_data$Model_Year)
  for (i in model_years) {
    new_vehicles <- sum(fleet_vint_stock$Value[which(fleet_vint_stock$Model_Year == i & fleet_vint_stock$Age == 0)])
    vehicles <- which(fleet_data$Model_Year == i)
    if (new_vehicles != 0) {
      average_age <- sum(fleet_data$Product[vehicles])/new_vehicles
    } else {
      average_age <- NA
    }
    newline <- list(i, average_age)
    vehicle_age_matrix <- rbind(vehicle_age_matrix, newline)
  }
}
