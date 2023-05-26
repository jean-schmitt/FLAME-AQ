FLAME_format_conversion_f <- function(fleet_dataset) {
  fleet_dataset <- fleet_dataset %>%
    rename("Fuel_Type_EF" = "Technology") %>%
    add_column("Model_Year" = fleet_dataset$Year - fleet_dataset$Age) %>%
    relocate("Year", .after = "Age") %>%
    relocate("Size", .after = "Year") %>%
    relocate("Size", .after = "Year") %>%
    relocate("Fuel_Type_EF", .after = "Size") %>%
    relocate("Model_Year", .after = "Fuel_Type_EF")
  names <- colnames(fleet_dataset)
  fleet_dataset  <- aggregate(fleet_dataset$Value, by = list(fleet_dataset$Age, fleet_dataset$Year, fleet_dataset$Size, fleet_dataset$Fuel_Type_EF, fleet_dataset$Model_Year), FUN = sum)
  colnames(fleet_dataset) <- names
  return(fleet_dataset)
}
