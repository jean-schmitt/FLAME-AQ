fuel_wtp_ef_f <- function(fleet_fuel_use_tot, fleet_fuel_use_tot_state, first_proj_yr = NA, share_diesel_ldv_cons = NA, share_gasoline_ldv_cons = NA, share_ldv_crude_oil = NA, share_prod_ethanol_fuel = NA, share_ng_cons_elec = NA, fleet_id = NA, scenario_id = NA) {
  attribute_f("fuel_wtp_ef_f")
  if (fleet_id == "Elec100_Truck100" | fleet_id == "Elec100_Car100") {
    results_path <- paste0(getwd(), "/outputs/air_quality/", Sys.Date(), "_", scenario_id, "_results")
    production_change_matrix <- read.csv(paste0(getwd(), "/inputs/air_quality/oil_and_gas_activity_changes_NoLDVs.csv"), row.names = 1)
  } else {
    results_path <- paste0(getwd(), "/outputs/air_quality/", Sys.Date(), "_", scenario_id, "_results")
    production_change_matrix_reference <- read.csv(paste0(getwd(), "/inputs/air_quality/oil_and_gas_activity_changes_NoLDVs.csv"), row.names = 1)
    GIS_matching_matrix <- read.csv(paste0(getwd(), "/inputs/air_quality/GIS_matching_matrix.csv"))
    production_change_matrix <- data.frame(matrix(data = NA, nrow = 0, ncol = 7))
    colnames(production_change_matrix) <- c("PADD", "Year", "Rel_Demand", "Rel_Refining", "Rel_Crude_oil", "Rel_Ethanol", "Rel_Natural_Gas")
    petroleum_products_statistics <- read.csv(paste0(getwd(), "/inputs/air_quality/oil_statistics_per_PADD.csv"))
    fuel_prod_US <- read.csv(paste0(getwd(), "/inputs/air_quality/fuel_production_US.csv"))
    fuel_prod_US <- add_column(fuel_prod_US, "Year" = 2021)
    fuel_prod_US$Unit <- "L"
    fuel_prod_US$Value <- as.numeric(fuel_prod_US$Value*159*1000)
    fuels <- c("Gasoline", "Diesel", "Ethanol", "Crude_oil")
    padds <- unique(fuel_prod_US$PADD)
    # Aggregation of the data at the PADD level and calculation of the relative fuel consumption
    fleet_fuel_use_tot_padd <- fleet_fuel_use_tot_state %>%
      add_column("PADD" = NA) %>%
      filter(Fuel != "Hydrogen" & Fuel != "CNG & LPG")
    padds <- unique(GIS_matching_matrix$PADD)
    for (i in padds) {
      states <- unique(GIS_matching_matrix$ST_FIPS[which(GIS_matching_matrix$PADD == i)])
      fleet_fuel_use_tot_padd$PADD[which(fleet_fuel_use_tot_padd$State%in%states)] <- i
    }
    fleet_fuel_use_tot_padd <- filter(fleet_fuel_use_tot_padd, !is.na(fleet_fuel_use_tot_padd$PADD))
    fleet_fuel_use_tot_padd <- select(fleet_fuel_use_tot_padd, -c("State"))
    if (dim(fleet_fuel_use_tot_padd)[1] == 0) {
      fleet_fuel_use_tot_padd[nrow(fleet_fuel_use_tot_padd)+1,] <- numeric(dim(fleet_fuel_use_tot_padd)[2])
    }
    fleet_fuel_use_tot_padd <- aggregate(.~ Fuel + Year + Unit + PADD, data = fleet_fuel_use_tot_padd, FUN = sum) %>%
      add_column("Non_LDV_demand" = 0) %>%
      add_column("Total_demand" = 0) %>%
      add_column("Relative_demand_change" = NA)
    # Calculation of the relative change in fuel demand, taking into account the use of fuel by other sectors (e.g., heavy duty vehicles)
    for (i in 1:dim(fleet_fuel_use_tot_padd)[1]) {
      if (fleet_fuel_use_tot_padd$Fuel[i] == "Gasoline") {
        ldv_factor <- share_gasoline_ldv_cons
      } else if (fleet_fuel_use_tot_padd$Fuel[i] == "Diesel") {
        ldv_factor <- share_diesel_ldv_cons
      } else if (fleet_fuel_use_tot_padd$Fuel[i] == "Ethanol") {
        ldv_factor <- share_prod_ethanol_fuel*share_gasoline_ldv_cons
      }
      if (fleet_fuel_use_tot_padd$Year[i] == min(unique(fleet_fuel_use_tot_padd$Year))) {
        fleet_fuel_use_tot_padd$Relative_demand_change[i] <- 1
      } else {
        consumption_non_ldv <- fleet_fuel_use_tot_padd$Value[which(fleet_fuel_use_tot_padd$Fuel == fleet_fuel_use_tot_padd$Fuel[i] &
                                                                     fleet_fuel_use_tot_padd$Year ==  first_proj_yr &
                                                                     fleet_fuel_use_tot_padd$PADD == fleet_fuel_use_tot_padd$PADD[i])]*(1/ldv_factor-1)
        if (fleet_id == "No_LDVs" & fleet_fuel_use_tot_padd$Year[i] > first_proj_yr) {
          fleet_fuel_use_tot_padd$Value[i] <- 0
        }
        fleet_fuel_use_tot_padd$Non_LDV_demand[i] <- consumption_non_ldv
        fleet_fuel_use_tot_padd$Total_demand[i] <- fleet_fuel_use_tot_padd$Value[i]+consumption_non_ldv
        fleet_fuel_use_tot_padd$Relative_demand_change[i] <- fleet_fuel_use_tot_padd$Total_demand[i]/fleet_fuel_use_tot_padd$Total_demand[which(fleet_fuel_use_tot_padd$Fuel == fleet_fuel_use_tot_padd$Fuel[i] &
                                                                                                                                                fleet_fuel_use_tot_padd$Year == fleet_fuel_use_tot_padd$Year[i]-1 &
                                                                                                                                                fleet_fuel_use_tot_padd$PADD == fleet_fuel_use_tot_padd$PADD[i])]
      }
    }
    fleet_fuel_use_tot_padd <- filter(fleet_fuel_use_tot_padd, fleet_fuel_use_tot_padd$Year >= 2021)
    # Synthesis of the flows in and out to calculate the available volume of fuels in each PADD in the year 2021
    for (i in fuels) {
      for (j in padds) {
        flows_in <- sum(fuel_prod_US$Value[which(fuel_prod_US$Fuel == i & fuel_prod_US$PADD == j & fuel_prod_US$Origin != "Exports" & fuel_prod_US$Origin != "Production")])
        flows_out <- sum(fuel_prod_US$Value[which(fuel_prod_US$Fuel == i & ((fuel_prod_US$PADD != j & fuel_prod_US$Origin == j) | (fuel_prod_US$PADD == j & fuel_prod_US$Origin == "Exports")))])
        production <- fuel_prod_US$Value[which(fuel_prod_US$Fuel == i & fuel_prod_US$PADD == j & fuel_prod_US$Origin == "Production")]
        value <- production + flows_in - flows_out
        fuel_prod_US[nrow(fuel_prod_US)+1,] <- list(j, i, "Available", value, "L", 2021)
      }
    }
    # Update of the yearly demand for refined products based on the change in demand for each fuel
    years <- (first_proj_yr+1):max(fleet_fuel_use_tot_padd$Year)
    temp_prod <- fuel_prod_US
    #fuels <- c("Gasoline", "Diesel", "Ethanol")
    fuels <- unique(fleet_fuel_use_tot_padd$Fuel)[which(unique(fleet_fuel_use_tot_padd$Fuel) != "Electricity")]
    for (i in years) {
      temp_prod$Year <- i
      for (j in fuels) {
        for (k in padds) {
          # Calculation of the change in demand
          scaling_factor <- fleet_fuel_use_tot_padd$Relative_demand_change[which(fleet_fuel_use_tot_padd$Year == i & fleet_fuel_use_tot_padd$Fuel == j & fleet_fuel_use_tot_padd$PADD == k)]
          temp <- which(temp_prod$PADD == k & temp_prod$Fuel == j & temp_prod$Origin != "Production")
          temp_prod$Value[temp] <- temp_prod$Value[temp]*scaling_factor
        }
        for (k in padds) {
          available <- temp_prod$Value[which(temp_prod$PADD == k & temp_prod$Fuel == j & temp_prod$Origin == "Available")]
          flows_in <- sum(temp_prod$Value[which(temp_prod$Fuel == j & temp_prod$PADD == k & temp_prod$Origin != "Exports" & temp_prod$Origin != "Available" & temp_prod$Origin != "Production")])
          flows_out <- sum(temp_prod$Value[which(temp_prod$Fuel == j & ((temp_prod$PADD != k & temp_prod$Origin == k) | (temp_prod$PADD == k & temp_prod$Origin == "Exports")))])
          production <- available - flows_in + flows_out
          temp_prod$Value[which(temp_prod$PADD == k & temp_prod$Fuel == j & temp_prod$Origin == "Production")] <- production
        }
      }
      fuel_prod_US <- rbind(fuel_prod_US, temp_prod)
    }
    # Calculate the production change of refining, derived from the change in demand
    for (i in years) {
      for (j in padds) {
        # Fraction of gasoline, diesel, and other produced in the PADD
        fraction_gasoline <- petroleum_products_statistics$Gasoline_production[which(petroleum_products_statistics$PADD == j)]/petroleum_products_statistics$Total_products[which(petroleum_products_statistics$PADD == j)]
        fraction_diesel <- petroleum_products_statistics$Diesel_production[which(petroleum_products_statistics$PADD == j)]/petroleum_products_statistics$Total_products[which(petroleum_products_statistics$PADD == j)]
        fraction_other <- 1-(fraction_gasoline+fraction_diesel)
        # Change in fuel production from one year to the other, based on the change in demand
        change_gasoline <- fuel_prod_US$Value[which(fuel_prod_US$Year == i & fuel_prod_US$PADD == j & fuel_prod_US$Fuel == "Gasoline" & fuel_prod_US$Origin == "Production")]/fuel_prod_US$Value[which(fuel_prod_US$Year == (i-1) & fuel_prod_US$PADD == j & fuel_prod_US$Fuel == "Gasoline" & fuel_prod_US$Origin == "Production")]
        change_diesel <- fuel_prod_US$Value[which(fuel_prod_US$Year == i & fuel_prod_US$PADD == j & fuel_prod_US$Fuel == "Diesel" & fuel_prod_US$Origin == "Production")]/fuel_prod_US$Value[which(fuel_prod_US$Year == (i-1) & fuel_prod_US$PADD == j & fuel_prod_US$Fuel == "Diesel" & fuel_prod_US$Origin == "Production")]
        relative_refining <- change_gasoline*fraction_gasoline+change_diesel*fraction_diesel+fraction_other
        #relative_refining <- change_gasoline
        # Update the production data
        petroleum_products_statistics$Gasoline_production[which(petroleum_products_statistics$PADD == j)] <- petroleum_products_statistics$Gasoline_production[which(petroleum_products_statistics$PADD == j)]*change_gasoline
        petroleum_products_statistics$Diesel_production[which(petroleum_products_statistics$PADD == j)] <- petroleum_products_statistics$Diesel_production[which(petroleum_products_statistics$PADD == j)]*change_diesel
        petroleum_products_statistics$Total_products[which(petroleum_products_statistics$PADD == j)] <- petroleum_products_statistics$Total_products[which(petroleum_products_statistics$PADD == j)]*fraction_other+petroleum_products_statistics$Gasoline_production[which(petroleum_products_statistics$PADD == j)]+petroleum_products_statistics$Diesel_production[which(petroleum_products_statistics$PADD == j)]
        # Copy the data into the final matrix
        production_change_matrix[nrow(production_change_matrix)+1,] <- list(j, i, NA, relative_refining, NA, NA, NA)
      }
    }
    # Update the crude oil data
    ng_demand_electricity <- do.call(cambium_el_grid_scenario_f, list(output = "ng"))[["ng_demand_electricity"]]
    years <- (first_proj_yr+1):max(fleet_fuel_use_tot_padd$Year)
    for (i in years) {
      for (k in padds) {
        # Calculation of the change in demand for crude oil based on the changes in refining activity within the PADD
        scaling_factor <- production_change_matrix$Rel_Refining[which(production_change_matrix$PADD == k & production_change_matrix$Year == i)]
        fuel_prod_US$Value[which(fuel_prod_US$PADD == k & fuel_prod_US$Fuel == "Crude_oil" & fuel_prod_US$Origin != "Production" & fuel_prod_US$Year == i)] <- fuel_prod_US$Value[which(fuel_prod_US$PADD == k & fuel_prod_US$Fuel == "Crude_oil" & fuel_prod_US$Origin != "Production" & fuel_prod_US$Year == i-1)]*scaling_factor
      }
      for (k in padds) {
        non_ldv_demand <- sum(fleet_fuel_use_tot_padd$Non_LDV_demand[which(fleet_fuel_use_tot_padd$Year == i &
                                                                           fleet_fuel_use_tot_padd$PADD == k)])
        available <- fuel_prod_US$Value[which(fuel_prod_US$Year == i & fuel_prod_US$PADD == k & fuel_prod_US$Fuel == "Crude_oil" & fuel_prod_US$Origin == "Available")]
        flows_in <- sum(fuel_prod_US$Value[which(fuel_prod_US$Year == i & fuel_prod_US$Fuel == "Crude_oil" & fuel_prod_US$PADD == k & fuel_prod_US$Origin != "Exports" & fuel_prod_US$Origin != "Available" & fuel_prod_US$Origin != "Production")])
        flows_out <- sum(fuel_prod_US$Value[which(fuel_prod_US$Year == i & fuel_prod_US$Fuel == "Crude_oil" & ((fuel_prod_US$PADD != k & fuel_prod_US$Origin == k) | (fuel_prod_US$PADD == k & fuel_prod_US$Origin == "Exports")))])
        production <- available - flows_in + flows_out
        fuel_prod_US$Value[which(fuel_prod_US$Year == i & fuel_prod_US$PADD == k & fuel_prod_US$Fuel == "Crude_oil" & fuel_prod_US$Origin == "Production")] <- production
        production_change_matrix$Rel_Demand[which(production_change_matrix$PADD == k & production_change_matrix$Year == i)] <- sum(fleet_fuel_use_tot_padd$Total_demand[which(fleet_fuel_use_tot_padd$Year == i & fleet_fuel_use_tot_padd$PADD == k)])/sum(fleet_fuel_use_tot_padd$Total_demand[which(fleet_fuel_use_tot_padd$Year == i-1 & fleet_fuel_use_tot_padd$PADD == k)])
        production_change_matrix$Rel_Crude_oil[which(production_change_matrix$PADD == k & production_change_matrix$Year == i)] <- production/fuel_prod_US$Value[which(fuel_prod_US$Year == i-1 & fuel_prod_US$PADD == k & fuel_prod_US$Fuel == "Crude_oil" & fuel_prod_US$Origin == "Production")]
        if (fleet_id != "Elec0_Truck100" & fleet_id != "Elec0_Car100") {
          production_change_matrix$Rel_Ethanol[which(production_change_matrix$PADD == k & production_change_matrix$Year == i)] <- fleet_fuel_use_tot_padd$Relative_demand_change[which(fleet_fuel_use_tot_padd$Fuel == "Ethanol" & fleet_fuel_use_tot_padd$Year == i & fleet_fuel_use_tot_padd$PADD == k)]
        }
        rel_demand_ng <- ng_demand_electricity$Relative_demand[which(ng_demand_electricity$Year == i)]
        rel_ng <- share_ng_cons_elec*rel_demand_ng+(1-share_ng_cons_elec)
        production_change_matrix$Rel_Natural_Gas[which(production_change_matrix$PADD == k & production_change_matrix$Year == i)] <- rel_ng
      }
    }
    if (fleet_id == "Elec0_Truck100" | fleet_id == "Elec0_Car100") {
      production_change_matrix$Rel_Ethanol <- production_change_matrix_reference$Rel_Ethanol
    }
  }
  write.csv(production_change_matrix, paste0(results_path, "/oil_and_gas_activity_changes.csv"))
  return(list(production_change_matrix = production_change_matrix))
}
