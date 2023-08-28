fuel_wtp_ef_f <- function(fleet_fuel_use_tot, fleet_fuel_use_tot_state, first_proj_yr = NA, share_diesel_ldv_cons = NA, share_gasoline_ldv_cons = NA, share_ldv_crude_oil = NA, share_prod_ethanol_fuel = NA, share_ng_cons_elec = NA, fleet_id = NA, scenario_id = NA) {
  attribute_f("fuel_wtp_ef_f")
  results_path <- paste0(getwd(), "/outputs/air_quality/", Sys.Date(), "_", scenario_id, "_results")
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
      fleet_fuel_use_tot_padd$Relative_demand_change[i] <- (fleet_fuel_use_tot_padd$Value[i]+consumption_non_ldv)/(fleet_fuel_use_tot_padd$Value[which(fleet_fuel_use_tot_padd$Fuel == fleet_fuel_use_tot_padd$Fuel[i] &
                                                                                                                                                         fleet_fuel_use_tot_padd$Year == fleet_fuel_use_tot_padd$Year[i]-1 &
                                                                                                                                                         fleet_fuel_use_tot_padd$PADD == fleet_fuel_use_tot_padd$PADD[i])]+consumption_non_ldv)
    }
  }
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
  fuels <- c("Gasoline", "Diesel", "Ethanol")
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
      change_gasoline <- fuel_prod_US$Value[which(fuel_prod_US$Year == i & fuel_prod_US$PADD == j & fuel_prod_US$Fuel == "Gasoline" & fuel_prod_US$Origin == "Production")]/fuel_prod_US$Value[which(fuel_prod_US$Year == i-1 & fuel_prod_US$PADD == j & fuel_prod_US$Fuel == "Gasoline" & fuel_prod_US$Origin == "Production")]
      change_diesel <- fuel_prod_US$Value[which(fuel_prod_US$Year == i & fuel_prod_US$PADD == j & fuel_prod_US$Fuel == "Diesel" & fuel_prod_US$Origin == "Production")]/fuel_prod_US$Value[which(fuel_prod_US$Year == i-1 & fuel_prod_US$PADD == j & fuel_prod_US$Fuel == "Diesel" & fuel_prod_US$Origin == "Production")]
      relative_refining <- change_gasoline*fraction_gasoline+change_diesel*fraction_diesel+fraction_other
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
      production_change_matrix$Rel_Ethanol[which(production_change_matrix$PADD == k & production_change_matrix$Year == i)] <- fleet_fuel_use_tot_padd$Relative_demand_change[which(fleet_fuel_use_tot_padd$Fuel == "Ethanol" & fleet_fuel_use_tot_padd$Year == i & fleet_fuel_use_tot_padd$PADD == k)]
      rel_demand_ng <- ng_demand_electricity$Relative_demand[which(ng_demand_electricity$Year == i)]
      rel_ng <- share_ng_cons_elec*rel_demand_ng+(1-share_ng_cons_elec)
      production_change_matrix$Rel_Natural_Gas[which(production_change_matrix$PADD == k & production_change_matrix$Year == i)] <- rel_ng
    }
  }
  write.csv(production_change_matrix, paste0(results_path, "/oil_and_gas_activity_changes.csv"))
  return(list(production_change_matrix = production_change_matrix))
  
  # Refined products (Diesel, Gasoline, Ethanol)
  # OK Step 1: calculate relative change in demand
  # OK Step 2: Calculate the amount of each fuel available in each PADD (production - flows_out + flows_in)
  # OK Step 3: Scale the amount available in the PADD
  # OK Step 4: Scale the flows IN and OUT by the same amount
  # OK Step 5: Apply to each PADD
  # OK Step 6: Recalculate the production in each PADD
  # OK Step 7: Once done for each PADD, scale the total refining activity depending on the relative production of refined products
  # OK Step 8: Calculate the absolute change in refining considering PADD-level data
  # OK Step 9: Repeat previous steps focusing only on crude oil data 
  # OK Step 10: Scale the production of crude oil accordingly
  # Step 11: Add data for ethanol production
  # Step 12: Add data for NG production
  
  

  
  
  parameter <- 0
  if (parameter == 1) {
    years <- unique(fleet_fuel_use_tot_padd$Year)[which(unique(fleet_fuel_use_tot_padd$Year) >= first_proj_yr)]
    padds <- unique(fleet_fuel_use_tot_padd$PADD)
    fuels <- c("Gasoline", "Diesel")
    
    for (j in padds) {
      for (i in years) {
        if (i <= first_proj_yr) {
          diesel_ldv_cons <- share_diesel_ldv_cons
          gasoline_ldv_cons <- share_gasoline_ldv_cons
          ldv_crude_oil <- share_ldv_crude_oil
          prod_ethanol_fuel <- share_prod_ethanol_fuel
          ng_cons_ldv <- share_ng_cons_ldv
        } else {
          diesel_ldv_cons <- diesel_cons_change*diesel_ldv_cons/(diesel_cons_change*diesel_ldv_cons+(1-share_diesel_ldv_cons))
          gasoline_ldv_cons <- gasoline_cons_change*gasoline_ldv_cons/(gasoline_cons_change*gasoline_ldv_cons+(1-share_gasoline_ldv_cons))
          ldv_crude_oil <- rel_crude_oil*ldv_crude_oil/(rel_crude_oil*ldv_crude_oil+(1-ldv_crude_oil))
          prod_ethanol_fuel <- rel_ethanol*prod_ethanol_fuel/(rel_ethanol*prod_ethanol_fuel+(1-prod_ethanol_fuel))
          ng_cons_ldv <- rel_ng*ng_cons_ldv/(rel_ng*ng_cons_ldv+(1-ng_cons_ldv))
        }
        demand_year <- sum(fleet_fuel_use_tot_padd$Value[which(fleet_fuel_use_tot_padd$Fuel%in%fuels & fleet_fuel_use_tot_padd$Year == i & fleet_fuel_use_tot_padd$PADD == j)])
        demand_previous_year <- sum(fleet_fuel_use_tot_padd$Value[which(fleet_fuel_use_tot_padd$Fuel%in%fuels & fleet_fuel_use_tot_padd$Year == i-1 & fleet_fuel_use_tot_padd$PADD == j)])
        rel_demand <- demand_year/demand_previous_year
        diesel_cons_change <- fleet_fuel_use_tot_padd$Relative_demand_change[which(fleet_fuel_use_tot_padd$Year == i & fleet_fuel_use_tot_padd$PADD == j & fleet_fuel_use_tot_padd$Fuel == "Diesel")]
        gasoline_cons_change <- fleet_fuel_use_tot_padd$Relative_demand_change[which(fleet_fuel_use_tot_padd$Year == i & fleet_fuel_use_tot_padd$PADD == j & fleet_fuel_use_tot_padd$Fuel == "Gasoline")]
        share_diesel_prod <- petroleum_products_statistics$Diesel_production[which(petroleum_products_statistics$PADD == j)]/petroleum_products_statistics$Total_products[which(petroleum_products_statistics$PADD == j)]
        share_gasoline_prod <- petroleum_products_statistics$Gasoline_production[which(petroleum_products_statistics$PADD == j)]/petroleum_products_statistics$Total_products[which(petroleum_products_statistics$PADD == j)]
        rel_refining_diesel <- (diesel_cons_change*diesel_ldv_cons+(1-diesel_ldv_cons))*share_diesel_prod
        rel_refining_gasoline <- (gasoline_cons_change*gasoline_ldv_cons+(1-gasoline_ldv_cons))*share_gasoline_prod
        rel_refining_other <- 1-(share_diesel_prod+share_gasoline_prod)
        rel_refining <- rel_refining_diesel+rel_refining_gasoline+rel_refining_other
        ldv_crude_cons <- ldv_crude_oil*rel_refining
        other_crude_cons <- 1-ldv_crude_oil
        rel_crude_oil <- ldv_crude_cons+other_crude_cons
        rel_demand_ethanol <- fleet_fuel_use_tot_padd$Relative_demand_change[which(fleet_fuel_use_tot_padd$Fuel == "Ethanol" & fleet_fuel_use_tot_padd$Year == i & fleet_fuel_use_tot_padd$PADD == j)]
        rel_ethanol <- prod_ethanol_fuel*rel_demand_ethanol+(1-share_prod_ethanol_fuel)
        rel_demand_ng <- ng_demand_electricity$Relative_demand[which(ng_demand_electricity$Year == i)]
        rel_ng <- ng_cons_ldv*rel_demand_ng+(1-ng_cons_ldv)
        production_change_matrix[nrow(production_change_matrix)+1,] <- c(j, i, rel_demand, rel_refining, rel_crude_oil, rel_ethanol, rel_ng)
      }
    }
    return(list(production_change_matrix = production_change_matrix))
    

    GIS_matching_matrix <- read.csv(paste0(getwd(), "/inputs/air_quality/GIS_matching_matrix.csv"))
    # Calculate the fleet's consumption by PADD
    fleet_fuel_use_tot_padd <- fleet_fuel_use_tot_state %>%
      add_column("PADD" = NA) %>%
      filter(Fuel != "Hydrogen")
    padds <- unique(GIS_matching_matrix$PADD)
    for (i in padds) {
      states <- unique(GIS_matching_matrix$ST_FIPS[which(GIS_matching_matrix$PADD == i)])
      fleet_fuel_use_tot_padd$PADD[which(fleet_fuel_use_tot_padd$State%in%states)] <- i
    }
    fleet_fuel_use_tot_padd <- filter(fleet_fuel_use_tot_padd, !is.na(fleet_fuel_use_tot_padd$PADD))
    fleet_fuel_use_tot_padd <- select(fleet_fuel_use_tot_padd, -c("State"))
    fleet_fuel_use_tot_padd <- aggregate(.~ Fuel + Year + Unit + PADD, data = fleet_fuel_use_tot_padd, FUN = sum) %>%
      add_column("Origin_1" = NA) %>%
      add_column("Origin_2" = NA) %>%
      add_column("Origin_3" = NA) %>%
      add_column("Origin_4" = NA) %>%
      add_column("Origin_5" = NA) %>%
      add_column("Imports" = NA) %>%
      add_column("Exports" = NA)
    # Create the dataset of ethanol production in the U.S.
    ethanol_production <- read.csv(paste0(getwd(), "/inputs/air_quality/ethanol_production_US.csv")) %>%
      add_column("County" = NA) %>%
      add_column("FIPS" = NA) %>%
      add_column("PADD" = NA) %>%
      add_column("Capacity" = NA) %>%
      add_column("Unit" = "L")
    # Adding data for raw materials: raw ethanol and crude oil
    temp_ethanol <- fleet_fuel_use_tot_padd[which(fleet_fuel_use_tot_padd$Fuel == "Gasoline"),]
    temp_ethanol$Fuel <- "Raw_ethanol"
    temp_ethanol$Value <- fleet_fuel_use_tot_padd$Value[which(fleet_fuel_use_tot_padd$Fuel == "Gasoline")]*ethanol_to_fuel
    temp_oil <- temp_ethanol
    temp_oil$Fuel <- "Crude_oil"
    temp_oil$Value <- fleet_fuel_use_tot_padd$Value[which(fleet_fuel_use_tot_padd$Fuel == "Gasoline")]/crude_to_gasoline
    fleet_fuel_use_tot_padd <- rbind(fleet_fuel_use_tot_padd, temp_ethanol, temp_oil)
    city_county_correspondance <- read.csv(paste0(getwd(), "/inputs/air_quality/uscities.csv"))
    for (i in 1:dim(ethanol_production)[1]) {
      ethanol_production$County[i] <- city_county_correspondance$county_name[which(city_county_correspondance$city == ethanol_production$City[i] & city_county_correspondance$state_name == ethanol_production$State[i])]
      ethanol_production$FIPS[i] <- city_county_correspondance$county_fips[which(city_county_correspondance$city == ethanol_production$City[i] & city_county_correspondance$state_name == ethanol_production$State[i])]
      ethanol_production$PADD[i] <- GIS_matching_matrix$PADD[which(GIS_matching_matrix$FIPS == ethanol_production$FIPS[i])]
    }
    ethanol_production$Capacity <- ethanol_production$Mmgal.year*1E6*get_input_f(input_name = "conversion_units")$'1 gal'[8][1]
    ethanol_production <- select(ethanol_production, -c("Operator", "Mmgal.year", "Mb.d", "City", "State", "County", "FIPS"))
    ethanol_production <- aggregate(. ~ Unit + PADD, data = ethanol_production, FUN = sum)
    # Import the production data for petroleum products
    fuel_production <- read.csv(paste0(getwd(), "/inputs/air_quality/fuel_production_US.csv"))
    fuel_production$Value <- fuel_production$Value*1E3*158.987294928
    fuel_production$Unit <- "L"
    # Add the ethanol production data to the fuel production data
    temp_ethanol <- fuel_production[which(fuel_production$Fuel == "Gasoline"),]
    temp_ethanol$Fuel <- "Raw_ethanol"
    temp_ethanol$Value <- 0
    for (i in 1:dim(temp_ethanol)[1]) {
      if (temp_ethanol$PADD[i] == temp_ethanol$Origin[i]) {
        temp_ethanol$Value[i] <-  ethanol_production$Capacity[which(ethanol_production$PADD == temp_ethanol$PADD[i])]
      } else {
        temp_ethanol$Value[i] <- 0
      }
    }
    fuel_production <- rbind(fuel_production, temp_ethanol)
    padds <- 1:5
    for (i in 1:dim(fleet_fuel_use_tot_padd)[1]) {
      if (fleet_fuel_use_tot_padd$Year[i] <= first_proj_yr) {
        # The quantity of fuel from PADD X used in PADD X is taken as the total production in PADD X minus the total quantity of fuel from PADD X in other PADDs.
        production <- fuel_production$Value[which(fuel_production$PADD == fleet_fuel_use_tot_padd$PADD[i] &
                                                         fuel_production$Fuel == fleet_fuel_use_tot_padd$Fuel[i] &
                                                         fuel_production$Origin == fleet_fuel_use_tot_padd$PADD[i])]
        to_other_padds <- sum(fuel_production$Value[which(fuel_production$PADD != fleet_fuel_use_tot_padd$PADD[i] &
                                                             fuel_production$Fuel == fleet_fuel_use_tot_padd$Fuel[i] &
                                                             fuel_production$Origin == fleet_fuel_use_tot_padd$PADD[i])])
        exports <- fuel_production$Value[which(fuel_production$PADD == fleet_fuel_use_tot_padd$PADD[i] &
                                                 fuel_production$Fuel == fleet_fuel_use_tot_padd$Fuel[i] &
                                                 fuel_production$Origin == "Exports")]
        fleet_fuel_use_tot_padd[i,grep(fleet_fuel_use_tot_padd$PADD[i],colnames(fleet_fuel_use_tot_padd))] <- production-to_other_padds-exports
        for (j in padds[!padds%in%fleet_fuel_use_tot_padd$PADD[i]]) {
          fleet_fuel_use_tot_padd[i,grep(j,colnames(fleet_fuel_use_tot_padd))] <- fuel_production$Value[which(fuel_production$PADD == fleet_fuel_use_tot_padd$PADD[i] &
                                                                                                                    fuel_production$Fuel == fleet_fuel_use_tot_padd$Fuel[i] &
                                                                                                                    fuel_production$Origin == j)]
        }
        fleet_fuel_use_tot_padd$Imports[i] <- fuel_production$Value[which(fuel_production$PADD == fleet_fuel_use_tot_padd$PADD[i] &
                                                                          fuel_production$Fuel == fleet_fuel_use_tot_padd$Fuel[i] &
                                                                          fuel_production$Origin == "Imports")]
        fleet_fuel_use_tot_padd$Exports[i] <- fuel_production$Value[which(fuel_production$PADD == fleet_fuel_use_tot_padd$PADD[i] &
                                                                            fuel_production$Fuel == fleet_fuel_use_tot_padd$Fuel[i] &
                                                                            fuel_production$Origin == "Exports")]
        #fleet_fuel_use_tot_padd$Available_fuel[i] <- sum(fleet_fuel_use_tot_padd[i,7:13])
      } else {
        # Method 1: Changes in production and exchanges are scaled proportionally to changes in demand
        year_previous <- which(fleet_fuel_use_tot_padd$Fuel == fleet_fuel_use_tot_padd$Fuel[i] &
                                 fleet_fuel_use_tot_padd$Year == fleet_fuel_use_tot_padd$Year[i]-1 &
                                 fleet_fuel_use_tot_padd$PADD == fleet_fuel_use_tot_padd$PADD[i])
        relative_change <- fleet_fuel_use_tot_padd$Value[i]/fleet_fuel_use_tot_padd$Value[year_previous]
        for (j in 6:11) {
          fleet_fuel_use_tot_padd[i,j] <- fleet_fuel_use_tot_padd[year_previous,j]*relative_change
        }
        fleet_fuel_use_tot_padd[i,12] <- fleet_fuel_use_tot_padd[year_previous,12]
        # Method 2: Changes in demand is subtracted to production and exchanges 
        # Code to be added
      }
    }
    years <- min(fleet_fuel_use_tot_padd$Year):max(fleet_fuel_use_tot_padd$Year)
    fuel_dataset <- data.frame(matrix(nrow = 0, ncol = 8))
    fuels <- unique(fleet_fuel_use_tot_padd$Fuel)
    colnames(fuel_dataset) <- c("PADD", "Year", "Fuel", "Unit", "Production", "Consumption", "Rel_Production", "Rel_Consumption")
    for (i in padds) {
      for (j in years) {
        for (k in fuels) {
          production <-sum(fleet_fuel_use_tot_padd[which(fleet_fuel_use_tot_padd$Fuel == k & 
                                                           fleet_fuel_use_tot_padd$Year == j),grep(i, colnames(fleet_fuel_use_tot_padd))])#+sum(fleet_fuel_use_tot_padd$Exports[which(fleet_fuel_use_tot_padd$Fuel == k & 
                                                                                                                                          #                                             fleet_fuel_use_tot_padd$Year == j &
                                                                                                                                          #                                             fleet_fuel_use_tot_padd$PADD == i)])
          consumption <- fleet_fuel_use_tot_padd$Value[which(fleet_fuel_use_tot_padd$PADD == i &
                                                               fleet_fuel_use_tot_padd$Year == j &
                                                               fleet_fuel_use_tot_padd$Fuel == k)]
          if (j == min(years)) {
            fuel_dataset[nrow(fuel_dataset)+1,] <- list(i, j, k, "L", production, consumption, 1, 1)
          } else {
            production_previous_year <- sum(fleet_fuel_use_tot_padd[which(fleet_fuel_use_tot_padd$Fuel == k & 
                                                                            fleet_fuel_use_tot_padd$Year == j-1),grep(i, colnames(fleet_fuel_use_tot_padd))])#+sum(fleet_fuel_use_tot_padd$Exports[which(fleet_fuel_use_tot_padd$Fuel == k & 
                                                                                                                                                             #                                             fleet_fuel_use_tot_padd$Year == j &
                                                                                                                                                             #                                             fleet_fuel_use_tot_padd$PADD == i)])
            consumption_previous_year <- fleet_fuel_use_tot_padd$Value[which(fleet_fuel_use_tot_padd$PADD == i &
                                                                               fleet_fuel_use_tot_padd$Year == j-1 &
                                                                               fleet_fuel_use_tot_padd$Fuel == k)]
            fuel_dataset[nrow(fuel_dataset)+1,] <- list(i, j, k, "L", production, consumption, production/production_previous_year, consumption/consumption_previous_year)
          }
        }
      }
    }
    return(list(fuel_dataset = fuel_dataset))
  }
}
