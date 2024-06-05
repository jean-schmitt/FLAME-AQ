cobra_health_impact_f <- function(vehicles_emissions_county, electricity_emissions_fleet, electricity_emissions_background, ng_frac_usage, fleet_fuel_usage_US, emissions_battery_production, last_yr = NA, scenario_id = NA, fleet_electricity_consumption_source = NA, relative_transportation_electricity_demand = NA, calculation_mode = NA, year_break = NA, fleet_id = NA, COBRA_inflation_correction = NA, include_battery_manufacturing = NA, battery_calculation_method = NA, vehicle_EF_factor_NOx = NA, vehicle_EF_factor_SO2 = NA, vehicle_EF_factor_PM25 = NA, first_proj_yr = NA, background_emissions_evolution = NA, include_ng_emissions = NA, stack_height_allocation = NA) {
  attribute_f("cobra_health_impact_f")
  list_states_MOVES <- read.csv(paste0(getwd(), "/inputs/air_quality/list_states.csv"), sep = ";")
  list_states_COBRA <- get_input_f(input_name = 'COBRA_SOURCEINDX to FIPS crosswalk')
  states_correspondence <- data.frame(unique(list_states_COBRA$STNAME))
  colnames(states_correspondence) <- "name_state"
  states_correspondence <- states_correspondence %>%  
    add_column(COBRA_code = NA) %>%
    add_column(MOVES_code = NA)
  for (i in 1:dim(states_correspondence)[1]) {
    states_correspondence$COBRA_code[i] <- unique(list_states_COBRA$STFIPS[which(list_states_COBRA$STNAME == states_correspondence$name_state[i])])
    states_correspondence$MOVES_code[i] <- list_states_MOVES$stateID[which(list_states_MOVES$stateName == toupper(states_correspondence$name_state[i]))]
  }
  results_path <- paste0(getwd(), "/outputs/air_quality/", Sys.Date(), "_", scenario_id, "_results")
  GIS_matching_matrix <- read.csv(paste0(getwd(), "/inputs/air_quality/GIS_matching_matrix.csv"))
  emissions_tiers_definition <- get_input_f(input_name = 'COBRA_EmissionsTier Definitions')
  background_emissions_change <- read.csv(paste0(getwd(), "/inputs/air_quality/", "Background_emissions_change_", background_emissions_evolution, ".csv")) %>%
    pivot_wider(names_from = Pollutant, values_from = Change)
  
  # Generate proper baseline and scenario inputs for COBRA
  template_baseline <- get_input_f(input_name = 'COBRA_emissions_baseline_template')
  template_scenario <- get_input_f(input_name = 'COBRA_emissions_scenario_template')
  baseline <- template_baseline
  scenario <- template_scenario
  dir.create(paste0(results_path, "/COBRA_baseline_files"))
  dir.create(paste0(results_path, "/COBRA_scenario_files"))
  
  ## SOA emissions set to zero - Not considered in FLAME-AQ
  baseline$SOA <- 0
  scenario$SOA <- 0
  
  ## Generate the emissions files
  #if (fleet_id == "no_ldvs" | scenario_id == "ZEV_All_2014_mix_benefits_per_ev" | fleet_id == "no_ICEVs" | scenario_id == "NoEVs_All_2014_mix_benefits_ev" | scenario_id == "ZEV_All_NG_COBRA_benefits_per_ev_2023" | scenario_id == "ZEV_All_RE_COBRA_benefits_per_ev_2023" | grepl("elec_impacts_2022", scenario_id)) {
  if (scenario_id == "ZEV_All_2014_mix_benefits_per_ev" | fleet_id == "no_ICEVs" | scenario_id == "NoEVs_All_2014_mix_benefits_ev" | scenario_id == "ZEV_All_NG_COBRA_benefits_per_ev_2023" | scenario_id == "ZEV_All_RE_COBRA_benefits_per_ev_2023" | grepl("elec_impacts_2022", scenario_id)) {
    years <- first_proj_yr+2
    #years <- (first_proj_yr+2):last_yr
  } else {
    years <- (first_proj_yr+2):last_yr
  }
  COBRA_files_generation <- function(i) {
    if (i > first_proj_yr+2) {
      baseline <- read.csv(paste0(results_path, "/COBRA_scenario_files/COBRA_emissions_scenario_", (i-1), ".csv"))
    }
    scenario <- baseline
    ## Integration of fleet emissions into the baseline and scenario files
    baseline[which(scenario$TIER1 == 11 & baseline$TIER3 == 2),9:14] <- 0
    scenario[which(scenario$TIER1 == 11 & scenario$TIER3 == 2),9:14] <- 0
    index_fleet <- which(scenario$TIER1 == 11 & scenario$TIER2 == 14 & scenario$TIER3 == 2)
    temp <- left_join(vehicles_emissions_county, select(GIS_matching_matrix, c("FIPS", "COBRA_SOURCEINDX")), by = "FIPS")
    vehicles_emissions <- data.frame(Year = temp$Year,
                                     sourceindx = temp$COBRA_SOURCEINDX,
                                     NO2 = temp$NOx,
                                     SO2 = temp$SO2,
                                     NH3 = temp$NH3,
                                     SOA = 0,
                                     PM25 = temp$Total_PM25+temp$Brake_PM25+temp$Tire_PM25,
                                     VOC = temp$VOC)
    baseline_vehicles <- baseline[index_fleet,]
    scenario_vehicles <- scenario[index_fleet,]
    baseline_vehicles <- left_join(select(baseline_vehicles, -c("NO2", "SO2", "NH3", "SOA", "PM25", "VOC")), select(filter(vehicles_emissions, vehicles_emissions$Year == i-1), -c("Year")), by = "sourceindx")
    baseline_vehicles <- filter(baseline_vehicles, !is.na(baseline_vehicles$NO2))
    scenario_vehicles <- left_join(select(scenario_vehicles, -c("NO2", "SO2", "NH3", "SOA", "PM25", "VOC")), select(filter(vehicles_emissions, vehicles_emissions$Year == i), -c("Year")), by = "sourceindx")
    scenario_vehicles <- filter(scenario_vehicles, !is.na(scenario_vehicles$NO2))
    
    ## Integration of emissions from electricity production
    index_electricity <- which(scenario$TIER1 == 1 & scenario$TIER2%in%c(1,2,3))
    ## Allocation of the emissions
    if (stack_height_allocation == "default") {
      ### Allocate emissions based on existing inventory in COBRA
      electricity_emissions_fleet <- filter(electricity_emissions_fleet, !electricity_emissions_fleet$Fuel_type%in%c("Renewable", "Nuclear"))
      electricity_emissions_fleet <- aggregate(.~FIPS+Year, data = select(electricity_emissions_fleet, -c("Fuel_type", "Reg_entity", "Unit")), FUN = sum) %>%
        left_join(select(GIS_matching_matrix, c("FIPS", "Cambium.GEA")), by = "FIPS") %>%
        select(-c("FIPS"))
      electricity_emissions_fleet <- aggregate(.~Cambium.GEA+Year, data = electricity_emissions_fleet, FUN = sum)
      electricity_emissions_background <- filter(electricity_emissions_background, !electricity_emissions_background$Fuel_type%in%c("Renewable", "Nuclear"))
      electricity_emissions_background <- aggregate(.~FIPS+Year, data = select(electricity_emissions_background, -c("Fuel_type", "Reg_entity", "Unit")), FUN = sum) %>%
        left_join(select(GIS_matching_matrix, c("FIPS", "Cambium.GEA")), by = "FIPS") %>%
        select(-c("FIPS"))
      electricity_emissions_background <- aggregate(.~Cambium.GEA+Year, data = electricity_emissions_background, FUN = sum)
      ## Normalize electricity emissions in the COBRA dataset by location
      baseline_electricity <- baseline[index_electricity,]
      colnames(baseline_electricity)[3] <- "COBRA_SOURCEINDX"
      baseline_electricity <- left_join(baseline_electricity, select(GIS_matching_matrix, c("COBRA_SOURCEINDX", "Cambium.GEA")), by = "COBRA_SOURCEINDX")
      aggregated_emissions <- aggregate(.~Cambium.GEA, data = select(baseline_electricity, -c("ID", "typeindx", "COBRA_SOURCEINDX", "stid", "cyid", "TIER1", "TIER2", "TIER3")), FUN = sum)
      baseline_electricity_normalized <- left_join(baseline_electricity, aggregated_emissions, by = c("Cambium.GEA"), relationship = "many-to-many")
      baseline_electricity_normalized[,9:14] <- baseline_electricity_normalized[,9:14]/baseline_electricity_normalized[,16:21]
      baseline_electricity_normalized <- baseline_electricity_normalized[,1:15]
      baseline_electricity_normalized <- select(baseline_electricity_normalized, -c("SOA.x"))
      colnames(baseline_electricity_normalized)[9:13] <- c("NO2_Norm", "SO2_Norm", "NH3_Norm", "PM25_Norm", "VOC_Norm")
      ### Calculate the baseline and scenario emissions from electricity production
        ### Baseline
      baseline_electricity <- left_join(baseline_electricity_normalized, electricity_emissions_fleet[which(electricity_emissions_fleet$Year == i-1),], by = c("Cambium.GEA"))
      baseline_electricity[,9:13] <- baseline_electricity[,9:13]*baseline_electricity[,16:20]
      baseline_electricity[is.na(baseline_electricity)] <- 0
      baseline_electricity <- baseline_electricity[,1:13]
      colnames(baseline_electricity)[9:13] <- c("NO2", "SO2", "NH3", "PM25", "VOC")
      colnames(baseline_electricity)[3] <- "sourceindx"
      baseline_electricity <- add_column(baseline_electricity, "SOA" = 0, .after = "NH3")
        ### Scenario
      scenario_electricity <- left_join(baseline_electricity_normalized, electricity_emissions_fleet[which(electricity_emissions_fleet$Year == i),], by = c("Cambium.GEA"))
      scenario_electricity[,9:13] <- scenario_electricity[,9:13]*scenario_electricity[,16:20]
      scenario_electricity[is.na(scenario_electricity)] <- 0
      scenario_electricity <- scenario_electricity[,1:13]
      colnames(scenario_electricity)[9:13] <- c("NO2", "SO2", "NH3", "PM25", "VOC")
      colnames(scenario_electricity)[3] <- "sourceindx"
      scenario_electricity <- add_column(scenario_electricity, "SOA" = 0, .after = "NH3")
        ### Background
      background_electricity <- left_join(baseline_electricity_normalized, electricity_emissions_background[which(electricity_emissions_background$Year == i),], by = c("Cambium.GEA"))
      background_electricity[,9:13] <- background_electricity[,9:13]*background_electricity[,16:20]
      background_electricity[is.na(background_electricity)] <- 0
      background_electricity <- background_electricity[,1:13]
      colnames(background_electricity)[9:13] <- c("NO2", "SO2", "NH3", "PM25", "VOC")
      colnames(background_electricity)[3] <- "sourceindx"
      background_electricity <- add_column(background_electricity, "SOA" = 0, .after = "NH3")
    } else if (grepl("class", stack_height_allocation)) {
      ### Baseline = background emissions
      baseline_electricity <- select(electricity_emissions_fleet[which(electricity_emissions_fleet$Year == i-1),], -c("Reg_entity", "Year", "Unit")) %>%
        filter(!Fuel_type%in%c("Renewable", "Nuclear")) %>%
        add_column("ID" = 2) %>%
        add_column("typeindx" = str_split(stack_height_allocation, "_")[[1]][2]) %>%
        add_column("TIER1" = 1) %>%
        add_column("TIER2" = NA) %>%
        add_column("TIER3" = 1) %>%
        left_join(select(GIS_matching_matrix, c("ST_FIPS", "CY_FIPS", "COBRA_SOURCEINDX", "FIPS")), by = "FIPS")
      baseline_electricity$TIER2[which(baseline_electricity$Fuel_type == "Coal")] <- 1
      baseline_electricity$TIER2[which(baseline_electricity$Fuel_type == "Oil")] <- 2
      baseline_electricity$TIER2[which(baseline_electricity$Fuel_type == "Natural Gas")] <- 3
      baseline_electricity <- data.frame(ID = 2,
                                         typeindx = baseline_electricity$typeindx,
                                         sourceindx = baseline_electricity$COBRA_SOURCEINDX,
                                         stid = baseline_electricity$ST_FIPS,
                                         cyid = baseline_electricity$CY_FIPS,
                                         TIER1 = baseline_electricity$TIER1,
                                         TIER2 = baseline_electricity$TIER2,
                                         TIER3 = baseline_electricity$TIER3,
                                         NO2 = baseline_electricity$NOx,
                                         SO2 = baseline_electricity$SO2,
                                         NH3 = baseline_electricity$NH3,
                                         SOA = 0,
                                         PM25 = baseline_electricity$PM25,
                                         VOC = baseline_electricity$VOC)
      ### Scenario = fleet-related emissions
      scenario_electricity <- select(electricity_emissions_fleet[which(electricity_emissions_fleet$Year == i),], -c("Reg_entity", "Year", "Unit")) %>%
        filter(!Fuel_type%in%c("Renewable", "Nuclear")) %>%
        add_column("ID" = 2) %>%
        add_column("typeindx" = str_split(stack_height_allocation, "_")[[1]][2]) %>%
        add_column("TIER1" = 1) %>%
        add_column("TIER2" = NA) %>%
        add_column("TIER3" = 1) %>%
        left_join(select(GIS_matching_matrix, c("ST_FIPS", "CY_FIPS", "COBRA_SOURCEINDX", "FIPS")), by = "FIPS")
      scenario_electricity$TIER2[which(scenario_electricity$Fuel_type == "Coal")] <- 1
      scenario_electricity$TIER2[which(scenario_electricity$Fuel_type == "Oil")] <- 2
      scenario_electricity$TIER2[which(scenario_electricity$Fuel_type == "Natural Gas")] <- 3
      scenario_electricity <- data.frame(ID = 2,
                                         typeindx = scenario_electricity$typeindx,
                                         sourceindx = scenario_electricity$COBRA_SOURCEINDX,
                                         stid = scenario_electricity$ST_FIPS,
                                         cyid = scenario_electricity$CY_FIPS,
                                         TIER1 = scenario_electricity$TIER1,
                                         TIER2 = scenario_electricity$TIER2,
                                         TIER3 = scenario_electricity$TIER3,
                                         NO2 = scenario_electricity$NOx,
                                         SO2 = scenario_electricity$SO2,
                                         NH3 = scenario_electricity$NH3,
                                         SOA = 0,
                                         PM25 = scenario_electricity$PM25,
                                         VOC = scenario_electricity$VOC)
      background_electricity <- select(electricity_emissions_background[which(electricity_emissions_background$Year == i),], -c("Reg_entity", "Year", "Unit")) %>%
        filter(!Fuel_type%in%c("Renewable", "Nuclear")) %>%
        add_column("ID" = 2) %>%
        add_column("typeindx" = str_split(stack_height_allocation, "_")[[1]][2]) %>%
        add_column("TIER1" = 1) %>%
        add_column("TIER2" = NA) %>%
        add_column("TIER3" = 1) %>%
        left_join(select(GIS_matching_matrix, c("ST_FIPS", "CY_FIPS", "COBRA_SOURCEINDX", "FIPS")), by = "FIPS")
      background_electricity$TIER2[which(background_electricity$Fuel_type == "Coal")] <- 1
      background_electricity$TIER2[which(background_electricity$Fuel_type == "Oil")] <- 2
      background_electricity$TIER2[which(background_electricity$Fuel_type == "Natural Gas")] <- 3
      background_electricity <- data.frame(ID = 2,
                                           typeindx = background_electricity$typeindx,
                                           sourceindx = background_electricity$COBRA_SOURCEINDX,
                                           stid = background_electricity$ST_FIPS,
                                           cyid = background_electricity$CY_FIPS,
                                           TIER1 = background_electricity$TIER1,
                                           TIER2 = background_electricity$TIER2,
                                           TIER3 = background_electricity$TIER3,
                                           NO2 = background_electricity$NOx,
                                           SO2 = background_electricity$SO2,
                                           NH3 = background_electricity$NH3,
                                           SOA = 0,
                                           PM25 = background_electricity$PM25,
                                           VOC = background_electricity$VOC)
    } else if (stack_height_allocation == "average") {
      ###  Calculate the national average stack height distribution from the COBRA input dataset
      average_stack_height <- aggregate(.~typeindx+TIER2, data = select(baseline[index_electricity,], -c("ID", "sourceindx", "cyid", "stid", "TIER1", "TIER3")), FUN = sum)
      average_stack_height <- left_join(average_stack_height, aggregate(.~TIER2, data = select(baseline[index_electricity,], -c("ID", "typeindx", "sourceindx", "cyid", "stid", "TIER1", "TIER3")), FUN = sum), by = c("TIER2"))
      average_stack_height[,3:8] <- average_stack_height[,3:8]/average_stack_height[,9:14]
      average_stack_height$SOA.x <- 0
      average_stack_height <- average_stack_height[,1:8]
      colnames(average_stack_height)[3:8] <- c("NO2_stack", "SO2_stack", "NH3_stack", "SOA_stack", "PM25_stack", "VOC_stack")
      states <- unique(baseline$stid)
      fun <- function(i) {
        average_stack_height <- add_column(average_stack_height, "stid" = i, .after = "typeindx")
        return(average_stack_height)
      }
      stack_height_data <- do.call(rbind, lapply(states, fun))
      ### Baseline = background emissions
      baseline_electricity <- select(electricity_emissions_fleet[which(electricity_emissions_fleet$Year == i-1),], -c("Reg_entity", "Year", "Unit")) %>%
        filter(!Fuel_type%in%c("Renewable", "Nuclear")) %>%
        add_column("ID" = 2) %>%
        add_column("TIER1" = 1) %>%
        add_column("TIER2" = NA) %>%
        add_column("TIER3" = 1) %>%
        left_join(select(GIS_matching_matrix, c("ST_FIPS", "CY_FIPS", "COBRA_SOURCEINDX", "FIPS")), by = "FIPS")
      baseline_electricity$TIER2[which(baseline_electricity$Fuel_type == "Coal")] <- 1
      baseline_electricity$TIER2[which(baseline_electricity$Fuel_type == "Oil")] <- 2
      baseline_electricity$TIER2[which(baseline_electricity$Fuel_type == "Natural Gas")] <- 3
      baseline_electricity <- data.frame(ID = 2,
                                         sourceindx = baseline_electricity$COBRA_SOURCEINDX,
                                         stid = baseline_electricity$ST_FIPS,
                                         cyid = baseline_electricity$CY_FIPS,
                                         TIER1 = baseline_electricity$TIER1,
                                         TIER2 = baseline_electricity$TIER2,
                                         TIER3 = baseline_electricity$TIER3,
                                         NO2 = baseline_electricity$NOx,
                                         SO2 = baseline_electricity$SO2,
                                         NH3 = baseline_electricity$NH3,
                                         SOA = 0,
                                         PM25 = baseline_electricity$PM25,
                                         VOC = baseline_electricity$VOC)
      baseline_electricity <- left_join(baseline_electricity, stack_height_data, by = c("stid", "TIER2"), relationship = "many-to-many")
      baseline_electricity$NO2 <- baseline_electricity$NO2*baseline_electricity$NO2_stack
      baseline_electricity$SO2 <- baseline_electricity$SO2*baseline_electricity$SO2_stack
      baseline_electricity$NH3 <- baseline_electricity$NH3*baseline_electricity$NH3_stack
      baseline_electricity$SOA <- baseline_electricity$SOA*baseline_electricity$SOA_stack
      baseline_electricity$PM25 <- baseline_electricity$PM25*baseline_electricity$PM25_stack
      baseline_electricity$VOC <- baseline_electricity$VOC*baseline_electricity$VOC_stack
      baseline_electricity <- baseline_electricity[,1:14] %>%
        relocate("typeindx", .before = "sourceindx")
      ### Scenario = fleet-related emissions
      scenario_electricity <- select(electricity_emissions_fleet[which(electricity_emissions_fleet$Year == i),], -c("Reg_entity", "Year", "Unit")) %>%
        filter(!Fuel_type%in%c("Renewable", "Nuclear")) %>%
        add_column("ID" = 2) %>%
        add_column("TIER1" = 1) %>%
        add_column("TIER2" = NA) %>%
        add_column("TIER3" = 1) %>%
        left_join(select(GIS_matching_matrix, c("ST_FIPS", "CY_FIPS", "COBRA_SOURCEINDX", "FIPS")), by = "FIPS")
      scenario_electricity$TIER2[which(scenario_electricity$Fuel_type == "Coal")] <- 1
      scenario_electricity$TIER2[which(scenario_electricity$Fuel_type == "Oil")] <- 2
      scenario_electricity$TIER2[which(scenario_electricity$Fuel_type == "Natural Gas")] <- 3
      scenario_electricity <- data.frame(ID = 2,
                                         sourceindx = scenario_electricity$COBRA_SOURCEINDX,
                                         stid = scenario_electricity$ST_FIPS,
                                         cyid = scenario_electricity$CY_FIPS,
                                         TIER1 = scenario_electricity$TIER1,
                                         TIER2 = scenario_electricity$TIER2,
                                         TIER3 = scenario_electricity$TIER3,
                                         NO2 = scenario_electricity$NOx,
                                         SO2 = scenario_electricity$SO2,
                                         NH3 = scenario_electricity$NH3,
                                         SOA = 0,
                                         PM25 = scenario_electricity$PM25,
                                         VOC = scenario_electricity$VOC)
      scenario_electricity <- left_join(scenario_electricity, stack_height_data, by = c("stid", "TIER2"), relationship = "many-to-many")
      scenario_electricity$NO2 <- scenario_electricity$NO2*scenario_electricity$NO2_stack
      scenario_electricity$SO2 <- scenario_electricity$SO2*scenario_electricity$SO2_stack
      scenario_electricity$NH3 <- scenario_electricity$NH3*scenario_electricity$NH3_stack
      scenario_electricity$SOA <- scenario_electricity$SOA*scenario_electricity$SOA_stack
      scenario_electricity$PM25 <- scenario_electricity$PM25*scenario_electricity$PM25_stack
      scenario_electricity$VOC <- scenario_electricity$VOC*scenario_electricity$VOC_stack
      scenario_electricity <- scenario_electricity[,1:14] %>%
        relocate("typeindx", .before = "sourceindx")
      ### Background electricity emissions
      background_electricity <- select(electricity_emissions_background[which(electricity_emissions_background$Year == i),], -c("Reg_entity", "Year", "Unit")) %>%
        filter(!Fuel_type%in%c("Renewable", "Nuclear")) %>%
        add_column("ID" = 2) %>%
        add_column("TIER1" = 1) %>%
        add_column("TIER2" = NA) %>%
        add_column("TIER3" = 1) %>%
        left_join(select(GIS_matching_matrix, c("ST_FIPS", "CY_FIPS", "COBRA_SOURCEINDX", "FIPS")), by = "FIPS")
      background_electricity$TIER2[which(background_electricity$Fuel_type == "Coal")] <- 1
      background_electricity$TIER2[which(background_electricity$Fuel_type == "Oil")] <- 2
      background_electricity$TIER2[which(background_electricity$Fuel_type == "Natural Gas")] <- 3
      background_electricity <- data.frame(ID = 2,
                                           sourceindx = background_electricity$COBRA_SOURCEINDX,
                                           stid = background_electricity$ST_FIPS,
                                           cyid = background_electricity$CY_FIPS,
                                           TIER1 = background_electricity$TIER1,
                                           TIER2 = background_electricity$TIER2,
                                           TIER3 = background_electricity$TIER3,
                                           NO2 = background_electricity$NOx,
                                           SO2 = background_electricity$SO2,
                                           NH3 = background_electricity$NH3,
                                           SOA = 0,
                                           PM25 = background_electricity$PM25,
                                           VOC = background_electricity$VOC)
      background_electricity <- left_join(background_electricity, stack_height_data, by = c("stid", "TIER2"), relationship = "many-to-many")
      background_electricity$NO2 <- background_electricity$NO2*background_electricity$NO2_stack
      background_electricity$SO2 <- background_electricity$SO2*background_electricity$SO2_stack
      background_electricity$NH3 <- background_electricity$NH3*background_electricity$NH3_stack
      background_electricity$SOA <- background_electricity$SOA*background_electricity$SOA_stack
      background_electricity$PM25 <- background_electricity$PM25*background_electricity$PM25_stack
      background_electricity$VOC <- background_electricity$VOC*background_electricity$VOC_stack
      background_electricity <- background_electricity[,1:14] %>%
        relocate("typeindx", .before = "sourceindx")
    }
  
    ## Integration of emissions from oil&gas processing
    ###   Indexes related oil and gas extraction, refining, and distribution 
    index_petroleum <- which(scenario$TIER1 == 6 & scenario$TIER2 == 1 & scenario$TIER3 == 99)
    index_petroleum_storage <- which(scenario$TIER1 == 9 & scenario$TIER2 == 2 & (scenario$TIER3 == 2 | scenario$TIER3 == 4 | scenario$TIER3 == 6 | scenario$TIER3 == 8))
    index_refining <- which(scenario$TIER1 == 6 & scenario$TIER2 == 2)
    index_refined_storage <- which(scenario$TIER1 == 9 & scenario$TIER2 == 2 &(scenario$TIER3 == 1 | scenario$TIER3 == 3 | scenario$TIER3 == 5 | scenario$TIER3 == 7 | scenario$TIER3 == 9 | scenario$TIER3 == 10))
    index_refined_storage_bis <- which(scenario$TIER1 == 9 & scenario$TIER2 == 3 & scenario$TIER3 != 5)
    index_refined_distribution <- which(scenario$TIER1 == 9 & (scenario$TIER2 == 4 | scenario$TIER2 == 5 | scenario$TIER2 == 6))
    index_ethanol <- which(scenario$TIER1 == 7 & scenario$TIER2 == 99 & scenario$TIER3 == 1)
    index_ng <- which(scenario$TIER1 == 6 & scenario$TIER2 == 1 & scenario$TIER3 == 1)
    ###   Sub-classification of the indexes
    index_demand <- c(index_refined_distribution, index_refined_storage_bis, index_refined_storage)
    index_refining <- c(index_refining)
    index_crude <- c(index_petroleum_storage, index_petroleum)
    index_total_oil <- c(index_demand, index_refining, index_crude, index_ethanol, index_ng)
    ###   Integration into the baseline and scenario datasets
    baseline_refining <- baseline
    scenario_refining <- scenario
    ### Apply the emissions correction factor (changes in background)
    baseline_refining$NO2 <- baseline_refining$NO2*(1+background_emissions_change$NOx[which(background_emissions_change$Year == i)]/100)
    baseline_refining$SO2 <- baseline_refining$SO2*(1+background_emissions_change$SO2[which(background_emissions_change$Year == i)]/100)
    baseline_refining$NH3 <- baseline_refining$NH3*(1+background_emissions_change$NH3[which(background_emissions_change$Year == i)]/100)
    baseline_refining$PM25 <- baseline_refining$PM25*(1+background_emissions_change$PM25[which(background_emissions_change$Year == i)]/100)
    baseline_refining$VOC <- baseline_refining$VOC*(1+background_emissions_change$VOC[which(background_emissions_change$Year == i)]/100)
    scenario_refining$NO2 <- scenario_refining$NO2*(1+background_emissions_change$NOx[which(background_emissions_change$Year == i)]/100)
    scenario_refining$SO2 <- scenario_refining$SO2*(1+background_emissions_change$SO2[which(background_emissions_change$Year == i)]/100)
    scenario_refining$NH3 <- scenario_refining$NH3*(1+background_emissions_change$NH3[which(background_emissions_change$Year == i)]/100)
    scenario_refining$PM25 <- scenario_refining$PM25*(1+background_emissions_change$PM25[which(background_emissions_change$Year == i)]/100)
    scenario_refining$VOC <- scenario_refining$VOC*(1+background_emissions_change$VOC[which(background_emissions_change$Year == i)]/100)
    ###   Emissions from changes in demand
    oil_gas_activity <- left_join(fleet_fuel_usage_US[which(fleet_fuel_usage_US$Year == i),], select(GIS_matching_matrix, c("PADD", "COBRA_SOURCEINDX")), by = "PADD", relationship = "many-to-many")
    colnames(oil_gas_activity)[10] <- "sourceindx"
    scenario_refining <- left_join(scenario_refining, select(oil_gas_activity, -c("PADD", "Year")), by = "sourceindx")
    scenario_refining[index_demand,9:14] <- scenario_refining[index_demand,9:14]*scenario_refining$Rel_Demand[index_demand]
    scenario_refining[index_refining,9:14] <- scenario_refining[index_refining,9:14]*scenario_refining$Rel_Refining[index_refining]
    scenario_refining[index_crude,9:14] <- scenario_refining[index_crude,9:14]*scenario_refining$Rel_Crude_oil[index_crude]
    scenario_refining[index_ethanol,9:14] <- scenario_refining[index_ethanol,9:14]*scenario_refining$Rel_Ethanol[index_ethanol]
    if (include_ng_emissions == "y") {
      scenario_refining[index_ng,9:14] <- scenario_refining[index_ng,9:14]*scenario_refining$Rel_Natural_Gas_total
    } else {
      scenario_refining[index_ng,9:14] <- scenario_refining[index_ng,9:14]
    }
    #scenario_refining <- scenario_refining[index_total_oil,]
    scenario_refining <- filter(scenario_refining, !is.na(scenario_refining$Rel_Demand))
    scenario_refining <- scenario_refining[,1:14]
    ###   Adjustment of the baseline
    baseline_refining <- left_join(baseline_refining, select(oil_gas_activity, -c("PADD", "Year")), by = "sourceindx")
    baseline_refining[index_demand,9:14] <- baseline_refining[index_demand,9:14]
    baseline_refining[index_refining,9:14] <- baseline_refining[index_refining,9:14]
    baseline_refining[index_crude,9:14] <- baseline_refining[index_crude,9:14]
    baseline_refining[index_ethanol,9:14] <-baseline_refining[index_ethanol,9:14]
    if (include_ng_emissions == "y") {
      baseline_refining[index_ng,9:14] <- baseline_refining[index_ng,9:14]*baseline_refining$Rel_Natural_Gas_background
    } else {
      baseline_refining[index_ng,9:14] <- baseline_refining[index_ng,9:14]
    }
    #baseline_refining <- baseline_refining[index_total_oil,]
    baseline_refining <- filter(baseline_refining, !is.na(baseline_refining$Rel_Demand))
    baseline_refining <- baseline_refining[,1:14]
    ## Select only the lines of interest
    baseline_refining <- baseline_refining[index_total_oil,]
    scenario_refining <- scenario_refining[index_total_oil,]
    ## Extract the emissions from oil&gas refining
    oil_gas_emissions_scenario <- scenario_refining %>%
      add_column("Year" = i) %>%
      add_column("Type" = "Scenario")
    colnames(oil_gas_emissions_scenario)[3] <- "COBRA_SOURCEINDX"
    oil_gas_emissions_scenario <- left_join(oil_gas_emissions_scenario, select(GIS_matching_matrix, c("COBRA_SOURCEINDX", "FIPS", "ST_FIPS")), by = "COBRA_SOURCEINDX")
    oil_gas_emissions_scenario_county <- aggregate(.~FIPS+ST_FIPS+Year+Type, data = select(oil_gas_emissions_scenario, -c("ID", "typeindx", "COBRA_SOURCEINDX", "stid", "cyid", "TIER1", "TIER2", "TIER3")), FUN = sum)
    oil_gas_emissions_scenario_state <- aggregate(.~ST_FIPS+Year+Type, data = select(oil_gas_emissions_scenario_county, -c("FIPS")), FUN = sum)
    oil_gas_emissions_scenario <- aggregate(.~Year+Type, data = select(oil_gas_emissions_scenario_state, -c("ST_FIPS")), FUN = sum)
    
    oil_gas_emissions_background <- baseline_refining %>%
      add_column("Year" = i) %>%
      add_column("Type" = "Baseline")
    colnames(oil_gas_emissions_background)[3] <- "COBRA_SOURCEINDX"
    oil_gas_emissions_background <- left_join(oil_gas_emissions_background, select(GIS_matching_matrix, c("COBRA_SOURCEINDX", "FIPS", "ST_FIPS")), by = "COBRA_SOURCEINDX")
    oil_gas_emissions_background_county <- aggregate(.~FIPS+ST_FIPS+Year+Type, data = select(oil_gas_emissions_background, -c("ID", "typeindx", "COBRA_SOURCEINDX", "stid", "cyid", "TIER1", "TIER2", "TIER3")), FUN = sum)
    oil_gas_emissions_background_state <- aggregate(.~ST_FIPS+Year+Type, data = select(oil_gas_emissions_background_county, -c("FIPS")), FUN = sum)
    oil_gas_emissions_background <- aggregate(.~Year+Type, data = select(oil_gas_emissions_background_state, -c("ST_FIPS")), FUN = sum)
    
    oil_gas_emissions_county <- rbind(oil_gas_emissions_background_county, oil_gas_emissions_scenario_county)
    oil_gas_emissions_state <- rbind(oil_gas_emissions_background_state, oil_gas_emissions_scenario_state)
    oil_gas_emissions <- rbind(oil_gas_emissions_background, oil_gas_emissions_scenario)
    
    ## Integration of emissions from battery manufacturing
    if (grepl("GREET", battery_calculation_method) & include_battery_manufacturing == "Y"){
      ## Import the distribution of automotive manufacturing plants
      automotive_manufacturing_data <- read.csv(paste0(getwd(), "/inputs/air_quality/US_NEI_Vehicle_Manufacturing.csv")) %>%
        select(c("FIPS", "Pollutant", "Emissions"))
      automotive_manufacturing_data <- aggregate(.~FIPS+Pollutant, data = automotive_manufacturing_data, FUN = sum)
      automotive_manufacturing_data <- left_join(automotive_manufacturing_data, aggregate(.~Pollutant, data = select(automotive_manufacturing_data, -c("FIPS")), FUN = sum), by = "Pollutant")
      automotive_manufacturing_data$Emissions.x <- automotive_manufacturing_data$Emissions.x/automotive_manufacturing_data$Emissions.y
      automotive_manufacturing_data <- pivot_wider(data = select(automotive_manufacturing_data, -c("Emissions.y")), names_from = "Pollutant", values_from = "Emissions.x", values_fill = 0)
      colnames(automotive_manufacturing_data) <- c("FIPS", "NO2_factor", "PM25_factor", "SO2_factor", "VOC_factor")
      ## Allocate the emissions
      battery_emissions <- expand.grid(Year = emissions_battery_production$Year, FIPS = automotive_manufacturing_data$FIPS)
      battery_emissions <- left_join(left_join(battery_emissions, automotive_manufacturing_data, by = "FIPS"), emissions_battery_production, by = "Year")
      battery_emissions$NOx <- battery_emissions$NOx*battery_emissions$NO2_factor
      battery_emissions$SO2 <- battery_emissions$SO2*battery_emissions$SO2_factor
      battery_emissions$PM25 <- battery_emissions$PM25*battery_emissions$PM25_factor
      battery_emissions$VOC <- battery_emissions$VOC*battery_emissions$VOC_factor
      battery_emissions <- select(battery_emissions, -c("NO2_factor", "PM25_factor", "SO2_factor", "VOC_factor"))
      battery_emissions <- left_join(battery_emissions, select(GIS_matching_matrix, c("COBRA_SOURCEINDX", "FIPS", "ST_FIPS", "CY_FIPS")), by = "FIPS")
      colnames(battery_emissions)[3] <- "NO2"
      ## Generate the baseline and scenario files
      baseline_battery <- data.frame(ID = 2,
                                     typeindx = 2,
                                     sourceindx = battery_emissions$COBRA_SOURCEINDX[which(battery_emissions$Year == i-1)],
                                     stid = battery_emissions$ST_FIPS[which(battery_emissions$Year == i-1)],
                                     cyid = battery_emissions$CY_FIPS[which(battery_emissions$Year == i-1)],
                                     TIER1 = 7,
                                     TIER2 = 8,
                                     TIER3 = 99,
                                     NO2 = battery_emissions$NO2[which(battery_emissions$Year == i-1)],
                                     SO2 = battery_emissions$SO2[which(battery_emissions$Year == i-1)],
                                     NH3 = battery_emissions$NH3[which(battery_emissions$Year == i-1)],
                                     SOA = 0,
                                     PM25 = battery_emissions$PM25[which(battery_emissions$Year == i-1)],
                                     VOC = battery_emissions$VOC[which(battery_emissions$Year == i-1)])
      scenario_battery <- data.frame(ID = 2,
                                     typeindx = 2,
                                     sourceindx = battery_emissions$COBRA_SOURCEINDX[which(battery_emissions$Year == i)],
                                     stid = battery_emissions$ST_FIPS[which(battery_emissions$Year == i)],
                                     cyid = battery_emissions$CY_FIPS[which(battery_emissions$Year == i)],
                                     TIER1 = 7,
                                     TIER2 = 8,
                                     TIER3 = 99,
                                     NO2 = battery_emissions$NO2[which(battery_emissions$Year == i)],
                                     SO2 = battery_emissions$SO2[which(battery_emissions$Year == i)],
                                     NH3 = battery_emissions$NH3[which(battery_emissions$Year == i)],
                                     SOA = 0,
                                     PM25 = battery_emissions$PM25[which(battery_emissions$Year == i)],
                                     VOC = battery_emissions$VOC[which(battery_emissions$Year == i)])
    } else {
      baseline_battery <- data.frame(ID = 2, typeindx = 2, sourceindx = 1, stid = 1, cyid = 1, TIER1 = 7, TIER2 = 8, TIER3 = 99, NO2 = 0, SO2 = 0, NH3 = 0, SOA = 0, PM25 = 0, VOC = 0)
      scenario_battery <- data.frame(ID = 2, typeindx = 2, sourceindx = 1, stid = 1, cyid = 1, TIER1 = 7, TIER2 = 8, TIER3 = 99, NO2 = 0, SO2 = 0, NH3 = 0, SOA = 0, PM25 = 0, VOC = 0)
    }

    ## Generation of the general baseline and scenario files containing information on background emissions
    index_background <- rownames(scenario)[!rownames(scenario)%in%c(index_fleet, index_electricity, index_total_oil)]
    baseline_background <- baseline[index_background,]
    scenario_background <- scenario[index_background,]
    baseline_background$NO2 <- baseline_background$NO2*(1+background_emissions_change$NOx[which(background_emissions_change$Year == i)]/100)
    baseline_background$SO2 <- baseline_background$SO2*(1+background_emissions_change$SO2[which(background_emissions_change$Year == i)]/100)
    baseline_background$NH3 <- baseline_background$NH3*(1+background_emissions_change$NH3[which(background_emissions_change$Year == i)]/100)
    baseline_background$PM25 <- baseline_background$PM25*(1+background_emissions_change$PM25[which(background_emissions_change$Year == i)]/100)
    baseline_background$VOC <- baseline_background$VOC*(1+background_emissions_change$VOC[which(background_emissions_change$Year == i)]/100)
    scenario_background$NO2 <- scenario_background$NO2*(1+background_emissions_change$NOx[which(background_emissions_change$Year == i)]/100)
    scenario_background$SO2 <- scenario_background$SO2*(1+background_emissions_change$SO2[which(background_emissions_change$Year == i)]/100)
    scenario_background$NH3 <- scenario_background$NH3*(1+background_emissions_change$NH3[which(background_emissions_change$Year == i)]/100)
    scenario_background$PM25 <- scenario_background$PM25*(1+background_emissions_change$PM25[which(background_emissions_change$Year == i)]/100)
    scenario_background$VOC <- scenario_background$VOC*(1+background_emissions_change$VOC[which(background_emissions_change$Year == i)]/100)
    
    ## Creation of the total baseline and scenario files for the given year
    baseline <- rbind(baseline_background, baseline_vehicles, background_electricity, baseline_electricity, baseline_refining, baseline_battery)
    scenario <- rbind(scenario_background, scenario_vehicles, background_electricity, scenario_electricity, scenario_refining, scenario_battery)
    if (grepl("elec_impacts_2022", scenario_id)) {
      baseline <- rbind(baseline_background, baseline_vehicles, baseline_refining, baseline_battery)
      scenario <- rbind(baseline_background, baseline_vehicles, background_electricity, scenario_electricity, baseline_refining, baseline_battery)
    }
    
    ## Export the files
    baseline <- aggregate(.~ID+typeindx+sourceindx+stid+cyid+TIER1+TIER2+TIER3, data = baseline, FUN = sum)
    scenario <- aggregate(.~ID+typeindx+sourceindx+stid+cyid+TIER1+TIER2+TIER3, data = scenario, FUN = sum)
    write.csv(baseline, paste0(results_path, "/COBRA_baseline_files/COBRA_emissions_baseline_", i, ".csv"), row.names = FALSE)
    write.csv(scenario, paste0(results_path, "/COBRA_scenario_files/COBRA_emissions_scenario_", i, ".csv"), row.names = FALSE)
    #Write the oil and gas emissions files
    if (i == first_proj_yr+2) {
      write.table(oil_gas_emissions, paste0(results_path, "/oil_gas_emissions.csv"), sep = ",", row.names = FALSE, append = FALSE)
      write.table(oil_gas_emissions_state, paste0(results_path, "/oil_gas_emissions_state.csv"), sep = ",", row.names = FALSE, append = FALSE)
      write.table(oil_gas_emissions_county, paste0(results_path, "/oil_gas_emissions_county.csv"), sep = ",", row.names = FALSE, append = FALSE)
    } else {
      write.table(oil_gas_emissions, paste0(results_path, "/oil_gas_emissions.csv"), sep = ",", row.names = FALSE, append = TRUE, col.names = FALSE)
      write.table(oil_gas_emissions_state, paste0(results_path, "/oil_gas_emissions_state.csv"), sep = ",", row.names = FALSE, append = TRUE, col.names = FALSE)
      write.table(oil_gas_emissions_county, paste0(results_path, "/oil_gas_emissions_county.csv"), sep = ",", row.names = FALSE, append = TRUE, col.names = FALSE)
    }

  }
  lapply(years, COBRA_files_generation)
  
  # Call the population data function to generate input files for COBRA
  print("Generating the population datafiles for COBRA")
  #do.call(COBRA_population_data_generation_f, list())
  #Script to run COBRA using windows cmd
  # Generate .bat files //Add the correct year to the files and generate baseline and scenario files for each year // Add corresponding population data
  dir.create(paste0(results_path, "/COBRA_results"))
  if (calculation_mode == "default" | calculation_mode == "all") {
    print("Generating batch files for COBRA - Default mode")
    for (i in years) {
      executable_path <- '"C:/Program Files/COBRA/cobra_console.exe"'
      db_path <- '"C:/Program Files/COBRA/data/cobra.db"'
      A <- paste0(results_path, "/COBRA_baseline_files/COBRA_emissions_baseline_", i, ".csv")
      B <- paste0(results_path, "/COBRA_scenario_files/COBRA_emissions_scenario_", i, ".csv")
      C <- paste0(getwd(), "/inputs/air_quality/cobra_population/COBRA_population_data_", i, ".csv")
      D <- paste0(getwd(), "/inputs/air_quality/COBRA_mortality_baseline/", "default_", i, "_incidence_data.csv")
      E <- paste0(results_path, "/COBRA_results/COBRA_results_", i, ".csv")
      F <- "YES"
      G <- 2023
      if (i == (first_proj_yr+2)) {
        command <- paste0(executable_path, ' -d ', db_path, ' -b', A, ' -c ', B, ' ', C, ' ', D, ' -o ', E, ' --pct3 ', F, ' -y ', G, "\n")
      } else {
        command <- paste0(command, executable_path, ' -d ', db_path, ' -b', A, ' -c ', B, ' ', C, ' ', D, ' -o ', E, ' --pct3 ', F, ' -y ', G, "\n")
      }
      path <- paste0(results_path, "/COBRA_batch.bat")
      writeLines(command, path, sep = "\n")
    }
    path <- paste0(results_path, "/COBRA_batch.bat")
    print("Starting COBRA - default mode calculation")
    shell(path, wait = TRUE)
  }
  print("Calculating the valuation of the health outcomes")
  do.call(valuation_health_outcomes_f, list(emissions_battery_production = emissions_battery_production))
}
