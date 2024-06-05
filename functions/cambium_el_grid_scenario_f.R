cambium_el_grid_scenario_f <- function(output, elec_mix_mdl, first_yr=NA, last_yr=NA, first_proj_yr = NA, include_biomass = NA, grid_shift_year = NA, scenario_id = NA) {
  attribute_f("cambium_el_grid_scenario_f")
  first_proj_yr <- 2022
  results_path <- paste0(getwd(), "/outputs/air_quality/", Sys.Date(), "_", scenario_id, "_results")
  sources <- c("Coal", "Natural Gas", "Nuclear", "Oil", "Renewable", "Biomass")
  ## Open the electricity mix scenario and calculate the regional mix
  if (grepl("AEO", elec_mix_mdl)) {
    ## Load the 2022 scenario from NREL
    scenario_elec_mix <- filter(get_input_f(input_name = "CAMBIUM_2021_mid_case"), t==2022)
    years <- min(scenario_elec_mix$t):last_yr
    fun <- function(i) {
      scenario_elec_mix$t <- i
      return(scenario_elec_mix)
    }
    scenario_elec_mix <- do.call(rbind, lapply(years, fun)) %>%
      add_column("re-ct_MWh" = 0)
    ## Load the AEO scenario and calculate the relative change in each fuel type
    scenario_AEO <- get_input_f(input_name = elec_mix_mdl)
    row_id <- c("Coal", "Natural Gas", "Petroleum", "Nuclear Power", "Renewable Sources")
    scenario_AEO <- scenario_AEO[which(scenario_AEO$Source%in%row_id),which(colnames(scenario_AEO)%in%c("Source", first_yr:last_yr))]
    scenario_AEO$Source <- c("Coal", "Oil", "Natural Gas", "Nuclear", "Renewable")
    scenario_AEO <- pivot_longer(scenario_AEO, cols = 2:length(colnames(scenario_AEO)), names_to = "Year", values_to = "Value")
    #scenario_AEO <- left_join(scenario_AEO, aggregate(.~Year, data = select(scenario_AEO, -c("Source")), FUN = sum), by = c("Year"))
    #scenario_AEO <- left_join(scenario_AEO, select(scenario_AEO[which(scenario_AEO$Year == 2022),], -c("Year")), by = c("Source"))
    #scenario_AEO$Value.x <- scenario_AEO$Value.x/scenario_AEO$Value.y
    #scenario_AEO <- select(scenario_AEO, -c("Value.y"))
    colnames(scenario_AEO) <- c("Source", "t", "Prod_AEO")
    scenario_AEO$t <- as.numeric(scenario_AEO$t)
    scenario_elec_mix <- get_input_f(input_name = "CAMBIUM_2021_mid_case")
  } else if (grepl("EFS", elec_mix_mdl)) {
    ## Load the 2022 scenario from NREL
    scenario_elec_mix <- filter(get_input_f(input_name = "CAMBIUM_2021_mid_case"), t==2022)
    years <- min(scenario_elec_mix$t):last_yr
    fun <- function(i) {
      scenario_elec_mix$t <- i
      return(scenario_elec_mix)
    }
    scenario_elec_mix <- do.call(rbind, lapply(years, fun)) %>%
      add_column("re-ct_MWh" = 0)
    scenario_EFS <- get_input_f(input_name = elec_mix_mdl)
    scenario_EFS <- select(scenario_EFS, c("time", colnames(scenario_EFS)[grep("_TWh", colnames(scenario_EFS))])) %>%
      pivot_longer(cols = 2:17, names_to = "Source")
    scenario_EFS$Source[grep("Coal", scenario_EFS$Source)] <- "Coal"
    scenario_EFS$Source[grep("NG-", scenario_EFS$Source)] <- "Natural Gas"
    scenario_EFS$Source[grep("Nuclear", scenario_EFS$Source)] <- "Nuclear"
    scenario_EFS$Source[grep("Oil", scenario_EFS$Source)] <- "Oil"
    if (include_biomass == "yes") {
      scenario_EFS$Source[grep("Biopower", scenario_EFS$Source)] <- "Biomass"
    }
    scenario_EFS$Source[grep("_TWh", scenario_EFS$Source)] <- "Renewable"
    scenario_EFS <- aggregate(.~time+Source, data = scenario_EFS, FUN = sum)
    colnames(scenario_EFS) <- c("t", "Source", "Prod_AEO")
    scenario_AEO <- expand.grid(Source = unique(scenario_EFS$Source), t = first_proj_yr:last_yr) %>%
      left_join(scenario_EFS, by = c("t", "Source")) %>%
      arrange(Source, t)
    scenario_AEO$Prod_AEO <- na.approx(scenario_AEO$Prod_AEO)
    scenario_elec_mix <- get_input_f(input_name = "CAMBIUM_2021_mid_case")
    elec_mix_mdl <- "AEO"
  } else if (tolower(elec_mix_mdl) == "current_mix") {
    #scenario_elec_mix <- filter(get_input_f(input_name = "NREL_elec_intermediate"), t==2022)
    scenario_elec_mix <- filter(get_input_f(input_name = "CAMBIUM_2021_mid_case"), t==2022)
    years <- min(scenario_elec_mix$t):last_yr
    fun <- function(i) {
      scenario_elec_mix$t <- i
      return(scenario_elec_mix)
    }
    scenario_elec_mix <- do.call(rbind, lapply(years, fun)) %>%
      add_column("re-ct_MWh" = 0)
    #elec_mix_mdl <- "NREL_elec_intermediate"
    elec_mix_mdl <- "CAMBIUM_2021_mid_case"
  } else if (tolower(elec_mix_mdl) == "2014_mix") {
    scenario_elec_mix <- filter(get_input_f(input_name = "Mid_Case_2014"), t==2014) %>%
      select(-c("country"))
    years <- min(scenario_elec_mix$t):last_yr
    fun <- function(i) {
      scenario_elec_mix$t <- i
      return(scenario_elec_mix)
    }
    scenario_elec_mix <- do.call(rbind, lapply(years, fun)) %>%
      add_column("re-ct_MWh" = 0)
    elec_mix_mdl <- "NREL_elec_intermediate"
    colnames(scenario_elec_mix)[grep("generation", colnames(scenario_elec_mix))] <- "generation"
  } else if (elec_mix_mdl == "CAMBIUM_Delta") {
    scenario_elec_mix <- get_input_f(input_name = "CAMBIUM_mid_case")
  } else if (elec_mix_mdl == "all_NG" | elec_mix_mdl == "all_RE") {
    scenario_elec_mix <- filter(get_input_f(input_name = "CAMBIUM_2021_mid_case"), t==2022)
    years <- min(scenario_elec_mix$t):last_yr
    fun <- function(i) {
      scenario_elec_mix$t <- i
      return(scenario_elec_mix)
    }
    scenario_elec_mix <- do.call(rbind, lapply(years, fun)) %>%
      add_column("re-ct_MWh" = 0)
    elec_mix_mdl <- paste0("CAMBIUM_", elec_mix_mdl)
  } else {
    scenario_elec_mix <- get_input_f(input_name = paste0(elec_mix_mdl))
  }
  ## Load the 2022 data as the starting mix
  if (!grepl("NREL", elec_mix_mdl)) {
    scenario_elec_mix_2022 <- filter(get_input_f(input_name = "CAMBIUM_2022_mid-case"), t == 2022)
  }
  ## Calculation of the predicted generation share by state or GEA
  if (grepl("NREL", elec_mix_mdl)) {
    share_by_state <- select(scenario_elec_mix, c("state", "t", "generation"))
    generation_yearly <- aggregate(generation ~ t, data = select(share_by_state, -c("state")), FUN = sum)
    share_by_state <- left_join(share_by_state, generation_yearly, by = "t")
    share_by_state$generation.x <- share_by_state$generation.x/share_by_state$generation.y
    share_by_state <- select(share_by_state, -c("generation.y"))
    colnames(share_by_state) <- c("State.Abbr", "Year", "Share")
    if (min(share_by_state$Year) > first_proj_yr) {
      first_year <- share_by_state[which(share_by_state$Year == min(share_by_state$Year)),]
      first_year$Year <- first_proj_yr
      share_by_state <- rbind(share_by_state, first_year)
    }
    years <- first_proj_yr:max(share_by_state$Year)
    reg_yearly_share <- expand.grid(years, unique(share_by_state$State.Abbr))
    colnames(reg_yearly_share) <- c("Year", "State.Abbr")
    reg_yearly_share <- left_join(reg_yearly_share, share_by_state, by = c("Year", "State.Abbr"))
    reg_yearly_share$Share <- na.approx(reg_yearly_share$Share)
  } else if (grepl("CAMBIUM", elec_mix_mdl) | grepl("AEO", elec_mix_mdl)) {
    share_by_GEA <- select(scenario_elec_mix, c("gea", "t", "generation"))
    generation_yearly <- aggregate(generation ~ t, data = select(share_by_GEA, -c("gea")), FUN = sum)
    share_by_GEA <- left_join(share_by_GEA, generation_yearly, by = "t")
    share_by_GEA$generation.x <- share_by_GEA$generation.x/share_by_GEA$generation.y
    share_by_GEA <- select(share_by_GEA, -c("generation.y"))
    colnames(share_by_GEA) <- c("Cambium.GEA", "Year", "Share")
    if (min(share_by_GEA$Year) > first_proj_yr) {
      first_year <- share_by_GEA[which(share_by_GEA$Year == min(share_by_GEA$Year)),]
      first_year$Year <- first_proj_yr
      share_by_GEA <- rbind(share_by_GEA, first_year)
    }
    years <- first_proj_yr:max(share_by_GEA$Year)
    reg_yearly_share <- expand.grid(years, unique(share_by_GEA$Cambium.GEA))
    colnames(reg_yearly_share) <- c("Year", "Cambium.GEA")
    reg_yearly_share <- left_join(reg_yearly_share, share_by_GEA, by = c("Year", "Cambium.GEA"))
    reg_yearly_share$Share <- na.approx(reg_yearly_share$Share)
  }
  
  ## Calculation of the electricity mix
  scenario_elec_mix <- scenario_elec_mix[,c(1,2,grep("MWh",colnames(scenario_elec_mix)))]
  if (!grepl("NREL", elec_mix_mdl)) {
    scenario_elec_mix_2022 <- scenario_elec_mix_2022[,c(1,2,grep("MWh",colnames(scenario_elec_mix_2022)))]
    scenario_elec_mix <- bind_rows(scenario_elec_mix, scenario_elec_mix_2022)
  }
  scenario_elec_mix <- pivot_longer(scenario_elec_mix, cols = grep("MWh", colnames(scenario_elec_mix)), names_to = "Type", values_to = "Generation") %>%
    add_column("Source" = NA)
  scenario_elec_mix$Source[which(scenario_elec_mix$Type%in%c("biomass_MWh", "beccs_MWh", "biomass-ccs_MWh", "biomass_re_ct_MWh"))] <- "Biomass"
  scenario_elec_mix$Source[which(scenario_elec_mix$Type%in%c("coal_MWh", "coal-ccs_MWh", "coal_ccs_MWh"))] <- "Coal"
  scenario_elec_mix$Source[which(scenario_elec_mix$Type%in%c("gas-cc_MWh", "gas-cc-ccs_MWh", "gas-ct_MWh", "gas_cc_ccs_MWh", "gas_cc_MWh", "gas_ct_MWh"))] <- "Natural Gas"
  scenario_elec_mix$Source[which(scenario_elec_mix$Type%in%c("nuclear_MWh", "nuclear_smr_MWh"))] <- "Nuclear"
  scenario_elec_mix$Source[which(scenario_elec_mix$Type%in%c("o-g-s_MWh"))] <- "Oil"
  scenario_elec_mix$Source[which(scenario_elec_mix$Type%in%c("battery_2_MWh", "battery_4_MWh", "battery_6_MWh", "battery_8_MWh", "battery_10_MWh", "battery_pv_MWh", "battery_MWh", "canada_MWh", "csp_MWh", "distpv_MWh", "geothermal_MWh", "hydro_MWh", "phs_MWh", "re-ct_MWh", "upv_MWh", "wind-ons_MWh", "wind_ons_MWh", "wind-ofs_MWh", "wind_ofs_MWh", "battery_energy_cap_MWh", "phs_energy_cap_MWh", "csp_energy_cap_MWh", "upv_MWh", "curtailment_MWh"))] <- "Renewable"
  if (include_biomass == "no") {
    scenario_elec_mix$Source[which(scenario_elec_mix$Source == "Biomass")] <- "Renewable"
  }
  scenario_elec_mix <- select(scenario_elec_mix, -c("Type"))
  if (grepl("AEO", elec_mix_mdl)) {
    # Distribution by GEA
    scenario_elec_mix <- left_join(filter(scenario_elec_mix, scenario_elec_mix$t == 2022), aggregate(.~t+Source, data = select(filter(scenario_elec_mix, scenario_elec_mix$t == 2022), -c("gea")), FUN = sum), by = c("t", "Source")) 
    years <- min(scenario_elec_mix$t):last_yr
    fun <- function(i) {
      scenario_elec_mix$t <- i
      return(scenario_elec_mix)
    }
    scenario_elec_mix <- do.call(rbind, lapply(years, fun))
    scenario_elec_mix$Generation.x <- scenario_elec_mix$Generation.x/scenario_elec_mix$Generation.y
    scenario_elec_mix <- select(scenario_elec_mix, -c("Generation.y"))
    colnames(scenario_elec_mix) <- c("gea", "t", "GEA_fraction", "Source")
    scenario_elec_mix <- left_join(scenario_elec_mix, scenario_AEO, by = c("t", "Source"))
    scenario_elec_mix$Generation <- scenario_elec_mix$GEA_fraction*scenario_elec_mix$Prod_AEO
    scenario_elec_mix <- select(scenario_elec_mix, -c("GEA_fraction", "Prod_AEO"))
    #scenario_elec_mix <- filter(scenario_elec_mix, scenario_elec_mix$t == years[1])
    #aeo_scenario <- function(i) {
    #  temp <- scenario_elec_mix
    #  temp$t <- i
    #  scenario_elec_mix <- rbind(scenario_elec_mix, temp)
    #}
    #scenario_elec_mix <- do.call(rbind, lapply(years[2:length(years)], aeo_scenario))
    #scenario_elec_mix <- left_join(scenario_elec_mix, scenario_AEO, by = c("t", "Source"))
    #scenario_elec_mix$Generation <- scenario_elec_mix$Generation*scenario_elec_mix$Fraction_AEO
    #scenario_elec_mix <- select(scenario_elec_mix, -c("Fraction_AEO"))
  }
  if (elec_mix_mdl == "CAMBIUM_Delta") {
    scenario_high_elec <- get_input_f(input_name = "CAMBIUM_high_electrification")
    scenario_high_elec <- scenario_high_elec[,c(1,2,grep("MWh",colnames(scenario_high_elec)))]
    scenario_high_elec <- bind_rows(scenario_high_elec, scenario_elec_mix_2022)
    scenario_high_elec <- pivot_longer(scenario_high_elec, cols = grep("MWh", colnames(scenario_high_elec)), names_to = "Type", values_to = "Generation") %>%
      add_column("Source" = NA)
    scenario_high_elec$Source[which(scenario_high_elec$Type%in%c("biomass_MWh", "beccs_MWh", "biomass-ccs_MWh", "biomass_re_ct_MWh"))] <- "Biomass"
    scenario_high_elec$Source[which(scenario_high_elec$Type%in%c("coal_MWh", "coal-ccs_MWh", "coal_ccs_MWh"))] <- "Coal"
    scenario_high_elec$Source[which(scenario_high_elec$Type%in%c("gas-cc_MWh", "gas-cc-ccs_MWh", "gas-ct_MWh", "gas_cc_ccs_MWh", "gas_cc_MWh", "gas_ct_MWh"))] <- "Natural Gas"
    scenario_high_elec$Source[which(scenario_high_elec$Type%in%c("nuclear_MWh", "nuclear_smr_MWh"))] <- "Nuclear"
    scenario_high_elec$Source[which(scenario_high_elec$Type%in%c("o-g-s_MWh"))] <- "Oil"
    scenario_high_elec$Source[which(scenario_high_elec$Type%in%c("battery_2_MWh", "battery_4_MWh", "battery_6_MWh", "battery_8_MWh", "battery_10_MWh", "battery_pv_MWh", "battery_MWh", "canada_MWh", "csp_MWh", "distpv_MWh", "geothermal_MWh", "hydro_MWh", "phs_MWh", "re-ct_MWh", "upv_MWh", "wind-ons_MWh", "wind_ons_MWh", "wind-ofs_MWh", "wind_ofs_MWh", "battery_energy_cap_MWh", "phs_energy_cap_MWh", "csp_energy_cap_MWh", "upv_MWh"))] <- "Renewable"
    if (include_biomass == "no") {
      scenario_high_elec$Source[which(scenario_high_elec$Source == "Biomass")] <- "Renewable"
    }
    scenario_high_elec <- select(scenario_high_elec, -c("Type"))
    scenario_elec_mix$Generation[which(scenario_elec_mix$t == 2022)] <- 0
    
    scenario_elec_mix <- aggregate(.~gea+t+Source, data = scenario_elec_mix, FUN = sum)
    scenario_high_elec <- aggregate(.~gea+t+Source, data = scenario_high_elec, FUN = sum)
    colnames(scenario_elec_mix) <- c("Cambium.GEA", "Year", "Source", "Generation")
    colnames(scenario_high_elec) <- c("Cambium.GEA", "Year", "Source", "Generation_high_elec")
    scenario_elec_mix <- left_join(scenario_elec_mix, scenario_high_elec, by = c("Cambium.GEA", "Year", "Source"))
    scenario_elec_mix$Delta <- scenario_elec_mix$Generation_high_elec-scenario_elec_mix$Generation
    scenario_elec_mix <- aggregate(.~Cambium.GEA+Year+Source, data = select(scenario_elec_mix, -c("Generation", "Generation_high_elec")), FUN = sum)
    scenario_elec_mix$Delta[scenario_elec_mix$Delta<0] <- 0
    colnames(scenario_elec_mix)[4] <- "Generation"
    GEA_generation <- aggregate(.~ Cambium.GEA+Year, data = select(scenario_elec_mix, -c("Source")), FUN = sum)
    scenario_elec_mix <- left_join(scenario_elec_mix, GEA_generation, by = c("Cambium.GEA", "Year"))
    #scenario_elec_mix[which(scenario_elec_mix$Year == 2022),] <- scenario_high_elec[which(scenario_high_elec$Year == 2022),]
  } else if (grepl("NREL", elec_mix_mdl)) {
    scenario_elec_mix <- aggregate(.~state+t+Source, data = scenario_elec_mix, FUN = sum)
    colnames(scenario_elec_mix) <- c("State.Abbr", "Year", "Source", "Generation")
    state_generation <- aggregate(.~ State.Abbr+Year, data = select(scenario_elec_mix, -c("Source")), FUN = sum)
    scenario_elec_mix <- left_join(scenario_elec_mix, state_generation, by = c("State.Abbr", "Year"))
  } else if (grepl("CAMBIUM", elec_mix_mdl) | grepl("AEO", elec_mix_mdl) | tolower(elec_mix_mdl) == "all_ng" | tolower(elec_mix_mdl) == "all_re") {
    scenario_elec_mix <- aggregate(.~gea+t+Source, data = scenario_elec_mix, FUN = sum)
    colnames(scenario_elec_mix) <- c("Cambium.GEA", "Year", "Source", "Generation")
    GEA_generation <- aggregate(.~ Cambium.GEA+Year, data = select(scenario_elec_mix, -c("Source")), FUN = sum)
    scenario_elec_mix <- left_join(scenario_elec_mix, GEA_generation, by = c("Cambium.GEA", "Year"))
  }
  scenario_elec_mix$Generation.x <- scenario_elec_mix$Generation.x/scenario_elec_mix$Generation.y
  scenario_elec_mix <- select(scenario_elec_mix, -c("Generation.y"))
  colnames(scenario_elec_mix)[4] <- "Fraction"
  #if (grepl("AEO", elec_mix_mdl)) {
  #  colnames(scenario_AEO)[which(colnames(scenario_AEO) == "t")] <- "Year"
  #  scenario_elec_mix <- left_join(scenario_elec_mix, scenario_AEO, by = c("Year", "Source")) %>%
  #    select(-c("Fraction"))
  #  colnames(scenario_elec_mix)[4] <- "Fraction"
  #}
  
  ## Apply the grid shift year
  if (grid_shift_year != 0) {
    scenario_elec_mix$Year <- scenario_elec_mix$Year + grid_shift_year
    # Alternative way to consider grid shift
    scenario_elec_mix <- filter(scenario_elec_mix, scenario_elec_mix$Year <= 2050)
    scenario_elec_mix <- rbind(scenario_elec_mix[which(scenario_elec_mix$Year == min(scenario_elec_mix$Year)),], scenario_elec_mix[which(scenario_elec_mix$Year == max(scenario_elec_mix$Year)),])
    scenario_elec_mix$Year[which(scenario_elec_mix$Year == min(scenario_elec_mix$Year))] <- first_proj_yr
    scenario_elec_mix$Year[which(scenario_elec_mix$Year == max(scenario_elec_mix$Year))] <- last_yr
  }
  
  ## Add first year
  if (min(scenario_elec_mix$Year) > first_proj_yr) {
    initial_year <- scenario_elec_mix[which(scenario_elec_mix$Year == min(unique(scenario_elec_mix$Year))),]
    initial_year$Year <- first_proj_yr
    scenario_elec_mix <- rbind(initial_year, scenario_elec_mix)
  }
  
  ## Linear interpolation for the missing years
  years <- first_proj_yr:max(last_yr, max(scenario_elec_mix$Year))
  if (grepl("NREL", elec_mix_mdl)) {
    grid_mix_reg <- expand.grid(years, unique(scenario_elec_mix$State.Abbr), unique(scenario_elec_mix$Source))
    colnames(grid_mix_reg) <- c("Year", "State.Abbr", "Source")
    grid_mix_reg <- left_join(grid_mix_reg, scenario_elec_mix, by = c("Year", "State.Abbr", "Source"))
  } else if (grepl("CAMBIUM", elec_mix_mdl) | grepl("AEO", elec_mix_mdl) | tolower(elec_mix_mdl) == "all_ng" | tolower(elec_mix_mdl) == "all_re") {
    grid_mix_reg <- expand.grid(years, unique(scenario_elec_mix$Cambium.GEA), unique(scenario_elec_mix$Source))
    colnames(grid_mix_reg) <- c("Year", "Cambium.GEA", "Source")
    grid_mix_reg <- left_join(grid_mix_reg, scenario_elec_mix, by = c("Year", "Cambium.GEA", "Source"))
  }
  grid_mix_reg$Fraction <- na.approx(grid_mix_reg$Fraction)
  grid_mix_reg <- filter(grid_mix_reg, grid_mix_reg$Year <= last_yr) 
    
  ## Calculation of the national grid mix
  if (grepl("NREL", elec_mix_mdl)) {
    grid_mix_national <- left_join(grid_mix_reg, reg_yearly_share, by = c("Year", "State.Abbr"), relationship = "many-to-many")
    grid_mix_national$Fraction <- grid_mix_national$Fraction*grid_mix_national$Share
    grid_mix_national <- aggregate(Fraction ~ Year+Source, data = select(grid_mix_national, -c("State.Abbr", "Share")), FUN = sum)
  } else if (grepl("CAMBIUM", elec_mix_mdl) | grepl("AEO", elec_mix_mdl) | tolower(elec_mix_mdl) == "all_ng" | tolower(elec_mix_mdl) == "all_re") {
    grid_mix_national <- left_join(grid_mix_reg, reg_yearly_share, by = c("Year", "Cambium.GEA"), relationship = "many-to-many")
    grid_mix_national$Fraction <- grid_mix_national$Fraction*grid_mix_national$Share
    grid_mix_national <- aggregate(Fraction ~ Year+Source, data = select(grid_mix_national, -c("Cambium.GEA", "Share")), FUN = sum)
  }
  
  ## Modify the mix for the no elec scenario
  if (grepl("no_elec", scenario_id)) {
    grid_mix_national$Fraction[which(grid_mix_national$Source == "Renewable")] <- 1
    grid_mix_national$Fraction[which(!grid_mix_national$Source == "Renewable")] <- 0
    grid_mix_reg$Fraction[which(grid_mix_reg$Source == "Renewable")] <- 1
    grid_mix_reg$Fraction[which(!grid_mix_reg$Source == "Renewable")] <- 0
  }
  
  ## Modify the mix for other specific scenarios
  if (grepl("all_NG", elec_mix_mdl)) {
    grid_mix_reg$Fraction <- 0
    grid_mix_reg$Fraction[which(grid_mix_reg$Source == "Natural Gas")] <- 1
    grid_mix_national$Fraction <- 0
    grid_mix_national$Fraction[which(grid_mix_national$Source == "Natural Gas")] <- 1
  } else if (grepl("all_RE", elec_mix_mdl)) {
    grid_mix_reg$Fraction <- 0
    grid_mix_reg$Fraction[which(grid_mix_reg$Source == "Renewable")] <- 1
    grid_mix_national$Fraction <- 0
    grid_mix_national$Fraction[which(grid_mix_national$Source == "Renewable")] <- 1
  }
  
  ## Export the grid mix data
  write.csv(grid_mix_reg, paste0(results_path, "/grid_mix_reg_", elec_mix_mdl, ".csv"), row.names = FALSE)
  write.csv(grid_mix_national, paste0(results_path, "/grid_mix_national_", elec_mix_mdl, ".csv"), row.names = FALSE)
    
  ## Return end results
  
  return(list(grid_mix_reg = grid_mix_reg,
              grid_mix_national = grid_mix_national,
              reg_yearly_share = reg_yearly_share))
}
