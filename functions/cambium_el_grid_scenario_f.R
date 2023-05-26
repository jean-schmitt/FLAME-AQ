cambium_el_grid_scenario_f <- function(output, elec_mix_mdl=NA, first_yr=NA, last_yr=NA, first_proj_yr = NA) {
  attribute_f("cambium_el_grid_scenario_f")
  if (tolower(elec_mix_mdl) == "current_mix") {
    data <- filter(get_input_f(input_name = "NREL_elec_intermediate"), t == "2022")
  } else {
    data <- get_input_f(input_name = paste0(elec_mix_mdl)) #paste0(strsplit(as.character(elec_mix_mdl), "_")[[1]][1], "_elec_", strsplit(as.character(elec_mix_mdl), "_")[[1]][2]))
  }
  if (length(grep("o.g.s_MWh", colnames(data))!=0)) {
    colnames(data)[grep("o.g.s_MWh", colnames(data))] <- "o-g-s_MWh"
  }
  grid_mix_data <- data.frame(data[,1]) %>%
    add_column("Years" = data$t) %>%
    add_column("Generation" = data$generation)
  colnames(grid_mix_data)[1] <- "Region"
  grid_mix_data <- cbind(grid_mix_data, data[,grep("MWh", colnames(data))])
  emission_sources <- c("Coal", "Natural Gas", "Nuclear", "Other", "Renewable")
  years <- first_yr:max(grid_mix_data$Years)
  regions <- unique(grid_mix_data$Region)
  m <- 0
  # Calculation of the relative natural gas demand for electricity production
  if (output == "ng") {
    ng_demand_electricity <- data.frame(matrix(data = NA, nrow = 0, ncol = 2))
    colnames(ng_demand_electricity) <- c("Year", "Total_generation")
    for (i in first_proj_yr:max(grid_mix_data$Years)) {
      temp <- which(grid_mix_data$Years == i)
      value <- sum(grid_mix_data[temp, grep("gas", colnames(grid_mix_data))])
      ng_demand_electricity[nrow(ng_demand_electricity)+1,] <- c(i, value)
    }
    years_interpolation <- as.numeric(unique(ng_demand_electricity$Year)[which(!unique(ng_demand_electricity$Year)%in%unique(grid_mix_data$Years))])
    for (i in years_interpolation) {
      years_data <- years[!years%in%years_interpolation]
      if (i <= min(grid_mix_data$Years)) {
        ng_demand_electricity$Total_generation[which(ng_demand_electricity$Year == i)] <- ng_demand_electricity$Total_generation[which(ng_demand_electricity$Year == min(grid_mix_data$Years))]
      } else {
        year_before <- max(years_data[years_data <i])
        year_after <- min(years_data[years_data >i])
        delta_year <- year_after-year_before
        a <- (ng_demand_electricity$Total_generation[which(ng_demand_electricity$Year == year_after)]- ng_demand_electricity$Total_generation[which(ng_demand_electricity$Year == year_before)])/(year_after-year_before)
        ng_demand_electricity$Total_generation[which(ng_demand_electricity$Year == i)] <- ng_demand_electricity$Total_generation[which(ng_demand_electricity$Year == year_before)]+a*(i-year_before)
      }
    }
    ng_demand_electricity <- add_column(ng_demand_electricity, "Relative_demand" = NA)
    for (i in 1:dim(ng_demand_electricity)[1]) {
      if (i == 1) {
        ng_demand_electricity$Relative_demand[i] <- 1
      } else {
        ng_demand_electricity$Relative_demand[i] <- ng_demand_electricity$Total_generation[i]/ng_demand_electricity$Total_generation[i-1]
      }
    }
    if (max(ng_demand_electricity$Year) < last_yr) {
      for (i in (max(ng_demand_electricity$Year)+1):last_yr) {
        ng_demand_electricity[nrow(ng_demand_electricity)+1,] <- c(i, ng_demand_electricity$Total_generation[nrow(ng_demand_electricity)], ng_demand_electricity$Relative_demand[nrow(ng_demand_electricity)])

      }
    }
    ng_demand_electricity <- add_column(ng_demand_electricity, "Fuel" = "Natural_Gas")
    return(list(ng_demand_electricity = ng_demand_electricity))
  } else if (output == "electricity") {
    # Calculation of the grid mix dataset
    for (i in 1:length(years)) {
      if (years[i] >= min(grid_mix_data$Years)) {
        temp_us <- which(grid_mix_data$Years == years[i])
        if (length(temp_us) != 0) {
          total_generation <- sum(grid_mix_data$Generation[temp_us])
          total_coal <- sum(grid_mix_data[temp_us, grep("coal", colnames(grid_mix_data))])
          total_NG <- sum(grid_mix_data[temp_us, grep("gas", colnames(grid_mix_data))])
          total_nuclear <- sum(grid_mix_data[temp_us, grep("nuclear", colnames(grid_mix_data))])
          total_renewables <- sum(grid_mix_data$hydro_MWh[temp_us]+
            grid_mix_data$geothermal_MWh[temp_us]+
            rowSums(grid_mix_data[temp_us, grep("biomass", colnames(grid_mix_data))])+
            rowSums(grid_mix_data[temp_us, grep("wind", colnames(grid_mix_data))])+
            grid_mix_data$upv_MWh[temp_us]+
            grid_mix_data$distpv_MWh[temp_us]+
            rowSums(grid_mix_data[temp_us, grep("phs", colnames(grid_mix_data)), drop = FALSE])+
            rowSums(grid_mix_data[temp_us, grep("csp", colnames(grid_mix_data)), drop = FALSE])+
            grid_mix_data$canada_MWh[temp_us]+
            rowSums(grid_mix_data[temp_us, grep("battery", colnames(grid_mix_data))]))
          total_other <- sum(grid_mix_data$`o-g-s_MWh`[temp_us])
          fraction_coal <- total_coal/total_generation
          fraction_NG <- total_NG/total_generation
          fraction_nuclear <- total_nuclear/total_generation
          fraction_renewables <- total_renewables/total_generation
          fraction_other <- total_other/total_generation
        } else {
          fraction_coal <- 0
          fraction_NG <- 0
          fraction_nuclear <- 0
          fraction_renewables <- 0
          fraction_other <- 0
        }
        if (years[i] == min(grid_mix_data$Years)) {
          matrix_mix <- data.frame(years[i], "Coal", fraction_coal)
          matrix_mix[nrow(matrix_mix)+1,] <- c(years[i], "Natural Gas", fraction_NG)
          matrix_mix[nrow(matrix_mix)+1,] <- c(years[i], "Nuclear", fraction_nuclear)
          matrix_mix[nrow(matrix_mix)+1,] <- c(years[i], "Other", fraction_other)
          matrix_mix[nrow(matrix_mix)+1,] <- c(years[i], "Renewable", fraction_renewables)
          #matrix_mix[nrow(matrix_mix)+1,] <- c(years[i], "Canada", fraction_canada)
        } else {
          matrix_mix[nrow(matrix_mix)+1,] <- c(years[i], "Coal", fraction_coal)
          matrix_mix[nrow(matrix_mix)+1,] <- c(years[i], "Natural Gas", fraction_NG)
          matrix_mix[nrow(matrix_mix)+1,] <- c(years[i], "Nuclear", fraction_nuclear)
          matrix_mix[nrow(matrix_mix)+1,] <- c(years[i], "Other", fraction_other)
          matrix_mix[nrow(matrix_mix)+1,] <- c(years[i], "Renewable", fraction_renewables)
          #matrix_mix[nrow(matrix_mix)+1,] <- c(years[i], "Canada", fraction_canada)
        }
      }
      for (j in 1:length(regions)) {
        temp <- which(grid_mix_data$Years == years[i] & grid_mix_data$Region == regions[j])
        if (length(temp) != 0) {
          total_generation <- grid_mix_data$Generation[temp]
          total_coal <- sum(grid_mix_data[temp, grep("coal", colnames(grid_mix_data))])
          total_NG <- sum(grid_mix_data[temp, grep("gas", colnames(grid_mix_data))])
          total_nuclear <- sum(grid_mix_data[temp, grep("nuclear", colnames(grid_mix_data))])
          total_renewables <- grid_mix_data$hydro_MWh[temp]+
            grid_mix_data$geothermal_MWh[temp]+
            sum(grid_mix_data[temp, grep("biomass", colnames(grid_mix_data))])+
            sum(grid_mix_data[temp, grep("wind", colnames(grid_mix_data))])+
            grid_mix_data$upv_MWh[temp]+
            grid_mix_data$distpv_MWh[temp]+
            sum(grid_mix_data[temp, grep("phs", colnames(grid_mix_data))])+
            sum(grid_mix_data[temp, grep("csp", colnames(grid_mix_data))])+
            grid_mix_data$canada_MWh[temp]+
            sum(grid_mix_data[temp, grep("battery", colnames(grid_mix_data))])
          total_other <- grid_mix_data$`o-g-s_MWh`[temp]
          fraction_coal <- total_coal/total_generation
          fraction_NG <- total_NG/total_generation
          fraction_nuclear <- total_nuclear/total_generation
          fraction_renewables <- total_renewables/total_generation
          fraction_other <- total_other/total_generation
        } else {
          fraction_coal <- 0
          fraction_NG <- 0
          fraction_nuclear <- 0
          fraction_renewables <- 0
          fraction_other <- 0
        }
        if (m == 0) {
          reg_matrix_mix <- data.frame(years[i], regions[j], "Coal", fraction_coal)
          reg_matrix_mix[nrow(reg_matrix_mix)+1,] <- c(years[i], regions[j], "Natural Gas", fraction_NG)
          reg_matrix_mix[nrow(reg_matrix_mix)+1,] <- c(years[i], regions[j], "Nuclear", fraction_nuclear)
          reg_matrix_mix[nrow(reg_matrix_mix)+1,] <- c(years[i], regions[j], "Other", fraction_other)
          reg_matrix_mix[nrow(reg_matrix_mix)+1,] <- c(years[i], regions[j], "Renewable", fraction_renewables)
          #reg_matrix_mix[nrow(reg_matrix_mix)+1,] <- c(years[i], regions[j], "Canada", fraction_canada)
        } else {
          reg_matrix_mix[nrow(reg_matrix_mix)+1,] <- c(years[i], regions[j], "Coal", fraction_coal)
          reg_matrix_mix[nrow(reg_matrix_mix)+1,] <- c(years[i], regions[j], "Natural Gas", fraction_NG)
          reg_matrix_mix[nrow(reg_matrix_mix)+1,] <- c(years[i], regions[j], "Nuclear", fraction_nuclear)
          reg_matrix_mix[nrow(reg_matrix_mix)+1,] <- c(years[i], regions[j], "Other", fraction_other)
          reg_matrix_mix[nrow(reg_matrix_mix)+1,] <- c(years[i], regions[j], "Renewable", fraction_renewables)
          #reg_matrix_mix[nrow(reg_matrix_mix)+1,] <- c(years[i], regions[j], "Canada", fraction_canada)
        }
        m <- m+1
      }
    }
    colnames(reg_matrix_mix) <- c("Year", "Region", "Source", "Value")
    colnames(matrix_mix) <- c("Year", "Source", "Value")
    reg_matrix_mix$Year <- as.numeric(reg_matrix_mix$Year)
    reg_matrix_mix$Value <- as.numeric(reg_matrix_mix$Value)
    matrix_mix$Year <- as.numeric(matrix_mix$Year)
    matrix_mix$Value <- as.numeric(matrix_mix$Value)
    years_interpolation <- as.numeric(unique(reg_matrix_mix$Year)[which(!unique(reg_matrix_mix$Year)%in%unique(grid_mix_data$Years))])
    for (i in years_interpolation) {
      years_data <- years[!years%in%years_interpolation]
      if (i <= min(grid_mix_data$Years)) {
        for (j in emission_sources) {
          for (k in regions) {
            reg_matrix_mix$Value[which(reg_matrix_mix$Year == i & reg_matrix_mix$Source == j & reg_matrix_mix$Region == k)] <- reg_matrix_mix$Value[which(reg_matrix_mix$Year == min(grid_mix_data$Years) & reg_matrix_mix$Source == j & reg_matrix_mix$Region == k)]
           }
        }
      } else {
        year_before <- max(years_data[years_data <i])
        year_after <- min(years_data[years_data >i])
        delta_year <- year_after-year_before
        for (j in emission_sources) {
          a <- (matrix_mix$Value[which(matrix_mix$Year == year_after & matrix_mix$Source == j)]- matrix_mix$Value[which(matrix_mix$Year == year_before & matrix_mix$Source == j)])/(year_after-year_before)
          matrix_mix$Value[which(matrix_mix$Year == i & matrix_mix$Source == j)] <- matrix_mix$Value[which(matrix_mix$Year == year_before & matrix_mix$Source == j)]+a*(i-year_before)
          for (k in regions) {
            a <- (reg_matrix_mix$Value[which(reg_matrix_mix$Year == year_after & reg_matrix_mix$Source == j & reg_matrix_mix$Region == k)]- reg_matrix_mix$Value[which(reg_matrix_mix$Year == year_before & reg_matrix_mix$Source == j & reg_matrix_mix$Region == k)])/(year_after-year_before)
            reg_matrix_mix$Value[which(reg_matrix_mix$Year == i & reg_matrix_mix$Source == j & reg_matrix_mix$Region == k)] <- reg_matrix_mix$Value[which(reg_matrix_mix$Year == year_before & reg_matrix_mix$Source == j & reg_matrix_mix$Region == k)]+a*(i-year_before)
          }
        }
      }
    }
    if (tolower(elec_mix_mdl) == "current_mix") {
      years <- (max(reg_matrix_mix$Year)+1):last_yr
      for (i in years) {
        temp_reg <- reg_matrix_mix[which(reg_matrix_mix$Year == i-1),]
        temp_reg$Year <- i
        reg_matrix_mix <- rbind(reg_matrix_mix, temp_reg)
        temp <- matrix_mix[which(matrix_mix$Year == i-1),]
        temp$Year <- i
        matrix_mix <- rbind(matrix_mix, temp)
      }
      rownames(reg_matrix_mix) <- NULL
      rownames(matrix_mix) <- NULL
    }
    if (strsplit(as.character(elec_mix_mdl), "_")[[1]][1] == "NREL" | elec_mix_mdl == "current_mix") {
      # DC is not included in the dataset, it will be added manually based on the grid mix of the Delaware
      data_DC <- reg_matrix_mix[which(reg_matrix_mix$Region == "DE"),]
      data_DC$Region <- "DC"
      reg_matrix_mix <- rbind(reg_matrix_mix, data_DC)
      # Standard scenarios refer to states instead of GEA regions; the regionalized mix is converted from states to GEA regions (states -> counties -> GEA regions)
      GIS_matching_matrix <- read.csv(paste0(getwd(), "/inputs/air_quality/GIS_matching_matrix.csv"))
      county_elec_prod <- read.csv(paste0(getwd(), "/inputs/air_quality/electricity_production_county.csv"))
      years <- unique(reg_matrix_mix$Year)
      m <- 0
      n <- 0
      for (i in 1:length(unique(GIS_matching_matrix$CAMBIUM_GEA))) {
        counties <- GIS_matching_matrix$FIPS[which(GIS_matching_matrix$CAMBIUM_GEA == unique(GIS_matching_matrix$CAMBIUM_GEA)[i])]
        mix_matrix <- matrix(0, nrow=length(unique(reg_matrix_mix$Source)), ncol=length(counties))
        rownames(mix_matrix) <- unique(reg_matrix_mix$Source)
        colnames(mix_matrix) <- counties
        weight_matrix <- matrix(0, nrow=length(counties), ncol = 1)
        rownames(weight_matrix) <- counties
        for (j in 1:length(years)) {
            for (l in 1:length(counties)) {
              state <- GIS_matching_matrix$States_Abbreviation[which(GIS_matching_matrix$FIPS == counties[l])]
              mix_matrix[,l] <- reg_matrix_mix$Value[which(reg_matrix_mix$Year == years[j] & reg_matrix_mix$Region == state)]
              if (length(which(county_elec_prod$FIPS == counties[l])) != 0) {
                weight_matrix[l] <- county_elec_prod$Fraction[which(county_elec_prod$FIPS == counties[l])]
              } else {
                weight_matrix[l] <- 0
              }
          }
          if (m == 0) {
            reg_matrix_mix_temp <- data.frame(years[j], unique(GIS_matching_matrix$CAMBIUM_GEA)[i], unique(reg_matrix_mix$Source), mix_matrix%*%weight_matrix)
          } else {
            reg_matrix_mix_temp <- rbind(reg_matrix_mix_temp, data.frame(years[j], unique(GIS_matching_matrix$CAMBIUM_GEA)[i], unique(reg_matrix_mix$Source), mix_matrix%*%weight_matrix))
          }
          m <- m+1
        }
        if (n == 0) {
          reg_matrix_mix_out <- reg_matrix_mix_temp
        } else {
          reg_matrix_mix_out <- rbind(reg_matrix_mix_out, reg_matrix_mix_temp)
        }
        m <- 0
        n <- n+1
      }
      colnames(reg_matrix_mix_out) <- c("Year", "Region", "Source", "Value")
      rownames(reg_matrix_mix_out) <- NULL
      reg_matrix_mix <- reg_matrix_mix_out
    }
    return(list(matrix_mix = matrix_mix, reg_matrix_mix = reg_matrix_mix)) 
  } else if (output == "total_emissions_electricity") {
    total_electricity_production <- data.frame(matrix(data = NA, nrow = 0, ncol = 3))
    colnames(total_electricity_production) <- c("Year", "Electricity_production", "Unit")
    for (i in years) {
      if (i %in% grid_mix_data$Years) {
        total_electricity_production[nrow(total_electricity_production)+1,] <- c(i, sum(grid_mix_data[which(grid_mix_data$Years == i), grep("MWh", colnames(grid_mix_data))])*1000, "kWh")
      } else if (i < min(grid_mix_data$Years)) {
        total_electricity_production[nrow(total_electricity_production)+1,] <- c(i, sum(grid_mix_data[which(grid_mix_data$Years == min(grid_mix_data$Years)), grep("MWh", colnames(grid_mix_data))])*1000, "kWh")
      } else {
        year_before <- grid_mix_data$Years[which((unique(grid_mix_data$Years)-i) == -min(abs(unique(grid_mix_data$Years) - i)))]
        year_after <- grid_mix_data$Years[which((unique(grid_mix_data$Years)-i) == min(abs(unique(grid_mix_data$Years) - i)))]
        nbr_years <- year_after-year_before
        a <- (sum(grid_mix_data[which(grid_mix_data$Years == year_after), grep("MWh", colnames(grid_mix_data))])*1000-sum(grid_mix_data[which(grid_mix_data$Years == year_before), grep("MWh", colnames(grid_mix_data))])*1000)/(year_after-year_before)
        production <- sum(grid_mix_data[which(grid_mix_data$Years == year_before), grep("MWh", colnames(grid_mix_data))])*1000+a*(i-year_before)
        total_electricity_production[nrow(total_electricity_production)+1,] <- c(i, production, "kWh")
      }
    }
    return(list(total_electricity_production = total_electricity_production))
  }
}
