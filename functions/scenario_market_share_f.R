scenario_market_share_f <- function(technology_market_share, fleet_initial_year = NA, fleet_final_year = NA, first_proj_yr = NA, fleet_id = NA, scenario_id = NA) {
  attribute_f("fleet_composition_scenario_f")
  vehicles_sizes <- c("Car", "Light truck")
  scenario_entire_us <- fleet_read_scenario_f(fleet_id = NA)
  scenario_entire_us <- arrange(scenario_entire_us, scenario_entire_us$Year)
  for (i in 1:length(vehicles_sizes)) {
    scenario_entire_us$Size[which(scenario_entire_us$Size == tolower(vehicles_sizes[i]))] <- vehicles_sizes[i]
  }
  state_groups <- unique(scenario_entire_us$State)
  for (iterations in 1:length(state_groups)) {
    scenario <- scenario_entire_us[which(scenario_entire_us$State == state_groups[iterations] | scenario_entire_us$State == "all"),]
    market_share_rel <- data.frame(technology_market_share)
    colnames(market_share_rel) <- colnames(technology_market_share)
    size_tech <- 0
    default_sizes_distribution <- data.frame()
    # First step: Calculate the share of cars and light trucks in the passenger vehicles fleet
    for (i in 1:length(colnames(market_share_rel))) {
      for (j in 1:length(vehicles_sizes)) {
        default_sizes_distribution[j,i] <- sum(market_share_rel[which(grepl(vehicles_sizes[j], rownames(market_share_rel))), i])
      }
    }
    market_share_rel_temp <- market_share_rel
    market_share_rel_temp[,which(colnames(market_share_rel)>=fleet_initial_year)] <- NA
    colnames(default_sizes_distribution) <- colnames(market_share_rel)
    rownames(default_sizes_distribution) <- vehicles_sizes
    updated_size_distribution <- default_sizes_distribution
    updated_size_distribution[,which(colnames(updated_size_distribution)>=fleet_initial_year)] <- NA 
    scenario_market <- scenario[which(scenario$Event == "sales_share" | scenario$Event == "sales_ban"),]
    scenario_market$Value <- as.numeric(scenario_market$Value)
    years_scenario <- unique(scenario_market$Year)
    # Construction of the dataset for the market shares for different technologies
    tech_market_shares_temp <- filter(scenario, scenario$Event == "tech_share")
    if (dim(scenario_market)[1] != 0) {
      if (dim(tech_market_shares_temp)[1]!=0) {
        tech_market_shares <- data.frame()
        m<-1
        for (i in 1:dim(tech_market_shares_temp)[1]) {
          techs <- strsplit(tech_market_shares_temp$Technology, "_")[[1]]
          for (j in 1:length(techs)) {
            tech_market_shares[m,1] <- tech_market_shares_temp$Year[i]
            tech_market_shares[m,2] <- tech_market_shares_temp$Event[i]
            tech_market_shares[m,3] <- tech_market_shares_temp$Size[i]
            tech_market_shares[m,4] <- strsplit(tech_market_shares_temp$Technology[i], "_")[[1]][j]
            tech_market_shares[m,5] <- strsplit(tech_market_shares_temp$Value[i], "_")[[1]][j]
            tech_market_shares[m,6] <- tech_market_shares_temp$Interpolation_type[i]
            tech_market_shares[m,7] <- paste0(tech_market_shares_temp$Size[i], "_", strsplit(tech_market_shares_temp$Technology[i], "_")[[1]][j])
            m<-m+1
          }
        }
        colnames(tech_market_shares) <- c("Year", "Event", "Size", "Technology", "Value", "Interpolation", "Size_Tech")
        for (i in fleet_initial_year:fleet_final_year) {
          sizes <- unique(tech_market_shares$Size)
          for (k in 1:length(sizes)) {
            if (!(i%in%unique(filter(tech_market_shares, tech_market_shares$Size == sizes[k]))$Year)) {
              #technologies <- unique(tech_market_shares$Technology)
              technologies <- unique(tech_market_shares$Technology[which(tech_market_shares$Size == sizes[k])])
              for (j in 1:length(technologies)) {
                #sizes <- unique(tech_market_shares$Size[which(tech_market_shares$Technology == technologies[j])])
                #for (k in 1:length(sizes)) {
                temp1 <- tech_market_shares$Year[which(tech_market_shares$Year <= i & tech_market_shares$Technology == technologies[j] & tech_market_shares$Size == sizes[k])]
                temp2 <- tech_market_shares$Year[which(tech_market_shares$Year >= i & tech_market_shares$Technology == technologies[j] & tech_market_shares$Size == sizes[k])]
                if (length(temp1) != 0) {
                  year_interpolation_start <- as.numeric(max(temp1))
                } else {
                  year_interpolation_start <- fleet_initial_year
                }
                if (length(temp2) !=0) {
                  year_interpolation_end <- as.numeric(min(temp2))
                } else {
                  year_interpolation_end <- i
                }
                value_interpolation_start <- as.numeric(tech_market_shares$Value[which(tech_market_shares$Year == year_interpolation_start & tech_market_shares$Technology == technologies[j] & tech_market_shares$Size == sizes[k])])
                value_interpolation_end <- as.numeric(tech_market_shares$Value[which(tech_market_shares$Year == year_interpolation_end & tech_market_shares$Technology == technologies[j] & tech_market_shares$Size == sizes[k])])
                interpolation_type <- tech_market_shares$Interpolation[which(tech_market_shares$Year == year_interpolation_end & tech_market_shares$Technology == technologies[j] & tech_market_shares$Size == sizes[k])]
                if (year_interpolation_end == i) {
                  interpolation_type <- "linear"
                  tech_market_shares[nrow(tech_market_shares)+1,] <- c(i, "tech_share", sizes[k], technologies[j], value_interpolation_start, interpolation_type, paste0(sizes[k], "_", technologies[j]))
                } else if (year_interpolation_start == i) {
                  temp <- fleet_vint_stock$Value[which(fleet_vint_stock$Year == i & fleet_vint_stock$Age == 0 & fleet_vint_stock$Size == sizes[k] & fleet_vint_stock$Technology == technologies[j])]/sum(fleet_vint_stock$Value[which(fleet_vint_stock$Year == i & fleet_vint_stock$Age == 0 & fleet_vint_stock$Size == sizes[k] & fleet_vint_stock$Technology%in%technologies)])
                  tech_market_shares[nrow(tech_market_shares)+1,] <- c(i, "tech_share", sizes[k], technologies[j], temp, interpolation_type, paste0(sizes[k], "_", technologies[j]))
                } else if (interpolation_type != "erf") {
                  if (interpolation_type == "linear") {
                    FUN <- prod
                  } else if (interpolation_type == "sqrt") {
                    FUN <- sqrt
                  } else if (interpolation_type == "log10") {
                    FUN <- log10
                  } else if (interpolation_type == "exp") {
                    FUN <- exp
                  }
                  a <- (value_interpolation_end-value_interpolation_start)/lapply((year_interpolation_end-year_interpolation_start), FUN)[[1]]
                  b <- value_interpolation_start
                  tech_market_shares[nrow(tech_market_shares)+1,] <- c(i, "tech_share", sizes[k], technologies[j], lapply((i-year_interpolation_start), FUN)[[1]]*a+b, interpolation_type, paste0(sizes[k], "_", technologies[j]))
                } else if (interpolation_type == "erf") {
                  mu <- year_interpolation_start+(year_interpolation_end-year_interpolation_start)/2
                  sigma <- sqrt((year_end-year_start)/5)
                  tech_market_shares[nrow(tech_market_shares)+1,] <- c(i, "tech_share", sizes[k], technologies[j], value_interpolation_start+(value_interpolation_end-value_interpolation_start)*(1/2*(1+erf((i-mu)/(sigma^2*sqrt(2))))), interpolation_type, paste0(sizes[k], "_", technologies[j]))
                }
              }
            }
          }
        }
        tech_market_shares$Value <- as.numeric(tech_market_shares$Value)
      } else {
        tech_market_shares <- data.frame()
      }
      for (i in years_scenario) {
        events_per_year <- scenario_market[which(scenario_market$Year == i),]
        if ("all"%in%events_per_year$Size) {
          temp <- events_per_year[which("all"%in%events_per_year$Size),]
          events_per_year$Size[which("all"%in%events_per_year$Size)] <- "Car"
          temp$Size <- "Light truck"
          events_per_year <- rbind(events_per_year, temp)
        }
        for (j in 1:dim(events_per_year)[1]) {
          if (events_per_year$Technology[j] == "all") {
            technologies <- c("all")
          } else if (events_per_year$Technology[j] == "electric") {
            technologies <- c("BEV300")
          } else if (events_per_year$Technology[j] == "icev") {
            technologies <- c("CNG", "FFV", "ICEV-D", "icev-g")
          } else if (events_per_year$Technology[j] == "hybrid") {
            technologies <- c("HEV", "PHEV20", "PHEV40")
          } else if (events_per_year$Technology[j] == "hydrogen") {
            technologies <- "FCV" 
          } else if (events_per_year$Technology[j] == "zev-ca") {
            technologies <- c("BEV300", "FCV", "PHEV20", "PHEV40", "HEV")
          } else {
            technologies <- events_per_year$Technology[j]
          }
          sizes <- events_per_year$Size[j]
          m <- 1
          size_tech <- 0
          for (k in 1:length(technologies)) {
            for (l in 1:length(sizes)) {
              size_tech[m] <- paste0(sizes[l], "_", technologies[k])
              m<-m+1
            }
          }
          if (events_per_year$Event[j] == "sales_ban") {
            targeted_share <- 0
          } else if (events_per_year$Event[j] == "sales_share") {
            targeted_share <- events_per_year$Value[j]
          }
          # First step: add the values explicitly mentioned in the scenario
          if ("all"%in%technologies) {
            updated_size_distribution[which(rownames(updated_size_distribution) == sizes), which(colnames(updated_size_distribution) == i)] <- targeted_share
            updated_size_distribution[which(rownames(updated_size_distribution) != sizes), which(colnames(updated_size_distribution) == i)] <- 1-targeted_share
          } else {
            for (m in 1:length(size_tech)) {
              if (length(tech_market_shares$Value[which(tech_market_shares$Year == i & tech_market_shares$Size_Tech == size_tech[m])]) != 0) {
                targeted_ratio <- tech_market_shares$Value[which(tech_market_shares$Year == i & tech_market_shares$Size_Tech == size_tech[m])]
              } else {
                targeted_ratio <- market_share_rel[which(rownames(market_share_rel) == size_tech[m]),which(colnames(market_share_rel_temp) == i)]/sum(market_share_rel[which(rownames(market_share_rel)%in%size_tech),which(colnames(market_share_rel_temp) == i)])
              }
              market_share_rel_temp[which(rownames(market_share_rel_temp) == size_tech[m]),which(colnames(market_share_rel_temp) == i)] <- targeted_share*targeted_ratio
            }
          }
          # Second step: interpolate the values for the given technology before the targeted year
          for (m in 1:length(size_tech)) {
            if ("all"%in%technologies) {
              interpolation_start_value <- updated_size_distribution[which(rownames(updated_size_distribution) == sizes), min(which(is.na(updated_size_distribution[which(rownames(updated_size_distribution) == sizes),which(colnames(updated_size_distribution) <= i)])))-1]
              interpolation_start_year <- as.numeric(colnames(updated_size_distribution)[min(which(is.na(updated_size_distribution[which(rownames(updated_size_distribution) == sizes),which(colnames(updated_size_distribution) <= i)])))-1])
              interpolation_end_value <- updated_size_distribution[which(rownames(updated_size_distribution) == sizes), which(colnames(updated_size_distribution) == i)]
            } else {
              interpolation_start_year <- as.numeric(colnames(market_share_rel_temp)[min(which(is.na(market_share_rel_temp[which(rownames(market_share_rel_temp) == size_tech[m]),which(colnames(market_share_rel_temp) <= i)])))-1])
              interpolation_start_value <- sum(market_share_rel_temp[which(rownames(market_share_rel_temp)%in%size_tech), min(which(is.na(market_share_rel_temp[which(rownames(market_share_rel_temp) == size_tech[m]),which(colnames(market_share_rel_temp) <= i)])))-1])
              interpolation_end_value <- sum(market_share_rel_temp[which(rownames(market_share_rel_temp)%in%size_tech),which(colnames(market_share_rel_temp) == i)])
            }
            interpolation_end_year <- i
            if (events_per_year$Interpolation_type[j] == "linear") {
              FUN <- prod
            } else if (events_per_year$Interpolation_type[j] == "sqrt") {
              FUN <- sqrt
            } else if (events_per_year$Interpolation_type[j] == "log10") {
              FUN <- log10
            } else if (events_per_year$Interpolation_type[j] == "exp") {
              FUN <- exp
            }
            a <- (interpolation_end_value-interpolation_start_value)/lapply((interpolation_end_year-interpolation_start_year),FUN)[[1]]
            b <- interpolation_start_value
            for (k in (interpolation_start_year+1):(interpolation_end_year-1)) {
              if ("all"%in%technologies) {
                updated_size_distribution[which(rownames(updated_size_distribution) == sizes), which(colnames(updated_size_distribution) == k)] <- lapply((k-interpolation_start_year),FUN)[[1]]*a+b
                updated_size_distribution[which(rownames(updated_size_distribution) != sizes), which(colnames(updated_size_distribution) == k)] <- 1-updated_size_distribution[which(rownames(updated_size_distribution) == sizes), which(colnames(updated_size_distribution) == k)]
              } else {
                targeted_share <- (lapply((k-interpolation_start_year),FUN)[[1]]*a+b)
                if (length(tech_market_shares$Value[which(tech_market_shares$Year == k & tech_market_shares$Size_Tech == size_tech[m])]) != 0) {
                  targeted_ratio <- tech_market_shares$Value[which(tech_market_shares$Year == k & tech_market_shares$Size_Tech == size_tech[m])]
                } else {
                  targeted_ratio <- market_share_rel[which(rownames(market_share_rel) == size_tech[m]),which(colnames(market_share_rel_temp) == k)]/sum(market_share_rel[which(rownames(market_share_rel)%in%size_tech),which(colnames(market_share_rel_temp) == k)])
                }
                market_share_rel_temp[which(rownames(market_share_rel_temp) == size_tech[m]), which(colnames(market_share_rel_temp) == k)] <- targeted_share*targeted_ratio
              }
            }
          }
        }
      }
      if (is.na(updated_size_distribution)[1,which(colnames(updated_size_distribution) == fleet_initial_year)]) {
        updated_size_distribution <- default_sizes_distribution
      }
      modified_values <- which(!is.na(market_share_rel_temp[,which(colnames(market_share_rel_temp) == fleet_initial_year)]))
      non_modified_values <- which(is.na(market_share_rel_temp[,which(colnames(market_share_rel_temp) == fleet_initial_year)]))
      for (i in fleet_initial_year:as.numeric(colnames(market_share_rel_temp)[length(colnames(market_share_rel_temp))])) {
        # Third step: fill-in the future value of the modified technologies
        if (is.na(updated_size_distribution[1,which(colnames(updated_size_distribution) == i)])) {
          updated_size_distribution[1,which(colnames(updated_size_distribution) == i)] <- updated_size_distribution[1,which(colnames(updated_size_distribution) == i-1)]
          updated_size_distribution[2,which(colnames(updated_size_distribution) == i)] <- 1-updated_size_distribution[1,which(colnames(updated_size_distribution) == i)]
        }
        # Fourth step: fill the missing value after the last scenario year
        for (j in 1:length(vehicles_sizes)) {
          for (k in grep(vehicles_sizes[j], rownames(market_share_rel_temp)[modified_values])) {
            if (is.na(market_share_rel_temp[modified_values[k],which(colnames(market_share_rel_temp) == i)])) {
              targeted_share <- sum(market_share_rel_temp[modified_values[grep(vehicles_sizes[j], rownames(market_share_rel_temp)[modified_values])],which(colnames(market_share_rel_temp) == i-1)])
              if (length(tech_market_shares$Value[which(tech_market_shares$Year == i & tech_market_shares$Size_Tech == rownames(market_share_rel_temp)[modified_values[k]])]) != 0) {
                targeted_ratio <- tech_market_shares$Value[which(tech_market_shares$Year == i & tech_market_shares$Size_Tech == rownames(market_share_rel_temp)[modified_values[k]])]
              } else {
                targeted_ratio <- market_share_rel[modified_values[k],which(colnames(market_share_rel_temp) == i)]/sum(market_share_rel[modified_values[grep(vehicles_sizes[j], rownames(market_share_rel)[modified_values])],which(colnames(market_share_rel) == i)])
              }
              market_share_rel_temp[modified_values[k],which(colnames(market_share_rel_temp) == i)] <- targeted_share*targeted_ratio
            }
          }
          for (k in grep(vehicles_sizes[j], rownames(market_share_rel_temp)[non_modified_values])) {
            if (is.na(market_share_rel_temp[non_modified_values[k],which(colnames(market_share_rel_temp) == i)])) {
              targeted_share <- 1-sum(market_share_rel_temp[modified_values[grep(vehicles_sizes[j], rownames(market_share_rel_temp)[modified_values])],which(colnames(market_share_rel_temp) == i)])
              if (length(tech_market_shares$Value[which(tech_market_shares$Year == i & tech_market_shares$Size_Tech == rownames(market_share_rel_temp)[non_modified_values[k]])]) != 0) {
                targeted_ratio <- tech_market_shares$Value[which(tech_market_shares$Year == i & tech_market_shares$Size_Tech == rownames(market_share_rel_temp)[non_modified_values[k]])]
              } else {
                targeted_ratio <- market_share_rel[non_modified_values[k],which(colnames(market_share_rel_temp) == i)]/sum(market_share_rel[non_modified_values[grep(vehicles_sizes[j], rownames(market_share_rel)[non_modified_values])],which(colnames(market_share_rel) == i)])
                if (is.na(targeted_ratio)) {
                  targeted_ratio <- 0
                }
              }
              market_share_rel_temp[non_modified_values[k],which(colnames(market_share_rel_temp) == i)] <- targeted_share*targeted_ratio
            }
          }
        }
      }
      # Fifth step: expand the share by size to have market shares for the complete fleet
      market_share_rel_scenario <- market_share_rel_temp
      for (i in 1:length(vehicles_sizes)) {
        for (j in which(colnames(market_share_rel_temp) == first_proj_yr):length(colnames(market_share_rel_temp))) {
          market_share_rel_scenario[which(grepl(vehicles_sizes[i],rownames(market_share_rel_temp))),j] <- market_share_rel_temp[which(grepl(vehicles_sizes[i],rownames(market_share_rel_temp))),j]*updated_size_distribution[which(grepl(vehicles_sizes[i],rownames(updated_size_distribution))),j]
        }
      }
    } else {
      market_share_rel_scenario <- market_share_rel
    }
    if (iterations == 1) {
      market_share_scenario <- list(state_groups = state_groups)
    }
    market_share_scenario <- append(market_share_scenario, list(market_share_rel_scenario = market_share_rel_scenario))
  }
  if (!"all"%in%state_groups) {
    market_share_scenario[["state_groups"]] <- append(market_share_scenario[["state_groups"]], "all")
    market_share_scenario <- append(market_share_scenario, list(technology_market_share = technology_market_share))
  }
  return(market_share_scenario)
}
