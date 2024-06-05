fleet_composition_scenario_f <- function(fleet, fleet_initial_year = NA, fleet_final_year = NA, first_proj_yr = NA, fleet_id = NA, fuel_matching_option = NA) {
  attribute_f("fleet_composition_scenario_f")
  market_share <- fleet$technology_market_share
  
  
  
  
  fleet_vint_stock <- fleet$get_list_dataframe()[["fleet_vint_stock"]]
  fleet_vint_scrap <- fleet$get_list_dataframe()[["fleet_vint_scrap"]]
  if (fuel_matching_option == "hybrid") {
    correspondence_file <- read.delim(paste0(getwd(), "/inputs/data/FLAME_to_MOVES_correspondance_files/technologies_", fuel_matching_option, ".txt"), header = TRUE, sep = ";", dec = ".")
  } else if (fuel_matching_option == "FLAME") {
    # Placeholder for future developments, e.g. addition of hybrid vehicles
  } else if (fuel_matching_option == "MOVES") {
    # Placeholder for future developments, e.g. addition of hybrid vehicles
  }
  fleet_vint_stock <- left_join(fleet_vint_stock, correspondence_file, by = "Technology") %>%
    select(-c("Technology", "ID"))
  colnames(fleet_vint_stock)[5] <- "Technology"
  fleet_vint_scrap <- left_join(fleet_vint_scrap, correspondence_file, by = "Technology") %>%
    select(-c("Technology", "ID"))
  colnames(fleet_vint_scrap)[5] <- "Technology"
  fleet_vint_stock <- aggregate(Value ~ Age+Year+Size+Technology, data = fleet_vint_stock, FUN = sum)
  fleet_vint_scrap <- aggregate(Value ~ Age+Year+Size+Technology, data = fleet_vint_scrap, FUN = sum)
  if (fleet_id == "no_EVs") {
    #Update the stock dataset
    bevs_stock <- fleet_vint_stock[which(grepl("BEV", fleet_vint_stock$Technology)),]
    bevs_stock$Value <- 1
    fleet_vint_stock$Technology[which(grepl("BEV", fleet_vint_stock$Technology))] <- "ICEV-G"
    fleet_vint_stock <- aggregate(Value ~ Age+Year+Size+Technology, fleet_vint_stock, FUN = sum)
    fleet_vint_stock <- rbind(fleet_vint_stock, bevs_stock)
    # Update the scrap dataset
    bevs_scrap <- fleet_vint_scrap[which(grepl("BEV", fleet_vint_scrap$Technology)),]
    bevs_scrap$Value <- 1
    fleet_vint_scrap$Technology[which(grepl("BEV", fleet_vint_scrap$Technology))] <- "ICEV-G"
    fleet_vint_scrap <- aggregate(Value ~ Age+Year+Size+Technology, fleet_vint_scrap, FUN = sum)
    fleet_vint_scrap <- rbind(fleet_vint_scrap, bevs_scrap)
  }
  vehicles_sizes <- unique(fleet_vint_stock$Size)
  scenario_entire_us <- fleet_read_scenario_f(fleet_id = NA)
  scenario_entire_us <- arrange(scenario_entire_us, scenario_entire_us$Year)
  for (i in 1:length(unique(fleet_vint_stock$Size))) {
    scenario_entire_us$Size[which(scenario_entire_us$Size == tolower(unique(fleet_vint_stock$Size)[i]))] <- unique(fleet_vint_stock$Size)[i]
  }
  total_fleet_size <- total_fleet_size_f(fleet_vint_stock)
  state_groups <- unique(scenario_entire_us$State)
  population_distribution_states <- get_input_f(input_name = 'vehicle_population_by_state')
  list_us_states <- read.csv(paste0(getwd(), "/inputs/air_quality/list_states.csv"), sep = ";")$stateID
  state_groups_composition <- get_input_f(input_name = 'state_groups_composition')
  states_not_in_any_group <- c()
  # Create a separate group for states not in any group to avoid having double states / Remove the states already in another group
  if (length(state_groups) != 0 & !isTRUE(state_groups == "all")) {
    for (i in 1:length(state_groups)) {
      if (state_groups[i] != "all" & !grepl("!", state_groups[i])) {
        states_not_in_any_group <- setdiff(list_us_states, append(states_not_in_any_group, strsplit(state_groups_composition$states[which(state_groups_composition$group_name == state_groups[i])], split = "-")[[1]]))
      } else if (state_groups[i] != "all" & grepl("!", state_groups[i])) {
        states_not_in_any_group <- setdiff(list_us_states, append(states_not_in_any_group, strsplit(state_groups_composition$states[which(state_groups_composition$group_name == gsub("!","",state_groups[i]))], split = "-")[[1]]))
      }
    }
  } else {
    states_not_in_any_group <- list_us_states
  }
  states_not_in_any_group <- unique(states_not_in_any_group)
  if (!"all"%in%state_groups) {
    state_groups <- append(state_groups, "all")
  }
  # Create the scenario files
  fleet_vint_stock_scenario <- data.frame()
  fleet_vint_scrap_scenario <- data.frame()
  for (iterations in 1:length(state_groups)) {
    scenario <- scenario_entire_us[which(scenario_entire_us$State == state_groups[iterations] | scenario_entire_us$State == "all"),]
    if (state_groups[iterations] == "all") {
      states_in_group <- states_not_in_any_group
    } else if (grepl("!", state_groups[iterations])) {
      states_in_group <- setdiff(list_us_states, strsplit(state_groups_composition$states[which(state_groups_composition$group_name == gsub("!","",state_groups[iterations]))], split = "-")[[1]])
    } else {
      states_in_group <- strsplit(state_groups_composition$states[which(state_groups_composition$group_name == state_groups[iterations])], split = "-")[[1]]
    }
    # Modify the total fleet size based on the growth rate given in the scenario
    scenario_total_fleet <- arrange(scenario[which(scenario$Event == "growth_rate"),], Year)
    if (dim(scenario_total_fleet)[1]!=0) {
      total_fleet_size_scenario <- total_fleet_size
      total_fleet_size_scenario[which(rownames(total_fleet_size)>= fleet_initial_year),] <- NA
      years <- rownames(total_fleet_size_scenario)
      growth_rate <- NA
      for (i in 1:length(years)) {
        if (years[i] >= fleet_initial_year) {
          if(length(which(scenario_total_fleet$Year == years[i]))!=0) {
            growth_rate <- scenario_total_fleet$Value[which(scenario_total_fleet$Year == years[i])]
          }
          if (is.na(growth_rate)) {
            total_fleet_size_scenario$Total[i] <- total_fleet_size$Total[i]
          } else {
            total_fleet_size_scenario$Total[i] <- (1+growth_rate)*total_fleet_size_scenario$Total[i-1]
          }
        }
      }
    } else {
      total_fleet_size_scenario <- total_fleet_size
    }
    # Calculation of the market share
    market_share_rel <- market_share_matrix_f(fleet_vint_stock)[["market_share_rel"]]
    #vehicles_sizes <- strsplit(rownames(market_share_rel), split = "_")
    size_tech <- 0
    default_sizes_distribution <- data.frame()
    # First step: Calculate the share of cars and light trucks in the passenger vehicles fleet
    for (i in 1:length(colnames(market_share_rel))) {
      for (j in 1:length(vehicles_sizes)) {
        default_sizes_distribution[j,i] <- sum(market_share_rel[which(grepl(vehicles_sizes[j], rownames(market_share_rel))), i])
        #market_share_rel[which(grepl(vehicles_sizes[j],rownames(market_share_rel))), i]/sum(market_share_rel[which(grepl(vehicles_sizes[j],rownames(market_share_rel))), i])
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
            technologies <- c("BEV100", "BEV300")
          } else if (events_per_year$Technology[j] == "icev") {
            technologies <- c("CNG", "FFV", "ICEV-D", "icev-g")
          } else if (events_per_year$Technology[j] == "hybrid") {
            technologies <- c("HEV", "PHEV20", "PHEV40")
          } else if (events_per_year$Technology[j] == "hydrogen") {
            technologies <- "FCV" 
          } else if (events_per_year$Technology[j] == "zev-ca") {
            technologies <- c("BEV100", "BEV300", "FCV", "PHEV20", "PHEV40", "HEV")
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
    # Reconstruction of the fleet composition // Dataset for the fleet composition and the scrap
    fleet_vint_stock_scenario_temp <- fleet_vint_stock
    fleet_vint_stock_scenario_temp$Value[which(fleet_vint_stock_scenario_temp$Year >= fleet_initial_year)] <- NA
    fleet_vint_scrap_scenario_temp <- fleet_vint_scrap
    fleet_vint_scrap_scenario_temp$Value[which(fleet_vint_scrap_scenario_temp$Year >= fleet_initial_year)] <- NA
    years <- unique(fleet_vint_stock_scenario_temp$Year)[which(unique(fleet_vint_stock_scenario_temp$Year) >= (fleet_initial_year-1))]
    ages <- unique(fleet_vint_stock_scenario_temp$Age)
    technologies <- unique(fleet_vint_stock_scenario_temp$Technology)
    sizes <- unique(fleet_vint_stock_scenario_temp$Size)
    
    
    
    fun <- function(i) {
      matrix_sur_rate <- data.frame(t(sapply(rownames(fleet$ldv_sales),function(x) do.call(survival_rate_f,list(year=i, size=strsplit(x,"_")[[1]][1], techno=strsplit(x,"_")[[1]][2], cumulative_rate="n"))[1,])))
      colnames(matrix_sur_rate) <- 1:30
      matrix_sur_rate <- add_column(matrix_sur_rate, "Year" = i) %>%
        add_column("Size" = str_split_fixed(rownames(matrix_sur_rate), "_", 2)[,1]) %>%
        add_column("Technology" = str_split_fixed(rownames(matrix_sur_rate), "_", 2)[,2])
    }
    matrix_sur_rate <- do.call(rbind, lapply(years[2:length(years)], fun)) %>%
      pivot_longer(cols = 1:30, names_to = "Age", values_to = "Survival_rate")
    fleet_vint_stock_scenario_temp <- filter(fleet_vint_stock_scenario_temp, fleet_vint_stock_scenario_temp$Year <= fleet_initial_year)
    fun <- function(i) {
      data <- fleet_vint_stock[which(fleet_vint_stock$Year == fleet_initial_year),]
      data$Year <- i
      fleet_vint_stock <- rbind(fleet_vint_stock, data)
      return(fleet_vint_stock)
    }
    fleet_vint_stock <- do.call(rbind, lapply(years[2:length(years)], fun))
    test <- left_join(fleet_vint_stock, matrix_sur_rate, by = c("Year", "Size", "Technology", "Age"))

    
    for (i in 2:length(years)) {
      matrix_sur_rate <- t(sapply(rownames(fleet$ldv_sales),function(x) do.call(survival_rate_f,list(year=years[i], size=strsplit(x,"_")[[1]][1], techno=strsplit(x,"_")[[1]][2], cumulative_rate="n"))[1,]))
      total_vehicles_new <- 0
      for (j in 1:length(sizes)) {
        for (k in 1:length(technologies)) {
          for (l in 2:length(ages)) {
            survival_rate <- matrix_sur_rate[which(rownames(matrix_sur_rate) == paste0(sizes[j], "_",technologies[k])), which(colnames(matrix_sur_rate) == ages[l])]
            stock_old <- fleet_vint_stock_scenario_temp$Value[which(fleet_vint_stock_scenario_temp$Year == years[i-1] & fleet_vint_stock_scenario_temp$Size == sizes[j] & fleet_vint_stock_scenario_temp$Technology == technologies[k] & fleet_vint_stock_scenario_temp$Age == ages[l-1])]
            #survival_rate <- fleet_vint_survival_rate_scenario$Value[which(fleet_vint_survival_rate_scenario$Year == years[i] & fleet_vint_survival_rate_scenario$Size == sizes[j] & fleet_vint_survival_rate_scenario$Technology == technologies[k] & fleet_vint_survival_rate_scenario$Age == ages[l])]
            stock_new <- stock_old*survival_rate
            fleet_vint_stock_scenario_temp$Value[which(fleet_vint_stock_scenario_temp$Year == years[i] & fleet_vint_stock_scenario_temp$Size == sizes[j] & fleet_vint_stock_scenario_temp$Technology == technologies[k] & fleet_vint_stock_scenario_temp$Age == ages[l])] <- stock_new
            total_vehicles_new <- total_vehicles_new + stock_new
            #if (is.nan(total_vehicles_new)) {
            #  total_vehicles_new <- 0
            #}
            fleet_vint_scrap_scenario_temp$Value[which(fleet_vint_scrap_scenario_temp$Year == years[i] & fleet_vint_scrap_scenario_temp$Size == sizes[j] & fleet_vint_scrap_scenario_temp$Technology == technologies[k] & fleet_vint_scrap_scenario_temp$Age == ages[l])] <- stock_old-stock_new
          }
        }
      }
      targeted_stock <- total_fleet_size_scenario$Total[which(rownames(total_fleet_size_scenario) == years[i])]
      new_sales <- targeted_stock - total_vehicles_new
      if (length(new_sales) == 0) {
        new_sales <- 0
      }
      if (new_sales <0) {
        new_sales <- 0
      }
      for (j in 1:length(sizes)) {
        for (k in 1:length(technologies)) {
          fleet_vint_stock_scenario_temp$Value[which(fleet_vint_stock_scenario_temp$Year == years[i] & fleet_vint_stock_scenario_temp$Size == sizes[j] & fleet_vint_stock_scenario_temp$Technology == technologies[k] & fleet_vint_stock_scenario_temp$Age == 0)] <- market_share_rel_scenario[which(rownames(market_share_rel_scenario) == paste0(sizes[j], "_", technologies[k])), which(colnames(market_share_rel_scenario) == years[i])]*new_sales
        }
      }
    }
    
    # Update the total fleet size dataset
    years <- rownames(total_fleet_size_scenario)[which(rownames(total_fleet_size_scenario) >= fleet_initial_year)]
    for (i in 1:length(years)) {
      for (j in 1:length(vehicles_sizes)) {
        total_fleet_size_scenario[which(rownames(total_fleet_size_scenario) == years[i]), which(colnames(total_fleet_size_scenario) == vehicles_sizes[j])] <- sum(fleet_vint_stock_scenario_temp$Value[which(fleet_vint_stock_scenario_temp$Year == years[i] & fleet_vint_stock_scenario_temp$Size == vehicles_sizes[j])])
      }
    }
    #fleet_vint_stock_scenario_temp$Value <- round(fleet_vint_stock_scenario_temp$Value,0)
    fleet_vint_stock_scenario_temp <- add_column(fleet_vint_stock_scenario_temp, "State" = NA) %>%
      add_column("State_factor" = NA)
    #fleet_vint_scrap_scenario_temp$Value <- round(fleet_vint_scrap_scenario_temp$Value,0)
    fleet_vint_scrap_scenario_temp <- add_column(fleet_vint_scrap_scenario_temp, "State" = NA) %>%
      add_column("State_factor" = NA)
    if (length(states_in_group) != 0) {
      for (j in 1:length(states_in_group)) {
        fleet_vint_stock_scenario_state <- fleet_vint_stock_scenario_temp
        fleet_vint_scrap_scenario_state <- fleet_vint_scrap_scenario_temp
        fleet_vint_stock_scenario_state$State <- states_in_group[j]
        fleet_vint_scrap_scenario_state$State <- states_in_group[j]
        plan(multisession)
        evaluation <- unique(fleet_vint_stock_scenario_state$Year)
        data <- future_lapply(evaluation, function(k) {
          if (i < min(population_distribution_states$Year)) {
            state_population_adjustment_factor <- population_distribution_states$Activity[which(population_distribution_states$State == states_in_group[j] &
                                                                                                  population_distribution_states$Year == min(population_distribution_states$Year))]
          } else {
            state_population_adjustment_factor <- population_distribution_states$Activity[which(population_distribution_states$State == states_in_group[j] &
                                                                                                  population_distribution_states$Year == k)]
          }
          index_stock <- which(fleet_vint_stock_scenario_state$Year == k)
          index_scrap <- which(fleet_vint_scrap_scenario_state$Year == k)
          return(list(state_population_adjustment_factor = state_population_adjustment_factor, index_stock = index_stock, index_scrap = index_scrap))
        })
        for (k in 1:length(evaluation)) {
          fleet_vint_stock_scenario_state$State_factor[data[[k]]$index_stock] <- data[[k]]$state_population_adjustment_factor
          if (length(data[[k]]$index_scrap) != 0) {
            fleet_vint_scrap_scenario_state$State_factor[data[[k]]$index_scrap] <- data[[k]]$state_population_adjustment_factor
          }
        }
        #fleet_vint_stock_scenario_temp$Value <- fleet_vint_stock_scenario_temp$Value*fleet_vint_stock_scenario_temp$State_factor
        #fleet_vint_scrap_scenario_temp$Value <- fleet_vint_scrap_scenario_temp$Value*fleet_vint_scrap_scenario_temp$State_factor
        #fleet_vint_stock_scenario_temp <- select(fleet_vint_stock_scenario_temp, -c("State_factor"))
        #fleet_vint_scrap_scenario_temp <- select(fleet_vint_scrap_scenario_temp, -c("State_factor"))
        fleet_vint_stock_scenario <- rbind(fleet_vint_stock_scenario, fleet_vint_stock_scenario_state)
        fleet_vint_scrap_scenario <- rbind(fleet_vint_scrap_scenario, fleet_vint_scrap_scenario_state)
      }
      #fleet_vint_stock_scenario$Value <- fleet_vint_stock_scenario$Value*fleet_vint_stock_scenario$State_factor
      #fleet_vint_scrap_scenario$Value <- fleet_vint_scrap_scenario$Value*fleet_vint_scrap_scenario$State_factor
      #fleet_vint_stock_scenario <- select(fleet_vint_stock_scenario, -c("State_factor"))
      #fleet_vint_scrap_scenario <- select(fleet_vint_scrap_scenario, -c("State_factor"))
    }
  }
  fleet_vint_stock_scenario$Value <- fleet_vint_stock_scenario$Value*fleet_vint_stock_scenario$State_factor
  fleet_vint_scrap_scenario$Value <- fleet_vint_scrap_scenario$Value*fleet_vint_scrap_scenario$State_factor
  fleet_vint_stock_scenario <- select(fleet_vint_stock_scenario, -c("State_factor"))
  fleet_vint_scrap_scenario <- select(fleet_vint_scrap_scenario, -c("State_factor"))
  # Distribution into the final fleet composition according to the relative weight of each state
  if (fleet_id == "Evs100") {
    #Update the stock dataset
    bevs_stock <- fleet_vint_stock_scenario[which(!grepl("BEV", fleet_vint_stock_scenario$Technology)),]
    bevs_stock$Value <- 1
    fleet_vint_stock_scenario$Technology[which(!grepl("BEV", fleet_vint_stock_scenario$Technology))] <- "BEV300"
    fleet_vint_stock_scenario <- aggregate(Value ~ Age+Year+Size+Technology+State, fleet_vint_stock_scenario, FUN = sum)
    fleet_vint_stock_scenario <- rbind(fleet_vint_stock_scenario, bevs_stock)
    # Update the scrap dataset
    bevs_scrap <- fleet_vint_scrap_scenario[which(!grepl("BEV", fleet_vint_scrap_scenario$Technology)),]
    bevs_scrap$Value <- 1
    fleet_vint_scrap_scenario$Technology[which(!grepl("BEV", fleet_vint_scrap_scenario$Technology))] <- "BEV300"
    fleet_vint_scrap_scenario <- aggregate(Value ~ Age+Year+Size+Technology+State, fleet_vint_scrap_scenario, FUN = sum)
    fleet_vint_scrap_scenario <- rbind(fleet_vint_scrap_scenario, bevs_scrap)
  }
  fleet_vint_stock_scenario_state_breakdown <- fleet_vint_stock_scenario
  fleet_vint_scrap_scenario_state_breakdown <- fleet_vint_scrap_scenario
  fleet_vint_stock_scenario_state_breakdown$Value <- round(fleet_vint_stock_scenario_state_breakdown$Value, 0)
  fleet_vint_scrap_scenario_state_breakdown$Value <- round(fleet_vint_scrap_scenario_state_breakdown$Value, 0)
  fleet_vint_stock_scenario <- select(fleet_vint_stock_scenario, -"State")
  fleet_vint_stock_scenario <- aggregate(Value ~ Age + Year + Size + Technology, data = fleet_vint_stock_scenario, FUN = sum, na.rm = TRUE) %>%
    relocate(Value, .before = Year)
  fleet_vint_stock_scenario$Value <- round(fleet_vint_stock_scenario$Value, 0)
  fleet_vint_scrap_scenario <- select(fleet_vint_scrap_scenario, -"State")
  fleet_vint_scrap_scenario <- aggregate(Value ~ Age + Year + Size + Technology, data = fleet_vint_scrap_scenario, FUN = sum, na.rm = TRUE) %>%
    relocate(Value, .before = Year)
  fleet_vint_scrap_scenario$Value <- round(fleet_vint_scrap_scenario$Value, 0)
  
  # Modify the MOVES emission factors (e.g., updated brake and tirewear to take into account heavier electric vehicles)
  MOVES_modify_emission_factors_f()
  fleet_vint_stock_scenario <- add_column(fleet_vint_stock_scenario, "Model_Year" = NA)
  fleet_vint_stock_scenario$Model_Year <- fleet_vint_stock_scenario$Year-fleet_vint_stock_scenario$Age
  fleet_vint_scrap_scenario <- add_column(fleet_vint_scrap_scenario, "Model_Year" = NA)
  fleet_vint_scrap_scenario$Model_Year <- fleet_vint_scrap_scenario$Year-fleet_vint_scrap_scenario$Age
  fleet_vint_stock_scenario_state_breakdown <- add_column(fleet_vint_stock_scenario_state_breakdown, "Model_Year" = NA)
  fleet_vint_stock_scenario_state_breakdown$Model_Year <- fleet_vint_stock_scenario_state_breakdown$Year-fleet_vint_stock_scenario_state_breakdown$Age
  fleet_vint_scrap_scenario_state_breakdown <- add_column(fleet_vint_scrap_scenario_state_breakdown, "Model_Year" = NA)
  fleet_vint_scrap_scenario_state_breakdown$Model_Year <- fleet_vint_scrap_scenario_state_breakdown$Year-fleet_vint_scrap_scenario_state_breakdown$Age
  return(list(fleet_vint_stock = fleet_vint_stock_scenario, fleet_vint_scrap = fleet_vint_scrap_scenario, fleet_vint_stock_scenario_state_breakdown = fleet_vint_stock_scenario_state_breakdown, fleet_vint_scrap_scenario_state_breakdown = fleet_vint_scrap_scenario_state_breakdown))
}
