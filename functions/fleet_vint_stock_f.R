#' fleet_vint_stock_f
#' Function: Calculates the vintaged stock of the U.S. LDVs by vehicle technology, size and model year.
#' @importFrom reshape2 acast
#' @export
fleet_vint_stock_f <- function(first_yr=NA,last_yr=NA,fleet_vint_procedure=NA,fleet_tot_stock=NA,aeo_scen=NA, fleet_composition_source=NA, scenario_id = NA, first_proj_yr = NA, fleet_id = NA, years_simulation = NA,fuel_matching_option=NA, benefits_per_ev_year = NA){
  attribute_f("fleet_vint_stock_f")
  results_path <- paste0(getwd(), "/outputs/air_quality/", Sys.Date(), "_", scenario_id, "_results")
  if (fleet_composition_source == "FLAME") {
    #Initialize
    fleet <- do.call(fleet_initialize_f,list())
    #Create historical vintaged stock
    fleet <- do.call(fleet_vint_stock_initialization_f,list(fleet=fleet))
    #Select the vintaged stock procedure. Sales = Sales are forced then stock are calculated (option of adjusting prospective vintaged stock to match a total stock). Stock = Sales are calculated from previous vintaged stocks and total prospective stocks.
    if (fleet_vint_procedure=="sales"){
      #Update stock with projected data
      #Extract input data of stock from AEO
      proj_stock_aeo  <- get_input_f(input_name = 'fleet_stock_proj_aeo')
      #Create a matrix of the previous data
      matrix_stock_proj <- acast(data=subset(proj_stock_aeo,Aeo_case==aeo_scen & Year >= first_proj_yr), Size + Technology ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
      #Update the ldv_on_road_stock field within the fleet object with the previous matrix
      fleet$ldv_on_road_stock[rownames(matrix_stock_proj),colnames(matrix_stock_proj)] <- matrix_stock_proj
      #Update the total stock field of the fleet object
      fleet$ldv_on_road_stock_tot[,colnames(matrix_stock_proj)] <- colSums(matrix_stock_proj)
      #Update technology market share
      fleet_technology_market_share <- do.call(fun_res_f,list(fun_name="fleet_technology_market_share_proj_f"))[["fleet_technology_market_share"]]
      #Update the technology market share field in the fleet object
      fleet$technology_market_share[rownames(fleet_technology_market_share),colnames(fleet_technology_market_share)] <- fleet_technology_market_share
      #Extract input data of sales from AEO
      proj_sales_aeo  <- get_input_f(input_name = 'fleet_sales_proj_aeo')
      #Estimate total sales from the previous file
      tot_sales_aeo <- aggregate(Value ~ Year,data=subset(proj_sales_aeo,Aeo_case==aeo_scen & Year >= first_proj_yr),FUN=sum)
      #Create a matrix from previous file. Diagonale matrix with data on the diagonal.
      matrix_tot_sales <- diag(x=tot_sales_aeo$Value,nrow=nrow(tot_sales_aeo),ncol=nrow(tot_sales_aeo))
      #Renaming the names of the columns and rows
      dimnames(matrix_tot_sales) <- list(tot_sales_aeo$Year,tot_sales_aeo$Year)
      #Calculate technology-specific sales in a matrix format. Update the associated fiel within the fleet object
      fleet$ldv_sales[rownames(fleet$technology_market_share),colnames(matrix_tot_sales)] <- fleet$technology_market_share[,colnames(matrix_tot_sales)] %*% matrix_tot_sales
      #Caculate prospective vintaged stocks and update the fleet object
      for (year in first_proj_yr:last_yr){
        fleet <- do.call(fleet_vint_stock_update_with_sales_f,list(fleet=fleet,year=year))
      }
    } else if(fleet_vint_procedure=="stock"){
      #Update total projected stock with the specified data
      #Use GCAM data
      if (fleet_tot_stock=="gcam"){
        fleet <- do.call(fleet_ldv_stock_update_gcam_f,list(fleet=fleet))
      #Considers constant stock
      } else if(fleet_tot_stock=="constant"){
          fleet$ldv_on_road_stock_tot["Total",as.character(first_proj_yr:last_yr)] <- fleet$ldv_on_road_stock_tot["Total",as.character(first_proj_yr-1)]
      #Uses AEO prospective total stocks
      } else if(fleet_tot_stock=="aeo"){
        #Extract input data of stock from AEO
        proj_stock_aeo  <- get_input_f(input_name = 'fleet_stock_proj_aeo')
        # Adjust the technologies
        if (fuel_matching_option == "hybrid") {
          correspondence_file <- read.delim(paste0(getwd(), "/inputs/data/FLAME_to_MOVES_correspondance_files/technologies_", fuel_matching_option, ".txt"), header = TRUE, sep = ";", dec = ".")
        }
        proj_stock_aeo <- left_join(proj_stock_aeo, correspondence_file, by = "Technology") %>%
          select(-c("ID", "Technology")) %>%
          relocate("Technology_Alt", .before = "Size")
        colnames(proj_stock_aeo)[1] <- "Technology"
        proj_stock_aeo <- aggregate(.~Technology + Size + Year + Data_type + Aeo_case + Aeo_year + Unit, data = proj_stock_aeo, FUN = sum)
        #Create the matrix of total stock
        matrix_tot_stock_proj <- acast(data=subset(proj_stock_aeo,Aeo_case==aeo_scen & Year >= first_proj_yr), Data_type ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
        #Update prospective stock
        fleet$ldv_on_road_stock_tot["Total",as.character(first_proj_yr:last_yr)] <- matrix_tot_stock_proj[,as.character(first_proj_yr:last_yr)]
      }
      #Update technology market share. Keep market share of AEO.
      fleet_technology_market_share <- do.call(fun_res_f,list(fun_name="fleet_technology_market_share_proj_f"))[["fleet_technology_market_share"]]
      fleet$technology_market_share[rownames(fleet_technology_market_share),colnames(fleet_technology_market_share)] <- fleet_technology_market_share
      if (fleet_id == "aeo") {
        state_groups <- "all"
        market_share <- list(state_groups = state_groups)
        market_share <- append(market_share, list(market_share = fleet$technology_market_share))
        groups <- "all"
      } else {
        # Update the technology market share with the custom scenario
        print("Application of the fleet scenario")
        market_share <- do.call(scenario_market_share_f, list(technology_market_share = fleet$technology_market_share))
        ## Modify the fleet if calculation of the NoEVs scenario
        if (tolower(fleet_id) == "no_evs") {
          market_share$market_share_rel_scenario[which(rownames(market_share$market_share_rel_scenario) == "Car_ICEV-G"),] <- market_share$market_share_rel_scenario[which(rownames(market_share$market_share_rel_scenario) == "Car_ICEV-G"),]+market_share$market_share_rel_scenario[which(rownames(market_share$market_share_rel_scenario) == "Car_BEV300"),]
          market_share$market_share_rel_scenario[which(rownames(market_share$market_share_rel_scenario) == "Light truck_ICEV-G"),] <- market_share$market_share_rel_scenario[which(rownames(market_share$market_share_rel_scenario) == "Light truck_ICEV-G"),]+market_share$market_share_rel_scenario[which(rownames(market_share$market_share_rel_scenario) == "Light truck_BEV300"),]
          market_share$market_share_rel_scenario[which(rownames(market_share$market_share_rel_scenario) == "Car_BEV300"),] <- 0
          market_share$market_share_rel_scenario[which(rownames(market_share$market_share_rel_scenario) == "Light truck_BEV300"),] <- 0
        } else if (tolower(fleet_id) == "no_icevs") {
          #market_share$market_share_rel_scenario[which(rownames(market_share$market_share_rel_scenario) == "Car_ICEV-G"),] <- 0
          #market_share$market_share_rel_scenario[which(rownames(market_share$market_share_rel_scenario) == "Light truck_ICEV-G"),] <- 0
          #market_share$market_share_rel_scenario[which(rownames(market_share$market_share_rel_scenario) == "Car_BEV300"),] <- market_share$market_share_rel_scenario[which(rownames(market_share$market_share_rel_scenario) == "Car_ICEV-G"),]+market_share$market_share_rel_scenario[which(rownames(market_share$market_share_rel_scenario) == "Car_BEV300"),]
          #market_share$market_share_rel_scenario[which(rownames(market_share$market_share_rel_scenario) == "Light truck_BEV300"),] <- market_share$market_share_rel_scenario[which(rownames(market_share$market_share_rel_scenario) == "Light truck_ICEV-G"),]+market_share$market_share_rel_scenario[which(rownames(market_share$market_share_rel_scenario) == "Light truck_BEV300"),]
        }
        ## Modify the fleet if calculation of the benefits per EV
        if (grepl("benefits_per_ev", scenario_id)) {
          car_market_share <- sum(market_share$market_share_rel_scenario[grep("Car", rownames(market_share$market_share_rel_scenario)),
                                                                         which(colnames(market_share$market_share_rel_scenario) == benefits_per_ev_year)])
          trucks_market_share <- sum(market_share$market_share_rel_scenario[grep("truck", rownames(market_share$market_share_rel_scenario)),
                                                                            which(colnames(market_share$market_share_rel_scenario) == benefits_per_ev_year)])
          market_share$market_share_rel_scenario[,which(colnames(market_share$market_share_rel_scenario) == benefits_per_ev_year)] <- 0
          market_share$market_share_rel_scenario[which(rownames(market_share$market_share_rel_scenario) == "Car_BEV300"),
                                                 which(colnames(market_share$market_share_rel_scenario) == benefits_per_ev_year)] <- car_market_share
          market_share$market_share_rel_scenario[which(rownames(market_share$market_share_rel_scenario) == "Light truck_BEV300"),
                                                 which(colnames(market_share$market_share_rel_scenario) == benefits_per_ev_year)] <- trucks_market_share
        }
        #Calculate the vintaged stocks year by year from total stocks.
        groups <- market_share[["state_groups"]]
      }
      GIS_matrix <- read.csv(paste0(getwd(), "/inputs/air_quality/GIS_matching_matrix.csv"))
      for (i in 1:length(groups)) {
        fleet$technology_market_share <- as.matrix(market_share[[i+1]])
        for (year in first_proj_yr:last_yr){
          fleet <- do.call(fleet_vint_stock_update_with_stock_f,list(fleet=fleet,year=year))
        }
        fleet_vint_stock <- fleet$get_list_dataframe()[["fleet_vint_stock"]] %>%
          add_column("Group" = groups[i])
        fleet_vint_scrap <- fleet$get_list_dataframe()[["fleet_vint_scrap"]] %>%
          add_column("Group" = groups[i])
        if (i == 1) {
          fleet_stock <- list(fleet_vint_stock = fleet_vint_stock)
          fleet_scrap <- list(fleet_vint_scrap = fleet_vint_scrap)
        } else {
          fleet_stock <- append(fleet_stock, list(fleet_vint_stock = fleet_vint_stock))
          fleet_scrap <- append(fleet_scrap, list(fleet_vint_scrap = fleet_vint_scrap))
        }
      }
      ## Apply specific scenarios
      ###   NoEVs scenario
      if (tolower(fleet_id) == "no_evs"  & !grepl("benefits_per_ev", scenario_id)) {
        stock_BEV <- fleet_stock[["fleet_vint_stock"]][which(fleet_stock[["fleet_vint_stock"]]$Technology%in%c("BEV100", "BEV300")),]
        stock_BEV$Value <- 0
        fleet_stock[["fleet_vint_stock"]]$Technology[which(fleet_stock[["fleet_vint_stock"]]$Technology%in%c("BEV100", "BEV300"))] <- "ICEV-G"
        fleet_stock[["fleet_vint_stock"]] <- aggregate(Value~Age+Year+Size+Technology+Group, data = fleet_stock[["fleet_vint_stock"]], FUN = sum) %>%
          relocate(Value, .after = Age)
        fleet_stock[["fleet_vint_stock"]] <- rbind(fleet_stock[["fleet_vint_stock"]], stock_BEV)
        scrap_BEV <- fleet_scrap[["fleet_vint_scrap"]][which(fleet_scrap[["fleet_vint_scrap"]]$Technology%in%c("BEV100", "BEV300")),]
        scrap_BEV$Value <- 0
        fleet_scrap[["fleet_vint_scrap"]]$Technology[which(fleet_scrap[["fleet_vint_scrap"]]$Technology%in%c("BEV100", "BEV300"))] <- "ICEV-G"
        fleet_scrap[["fleet_vint_scrap"]] <- aggregate(.~Age+Year+Size+Technology+Group, data = fleet_scrap[["fleet_vint_scrap"]], FUN = sum) %>%
          relocate(Value, .after = Age)
        fleet_scrap[["fleet_vint_scrap"]] <- rbind(fleet_scrap[["fleet_vint_scrap"]], scrap_BEV)
      } else if (tolower(fleet_id) == "no_icevs"  & !grepl("benefits_per_ev", scenario_id)) {
        #stock_BEV <- fleet_stock[["fleet_vint_stock"]][which(fleet_stock[["fleet_vint_stock"]]$Technology%in%c("BEV100", "BEV300") & fleet_stock[["fleet_vint_stock"]]$Year == first_proj_yr+1),]
        #stock_BEV$Value <- 0
        #fleet_stock[["fleet_vint_stock"]]$Technology[which(fleet_stock[["fleet_vint_stock"]]$Technology%in%c("BEV100", "BEV300") & fleet_stock[["fleet_vint_stock"]]$Year == first_proj_yr+1)] <- "ICEV-G"
        #fleet_stock[["fleet_vint_stock"]] <- aggregate(Value~Age+Year+Size+Technology+Group, data = fleet_stock[["fleet_vint_stock"]], FUN = sum) %>%
        #  relocate(Value, .after = Age)
        #fleet_stock[["fleet_vint_stock"]] <- rbind(fleet_stock[["fleet_vint_stock"]], stock_BEV)
        #scrap_BEV <- fleet_scrap[["fleet_vint_scrap"]][which(fleet_scrap[["fleet_vint_scrap"]]$Technology%in%c("BEV100", "BEV300") & fleet_scrap[["fleet_vint_scrap"]]$Year == first_proj_yr+1),]
        #scrap_BEV$Value <- 0
        #fleet_scrap[["fleet_vint_scrap"]]$Technology[which(fleet_scrap[["fleet_vint_scrap"]]$Technology%in%c("BEV100", "BEV300") & fleet_scrap[["fleet_vint_scrap"]]$Year == first_proj_yr+1)] <- "ICEV-G"
        #fleet_scrap[["fleet_vint_scrap"]] <- aggregate(.~Age+Year+Size+Technology+Group, data = fleet_scrap[["fleet_vint_scrap"]], FUN = sum) %>%
        #  relocate(Value, .after = Age)
        #fleet_scrap[["fleet_vint_scrap"]] <- rbind(fleet_scrap[["fleet_vint_scrap"]], scrap_BEV)
        ##
        stock_ICEVs <- fleet_stock[["fleet_vint_stock"]][which(!fleet_stock[["fleet_vint_stock"]]$Technology%in%c("BEV100", "BEV300") & fleet_stock[["fleet_vint_stock"]]$Year > first_proj_yr+1),]
        stock_ICEVs$Value <- 0
        fleet_stock[["fleet_vint_stock"]]$Technology[which(!fleet_stock[["fleet_vint_stock"]]$Technology%in%c("BEV100", "BEV300") & fleet_stock[["fleet_vint_stock"]]$Year > first_proj_yr+1)] <- "BEV300"
        fleet_stock[["fleet_vint_stock"]] <- aggregate(Value~Age+Year+Size+Technology+Group, data = fleet_stock[["fleet_vint_stock"]], FUN = sum) %>%
          relocate(Value, .after = Age)
        fleet_stock[["fleet_vint_stock"]] <- rbind(fleet_stock[["fleet_vint_stock"]], stock_ICEVs)
        scrap_ICEVs <- fleet_scrap[["fleet_vint_scrap"]][which(!fleet_scrap[["fleet_vint_scrap"]]$Technology%in%c("BEV100", "BEV300") & fleet_scrap[["fleet_vint_scrap"]]$Year > first_proj_yr+1),]
        scrap_ICEVs$Value <- 0
        fleet_scrap[["fleet_vint_scrap"]]$Technology[which(!fleet_scrap[["fleet_vint_scrap"]]$Technology%in%c("BEV100", "BEV300") & fleet_scrap[["fleet_vint_scrap"]]$Year > first_proj_yr+1)] <- "BEV300"
        fleet_scrap[["fleet_vint_scrap"]] <- aggregate(.~Age+Year+Size+Technology+Group, data = fleet_scrap[["fleet_vint_scrap"]], FUN = sum) %>%
          relocate(Value, .after = Age)
        fleet_scrap[["fleet_vint_scrap"]] <- rbind(fleet_scrap[["fleet_vint_scrap"]], scrap_ICEVs)
      }
      if (grepl("new_only", scenario_id)) {
        fleet_stock[["fleet_vint_stock"]] <- filter(fleet_stock[["fleet_vint_stock"]], fleet_stock[["fleet_vint_stock"]]$Age == 0)
        #fleet_scrap[["fleet_vint_scrap"]] <- filter(fleet_scrap[["fleet_vint_scrap"]], fleet_scrap[["fleet_vint_scrap"]]$Age == 0)
      } else if (grepl("old_only", scenario_id)) {
        fleet_stock[["fleet_vint_stock"]] <- filter(fleet_stock[["fleet_vint_stock"]], fleet_stock[["fleet_vint_stock"]]$Age > 0)
        #fleet_scrap[["fleet_vint_scrap"]] <- filter(fleet_scrap[["fleet_vint_scrap"]], fleet_scrap[["fleet_vint_scrap"]]$Age > 0)
      }
      ## Allocate the fleet compositions to the counties
      print("Generate the fleet composition by county")
      if ("urban"%in%groups) {
        counties <- select(GIS_matrix, c("Urban", "FIPS", "ST_FIPS"))
        colnames(counties)[1] <- "Group"
        counties$Group[which(counties$Group == 1)] <- "urban"
        fleet_stock_urban <- left_join(filter(fleet_stock[[which(groups == "urban")]], Year >= first_proj_yr), counties, by = "Group", relationship = "many-to-many")
        fleet_scrap_urban <- left_join(filter(fleet_scrap[[which(groups == "urban")]], Year >= first_proj_yr), counties, by = "Group", relationship = "many-to-many")
        counties$Group[which(counties$Group == 0)] <- "rural"
        fleet_stock_rural <- left_join(filter(fleet_stock[[which(groups == "rural")]], Year >= first_proj_yr), counties, by = "Group", relationship = "many-to-many")
        fleet_scrap_rural <- left_join(filter(fleet_scrap[[which(groups == "rural")]], Year >= first_proj_yr), counties, by = "Group", relationship = "many-to-many")
        ## Create the unique fleet dataset
        fleet_vint_stock_county <- rbind(fleet_stock_urban, fleet_stock_rural)
        fleet_vint_scrap_county <- rbind(fleet_scrap_urban, fleet_scrap_rural)
      } else if ("zev"%in%groups) {
        counties <- select(GIS_matrix, c("ZEV", "FIPS", "ST_FIPS"))
        colnames(counties)[1] <- "Group"
        counties$Group[which(counties$Group == 1)] <- "zev"
        fleet_stock_zev <- left_join(filter(fleet_stock[[which(groups == "zev")]], Year >= first_proj_yr), counties, by = "Group", relationship = "many-to-many")
        fleet_scrap_zev <- left_join(filter(fleet_scrap[[which(groups == "zev")]], Year >= first_proj_yr), counties, by = "Group", relationship = "many-to-many")
        counties$Group[which(counties$Group == 0)] <- "all"
        fleet_stock_nonzev <- left_join(filter(fleet_stock[[which(groups == "all")]], Year >= first_proj_yr), counties, by = "Group", relationship = "many-to-many")
        fleet_scrap_nonzev <- left_join(filter(fleet_scrap[[which(groups == "all")]], Year >= first_proj_yr), counties, by = "Group", relationship = "many-to-many")
        ## Create the unique fleet dataset
        fleet_vint_stock_county <- rbind(fleet_stock_zev, fleet_stock_nonzev)
        fleet_vint_scrap_county <- rbind(fleet_scrap_zev, fleet_scrap_nonzev)
      } else {
        counties <- select(GIS_matrix, c("FIPS", "ST_FIPS")) %>%
          add_column("Group" = "all")
        fleet_vint_stock_county <- left_join(filter(fleet_stock[[which(groups == "all")]], Year >= first_proj_yr), counties, by = "Group", relationship = "many-to-many")
        fleet_vint_scrap_county <- left_join(filter(fleet_scrap[[which(groups == "all")]], Year >= first_proj_yr), counties, by = "Group", relationship = "many-to-many")
      }
      # Add the state and fleet allocation factors
      state_allocation_factors <- get_input_f(input_name = "vehicle_population_by_state")
      colnames(state_allocation_factors)[1] <- "ST_FIPS"
      county_allocation_factors <- get_input_f(input_name = "normalized_vehicles_distribution_by_county")
      fleet_vint_stock_county <- left_join(select(fleet_vint_stock_county, -c("Group")), state_allocation_factors, by = c("Year", "ST_FIPS")) %>%
        left_join(county_allocation_factors, by = "FIPS")
      fleet_vint_scrap_county <- left_join(select(fleet_vint_scrap_county, -c("Group")), state_allocation_factors, by = c("Year", "ST_FIPS")) %>%
        left_join(county_allocation_factors, by = "FIPS")
      fleet_vint_stock_county$Value <- fleet_vint_stock_county$Value*fleet_vint_stock_county$Activity*fleet_vint_stock_county$county_allocation_factor
      fleet_vint_stock_county <- select(fleet_vint_stock_county, -c("Activity", "State_ID", "county_allocation_factor"))
      fleet_vint_scrap_county$Value <- fleet_vint_scrap_county$Value*fleet_vint_scrap_county$Activity*fleet_vint_scrap_county$county_allocation_factor
      fleet_vint_scrap_county <- select(fleet_vint_scrap_county, -c("Activity", "State_ID", "county_allocation_factor"))
      ## Generate the composition by state
      print("Generate the fleet composition by state")
      fleet_vint_stock_state <- aggregate(.~Age + Year + Size + Technology + ST_FIPS, data = select(fleet_vint_stock_county, -c("FIPS")), FUN = sum)
      fleet_vint_scrap_state <- aggregate(.~Age + Year + Size + Technology + ST_FIPS, data = select(fleet_vint_scrap_county, -c("FIPS")), FUN = sum)
      print("Generate the total fleet composition")
      fleet_vint_stock <- aggregate(.~Age + Year + Size + Technology, data = select(fleet_vint_stock_state, -c("ST_FIPS")), FUN = sum)
      fleet_vint_scrap <- aggregate(.~Age + Year + Size + Technology, data = select(fleet_vint_scrap_state, -c("ST_FIPS")), FUN = sum)
    }
  }
  if (fleet_id == "no_ldvs") {
    fleet_vint_stock$Value[which(fleet_vint_stock$Year >= first_proj_yr+2)] <- 0
    fleet_vint_scrap$Value[which(fleet_vint_scrap$Year >= first_proj_yr+2)] <- 0
    fleet_vint_stock_state$Value[which(fleet_vint_stock_state$Year >= first_proj_yr+2)] <- 0
    fleet_vint_scrap_state$Value[which(fleet_vint_scrap_state$Year >= first_proj_yr+2)] <- 0
    fleet_vint_stock_county$Value[which(fleet_vint_stock_county$Year >= first_proj_yr+2)] <- 0
    fleet_vint_scrap_county$Value[which(fleet_vint_scrap_county$Year >= first_proj_yr+2)] <- 0
  }
  fleet_composition <- list(fleet_vint_stock = fleet_vint_stock,
                            fleet_vint_scrap = fleet_vint_scrap,
                            fleet_vint_stock_state = fleet_vint_stock_state,
                            fleet_vint_scrap_state = fleet_vint_scrap_state,
                            fleet_vint_stock_county = fleet_vint_stock_county,
                            fleet_vint_scrap_county = fleet_vint_scrap_county)
  print("Fleet composition dataset created")
  write.csv(fleet_composition[["fleet_vint_stock"]], paste0(results_path, "/fleet_composition.csv"))
  write.csv(fleet_composition[["fleet_vint_stock_state"]], paste0(results_path, "/fleet_composition_by_state.csv"))
  print("Fleet composition files exported")
  return(fleet_composition)
}
