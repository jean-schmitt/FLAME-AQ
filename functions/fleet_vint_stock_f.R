#' fleet_vint_stock_f
#' Function: Calculates the vintaged stock of the U.S. LDVs by vehicle technology, size and model year.
#' @importFrom reshape2 acast
#' @export
fleet_vint_stock_f <- function(first_yr=NA,last_yr=NA,fleet_vint_procedure=NA,fleet_tot_stock=NA,aeo_scen=NA, fleet_composition_source=NA, scenario_id = NA, first_proj_yr = NA, fleet_id = NA){
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
        #Create the matrix of total stock
        matrix_tot_stock_proj <- acast(data=subset(proj_stock_aeo,Aeo_case==aeo_scen & Year >= first_proj_yr), Data_type ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
        #Update prospective stock
        fleet$ldv_on_road_stock_tot["Total",as.character(first_proj_yr:last_yr)] <- matrix_tot_stock_proj[,as.character(first_proj_yr:last_yr)]
      }
      #Update technology market share. Keep market share of AEO.
      fleet_technology_market_share <- do.call(fun_res_f,list(fun_name="fleet_technology_market_share_proj_f"))[["fleet_technology_market_share"]]
      fleet$technology_market_share[rownames(fleet_technology_market_share),colnames(fleet_technology_market_share)] <- fleet_technology_market_share
      #Calculate the vintaged stocks year by year from total stocks.
      for (year in first_proj_yr:last_yr){
        fleet <- do.call(fleet_vint_stock_update_with_stock_f,list(fleet=fleet,year=year))
      }
    }
  } else if (fleet_composition_source == "custom") {
  }
  print("Base fleet created, custom scenario will be applied")
  if (fleet_id != "2020_fleet") {
    fleet_data <- do.call(fleet_composition_scenario_f, list(fleet = fleet))
    fleet_composition <- list(fleet_vint_stock = fleet_data[["fleet_vint_stock"]], 
                              fleet_vint_scrap = fleet_data[["fleet_vint_scrap"]], 
                              fleet_vint_stock_scenario_state_breakdown = fleet_data[["fleet_vint_stock_scenario_state_breakdown"]], 
                              fleet_vint_scrap_scenario_state_breakdown = fleet_data[["fleet_vint_scrap_scenario_state_breakdown"]])
    fleet_composition_state_breakdown <- list(fleet_vint_stock_scenario_state_breakdown = fleet_data[["fleet_vint_stock_scenario_state_breakdown"]], fleet_vint_scrap_scenario_state_breakdown = fleet_data[["fleet_vint_scrap_scenario_state_breakdown"]])
  } else {
    population_distribution_states <- get_input_f(input_name = 'vehicle_population_by_state')
    states <- unique(population_distribution_states$State)
    fleet_composition_stock <- fleet$get_list_dataframe()[["fleet_vint_stock"]]
    fleet_composition_stock <- filter(fleet_composition_stock, fleet_composition_stock$Year <= first_proj_yr)
    fleet_composition_stock <- add_column(fleet_composition_stock, "Model_Year" = NA)
    fleet_composition_stock$Model_Year <- fleet_composition_stock$Year-fleet_composition_stock$Age
    fleet_composition_scrap <- fleet$get_list_dataframe()[["fleet_vint_scrap"]]
    fleet_composition_scrap <- filter(fleet_composition_scrap, fleet_composition_scrap$Year <= first_proj_yr)
    fleet_composition_scrap <- add_column(fleet_composition_scrap, "Model_Year" = NA)
    fleet_composition_scrap$Model_Year <- fleet_composition_scrap$Year-fleet_composition_scrap$Age
    for (i in (first_proj_yr+1):last_yr) {
      temp_stock <- fleet_composition_stock[which(fleet_composition_stock$Year == first_proj_yr),]
      temp_scrap <- fleet_composition_scrap[which(fleet_composition_scrap$Year == first_proj_yr),]
      temp_stock$Year <- i
      temp_scrap$Year <- i
      #temp_stock$Age <- temp_stock$Age+(i-(first_proj_yr+1))
      #temp_scrap$Age <- temp_scrap$Age+(i-(first_proj_yr+1))
      fleet_composition_stock <- rbind(fleet_composition_stock, temp_stock)
      fleet_composition_scrap <- rbind(fleet_composition_scrap, temp_scrap)
    }
    rownames(fleet_composition_stock) <- NULL
    rownames(fleet_composition_scrap) <- NULL
    for (i in states) {
      fleet_composition_stock_state_temp <- fleet_composition_stock %>%
        add_column("State" = NA)
      fleet_composition_scrap_state_temp <- fleet_composition_scrap %>%
        add_column("State" = NA)
      fleet_composition_stock_state_temp$State <- i
      fleet_composition_stock_state_temp$Value <- fleet_composition_stock_state_temp$Value*population_distribution_states$Activity[which(population_distribution_states$State == i)]
      fleet_composition_scrap_state_temp$State <- i
      fleet_composition_scrap_state_temp$Value <- fleet_composition_scrap_state_temp$Value*population_distribution_states$Activity[which(population_distribution_states$State == i)]
      if (i == states[1]) {
        fleet_composition_stock_state <- fleet_composition_stock_state_temp
        fleet_composition_scrap_state <- fleet_composition_scrap_state_temp
      } else {
        fleet_composition_stock_state <- rbind(fleet_composition_stock_state, fleet_composition_stock_state_temp)
        fleet_composition_scrap_state <- rbind(fleet_composition_scrap_state, fleet_composition_scrap_state_temp)
      }
    }
    fleet_composition_stock_state$Value <- round(fleet_composition_stock_state$Value)
    fleet_composition_scrap_state$Value <- round(fleet_composition_scrap_state$Value)
    fleet_composition_state_breakdown <- list(fleet_vint_stock_scenario_state_breakdown = fleet_composition_stock_state, fleet_vint_stock_scenario_scrap_breakdown = fleet_composition_scrap_state)
    fleet_composition <- list(fleet_vint_stock = fleet_composition_stock, fleet_vint_scrap = fleet_composition_scrap, fleet_vint_stock_scenario_state_breakdown = fleet_composition_stock_state, fleet_vint_scrap_scenario_state_breakdown = fleet_composition_scrap_state)
  }
  print("Fleet composition dataset created")
  write.csv(fleet_composition[["fleet_vint_stock"]], paste0(results_path, "/fleet_composition.csv"))
  write.csv(fleet_composition_state_breakdown[["fleet_vint_stock_scenario_state_breakdown"]], paste0(results_path, "/fleet_composition_by_state.csv"))
  print("Fleet composition files exported")
  # Returns the fleet composition dataset
  return(fleet_composition)
  # Code self-validation
  do.call(verification_fleet_composition_f, list(fleet = fleet, fleet_composition = fleet_composition))
}
