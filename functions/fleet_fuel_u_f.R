#' fleet_fuel_u_f
#' Function: Calculates on-road fuel use by LDVs
#' @export
fleet_fuel_u_f<-function (first_yr = NA,last_yr = NA, fc_deg=NA, fc_deg_mdl = NA, vkt_turnover_adj_mdl = NA, survival_rate_adj_age = NA, scenario_id = NA, fleet_id = NA, fuel_matching_option = NA){
  attribute_f("fleet_fuel_u_f")
  results_path <- paste0(getwd(), "/outputs/air_quality/", Sys.Date(), "_", scenario_id, "_results")
  #Inputs files
  vh_techno <- get_input_f(input_name = "model_matching_technology")
  if (fuel_matching_option == "hybrid") {
    correspondence_file <- read.delim(paste0(getwd(), "/inputs/data/FLAME_to_MOVES_correspondance_files/technologies_", fuel_matching_option, ".txt"), header = TRUE, sep = ";", dec = ".")
  } else if (fuel_matching_option == "FLAME") {
    
  } else if (fuel_matching_option == "MOVES") {
    
  }
  vh_techno <- vh_techno[which(vh_techno$Own%in%unique(correspondence_file$Technology_Alt)),]
  fc_degradation <- get_input_f(input_name = "fuel_consumption_degradation")
  #Functions' Outputs
  vehicle_module_f_res <- do.call(fun_res_f,list(fun_name="vehicle_module_f"))
  fleet_fc_dt <- vehicle_module_f_res[["fleet_fc_dt"]]
  fleet_uf_dt <- vehicle_module_f_res[["fleet_uf_dt"]]
  #fleet_vkt
  fleet_vkt_f_res <- do.call(fun_res_f,list(fun_name="fleet_vkt_f"))
  fleet_vint_vkt <- fleet_vkt_f_res[["fleet_vint_vkt"]]
  if (vkt_turnover_adj_mdl=="y" & survival_rate_adj_age!=0){
    #Get total fleet VKT from current simulations
    tot_vkt <- aggregate(formula = Value~Year,data = fleet_vint_vkt,FUN=sum)
    #Simulate fleet with no turnover
    #Update attribute value with no turnover adjustments
    update_attribute_values(list(survival_rate_adj_age=0))
    new_fleet_vkt_f_res <- do.call(fleet_vkt_f,list(use_res_env="n"))
    new_fleet_vint_vkt <- new_fleet_vkt_f_res[["fleet_vint_vkt"]]
    new_tot_vkt <- aggregate(formula = Value~Year,data = new_fleet_vint_vkt,FUN=sum)
    #Calculate adjustment ratio 
    tot_vkt_adj_ratio <- tot_vkt
    tot_vkt_adj_ratio$Value <- sapply(1:nrow(tot_vkt_adj_ratio),function(x)subset(new_tot_vkt,Year==tot_vkt_adj_ratio[x,"Year"])$Value/subset(tot_vkt,Year==tot_vkt_adj_ratio[x,"Year"])$Value)
    matrix_vkt_adj_ratio <- acast(data=tot_vkt_adj_ratio, Year ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
    #Update attribute value with no turnover adjustments
    update_attribute_values(list(survival_rate_adj_age=survival_rate_adj_age))
  } else {
    matrix_vkt_adj_ratio <- diag(1,ncol = length(unique(fleet_vint_vkt$Year)),nrow = length(unique(fleet_vint_vkt$Year)))
  }
  #Creation output files
  #fuel_use is the data.frame with the fuel use per year, scenario and fuel
  dt_col <- c("Year","Age","Size","Technology","Fuel","Unit","Value")
  fleet_vint_fuel_use <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
  #Loop for size
  #for (size in c("Car", "Light truck")) {
  for (size in unique(fleet_vint_vkt$Size)) {
    #Loop for technology
    #for (techno in unique(vh_techno$Own)) {
    for (techno in unique(fleet_vint_vkt$Technology)) {
      #Loop for fuel_type
      #model_year_list is the list of Model Years we should consider in the matrices
      model_year_list <- unique(subset(fleet_fc_dt, Size==size & Technology==techno)$Model_year)[order(unique(subset(fleet_fc_dt, Size==size & Technology==techno)$Model_year))]
      #Create matrix of vintaged vkt. Rows: Year. Cols: Model year. Unit: km. If model year is older than the minimum model year data, assume it is the oldest
      stock_vkt <- subset(fleet_vint_vkt,Size==size & Technology == techno & Year%in%c(first_yr:last_yr))
      stock_vkt[,"Model_year"] <- sapply(stock_vkt[,"Year"] - stock_vkt[,"Age"],function(x) ifelse(x < min(model_year_list),min(model_year_list),x))
      #tmp_mat_vkt is a temporary matrix. Dimensions may not correspond to final format
      tmp_mat_vkt <- acast(stock_vkt, Year ~ Model_year , value.var='Value',fun.aggregate=sum, margins=FALSE)
      #matrix_vkt contains the vkt by year and model year in final format
      matrix_vkt <- matrix(0,ncol = length(model_year_list),nrow = length(first_yr:last_yr),dimnames = list(first_yr:last_yr,model_year_list))
      matrix_vkt[rownames(tmp_mat_vkt),colnames(tmp_mat_vkt)] <- matrix_vkt_adj_ratio %*% tmp_mat_vkt
      #Fuel types per technology
      fuel_l <- unlist(strsplit(vh_techno$`Fuel type`[which(vh_techno$Own == techno)][1], ";"))
      for (fuel_type in fuel_l) {
        fuel_unit <- subset(vh_techno,`Fuel type`==fuel_type)[1,'Fuel unit']
        #Create matrix of fuel Consumption. Rows = Year. Columns = Model year. Unit: fuel unit / 100 km
        fc_dt <- subset(fleet_fc_dt, Size==size & Technology==techno & Fuel==fuel_type)
        #matrix_fc is the matrix of fuel consumption ratins
        matrix_fc <- matrix(fc_dt$Value[order(fc_dt$Model_year)],ncol = length(model_year_list),nrow = length(first_yr:last_yr),byrow = TRUE,dimnames = list(first_yr:last_yr,model_year_list))
        #mat_deg is the matrix of fuel consumption degradation for all years and model years. Assumed to be 1 (no degradation)
        mat_deg <- matrix(1,ncol = length(model_year_list),nrow = length(first_yr:last_yr),dimnames = list(first_yr:last_yr,model_year_list))
        #If degradation to be included
        if (fc_deg!="n" & nrow(subset(fc_degradation,techno%in%unlist(strsplit(Technology,";")) & Fuel==fuel_type))>0){
          #Only consider the degradation specific to the technology
          tmp_fc_degradation <- subset(fc_degradation,techno%in%unlist(strsplit(Technology,";")) & Fuel==fuel_type)
          #max_age is the maximum age available in the data
          max_age <- max(tmp_fc_degradation$Age)
          #tmp_mat_fc_deg is a temporary matrix of degradation factors by age (up to max_age)
          tmp_mat_fc_deg <- acast(tmp_fc_degradation,Age ~ Technology,value.var = fc_deg)
          #mat_age_deg is the matrix of degradation factors by age (up to 30)
          mat_age_deg <- matrix(1,nrow=31,ncol=1,dimnames = list(0:30,"Degradation"))
          mat_age_deg[rownames(tmp_mat_fc_deg),1] <- tmp_mat_fc_deg[,1]
          #Create rate for prospective values (after max_age)
          if (fc_deg_mdl=="constant"){
            fc_deg_rate <- 0
          } else if (fc_deg_mdl=="linear"){
            fc_deg_rate <- (mat_age_deg[as.character(max_age),1]-mat_age_deg[as.character(0),1])/max_age
          }
          #Project the values
          mat_age_deg[as.character((max_age+1):30),1] <- mat_age_deg[as.character(max_age),1]+(1:(30-max_age))*fc_deg_rate
          #Update mat_deg
          mat_deg[,] <- as.matrix(sapply(colnames(mat_deg),function(x)sapply(rownames(mat_deg),function(y)ifelse(as.numeric(y)-as.numeric(x)>0,ifelse(as.numeric(y)-as.numeric(x)<31,mat_age_deg[as.character(as.numeric(y)-as.numeric(x)),1],mat_age_deg["30",1]),1))))
        }
        matrix_fc_deg <- matrix_fc * mat_deg
        #Create matrix of VKT share. Diagonal matrix with the share of vkt on fuel_type by model year
        vkt_share <- subset(fleet_uf_dt,Size==size & Technology==techno & Fuel==fuel_type & Model_year%in%model_year_list)
        matrix_vkt_share <- diag(x=vkt_share$Value[order(vkt_share$Model_year)],nrow=length(vkt_share$Model_year),ncol=length(vkt_share$Model_year))
        dimnames(matrix_vkt_share) <- list(vkt_share$Model_year,vkt_share$Model_year)
        #Calculate the fuel use by technology
        matrix_fuel_use <- (matrix_vkt %*% matrix_vkt_share) * matrix_fc_deg/100
        #Convert matrix into long table
        tmp_fuel_use_dt <- as.data.frame(matrix_fuel_use) %>% 
          cbind(Year=as.numeric(rownames(matrix_fuel_use)),stringsAsFactors = FALSE) %>% 
          gather("Model_year","Value",-Year,convert=TRUE) %>% 
          cbind(Technology=techno,Size=size,Fuel=fuel_type,Unit=fuel_unit,stringsAsFactors = FALSE)
        tmp_fuel_use_dt[,"Age"] <- tmp_fuel_use_dt[,"Year"] - tmp_fuel_use_dt[,"Model_year"]
        #Combine
        fleet_vint_fuel_use <- rbind(fleet_vint_fuel_use,subset(tmp_fuel_use_dt,Age %in% c(0:30),-Model_year))
      }
    }
  }
  # Calculation of the fuel use at the county, state, and national level
  print("Calculation of the fuel use at the county, state, and national level")
  per_vehicle_fuel_use <- left_join(fleet_vkt_f_res[["fleet_composition"]][["fleet_vint_stock"]], fleet_vint_fuel_use, by = c("Age", "Year", "Size", "Technology")) %>%
    add_column("Value_per_vehicle" = 0)
  per_vehicle_fuel_use$Value_per_vehicle <- per_vehicle_fuel_use$Value.y/per_vehicle_fuel_use$Value.x
  per_vehicle_fuel_use <- select(per_vehicle_fuel_use, -c("Value.x", "Value.y"))
  per_vehicle_fuel_use$Value_per_vehicle[which(is.nan(per_vehicle_fuel_use$Value_per_vehicle))] <- 0
  fleet_vint_fuel_use_state <- left_join(fleet_vkt_f_res[["fleet_composition"]][["fleet_vint_stock_state"]], per_vehicle_fuel_use, by = c("Age", "Year", "Size", "Technology"))
  fleet_vint_fuel_use_state$Value <- fleet_vint_fuel_use_state$Value*fleet_vint_fuel_use_state$Value_per_vehicle
  fleet_vint_fuel_use_county <- left_join(fleet_vkt_f_res[["fleet_composition"]][["fleet_vint_stock_county"]], per_vehicle_fuel_use, by = c("Age", "Year", "Size", "Technology"))
  fleet_vint_fuel_use_county$Value <- fleet_vint_fuel_use_county$Value*fleet_vint_fuel_use_county$Value_per_vehicle
  fleet_fuel_use_tot_county <- aggregate(.~ Fuel + Year + FIPS + ST_FIPS + Unit, data = select(fleet_vint_fuel_use_county, -c("Age", "Size", "Technology", "Value_per_vehicle")), FUN = sum)
  fleet_fuel_use_tot_state <- aggregate(.~ Fuel + Year + ST_FIPS + Unit, data = select(fleet_vint_fuel_use_state, -c("Age", "Size", "Technology", "Value_per_vehicle")), FUN = sum)
  fleet_fuel_use_tot <- aggregate(data = fleet_vint_fuel_use,Value ~ Fuel + Year + Unit,FUN=sum)
  print("Calculation of the electricity use at the county, state, and national level")
  fleet_elec_use_tot_county <- filter(fleet_fuel_use_tot_county, fleet_fuel_use_tot_county$Fuel == "Electricity")
  fleet_elec_use_tot_state <- filter(fleet_fuel_use_tot_state, fleet_fuel_use_tot_state$Fuel == "Electricity")
  fleet_elec_use_tot <- filter(fleet_fuel_use_tot, fleet_fuel_use_tot$Fuel == "Electricity")
  ## Generate the final results
  results<-list(fleet_vint_fuel_use = fleet_vint_fuel_use,
                fleet_fuel_use_tot = fleet_fuel_use_tot,
                fleet_fuel_use_tot_state = fleet_fuel_use_tot_state,
                fleet_fuel_use_tot_county = fleet_fuel_use_tot_county,
                fleet_elec_use_tot = fleet_elec_use_tot,
                fleet_elec_use_tot_state = fleet_elec_use_tot_state,
                fleet_elec_use_tot_county = fleet_elec_use_tot_county,
                fleet_composition = fleet_vkt_f_res[["fleet_composition"]])
  return(results)
}
