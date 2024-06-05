#' fleet_demand_matrix_f
#' Function: Creates a matrix of demand by life cycle stage, process and phase. This matrix is multiplied by a matrix of GHG emission factors to obtain the U.S. LDV fleet GHG emissions.
#' @import tidyr
#' @importFrom reshape2 acast
#' @export
fleet_demand_matrix_f<-function(results_path, first_yr=NA,last_yr=NA, fuel_matching_option = NA, first_proj_yr = NA){
  attribute_f("fleet_demand_matrix_f")
  #Input files
  lca_process  <- get_input_f(input_name = 'lca_process')
  #Functions' Outputs
  fleet_vint_stock_f_res <- do.call(fun_res_f,list(fun_name="fleet_vint_stock_f"))
  fleet_vint_stock <- fleet_vint_stock_f_res[["fleet_vint_stock"]]
  fleet_vint_scrap <- fleet_vint_stock_f_res[["fleet_vint_scrap"]]
  fleet_tot_new <-  fleet_vint_stock %>% subset(Age==0 & Year%in%first_proj_yr:last_yr) %>% 
    aggregate(reformulate(c("Year"),response="Value"),data=.,FUN=sum)
  fleet_tot_scrap <-  fleet_vint_scrap %>% subset(Year%in%first_proj_yr:last_yr) %>% 
    aggregate(reformulate(c("Year"),response="Value"),data=.,FUN=sum)
  #
  vehicle_module_f_res <- do.call(fun_res_f,list(fun_name="vehicle_module_f"))
  if (fuel_matching_option == "hybrid") {
    correspondence_file <- read.delim(paste0(getwd(), "/inputs/data/FLAME_to_MOVES_correspondance_files/technologies_", fuel_matching_option, ".txt"), header = TRUE, sep = ";", dec = ".")
  } else if (fuel_matching_option == "FLAME") {
    # Placeholder for future developments, e.g. addition of hybrid vehicles
  } else if (fuel_matching_option == "MOVES") {
    # Placeholder for future developments, e.g. addition of hybrid vehicles
  }
  bat_wgt_dt <- vehicle_module_f_res[["fleet_mc_cpt_dt"]] %>% 
    filter(Technology%in%unique(correspondence_file$Technology_Alt)) %>%
    aggregate(Weight~Component+Model_year+Technology+Size,data=.,FUN=sum) %>%
    subset(Component=="EV Battery")
  #
  fleet_fuel_u_f_res <- do.call(fun_res_f,list(fun_name="fleet_fuel_u_f"))
  fleet_fuel_use_tot <- fleet_fuel_u_f_res[["fleet_fuel_use_tot"]]
  #
  fleet_efuel_f_res <- do.call(fun_res_f,list(fun_name="fleet_efuel_f"))
  fleet_efuel_dt <- fleet_efuel_f_res[["fleet_efuel_dt"]]
  fleet_efuel_feedstock_dt <- fleet_efuel_f_res[["fleet_efuel_feedstock_dt"]]
  #
  fleet_mfa_f_res <- do.call(fun_res_f,list(fun_name="fleet_mfa_f"))
  fleet_mfa_dt <- fleet_mfa_f_res[["fleet_mfa_dt"]]
  #Create the demand matrix
  fleet_demand_matrix <- matrix(0,ncol = length(first_proj_yr:last_yr),nrow = nrow(lca_process),dimnames = list(paste(lca_process$Phase,lca_process$Process),first_proj_yr:last_yr)) 
  #Fill demand matrix with primary material production
  prim_material_matrix <- fleet_mfa_dt %>% 
    subset(Name=="prim" & Year%in%first_proj_yr:last_yr) %>%
    acast(Material ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  fleet_demand_matrix[lca_process$Phase=="Primary Material Production",] <- prim_material_matrix[subset(lca_process,Phase=="Primary Material Production")$Process,]
  #Fill demand matrix with secondary material production
  sec_material_matrix <- fleet_mfa_dt %>% 
    subset(Name%in%c("sec_int","sec_ext") & Year%in%first_proj_yr:last_yr) %>%
    acast(Material ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  fleet_demand_matrix[lca_process$Phase=="Secondary Material Production",] <- sec_material_matrix[subset(lca_process,Phase=="Secondary Material Production")$Process,]
  #Fill demand matrix with vehicle manufacturing
  fleet_demand_matrix[lca_process$Process=="Vehicle Assembly",] <- fleet_tot_new$Value[order(fleet_tot_new$Year)]
  #Fill demand matrix with manufacturing materials
  material_demand_matrix <- fleet_mfa_dt %>% 
    subset(Name=="dem" & Year%in%first_proj_yr:last_yr) %>%
    acast(Material ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  fleet_demand_matrix[lca_process$Phase=="Manufacturing" & lca_process$Process!="Vehicle Assembly",] <- material_demand_matrix[subset(lca_process, Phase=="Manufacturing" & Process!="Vehicle Assembly")$Process,]
  #Fill demand matrix with battery total weight. Be careful, in current model, battery weight starts not at beginning
  bat_wgt_matrix <- bat_wgt_dt %>% 
    subset(Model_year%in%first_proj_yr:last_yr) %>%
    acast(Size + Technology ~ Model_year , value.var='Weight',fun.aggregate=sum, margins=FALSE)
  new_veh_matrix <- fleet_vint_stock %>%
    subset(Age==0 & Year%in%first_proj_yr:last_yr & Technology%in%bat_wgt_dt$Technology) %>%
    acast(Size + Technology ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  tot_bat_wgt_matrix <- (new_veh_matrix[rownames(bat_wgt_matrix),colnames(bat_wgt_matrix)] * bat_wgt_matrix) %>% colSums()
  fleet_demand_matrix[lca_process$Process=="Battery production",colnames(bat_wgt_matrix)] <- tot_bat_wgt_matrix
  fleet_demand_matrix[lca_process$Process=="Battery Assembly",colnames(bat_wgt_matrix)] <- tot_bat_wgt_matrix
  #Fill demand matrix with fuel use
  #first start with all fuels except gasoline fuels
  #if (unique(fleet_fuel_use_tot$Fuel) != "Gasoline") {
    fuel_use_matrix <- fleet_fuel_use_tot %>%
    subset(Year%in%first_proj_yr:last_yr & Fuel!="Gasoline") %>%
    acast(Fuel ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
    fleet_demand_matrix[lca_process$Phase=="Fuel Production" & lca_process$Process%in%rownames(fuel_use_matrix),] <- fuel_use_matrix[subset(lca_process, Phase=="Fuel Production" & lca_process$Process%in%rownames(fuel_use_matrix))$Process,colnames(fleet_demand_matrix)]
  #}
  
  #Second, gasoline and efuels
  fleet_demand_matrix[lca_process$Phase=="Fuel Production" & lca_process$Process=="Gasoline",] <- acast(data=subset(fleet_efuel_dt,Fuel=="Gasoline" & Year%in%first_proj_yr:last_yr),Fuel ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)[,colnames(fleet_demand_matrix)]
  fleet_demand_matrix[lca_process$Phase=="Fuel Production" & lca_process$Process=="E-fuel-Fuel synthesis",] <- acast(data=subset(fleet_efuel_dt,Fuel=="Efuel" & Year%in%first_proj_yr:last_yr),Fuel ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)[,colnames(fleet_demand_matrix)]
  fleet_demand_matrix[lca_process$Phase=="Fuel Production" & lca_process$Process=="E-fuel-Fuel distribution",] <- acast(data=subset(fleet_efuel_dt,Fuel=="Efuel" & Year%in%first_proj_yr:last_yr),Fuel ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)[,colnames(fleet_demand_matrix)]
  for (i in which(lca_process$Source=="own efuel")){
    fleet_demand_matrix[i,] <- acast(data=subset(fleet_efuel_feedstock_dt,Feedstock==lca_process[i,"GREET"] & Year%in%first_proj_yr:last_yr),Feedstock ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)[,colnames(fleet_demand_matrix)]
  }
  #Fuel use
  #if (unique(fleet_fuel_use_tot$Fuel) != "Gasoline") {
    fleet_demand_matrix[lca_process$Phase=="Fuel Use" & lca_process$Process%in%rownames(fuel_use_matrix),] <-  fuel_use_matrix[subset(lca_process, Phase=="Fuel Production" & lca_process$Process%in%rownames(fuel_use_matrix))$Process,colnames(fleet_demand_matrix)]
  #}
  fleet_demand_matrix[lca_process$Phase=="Fuel Use" & lca_process$Process=="Gasoline",] <- acast(data=subset(fleet_efuel_dt,Fuel=="Gasoline" & Year%in%first_proj_yr:last_yr),Fuel ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)[,colnames(fleet_demand_matrix)]
  #Fill demand matrix with vehicle disposal
  fleet_demand_matrix[lca_process$Process=="Vehicle Disposal",] <- fleet_tot_scrap$Value[order(fleet_tot_scrap$Year)]
  return(list(fleet_demand_matrix=fleet_demand_matrix))
}
