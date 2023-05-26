#' fleet_battery_flow_f
#' Function: Calculates material flow associated with batteries used by EVs.
#' @importFrom reshape2 acast
#' @import tidyr
#' @export
fleet_battery_flow_f <- function(bat_mat_comp_mdl=NA){
  attribute_f("fleet_battery_flow_f")
  #Inputs
  battery_material_composition  <- get_input_f(input_name = 'battery_material_composition')
  #Functions' Outputs
  #
  fleet_vint_stock_f_res <- do.call(fun_res_f,list(fun_name="fleet_vint_stock_f"))
  fleet_vint_stock <- fleet_vint_stock_f_res[["fleet_vint_stock"]]
  fleet_vint_scrap <- fleet_vint_stock_f_res[["fleet_vint_scrap"]]
  #
  vehicle_module_f_res <- do.call(fun_res_f,list(fun_name="vehicle_module_f"))
  fleet_veh_specs_dt <- vehicle_module_f_res[["fleet_veh_specs_dt"]]
  fleet_mc_cpt_dt <- vehicle_module_f_res[["fleet_mc_cpt_dt"]]
  #
  fleet_battery_market_share_f_res <- do.call(fun_res_f,list(fun_name="fleet_battery_market_share_f"))
  fleet_battery_market_share_dt <- fleet_battery_market_share_f_res[["fleet_battery_market_share_dt"]]
  #Calculate battery production flow (in GWh)
  year_tbc <- unique(fleet_mc_cpt_dt$Model_year)
  #ev_techno_l <- unique(subset(fleet_mc_cpt_dt,Subcomponent=="EV Battery")$Technology) #SOLVE ISSUE WITH BATTERY DENSITY OF FCV AND HEV
  ev_techno_l <- c("BEV100","BEV300","PHEV20","PHEV40")
  #Production of eVs: Unit = number of vehicles
  mat_ev_prod <- subset(fleet_vint_stock,Age==0 & Year%in%year_tbc & Technology%in%ev_techno_l) %>%
    acast(data=., paste0(Size,"_",Technology) ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  #Mass of battery by veh: Unit = kg
  mat_bat_wgt <- subset(fleet_mc_cpt_dt,Model_year%in%year_tbc & Technology%in%ev_techno_l & Subcomponent=="EV Battery") %>%
    acast(data=., paste0(Size,"_",Technology) ~ Model_year , value.var='Weight',fun.aggregate=sum, margins=FALSE)
  #Battery density: Unit = kWh/kg
  mat_bat_density <- subset(fleet_veh_specs_dt,Model_year%in%year_tbc & Technology%in%ev_techno_l & Attribute=="battery_density") %>%
    acast(data=., paste0(Size,"_",Technology) ~ Model_year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  #Calculate battery production in energy by technology: in GWh (converted from kWh to GWh)
  mat_bat_prod_en <- (mat_ev_prod * mat_bat_wgt[rownames(mat_ev_prod),colnames(mat_ev_prod)] * mat_bat_density[rownames(mat_ev_prod),colnames(mat_ev_prod)])*10^(-6)
  #Create final output
  fleet_bat_emb_mat <- NULL
  for (size in c("Car","Light truck")){
    for (techno in ev_techno_l){
      #Calculate battery production volume in mass. Unit = kg
      mat_bat_prod_wgt <- mat_ev_prod[paste0(size,"_",techno),] * mat_bat_wgt[paste0(size,"_",techno),colnames(mat_ev_prod)]
      #Battery market share
      mat_bat_ms <- subset(fleet_battery_market_share_dt,Technology==techno) %>%
        acast(data=., `Battery type` ~ Year , value.var="Value",fun.aggregate=sum, margins=FALSE)
      #Material composition by battery type
      mat_bat_mat_comp <- subset(battery_material_composition) %>%
        acast(data=., Metal ~ `Battery type` , value.var="Value",fun.aggregate=sum, margins=FALSE)
      #Sales-weighted material composition
      mat_bat_mat_comp_sw <- mat_bat_mat_comp %*% mat_bat_ms[colnames(mat_bat_mat_comp),]
      #Calculate battery material embodied: Unit = kg
      mat_bat_emb_mat <- mat_bat_mat_comp_sw %*% diag(x=mat_bat_prod_wgt,nrow=ncol(mat_bat_mat_comp_sw),ncol=ncol(mat_bat_mat_comp_sw))
      colnames(mat_bat_emb_mat) <- colnames(mat_bat_mat_comp_sw)
      #Save output in temporary dataframe
      tmp_dt <- as.data.frame(mat_bat_emb_mat,stringsAsFactors = FALSE) %>%
        cbind(Metal=rownames(mat_bat_emb_mat),stringsAsFactors = FALSE) %>% 
        gather("Year","Value",-Metal,convert=TRUE)
      tmp_dt[,"Size"] <- size
      tmp_dt[,"Technology"] <- techno
      #Bind with final output
      fleet_bat_emb_mat <- rbind(fleet_bat_emb_mat,tmp_dt)
    }
  }
  #Format results
  #
  fleet_bat_prod_cap <- as.data.frame(mat_bat_prod_en,stringsAsFactors = FALSE) %>%
    cbind(Type=rownames(mat_bat_prod_en),stringsAsFactors = FALSE) %>% 
    gather("Year","Value",-Type,convert=TRUE)
  fleet_bat_prod_cap[,"Size"] <- substr(fleet_bat_prod_cap$Type,0,as.numeric(regexpr(pattern="_",fleet_bat_prod_cap$Type))-1)
  fleet_bat_prod_cap[,"Technology"] <- substr(fleet_bat_prod_cap$Type,as.numeric(regexpr(pattern="_",fleet_bat_prod_cap$Type))+1,200)
  fleet_bat_prod_cap[,"Type"] <- NULL
  fleet_bat_prod_cap[,"Unit"] <- "GWh"
  #
  fleet_bat_emb_mat[,"Unit"] <- "kg"
  return(list(fleet_bat_prod_cap_dt=fleet_bat_prod_cap,fleet_bat_emb_mat_dt=fleet_bat_emb_mat))
}
