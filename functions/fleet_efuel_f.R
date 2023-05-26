#' fleet_efuel_f
#' Function: Calculate the amount of e-fuels
#' @export
fleet_efuel_f<-function (efuel_dep_mdl=NA,efuel_dep_rate=NA,efuel_param_scen=NA){
  attribute_f("fleet_efuel_f")
  #Inputs
  fuel_conversion  <- get_input_f(input_name = 'fuel_conversion')
  #efuel_adj_factor is an adjustment factors that converts 1 L of gasoline into the equivalent volume of efuel with the same energy. Unit : L(Efuel)/L(Gasoline)
  efuel_adj_factor <- subset(fuel_conversion,Fuel=="Gasoline" & Unit=="J/L")$value/subset(fuel_conversion,Fuel=="Efuel" & Unit=="J/L")$value
  efuel_parameters  <- get_input_f(input_name = 'efuel_parameters')
  #Select only the interesting parameters
  efuel_parameters <- subset(efuel_parameters,Scenario==efuel_param_scen)
  #Function's results
  fleet_fuel_u_f_res <- do.call(fun_res_f,list(fun_name="fleet_fuel_u_f"))
  fleet_fuel_use_tot <- fleet_fuel_u_f_res[["fleet_fuel_use_tot"]]
  #mat_ff is the matrix of fossil fuels (gasoline) use. Unit = L.
  mat_ff <- acast(data=subset(fleet_fuel_use_tot,Fuel=="Gasoline"), Fuel ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  #mat_efuel_dep is the matrix of efuel deployment. It is a ratio of total gasoline fuel.
  mat_efuel_dep <- matrix(0,nrow=1,ncol=ncol(mat_ff),dimnames=list("Efuel ratio",colnames(mat_ff)))
  #Last 
  last_hist_yr <- 2021
  last_yr <- max(as.numeric(colnames(mat_efuel_dep)))
  if (efuel_dep_mdl=="y"){
    mat_efuel_dep[,as.character((last_hist_yr+1):last_yr)] <- sapply(efuel_dep_rate*1:(last_yr-last_hist_yr),function(x)ifelse(x<=1,x,1))
  }
  #mat_efuel is the matrix of efuel volume. Unit = L. We adjust by energy density to ensure equivalent energy output
  mat_efuel <- mat_ff*efuel_adj_factor*mat_efuel_dep
  #mat_ff_after_efuel is the matrix of remaining gasoline. Unit = L
  mat_ff_after_efuel <- mat_ff*(1-mat_efuel_dep)
  #Convert matrices into dataframe
  fleet_efuel_dt <- rbind(as.data.frame(mat_efuel) %>% 
                            cbind(Fuel="Efuel",stringsAsFactors = FALSE) %>% 
                            gather("Year","Value",-Fuel,convert=TRUE),
                          as.data.frame(mat_ff_after_efuel) %>% 
                            cbind(Fuel="Gasoline",stringsAsFactors = FALSE) %>% 
                            gather("Year","Value",-Fuel,convert=TRUE))
  fleet_efuel_dt[,"Unit"] <- "L"
  #Calculate the associated feedstock and electricity consumption
  feedstock_list <- subset(efuel_parameters)$Feedstock
  #mat_efuel_feedstock_factor is the matrix of factors that convert the efuel volumes into feedstock
  mat_efuel_feedstock_factor <- matrix(subset(efuel_parameters)$Value,nrow=length(feedstock_list),ncol=ncol(mat_efuel),dimnames=list(feedstock_list,colnames(mat_ff)))
  ##mat_efuel_feedstock is the matrix of feedstock needed to fulfill the volume of efuel
  mat_efuel_feedstock <- mat_efuel_feedstock_factor * matrix(mat_efuel[1,],nrow=length(feedstock_list),ncol=ncol(mat_efuel),byrow = TRUE)
  #fleet_efuel_feedstock_dt contains the data frame of the requried feedstock to produce the efuel
  fleet_efuel_feedstock_dt <- as.data.frame(mat_efuel_feedstock) %>% 
    cbind(Feedstock=rownames(mat_efuel_feedstock),stringsAsFactors = FALSE) %>% 
    gather("Year","Value",-Feedstock,convert=TRUE)
  fleet_efuel_feedstock_dt[,"Unit"] <- sapply(fleet_efuel_feedstock_dt[,"Feedstock"],function(x)gsub("/L","",subset(efuel_parameters,Feedstock==x)$Unit))
  results<-list(fleet_efuel_dt=fleet_efuel_dt,fleet_efuel_feedstock_dt=fleet_efuel_feedstock_dt)
}
