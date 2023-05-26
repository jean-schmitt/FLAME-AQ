#' vehicle_gwp_f
#' Function: Calculate life cycle Global Warming Potential of U.S. LDVs by life cycle processes, stages and GHGs.
#' @import tidyr
#' @export
vehicle_gwp_f <- function(gwp_th=NA,gwp_cc_fb=NA){
  attribute_f("vehicle_gwp_f")
  #Get LC Inventory by GHG
  vehicle_lci_f_res <- do.call(fun_res_f,list(fun_name="vehicle_lci_f"))
  vehicle_lci <- vehicle_lci_f_res[["vehicle_lci"]]
  #Input
  ipcc_gwp = get_input_f(input_name = "ipcc_gwp")
  #Create output vehicle_lci
  vehicle_gwp <- vehicle_lci
  #Characterize the GHGs
  vehicle_gwp$Value <- sapply(1:nrow(vehicle_gwp),function(x)ifelse(vehicle_gwp[x,"GHG"]=="CO2",vehicle_gwp[x,"Value"],vehicle_gwp[x,"Value"]*subset(ipcc_gwp,Time_horizon==gwp_th & cc_fb==gwp_cc_fb & GHG==vehicle_gwp[x,"GHG"])$Value))
  vehicle_gwp$Unit <- "kg CO2 eq."
  return(list(vehicle_gwp=vehicle_gwp))
}
