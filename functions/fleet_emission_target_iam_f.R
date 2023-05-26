#' fleet_emission_target_iam_f
#' Function: Calculates GHG emission budgets for the US LDV fleet derived from the GCAM model that are consistent with given climate change targets
#' @export
fleet_emission_target_iam_f<-function(ssp_scen=NA,ssp_mitigation_scen=NA,last_yr=NA){
  #Assign default value
  attribute_f(fun_name = "fleet_emission_target_iam_f")
  #Inputs
  co2_emission_pathway  <- get_input_f(input_name = 'usa_carbon_budget_iam')
  #Other parameters
  first_data_yr <- 2015
  target_tbc <- switch(ssp_mitigation_scen,
                       "26"="2C",
                       "19"="1.5C")
  tot_budgets <- subset(co2_emission_pathway,Target==target_tbc & Year%in%c(first_data_yr:last_yr))
  quantile_tbc = 0.5
  fleet_emission_target <- data.frame(Year=first_data_yr:last_yr,
                                      Value=sapply(first_data_yr:last_yr,function(x)as.numeric(quantile(x=subset(tot_budgets,Year==x)$Value,probs=quantile_tbc)))*10^9,
                                      Unit="kg CO2")
  return(list(fleet_emission_target=fleet_emission_target))
}
