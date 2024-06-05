#' lca_ef_efuel_elec_f
#' Function: Returns the annual life cycle GHG emission factors of electricity production associated with e-fuel production
#' @import modelframework
#' @import tidyr
#' @export
lca_ef_efuel_elec_f<-function(ghg=NA,first_yr=NA,last_yr=NA,ef_efuel_elec_mdl=NA,ef_efuel_elec_adj_coef=NA){
  attribute_f("lca_ef_efuel_elec_f")
  #Get emission factors calculated from sources
  res <- do.call(ef_elec_own_f,list(ghg=ghg))
  matrix_ef_elec <- res[["matrix_ef_elec"]]
  last_hist_yr <- 2020
  if (ef_efuel_elec_mdl=="constant"){
    matrix_ef_elec[,as.character(last_hist_yr:last_yr)] <- matrix_ef_elec[,as.character(last_hist_yr)]
  } else if (ef_efuel_elec_mdl=="linear"){
    matrix_ef_elec[,as.character(last_hist_yr:last_yr)] <- matrix_ef_elec[,as.character(last_hist_yr)]+ef_efuel_elec_adj_coef*0:(last_yr-last_hist_yr)
  } else if(ef_efuel_elec_mdl=="renewable"){
    matrix_ef_elec[,as.character(last_hist_yr:last_yr)] <- 0
  }
  #Output
  lca_ef_elec <- as.data.frame(matrix_ef_elec) %>% 
    cbind(Unit=paste("kg",ghg,"/ kWh"),stringsAsFactors = FALSE) %>% 
    gather("Year","Value",-Unit,convert=TRUE) %>%
    subset(.,Year%in%c(first_yr:last_yr))
  lca_ef_elec["GHG"] <- ghg
  return(list(lca_ef_elec=lca_ef_elec))
}
  
