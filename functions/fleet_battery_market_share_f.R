#' fleet_battery_market_share_f
#' Function: Calculates battery chemistry market share for each EV technology
#' @importFrom reshape2 acast
#' @import tidyr
#' @export
fleet_battery_market_share_f <- function(bat_ms_scen=NA,first_yr=NA,last_yr=NA){
  attribute_f("fleet_battery_market_share_f")
  #
  battery_market_share <- get_input_f(input_name = 'battery_market_share')
  #List of EV technologies
  ev_techno_l <- c("BEV100","BEV300","PHEV20","PHEV40")
  #Output dataframe
  fleet_battery_market_share_dt <- NULL
  for (techno in ev_techno_l){
    #Create matrix to populate
    mat_bat_ms <- matrix(NA,nrow=length(unique(battery_market_share$`Battery type`)),ncol=last_yr-first_yr+1,dimnames=list(unique(battery_market_share$`Battery type`),first_yr:last_yr))
    #Obtain historical data
    hist_mat_bat_ms <- subset(battery_market_share,Technology%in%c(techno,"all")) %>%
      acast(data=., `Battery type` ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
    #Populate matrix with historical data
    mat_bat_ms[,colnames(hist_mat_bat_ms)] <- hist_mat_bat_ms[rownames(mat_bat_ms),]/100
    #Complete historical data with latest data
    first_yr_data <- as.numeric(colnames(mat_bat_ms)[min(which(colSums(!is.na(mat_bat_ms))!=0))])
    mat_bat_ms[,as.character(first_yr:first_yr_data)] <- mat_bat_ms[,as.character(first_yr_data)]
    #Build prospective scenarios
    if(bat_ms_scen=="constant"){
      last_yr_data <- as.numeric(colnames(mat_bat_ms)[max(which(colSums(!is.na(mat_bat_ms))!=0))])
      mat_bat_ms[,as.character(last_yr_data:last_yr)] <- mat_bat_ms[,as.character(last_yr_data)]
    }
    #Save data
    tmp_dt <- as.data.frame(mat_bat_ms,stringsAsFactors = FALSE) %>%
      cbind(`Battery type`=rownames(mat_bat_ms),stringsAsFactors = FALSE) %>% 
      gather("Year","Value",-`Battery type`,convert=TRUE)
    tmp_dt[,"Technology"] <- techno
    fleet_battery_market_share_dt <- rbind(fleet_battery_market_share_dt,tmp_dt)
  }
  return(list(fleet_battery_market_share_dt=fleet_battery_market_share_dt))
}
