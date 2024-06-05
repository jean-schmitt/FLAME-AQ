#' fleet_initialize_f
#' Function: Creates a fleetClass object and stored the first data.
#' @export
fleet_initialize_f <- function(first_yr=NA,last_yr=NA, fuel_matching_option=NA, fleet_id = NA){
  attribute_f("fleet_initialize_f")
  #Inputs
  vh_techno <- get_input_f(input_name = 'model_matching_technology')
  hist_stock_dt  <- get_input_f(input_name = 'fleet_stock_hist')
  hist_sales_dt  <- get_input_f(input_name = 'fleet_sales_hist')
  # Modify the technologies of vehicles depending on the scenario
  if (fuel_matching_option == "hybrid") {
    correspondence_file <- read.delim(paste0(getwd(), "/inputs/data/FLAME_to_MOVES_correspondance_files/technologies_", fuel_matching_option, ".txt"), header = TRUE, sep = ";", dec = ".")
  }
  hist_stock_dt <- left_join(hist_stock_dt, correspondence_file, by = "Technology") %>%
    select(-c("ID", "Technology")) %>%
    relocate("Technology_Alt", .before = "Size")
  colnames(hist_stock_dt)[1] <- "Technology"
  hist_stock_dt <- aggregate(.~Technology + Size + Year + Data_type + Unit, data = hist_stock_dt, FUN = sum)
  hist_sales_dt <- left_join(hist_sales_dt, correspondence_file, by = "Technology") %>%
    select(-c("ID", "Technology")) %>%
    relocate("Technology_Alt", .before = "Size")
  colnames(hist_sales_dt)[1] <- "Technology"
  hist_sales_dt <- aggregate(.~Technology + Size + Year + Data_type + Unit, data = hist_sales_dt, FUN = sum)
  ## Modify the historic stock and sales for the no EVs scenario
  if (tolower(fleet_id) == "no_evs") {
    stock_wo_bevs <- hist_stock_dt
    stock_wo_bevs$Technology[which(stock_wo_bevs$Technology%in%c("BEV100", "BEV300"))] <- "ICEV-G"
    hist_stock_dt <- left_join(hist_stock_dt, aggregate(.~Technology+Size+Year+Data_type+Unit, data = stock_wo_bevs, FUN = sum), by = c("Technology", "Size", "Year", "Data_type", "Unit"))
    hist_stock_dt$Value.x[which(hist_stock_dt$Technology == "ICEV-G")] <- hist_stock_dt$Value.y[which(hist_stock_dt$Technology == "ICEV-G")]
    hist_stock_dt$Value.x[which(hist_stock_dt$Technology%in%c("BEV100", "BEV300"))] <- 0
    hist_stock_dt <- select(hist_stock_dt, -c("Value.y"))
    colnames(hist_stock_dt)[6] <- "Value"
    sales_wo_bevs <- hist_sales_dt
    sales_wo_bevs$Technology[which(sales_wo_bevs$Technology%in%c("BEV100", "BEV300"))] <- "ICEV-G"
    hist_sales_dt <- left_join(hist_sales_dt, aggregate(.~Technology+Size+Year+Data_type+Unit, data = sales_wo_bevs, FUN = sum), by = c("Technology", "Size", "Year", "Data_type", "Unit"))
    hist_sales_dt$Value.x[which(hist_sales_dt$Technology == "ICEV-G")] <- hist_sales_dt$Value.y[which(hist_sales_dt$Technology == "ICEV-G")]
    hist_sales_dt$Value.x[which(hist_sales_dt$Technology%in%c("BEV100", "BEV300"))] <- 0
    hist_sales_dt <- select(hist_sales_dt, -c("Value.y"))
    colnames(hist_sales_dt)[6] <- "Value"
  }
  #Other parameters
  #i_year is the initilization year for the model. It has to be lower than firsT_yr. Preferably as old as the first stock and sales data to allow the model to correct the stocks
  i_year <- 1970
  first_proj_yr <- max(hist_stock_dt$Year)+1
  #Create historical matrix stock and sales
  matrix_stock_hist <- acast(data=subset(hist_stock_dt,Data_type=="stock"), Size + Technology ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  matrix_sales_hist <- acast(data=subset(hist_sales_dt,Data_type=="sales"), Size + Technology ~ Year , value.var='Value',fun.aggregate=sum, margins=FALSE)
  #Initialize the fleet class
  fleet <- new("fleetClass")
  fleet$ldv_on_road_stock <- matrix(NA,nrow=nrow(matrix_stock_hist),ncol=length(i_year:last_yr),dimnames=list(rownames(matrix_stock_hist),i_year:last_yr))
  fleet$ldv_on_road_stock[rownames(matrix_stock_hist),colnames(matrix_stock_hist)] <- matrix_stock_hist
  #
  fleet$ldv_on_road_stock_tot <- matrix(NA,nrow=1,ncol=length(i_year:last_yr),dimnames=list("Total",i_year:last_yr))
  fleet$ldv_on_road_stock_tot[,colnames(matrix_stock_hist)] <- colSums(matrix_stock_hist)
  #
  fleet$ldv_sales <- matrix(NA,nrow=nrow(matrix_sales_hist),ncol=length(i_year:last_yr),dimnames=list(rownames(matrix_sales_hist),i_year:last_yr))
  fleet$ldv_sales[rownames(matrix_sales_hist),colnames(matrix_sales_hist)] <- matrix_sales_hist
  #
  fleet$technology_market_share <- matrix(NA,nrow=nrow(matrix_sales_hist),ncol=length(i_year:last_yr),dimnames=list(rownames(matrix_sales_hist),i_year:last_yr))
  fleet$technology_market_share[rownames(matrix_sales_hist),colnames(matrix_sales_hist)] <- matrix_sales_hist %*% diag(x=1/colSums(matrix_sales_hist))
  return(fleet)
}
