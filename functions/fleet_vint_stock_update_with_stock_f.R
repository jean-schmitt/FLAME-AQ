#' fleet_vint_stock_update_with_stock_f
#' Function: Update the vintaged fleet stock by vehicle type of a given year from previous year vintaged stock and total stock to match. New sales are estimated.
#' @export
#Require: Previous year vintaged stock, current year sales by type, current year stock by type
fleet_vint_stock_update_with_stock_f <- function(fleet,year, first_proj_yr = NA){
  attribute_f("fleet_vint_stock_update_with_stock_f")
  #Inputs
  #Other parameters
  #first_proj_yr <- 2021
  last_age_tbc <- 30
  #Survival rates matrix
  matrix_sur_rate <- t(sapply(rownames(fleet$ldv_sales),function(x) do.call(survival_rate_f,list(year=year, size=strsplit(x,"_")[[1]][1], techno=strsplit(x,"_")[[1]][2], cumulative_rate="n"))[1,]))
  #Create matrix_vint_stock
  matrix_vint_stock <- matrix(0,ncol = last_age_tbc+1,nrow = nrow(fleet$ldv_sales),dimnames = list(rownames(fleet$ldv_on_road_stock),0:last_age_tbc))
  #Update stock based on previous year matrix stock and survival rates
  matrix_vint_stock[,as.character(1:last_age_tbc)] <- fleet$vint_stock[[as.character(year-1)]][rownames(matrix_vint_stock),as.character(0:(last_age_tbc-1))] * matrix_sur_rate[rownames(matrix_vint_stock),as.character(1:(last_age_tbc))]
  #Update new vehicle sales: Get sales number from difference between on-road old vehicles and obj stock. 
  #Check that objective stock is higher than vintaged stock from last year. Otherwise, no new sales
  if (fleet$ldv_on_road_stock_tot["Total",as.character(year)]-sum(matrix_vint_stock[,as.character(1:last_age_tbc)])>=0){
    matrix_vint_stock[,"0"] <- fleet$technology_market_share[rownames(matrix_vint_stock),as.character(year)] * (fleet$ldv_on_road_stock_tot["Total",as.character(year)]-sum(matrix_vint_stock[,as.character(1:last_age_tbc)]))
  } else {
    fleet$ldv_on_road_stock_tot["Total",as.character(year)] <- sum(matrix_vint_stock[,as.character(1:last_age_tbc)])
  }
  #Calculate the matrix_scrap. Contains number of scrapped vehicles at age for the given year
  matrix_scrap <- fleet$vint_stock[[as.character(year-1)]][rownames(matrix_vint_stock),as.character(0:last_age_tbc)] - cbind(matrix_vint_stock[,as.character(1:last_age_tbc)],0)
  dimnames(matrix_scrap) <- list(rownames(matrix_vint_stock),1:(last_age_tbc+1))
  #Update fleet
  fleet$vint_stock[[as.character(year)]] <- trunc(matrix_vint_stock)
  fleet$vint_scrap[[as.character(year)]] <- trunc(matrix_scrap)
  #Update the current year on-road stock and sales with actual values
  fleet$ldv_on_road_stock[rownames(matrix_vint_stock),as.character(year)] <- trunc(rowSums(matrix_vint_stock))
  fleet$ldv_sales[rownames(matrix_vint_stock),as.character(year)] <- trunc(matrix_vint_stock[,"0"])
  return(fleet)
}
