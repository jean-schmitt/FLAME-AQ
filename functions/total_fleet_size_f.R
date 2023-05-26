total_fleet_size_f <- function(fleet_vint_stock) {
  vehicles_sizes <- unique(fleet_vint_stock$Size)
  years <- unique(fleet_vint_stock$Year)
  total_fleet_size <- data.frame()
  for (i in 1:length(years)) {
    for (j in 1:length(vehicles_sizes)) {
      total_fleet_size[i,j] <- sum(fleet_vint_stock$Value[which(fleet_vint_stock$Year == years[i] & fleet_vint_stock$Size == vehicles_sizes[j])])
    }
  }
  total_fleet_size <- add_column(total_fleet_size, "Total" = total_fleet_size$V1+total_fleet_size$V2)
  colnames(total_fleet_size) <- c("Car", "Light truck", "Total")
  rownames(total_fleet_size) <- years
  return(total_fleet_size)
}
