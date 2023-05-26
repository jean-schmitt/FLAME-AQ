# Calculates the absolute and relative market share for new sales
market_share_matrix_f <- function(fleet_vint_stock) {
  years <- unique(fleet_vint_stock$Year)
  sizes <- unique(fleet_vint_stock$Size)
  technologies <- unique(fleet_vint_stock$Technology)
  market_share_matrix_abs <- data.frame()
  market_share_matrix_rel <- data.frame()
  row_names <- data.frame()
  vehicles_sizes <- c("car", "light truck")
  for (i in 1:length(years)) {
    total_sales <- sum(fleet_vint_stock$Value[which(fleet_vint_stock$Age == 0 & fleet_vint_stock$Year == years[i])])
    m <- 0
    for (j in 1:length(sizes)) {
      for (k in 1:length(technologies)) {
        m <- m+1
        market_share_matrix_abs[m,i] <- sum(fleet_vint_stock$Value[which(fleet_vint_stock$Age == 0 & fleet_vint_stock$Year == years[i] & fleet_vint_stock$Size == sizes[j] & fleet_vint_stock$Technology == technologies[k])])
        market_share_matrix_rel[m,i] <- market_share_matrix_abs[m,i]/total_sales
        row_names[m,1] <- paste0(sizes[j], "_", technologies[k])
      }
    }
  }
  colnames(market_share_matrix_abs) <- years
  colnames(market_share_matrix_rel) <- years
  rownames(market_share_matrix_abs) <- row_names[,1]
  rownames(market_share_matrix_rel) <- row_names[,1]
  #for (i in 1:length(vehicles_sizes)) {
  #  market_share_matrix_abs[nrow(market_share_matrix_abs)+1,]<-0
  #  rownames(market_share_matrix_abs)[nrow(market_share_matrix_abs)] <- paste0(vehicles_sizes[i], "_All")
  #  market_share_matrix_rel[nrow(market_share_matrix_rel)+1,]<-0
  #  rownames(market_share_matrix_rel)[nrow(market_share_matrix_rel)] <- paste0(vehicles_sizes[i], "_All")
  #  for (j in 1:length(colnames(market_share_matrix_abs))) {
  #    market_share_matrix_abs[which(rownames(market_share_matrix_abs) == paste0(vehicles_sizes[i], "_All")),j] <- sum(market_share_matrix_abs[which(grepl(vehicles_sizes[i], rownames(market_share_matrix_abs))),j])
  #    market_share_matrix_rel[which(rownames(market_share_matrix_rel) == paste0(vehicles_sizes[i], "_All")),j] <- sum(market_share_matrix_rel[which(grepl(vehicles_sizes[i], rownames(market_share_matrix_rel))),j])
  #  }
  #}
  return(list(market_share_abs = market_share_matrix_abs, market_share_rel = market_share_matrix_rel))
}
