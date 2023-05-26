fleet_sales_f <- function (fleet_database) {
  fleet_composition <- fleet_database[which(fleet_database$Age == 0),]
  sizes <- unique(fleet_composition$Size)
  Market_Share <- data.frame(x1 = 1)
  m <- 0
  for (i in 1:length(sizes)) {
    temp <- fleet_composition[which(fleet_composition$Size == sizes[i]),]
    years <- sort(unique(temp$Year))
    technologies <- sort(unique(temp$Technology))
    for (j in 1:length(technologies)) {
      for (k in 1:length(years)) {
        m <- m+1
        Market_Share[m,1] <- years[k]
        Market_Share[m,2] <- technologies[j]
        Market_Share[m,3] <- sizes[i]
        if (!is.empty(which(temp$Technology == technologies[j] & temp$Year == years[k])) & !is.empty(which(temp$Year == years[k]))) {
          Market_Share[m,4] <- temp$Value[which(temp$Technology == technologies[j] & temp$Year == years[k])]/sum(temp$Value[which(temp$Year == years[k])])
        } else {
          Market_Share[m,4] <- 0
        }
      }
    }
  }
  colnames(Market_Share) <- c("Year", "Technology", "Size", "Market Share")
  return(Market_Share)
}
