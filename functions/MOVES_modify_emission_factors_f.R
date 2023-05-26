MOVES_modify_emission_factors_f <- function() {
  emission_factors <- read.csv(paste0(getwd(), "/inputs/moves/MOVES_base_emissionrate.csv"), sep = ",")
  pm10emissionratio <- read.csv(paste0(getwd(), "/inputs/moves/MOVES_base_pm10emissionratio.csv"), sep = ",")
  emission_factors$sourceBinID <- emission_factors$sourceBinID/10^9
  mod_PM10_brake <- 0.75
  mod_PM25_brake <- 0.75
  mod_PM10_tire <- 1.27
  mod_PM25_tire <- 1.18
  pm10ratio_brake <- mod_PM10_brake/mod_PM25_brake
  pm10ratio_tire <- mod_PM10_tire/mod_PM25_tire
  # Modify Brake PM2.5 emissions - ID 11609
  temp <- which(emission_factors$polProcessID == 11609
                & grepl("10930", emission_factors$sourceBinID))
  emission_factors$meanBaseRate[temp] <- emission_factors$meanBaseRate[temp]*mod_PM25_brake
  emission_factors$meanBaseRateCV[temp] <- emission_factors$meanBaseRateCV[temp]*mod_PM25_brake
  emission_factors$meanBaseRateIM[temp] <- emission_factors$meanBaseRateIM[temp]*mod_PM25_brake
  emission_factors$meanBaseRateIMCV[temp] <- emission_factors$meanBaseRateIMCV[temp]*mod_PM25_brake
  # Modify Tire PM2.5 emissions - ID 11710
  temp <- which(emission_factors$polProcessID == 11710
                & grepl("10930", emission_factors$sourceBinID))
  emission_factors$meanBaseRate[temp] <- emission_factors$meanBaseRate[temp]*mod_PM25_tire
  emission_factors$meanBaseRateCV[temp] <- emission_factors$meanBaseRateCV[temp]*mod_PM25_tire
  emission_factors$meanBaseRateIM[temp] <- emission_factors$meanBaseRateIM[temp]*mod_PM25_tire
  emission_factors$meanBaseRateIMCV[temp] <- emission_factors$meanBaseRateIMCV[temp]*mod_PM25_tire
  # Modify Brake PM10 emissions - ID 10609
  temp <- which(pm10emissionratio$polProcessID == 10609
                & pm10emissionratio$fuelTypeID == 9)
  pm10emissionratio$PM10PM25Ratio[temp] <- pm10emissionratio$PM10PM25Ratio[temp]*pm10ratio_brake
  pm10emissionratio$PM10PM25RatioCV[temp] <- pm10emissionratio$PM10PM25RatioCV[temp]*pm10ratio_brake
  # Modify Tire PM10 emissions - ID 10710
  temp <- which(pm10emissionratio$polProcessID == 10710
                & pm10emissionratio$fuelTypeID == 9)
  pm10emissionratio$PM10PM25Ratio[temp] <- pm10emissionratio$PM10PM25Ratio[temp]*pm10ratio_tire
  pm10emissionratio$PM10PM25RatioCV[temp] <- pm10emissionratio$PM10PM25RatioCV[temp]*pm10ratio_tire
  # Export the resulting dataset
  emission_factors$sourceBinID <- as.character(emission_factors$sourceBinID)
  emission_factors$sourceBinID <- paste0(emission_factors$sourceBinID, "000000000")
  write.csv(emission_factors, paste0(getwd(), "/outputs/moves_files/emissionrate.csv"), row.names = FALSE)
  write.csv(pm10emissionratio, paste0(getwd(), "/outputs/moves_files/pm10emissionratio.csv"), row.names = FALSE)
}
