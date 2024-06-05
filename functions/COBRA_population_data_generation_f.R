COBRA_population_data_generation_f <- function(ssp_scen = NA, last_yr = NA) {
  attribute_f("COBRA_population_data_generation_f")
  # Generates population data from the 
  age_max <- 99
  filenames <- list.files(paste0(getwd(), "/inputs/air_quality/SEDAC_georeferenced_county_population_proj_excel files/"), pattern="*.csv", full.names=TRUE)
  for (i in 1:length(filenames)) {
    temp <- read.csv(filenames[i],  fileEncoding="UTF-8-BOM")
    if (i == 1) {
      SEDAC_county_population <- temp
    } else {
      SEDAC_county_population <- rbind(SEDAC_county_population, temp)
    }
  }
  SEDAC_county_population <- SEDAC_county_population %>%
    add_column("FIPS" = NA) %>%
    add_column("SOURCEINDX" = NA)
  SOURCEINDX_to_FIPS <- get_input_f(input_name = "COBRA_SOURCEINDX to FIPS crosswalk")
  for (i in 1:dim(SOURCEINDX_to_FIPS)[1]) {
    temp <- which(SEDAC_county_population$state == SOURCEINDX_to_FIPS$STFIPS[i] & SEDAC_county_population$county == SOURCEINDX_to_FIPS$CNTYFIPS[i])
    SEDAC_county_population$FIPS[temp] <- SOURCEINDX_to_FIPS$FIPS[i]
    SEDAC_county_population$SOURCEINDX[temp] <- SOURCEINDX_to_FIPS$SOURCEINDX[i]
  }
  SEDAC_county_population <- SEDAC_county_population %>%
    select(-c(STATEFP10, COUNTYFP10, COUNTYNS10, GEOID10, NAME10, NAMELSAD10, LSAD10, CLASSFP10, MTFCC10, CSAFP10, CBSAFP10, METDIVFP10, FUNCSTAT10, ALAND10, AWATER10, INTPTLAT10, INTPTLON10, geoid, state, county))
  SEDAC_county_population <- melt(SEDAC_county_population, id = c("SOURCEINDX", "FIPS", "age")) %>%
    add_column("SSP" = NA) %>%
    add_column("Year" = NA) %>%
    add_column("Age" = NA)
  SEDAC_county_population$SSP <- str_sub(SEDAC_county_population$variable, 4, 4)
  SEDAC_county_population$Year <- str_sub(SEDAC_county_population$variable, 5, 8)
  SEDAC_county_population <- SEDAC_county_population %>%
    filter(SSP == ssp_scen) %>%
    filter(Year <= last_yr) %>%
    filter(!is.na(FIPS))
  # Add the missing year using linear interpolation of the population data
  first_year <- min(SEDAC_county_population$Year)
  years <- first_year:last_yr
  years_SEDAC <- as.numeric(unique(SEDAC_county_population$Year))
  for (i in 2:length(years_SEDAC)) {
    temp_start <- SEDAC_county_population[which(SEDAC_county_population$Year == years_SEDAC[i-1]),]
    temp_end <- SEDAC_county_population[which(SEDAC_county_population$Year == years_SEDAC[i]),]
    for (j in (years_SEDAC[i-1]+1):(years_SEDAC[i]-1)) {
      temp <- temp_start
      temp$value <- 0
      temp$Year <- j
      temp$value <- (temp_end$value-temp_start$value)/(years_SEDAC[i]-years_SEDAC[i-1])*j+(temp_start$value-(temp_end$value-temp_start$value)/(years_SEDAC[i]-years_SEDAC[i-1])*years_SEDAC[i-1])
      SEDAC_county_population <- rbind(SEDAC_county_population, temp)
    }
  }
  #Add the missing ages by equally distributing the population from the age bins
  ages_SEDAC <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85)
  years_old <- min(unique(SEDAC_county_population$age)):max(unique(SEDAC_county_population$age))
  for (i in years_old) {
    SEDAC_county_population$Age[which(SEDAC_county_population$age == i)] <- ages_SEDAC[i]
    if (i != max(unique(SEDAC_county_population$age))) {
      bin_width <- ages_SEDAC[i+1]-ages_SEDAC[i]
    } else {
      bin_width <- age_max-ages_SEDAC[i]+1
    }
    SEDAC_county_population$value[which(SEDAC_county_population$age == i)] <- SEDAC_county_population$value[which(SEDAC_county_population$age == i)]/bin_width
    for (j in 1:(bin_width-1)) {
      temp <- SEDAC_county_population[which(SEDAC_county_population$age == i),]
      temp$Age <- ages_SEDAC[i]+j
      if (i == 1 & j == 1) {
        SEDAC_county_population_temp <- temp
      } else {
        SEDAC_county_population_temp <- bind_rows(SEDAC_county_population_temp, temp)
      }
    }
  }
  SEDAC_county_population <- bind_rows(SEDAC_county_population, SEDAC_county_population_temp)
  SEDAC_county_population <- select(SEDAC_county_population, -c(age, variable, SSP))
  population_projections_dataset <- dcast(SEDAC_county_population, SOURCEINDX + FIPS + Year ~ Age, value.var="value")
  ages <- paste0("Age", 0:99)
  population_projections_dataset <- mutate(population_projections_dataset, id = row_number())
  population_projections_dataset <- relocate(population_projections_dataset, Year, .before = SOURCEINDX)
  colname <- c("Year", "DestinationID", "FIPS", ages, "rowid")
  colnames(population_projections_dataset) <- colname
  # Export the datafiles by year
  years <- unique(population_projections_dataset$Year)
  print("The population files for COBRA will be exported")
  for (i in years) {
    filename <- paste0("COBRA_population_data_", i, ".csv")
    data <- population_projections_dataset[which(population_projections_dataset$Year == i),]
    write.csv(data, paste0(getwd(), "/inputs/air_quality/cobra_population/", filename), row.names = FALSE)
  }
}
