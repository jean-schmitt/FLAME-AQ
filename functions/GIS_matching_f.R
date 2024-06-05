# This function converts different geographical entities used in the model into other ones (e.g., from counties to FIPS)
# Input dataset contains a state dataset and a counties dataset / matches states names, counties names, 
GIS_matching_f <- function() {
  # Informations on geographical subdivisions
  GIS_matching_matrix <- get_input_f(input_name = 'COBRA_SOURCEINDX to FIPS crosswalk')
  colnames(GIS_matching_matrix) <- c("COBRA_SOURCEINDX", "FIPS", "ST_FIPS", "CY_FIPS", "ST_NAME", "CY_NAME")
  GIS_matching_matrix <- GIS_matching_matrix %>%
    add_column("5_digit_FIPS" = NA) %>%
    add_column("2_digit_ST_FIPS" = NA) %>%
    add_column("3_digit_CY_FIPS" = NA)
  GIS_matching_matrix$`2_digit_ST_FIPS`[which(GIS_matching_matrix$ST_FIPS<10)] <- paste0("0", GIS_matching_matrix$ST_FIPS[which(GIS_matching_matrix$ST_FIPS<10)])
  GIS_matching_matrix$`2_digit_ST_FIPS`[which(GIS_matching_matrix$ST_FIPS>=10)] <- GIS_matching_matrix$ST_FIPS[which(GIS_matching_matrix$ST_FIPS>=10)]  
  GIS_matching_matrix$`3_digit_CY_FIPS`[which(GIS_matching_matrix$CY_FIPS<10)] <- paste0("00", GIS_matching_matrix$CY_FIPS[which(GIS_matching_matrix$CY_FIPS<10)])
  GIS_matching_matrix$`3_digit_CY_FIPS`[which(GIS_matching_matrix$CY_FIPS<100 & GIS_matching_matrix$CY_FIPS>=10)] <- paste0("0", GIS_matching_matrix$CY_FIPS[which(GIS_matching_matrix$CY_FIPS<100 & GIS_matching_matrix$CY_FIPS>=10)])
  GIS_matching_matrix$`3_digit_CY_FIPS`[which(GIS_matching_matrix$CY_FIPS>=100)] <- GIS_matching_matrix$CY_FIPS[which(GIS_matching_matrix$CY_FIPS>=100)]
  GIS_matching_matrix$`5_digit_FIPS` <- paste0(GIS_matching_matrix$`2_digit_ST_FIPS`, GIS_matching_matrix$`3_digit_CY_FIPS`)
  # Add information related to CAMBIUM: ReEDS Balancing Authorities and CAMBIUM Generation and Emission Assessment regions
   CAMBIUM_FIPS_to_GEA <- select(get_input_f(input_name = 'CAMBIUM_FIPS_to_GEA'), -c("County", "State"))
  colnames(CAMBIUM_FIPS_to_GEA)[1:2] <- c("ST_FIPS", "CY_FIPS")
  GIS_matching_matrix <- left_join(GIS_matching_matrix, CAMBIUM_FIPS_to_GEA, by = c("ST_FIPS", "CY_FIPS"))
  #GIS_matching_matrix <- GIS_matching_matrix[!is.na(GIS_matching_matrix$`Cambium GEA`),]
  ## Correction for county FIPS 46102, also identified by FIPS 46113
  GIS_matching_matrix$`State Abbr`[which(GIS_matching_matrix$FIPS == 46102)] <- CAMBIUM_FIPS_to_GEA$`State Abbr`[which(CAMBIUM_FIPS_to_GEA$ST_FIPS == 46 & CAMBIUM_FIPS_to_GEA$CY_FIPS == 113)]
  GIS_matching_matrix$`ReEDS BA`[which(GIS_matching_matrix$FIPS == 46102)] <- CAMBIUM_FIPS_to_GEA$`ReEDS BA`[which(CAMBIUM_FIPS_to_GEA$ST_FIPS == 46 & CAMBIUM_FIPS_to_GEA$CY_FIPS == 113)]
  GIS_matching_matrix$`Cambium GEA`[which(GIS_matching_matrix$FIPS == 46102)] <- CAMBIUM_FIPS_to_GEA$`Cambium GEA`[which(CAMBIUM_FIPS_to_GEA$ST_FIPS == 46 & CAMBIUM_FIPS_to_GEA$CY_FIPS == 113)]
  # Add information related to eGRID: eGRID subregions and NERC
  eGRID_dataset <- get_input_f(input_name = 'eGRID_Regions_Correspondance')
  colnames(eGRID_dataset)[5:6] <- c("ST_FIPS", "CY_FIPS")
  eGRID_dataset <- left_join(eGRID_dataset, select(GIS_matching_matrix, c("FIPS", "ST_FIPS", "CY_FIPS")), by = c("ST_FIPS", "CY_FIPS")) %>%
    group_by_all() %>%
    count %>%
    group_by(FIPS) %>%
    slice(c(which.max(n))) %>%
    select(-c("n"))
  GIS_matching_matrix <- left_join(GIS_matching_matrix, eGRID_dataset, by = c("FIPS", "ST_FIPS", "CY_FIPS"))
  for (i in which(is.na(GIS_matching_matrix$eGRID_subregion))) {
    GIS_matching_matrix$eGRID_subregion[i] <- eGRID_dataset$eGRID_subregion[Closest(eGRID_dataset$FIPS, GIS_matching_matrix$FIPS[i], which = TRUE, na.rm = TRUE)[1]]
    GIS_matching_matrix$NERC_Acronym [i] <- eGRID_dataset$NERC_Acronym[Closest(eGRID_dataset$FIPS, GIS_matching_matrix$FIPS[i], which = TRUE, na.rm = TRUE)[1]]
    GIS_matching_matrix$BA_Code[i] <- eGRID_dataset$BA_Code[Closest(eGRID_dataset$FIPS, GIS_matching_matrix$FIPS[i], which = TRUE, na.rm = TRUE)[1]]
    GIS_matching_matrix$BA_Name[i] <- eGRID_dataset$BA_Name[Closest(eGRID_dataset$FIPS, GIS_matching_matrix$FIPS[i], which = TRUE, na.rm = TRUE)[1]]
  }
  #ZIP_code_subregions <- read.csv(paste0(getwd(), "/inputs/air_quality/power_profiler_zipcode_tool.csv"))
  #colnames(ZIP_code_subregions)[1] <- "ZIP"
  #ZIP_code_county <- read.csv(paste0(getwd(), "/inputs/air_quality/ZIP-COUNTY-FIPS_2017-06.csv"))
  #ZIP_code_county <- left_join(ZIP_code_county, ZIP_code_subregions, by = "ZIP") %>%
  #  select(-c("ZIP")) %>%
  #  group_by_all() %>%
  #  count%>%
  #  group_by(STCOUNTYFP) %>%
  #  slice(c(which.max(n)))
  #colnames(ZIP_code_county)[3] <- "FIPS"
  #GIS_matching_matrix <- left_join(GIS_matching_matrix, select(ZIP_code_county, c("Subregion", "FIPS", "STATE")), by = "FIPS")
  # Adding the AVERT regions
  AVERT_regions <- read.csv(paste0(getwd(), "/inputs/air_quality/AVERT_regions_county.csv"))
  colnames(AVERT_regions) <- c("FIPS", "AVERT_region")
  GIS_matching_matrix <- left_join(GIS_matching_matrix, AVERT_regions, by = "FIPS")
  # Adding PADD districts for fuel production and distribution
  PAD_districts <- read.csv(paste0(getwd(), "/inputs/air_quality/PADD_districts.csv")) %>%
    select(c("stateID", "PADD"))
  colnames(PAD_districts) <- c("ST_FIPS", "PADD")
  GIS_matching_matrix <- left_join(GIS_matching_matrix, PAD_districts, by = "ST_FIPS")
  # Add the groups for fleet allocations
  urban_rural_counties <- get_input_f(input_name = 'urban_rural_counties') %>%
    select(-c("stateID"))
  colnames(urban_rural_counties) <- c("FIPS", "Urban")
  zev_states <- get_input_f(input_name = 'ZEV_states')
  colnames(zev_states)[1] <- c("ST_NAME")
  GIS_matching_matrix <- left_join(GIS_matching_matrix, urban_rural_counties, by = "FIPS") %>%
    add_column("ZEV" = 0)
  GIS_matching_matrix$ZEV[which(GIS_matching_matrix$ST_FIPS%in%zev_states$ID)] <- 1
  write.csv(GIS_matching_matrix, paste0(getwd(), "/inputs/air_quality/GIS_matching_matrix.csv"))
  #return(list(GIS_matching_matrix = GIS_matching_matrix))
}
