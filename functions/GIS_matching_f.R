# This function converts different geographical entities used in the model into other ones (e.g., from counties to FIPS)
# Input dataset contains a state dataset and a counties dataset / matches states names, counties names, 
GIS_matching_f <- function() {
  # Informations on geographical subdivisions
  GIS_matching_matrix <- get_input_f(input_name = 'COBRA_SOURCEINDX to FIPS crosswalk')
  colnames(GIS_matching_matrix) <- c("COBRA_SOURCEINDX", "FIPS", "ST_FIPS", "CY_FIPS", "ST_NAME", "CY_NAME")
  GIS_matching_matrix <- GIS_matching_matrix %>%
    add_column("5_digit_FIPS" = NA) %>%
    add_column("2_digit_ST_FIPS" = NA) %>%
    add_column("3_digit_CY_FIPS" = NA) %>%
    add_column("States_Abbreviation" = NA)
  GIS_matching_matrix$`2_digit_ST_FIPS`[which(GIS_matching_matrix$ST_FIPS<10)] <- paste0("0", GIS_matching_matrix$ST_FIPS[which(GIS_matching_matrix$ST_FIPS<10)])
  GIS_matching_matrix$`2_digit_ST_FIPS`[which(GIS_matching_matrix$ST_FIPS>=10)] <- GIS_matching_matrix$ST_FIPS[which(GIS_matching_matrix$ST_FIPS>=10)]  
  GIS_matching_matrix$`3_digit_CY_FIPS`[which(GIS_matching_matrix$CY_FIPS<10)] <- paste0("00", GIS_matching_matrix$CY_FIPS[which(GIS_matching_matrix$CY_FIPS<10)])
  GIS_matching_matrix$`3_digit_CY_FIPS`[which(GIS_matching_matrix$CY_FIPS<100 & GIS_matching_matrix$CY_FIPS>=10)] <- paste0("0", GIS_matching_matrix$CY_FIPS[which(GIS_matching_matrix$CY_FIPS<100 & GIS_matching_matrix$CY_FIPS>=10)])
  GIS_matching_matrix$`3_digit_CY_FIPS`[which(GIS_matching_matrix$CY_FIPS>=100)] <- GIS_matching_matrix$CY_FIPS[which(GIS_matching_matrix$CY_FIPS>=100)]
  GIS_matching_matrix$`5_digit_FIPS` <- paste0(GIS_matching_matrix$`2_digit_ST_FIPS`, GIS_matching_matrix$`3_digit_CY_FIPS`)
  CAMBIUM_FIPS_to_GEA <- get_input_f(input_name = 'CAMBIUM_FIPS_to_GEA')
  for (i in unique(GIS_matching_matrix$ST_NAME)) {
    GIS_matching_matrix$States_Abbreviation[which(GIS_matching_matrix$ST_NAME == i)] <- unique(CAMBIUM_FIPS_to_GEA$`State Abbr`[which(CAMBIUM_FIPS_to_GEA$State ==i)])
  }
  # Add information related to CAMBIUM: ReEDS Balancing Authorities and CAMBIUM Generation and Emission Assessment regions
  GIS_matching_matrix <- GIS_matching_matrix %>%
    add_column("ReEDS_BA" = NA) %>%
    add_column("CAMBIUM_GEA" = NA)
  for (i in 1:dim(GIS_matching_matrix)[1]) {
    temp <- which(CAMBIUM_FIPS_to_GEA$`State FIPS` == GIS_matching_matrix$ST_FIPS[i] & CAMBIUM_FIPS_to_GEA$`County FIPS` == GIS_matching_matrix$CY_FIPS[i])
    if (length(temp) == 0) {
      temp <- which(CAMBIUM_FIPS_to_GEA$`State FIPS` == GIS_matching_matrix$ST_FIPS[i] & CAMBIUM_FIPS_to_GEA$`County FIPS` == GIS_matching_matrix$CY_FIPS[i-1])
    }
    GIS_matching_matrix$ReEDS_BA[i] <- CAMBIUM_FIPS_to_GEA$`ReEDS BA`[temp]
    GIS_matching_matrix$CAMBIUM_GEA[i] <- CAMBIUM_FIPS_to_GEA$`Cambium GEA`[temp]
  }
  # Add information related to eGRID: eGRID subregions and NERC
  eGRID_dataset <- get_input_f(input_name = 'eGRID_Regions_Correspondance')
  GIS_matching_matrix <- GIS_matching_matrix %>%
    add_column("eGRID_subregion" = NA) %>%
    add_column("NERC_Acronym" = NA) %>%
    add_column("BA_Code" = NA) %>%
    add_column("BA_Acronym" = NA)
  ZIP_code_subregions <- read.csv(paste0(getwd(), "/inputs/air_quality/power_profiler_zipcode_tool.csv"))
  ZIP_code_county <- read.csv(paste0(getwd(), "/inputs/air_quality/ZIP-COUNTY-FIPS_2017-06.csv"))
  ZIP_code_county <- ZIP_code_county %>%
    add_column("eGRID_region" = NA)
  for (i in 1:dim(ZIP_code_county)[1]) {
    temp <- which(ZIP_code_subregions$zip == ZIP_code_county$ZIP[i])
    if (length(temp) != 0) {
      ZIP_code_county$eGRID_region[i] <- ZIP_code_subregions$Subregion[temp]
    }
  }
  ZIP_code_county$STCOUNTYFP[which(ZIP_code_county$STCOUNTYFP < 10000)] <- paste0("0", ZIP_code_county$STCOUNTYFP[which(ZIP_code_county$STCOUNTYFP < 10000)])
  for (i in 1:dim(GIS_matching_matrix)[1]) {
    temp1 <- which(ZIP_code_county$STCOUNTYFP == GIS_matching_matrix$`5_digit_FIPS`[i])
    if (length(temp1) == 0) {
      temp1 <- which(ZIP_code_county$STCOUNTYFP == GIS_matching_matrix$`5_digit_FIPS`[i-1])
    }
    GIS_matching_matrix$eGRID_subregion[i] <- unique(ZIP_code_county$eGRID_region[temp1])[1]
    temp2 <- which(eGRID_dataset$STATE_FIPS == GIS_matching_matrix$ST_FIPS[i] & eGRID_dataset$COUNTY_FIPS == GIS_matching_matrix$CY_FIPS[i])
    if (length(temp2) == 0) {
      j <- 1
      while(length(temp2) == 0) {
        temp2 <- which(eGRID_dataset$STATE_FIPS == GIS_matching_matrix$ST_FIPS[i] & eGRID_dataset$COUNTY_FIPS == GIS_matching_matrix$CY_FIPS[i-j])
        j <- j+1
      }
    }
    GIS_matching_matrix$NERC_Acronym[i] <- unique(eGRID_dataset$NERC_Acronym[temp2])[1]
    GIS_matching_matrix$BA_Code[i] <- unique(eGRID_dataset$BA_Code[temp2])[1]
    GIS_matching_matrix$BA_Acronym[i] <- unique(eGRID_dataset$BA_Name[temp2])[1]
  }
  # Adding the AVERT regions
  AVERT_regions <- read.csv(paste0(getwd(), "/inputs/air_quality/AVERT_regions_county.csv"))
  AVERT_regions$State.and.County.FIPS.Code[which(AVERT_regions$State.and.County.FIPS.Code < 10000)] <- paste0("0", AVERT_regions$State.and.County.FIPS.Code[which(AVERT_regions$State.and.County.FIPS.Code < 10000)])
  GIS_matching_matrix <- add_column(GIS_matching_matrix, "AVERT_Region" = NA)
  for (i in 1:dim(GIS_matching_matrix)[1]) {
    temp <- which(AVERT_regions$State.and.County.FIPS.Code == GIS_matching_matrix$`5_digit_FIPS`[i])
    if (length(temp) == 0) {
      temp <- which(AVERT_regions$State.and.County.FIPS.Code == GIS_matching_matrix$`5_digit_FIPS`[i-1])
    }
    GIS_matching_matrix$AVERT_Region[i] <- AVERT_regions$AVERT.Region[temp]
  }
  # Adding PADD districts for fuel production and distribution
  GIS_matching_matrix <- add_column(GIS_matching_matrix, "PADD" = NA)
  PAD_districts <- read.csv(paste0(getwd(), "/inputs/air_quality/PADD_districts.csv")) 
  for (i in 1:dim(GIS_matching_matrix)[1]) {
    GIS_matching_matrix$PADD[i] <- PAD_districts$PADD[which(PAD_districts$stateID == GIS_matching_matrix$ST_FIPS[i])]
  }
  write.csv(GIS_matching_matrix, paste0(getwd(), "/inputs/air_quality/GIS_matching_matrix.csv"))
  #return(list(GIS_matching_matrix = GIS_matching_matrix))
}
