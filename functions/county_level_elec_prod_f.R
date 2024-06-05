# Calculates the electricity production at the county level based on information from the Energy Information Administration.
# The output table contains the fraction of the total electricity production in the GEA area taking place in each county
county_level_elec_prod_f <- function() {
  GIS_matching_matrix <- read.csv(paste0(getwd(), "/inputs/air_quality/GIS_matching_matrix.csv"))
  power_plants <- get_input_f(input_name = 'EIA_Power_Plants') %>%
    filter(State != "Alaska" & State != "Hawaii" & State != "Puerto Rico") %>%
    select(c("County", "State", "Total_MW"))
  colnames(power_plants)[which(colnames(power_plants) == "County")] <- "CY_NAME"
  colnames(power_plants)[which(colnames(power_plants) == "State")] <- "ST_NAME"
  county_level_elec_prod <- left_join(power_plants, select(GIS_matching_matrix, c("CY_NAME", "ST_NAME", "FIPS", "Cambium.GEA")), by = c("CY_NAME", "ST_NAME")) %>%
    filter(!is.na(FIPS)) %>%
    select(-c("CY_NAME", "ST_NAME"))
  county_level_elec_prod <- left_join(county_level_elec_prod, aggregate(.~ Cambium.GEA, data = select(county_level_elec_prod, -c("FIPS")), FUN = sum), by = "Cambium.GEA")
  county_level_elec_prod$Total_MW.x <- county_level_elec_prod$Total_MW.x/county_level_elec_prod$Total_MW.y
  county_level_elec_prod <- select(county_level_elec_prod, -c("Total_MW.y"))
  colnames(county_level_elec_prod) <- c("Fraction", "FIPS", "GEA_region")
  county_level_elec_prod <- relocate(county_level_elec_prod, "Fraction", .after = "GEA_region")
  write.csv(county_level_elec_prod, paste0(getwd(), "/inputs/air_quality/electricity_production_county.csv"))
}
