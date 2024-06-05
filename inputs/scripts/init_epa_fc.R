#Extract fleet average curb weight data by vehicle siz and type from EPA tables (Appendix I of fuel economy trends)
library(readxl)
#Conversion file
conv <- read.csv("inputs/user/conversion_units.csv",stringsAsFactors = FALSE,check.names = FALSE,row.names = 1)
vh_techno <- as.data.frame(read_excel("inputs/user/model_matching.xlsx", sheet = "vehicle_technology"))
fuel_conv <- read.csv("inputs/data/fuel_conversion.csv",stringsAsFactors = FALSE,check.names = FALSE)

app_i_dt <- read.csv("inputs/data/us_epa_fe_trends_app_i_210525.csv",stringsAsFactors = FALSE,check.names = FALSE,na.strings="-")
#Rename column names
colnames(app_i_dt)[1] <- "Vehicle Type"
colnames(app_i_dt)[colnames(app_i_dt)=="Production (000)"] <- "Production"
app_i_dt$Production <- as.numeric(app_i_dt$Production)*1000
#Delete rows without prod ("-")
app_i_dt <- subset(app_i_dt,!is.na(Production))
#Format and convert adjusted Fuel Economy
colnames(app_i_dt)[colnames(app_i_dt)=="Real-World MPG"] <- "Adj_fc"
app_i_dt$Adj_fc <- as.numeric(app_i_dt$Adj_fc)
#Convert MPG in L of gasoline /100km
app_i_dt$Adj_fc <- 1/app_i_dt$Adj_fc*conv["L","1 gal"]*conv["mile","1 km"]*100

#Match EPA size classes with FLAME classes
veh_size <- data.frame("Category" = sort(unique(app_i_dt$'Vehicle Type')), stringsAsFactors = FALSE)
veh_size[veh_size$Category%in%c("Car","Car SUV","Sedan/Wagon"),"Size"] <- "Car"
veh_size[veh_size$Category%in%c("Pickup","Truck SUV","Minivan/Van"),"Size"] <- "Light truck"
#Match EPA technology with FLAME technology
veh_techno <- data.frame("Epa_techno" = setdiff(unique(app_i_dt$`Engine Package`),NA), stringsAsFactors = FALSE)
veh_techno[veh_techno$Epa_techno=='Diesel','Technology'] <- "ICEV-D"
veh_techno[veh_techno$Epa_techno=='Alternative Fuel','Technology'] <- "AFV"
veh_techno[is.na(veh_techno$Technology),'Technology'] <- "ICEV-G"

year_list <- setdiff(unique(app_i_dt$`Model Year`),NA)
#Output
dt_col <- c("Model_year","Size","Technology","Fuel_type","Value")
dtf_fe <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
for (year in year_list){
  for (size in unique(veh_size$Size)){
    for (techno in unique(veh_techno$Technology)){
      tmp_dt <- subset(app_i_dt,`Model Year`==year & `Vehicle Type`%in%subset(veh_size,Size==size)$Category & `Engine Package`%in%subset(veh_techno,Technology==techno)$Epa_techno)
      if (sum(tmp_dt$Production)>0){
        if (techno!="AFV"){
          fuel_type <- subset(vh_techno,Own==techno)$`Fuel type`
          conv_val <- subset(fuel_conv,Fuel=="Gasoline" & Data=="Conversion factor")$Value/subset(fuel_conv,Fuel==fuel_type & Data=="Conversion factor")$Value
        } else {
          fuel_type <- "Gasoline"
          conv_val <- 1
        }
        fc_val <- sum(tmp_dt$Production*tmp_dt$Adj_fc)/sum(tmp_dt$Production)*conv_val
        dtf_fe[nrow(dtf_fe)+1,] <- c(year,size,techno,fuel_type,fc_val)
      }
    }
  }
}
dtf_fe[,"Source"] <- "epa"
#Convert lbs in kg
dtf_fe[,'Unit'] <- "L/100km"

write.csv(dtf_fe, "inputs/model/epa_fleet_fc_hist.csv", row.names = FALSE)

