#Extract fleet average curb weight data by vehicle siz and type from EPA tables (Appendix I of fuel economy trends)
library(readxl)
#Conversion file
conv <- read.csv("inputs/user/conversion_units.csv",stringsAsFactors = FALSE,check.names = FALSE,row.names = 1)

app_i_dt <- read.csv("inputs/data/us_epa_fe_trends_app_i_210525.csv",stringsAsFactors = FALSE,check.names = FALSE,na.strings="-")
#Rename column names
colnames(app_i_dt)[1] <- "Vehicle Type"
colnames(app_i_dt)[colnames(app_i_dt)=="Production (000)"] <- "Production"
app_i_dt$Production <- as.numeric(app_i_dt$Production)*1000
#Delete rows without prod ("-")
app_i_dt <- subset(app_i_dt,!is.na(Production))
colnames(app_i_dt)[colnames(app_i_dt)=="Weight (lbs)"] <- "Weight"
app_i_dt$`Weight`[is.na(app_i_dt$`Weight`)] <- 0
app_i_dt$Weight <- as.numeric(app_i_dt$Weight)

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
dt_col <- c("Model_year","Size","Technology","Value")
dtf_wgt <- setNames(data.frame(matrix(0,ncol = length(dt_col), nrow = 0),stringsAsFactors = FALSE,check.names = FALSE),dt_col)
for (year in year_list){
  tmp_dt <- subset(app_i_dt,`Model Year`==year)
  wgt_val <- sum(tmp_dt$Production*tmp_dt$Weight)/sum(tmp_dt$Production)
  dtf_wgt[nrow(dtf_wgt)+1,] <- c(year,"All","All",wgt_val)
  for (size in unique(veh_size$Size)){
    tmp_dt <- subset(app_i_dt,`Model Year`==year & `Vehicle Type`%in%subset(veh_size,Size==size)$Category)
    if (sum(tmp_dt$Production)>0){
      wgt_val <- sum(tmp_dt$Production*tmp_dt$Weight)/sum(tmp_dt$Production)
      dtf_wgt[nrow(dtf_wgt)+1,] <- c(year,size,"All",wgt_val)
      for (techno in unique(veh_techno$Technology)){
        tmp_dt <- subset(app_i_dt,`Model Year`==year & `Vehicle Type`%in%subset(veh_size,Size==size)$Category & `Engine Package`%in%subset(veh_techno,Technology==techno)$Epa_techno)
        if (sum(tmp_dt$Production)>0){
          wgt_val <- sum(tmp_dt$Production*tmp_dt$Weight)/sum(tmp_dt$Production)
          dtf_wgt[nrow(dtf_wgt)+1,] <- c(year,size,techno,wgt_val)
        }
      }
    }
  }
}
dtf_wgt[,"Source"] <- "epa"
#Convert lbs in kg
dtf_wgt$Value <- as.numeric(dtf_wgt$Value)*conv["kg","1 lb"]
dtf_wgt[,'Unit'] <- "kg"

write.csv(dtf_wgt, "inputs/model/epa_fleet_wgt_hist.csv", row.names = FALSE)

