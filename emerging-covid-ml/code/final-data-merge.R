library(ROCR)
library(randomForest)
require(caTools)
library(glmnet)
library(tidyverse)
library(caret)
library(geodist)
library(pROC)
library(car)
library(InformationValue)
library(MASS)
library(cvAUC)
library(janitor)


get_vif <- function(analysis_tensor){
  func_vif_df <- as.data.frame(matrix(nrow=ncol(analysis_tensor),ncol = 2))
  func_vif_df$V1 <- colnames(analysis_tensor)
  
  for (i in colnames(analysis_tensor)){
    print(i)
    print(which(func_vif_df$V1 == i))
    
    #create regression model
    form <- as.formula(paste(i,'~.-',i))
    vif_reg <- lm(form,data=analysis_tensor)
    #get vif value and place into df
    func_vif_df$V2[which(func_vif_df$V1 == i)] <- summary(vif_reg)$r.squared
    
  }
  return(func_vif_df)
}

get_vif_r2 <- function(analysis_tensor){
  func_vif_df <- as.data.frame(matrix(nrow=ncol(analysis_tensor),ncol = 2))
  func_vif_df$V1 <- colnames(analysis_tensor)
  
  for (i in colnames(analysis_tensor)){
    print(i)
    print(which(func_vif_df$V1 == i))
    
    #create regression model
    form <- as.formula(paste(i,'~.-',i))
    vif_reg <- lm(form,data=analysis_tensor)
    #get vif value and place into df
    func_vif_df$V2[which(func_vif_df$V1 == i)] <- 1/(1-summary(vif_reg)$r.squared)
    
  }
  return(func_vif_df)
}
setwd('./data')

#Read in time-series cases and deaths available from: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
df <- read.csv('time_series_covid19_confirmed_US.csv', header = T)
deaths_df <- read.csv('time_series_covid19_deaths_US.csv', header = T)
df <- df[-which( !(df$FIPS %in% deaths_df$FIPS)),]
#Add population as column for cases time series which is reported in deaths time series and not cases time series
df <- cbind(df[,c(1:11)],deaths_df$Population,df[,c(12:ncol(df))])

#Remove regions without lat/long - also called "Out of [state]"
df <- df[-c(which(df$Lat == 0)),]
deaths_df <- deaths_df[-c(which(deaths_df$Lat == 0)),]

#Change cumulative case counts to cumulative cases per 100k - same for deaths
for (i in 1:nrow(df)) {
  df[i,c(13:ncol(df))] = df[i,13:ncol(df)] * (100000/df[i,12])
  # deaths_df[i,c(13:ncol(deaths_df))] = deaths_df[i,13:ncol(deaths_df)] * (100000/deaths_df[i,12])
}
#df <- df[which(df$FIPS %in% final_combine_tensor$Geo_FIPS),]

#create list to store first_case_index per county
fci <- rep(0,nrow(df))
fdi <- rep(0,nrow(deaths_df))

#Create new_df to store cases per 100k from 1st reported case onwards per county
new_df = data.frame(matrix(ncol = length(df), nrow = nrow(df)))
new_ddf = data.frame(matrix(ncol = length(deaths_df), nrow = nrow(deaths_df)))

#for each row, select the first non-zero case date and the entries after
for (i in 1:nrow(df)) {
  cases_row = df[i,c(13:ncol(df))] #row contains only cases per 100k data
  first_case_index =  which(cases_row != 0)[1] #index of first reported cases
  fci[i] <- first_case_index #add index to list
  if (is.na(first_case_index) == TRUE) { #if no cases reported, move on to next county
    next
  }
  else {
    #If the first case index occurs less than 100 days from the end of df, replace with data available
    if (first_case_index+99+12 >= (ncol(df))) {
      new_df[i,c(13:((ncol(df)) - first_case_index))] = df[i,c((first_case_index+12):(ncol(df)-1))]
      next
    }
    #Set all data from first case onwards in new_df
    new_df[i,c(13:(ncol(df)))] = df[i,c((first_case_index+12):((ncol(df))))]
  }
}
#Replace first 12 columns
new_df[,c(1:12)] <- df[,c(1:12)]

#Replace NA with 0s
na_indices <- which(is.na(new_df$X13))
for (i in na_indices) {
  new_df[i,c(13:ncol(new_df))] <- rep(0,ncol(new_df)-12)
}


#for each row, select the first non-zero death date and the entries after
for (i in 1:nrow(deaths_df)) {
  cases_row = deaths_df[i,c(13:ncol(deaths_df))] #row contains only cases per 100k data
  first_case_index =  which(cases_row != 0)[1] #index of first reported cases
  fdi[i] <- first_case_index #add index to list
  if (is.na(first_case_index) == TRUE) { #if no cases reported, move on to next county
    next
  }
  else {
    #If the first case index occurs less than 100 days from the end of df, replace with data available
    if (first_case_index+99+12 >= (ncol(deaths_df))) {
      new_ddf[i,c(13:((ncol(deaths_df)) - first_case_index))] = deaths_df[i,c((first_case_index+12):(ncol(deaths_df)-1))]
      next
    }
    #Set all data from first case onwards in new_df
    new_ddf[i,c(13:(ncol(deaths_df)))] = deaths_df[i,c((first_case_index+12):((ncol(deaths_df))))]
  }
}
#Replace first 12 columns
new_ddf[,c(1:12)] <- deaths_df[,c(1:12)]

#Replace NA with 0s
na_indices <- which(is.na(new_ddf$X13))
for (i in na_indices) {
  new_ddf[i,c(13:ncol(new_ddf))] <- rep(0,ncol(new_ddf)-12)
}


#Subtract counties for which we have no economic, census, health-related data
reduced_df <- new_df
reduced_ddf <- new_ddf
#Sort the reduced data frame
s_reduced_df <- reduced_df[order(reduced_df$X5),]
s_reduced_ddf <- reduced_ddf[order(reduced_ddf$X5),]

acs_5 <- read.csv('R12861621_SL050.csv',header=T,na.strings=c(""," ","NA"))[]
colnames(acs_5) <- acs_5[1,]
acs_5 <- acs_5[-1,]
acs_5 <- as.data.frame(sapply( acs_5, as.numeric ))
for(i in 1:ncol(acs_5)){
  acs_5[is.na(acs_5[,i]), i] <- mean(acs_5[,i], na.rm = TRUE)
}
# acs_5 <- na.replace(acs_5, replace = colMeans(acs_5, na.rm = TRUE))

health_var <- read.csv('R12861549_SL050.csv',header=T,na.strings=c(""," ","NA"))[,-c(2:6)]
colnames(health_var) <- health_var[1,]
health_var <- health_var[-1,]
health_var[,which(colnames(health_var) == 'SE_T016_002')] <- ifelse(health_var[,which(colnames(health_var) == 'SE_T016_002')]=="Yes", 1, 2)
health_var <- as.data.frame(sapply( health_var, as.numeric ))
health_var2 <- health_var[,-which(colnames(health_var) =='SE_T016_002')]
for(i in 1:ncol(health_var2)){
  health_var2[is.na(health_var2[,i]), i] <- mean(health_var2[,i], na.rm = TRUE)
}
# new_health_var <- na.replace(health_var[,-which(colnames(health_var)=='SE_T016_002')], m = colMeans(health_var[,-which(colnames(health_var)=='SE_T016_002')], na.rm = TRUE))
health_var <- cbind(health_var2,health_var$SE_T016_002)
colnames(health_var)[which(colnames(health_var)=='health_var$SE_T016_002')] <- 'SE_T016_002'
health_var$SE_T016_002[which(is.na(health_var$SE_T016_002))] <- tail(names(sort(table(health_var$SE_T016_002))), 1)
health_var$SE_T016_002 <- as.factor(health_var$SE_T016_002)
 


first_join <- merge(acs_5,health_var,by.x = "Geo_FIPS",by.y = "Geo_FIPS")

#2019 Temps and Rain



# passengers <- read.csv('passenger-data-airports.csv',header=T)


#Min dist for airport
#Get the latitude and longitude of reduced counties
county_lat_long <- reduced_df[c('X5','X9','X10')]
#Read in airport data .csv
airport <- read.csv('us-airports.csv', header = T)
#Get only airports with scheduled service
service_airports <- airport[which(airport$scheduled_service == 1),]
#get only airports with passenger data
# service_airports <- service_airports[which(service_airports$iata_code %in% passengers$Locid),]
# service_pass_num = rep(0,nrow(service_airports))
# for (i in 1:nrow(service_airports)){
#   service_pass_num[i] <- passengers$CY.19.Enplanements[which(service_airports$iata_code[i] == passengers$Locid)]
# }
# service_airports <- cbind(service_airports,service_pass_num)
#Get latitude and longitude coords for large airports 
airport_lat_long <- service_airports[c('latitude_deg','longitude_deg')]
#Set new colnames for analysis
colnames(airport_lat_long) <-  c('x','y')
colnames(county_lat_long) <- c('FIPS','x','y')
#Get matrix of distances where rows = counties while cols = airports
air_dist_mat <- geodist(county_lat_long[,c(2:3)],airport_lat_long,measure = 'haversine')
#Get minimum distance for each county to large airport
min_dist <- apply(air_dist_mat, 1, FUN=min)
#Get column index of shortest distance from county to airport by county
# col_min_air <- apply(air_dist_mat,1,FUN=which.min)
# passenger_num <- rep(0,length(col_min_air))
# for (i in 1:length(col_min_air)){
#   passenger_num[i] = service_airports$service_pass_num[col_min_air[i]]
# }

min_dist_mat <- as.data.frame(cbind(county_lat_long$FIPS,min_dist)) #,passenger_num

second_join <- merge(first_join,min_dist_mat,by.x = "Geo_FIPS",by.y = "V1")

#Social vulnerability index
svi_tensor <- read.csv('SVI2018_US_COUNTY.csv',header = TRUE,na.strings=c(""," ","NA"))[-1,]
#GET SVI subindices and overall index by finding colnames that begin with RPL. Also, get FIPS
svi_subset_cols <- svi_tensor[,c(which(substr(colnames(svi_tensor),1,3) == 'RPL'),which(colnames(svi_tensor) == 'FIPS'))]

ordered_svi <- svi_subset_cols[order(svi_subset_cols$FIPS,decreasing = FALSE),]
#Remove row 549 which we don't have more recent census data for
#ordered_svi <- ordered_svi[-549,]
#Remove row 1816 which we don't have accurate SVI data for
#census_tensor <- census_tensor[-1816,]
third_join <-  merge(second_join,ordered_svi,by.x = "Geo_FIPS",by.y = "FIPS")

climate_tensor <- read.csv('counties.csv',header = TRUE,na.strings=c(""," ","NA"))
climate_tensor <- climate_tensor[-1,c(1,58:81)]
climate_tensor <- na.replace(climate_tensor, m = colMeans(climate_tensor, na.rm = TRUE))


fourth_join <- merge(third_join,climate_tensor,by.x = "Geo_FIPS",by.y = "FIPS")


#FIPS, PCP, HOSPITAL BEDS
ahrf2 <- readRDS('ahrf.RDS')

new_fips <- c(substr(ahrf2$f00002[1:316],2,5),ahrf2$f00002[317:nrow(ahrf2)])
ahrf2$f00002 <- new_fips

fifth_join <- merge(fourth_join,ahrf2,by.x = "Geo_FIPS",by.y = "f00002")
# #reduce counties for those that we have data on
# ahrf3 <- ahrf2[which(ahrf2$f00002 %in% left_zero_fips),]
# new_fips <- c(substr(ahrf3$f00002[1:316],2,5),ahrf3$f00002[317:nrow(ahrf3)])
# ahrf3$f00002 <- new_fips

# #LIFE EXPECTANCY
# l_e <- read.csv('R12856445_SL140.csv',header=T,na.strings = '#N/A')
# l_e <- na.exclude(l_e)

# fips_col <- unique(l_e$county_fips)
# life_exp_col <- rep(0,length(fips_col))
# life_exp_df <- data.frame(cbind(fips_col,life_exp_col))
# k=1
# for (i in fips_col){
  
#   small <- l_e[which(l_e$county_fips == i),]
#   print(sum(small$weighted_life)/sum(small$SE_A00001_001))
#   life_exp_df$life_exp_col[k] = sum(small$weighted_life)/sum(small$SE_A00001_001)
  
#   k=k+1
# }

# sixth_join <- merge(fifth_join,life_exp_df,by.x = "Geo_FIPS",by.y = "fips_col")


svi_data <- read.csv('SVI2018_US_COUNTY.csv',header=T)
svi_cols <- c('EP_POV','EP_UNEMP','EP_PCI','EP_NOHSDP','EP_AGE65','EP_AGE17','EP_SNGPNT','EP_DISABL','EP_MINRTY','EP_LIMENG','EP_MUNIT','EP_MOBILE','EP_CROWD','EP_NOVEH','EP_GROUPQ')
svi_tensor <- svi_data[,c('FIPS',svi_cols)]
svi_tensor <- svi_tensor[-1,]

seventh_join <- merge(fifth_join,svi_tensor,by.x = "Geo_FIPS",by.y = "FIPS")

s2701_data <- read.csv('ACSST5Y2018.S2701_data_with_overlays_2021-07-03T172209.csv',header=T)
s0801_data <- read.csv('ACSST5Y2018.S0801_data_with_overlays_2021-07-03T180155.csv',header=T)
combine <- merge(s2701_data,s0801_data)


food_insecurity_data <- read.csv('MMG2020_2018Data_ToShare.csv',header=F)
colnames(food_insecurity_data) <- food_insecurity_data[2,]
food_insecurity_data <- food_insecurity_data[-c(1:2),-c(5:ncol(food_insecurity_data))]



further_combine <- merge(combine,food_insecurity_data,by.x = 'NAME',by.y = 'County, State')


icu_data <- read.csv('KHN_ICU_bed_county_analysis_2.csv',header=T)
icu_data <- icu_data[,c('cnty_fips','all_icu')]

big_combine <- merge(further_combine,icu_data,by.x = 'FIPS',by.y = 'cnty_fips')


obesity_data <- read.csv('DiabetesAtlasCountyData.csv',header=T)
obesity_data <- obesity_data[-c(1:6),]
obesity_data <- obesity_data[,c(3:4)]

farm_combine <-  merge(big_combine,obesity_data,by.x = 'FIPS',by.y = 'CountyFIPS')
eighth_join <- merge(seventh_join,big_combine,by.x = "Geo_FIPS",by.y = "FIPS")

final_combine_tensor <- eighth_join[,-which(colnames(eighth_join) %in% c('NAME','GEO_ID','State'))]



fin <- final_combine_tensor
fin <- as.data.frame(sapply( fin, as.numeric ))
fin$SE_T016_002 <- as.factor(fin$SE_T016_002)

colSums(is.na(fin))
lapply(fin,'class')
saveRDS(fin,'fin_no_life_exp.RDS')

# #50-FCI
# trial <- merge(fin,s_reduced_df[,c('X5','X62')],by.x = 'Geo_FIPS',by.y = 'X5')
# contained <- ifelse(trial$X62 > 254,1,-1)
# analysis_tensor <- cbind(trial[,-1],contained)
# analysis_tensor$contained <- as.factor(analysis_tensor$contained)
# analysis_tensor <- analysis_tensor[,-which(colnames(analysis_tensor) == 'X62')]
# setwd('...\\sensitivity\\50-fci')
# write.csv(analysis_tensor,'analysis-tensor-largest.csv')
# 
# #100-FCI
# trial <- merge(fin,s_reduced_df[,c('X5','X112')],by.x = 'Geo_FIPS',by.y = 'X5')
# contained <- ifelse(trial$X112 > 759,1,-1)
# analysis_tensor <- cbind(trial[,-1],contained)
# analysis_tensor$contained <- as.factor(analysis_tensor$contained)
# analysis_tensor <- analysis_tensor[,-which(colnames(analysis_tensor) == 'X112')]
# setwd('...\\sensitivity\\100-fci')
# write.csv(analysis_tensor,'analysis-tensor-largest.csv')
# 
# #march-10
# trial <- merge(fin,df[,c('FIPS','X3.10.2020')],by.x = 'Geo_FIPS',by.y = 'FIPS')
# contained <- ifelse(trial$X3.10.2020 != 0,1,-1)
# analysis_tensor <- cbind(trial[,-1],contained)
# analysis_tensor$contained <- as.factor(analysis_tensor$contained)
# analysis_tensor <- analysis_tensor[,-which(colnames(analysis_tensor) == 'X3.10.2020')]
# setwd('...\\sensitivity\\march10')
# write.csv(analysis_tensor,'analysis-tensor-largest.csv')
# 
# #april-30p
# trial <- merge(fin,df[,c('FIPS','X4.30.2020')],by.x = 'Geo_FIPS',by.y = 'FIPS')
# contained <- ifelse(trial$X4.30.2020 != 0,1,-1)
# analysis_tensor <- cbind(trial[,-1],contained)
# analysis_tensor$contained <- as.factor(analysis_tensor$contained)
# analysis_tensor <- analysis_tensor[,-which(colnames(analysis_tensor) == 'X4.30.2020')]
# setwd('...\\sensitivity\\april-30p')
# write.csv(analysis_tensor,'analysis-tensor-largest.csv')

#april-30q
t <- data.frame(df[,c('FIPS','X4.30.20')])

trial <- merge(x=fin,y=df[,c('FIPS','X4.30.20')],by.x = 'Geo_FIPS',by.y = 'FIPS')
contained <- ifelse(trial$X4.30.20 > 151,1,-1)
analysis_tensor <- cbind(trial[,-1],contained)
analysis_tensor$contained <- as.factor(analysis_tensor$contained)
analysis_tensor <- analysis_tensor[,-which(colnames(analysis_tensor) == 'X4.30.20')]
# setwd('...\\sensitivity\\april-30q')
# write.csv(analysis_tensor,'analysis-tensor-largest.csv')

# analysis_tensor <- v_large
#rename 2018 food insecurity column
colnames(analysis_tensor)[which(colnames(analysis_tensor) == '2018 Food Insecurity Rate')] <- 'food_insecurity'
#exclude categorical variable 'SE_T016_002'
analysis_tensor <- analysis_tensor[,-which(colnames(analysis_tensor) == 'SE_T016_002')]
#exclude response variable 'contained'
analysis_tensor <- analysis_tensor[,-which(colnames(analysis_tensor) == 'contained')]


func_vif_df <- get_vif(analysis_tensor)
analysis_tensor <- analysis_tensor[,-which(func_vif_df$V2 == 1)]

return_vif_df <- get_vif_r2(analysis_tensor)
analysis_tensor <- analysis_tensor[,-which(colnames(analysis_tensor) == return_vif_df$V1[which.max(return_vif_df$V2)])]

while (return_vif_df$V2[which.max(return_vif_df$V2)]>2) {
  return_vif_df <- get_vif_r2(analysis_tensor)
  analysis_tensor <- analysis_tensor[,-which(colnames(analysis_tensor) == return_vif_df$V1[which.max(return_vif_df$V2)])]
}

return_vif_df <- get_vif_r2(analysis_tensor)

var_list<-paste(return_vif_df$V1)
var_list

saveRDS(analysis_tensor,'new_at.RDS')
#================================

#     VIF VARIABLES ONLY

#===============================




# acs_5 <- read.csv('R12861621_SL050.csv',header=T,na.strings=c(""," ","NA"))[]
# colnames(acs_5) <- acs_5[1,]
# acs_5 <- acs_5[-1,]
# acs_5 <- as.data.frame(sapply( acs_5, as.numeric ))
# acs_5 <- na.replace(acs_5, m = colMeans(acs_5, na.rm = TRUE))

# health_var <- read.csv('R12861549_SL050.csv',header=T,na.strings=c(""," ","NA"))[,-c(2:6)]
# colnames(health_var) <- health_var[1,]
# health_var <- health_var[-1,]
# health_var[,which(colnames(health_var) == 'SE_T016_002')] <- ifelse(health_var[,which(colnames(health_var) == 'SE_T016_002')]=="Yes", 1, 2)
# health_var <- as.data.frame(sapply( health_var, as.numeric ))
# new_health_var <- na.replace(health_var[,-which(colnames(health_var)=='SE_T016_002')], m = colMeans(health_var[,-which(colnames(health_var)=='SE_T016_002')], na.rm = TRUE))
# health_var <- cbind(new_health_var,health_var$SE_T016_002)
# colnames(health_var)[which(colnames(health_var)=='health_var$SE_T016_002')] <- 'SE_T016_002'
# health_var$SE_T016_002[which(is.na(health_var$SE_T016_002))] <- tail(names(sort(table(health_var$SE_T016_002))), 1)
# health_var$SE_T016_002 <- as.factor(health_var$SE_T016_002)



# first_join <- merge(acs_5,health_var,by.x = "Geo_FIPS",by.y = "Geo_FIPS")

# #2019 Temps and Rain
# # passengers <- read.csv('passenger-data-airports.csv',header=T)


# #Min dist for airport
# #Get the latitude and longitude of reduced counties
# county_lat_long <- reduced_df[c('X5','X9','X10')]
# #Read in airport data .csv
# airport <- read.csv('us-airports.csv', header = T)
# #Get only airports with scheduled service
# service_airports <- airport[which(airport$scheduled_service == 1),]
# #get only airports with passenger data
# service_airports <- service_airports[which(service_airports$iata_code %in% passengers$Locid),]
# # service_pass_num = rep(0,nrow(service_airports))
# # for (i in 1:nrow(service_airports)){
# #   service_pass_num[i] <- passengers$CY.19.Enplanements[which(service_airports$iata_code[i] == passengers$Locid)]
# # }
# # service_airports <- cbind(service_airports,service_pass_num)
# #Get latitude and longitude coords for large airports 
# airport_lat_long <- service_airports[c('latitude_deg','longitude_deg')]
# #Set new colnames for analysis
# colnames(airport_lat_long) <-  c('x','y')
# colnames(county_lat_long) <- c('FIPS','x','y')
# #Get matrix of distances where rows = counties while cols = airports
# air_dist_mat <- geodist(county_lat_long[,c(2:3)],airport_lat_long,measure = 'haversine')
# #Get minimum distance for each county to large airport
# min_dist <- apply(air_dist_mat, 1, FUN=min)
# #Get column index of shortest distance from county to airport by county
# col_min_air <- apply(air_dist_mat,1,FUN=which.min)
# # passenger_num <- rep(0,length(col_min_air))
# # for (i in 1:length(col_min_air)){
# #   passenger_num[i] = service_airports$service_pass_num[col_min_air[i]]
# # }

# min_dist_mat <- as.data.frame(cbind(county_lat_long$FIPS,min_dist))#,passenger_num

# second_join <- merge(first_join,min_dist_mat,by.x = "Geo_FIPS",by.y = "V1")

# #Social vulnerability index
# svi_tensor <- read.csv('SVI2018_US_COUNTY.csv',header = TRUE,na.strings=c(""," ","NA"))[-1,]
# #GET SVI subindices and overall index by finding colnames that begin with RPL. Also, get FIPS
# svi_subset_cols <- svi_tensor[,c(which(substr(colnames(svi_tensor),1,3) == 'RPL'),which(colnames(svi_tensor) == 'FIPS'))]

# ordered_svi <- svi_subset_cols[order(svi_subset_cols$FIPS,decreasing = FALSE),]
# #Remove row 549 which we don't have more recent census data for
# #ordered_svi <- ordered_svi[-549,]
# #Remove row 1816 which we don't have accurate SVI data for
# #census_tensor <- census_tensor[-1816,]
# third_join <-  merge(second_join,ordered_svi,by.x = "Geo_FIPS",by.y = "FIPS")

# climate_tensor <- read.csv('counties.csv',header = TRUE,na.strings=c(""," ","NA"))
# climate_tensor <- climate_tensor[-1,c(1,58:81)]
# climate_tensor <- na.replace(climate_tensor, m = colMeans(climate_tensor, na.rm = TRUE))


# fourth_join <- merge(third_join,climate_tensor,by.x = "Geo_FIPS",by.y = "FIPS")


# #FIPS, PCP, HOSPITAL BEDS
# ahrf2 <- readRDS('ahrf.RDS')

# new_fips <- c(substr(ahrf2$f00002[1:316],2,5),ahrf2$f00002[317:nrow(ahrf2)])
# ahrf2$f00002 <- new_fips

# fifth_join <- merge(fourth_join,ahrf2,by.x = "Geo_FIPS",by.y = "f00002")

# svi_data <- read.csv('SVI2018_US_COUNTY.csv',header=T)
# svi_cols <- c('EP_POV','EP_UNEMP','EP_PCI','EP_NOHSDP','EP_AGE65','EP_AGE17','EP_SNGPNT','EP_DISABL','EP_MINRTY','EP_LIMENG','EP_MUNIT','EP_MOBILE','EP_CROWD','EP_NOVEH','EP_GROUPQ')
# svi_tensor <- svi_data[,c('FIPS',svi_cols)]
# svi_tensor <- svi_tensor[-1,]

# seventh_join <- merge(fifth_join,svi_tensor,by.x = "Geo_FIPS",by.y = "FIPS")

# s2701_data <- read.csv('ACSST5Y2018.S2701_data_with_overlays_2021-07-03T172209.csv',header=T)
# s0801_data <- read.csv('ACSST5Y2018.S0801_data_with_overlays_2021-07-03T180155.csv',header=T)
# combine <- merge(s2701_data,s0801_data)


# food_insecurity_data <- read.csv('MMG2020_2018Data_ToShare.csv',header=F)
# colnames(food_insecurity_data) <- food_insecurity_data[2,]
# food_insecurity_data <- food_insecurity_data[-c(1:2),-c(5:ncol(food_insecurity_data))]



# further_combine <- merge(combine,food_insecurity_data,by.x = 'NAME',by.y = 'County, State')


# icu_data <- read.csv('KHN_ICU_bed_county_analysis_2.csv',header=T)
# icu_data <- icu_data[,c('cnty_fips','all_icu')]

# big_combine <- merge(further_combine,icu_data,by.x = 'FIPS',by.y = 'cnty_fips')


# # obesity_data <- read.csv('DiabetesAtlasCountyData.csv',header=T)
# # obesity_data <- obesity_data[-c(1:6),]
# # obesity_data <- obesity_data[,c(3:4)]
# # 
# # farm_combine <-  merge(big_combine,obesity_data,by.x = 'FIPS',by.y = 'CountyFIPS')
# eighth_join <- merge(seventh_join,big_combine,by.x = "Geo_FIPS",by.y = "FIPS")

# final_combine_tensor <- eighth_join[,-c(141,142,145)]

final_combine_tensor <- readRDS('new_at.RDS')
fin_le <- readRDS('fin_no_life_exp.RDS')

fin <- final_combine_tensor
fin <- as.data.frame(sapply( fin, as.numeric ))
#fin$SE_T016_002 <- as.factor(fin$SE_T016_002)
# vif_fin <- fin[,which(colnames(fin) %in% var_list)]
vif_fin <- cbind(fin_le$Geo_FIPS,fin)
colnames(vif_fin)[1] <- 'Geo_FIPS'
saveRDS(vif_fin,'vif_fin.RDS')


#50-FCI
trial <- merge(vif_fin,s_reduced_df[,c('X5','X62')],by.x = 'Geo_FIPS',by.y = 'X5')
contained <- ifelse(trial$X62 > 251.75,1,0)
analysis_tensor <- cbind(trial[,-1],contained)
analysis_tensor$contained <- as.factor(analysis_tensor$contained)
analysis_tensor <- analysis_tensor[,-which(colnames(analysis_tensor) == 'X62')]
analysis_tensor <- clean_names(analysis_tensor)
setwd('./sensitivity\\50-fci')
write.csv(analysis_tensor,'analysis-tensor-vif.csv')

#100-FCI
trial <- merge(vif_fin,s_reduced_df[,c('X5','X112')],by.x = 'Geo_FIPS',by.y = 'X5')
contained <- ifelse(trial$X112 > 751.5,1,0)
analysis_tensor <- cbind(trial[,-1],contained)
analysis_tensor$contained <- as.factor(analysis_tensor$contained)
analysis_tensor <- analysis_tensor[,-which(colnames(analysis_tensor) == 'X112')]
analysis_tensor <- clean_names(analysis_tensor)
setwd('...\\sensitivity\\100-fci')
write.csv(analysis_tensor,'analysis-tensor-vif.csv')

#march-10
trial <- merge(vif_fin,df[,c('FIPS','X3.10.2020')],by.x = 'Geo_FIPS',by.y = 'FIPS')
contained <- ifelse(trial$X3.10.2020 != 0,1,0)
analysis_tensor <- cbind(trial[,-1],contained)
analysis_tensor$contained <- as.factor(analysis_tensor$contained)
analysis_tensor <- analysis_tensor[,-which(colnames(analysis_tensor) == 'X3.10.2020')]
analysis_tensor <- clean_names(analysis_tensor)
setwd('...\\sensitivity\\march10')
write.csv(analysis_tensor,'analysis-tensor-vif.csv')

#april-30p
trial <- merge(vif_fin,df[,c('FIPS','X4.30.2020')],by.x = 'Geo_FIPS',by.y = 'FIPS')
contained <- ifelse(trial$X4.30.2020 != 0,1,0)
analysis_tensor <- cbind(trial[,-1],contained)
analysis_tensor$contained <- as.factor(analysis_tensor$contained)
analysis_tensor <- analysis_tensor[,-which(colnames(analysis_tensor) == 'X4.30.2020')]
analysis_tensor <- clean_names(analysis_tensor)
setwd('...\\sensitivity\\april-30p')
write.csv(analysis_tensor,'analysis-tensor-vif.csv')

#april-30q
trial <- merge(vif_fin,df[,c('FIPS','X4.30.2020')],by.x = 'Geo_FIPS',by.y = 'FIPS')
contained <- ifelse(trial$X4.30.2020 > 147.08,1,0)
analysis_tensor <- cbind(trial[,-1],contained)
analysis_tensor$contained <- as.factor(analysis_tensor$contained)
analysis_tensor <- analysis_tensor[,-which(colnames(analysis_tensor) == 'X4.30.2020')]
analysis_tensor <- clean_names(analysis_tensor)
setwd('...\\sensitivity\\april-30q')
write.csv(analysis_tensor,'analysis-tensor-vif.csv')
