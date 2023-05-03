

library(janitor)
#Read in time-series cases and deaths available from: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
df <- read.csv('time_series_covid19_confirmed_US.csv', header = T)
deaths_df <- read.csv('time_series_covid19_deaths_US.csv', header = T)

#Add population as column for cases time series which is reported in deaths time series and not cases time series
#df <- cbind(df[,c(1:11)],deaths_df$Population,df[,c(12:ncol(df))])

#Remove regions without lat/long - also called "Out of [state]"
df <- df[-c(which(df$Lat == 0)),]
deaths_df <- deaths_df[-c(which(deaths_df$Lat == 0)),]

#Change cumulative case counts to cumulative cases per 100k - same for deaths
for (i in 1:nrow(df)) {
  df[i,c(13:ncol(df))] = df[i,13:ncol(df)] * (100000/df[i,12])
  deaths_df[i,c(13:ncol(deaths_df))] = deaths_df[i,13:ncol(deaths_df)] * (100000/deaths_df[i,12])
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
setwd("./sensitvity\\march10")
x_test <- read.csv('x_test_counties.csv')
county_name_list <- rep(0,nrow(x_test))
fips_list <- rep(0,nrow(x_test))
setwd("./sensitivity\\data")
vif_fin <- readRDS('vif_fin.RDS')
for (i in 1:nrow(x_test)){
  fips_list[i] <- vif_fin$Geo_FIPS[as.numeric(x_test$X[i])]
  
}

for (i in 1:nrow(x_test)){
  county_name_list[i] <- reduced_df$X11[which(reduced_df$X5 == fips_list[i])]
  
}

# x_test$X[2] <- vif_fin$Geo_FIPS[which(vif_fin$SE_A00002_002 == x_test[2,2])]
names <- cbind(county_name_list,fips_list,x_test[,-1])
colnames(names)[2] <- 'Geo_FIPS'




setwd("./data")
pred_labels <- read.csv('pred-labels.csv')
pred_labels <- pred_labels[-which(pred_labels$Predictor.Name == 'life_exp_col'),]
fin_no_fips <- fin[,-1]

test_comb <- rbind(c(t(pred_labels$Predictor.Description),1),fin_no_fips[,-c(which(colnames(fin_no_fips) == 'SE_T016_002'),which(colnames(fin_no_fips) == 'passenger_num'))])

first_col <- as.data.frame(colnames(clean_names(test_comb)))
second_col <- as.data.frame(t(test_comb[1,]))
var_names_descriptions <- cbind(first_col,second_col)

vif_var_list <- var_names_descriptions[which(rownames(var_names_descriptions) %in% var_list),]

# read.csv('predictor-set-covid-19.csv')