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

setwd('"./data')

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

get_cases_per_100k_7_days <- function(date_string) {
    date_target <- as.Date(date_string,format='%m/%d/%Y')

    end_gather <- date_target
    start_gather <- date_target-6

    setwd("C:\\Users\\kvbsmith1\\Documents\\github\\county-level-analysis\\data")
    county_cases <- read.csv('time_series_covid19_confirmed_US.csv')
    deaths_df <- read.csv('time_series_covid19_deaths_US.csv', header = T)
    fin <- readRDS('fin.RDS')

    remove_extra <- county_cases[-which(!(county_cases$FIPS %in% fin$Geo_FIPS)),]
    remove_extra_deaths <- deaths_df[-which(!(deaths_df$FIPS %in% fin$Geo_FIPS)),]

    new_dates <-lapply(colnames(remove_extra),function(x) as.Date(substr(x,start=2,stop=9),format='%m.%d.%y'))

    # > colnames(county_cases)[which(new_dates == start_gather)]             
    # [1] "X6.4.22"
    # > start_gather
    # [1] "2022-06-04"

    cases_for_dates <- remove_extra[,(which(new_dates == start_gather)):which(new_dates == end_gather)]  

    new_cases_df <- cases_for_dates[,ncol(cases_for_dates)] - cases_for_dates[,1]

    return(new_cases_df * (100000/remove_extra_deaths$Population))
}
# case_ind2 <- get_cases_per_100k_7_days('11/18/2021')
# case_ind2[which(case_ind2 < 0)] <- 0
# case_ind1 <- get_cases_per_100k_7_days('11/11/2021')
# case_ind1[which(case_ind1 < 0)] <- 0
# write.csv(cbind(case_ind1,case_ind2),'case-indicators_dec.csv')

case_ind2 <- get_cases_per_100k_7_days('11/18/2021')
case_ind2[which(case_ind2 < 0)] <- 0
case_ind1 <- get_cases_per_100k_7_days('11/11/2021')
case_ind1[which(case_ind1 < 0)] <- 0
write.csv(cbind(case_ind1,case_ind2),'case-indicators_dec.csv')
##Enter date in MM/DD/YYYY format
get_cases_per_100k <- function(date_string) {
    date_target <- as.Date(date_string,format='%m/%d/%Y')

    end_gather <- date_target-14
    start_gather <- date_target-28

    setwd("./data")
    county_cases <- read.csv('time_series_covid19_confirmed_US.csv')
    deaths_df <- read.csv('time_series_covid19_deaths_US.csv', header = T)
    fin <- readRDS('fin.RDS')

    remove_extra <- county_cases[-which(!(county_cases$FIPS %in% fin$Geo_FIPS)),]
    remove_extra_deaths <- deaths_df[-which(!(deaths_df$FIPS %in% fin$Geo_FIPS)),]

    new_dates <-lapply(colnames(remove_extra),function(x) as.Date(substr(x,start=2,stop=9),format='%m.%d.%y'))

    # > colnames(county_cases)[which(new_dates == start_gather)]             
    # [1] "X6.4.22"
    # > start_gather
    # [1] "2022-06-04"

    cases_for_dates <- remove_extra[,(which(new_dates == start_gather)-1):which(new_dates == end_gather)]  

    new_cases_df <- cases_for_dates[,2:ncol(cases_for_dates)] - cases_for_dates[,1:(ncol(cases_for_dates)-1)]

    return(new_cases_df / (100000/remove_extra_deaths$Population))
}

date = '07/02/2022'
cases_returned <- get_cases_per_100k(date)

date_omicron = '12/11/2021'
date_target <- as.Date(date_omicron,format='%m/%d/%Y')
cases_returned_dec_2021 <- get_cases_per_100k(date_omicron)
View(cases_returned_dec_2021)

#=======================================

#==========December 2021 Omicron========

#=======================================

fin_le <- readRDS('fin_no_life_exp.RDS')
case_ind <- read.csv('case-indicators_dec.csv')[,-1]
hosp_ind1 <- read.csv('hospital-indicators-week-ending-11-11-21.csv')[,-1]
hosp_ind2 <- read.csv('hospital-indicators-week-ending-11-18-21.csv')[,-1]
v_large <- cbind(fin_le,case_ind,hosp_ind1,hosp_ind2)
v_large <- v_large[,-1]
View(v_large)

analysis_tensor <- v_large
colnames(analysis_tensor)[which(colnames(analysis_tensor) == '2018 Food Insecurity Rate')] <- 'food_insecurity'
#exclude categorical variable 'SE_T016_002'
analysis_tensor <- analysis_tensor[,-which(colnames(analysis_tensor) == 'SE_T016_002')]



func_vif_df <- get_vif(analysis_tensor)
analysis_tensor <- analysis_tensor[,-which(func_vif_df$V2 == 1)]

return_vif_df <- get_vif_r2(analysis_tensor)
analysis_tensor <- analysis_tensor[,-which(colnames(analysis_tensor) == return_vif_df$V1[which.max(return_vif_df$V2)])]

while (return_vif_df$V2[which.max(return_vif_df$V2)]>2) {
  return_vif_df <- get_vif_r2(analysis_tensor)
  analysis_tensor <- analysis_tensor[,-which(colnames(analysis_tensor) == return_vif_df$V1[which.max(return_vif_df$V2)])]
}

return_vif_df <- get_vif_r2(analysis_tensor)

df <-return_vif_df[order(return_vif_df$V2,decreasing=TRUE),]
head(df)
write.csv(df,'eTable2.csv')

var_list<-paste(return_vif_df$V1)
var_list
saveRDS(analysis_tensor,'vif_dec_omi.RDS')
#=======================================

#============May 2022 BA.5=============

#=======================================
setwd("./data")
fin <- readRDS('fin.RDS')
fin_le <- readRDS('fin_no_life_exp.RDS')
case_ind <- read.csv('case-indicators_dec.csv')[,-1]
hosp_ind1 <- read.csv('hospital-indicators-week-ending-11-11-21.csv')[,-1]
hosp_ind2 <- read.csv('hospital-indicators-week-ending-11-18-21.csv')[,-1]

com_lev <- read.csv('United_States_COVID-19_Community_Levels_by_County.csv')
april_21_ind <- com_lev[which((substr(com_lev[,which(colnames(com_lev) == 'date_updated')],start=1,stop=10) == "2022-04-21")),which(colnames(com_lev) %in% c('county_fips','covid_hospital_admissions_per_100k','covid_inpatient_bed_utilization','covid_cases_per_100k'))]
ind_fips_oredered <- april_21_ind[order(as.numeric(april_21_ind$county_fips)),]
ind_fips_oredered_reduced <- ind_fips_oredered[which(ind_fips_oredered$county_fips %in% fin$Geo_FIPS),]

april_14_ind <- com_lev[which((substr(com_lev[,which(colnames(com_lev) == 'date_updated')],start=1,stop=10) == "2022-04-14")),which(colnames(com_lev) %in% c('county_fips','covid_hospital_admissions_per_100k','covid_inpatient_bed_utilization','covid_cases_per_100k'))]
ind_fips_oredered2 <- april_14_ind[order(as.numeric(april_14_ind$county_fips)),]
ind_fips_oredered_reduced2 <- ind_fips_oredered2[which(ind_fips_oredered2$county_fips %in% fin$Geo_FIPS),]

v_large <- cbind(fin_le,case_ind,hosp_ind1,hosp_ind2,ind_fips_oredered_reduced[,-1],ind_fips_oredered_reduced2[,-1])
v_large <- v_large[,-1]
View(v_large)

analysis_tensor <- v_large
colnames(analysis_tensor)[which(colnames(analysis_tensor) == '2018 Food Insecurity Rate')] <- 'food_insecurity'
#exclude categorical variable 'SE_T016_002'
analysis_tensor <- analysis_tensor[,-which(colnames(analysis_tensor) == 'SE_T016_002')]



func_vif_df <- get_vif(analysis_tensor)
analysis_tensor <- analysis_tensor[,-which(func_vif_df$V2 == 1)]

return_vif_df <- get_vif_r2(analysis_tensor)
analysis_tensor <- analysis_tensor[,-which(colnames(analysis_tensor) == return_vif_df$V1[which.max(return_vif_df$V2)])]

while (return_vif_df$V2[which.max(return_vif_df$V2)]>2) {
  return_vif_df <- get_vif_r2(analysis_tensor)
  analysis_tensor <- analysis_tensor[,-which(colnames(analysis_tensor) == return_vif_df$V1[which.max(return_vif_df$V2)])]
}

return_vif_df <- get_vif_r2(analysis_tensor)
View(return_vif_df)
var_list<-paste(return_vif_df$V1)
var_list

df_vif_new <-return_vif_df[order(return_vif_df$V2,decreasing=TRUE),]
head(df_vif_new)
write.csv(df_vif_new,'eTable_new.csv')


saveRDS(analysis_tensor,'vif_may_ba5.RDS')
