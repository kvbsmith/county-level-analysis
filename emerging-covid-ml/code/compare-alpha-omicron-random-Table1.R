library(ggplot2)
library(usmap)
library(gridExtra)
library(grid)
library(RGraphics)
library(dplyr)
library(tableone)

get_cases_per_100k_14days <- function(date_string,end_string) {
  date_target <- as.Date(date_string,format='%m/%d/%Y')
  end_target <- as.Date(end_string,format='%m/%d/%Y')
  start_gather <- date_target
  end_gather <- end_target
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
  
  cases_for_dates <- remove_extra[,(which(new_dates == start_gather)):which(new_dates == end_gather)]  
  
  new_cases_df <- cases_for_dates[,ncol(cases_for_dates)] - cases_for_dates[,1]
  
  return(new_cases_df * (100000/remove_extra_deaths$Population))
}

#===========================================================================================================


#                                    UPPER QUARTILE IN CHANGE FROM 11/22/21 - 03/02/22


#===========================================================================================================

#Read in county-level data that includes FIPS codes and population density for each county
FIPS <- readRDS('fin.RDS')
at_RDS <- readRDS('vif_dec_omi.RDS')
df <- readRDS('cases_per_100k.RDS')
vif_fin <- readRDS('vif_fin.RDS')


contained <- ifelse(df$X4.30.20 > quantile(df$X4.30.20)[4][1],1,0)

setwd("./data")
pred_labels <- read.csv('predictor-labels.csv')
colnames(at_RDS)<-pred_labels$Predictor.Description[which(pred_labels$Predictor.Name %in% colnames(at_RDS))]
FIPS_minus1 <- FIPS[,-1]
colnames(FIPS_minus1)<-pred_labels$Predictor.Description[which(pred_labels$Predictor.Name %in% colnames(FIPS_minus1))]
FIPS <- cbind(Geo_FIPS=FIPS$Geo_FIPS,FIPS_minus1)

at_fin <- cbind(Geo_FIPS=FIPS$Geo_FIPS,at_RDS)

case_ind <- get_cases_per_100k_14days(date_string = '11/22/2021', end_string = '03/02/2022')
final_omi <- ifelse(case_ind > quantile(case_ind)[4],1,0)

setwd('..')
setwd("./sensitivity/april-30q")


dec_omi <- read.csv('analysis-tensor-vif.csv')[,-1]
outcomes <- dec_omi
alpha_counties <- outcomes[which(outcomes$contained == 1),1:44]
omi_counties <- outcomes[which(outcomes$final_omi == 1),1:44]
set.seed(101)
rand_785 = outcomes[sample(nrow(outcomes), 785), ]


table1 <- data.frame(rbind(alpha_counties,omi_counties))
table1_df <- cbind(table1,label=c(rep('Alpha: Upper Quartile',times=nrow(alpha_counties)),rep('Omicron: Upper Quartile',times=nrow(omi_counties))))

tablef <- data.frame(rbind(alpha_counties,omi_counties,rand_785))
tablef_df <- cbind(tablef,label=c(rep('Alpha: Upper Quartile',times=nrow(alpha_counties)),rep('Omicron: Upper Quartile',times=nrow(omi_counties)),rep('Random 785 Counties',times=nrow(rand_785))))


colnames(table1_df)[1:41] <- colnames(FIPS[2:42])
tableOne <- CreateTableOne(strata='label',data=table1_df)
print(tableOne,quote = TRUE)

colnames(tablef_df)[1:41] <- colnames(FIPS[2:42])
tableOne <- CreateTableOne(strata='label',data=tablef_df)
print(tableOne,quote = TRUE)




