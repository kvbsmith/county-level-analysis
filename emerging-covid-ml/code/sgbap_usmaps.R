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

  
  cases_for_dates <- remove_extra[,(which(new_dates == start_gather)):which(new_dates == end_gather)]  
  
  new_cases_df <- cases_for_dates[,ncol(cases_for_dates)] - cases_for_dates[,1]
  
  return(new_cases_df * (100000/remove_extra_deaths$Population))
}

case_ind <- get_cases_per_100k_14days(date_string = '11/22/2021', end_string = '03/02/2022')
#===========================================================================================================


#                                    UPPER QUARTILE IN CHANGE FROM 11/22/21 - 03/02/22


#===========================================================================================================


#Read in county-level data that includes FIPS codes and population density for each county
FIPS <- readRDS('fin.RDS')
#Read in model probability prediction for best-performing Omicron model.
setwd('..')
setwd("./sensitivity/april-30q")
pred <- read.csv('final_probabilities_omicron.csv',header = T)[,-1]
pred <- pred[,6]
#Merge FIPS, population density, incidence change, and model predictions
cases <- data.frame(cbind(FIPS[,1:2],case_ind,pred))

#Get the quantile of model predictions
quan <- quantile(cases$pred)
#Create labels for model prediction quartiles
risk_to_label <- function(pred,quan) {
  if (pred >= quan[4]) {
    x = 'Predicted High Risk'
  } else {
    x = 'Predicted Low Risk'
  }
  return(x)
}
pred_labels <- lapply(cases$pred,quan=quan,risk_to_label)
risk = (unlist(pred_labels))
#Merge new labels into existing data frame
cases <- cbind(cases,risk)

#Create labels for incidence change quartiles
quan_case <- quantile(cases$case_ind)
case_to_label <- function(case,quan_case) {
  if (case > quan_case[4]) {
    x = 'High Cases'
  } else {
    x = 'Low Cases'
  }
  return(x)
}
pred_case_labels <- lapply(cases$case_ind,quan_case=quan_case,case_to_label)
pred_case = (unlist(pred_case_labels))
cases <- cbind(cases,pred_case)


#Order data by prediction value
ordered <- cases[order(cases$pred,decreasing = T),]

top100 <- ordered[1:500,]

remainder <- FIPS[!(FIPS$Geo_FIPS %in% top100$Geo_FIPS),which(colnames(FIPS) %in% c('Geo_FIPS'))]
remainder_df <- data.frame(cbind(remainder,rep('Not Among Top 500 Counties',times=length(remainder))))
colnames(remainder_df) <- c('fips','X')
viz_df <- as.data.frame(cbind(top100$Geo_FIPS,top100$pred_case))
colnames(viz_df) <- c('fips','X')
viz_df <- rbind(viz_df,remainder_df)
p1_omi_100 = plot_usmap(regions = 'counties',size=0.15,color='#999999',data = viz_df,values = 'X')+scale_fill_manual(name="",labels = c("Correctly Predicted High Omicron Incidence (n=170)", "Did Not Experience High Omicron Incidence (n=330)",'Not Among Top 500 Counties (n=2640)','Not in Study (n=4)'),values = c('green','red','darkgray'))+
  ggtitle("Top 500 Counties by SFLR Model Prediction",subtitle="")+
  theme(legend.position = "bottom",legend.justification = "center", legend.title = element_text(size = 14),legend.text = element_text(size = 9),plot.title = element_text(hjust = 0.5,size=11),plot.subtitle = element_text(hjust = 0.5,size=14))+guides(fill = guide_legend(nrow = 2, byrow = TRUE))
p1_omi_100

setEPS()
postscript("sflr_omicron_discrimination.eps",width=7,height=6)
p1_omi_100
dev.off()

table(viz_df$X)



#Order data by Omicron cases value
ordered <- cases[order(cases$case_ind,decreasing = T),]
top100 <- ordered[1:500,]

remainder <- FIPS[!(FIPS$Geo_FIPS %in% top100$Geo_FIPS),which(colnames(FIPS) %in% c('Geo_FIPS'))]
remainder_df <- data.frame(cbind(remainder,rep('Not Among Top 500 Counties',times=length(remainder))))
colnames(remainder_df) <- c('fips','X')
viz_df <- as.data.frame(cbind(top100$Geo_FIPS,top100$risk))
colnames(viz_df) <- c('fips','X')
viz_df <- rbind(viz_df,remainder_df)
p1_omi_100 = plot_usmap(regions = 'counties',size=0.15,color='#999999',data = viz_df,values = 'X')+scale_fill_manual(name="",labels = c('Not Among Top 500 Counties (n=2640)',"SFLR Correctly Predicted High Omicron Incidence (n=151)", "SFLR Incorrectly Predicted Low Omicron Incidence (n=349)",'Not in Study (n=4)'),values = c('white','green','darkred'),na.value='purple')+
  ggtitle("Top 500 Counties by Omicron Incidence from 11/22/2021 - 03/02/2022",subtitle="")+
  theme(legend.position = "bottom",legend.justification = "center", legend.title = element_text(size = 14),legend.text = element_text(size = 9),plot.title = element_text(hjust = 0.5,size=11),plot.subtitle = element_text(hjust = 0.5,size=14))+guides(fill = guide_legend(nrow = 2, byrow = TRUE))
p1_omi_100

setEPS()
postscript("sflr_omicron_discrimination_cases_all3140.eps",width=9,height=7)
p1_omi_100
dev.off()

table(viz_df$X)
