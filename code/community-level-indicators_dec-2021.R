library(dplyr)

get_cases_per_100k_7_days <- function(date_string) {
    date_target <- as.Date(date_string,format='%m/%d/%Y')

    end_gather <- date_target
    start_gather <- date_target-6

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
# case_ind2 <- get_cases_per_100k_7_days('11/18/2021'

setwd("./data")
df_hosp <- read.csv('COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_Facility.csv')
fin <- readRDS('fin.RDS')
# unique(df_hosp$collection_week)

# 2021/11/12 and 2021/11/05





# #HSA 221 contains FIPS 1027 1037 & 1121
# as.numeric(df_hosp[which(df_hosp$fips_code == '1027'),which(colnames(df_hosp) == 'previous_day_admission_adult_covid_confirmed_7_day_sum')]) + as.numeric(df_hosp[which(df_hosp$fips_code == '1027'),which(colnames(df_hosp) == 'previous_day_admission_pediatric_covid_confirmed_7_day_sum')])
# as.numeric(df_hosp[which(df_hosp$fips_code == '1121'),which(colnames(df_hosp) == 'previous_day_admission_adult_covid_confirmed_7_day_sum')]) + as.numeric(df_hosp[which(df_hosp$fips_code == '1121'),which(colnames(df_hosp) == 'previous_day_admission_pediatric_covid_confirmed_7_day_sum')])


# fin$Geo_FIPS[which(!(fin$Geo_FIPS %in% unique(df_hosp$fips_code)))]


#Read in HSAs
HSA_FIPS_file <- read.csv('Health.Service.Areas.csv')

#Assign keys (HSAs) and vals (FIPS Code) NOTE:
indx_HSA_df <- lapply(fin$Geo_FIPS, function(x) HSA_FIPS_file$HSA....NCI.Modified.[which(HSA_FIPS_file$FIPS == x)])

# get county-level population based on 2019 census estimates
deaths_df <- read.csv('time_series_covid19_deaths_US.csv', header = T)
right_pop <- deaths_df[which(deaths_df$FIPS %in% fin$Geo_FIPS),]

#Verify that every HSA is represented in df_hosp
testDF <- data.frame(cbind(fin$Geo_FIPS,indx_HSA_df,pop=right_pop$Population))
#Manually assign
testDF[82,2] <- c(strtoi('820'))
testDF[77,2] <- c(strtoi('818'))
testDF[88,2] <- c(strtoi('817'))
testDF[89,2] <- c(strtoi('817'))
testDF[91,2] <- c(strtoi('818'))
testDF[94,2] <- c(strtoi('817'))
testDF[2411,2] <- c(strtoi('957'))


sums <- testDF %>% 
    group_by(indx_HSA_df) %>%
    summarise(X1=sum(unlist(pop), na.rm=TRUE))

hsa_pop_sum_df <- data.frame(sums)


#Aggregate metrics using HSA as the key
df_hosp <- df_hosp[which(df_hosp$collection_week == '2021/11/05'),]
df_hosp <- df_hosp[-which(!(df_hosp$fips_code %in% unique(fin$Geo_FIPS))),]

HSA_hosp <- lapply(df_hosp$fips_code, function(x) testDF$indx_HSA_df[which(testDF$V1 == x)])
new_df_hosp <- cbind(df_hosp,hsa=unlist(HSA_hosp))

new_df_hosp[,(12:(ncol(new_df_hosp)-1))] <- sapply(new_df_hosp[,(12:(ncol(new_df_hosp)-1))], as.numeric)
new_df_hosp[is.na(new_df_hosp)] <- 0

hosp_sums <- new_df_hosp %>% 
    group_by(hsa) %>%
    summarise(X1=sum(unlist(as.numeric(previous_day_admission_adult_covid_confirmed_7_day_sum))),
    X2=sum(unlist(as.numeric(previous_day_admission_pediatric_covid_confirmed_7_day_sum))),
    X3=sum(unlist(as.numeric(total_adult_patients_hospitalized_confirmed_covid_7_day_avg))),
    X4=sum(unlist(as.numeric(total_pediatric_patients_hospitalized_confirmed_covid_7_day_avg))),
    X5=sum(unlist(as.numeric(inpatient_beds_7_day_avg))))

pop_sum_df <- data.frame(hosp_sums)

remove_na_pop <- hsa_pop_sum_df[which(hsa_pop_sum_df$indx_HSA_df %in% pop_sum_df$hsa),]

agg_df <- cbind(pop_sum_df,pop=remove_na_pop$X1[which(remove_na_pop$indx_HSA_df %in% pop_sum_df$hsa)])

hosp_met <- c()
hosp_met <- lapply(testDF$indx_HSA_df, function(x) c(agg_df[which(agg_df$hsa == x),(2:ncol(agg_df))]))


met_df <- t(as.data.frame(do.call(cbind, hosp_met)))

met_df <- cbind(met_df,hsa=unlist(testDF$indx_HSA_df))
# df_hosp$previous_day_admission_adult_covid_confirmed_7_day_sum
# df_hosp$previous_day_admission_pediatric_covid_confirmed_7_day_sum
# ==================
# #COUNTY_POP

is.numeric0 <- function(x) {
  identical(x, numeric(0))
}
# df_hosp$total_adult_patients_hospitalized_confirmed_covid_7_day_avg
# df_hosp$total_pediatric_patients_hospitalized_confirmed_covid_7_day_avg
# ======================
# df_hosp$inpatient_beds_7_day_avg
tru <- lapply(met_df[,1], function(x) is.numeric0(x))

met_df[which(unlist(tru) == TRUE),(1:4)] <- 0
met_df[which(unlist(tru) == TRUE),(5:6)] <- 1
met_df[which(met_df[,5] == 0),5] <- 1

met1 <- ((as.numeric(met_df[,1])+as.numeric(met_df[,2]))/as.numeric(met_df[,6]))*100000
met2 <- (as.numeric(met_df[,3])+as.numeric(met_df[,4]))/as.numeric(met_df[,5])

hosp_met_final <- cbind(met1,met2)

write.csv(hosp_met_final,'hospital-indicators-week-ending-11-11-21.csv')

#=============================================================

#=====================December 11, 2021=======================

#=============================================================

#Aggregate metrics using HSA as the key
df_hosp <- df_hosp[which(df_hosp$collection_week == '2021/12/03'),]
df_hosp <- df_hosp[-which(!(df_hosp$fips_code %in% unique(fin$Geo_FIPS))),]

HSA_hosp <- lapply(df_hosp$fips_code, function(x) testDF$indx_HSA_df[which(testDF$V1 == x)])
new_df_hosp <- cbind(df_hosp,hsa=unlist(HSA_hosp))

new_df_hosp[,(12:(ncol(new_df_hosp)-1))] <- sapply(new_df_hosp[,(12:(ncol(new_df_hosp)-1))], as.numeric)
new_df_hosp[is.na(new_df_hosp)] <- 0

hosp_sums <- new_df_hosp %>% 
    group_by(hsa) %>%
    summarise(X1=sum(unlist(as.numeric(previous_day_admission_adult_covid_confirmed_7_day_sum))),
    X2=sum(unlist(as.numeric(previous_day_admission_pediatric_covid_confirmed_7_day_sum))),
    X3=sum(unlist(as.numeric(total_adult_patients_hospitalized_confirmed_covid_7_day_avg))),
    X4=sum(unlist(as.numeric(total_pediatric_patients_hospitalized_confirmed_covid_7_day_avg))),
    X5=sum(unlist(as.numeric(inpatient_beds_7_day_avg))))

pop_sum_df <- data.frame(hosp_sums)

remove_na_pop <- hsa_pop_sum_df[which(hsa_pop_sum_df$indx_HSA_df %in% pop_sum_df$hsa),]

agg_df <- cbind(pop_sum_df,pop=remove_na_pop$X1[which(remove_na_pop$indx_HSA_df %in% pop_sum_df$hsa)])

hosp_met <- c()
hosp_met <- lapply(testDF$indx_HSA_df, function(x) c(agg_df[which(agg_df$hsa == x),(2:ncol(agg_df))]))


met_df <- t(as.data.frame(do.call(cbind, hosp_met)))

met_df <- cbind(met_df,hsa=unlist(testDF$indx_HSA_df))
# df_hosp$previous_day_admission_adult_covid_confirmed_7_day_sum
# df_hosp$previous_day_admission_pediatric_covid_confirmed_7_day_sum
# ==================
# #COUNTY_POP

is.numeric0 <- function(x) {
  identical(x, numeric(0))
}
# df_hosp$total_adult_patients_hospitalized_confirmed_covid_7_day_avg
# df_hosp$total_pediatric_patients_hospitalized_confirmed_covid_7_day_avg
# ======================
# df_hosp$inpatient_beds_7_day_avg
tru <- lapply(met_df[,1], function(x) is.numeric0(x))

met_df[which(unlist(tru) == TRUE),(1:4)] <- 0
met_df[which(unlist(tru) == TRUE),(5:6)] <- 1
met_df[which(met_df[,5] == 0),5] <- 1

met1 <- ((as.numeric(met_df[,1])+as.numeric(met_df[,2]))/as.numeric(met_df[,6]))*100000
met2 <- (as.numeric(met_df[,3])+as.numeric(met_df[,4]))/as.numeric(met_df[,5])

case_ind <- get_cases_per_100k_7_days('12/10/2021')
hosp_met_final <- cbind(hosp_ind1=met1,hosp_ind2=met2,data.frame(case_ind))
com_lev_vector <- ifelse(((hosp_met_final$case_ind < 200 & hosp_met_final$hosp_ind1 < 10)|(hosp_met_final$case_ind < 200 & hosp_met_final$hosp_ind2 < 0.1)),
                      "Low",
                      ifelse(((hosp_met_final$case_ind < 200 & hosp_met_final$hosp_ind1 >= 10 & hosp_met_final$hosp_ind1 < 20)|(hosp_met_final$case_ind < 200 & hosp_met_final$hosp_ind2 >= 0.1 & hosp_met_final$hosp_ind2 < 0.15)|(hosp_met_final$case_ind >= 200 & hosp_met_final$hosp_ind1 < 10)|(hosp_met_final$case_ind >= 200 & hosp_met_final$hosp_ind2 < 0.1)),
                      "Medium",
                      "High") )

check_com_lev <- cbind(hosp_met_final,com_lev_vector)
saveRDS(check_com_lev,'cdc_levels_12_11_22.RDS')
# write.csv(hosp_met_final,'hospital-indicators-week-ending-11-11-21.csv')