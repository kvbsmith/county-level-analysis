setwd("./data")

library(janitor)


fin <- readRDS('fin.RDS')
fin$Geo_FIPS[which(nchar(fin$Geo_FIPS) == 4)] <- paste0(0,fin$Geo_FIPS[which(nchar(fin$Geo_FIPS) == 4)])

region_1 <- c("09","23","25","33","44","50")
region_FIPS <- cbind(region_1,rep(1,length(region_1)))
region_2 <- c("34","36")
region_FIPS <- rbind(region_FIPS,cbind(region_2,rep(2,length(region_2))))
region_3 <- c("10","11","24","42","51","54")
region_FIPS <- rbind(region_FIPS,cbind(region_3,rep(3,length(region_3))))
region_4 <- c("01","12","13","21","28","37",'45','47')
region_FIPS <- rbind(region_FIPS,cbind(region_4,rep(4,length(region_4))))
region_5 <- c("17","18","26","27","39","55")
region_FIPS <- rbind(region_FIPS,cbind(region_5,rep(5,length(region_5))))
region_6 <- c("05","22","35","40","48")
region_FIPS <- rbind(region_FIPS,cbind(region_6,rep(6,length(region_6))))
region_7 <- c("19","20","29","31")
region_FIPS <- rbind(region_FIPS,cbind(region_7,rep(7,length(region_7))))
region_8 <- c("08","30","38","46","49",'56')
region_FIPS <- rbind(region_FIPS,cbind(region_8,rep(8,length(region_8))))
region_9 <- c("04","06","15","32")
region_FIPS <- rbind(region_FIPS,cbind(region_9,rep(9,length(region_9))))
region_10 <- c("02","16","41","53")
region_FIPS <- rbind(region_FIPS,cbind(region_10,rep(10,length(region_10))))

state_codes <- data.frame(lapply(fin[,1], function(x) substr(x,start=1,stop=2)))

hhs <- lapply(state_codes,function(x) region_FIPS[which(region_FIPS[,1] == x[[1]]),2])
com_lev_df <- readRDS('cdc_levels_12_11_22.RDS')
com_lev_ind <- rep(0,nrow(com_lev_df))
com_lev_ind[which(com_lev_df$com_lev_vector == "High")] <- 1
com_lev_ind[which(is.na(com_lev_ind))] <- 0
com_lev_ind <- data.frame(com_lev_ind)

#========================

#              OUTCOME ASSIGNMENT - OMICRON

#========================
#========================

#              Dec 11

#========================
omicron_data <- read.csv('RegionsDashboard.csv')
dec_11_omicron <- omicron_data[which((omicron_data[,1] == "11-Dec-21" & omicron_data[,5] == "B.1.1.529")),]
dec_11_omicron <- dec_11_omicron[-1,]
omicron_county <- lapply(hhs,function(x) dec_11_omicron[which(dec_11_omicron[,4] == x[1]),7]    ) # dec_11_omicron[which(dec_11_omicron[,4] == x[[1]]),7]

data <- lapply(omicron_county,function(x) print(x)[[1]])
data_df <- as.data.frame(data)
omicron_quartile <- ifelse(t(data_df) > quantile(t(data_df),prob=0.75)[[1]],1,0)

analysis_tensor <- readRDS('vif_dec_omi.RDS')
analysis_tensor <- clean_names(analysis_tensor)

setwd("./sensitivity/april-30q")
tensor <- read.csv('analysis-tensor-vif-omicron.csv')
# omicron_tensor <- cbind(tensor,ba5_quartile_11)
new_tensor <- cbind(analysis_tensor,tensor[,((ncol(tensor)-1):ncol(tensor))],omicron_levels=com_lev_ind)
write.csv(new_tensor,'analysis-tensor-vif-omicron-dec.csv')

setwd(".sensitivity/april-30p")
tensor <- read.csv('analysis-tensor-vif-omicron.csv')
# omicron_tensor <- cbind(tensor,ba5_quartile_11)
new_tensor <- cbind(analysis_tensor,tensor[,((ncol(tensor)-1):ncol(tensor))],com_lev_ind)
write.csv(new_tensor,'analysis-tensor-vif-omicron-dec.csv')

setwd(".sensitivity/march10")
tensor <- read.csv('analysis-tensor-vif-omicron.csv')
# omicron_tensor <- cbind(tensor,ba5_quartile_11)
new_tensor <- cbind(analysis_tensor,tensor[,((ncol(tensor)-1):ncol(tensor))],com_lev_ind)
write.csv(new_tensor,'analysis-tensor-vif-omicron-dec.csv')

setwd(".sensitivity/100-fci")
tensor <- read.csv('analysis-tensor-vif-omicron.csv')
# omicron_tensor <- cbind(tensor,omicron_quartile,ba5_quartile_11)[,-1]
new_tensor <- cbind(analysis_tensor,tensor[,((ncol(tensor)-1):ncol(tensor))],com_lev_ind)
write.csv(new_tensor,'analysis-tensor-vif-omicron-dec.csv')

setwd(".sensitivity/50-fci")
tensor <- read.csv('analysis-tensor-vif-omicron.csv')
# omicron_tensor <- cbind(tensor,ba5_quartile_11)
new_tensor <- cbind(analysis_tensor,tensor[,((ncol(tensor)-1):ncol(tensor))],com_lev_ind)
write.csv(new_tensor,'analysis-tensor-vif-omicron-dec.csv')


#========================

#              BA.5 - May 14 '22

#========================
setwd("./data")
variant_data <- read.csv('SARS-CoV-2_Variant_Proportions.csv')
may_14_omicron <- variant_data[which((substr(variant_data[,2],start=1,stop=10) == "05/14/2022" & variant_data[,3] == "BA.5" & substr(variant_data[,ncol(variant_data)],start=1,stop=10) == "06/07/2022")),] #
may_14_omicron <- may_14_omicron[-1,]
may_ba5_county <- lapply(hhs,function(x) may_14_omicron[which(may_14_omicron[,1] == x[1]),4]    )

data <- lapply(may_ba5_county,function(x) print(x))
data_df <- as.data.frame(data)
ba5_quartile <- ifelse(t(data_df) > quantile(t(data_df),prob=0.75)[[1]],1,0)

com_lev <- read.csv('United_States_COVID-19_Community_Levels_by_County.csv')
may_12_levels <- com_lev[which((substr(com_lev[,which(colnames(com_lev) == 'date_updated')],start=1,stop=10) == "2022-05-12")),which(colnames(com_lev) %in% c('county_fips','covid.19_community_level'))]
fin <- readRDS('fin.RDS')
remove_extra <- may_12_levels[-which(!(may_12_levels$county_fips %in% fin$Geo_FIPS)),]

com_lev_ind <- rep(0,nrow(remove_extra))
com_lev_ind[which(remove_extra$covid.19_community_level == "High" | remove_extra$covid.19_community_level == "Medium" )] <- 1
com_lev_ind[which(is.na(com_lev_ind))] <- 0
com_lev_ind <- data.frame(com_lev_ind)



analysis_tensor <- readRDS('vif_may_ba5.RDS')
analysis_tensor <- clean_names(analysis_tensor)
rownames(analysis_tensor) <- 1:3140
analysis_tensor[which(is.na(analysis_tensor[,(ncol(analysis_tensor)-2)])),(ncol(analysis_tensor)-2)] <- 0

setwd("./sensitivity\april-30q")
tensor <- read.csv('analysis-tensor-vif-omicron-dec.csv')
new_tensor <- cbind(analysis_tensor,tensor[,((ncol(tensor)-2):ncol(tensor))],ba5_quartile=as.numeric(ba5_quartile),ba5_levels=as.numeric(com_lev_ind$com_lev_ind))
write.csv(new_tensor,'analysis-tensor-vif-ba5.csv')

setwd("./sensitivity/april-30p")
tensor <- read.csv('analysis-tensor-vif-omicron-dec.csv')
new_tensor <- cbind(analysis_tensor,tensor[,((ncol(tensor)-2):ncol(tensor))],ba5_quartile=as.numeric(ba5_quartile),ba5_levels=as.numeric(com_lev_ind$com_lev_ind))
write.csv(new_tensor,'analysis-tensor-vif-ba5.csv')

setwd("./sensitivity/march10")
tensor <- read.csv('analysis-tensor-vif-omicron-dec.csv')
new_tensor <- cbind(analysis_tensor,tensor[,((ncol(tensor)-2):ncol(tensor))],ba5_quartile=as.numeric(ba5_quartile),ba5_levels=as.numeric(com_lev_ind$com_lev_ind))
write.csv(new_tensor,'analysis-tensor-vif-ba5.csv')

setwd(".+/sensitivity/100-fci")
tensor <- read.csv('analysis-tensor-vif-omicron-dec.csv')
new_tensor <- cbind(analysis_tensor,tensor[,((ncol(tensor)-2):ncol(tensor))],ba5_quartile=as.numeric(ba5_quartile),ba5_levels=as.numeric(com_lev_ind$com_lev_ind))
write.csv(new_tensor,'analysis-tensor-vif-ba5.csv')

setwd("./sensitivity/50-fci")
tensor <- read.csv('analysis-tensor-vif-omicron-dec.csv')
new_tensor <- cbind(analysis_tensor,tensor[,((ncol(tensor)-2):ncol(tensor))],ba5_quartile=as.numeric(ba5_quartile),ba5_levels=as.numeric(com_lev_ind$com_lev_ind))
write.csv(new_tensor,'analysis-tensor-vif-ba5.csv')