setwd('.../data')
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

omicron_data <- read.csv('RegionsDashboard.csv')
dec_11_omicron <- omicron_data[which((omicron_data[,1] == "11-Dec-21" & omicron_data[,5] == "B.1.1.529")),]
dec_11_omicron <- dec_11_omicron[-1,]
omicron_county <- lapply(hhs,function(x) dec_11_omicron[which(dec_11_omicron[,4] == x[1]),7]    ) # dec_11_omicron[which(dec_11_omicron[,4] == x[[1]]),7]





# setwd(".../single-test-coef/april-30q")
# tensor <- read.csv('analysis-tensor-vif.csv')
# data <- lapply(omicron_county,function(x) print(x)[[1]])
# data_df <- as.data.frame(data)
# # 
# omicron_quartile_11 <- ifelse(t(data_df) > quantile(t(data_df),prob=0.75)[[1]],1,0)
# Omicron_labels <- data.frame(cbind(fin$Geo_FIPS,omicron_quartile_11))
# saveRDS(Omicron_labels,'omicron-labels.RDS')
# omicron_tensor <- cbind(tensor,omicron_quartile_11)
# write.csv(omicron_tensor,'analysis-tensor-vif-omicron.csv')
# 
# setwd(".../single-test-coef/april-30p")
# tensor <- read.csv('analysis-tensor-vif.csv')
# omicron_tensor <- cbind(tensor,omicron_quartile_11)
# write.csv(omicron_tensor,'analysis-tensor-vif-omicron.csv')
# 
# setwd(".../single-test-coef/march10")
# tensor <- read.csv('analysis-tensor-vif.csv')
# omicron_tensor <- cbind(tensor,omicron_quartile_11)
# write.csv(omicron_tensor,'analysis-tensor-vif-omicron.csv')
# 
# setwd(".../single-test-coef/100-fci")
# tensor <- read.csv('analysis-tensor-vif.csv')
# omicron_tensor <- cbind(tensor,omicron_quartile_11)
# write.csv(omicron_tensor,'analysis-tensor-vif-omicron.csv')
# 
# setwd(".../single-test-coef/50-fci")
# tensor <- read.csv('analysis-tensor-vif.csv')
# omicron_tensor <- cbind(tensor,omicron_quartile_11)
# write.csv(omicron_tensor,'analysis-tensor-vif-omicron.csv')




#========================

#              Dec 18

#========================




dec_18_omicron <- omicron_data[which((omicron_data[,1] == "18-Dec-21" & omicron_data[,5] == "B.1.1.529")),]
dec_18_omicron <- dec_18_omicron[-1,]
omicron_county <- lapply(hhs,function(x) dec_18_omicron[which(dec_18_omicron[,4] == x[[1]]),7]    )


# setwd(".../single-test-coef/april-30q")
# tensor <- read.csv('analysis-tensor-vif.csv')
# data <- lapply(omicron_county,function(x) print(x)[[1]])
# data_df <- as.data.frame(data)
# 
# # 
# omicron_quartile <- ifelse(t(data_df) > quantile(t(data_df),prob=0.75)[[1]],1,0)
# print(sum(omicron_quartile))
# 
# 
# 
# omicron_tensor <- cbind(tensor,omicron_quartile)
# write.csv(omicron_tensor,'analysis-tensor-vif-omicron.csv')



#========================

#              Dec 25

#========================

dec_25_omicron <- omicron_data[which((omicron_data[,1] == "25-Dec-21" & omicron_data[,5] == "B.1.1.529")),]
dec_25_omicron <- dec_25_omicron[-1,]
omicron_county <- lapply(hhs,function(x) dec_25_omicron[which(dec_25_omicron[,4] == x[[1]]),7]    )


# setwd(".../single-test-coef/april-30q")
# tensor <- read.csv('analysis-tensor-vif.csv')
# data <- lapply(omicron_county,function(x) print(x)[[1]])
# data_df <- as.data.frame(data)
# 
# # 
# omicron_quartile <- ifelse(t(data_df) > quantile(t(data_df),prob=0.75)[[1]],1,0)
# print(sum(omicron_quartile))
# 
# 
# 
# omicron_tensor <- cbind(tensor,omicron_quartile)
# write.csv(omicron_tensor,'analysis-tensor-vif-omicron.csv')


#========================

#              Jan 1

#========================

jan_1_omicron <- omicron_data[which((omicron_data[,1] == "1-Jan-22" & omicron_data[,5] == "B.1.1.529")),]
jan_1_omicron <- jan_1_omicron[-1,]
omicron_county <- lapply(hhs,function(x) jan_1_omicron[which(jan_1_omicron[,4] == x[[1]]),7]    )


# setwd(".../single-test-coef/april-30q")
# tensor <- read.csv('analysis-tensor-vif.csv')
# data <- lapply(omicron_county,function(x) print(x)[[1]])
# data_df <- as.data.frame(data)
# 
# # 
# omicron_quartile <- ifelse(t(data_df) > quantile(t(data_df),prob=0.75)[[1]],1,0)
# print(sum(omicron_quartile))
# 
# 
# 
# omicron_tensor <- cbind(tensor,omicron_quartile)
# write.csv(omicron_tensor,'analysis-tensor-vif-omicron.csv')
