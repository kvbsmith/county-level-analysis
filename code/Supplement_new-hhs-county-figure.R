library(janitor)
library(usmap)
library(ggplot2)
library(png)
library(patchwork)
library(magick)
library(RGraphics)
library(grid)

setwd('"./data')
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

omicron_vals <- data.frame(cbind(fin$Geo_FIPS,as.numeric(t(data_df)[,1])))

colnames(omicron_vals) <- c("fips",'X')
omicron_vals$X <- as.numeric(omicron_vals$X)
# p2 <- plot_usmap(regions = 'counties',data = omicron_vals,values = 'X')+
# ggtitle("(b) Omicron Incidence of Weeks Ending in December 2021")+ 
#   theme(legend.position = "bottom", legend.title = element_text(size = 22),legend.text = element_text(size = 16),plot.title = element_text(hjust = 0.5,size=22))
# p2

#Assign labels to cities that are the capitals of HHS Regions

cities_t <- read.csv('uscities.csv')
cities_t <- cities_t[which(cities_t$city %in% c('Boston','San Francisco','New York','Philadelphia','Atlanta','Chicago','Dallas','Kansas City','Denver','Seattle')),]
cities_t <- cities_t[c(1:10),]
cities_t[10,which(colnames(cities_t) == 'lng')] <- -96.4
cities_t[9,which(colnames(cities_t) == 'lat')] <- 43
cities_t[7,which(colnames(cities_t) == 'lat')] <- 44
colnames(cities_t)[which(colnames(cities_t) == 'lng')] <- 'lon'
cities_t <- usmap_transform(cities_t)

cities_t <- cbind(cities_t,hhs_label=c(paste(rep('HHS Region',times=10),c(2,5,6,3,4,1,10,9,8,7))))
p2 <- plot_usmap(
  data = omicron_vals, values = "X", regions = 'counties', color = "black"
) +   ggrepel::geom_label_repel(data = cities_t,
             aes(x = x, y = y, label = hhs_label),
             size = 2.1, alpha = 0.8,
             label.r = unit(0.5, "lines"), label.size = 0.5,
             segment.color = "red", segment.size = 1,
             seed = 1002) +
  scale_fill_continuous(
    low = "white", high = "blue", name = "Rate of Genomically Sequenced Cases Omicron Positive", label = scales::comma
  ) + 
  labs(title = "(a)", subtitle = "Geographic Depiction of CDC Genomic Data Nationally") +
  theme(plot.title = element_text(size=12,hjust = 0.5),plot.subtitle = element_text(size=8,hjust = 0.8),legend.position = "bottom",legend.justification='center',legend.title = element_text(size = 8),legend.text = element_text(size = 6))
p2

png(file="figure-2-omicron-data.png",
width=5, height=5, units="in", res=400)
p2
dev.off()

setwd("./sensitivity/april-30q")
risk_pred <- read.csv('alpha_ba5-retro_full-data-pred-table-plus-prob_cases.csv')
risk_vals <- data.frame(cbind(fin$Geo_FIPS,risk_pred[,which(colnames(risk_pred) == 'Six_Prob')]))
colnames(risk_vals) <- c("fips",'X')
risk_vals$X <- as.numeric(risk_vals$X)

p1 <- plot_usmap(
  data = risk_vals, values = "X", regions = 'counties', color = "black"
) +
  scale_fill_continuous(
    low = "white", high = "red", name = "Out-of-Sample Risk Prediction", label = scales::comma
  ) + 
  labs(title = "Figure 3", subtitle = "Depiction of Emerging Variant Prospective Risk Prediction") +
  theme(legend.position = "bottom",legend.justification='center')
p1



my_image1 <-  image_ggplot(image_read("cdc-variant-tracker-data.png"))+ 
  labs(title = "(c)", subtitle = "CDC Genomic Data Available when Omicron Variant is Dominant in US")+
  theme(plot.title = element_text(size=12,hjust = 0.5,vjust=18),plot.subtitle = element_text(size=8,hjust = 0.5,vjust=28),legend.position = "bottom",legend.justification='center',legend.title = element_text(size = 8),legend.text = element_text(size = 6)) 
  
my_image2 <-  image_ggplot(image_read("cdc-variant-tracker-data-dec11.png"))+ 
  labs(title = "(b)", subtitle = "CDC Genomic Data Available when Omicron Variant is Emerging in US")+
  theme(plot.title = element_text(size=12,hjust = 0.5,vjust=19),plot.subtitle = element_text(size=8,hjust = 0.5,vjust=29),legend.position = "bottom",legend.justification='center',legend.title = element_text(size = 8),legend.text = element_text(size = 6)) 
  



title_text = "Figure 2"
p_text <- splitTextGrob(title_text,gp=gpar(fontsize=16))

gl=list(my_image2,p2,p_text,my_image1)

png(file="Figure2_Omicron-CDC.png",
width=13, height=4, units="in", res=400)
grid.arrange(
  grobs = gl,
  widths = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
  layout_matrix = rbind(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,3,3,NA,NA,NA,NA,NA,NA,NA),
                        matrix(rep(c(2,2,2,2,2,2,2,1,1,1,1,1,NA,4,4,4,4,4),times=17),byrow=TRUE,ncol=18)))

dev.off()


png(file="Figure3_Pred.png",
width=9, height=6, units="in", res=400)
p1 <- plot_usmap(
  data = risk_vals, values = "X", regions = 'counties', color = "black"
) +
  scale_fill_continuous(
    low = "white", high = "red", name = "Out-of-Sample Risk Prediction", label = scales::comma
  ) + 
  labs(title = "Figure 3", subtitle = "Depiction of Emerging Variant Prospective Risk Prediction") +
  theme(legend.position = "bottom",legend.justification='center',plot.title = element_text(size=18,hjust = 0.5),plot.subtitle = element_text(size=12,hjust = 0.5))
p1

dev.off()
