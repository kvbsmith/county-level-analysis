library(usmap)
library(ggplot2)
library(gridExtra)
library(grid)
library(RGraphics)



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


setwd('./data')
vif_fin <- readRDS('fin.RDS')
fci_df <- readRDS('fci.RDS')
com_lev <- read.csv('United_States_COVID-19_Community_Levels_by_County.csv')
may_12_levels <- com_lev[which((substr(com_lev[,which(colnames(com_lev) == 'date_updated')],start=1,stop=10) == "2022-05-12")),]
levels_ord <- may_12_levels[order(as.numeric(may_12_levels$county_fips)),]
levels_ord_reduced <- levels_ord[which(levels_ord$county_fips %in% vif_fin$Geo_FIPS),]
rownames(levels_ord_reduced) <- 1:nrow(levels_ord_reduced)
com_lev_ind <- rep(0,nrow(levels_ord_reduced))
com_lev_ind[which(levels_ord_reduced$covid.19_community_level == "High" | levels_ord_reduced$covid.19_community_level == "Medium")] <- 1
com_lev_ind[which(is.na(com_lev_ind))] <- 0
com_lev_ind <- data.frame(com_lev_ind)

fips_fin <- cbind(vif_fin$Geo_FIPS,fci_df)
vif_fin$Geo_FIPS[which(nchar(vif_fin$Geo_FIPS) == 4)] <- paste0(0,vif_fin$Geo_FIPS[which(nchar(vif_fin$Geo_FIPS) == 4)])

# png('county-fci-us.png',width=9,height=5,res=400,units='in')
# #get zero-valued FCI
# no_fci_indices <- which(fips_fin$fci_list_fin == 0)
# #assign remaining FCI categories
# pred_vector <- ifelse(fips_fin$fci_list_fin <= 50,
#                       '1',ifelse(fips_fin$fci_list_fin >= 250,'4',
#                       ifelse(fips_fin$fci_list_fin >= 100,'3','2')))
# pred_vector[no_fci_indices] <- 700
# 
# 
# 
# 
# viz_df <- as.data.frame(cbind(vif_fin$Geo_FIPS,pred_vector))
# colnames(viz_df) <- c('fips','X')
# 
# p3 = plot_usmap(regions = 'counties',data = viz_df,values = 'X')+
#   scale_fill_manual(name="",labels = c("Before/On March 11",'March 12 - April 30',"May 1 - September 26", "September 27 and After", "No FCI Reported","No Testing Data"),values = c("green","#333366","red","yellow","blue",'black'))+
#   ggtitle("First Confirmed COVID-19 Infection (FCI) Date by County (2020)")+
#   theme(legend.position = 'bottom',plot.title = element_text(hjust = 0.5,size=22),legend.justification = 'center',legend.text=element_text(size=12),legend.title =element_text(size=12))
# p3
# dev.off()


omicron_labels <- readRDS('omicron-labels.RDS')
#==============================================

#             OMICRON VALIDATION CLASSIFICATION

#===============================================


#=========================================================================================================

#         CREATE NATIONAL MAP OF MODEL PREDICTIONS AND VALIDATION DATA

#=========================================================================================================

setwd("./sensitivity/april-30q")

py_data <- read.csv('OMICRON-full-data-pred-table-plus-prob.csv')
omicron_pred <- py_data[,colnames(py_data)%in%c('SAP')]
omicron_pred <- data.frame(cbind(vif_fin$Geo_FIPS,omicron_pred))
colnames(omicron_pred) <- c("fips",'X')

p1 <- plot_usmap(regions = 'counties',data = omicron_pred,values = 'X')+ scale_fill_manual(name="",labels = c("Not Upper Quartile", "Upper Quartile", "No Data"),values=c("#F5793A", "#85C0F9"))+
ggtitle("(a) Model Prediction of Omicron Incidence")+ #
  theme(legend.position = "bottom", legend.title = element_text(size = 22),legend.text = element_text(size = 16),plot.title = element_text(hjust = 0.5,size=22))


colnames(omicron_labels) <- c("fips",'X')
p2 <- plot_usmap(regions = 'counties',data = omicron_labels,values = 'X')+ scale_fill_manual(name="",labels = c("Not Upper Quartile", "Upper Quartile", "No Data"),values=c("#F5793A", "#85C0F9"))+
ggtitle("(b) Omicron Incidence of Weeks Ending in December 2021")+ 
  theme(legend.position = "bottom", legend.title = element_text(size = 22),legend.text = element_text(size = 16),plot.title = element_text(hjust = 0.5,size=22))



title_text = "Figure 1"
p_text <- splitTextGrob(title_text,gp=gpar(fontsize=22))

gl=list(p1,p2,p_text)

setEPS()
postscript("figure-1-omicron-classification.eps",width=8,height=9)
# png(file="figure-1-omicron-classification.png",
# width=8, height=9, units="in", res=400)
grid.arrange(
  grobs = gl,
  widths = c(1,1,1,1,1,1,1,1,1,1,1,1),
  layout_matrix = rbind(c(NA,NA,NA,3,3,3,3,3,3,3,NA,NA),
                        c(1,1,1,1,1,1,1,1,1,1,1,1),
                        c(1,1,1,1,1,1,1,1,1,1,1,1),
                        c(1,1,1,1,1,1,1,1,1,1,1,1),
                        c(1,1,1,1,1,1,1,1,1,1,1,1),
                        c(1,1,1,1,1,1,1,1,1,1,1,1),
                        c(1,1,1,1,1,1,1,1,1,1,1,1),
                        c(1,1,1,1,1,1,1,1,1,1,1,1),
                        c(1,1,1,1,1,1,1,1,1,1,1,1),
                        c(1,1,1,1,1,1,1,1,1,1,1,1),
                        c(1,1,1,1,1,1,1,1,1,1,1,1),
                        c(2,2,2,2,2,2,2,2,2,2,2,2),
                        c(2,2,2,2,2,2,2,2,2,2,2,2),
                        c(2,2,2,2,2,2,2,2,2,2,2,2),
                        c(2,2,2,2,2,2,2,2,2,2,2,2),
                        c(2,2,2,2,2,2,2,2,2,2,2,2),
                        c(2,2,2,2,2,2,2,2,2,2,2,2),
                        c(2,2,2,2,2,2,2,2,2,2,2,2),
                        c(2,2,2,2,2,2,2,2,2,2,2,2),
                        c(2,2,2,2,2,2,2,2,2,2,2,2),
                        c(2,2,2,2,2,2,2,2,2,2,2,2)))

dev.off()

#=========================================================================================================

#         CREATE NATIONAL MAP OF MODEL PREDICTIONS AND VALIDATION DATA

#=========================================================================================================

setwd("./sensitivity/april-30q")

py_data <- read.csv('dec-omi-retro_full-data-pred-table-plus-prob.csv')
omicron_pred <- py_data[,colnames(py_data)%in%c('SAP')]
omicron_pred <- data.frame(cbind(vif_fin$Geo_FIPS,omicron_pred))
colnames(omicron_pred) <- c("fips",'X')

p1 <- plot_usmap(regions = 'counties',data = omicron_pred,values = 'X')+ scale_fill_manual(name="",labels = c("Not Upper Quartile", "Upper Quartile", "No Data"),values=c("#F5793A", "#85C0F9"))+
ggtitle("(a) Model Prediction of Omicron Incidence")+ #
  theme(legend.position = "bottom", legend.title = element_text(size = 22),legend.text = element_text(size = 16),plot.title = element_text(hjust = 0.5,size=22))


colnames(omicron_labels) <- c("fips",'X')
p2 <- plot_usmap(regions = 'counties',data = omicron_labels,values = 'X')+ scale_fill_manual(name="",labels = c("Not Upper Quartile", "Upper Quartile", "No Data"),values=c("#F5793A", "#85C0F9"))+
ggtitle("(b) Omicron Incidence of Weeks Ending in December 2021")+ 
  theme(legend.position = "bottom", legend.title = element_text(size = 22),legend.text = element_text(size = 16),plot.title = element_text(hjust = 0.5,size=22))



title_text = "Figure 1"
p_text <- splitTextGrob(title_text,gp=gpar(fontsize=22))

gl=list(p1,p2,p_text)

# setEPS()
# postscript("figure-1-omicron-classification_new.eps",width=8,height=9)
png(file="figure-1-omicron-classification_new.png",
width=8, height=9, units="in", res=400)
grid.arrange(
  grobs = gl,
  widths = c(1,1,1,1,1,1,1,1,1,1,1,1),
  layout_matrix = rbind(c(NA,NA,NA,3,3,3,3,3,3,3,NA,NA),
                        c(1,1,1,1,1,1,1,1,1,1,1,1),
                        c(1,1,1,1,1,1,1,1,1,1,1,1),
                        c(1,1,1,1,1,1,1,1,1,1,1,1),
                        c(1,1,1,1,1,1,1,1,1,1,1,1),
                        c(1,1,1,1,1,1,1,1,1,1,1,1),
                        c(1,1,1,1,1,1,1,1,1,1,1,1),
                        c(1,1,1,1,1,1,1,1,1,1,1,1),
                        c(1,1,1,1,1,1,1,1,1,1,1,1),
                        c(1,1,1,1,1,1,1,1,1,1,1,1),
                        c(1,1,1,1,1,1,1,1,1,1,1,1),
                        c(2,2,2,2,2,2,2,2,2,2,2,2),
                        c(2,2,2,2,2,2,2,2,2,2,2,2),
                        c(2,2,2,2,2,2,2,2,2,2,2,2),
                        c(2,2,2,2,2,2,2,2,2,2,2,2),
                        c(2,2,2,2,2,2,2,2,2,2,2,2),
                        c(2,2,2,2,2,2,2,2,2,2,2,2),
                        c(2,2,2,2,2,2,2,2,2,2,2,2),
                        c(2,2,2,2,2,2,2,2,2,2,2,2),
                        c(2,2,2,2,2,2,2,2,2,2,2,2),
                        c(2,2,2,2,2,2,2,2,2,2,2,2)))

dev.off()


#=========================================================================================================

#         Produce Individual Plots

#=========================================================================================================



setEPS()
postscript("figure-1-part-a.eps",width=8,height=9)
# png(file="figure-1-part-a.png",
#     width=8, height=9, units="in", res=400)
plot_usmap(regions = 'counties',data = omicron_pred,values = 'X')+ scale_fill_manual(name="",labels = c("Not Upper Quartile", "Upper Quartile", "No Data"),values=c("#F5793A", "#85C0F9"))+
  ggtitle("")+ #
  theme(legend.position = "bottom", legend.title = element_text(size = 22),legend.text = element_text(size = 16),plot.title = element_text(hjust = 0.5,size=22))
dev.off()

setEPS()
postscript("figure-1-part-b.eps",width=8,height=9)
# png(file="figure-1-part-b.png",
#     width=8, height=9, units="in", res=400)
plot_usmap(regions = 'counties',data = omicron_labels,values = 'X')+ scale_fill_manual(name="",labels = c("Not Upper Quartile", "Upper Quartile", "No Data"),values=c("#F5793A", "#85C0F9"))+
  ggtitle("")+ 
  theme(legend.position = "bottom", legend.title = element_text(size = 22),legend.text = element_text(size = 16),plot.title = element_text(hjust = 0.5,size=22))
dev.off()

#=========================================================================================================

#         Model Predictions and Their Quality

#=========================================================================================================

setwd(".../sensitivity/april-30q")

py_data <- read.csv('dec-omi-retro_full-data-pred-table-plus-prob.csv')
py_data_tb <- read.csv('temporal+baseline_dec-omi-retro_full-data-pred-table-plus-prob.csv')
omicron_pred <- py_data[,colnames(py_data)%in%c('SAP')]
omicron_pred <- data.frame(cbind(vif_fin$Geo_FIPS,omicron_pred))
omicron_pred_tb <- py_data_tb[,colnames(py_data_tb)%in%c('SAP')]
omicron_pred_tb <- data.frame(cbind(vif_fin$Geo_FIPS,omicron_pred_tb))
colnames(omicron_pred) <- c("fips",'X')
colnames(omicron_pred_tb) <- c("fips",'X')
colnames(omicron_labels) <- c("fips",'X')

#,aes(pattern=X)-scale_pattern_manual(values = c('0'="stripe",'1'="none",'NA'='crosshatch')) +
p1 <- plot_usmap(regions = 'counties',data = omicron_pred,values = 'X')+ scale_fill_manual(name="",labels = c("Not Upper Quartile", "Upper Quartile", "No Data"),values=c("#F5793A", "#85C0F9"))+
ggtitle("(a) Baseline Prediction of Omicron Incidence")+ #
  theme(legend.position = "bottom", legend.title = element_text(size = 22),legend.text = element_text(size = 16),plot.title = element_text(hjust = 0.5,size=18))
  # guides(pattern = guide_legend(override.aes = list(fill = "white")))

# library(ggpattern)
# MainStates <- map_data("state")
# AllCounty <- map_data("county")
# ggplot(data = omicron_pred,values = 'X',aes(pattern='X')) + geom_polygon( data=AllCounty, aes(x=long, y=lat,group=group),
#                 color="darkblue", fill="lightblue", size = .1 ) +
  
#           geom_polygon( data=MainStates, aes(x=long, y=lat, group=group),
#                 color="black", fill="lightblue",  size = 1, alpha = .3) +
#        scale_fill_manual(name="",labels = c("Not Upper Quartile", "Upper Quartile", "No Data"),values=c("#F5793A", "#85C0F9"))+
# scale_pattern_manual(values = c('0'="stripe",'1'="none",'NA'='crosshatch')) +         
# ggtitle("(a) Baseline Prediction of Omicron Incidence")+ #
#   theme(legend.position = "bottom", legend.title = element_text(size = 22),legend.text = element_text(size = 16),plot.title = element_text(hjust = 0.5,size=18))


p2 <- plot_usmap(regions = 'counties',data = omicron_labels,values = 'X')+ scale_fill_manual(name="",labels = c("Not Upper Quartile", "Upper Quartile", "No Data"),values=c("#F5793A", "#85C0F9"))+
ggtitle("(c) Omicron Incidence of Weeks Ending in December 2021")+ 
  theme(legend.position = "bottom", legend.title = element_text(size = 22),legend.text = element_text(size = 16),plot.title = element_text(hjust = 0.5,size=18))

p3 <- plot_usmap(regions = 'counties',data = omicron_pred_tb,values = 'X')+ scale_fill_manual(name="",labels = c("Not Upper Quartile", "Upper Quartile", "No Data"),values=c("#F5793A", "#85C0F9"))+
ggtitle("(b) Temporal+Baseline Prediction of Omicron Incidence")+ #
  theme(legend.position = "bottom", legend.title = element_text(size = 22),legend.text = element_text(size = 16),plot.title = element_text(hjust = 0.5,size=18))

# pred_vector <- ifelse(py_data_tb$Test == 0,
#                       ifelse(py_data_tb$SAP == 0,'tn','fp'),
#                       ifelse(py_data_tb$SAP == 0,'fn','tp'))
case_ind <- get_cases_per_100k_7_days('12/11/2021')
case_ind[which(case_ind < 0)] <- 0
omicron_quartile <- ifelse(case_ind > quantile(case_ind,prob=0.75)[[1]],1,0)

com_lev_df <- readRDS('cdc_levels_12_11_22.RDS')
com_lev_ind <- rep(0,nrow(com_lev_df))
com_lev_ind[which(com_lev_df$com_lev_vector == "High")] <- 1
com_lev_ind[which(is.na(com_lev_ind))] <- 0
com_lev_ind <- data.frame(com_lev_ind)


pred_vector <- ifelse(com_lev_ind == 0,
                      ifelse(py_data_tb$SAP == 0,'tn','fp'),
                      ifelse(py_data_tb$SAP == 0,'fn','tp'))

viz_df <- as.data.frame(cbind(fin$Geo_FIPS,pred_vector)) #omicron_quartile
colnames(viz_df) <- c('fips','X')
p4 = plot_usmap(regions = 'counties',data = viz_df,values = 'X')+scale_fill_manual(name="Codes",labels = c("False Negative", "False Positive", "True Negative",'True Positive',"Not in Hold-out Testing Set"),values = c("red","yellow","#333366","green","blue"))+
ggtitle("(e) T-B Model Predictions on CDC 12/11")+ 
  theme(legend.position = "bottom", legend.title = element_text(size = 22),legend.text = element_text(size = 16),plot.title = element_text(hjust = 0.5,size=18))+guides(fill = guide_legend(nrow = 3, byrow = TRUE))


viz_df <- as.data.frame(cbind(fin$Geo_FIPS,com_lev_df$com_lev_vector)) #omicron_quartile
colnames(viz_df) <- c('fips','X')
p5 = plot_usmap(regions = 'counties',data = viz_df,values = 'X')+scale_fill_manual(name="",labels = c("High", "Low", "Medium","No data"),values = c("red","green","yellow","blue"))+
ggtitle("(d) CDC Community Level 12/11/2021")+ 
  theme(legend.position = "bottom", legend.title = element_text(size = 22),legend.text = element_text(size = 16),plot.title = element_text(hjust = 0.5,size=18))+guides(fill = guide_legend(nrow = 2, byrow = TRUE))


title_text = "Figure 1a-e"
p_text <- splitTextGrob(title_text,gp=gpar(fontsize=22))

gl=list(p1,p2,p_text,p3,p5,p4)

# setEPS()
# postscript("figure-1-omicron-classification_new.eps",width=8,height=9)
png(file="test_fig_dec_11.png",
width=16, height=12, units="in", res=400)
grid.arrange(
  grobs = gl,
  widths = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
  layout_matrix = rbind(c(NA,NA,NA,NA,NA,3,3,3,3,3,3,3,NA,NA,NA,NA,NA,NA),
                        c(1,1,1,1,1,1,1,1,1,4,4,4,4,4,4,4,4,4),
                        c(1,1,1,1,1,1,1,1,1,4,4,4,4,4,4,4,4,4),
                        c(1,1,1,1,1,1,1,1,1,4,4,4,4,4,4,4,4,4),
                        c(1,1,1,1,1,1,1,1,1,4,4,4,4,4,4,4,4,4),
                        c(1,1,1,1,1,1,1,1,1,4,4,4,4,4,4,4,4,4),
                        c(1,1,1,1,1,1,1,1,1,4,4,4,4,4,4,4,4,4),
                        c(1,1,1,1,1,1,1,1,1,4,4,4,4,4,4,4,4,4),
                        c(1,1,1,1,1,1,1,1,1,4,4,4,4,4,4,4,4,4),
                        c(1,1,1,1,1,1,1,1,1,4,4,4,4,4,4,4,4,4),
                        c(1,1,1,1,1,1,1,1,1,4,4,4,4,4,4,4,4,4),
                        c(2,2,2,2,2,2,5,5,5,5,5,5,6,6,6,6,6,6),
                        c(2,2,2,2,2,2,5,5,5,5,5,5,6,6,6,6,6,6),
                        c(2,2,2,2,2,2,5,5,5,5,5,5,6,6,6,6,6,6),
                        c(2,2,2,2,2,2,5,5,5,5,5,5,6,6,6,6,6,6),
                        c(2,2,2,2,2,2,5,5,5,5,5,5,6,6,6,6,6,6),
                        c(2,2,2,2,2,2,5,5,5,5,5,5,6,6,6,6,6,6),
                        c(2,2,2,2,2,2,5,5,5,5,5,5,6,6,6,6,6,6),
                        c(2,2,2,2,2,2,5,5,5,5,5,5,6,6,6,6,6,6),
                        c(2,2,2,2,2,2,5,5,5,5,5,5,6,6,6,6,6,6),
                        c(2,2,2,2,2,2,5,5,5,5,5,5,6,6,6,6,6,6)))

dev.off()
