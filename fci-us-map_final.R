library(usmap)
library(ggplot2)
library(gridExtra)
library(grid)
library(RGraphics)

vif_fin <- readRDS('fin.RDS')
fci_df <- readRDS('fci.RDS')
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

setwd(".../single-test-coef/april-30q")
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

