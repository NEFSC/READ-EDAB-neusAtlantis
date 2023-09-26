# Test post-processing procedure with new functions
library(ncdf4)
library(dplyr)
library(atlantistools)
library(RNetCDF)
library(gridExtra)
library(ds4psy)
library(plotfunctions)
library(grDevices)
library(RColorBrewer)
library(scales)

colorBlindGrey8  <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
                               
col_grey <- "#DDDDDD"
col_orange <- "#D55E00"
col_cyan <- "#56B4E9"
col_teal <- "#009E73"
col_vermillion <- "#CC79A7"
col_yellow <- "#F0E442"
col_blue <- "#0072B2"
col_wheat <- "#f8ead0"
col_bb1 <- "#E7ECFF"
col_bb2 <- "#C8DDF5"
col_vird_yellgreen <- "#DCE319FF"
col_vird_lightgreen <- "#95D840FF"  
col_vird_green <- "#55C667FF"
col_vird_turq <- "#20A387FF"
col_vird_blue <- "#39568CFF"
    
##Loads post-processing functions

source(paste0(here::here(), '/R/Post_Processing/get_atl_paramfiles.R'))
source(paste0(here::here(), '/R/Post_Processing/process_atl_output.R'))
source(paste0(here::here(), '/R/Post_Processing/make_atlantis_diagnostic_figures.R'))

# Reading other files
ref.run.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/reference_run/fishing_sensitivity_baseline/'
ssb_data <- readRDS(paste0(ref.run.dir, 'Post_Processed/Data/ref_run_SSB_catch.rds'))
adult_age_data <- read.csv(here::here('diagnostics','group_mature_age.csv'))

param.dir = here::here('currentVersion')
out.dir = paste0(ref.run.dir,'Post_Processed/Data/')
fig.dir = '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/figures/manuscript/'

reasonability_data <- read.csv(here::here('data', 'output_diag_reasonability.csv'))
priority_data <- read.csv(here::here('diagnostics','neus_atlantis_group_priority.csv'))%>%
  rename(Overall = 'priority.overall',Code = 'code')
priority_data <- arrange(priority_data,Overall,LongName)
reference_points <-read.csv(here::here('data','Atlantis_Ref_Points.csv'))
groups_with_reference_points <- reference_points$Code

#Run prefix is the filename prefix in the atlantis output (specified in run.bat)
run.prefix = 'neus_output'

#Run function that retreives parameter files
param.ls= get_atl_paramfiles(param.dir = param.dir,
                             atl.dir=ref.run.dir,
                             include_catch=T)

#Load groups data
group.code = atlantistools::get_age_acronyms(param.ls$groups.file)
group.data = atlantistools::load_fgs(param.ls$groups.file)
group.index = dplyr::select(group.data,c(Code,LongName))

reasonability_data <- full_join(reasonability_data,group.index, by="Code")

# Biomass Timeseries ------------------------------------------------------

out.dir <- '/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/reference_run/fishing_sensitivity_baseline/Post_Processed/Data/'
catchfile <-'/net/work3/EDAB/atlantis/Shared_Data/fishing_sensitivity_manuscript/reference_run/fishing_sensitivity_baseline/neus_outputCatch.txt'

#Make biomass timeseries plots
biomass = readRDS(paste0(out.dir,'biomass.rds'))
catch = read.table(catchfile, header=TRUE)
caught_groups <- colnames(catch)

temp.plot.1 = atlantistools::plot_line(biomass)

#biomass by species timeseries

groupNames <- unique(temp.plot.1$data$species)
num_plots <-length(groupNames)

high_priority_group_table <- filter(select(priority_data,Code,LongName,Overall),Overall == "H")
num_priority_plots <- nrow(high_priority_group_table)
high_priority_groups <- high_priority_group_table$LongName

plots_list <- vector(num_plots-1, mode='list')
priority_plots_list <- vector(num_priority_plots-1, mode='list')
times_to_plot <- c(0,1,2,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,
                   17.0,18.0,19.0,20.0,21.0,22.0,23.0,24.0,25.0,26.0,27.0,28.0,29.0,30.0,31.0,
                   32.0,33.0,34.0,35.0,36.0,37.0,38.0,39.0,40.0,41.0,42.0,43.0,44.0,45.0,46.0,
                   47.0,48.0,49.0,50.0,51.0,52.0,53.0,54.0)
times_to_plot_migratory <- c(0.2,1.2,2.2,3.2,4.2,5.2,6.2,7.2,8.2,9.2,10.2,11.2,12.2,13.2,14.2,
                             15.2,16.2,17.2,18.2,19.2,20.2,21.2,22.2,23.2,24.2,25.2,26.2,27.2,
                             28.2,29.2,30.2,31.2,32.2,33.2,34.2,35.2,36.2,37.2,38.2,39.2,40.2,
                             41.2,42.2,43.2,44.2,45.2,46.2,47.2,48.2,49.2,50.2,51.2,52.2,53.2,
                             54.2)

times_to_plot_migratory_2 <- c(0.4,1.4,2.4,3.4,4.4,5.4,6.4,7.4,8.4,9.4,10.4,11.4,12.4,13.4,14.4,
                               15.4,16.4,17.4,18.4,19.4,20.4,21.4,22.4,23.4,24.4,25.4,26.4,27.4,
                               28.4,29.4,30.4,31.4,32.4,33.4,34.4,35.4,36.4,37.4,38.4,39.4,40.4,
                               41.4,42.4,43.4,44.4,45.4,46.4,47.4,48.4,49.4,50.4,51.4,52.4,53.4,
                               54.4)
times_to_plot_migratory_3 <- c(0.6,1.6,2.6,3.6,4.6,5.6,6.6,7.6,8.6,9.6,10.6,11.6,12.6,13.6,14.6,
                               15.6,16.6,17.6,18.6,19.6,20.6,21.6,22.6,23.6,24.6,25.6,26.6,27.6,
                               28.6,29.6,30.6,31.6,32.6,33.6,34.6,35.6,36.6,37.6,38.6,39.6,40.6,
                               41.6,42.6,43.6,44.6,45.6,46.6,47.6,48.6,49.6,50.6,51.6,52.6,53.6,
                               54.6)
p <- 0
hp <- 0

plot.index.df = data.frame(start = c(1,30,60),
                           stop = c(29,59,87))

for(p.group in 1:nrow(plot.index.df)){

  graphics.off()
  png(paste0(fig.dir, 'Figure_2_Biomass_F_',p.group,'.png'),height=2000,width=2000, res= 216, pointsize = 4)
  par(mfrow=c(7,4),oma =c(5,10,2,5))
  for (g in plot.index.df$start[p.group]:plot.index.df$stop[p.group]) {
     groupName <- groupNames[g]
     groupCode <- group.index$Code[group.index$LongName == groupName]
      if ((groupCode != "DC") && (groupCode != "DL") && (groupCode != "DR") && (groupCode != "MA")) {
        print(groupCode)
        indices <- which(temp.plot.1$data$species == groupName)
        ssb_group_data <- filter(ssb_data, species == groupName)
        all_biomasses <- temp.plot.1$data$atoutput[indices]
        all_times <- temp.plot.1$data$time[indices]
        time_indices <- which(all_times %in% times_to_plot)
        if (length(time_indices) == 1) {
          time_indices <- which(all_times %in% times_to_plot_migratory)
        }
        if (length(time_indices) == 1) {
          time_indices <- which(all_times %in% times_to_plot_migratory_2)
        }
        if (length(time_indices) == 1) {
          time_indices <- which(all_times %in% times_to_plot_migratory_3)
        }
        time <- all_times[time_indices]
        years <- time + 1964
        time2 <- time
        ssb_group_data <- filter(ssb_group_data, time %in% time2)
        num_times<- length(time)
        biomass <- all_biomasses[time_indices]
        adult_biomass <- ssb_group_data$ssb
        juvenile_biomass <- biomass - adult_biomass
        juv_proportion <- juvenile_biomass / biomass
        
        group_reasonability <- filter(reasonability_data, LongName == groupName)
        min_reasonability_q <- group_reasonability$Min_Target_q
        max_reasonability_q <- group_reasonability$Max_Target_q
        doReasonability <- !is.na(group_reasonability$Min_Target_q)
        if(doReasonability) {
          min_reasonability_q_line <- rep(min_reasonability_q, num_times)
          max_reasonability_q_line <- rep(max_reasonability_q, num_times)
        } else {
          min_reasonability_q_line <- rep(0, num_times)
          max_reasonability_q_line <- rep(0, num_times)
        }
        
        min_reasonability_q <- group_reasonability$Min_Target / 100000
        max_reasonability_q <- group_reasonability$Max_Target / 100000
  
        biomass <- biomass / 100000
        
        y_limit <- max(biomass)
        if (doReasonability) {
          if ((max_reasonability_q < (max(biomass) * 2)) && (max_reasonability_q > max(biomass))) {
            y_limit <- max_reasonability_q
          } 
        }
        
        adult_proportion <- rep(1,length(juv_proportion))
        par(mar=c(5,3,3,5))
        
        plot(years,biomass,type='l',main=groupName, col = col_vird_blue, lwd=1.5, bty = "n", ylim=c(0,y_limit), xaxt = "n", ylab="", xlab="", las =2,cex.axis=2.75, cex.main=2.75)
        
        plot.set = (plot.index.df$start[p.group]:plot.index.df$stop[p.group])
        last.4 = tail(plot.set,4)
        if(g %in% last.4){
          axis(1, at = c(1970, 1980, 1990, 2000, 2010, 2020), labels=T, cex.axis=2)
        }else{
          axis(1, at = c(1970, 1980, 1990, 2000, 2010, 2020), labels=F, cex.axis=2)
        }

        right.7 = plot.set[(1:length(plot.set)) %% 4 == 0]
        
        if (length(juv_proportion > 0)) {
          par(new=TRUE)
          plot(adult_proportion, type = "l", axes = FALSE, xlab ="", ylab = "", bty = "n", xaxt = "n", yaxt = "n", ylim = c(0,1), col = col_bb1,las =2)
          polygon(c(1, seq(adult_proportion), length(adult_proportion)), c(0, adult_proportion, 0),
                  col = scales::alpha(col_bb1,0.5), border = NA)    
          
          if(g %in% right.7){
            axis(4,at = c(0,0.5,1),labels = T,ylim = c(0,1),cex.axis = 2.75,las =2)
          }else{
            axis(4,at = c(0,0.5,1),labels = F,ylim = c(0,1),cex.axis = 2.75)
          }
        }
        
        if (length(juv_proportion > 0)) {
          par(new=TRUE)
          plot(juv_proportion, type = "l", lwd=0, axes = FALSE, xlab ="", ylab = "", vty = "n", xaxt = "n", yaxt = "n", ylim = c(0,1), col = col_bb2)
          
          polygon(c(1, seq(juv_proportion), length(juv_proportion)), c(0, juv_proportion, 0),
                  col = scales::alpha(col_bb2,0.5), border = NA)    

        }
        
        if (nrow(ssb_group_data) > 0) {
          par(new=TRUE)
          
          ssb_group_data <- mutate(ssb_group_data,F_rate = Catch/ssb)
          F_rate <- ssb_group_data$F_rate
          
          plot(years,F_rate, type='l', col = col_vermillion, lty = 2, lwd=1.5, axes=FALSE,ylim=c(0,1),xaxt="n", yaxt="n",xlab="",ylab="")
          
        }
        

        
  #      if (doReasonability) {
  #        if (max_reasonability_q <= y_limit) {
  #          lines(years,max_reasonability_q_line, col=col_vermillion, lwd = 2, lty = 3, xlab="", ylab="", xaxt="n",yaxt="n")
  #        }
  #        if (min_reasonability_q <= y_limit) {
  #          lines(years,min_reasonability_q_line, col=col_vermillion, lwd = 2, lty = 3, xlab="", ylab="", xaxt="n",yaxt="n")
  #        }
  #        legend(15,y_limit*0.990, bty="n", legend=c("Biomass - surveyed (q corrected)","Biomass - surveyed"), col=c("blue","red"), lty=2:2, cex=0.8)
  #      }
  
      }    
  }
  
  mtext('Biomass (100,000 mt)',2,3,outer = T,cex = 3.5)
  # mtext('Year',1,2,outer = T, cex =3)
  graphics.off()
}


png(paste0(here::here(), '/currentVersion/output/Post_Processed/Data/catch_priority.png'),width=1000,height=1000)
par(mfrow=c(7,4))
for (g in 1:num_plots) {
  groupName <- groupNames[g]
  groupCode <- group.index$Code[group.index$LongName == groupName]
  if (groupName %in% high_priority_groups) {
    indices <- which(temp.plot.1$data$species == groupName)
    ssb_group_data <- filter(ssb_data, species == groupName)
    if (nrow(ssb_group_data) == 0) {
      ssb_group_data <- temp.plot.1$data$atoutput[indices]
    } else {
      ssb_group_data <- ssb_group_data$ssb
    }
    all_biomasses <- temp.plot.1$data$atoutput[indices]
    all_times <- temp.plot.1$data$time[indices]
    time_indices <- which(all_times %in% times_to_plot)
    if (length(time_indices) == 1) {
      time_indices <- which(all_times %in% times_to_plot_migratory)
    }
    if (length(time_indices) == 1) {
      time_indices <- which(all_times %in% times_to_plot_migratory_2)
    }
    if (length(time_indices) == 1) {
      time_indices <- which(all_times %in% times_to_plot_migratory_3)
    }
    time <- all_times[time_indices]
    years <- time + 1964
    time2 <- time
    
    ssb <- ssb_group_data[time_indices]
    ssb <- ssb / 100000
    
    num_times<- length(time)
    
    Bmsy <- 0
    Fmsy <- 0
    if (groupCode %in% groups_with_reference_points) {
      group_ref_points <- filter(reference_points, Code == groupCode)
      Bmsy <- as.numeric(group_ref_points$Bmsy) / 100000
      Fmsy <- as.numeric(group_ref_points$Fmsy)
      Bmsy_line <- rep(Bmsy, num_times)
      Fmsy_line <- rep(Fmsy, num_times)
    }
    
    if (Bmsy > max(ssb)) {
      y_limit <- Bmsy      
    } else {
      y_limit <- max(ssb)
    }
    
    if (groupCode %in% colnames(catch)) {
      groupCatch <- select(catch, c("Time", groupCode))
      groupCatch <- groupCatch[1:num_times,] 
      groupCatch[,2] <- groupCatch[,2] / 100000
      if (y_limit < max(groupCatch[,2])) {
        y_limit <- max(groupCatch[,2])
      }
    }
    y_limit <- y_limit *1.05
    
    par(mar=c(1.2,2.0,1,2.0))
    plot(years,ssb,type='l',main=groupName, lwd=2,col = col_cyan, xlab = "", ylab = "", ylim=c(0,y_limit), xaxt = "n")
    axis(1, at = c(1970, 1980, 1990, 2000, 2010, 2020), labels=F)
    axis(4, at = c(0,y_limit), labels=F)
    F_rate <- 0
    if (groupCode %in% colnames(catch)) {
      groupCatch <- select(catch, c("Time", groupCode))
      groupCatch <- groupCatch[1:num_times,] 
      groupCatch[,2] <- groupCatch[,2] / 100000
      
      if (max(groupCatch[,2]) > 0) {
        lines(years, groupCatch[,2], col = "#999999", lwd = 2,xaxt="n", yaxt="n",xlab="",ylab="")
      }
      F_rate <- groupCatch[,2] / ssb
      par(new = TRUE)
      plot(years,F_rate, type='l', col = col_vermillion, lty = 1, lwd=1, ylim=c(0,1),xaxt="n", yaxt="n",xlab="",ylab="")
    }
  }
}
graphics.off()


