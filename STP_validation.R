########### Read Maumee SWAT model HRU outputs ################
rm(list=ls())
library(tidyverse)
library(gridExtra)
library(here)


#### HABRI OLEC inputs ####
# make sure to use \\ or / instead of \ in the path
scenario_dir<-'D:\\Maumee model\\HABRI_OLEC_Scenarios_JAN2024'

yrs<-c(2007:2021)

headers_hru<-c("LULC",  "HRU"     ,  "GIS"  ,"SUB",  "MGT" , "MON"  , "AREAkm2"      ,"ETmm",  
               "SW_ENDmm"    ,"PERCmm","SURQ_CNTmm" ,"LATQGENmm"    ,"GW_Qmm"    ,"WYLDmm"   ,
               "QTILEmm" ,"SOLPkg/ha" ,"LATQCNTmm" ,"ORGPkg/ha" ,"SEDPkg/ha", "TNO3kg/ha", 
               "ORGNkg/ha","NSURQkg/ha",  "PUPkg/ha" ,"P_GWkg/ha",    "P_STRS",   "YLDt/ha","TVAPkg/ha")

# Folder structure should be

# Scenario dir
  # baseline
    # output.hru
    # hru_table.txt


# function to read hru and rch outputs



readSWATtxt<-function(wd,headers,output_file){
  
  setwd(wd)
  
  # HRU output structure
  # line 9 = header column
  
  tmp<-file(output_file)
  
  hru_output<-readLines(tmp, n = -1)
  close(tmp)
  
  file_heading<-hru_output[9] # hru file headings
  
  hru_txt<-hru_output[10:length(hru_output)] # hru data text, remove non-data lines
  
  hru_output<-tibble(.rows=length(hru_txt))
  
  #### FUNCTION TO EXTRACT COLUMNS AND ADD THEM TO A DATA FRAME #####
  
  # column name as defined as input in code
  # text in out file heading (with SWAT output spacing), 
  # readLines output of ONLY data columns (crop anything headers and above)
  # data frame to be filled
  
  start<-1 
  
  for (col_input in headers){
    
    location<-gregexpr(pattern =col_input,file_heading)
    start<-start # assign to 1 or previous stopping point
    stop<-unlist(location[1])+nchar(col_input)-1
    
    new_col<-substr(hru_txt,start,stop)
    
    hru_output<-hru_output %>% 
      mutate({{col_input}} := new_col)
    
    start<-stop+1
    
  }
  
  return(hru_output)
  
}


##################### HRU OUTPUTS ##########################################################

  ###### Combine output.hru from all baseline and scenario folders ############
  hru_output<-c()
  
  ### load baseline data ###
  # baseline data
  
  ### baseline hru table ###
  wd<-paste0(scenario_dir,'//','Baseline')
  hru_table_name<-list.files(wd)[grep('.txt',list.files(wd))] # assume hru table is only txt in folder
  
  hru_table<-read.csv(paste0(wd,'//',hru_table_name),sep="\t") %>% 
    select(HRU_GIS,SOL_SOLP_0_5,Tile_Drain)
  
  ### baseline output.hru ####
  baseline_hru<-readSWATtxt(paste0(scenario_dir,'//','baseline'),headers_hru,'output.hru') %>% 
    mutate(across(!LULC, as.numeric)) %>% # convert everything except lulc to numeric
    
    left_join(.,hru_table,by=c("GIS"="HRU_GIS")) %>% # join features from HRU table
    
    #add year column, assume monthly data
    mutate(YR=c(as.character(rep(yrs,each=length(unique(GIS))*12+length(unique(GIS)))),rep('all years',length(unique(GIS))))) %>%
    
    # total P and N
    rowwise() %>% 
    mutate(totp_kgha=`SOLPkg/ha`+`ORGPkg/ha`+`SEDPkg/ha`+`TVAPkg/ha`+`P_GWkg/ha`,
           totsolp_kgha=`SOLPkg/ha`+`TVAPkg/ha`,
           totn_kgha=`TNO3kg/ha`+`ORGNkg/ha`+`NSURQkg/ha`) %>% 
    
    # remove unneeded variables
    select(-c(SW_ENDmm, LATQGENmm , GW_Qmm, LATQCNTmm,`P_GWkg/ha`,`YLDt/ha`,`P_STRS`,`PUPkg/ha`)) %>% 

  
  
  # change from kg/ha/yr to kg/yr
    rowwise() %>% 
    mutate(totp_kg=totp_kgha*AREAkm2*100,
           totsolp_kg=totsolp_kgha*AREAkm2*100,
           totn_kg=totn_kgha*AREAkm2*100,
           QTILEm3=QTILEmm*AREAkm2*10^6/1000, # m3
           SURQ_CNTm3=SURQ_CNTmm*AREAkm2*10^6/1000,
           SOLP_kg=`SOLPkg/ha`*AREAkm2*100,
           TVAP_kg=`TVAPkg/ha`*AREAkm2*100,
           ORGP_kg=`ORGPkg/ha`*AREAkm2*100,
           SEDP_kg=`SEDPkg/ha`*AREAkm2*100) %>% 
    ungroup() # %>% 
    
    # Only look at row-crop agricultural HRUs
    # filter(LULC %in% c("CORN","SOYB","RYE","WWHT"))
    
    # gather(variable,value_b,-LULC,-HRU,-GIS,-SUB,-MGT,-MON,-AREAkm2,-YR) #%>% #,-YR,-SOL_SOLP_0_5
    
    
    #### extract only annual hru outputs #####
  annual_hru<-baseline_hru %>% 
    filter(MON %in% yrs) 
  
  write.csv(annual_hru,'annual_hru_output.csv')
  
  annual_hru_plot<-annual_hru %>% 
    select(GIS,MON,YR,Tile_Drain,SURQ_CNTmm,QTILEmm) %>% 
    rowwise() %>% 
    mutate(tot_discharge=SURQ_CNTmm+QTILEmm) %>% 
    ungroup() %>% 
    gather(variable,value,-YR,-MON,-GIS,-Tile_Drain) %>% 
    filter(Tile_Drain==1) %>% 
    ggplot(aes(x=variable,y=value))+geom_boxplot()+facet_wrap(~YR)+
    xlab("")+ylab('Discharge (mm)')+ 
    scale_x_discrete(labels=c("QTILEmm" = "Tile", "SURQ_CNTmm" = "Surface","tot_discharge"="Surface +\ntile"))+
    theme_bw()+
    theme(panel.background = element_blank(),   # Background of plotting area
          panel.border = element_rect(fill=NA),       # Border around plotting area.
          # fill argument should be NA
          panel.grid = element_blank())
  
  ggsave('surface_tile_annual.png',last_plot(),height=200,width=200,units='mm')
  
  
  P_loss_vSTP<-annual_hru %>% 
    rowwise() %>% 
    mutate(SOL_SOLP_CAT=NA) %>% 
    mutate(SOL_SOLP_CAT=replace(SOL_SOLP_CAT,SOL_SOLP_0_5<100,'<100 ppm')) %>% 
    mutate(SOL_SOLP_CAT=replace(SOL_SOLP_CAT,SOL_SOLP_0_5>=100,'>=100 ppm')) %>% 
    mutate(tot_flow_m3=SURQ_CNTm3+QTILEm3,
      totp_FWMC_mgl=totp_kg*1000/tot_flow_m3,
      totsolp_FWMC_mgl=totsolp_kg*1000/tot_flow_m3,
      solptile_FWMC_mgl=TVAP_kg*1000/QTILEm3,
      solpsurf_FWMC_mgl=SOLP_kg*1000/SURQ_CNTm3) %>% 
    select(GIS,MON,YR,Tile_Drain,SOL_SOLP_0_5,SOL_SOLP_CAT,Tile_Drain,
           totp_FWMC_mgl,totsolp_FWMC_mgl,solptile_FWMC_mgl,solpsurf_FWMC_mgl,tot_flow_m3,QTILEmm,SURQ_CNTmm) %>% 
    gather(variable,value,-YR,-MON,-GIS,-Tile_Drain,-SOL_SOLP_0_5,-SOL_SOLP_CAT,-tot_flow_m3,-QTILEmm,-SURQ_CNTmm) %>% 
    mutate(variable=factor(variable,levels=c('totp_FWMC_mgl','totsolp_FWMC_mgl','solptile_FWMC_mgl','solpsurf_FWMC_mgl'),ordered=T))
  
  variable_labs<-c('Total P','Total soluble P', 'Soluble P (tile)','Soluble P (surface)')
  names(variable_labs)<-c('totp_FWMC_mgl','totsolp_FWMC_mgl','solptile_FWMC_mgl','solpsurf_FWMC_mgl')
  
  P_loss_plot<-P_loss_vSTP %>% 
    filter(Tile_Drain==1) %>%
    ggplot(aes(x=SOL_SOLP_CAT,y=value))+geom_boxplot()+facet_grid(variable~YR,scales='free_y',labeller=labeller(variable=variable_labs))+
    xlab("STP")+ylab('FWMC (mg/l)')+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          panel.background = element_blank(),   # Background of plotting area
          panel.border = element_rect(fill=NA),       # Border around plotting area.
          # fill argument should be NA
          panel.grid = element_blank())
  
  ggsave('Ploss_annual_tile.png',last_plot(),height=200,width=225,units='mm')
  
  P_loss_plot<-P_loss_vSTP %>% 
    filter(Tile_Drain==0, !(variable %in% c("solptile_FWMC_mgl"))) %>%
    ggplot(aes(x=SOL_SOLP_CAT,y=value))+geom_boxplot()+facet_grid(variable~YR,scales='free_y',labeller=labeller(variable=variable_labs))+
    xlab("STP")+ylab('FWMC (mg/l)')+    
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                                              panel.background = element_blank(),   # Background of plotting area
                                              panel.border = element_rect(fill=NA),       # Border around plotting area.
                                              # fill argument should be NA
                                              panel.grid = element_blank())
  
  ggsave('Ploss_annual_notile.png',last_plot(),height=200,width=175,units='mm')  

  
  
  
    #### extract only mar-jul hru outputs ####
  mar_jul_hru<-baseline_hru %>% 
    filter(!(YR %in% 'all years'), !(MON==YR) , MON %in% c(3:7)) %>% 
    
    # create summary of mar-jul outputs for each hru, every year
    group_by(YR,GIS) %>% 
    summarize(totp_kg=sum(totp_kg),
              totsolp_kg=sum(totsolp_kg),
              totn_kg=sum(totn_kg),
              QTILEm3=sum(QTILEm3), # m3
              SURQ_CNTm3=sum(SURQ_CNTm3),
              SOLP_kg=sum(SOLP_kg),
              TVAP_kg=sum(TVAP_kg),
              
              SOL_SOLP_0_5=mean(SOL_SOLP_0_5), # lookup values
              Tile_Drain=mean(Tile_Drain), # lookup values
              
              totp_kgha=sum(totp_kgha),
              totn_kgha=sum(totn_kgha),
              totsolp_kgha=sum(totsolp_kgha),
              `SOLPkg/ha`=sum(`SOLPkg/ha`),
              `TVAPkg/ha`=sum(`TVAPkg/ha`))
  
  write.csv(mar_jul_hru,'marjul_hru_output.csv')