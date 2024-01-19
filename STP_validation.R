########### Read Maumee SWAT model HRU outputs ################
rm(list=ls())
library(tidyverse)
library(gridExtra)
library(here)


#### HABRI OLEC inputs ####
scenario_dir<-'D:\\Maumee model\\HABRI_OLEC_Scenarios_DEC'
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
    mutate(totp=`SOLPkg/ha`+`ORGPkg/ha`+`SEDPkg/ha`+`TVAPkg/ha`+`P_GWkg/ha`,
           totsolp=`SOLPkg/ha`+`TVAPkg/ha`,
           totn=`TNO3kg/ha`+`ORGNkg/ha`+`NSURQkg/ha`) %>% 
    
    # remove unneeded variables
    select(-c(SW_ENDmm, LATQGENmm , GW_Qmm, LATQCNTmm,`P_GWkg/ha`,`YLDt/ha`,`P_STRS`,`PUPkg/ha`)) %>% 

  
  
  # change from kg/ha/yr to kg/yr
    rowwise() %>% 
    mutate(totp=totp*AREAkm2*100,
           totsolp=totsolp*AREAkm2*100,
           totn=totn*AREAkm2*100,
           QTILEmm=QTILEmm*AREAkm2*10^6/1000, # m3
           SURQ_CNTmm=SURQ_CNTmm*AREAkm2*10^6/1000,
           `SOLPkg/ha`=`SOLPkg/ha`*AREAkm2*100,
           `TVAPkg/ha`=`TVAPkg/ha`*AREAkm2*100) %>% 
    ungroup() # %>%
    
    # gather(variable,value_b,-LULC,-HRU,-GIS,-SUB,-MGT,-MON,-AREAkm2,-YR) #%>% #,-YR,-SOL_SOLP_0_5
    
    
    #### extract only annual hru outputs #####
  annual_hru<-baseline_hru %>% 
    filter(MON %in% yrs) 
  
  write.csv(annual_hru,'annual_hru_output.csv')
  
    #### extract only mar-jul hru outputs ####
  mar_jul_hru<-baseline_hru %>% 
    filter(!(YR %in% 'all years'), !(MON==YR) , MON %in% c(3:7)) %>% 
    
    # create summary of mar-jul outputs for each hru, every year
    group_by(YR,GIS) %>% 
    summarize(totp=sum(totp),
              totsolp=sum(totsolp),
              totn=sum(totn),
              QTILEmm=sum(QTILEmm), # m3
              SURQ_CNTmm=sum(SURQ_CNTmm),
              `SOLPkg/ha`=sum(`SOLPkg/ha`),
              `TVAPkg/ha`=sum(`TVAPkg/ha`))
  
  write.csv(mar_jul_hru,'marjul_hru_output.csv')