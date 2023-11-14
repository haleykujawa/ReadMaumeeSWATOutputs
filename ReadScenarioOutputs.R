########### Read Maumee SWAT model HRU outputs ################
rm(list=ls())
library(tidyverse)

# assume HRU table in each folder containing a binary column with changed hrus called 'changed_hru'

# Place folder that contains all scenario folders below:
scenario_dir<-'D:\\Maumee model\\MaumeeScenarios' 
yrs<-c(2008:2010)

# Place list of scenarios here
scenario_list<-c('baseline','scenario 1','scenario 2')

headers_hru<-c("LULC",  "HRU"     ,  "GIS"  ,"SUB",  "MGT" , "MON"  , "AREAkm2"      ,"ETmm",  
           "SW_ENDmm"    ,"PERCmm","SURQ_CNTmm" ,"LATQGENmm"    ,"GW_Qmm"    ,"WYLDmm"   ,
           "QTILEmm" ,"SOLPkg/ha" ,"LATQCNTmm" ,"ORGPkg/ha" ,"SEDPkg/ha", "TNO3kg/ha", 
           "ORGNkg/ha","NSURQkg/ha",  "PUPkg/ha" ,"P_GWkg/ha",    "P_STRS",   "YLDt/ha","TILEPkg/ha")

# Folder structure should be

# Scenario dir
  # Scenario 1
      # output.hru
      # hru_table.txt
  # Scenario 2
      # output.hru
      # hru_table.txt
  # baseline
      # output.hru
      # hru_table.txt


# function to read hru outputs



readHRU<-function(wd,headers){
  
  setwd(wd)

# HRU output structure
# line 9 = header column

tmp<-file('output.hru')

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



hru_output<-c()

for (scen in scenario_list){
  
  wd<-paste0(scenario_dir,'//',scen)
  
  # if it is a scenario, read hru table to filter only changed HRUs
  
  if (!grepl('baseline',scen)){
    
    
  hru_table<-read.csv(paste0(wd,'//HRU_Table_OCT2023.txt')) %>% 
    mutate(changed_hru=1) %>% 
    select(HRU_GIS,changed_hru)
  
  add_df<-readHRU(wd,headers_hru) %>% 
    
    # Filter unchanged HRUs
    mutate(across(!LULC, as.numeric)) %>% # convert everything except lulc to numeric
    left_join(.,hru_table,by=c("GIS"="HRU_GIS")) %>% 
    filter(changed_hru==1) %>% 
    select(-changed_hru) %>% 
    
    #add year column, assume monthly data
    mutate(YR=rep(yrs,each=length(unique(GIS))*12)) %>% 
    
    # total P and N
    mutate(totp=`SOLPkg/ha`+`ORGPkg/ha`+`SEDPkg/ha`+`TILEPkg/ha`+`P_GWkg/ha`,
           totn=`TNO3kg/ha`+`ORGNkg/ha`+`NSURQkg/ha`) %>% 
    
    # remove unneeded variables
    select(-c(SW_ENDmm, LATQGENmm , GW_Qmm, LATQCNTmm,`P_GWkg/ha`,`YLDt/ha`,`P_STRS`,`PUPkg/ha`)) %>% 
    
    gather(variable,value,-LULC,-HRU,-GIS,-SUB,-MGT,-MON,-AREAkm2,-YR) %>% 
    mutate(scenario=scen) %>% 
    mutate(value=value*2) #TESTING 
  
  
  hru_output<-rbind(hru_output,add_df)
  
  rm(add_df,hru_table) # clear memory 
  
  }else{
    
    # baseline data
    
    add_df<-readHRU(wd,headers_hru) %>% 
      mutate(across(!LULC, as.numeric)) %>% # convert everything except lulc to numeric
      
      #add year column, assume monthly data
      mutate(YR=rep(yrs,each=length(unique(GIS))*12)) %>% 
      
      # total P and N
      mutate(totp=`SOLPkg/ha`+`ORGPkg/ha`+`SEDPkg/ha`+`TILEPkg/ha`+`P_GWkg/ha`,
             totn=`TNO3kg/ha`+`ORGNkg/ha`+`NSURQkg/ha`) %>% 
      
      # remove unneeded variables
      select(-c(SW_ENDmm, LATQGENmm , GW_Qmm, LATQCNTmm,`P_GWkg/ha`,`YLDt/ha`,`P_STRS`,`PUPkg/ha`)) %>% 
      
      gather(variable,value,-LULC,-HRU,-GIS,-SUB,-MGT,-MON,-AREAkm2,-YR) %>% 
      mutate(scenario=scen)
      
      
    
    hru_output<-rbind(hru_output,add_df)
    
    rm(add_df)
    
    
  }
  
  print(paste(scen, 'loaded'))
}


# calculate percent change for each scenario as compared to baseline

hru_output_yearly <- hru_output %>% 
  mutate(scenario=factor(scenario)) %>% 
  group_by(GIS,YR,variable,scenario) %>%
  summarize(value=sum(value,na.rm=T)) %>% # yearly average of monthly values
  mutate(percent_change=(value-value[scenario=="baseline"])*100/value[scenario=="baseline"]) 


#### HRU plots #########

variable_labs<-c('Sediment yield','Surface soluble P','Total P')
names(variable_labs)<-c('sedyld_change','surqsolp_change','totp_change')
#,labeller=labeller(variable=variable_labs)

#absolute values
hru_output_yearly %>% 
  ggplot(.,aes(x=scenario,y=value))+geom_boxplot()+facet_wrap(~variable,scales='free_y')+
  xlab("")+ylab("")+
  # scale_fill_manual(values=c('baseline (2013-2020)'='white', 'land management scenario (2013-2020)'='grey'))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(),text = element_text(size = 16),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        legend.title = element_blank())

#percent change
hru_output_yearly %>% 
  filter(!(scenario %in% 'baseline')) %>% 
  ggplot(.,aes(x=scenario,y=percent_change))+geom_boxplot()+facet_wrap(~variable,scales='free_y')+
  xlab("")+ylab("")+
  # scale_fill_manual(values=c('baseline (2013-2020)'='white', 'land management scenario (2013-2020)'='grey'))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(),text = element_text(size = 16),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        legend.title = element_blank())
  

  

