########### Read Maumee SWAT model HRU outputs ################
rm(list=ls())
library(tidyverse)
library(gridExtra)
library(here)

# best to run in command prompt
# set working directory to ReadMaumeeSWATOutputs (cd Path//To//ReadMaumeeSWATOutputs)
# To run script
# Path//to//R ReadScenarioOutputs.R
# e.g., "C:\Program Files\R\R-4.2.2\bin\x64\Rscript.exe" ReadScenarioOutputs.R

#### USER INPUTS ####

# Folder structure should be as follows
# Baseline folder needs to be called 'Baseline'
# All other folders can have any name

# scenario_dir
  # Baseline
      # output.hru
      # hru_table.txt
  # Scenario 1
      # output.hru
      # hru_table.txt # assume hru table is only txt in folder
      # output.rch
  # Scenario 2
      # output.hru
      # hru_table.txt


# The code assumes the hru table is the only txt in the folder. 
# assume HRU table in each folder containing a binary column with changed hrus (0/1) called 'changed_hru'

# Reading HRU outputs take a long time (hours) depending on the number of scenarios
# if you want to read them, change to 'yes' (must be all lowercase)
# otherwise, any other value will prevent the hru or rch outputs from being read
ReadHRUoutputs<-'yes'
ReadRCHoutputs<-'yes'

# run key that will be pasted with output name (csv files, graphs) 
# so they are not overwritten when re-running with new data
# can be basic numbers/characters
run_key<-'01262024' 


# Path to scenarios. Ensure use of // or \ in path directory
 scenario_dir<-'D:\\Maumee model\\HABRI_OLEC_Scenarios_DEC'

# years model was run for
 yrs<-c(2007:2021)

 # List of folders with scenario data
 # There needs to be a Baseline folder
 
 scenario_list<-c('Baseline',
                  '1.1. Soil Testing',
                  '1.2. Variable Rate Fertilizer',
                  '1.3. Subsurface Nutrient Application',
                  '1.4. Manure Incorporation',
                  '1.6. Cover Crops',
                  '1.7. Drainage water management',
                  '1.8. Edge-of-field buffers')
# 'Wetlands lit values')
 

# list order of scenarios on plots in reverse order
# all hru plots data plotted is relative to the baseline, i.e., don't need baseline listed
# ensure there is a matching value for each scenario in 'scenario_list', or will show up as NA on graphs
 levels_hru_plots<-c('Wetlands lit values',
                     '1.9. Wetlands',
                     '1.8. Edge-of-field buffers',
                     '1.7. Drainage water management',
                     '1.6. Cover Crops',
   '1.4. Manure Incorporation',
   '1.3. Subsurface Nutrient Application',
   '1.2. Variable Rate Fertilizer',
   '1.1. Soil Testing')

 # list order of scenarios on plots in reverse order
 # rch plots do include Baseline and should be the last value in the vector if you want it to be listed first
 levels_rch_plots<-c('Wetlands lit values',
                     '1.9. Wetlands',
                     '1.8. Edge-of-field buffers',
                     '1.7. Drainage water management',
                     '1.6. Cover Crops',
                     '1.4. Manure Incorporation',
                     '1.3. Subsurface Nutrient Application',
                     '1.2. Variable Rate Fertilizer',
                     '1.1. Soil Testing',
                     'Baseline')

 # HRU headers
 # Note this code is made to plot surface flow, tile flow, soluble p (surface and tile), total p, and total n
 # it will need at minimum the following:
 # SURQ_CNTmm, QTILEmm, SOLPkg/ha, ORGPkg/ha, SEDPkg/ha, TNO3kg/ha, ORGNkg/ha, NSURQkg/ha, TVAPkg/ha (tile p)
headers_hru<-c("LULC",  "HRU"     ,  "GIS"  ,"SUB",  "MGT" , "MON"  , "AREAkm2"      ,"ETmm",  
           "SW_ENDmm"    ,"PERCmm","SURQ_CNTmm" ,"LATQGENmm"    ,"GW_Qmm"    ,"WYLDmm"   ,
           "QTILEmm" ,"SOLPkg/ha" ,"LATQCNTmm" ,"ORGPkg/ha" ,"SEDPkg/ha", "TNO3kg/ha", 
           "ORGNkg/ha","NSURQkg/ha",  "PUPkg/ha" ,"P_GWkg/ha",    "P_STRS",   "YLDt/ha","TVAPkg/ha")

# RCH headers
# note this code is made to plot TP, DRP, and TN, and will need at minimum the following:
# MINP_OUTkg, TOT Pkg, TOT Nkg, FLOW_OUTcms
rch_headers<-c('RCH'      ,'GIS',   'MON',     'AREAkm2',  'FLOW_INcms', 'FLOW_OUTcms',     'EVAPcms',    
               'TLOSScms',  'SED_INtons', 'SED_OUTtons', 'SEDCONCmg/kg',   'ORGN_INkg',  'ORGN_OUTkg',   'ORGP_INkg',  'ORGP_OUTkg',    
               'NO3_INkg',   'NO3_OUTkg',    'NH4_INkg',   'NH4_OUTkg',    'NO2_INkg',   'NO2_OUTkg',   'MINP_INkg',  
               'MINP_OUTkg',   'CHLA_INkg',  'CHLA_OUTkg',   'CBOD_INkg',  'CBOD_OUTkg',  'DISOX_INkg',
               'DISOX_OUTkg', 'SOLPST_INmg', 'SOLPST_OUTmg', 'SORPST_INmg', 'SORPST_OUTmg',  'REACTPSTmg',    'VOLPSTmg',  'SETTLPSTmg',
               'RESUSP_PSTmg', 'DIFFUSEPSTmg', 'REACBEDPSTmg',   'BURYPSTmg',   'BED_PSTmg', 'BACTP_OUTct', 'BACTLP_OUTct',  'CMETAL#1kg',  
               'CMETAL#2kg',  'CMETAL#3kg',     'TOT Nkg',     'TOT Pkg', 'NO3ConcMg/l',    'WTMPdegc')


# ADDITIONAL TEXT TO MAKE EDITS TO HRU VARIABLES, BIAS CORRECTION AT BOTTOM OF SCRIPT


#### Function to read hru and rch outputs ####

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
if (ReadHRUoutputs == 'yes'){
###### Combine output.hru from all baseline and scenario folders ############
hru_output<-c()

### load baseline data ###
# baseline data

# hru_table<-read.csv(paste0(wd,'//',hru_table_name),sep=",") %>% 
#   select(HRU_GIS,SOL_SOLP_0_5)

### baseline hru table ###
wd<-paste0(scenario_dir,'//','Baseline')
hru_table_name<-list.files(wd)[grep('.txt',list.files(wd))] # assume hru table is only txt in folder

hru_table<-read.csv(paste0(wd,'//',hru_table_name),sep="\t") %>% 
  select(HRU_GIS,SOL_SOLP_0_5,Tile_Drain)

baseline_hru<-readSWATtxt(paste0(scenario_dir,'//','baseline'),headers_hru,'output.hru') %>% 
  mutate(across(!LULC, as.numeric)) %>% # convert everything except lulc to numeric
  
  # left_join(.,hru_table,by=c("GIS"="HRU_GIS")) %>% # join features from HRU table
  
  #add year column, assume monthly data
  mutate(YR=c(as.character(rep(yrs,each=length(unique(GIS))*12+length(unique(GIS)))),rep('all years',length(unique(GIS))))) %>%
  
  # total P and N
  mutate(totp=`SOLPkg/ha`+`ORGPkg/ha`+`SEDPkg/ha`+`TVAPkg/ha`+`P_GWkg/ha`,
         totsolp=`SOLPkg/ha`+`TVAPkg/ha`,
         totn=`TNO3kg/ha`+`ORGNkg/ha`+`NSURQkg/ha`) %>% 
  
  # remove unneeded variables
  select(c('LULC','HRU','GIS','SUB','MGT','MON','AREAkm2','YR','QTILEmm','SURQ_CNTmm','SOLPkg/ha','TVAPkg/ha','totsolp','totp','totn')) 

# create table needed for STP / P loss
stp_table<-left_join(baseline_hru, hru_table, by=c("GIS"="HRU_GIS"))
write.csv(stp_table,"STP_table.csv",row.names=F)
rm(stp_table,hru_table)
  
  
  # change from kg/ha/yr to kg/yr
baseline_hru<-baseline_hru %>% 
  rowwise() %>% 
  mutate(totp=totp*AREAkm2*100,
         totsolp=totsolp*AREAkm2*100,
         totn=totn*AREAkm2*100,
         QTILEmm=QTILEmm*AREAkm2*10^6/1000, # m3
         SURQ_CNTmm=SURQ_CNTmm*AREAkm2*10^6/1000,
         `SOLPkg/ha`=`SOLPkg/ha`*AREAkm2*100,
         `TVAPkg/ha`=`TVAPkg/ha`*AREAkm2*100) %>% 
  ungroup() %>% 
  
  gather(variable,value_b,-LULC,-HRU,-GIS,-SUB,-MGT,-MON,-AREAkm2,-YR) #%>% #,-YR,-SOL_SOLP_0_5
  # mutate(scenario=scen)



# compare scenario data to the baseline

hru_annual<-tibble()
hru_marjul<-tibble()


for (scen in scenario_list[!(scenario_list %in% 'Baseline')]){
  
  wd<-paste0(scenario_dir,'//',scen)
  
  # if it is a scenario, read hru table to filter only changed HRUs
    
  hru_table_name<-list.files(wd)[grep('.txt',list.files(wd))] # assume hru table is only txt in folder
    
  hru_table<-read.csv(paste0(wd,'//',hru_table_name),sep="\t") %>% 
    select(HRU_GIS,colnames(.)[grep('scen_change',colnames(.),ignore.case=T)]) %>% 
    rename('Scen_Change_HRU'=2)
  
  add_df<-readSWATtxt(wd,headers_hru,'output.hru') %>% 
    
    # Filter unchanged HRUs
    mutate(across(!LULC, as.numeric)) %>% # convert everything except lulc to numeric
    left_join(.,hru_table,by=c("GIS"="HRU_GIS")) %>% 
    
    #add year column, assume monthly data
    mutate(YR=c(as.character(rep(yrs,each=length(unique(GIS))*12+length(unique(GIS)))),rep('all years',length(unique(GIS))))) %>%
    
    
    # filter(Scen_Change_HRU==1) %>% 
    # select(-Scen_Change_HRU) %>% 
    
    # total P and N
    mutate(totp=`SOLPkg/ha`+`ORGPkg/ha`+`SEDPkg/ha`+`TVAPkg/ha`+`P_GWkg/ha`,
           totsolp=`SOLPkg/ha`+`TVAPkg/ha`,
           totn=`TNO3kg/ha`+`ORGNkg/ha`+`NSURQkg/ha`) %>% # extra slide 
    
    # remove unneeded variables
    select(c('HRU','GIS','SUB','MGT','MON','AREAkm2','YR',
             'QTILEmm','SURQ_CNTmm','SOLPkg/ha','TVAPkg/ha','totsolp','totp','totn')) %>% # remove LULC as it returns NA if baseline and scenario joined but managment changed
    
    # change from kg/ha/yr to kg/yr
    rowwise() %>% 
    mutate(totp=totp*AREAkm2*100,
           totsolp=totsolp*AREAkm2*100,
           totn=totn*AREAkm2*100,
           QTILEmm=QTILEmm*AREAkm2*10^6/1000, # m3
           SURQ_CNTmm=SURQ_CNTmm*AREAkm2*10^6/1000,
           `SOLPkg/ha`=`SOLPkg/ha`*AREAkm2*100,
           `TVAPkg/ha`=`TVAPkg/ha`*AREAkm2*100) %>% 
    ungroup() %>% 
    
    gather(variable,value,-HRU,-GIS,-SUB,-MGT,-MON,-AREAkm2,-Scen_Change_HRU,-YR)  #,-YR,-SOL_SOLP_0_5
    # mutate(scenario=scen)
  
  
  hru_output<-left_join(baseline_hru,add_df,by=c("variable","HRU","GIS","SUB","MGT","MON","YR","AREAkm2"))
  
  rm(add_df,hru_table) # clear memory 
  
  
  #### process total difference in loss from only changed HRUs and all HRUs #####
  
  # annual HRU - changed HRUs only
  # sum annual amount for all HRUs, take avg of all years, calculate % difference
  add_df<-hru_output %>% 
    filter(MON %in% yrs , Scen_Change_HRU==1) %>% 
    group_by(variable,YR) %>% # remove grouping by GIS
    summarize(value=sum(value),value_b=sum(value_b)) %>% # combine outputs from all HRUs
    ungroup() %>% 
    group_by(variable) %>% 
    summarize(value=mean(value),value_b=mean(value_b)) %>% # average annual sum of outputs from all HRUs
    mutate(percent_change=(value-value_b)*100/value_b) %>% 
    mutate(scenario=scen,HRU='changed HRUs only')
  
  hru_annual<-rbind(hru_annual,add_df)
  rm(add_df)
  
  # annual HRU - all HRUs
  add_df<-hru_output %>% 
    filter(MON %in% yrs) %>% 
    group_by(variable,YR) %>% # remove grouping by GIS
    summarize(value=sum(value),value_b=sum(value_b)) %>% # combine outputs from all HRUs
    ungroup() %>% 
    group_by(variable) %>% 
    summarize(value=mean(value),value_b=mean(value_b)) %>% # average annual sum of outputs from all HRUs
    mutate(percent_change=(value-value_b)*100/value_b) %>% 
    mutate(scenario=scen,HRU='all HRUs')
  
  hru_annual<-rbind(hru_annual,add_df)
  rm(add_df)
  
  # March - July, changed HRUs only
  add_df<-hru_output %>% 
    filter(!(YR %in% 'all years'), !(MON==YR) , Scen_Change_HRU==1, MON %in% c(3:7)) %>% 
    group_by(variable,YR) %>% # remove grouping by GIS
    summarize(value=sum(value),value_b=sum(value_b)) %>% # combine outputs from all HRUs for Mar-July
    ungroup() %>% 
    group_by(variable) %>% 
    summarize(value=mean(value),value_b=mean(value_b)) %>% # average March-July sum of outputs from all HRUs
    mutate(percent_change=(value-value_b)*100/value_b) %>% 
    mutate(scenario=scen,HRU='changed HRUs only')
  
  hru_marjul<-rbind(hru_marjul,add_df)
  rm(add_df)
  
  # March - July, all HRUs
  add_df<-hru_output %>% 
    filter(!(YR %in% 'all years'), !(MON==YR) , MON %in% c(3:7)) %>% 
    group_by(variable,YR) %>% # remove grouping by GIS
    summarize(value=sum(value),value_b=sum(value_b)) %>% # combine outputs from all HRUs for Mar-July
    ungroup() %>% 
    group_by(variable) %>% 
    summarize(value=mean(value),value_b=mean(value_b)) %>% # average March-July sum of outputs from all HRUs
    mutate(percent_change=(value-value_b)*100/value_b) %>% 
    mutate(scenario=scen,HRU='all HRUs')
  
  hru_marjul<-rbind(hru_marjul,add_df)
  rm(add_df)
    
    

  
  print(paste(scen, 'loaded'))
  rm(hru_output)
}

rm(baseline_hru)

###### HRU plots #####

variable_labs<-c('Tile discharge','Surface runoff','Surface DRP','Tile DRP','Total DRP','TP','TN')
names(variable_labs)<-c('QTILEmm','SURQ_CNTmm','SOLPkg/ha','TVAPkg/ha','totsolp','totp','totn')

##### percent change #########

# Annual percent change
hru_annual %>% 
  filter(variable %in% c('QTILEmm','SURQ_CNTmm','SOLPkg/ha','TVAPkg/ha','totsolp','totp','totn')) %>%
  mutate(variable=factor(variable,ordered=T, levels=c('QTILEmm','SURQ_CNTmm','SOLPkg/ha','TVAPkg/ha','totsolp','totp','totn'))) %>% 
  mutate(scenario=factor(scenario,ordered=T,levels=levels_hru_plots)) %>% 
  ggplot(.,aes(x=scenario,y=percent_change,fill=HRU))+geom_bar(stat='identity',position='dodge',width=0.7,color='black')+facet_wrap(~variable,labeller=labeller(variable=variable_labs),scales='free_x')+
  xlab("")+ylab("Change from baseline (%)")+
  scale_fill_manual(values=c('all HRUs'='grey','changed HRUs only'='white'))+
    coord_flip()+
  # scale_fill_manual(values=c('baseline (2013-2020)'='white', 'land management scenario (2013-2020)'='grey'))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(),text = element_text(size = 16),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        legend.title = element_blank())

setwd(scenario_dir)
ggsave(paste0('hru_per_change_annual_',run_key,'.png'),last_plot(),height=150,width=300,units='mm')

# Mar-July percent change
hru_marjul %>% 
  filter(variable %in% c('QTILEmm','SURQ_CNTmm','SOLPkg/ha','TVAPkg/ha','totsolp','totp','totn')) %>%
  mutate(variable=factor(variable,ordered=T, levels=c('QTILEmm','SURQ_CNTmm','SOLPkg/ha','TVAPkg/ha','totsolp','totp','totn'))) %>% 
  mutate(scenario=factor(scenario,ordered=T,levels=levels_hru_plots)) %>% 
  ggplot(.,aes(x=scenario,y=percent_change,fill=HRU))+geom_bar(stat='identity',position='dodge',width=0.7,color='black')+facet_wrap(~variable,labeller=labeller(variable=variable_labs),scales='free_x')+
  xlab("")+ylab("Change from baseline (%)")+
  scale_fill_manual(values=c('all HRUs'='grey','changed HRUs only'='white'))+
  coord_flip()+
  # scale_fill_manual(values=c('baseline (2013-2020)'='white', 'land management scenario (2013-2020)'='grey'))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(),text = element_text(size = 16),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        legend.title = element_blank())

setwd(scenario_dir)
ggsave(paste0('hru_per_change_marjul_',run_key,'.png'),last_plot(),height=150,width=300,units='mm')

##### Absolute values ############

variable_labs<-c('Tile discharge (m3)','Surface runoff (m3)','Surface DRP (kg)','Tile DRP (kg)','Total DRP (kg)','TP (kg)','TN (kg)')
names(variable_labs)<-c('QTILEmm','SURQ_CNTmm','SOLPkg/ha','TVAPkg/ha','totsolp','totp','totn')

# have to compare like this bc when looking at only the changed HRUs the baseline will be different for each scenario
hru_annual %>% 
  select(-percent_change) %>% 
  gather(b_s,value,-scenario,-HRU,-variable) %>% #baseline 
  filter(variable %in% c('QTILEmm','SURQ_CNTmm','SOLPkg/ha','TVAPkg/ha','totsolp','totp','totn')) %>%
  mutate(variable=factor(variable,ordered=T, levels=c('QTILEmm','SURQ_CNTmm','SOLPkg/ha','TVAPkg/ha','totsolp','totp','totn'))) %>% 
  mutate(scenario=factor(scenario,ordered=T,levels=levels_hru_plots)) %>% 
  ggplot(.,aes(x=scenario,y=value,fill=b_s))+geom_bar(stat='identity',position='dodge',width=0.7,color='black')+facet_grid(HRU~variable,labeller=labeller(variable=variable_labs),scales='free')+
  xlab("")+ylab("Average annual total loss from changed HRUs")+
  scale_fill_manual(values=c('value_b'='grey','value'='white'),labels=c('value_b'='Baseline','value'='Scenario'))+
  coord_flip()+
  # scale_fill_manual(values=c('baseline (2013-2020)'='white', 'land management scenario (2013-2020)'='grey'))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(),text = element_text(size = 16),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

setwd(scenario_dir)
ggsave(paste0('hru_annual_abs_',run_key,'.png'),last_plot(),height=150,width=500,units='mm')

hru_marjul %>% 
  select(-percent_change) %>% 
  gather(b_s,value,-scenario,-HRU,-variable) %>% #baseline 
  filter(variable %in% c('QTILEmm','SURQ_CNTmm','SOLPkg/ha','TVAPkg/ha','totsolp','totp','totn')) %>%
  mutate(variable=factor(variable,ordered=T, levels=c('QTILEmm','SURQ_CNTmm','SOLPkg/ha','TVAPkg/ha','totsolp','totp','totn'))) %>% 
  mutate(scenario=factor(scenario,ordered=T,levels=levels_hru_plots)) %>% 
  ggplot(.,aes(x=scenario,y=value,fill=b_s))+geom_bar(stat='identity',position='dodge',width=0.7,color='black')+facet_grid(HRU~variable,labeller=labeller(variable=variable_labs),scales='free')+
  xlab("")+ylab("Average March-July total loss from changed HRUs")+
  scale_fill_manual(values=c('value_b'='grey','value'='white'),labels=c('value_b'='Baseline','value'='Scenario'))+
  coord_flip()+
  # scale_fill_manual(values=c('baseline (2013-2020)'='white', 'land management scenario (2013-2020)'='grey'))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(),text = element_text(size = 16),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

setwd(scenario_dir)
ggsave(paste0('hru_marjul_abs_',run_key,'.png'),last_plot(),height=150,width=500,units='mm')

write.csv(hru_annual,paste0('hru_annual_',run_key,'.csv'),row.names=F)
write.csv(hru_marjul,paste0('hru_marjul_',run_key,'.csv'),row.names=F)
  
}


##################### RCH OUTPUTS ##########################################################
if (ReadRCHoutputs=='yes'){

rch_output_monthly<-c()
rch_output_annual<-c()

for (scen in scenario_list){
  
  wd<-paste0(scenario_dir,'//',scen)
  
  add_monthly<-readSWATtxt(wd,rch_headers,'output.rch') %>% 
    mutate(RCH=str_remove(RCH,'REACH')) %>% 
    mutate_if(is.character, as.numeric) %>% 
    filter(RCH==59) %>% 
    filter(!row_number() %in% c((length(unique(RCH))*12*length(yrs)+length(unique(RCH))*length(yrs))+1:nrow(.))) %>% # cut summary at the end of output.rch
    select('RCH','MON', 'FLOW_OUTcms',    'SED_OUTtons', 'SEDCONCmg/kg', 'ORGN_OUTkg','ORGP_OUTkg', 'NO3_OUTkg','NH4_OUTkg', 'NO2_OUTkg',  
           'MINP_OUTkg',    'TOT Nkg',     'TOT Pkg', 'NO3ConcMg/l') %>% 
    mutate(scenario=scen) 
  
  # move annual data to another data frame
  add_annual<-add_monthly %>% 
    filter(MON %in% yrs) %>% 
    rename('YR'='MON') %>% 
    gather(variable,value,-YR,-scenario,-RCH)
  
  # monthly data
  add_monthly <-add_monthly %>% 
    filter(!(MON %in% yrs)) %>% 
    mutate(YR=rep(yrs,each=12*length(unique(RCH)))) %>% 
    gather(variable,value,-YR,-MON,-scenario,-RCH)
    
    
   rch_output_annual<-rbind(   rch_output_annual,add_annual) 
   
   rch_output_monthly<-rbind(   rch_output_monthly,add_monthly) 
   
  
  
  
}


### process monthly to be march-July #####
# might need to change discharge to be average instead of a sum


mar_jul_MAW<-rch_output_monthly %>% 
   filter(MON %in% c(3:7)) %>% 
  # mutate(scenario=factor(scenario),
  #        variable=factor(variable)) %>% 
  group_by(YR,variable,scenario) %>% 
  summarize(value=sum(value)) %>% 
  ungroup() %>% 
  group_by(variable,scenario) %>% 
  summarize(value=mean(value)) %>% 
  
  mutate(scenario=factor(scenario,ordered=T,levels=levels_rch_plots)) %>% 
  mutate(percent_change=(value-value[scenario=="Baseline"])*100/value[scenario=="Baseline"])



annual_MAW_allYrs<-rch_output_monthly %>% 
  # mutate(scenario=factor(scenario),
  #        variable=factor(variable)) %>% 
  group_by(YR,variable,scenario) %>% 
  summarize(value=sum(value)) 

setwd(scenario_dir)
annual_MAW_allYrs %>% 
  pivot_wider(names_from=c(scenario),values_from=c(value)) %>% 
  write.csv(.,'annual_RCH_allYrs.csv',row.names=F) # paste key

# summarize all 20 years
annual_MAW<-annual_MAW_allYrs %>% 
  ungroup() %>% 
  group_by(variable,scenario) %>% 
  summarize(value=mean(value)) %>% 
  
  mutate(scenario=factor(scenario,ordered=T,levels=levels_rch_plots)) %>% 
  mutate(percent_change=(value-value[scenario=="Baseline"])*100/value[scenario=="Baseline"])


###################### bias correct outputs #############################

setwd(here('Waterville'))

obs_lookup<-read.csv('lookup.csv') %>% 
  mutate(annual_bias_c=NA) %>% 
  mutate(marjul_bias_c=NA)

mar_jul_MAW$value_corrected<-NA
annual_MAW$value_corrected<-NA

for (obs_var in obs_lookup$obs){
  
  sim_var<-obs_lookup %>% 
    filter(obs==obs_var) %>% 
    select(SWAT_out) %>% 
    pull
  
  # observed monthly sum (or mean for discharge)
  if (obs_var != 'discharge'){
  obs<-read.csv(paste0(obs_var,'_BOA.csv')) %>% 
    group_by(month,year) %>% 
    rename('obs'=4) %>% 
    summarize(obs=sum(obs))
  
  }else{
    
    obs<-read.csv(paste0(obs_var,'_BOA.csv')) %>% 
      group_by(month,year) %>% 
      rename('obs'=4) %>% 
      summarize(obs=mean(obs))
      
      
  }
  
  
  
  sim<-rch_output_monthly %>% 
    filter(scenario=='Baseline',variable==sim_var) %>% 
    rename('sim'='value')
  
  bias_c<-left_join(obs,sim,by=c('month'='MON','year'='YR'))
  
  # load 
  if (obs_var != 'discharge'){
    
  annual_bias<-bias_c %>% 
    filter(!is.na(sim),!is.na(obs)) %>% 
    group_by(year) %>% 
    summarise(obs=sum(obs),sim=sum(sim)) %>% # total annual load in one year
    ungroup() %>% 
    summarise(obs=mean(obs),sim=mean(sim)) # average annual load across all years
  
  annual_bias<-annual_bias$obs/annual_bias$sim
  
  
  marjul_bias<-bias_c %>% 
    filter(!is.na(sim),!is.na(obs),month %in% c(3:7)) %>% # filter only months 3-7
    group_by(year) %>% 
    summarise(obs=sum(obs),sim=sum(sim)) %>% # total annual load in one year
    ungroup() %>% 
    summarise(obs=mean(obs),sim=mean(sim)) # average annual mar-jul load across all years
  
  marjul_bias<-marjul_bias$obs/marjul_bias$sim
  
   }else{

     # discharge is average annual not sum of the total
     annual_bias<-bias_c %>%
       filter(!is.na(sim),!is.na(obs)) %>%
       group_by(year) %>%
       summarise(obs=mean(obs),sim=mean(sim)) %>% # total annual load in one year
       ungroup() %>%
       summarise(obs=mean(obs),sim=mean(sim)) # average annual load across all years
  
     annual_bias<-annual_bias$obs/annual_bias$sim
  
  
     marjul_bias<-bias_c %>%
       filter(!is.na(sim),!is.na(obs),month %in% c(3:7)) %>% # filter only months 3-7
       group_by(year) %>%
       summarise(obs=mean(obs),sim=mean(sim)) %>% # total annual load in one year
       ungroup() %>%
       summarise(obs=mean(obs),sim=mean(sim)) # average annual mar-jul load across all years
  
     marjul_bias<-marjul_bias$obs/marjul_bias$sim
  
   }
  
  # save bias factors to lookup table
  obs_lookup$annual_bias_c[obs_lookup$obs==obs_var]<-annual_bias
  obs_lookup$marjul_bias_c[obs_lookup$obs==obs_var]<-marjul_bias
  
  # apply bias correction factor to all rch data
  mar_jul_MAW$value_corrected[mar_jul_MAW$variable==sim_var]<-  mar_jul_MAW$value[mar_jul_MAW$variable==sim_var]*marjul_bias
  annual_MAW$value_corrected[annual_MAW$variable==sim_var]<-  annual_MAW$value[annual_MAW$variable==sim_var]*annual_bias
  
}


### March-July plots ###

max_y<-mar_jul_MAW %>% 
  filter(variable=='MINP_OUTkg') %>% 
  filter(value_corrected==max(value_corrected)) %>% 
  select(value_corrected) %>% 
  pull
  

mar_jul_DRP<-mar_jul_MAW %>% 
  filter(variable=='MINP_OUTkg') %>% 
  mutate(value_corrected=value_corrected/1000) %>% # convert to tons
  ggplot(.,aes(x=scenario,y=value_corrected)) + geom_bar(stat='identity',colour='black',fill='olivedrab4')+xlab("")+ylab('Metric tons DRP')+ggtitle('Average March-July DRP load')+
  scale_y_continuous(expand=c(0,0),limits=c(0,max_y*1.15/1000))+
  geom_hline(yintercept = 186,linetype='dashed')+
  geom_text(aes(label=paste0(round(percent_change,0),'%')),nudge_y=21,size=8)+
  coord_flip()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(),text = element_text(size = 24),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        legend.title = element_blank(),plot.margin = margin(t = 5,  # Top margin
                                                            r = 5,  # Right margin
                                                            b = 5,  # Bottom margin
                                                            l = 5, unit='mm'))
mar_jul_DRP

max_y<-mar_jul_MAW %>% 
  filter(variable=='TOT Pkg') %>% 
  filter(value_corrected==max(value_corrected)) %>% 
  select(value_corrected) %>% 
  pull

mar_jul_TP<-mar_jul_MAW %>% 
  filter(variable=='TOT Pkg') %>% 
  mutate(value_corrected=value_corrected/1000) %>% # convert to tons
  ggplot(.,aes(x=scenario,y=value_corrected)) + geom_bar(stat='identity',colour='black',fill='olivedrab4')+xlab("")+ylab('Metric tons TP')+ggtitle('Average March-July TP load')+
  scale_y_continuous(expand=c(0,0),limits=c(0,max_y*1.15/1000))+
  geom_hline(yintercept = 860,linetype='dashed')+
  geom_text(aes(label=paste0(round(percent_change,0),'%')),nudge_y=100,size=8)+
  coord_flip()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(),text = element_text(size = 24),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        legend.title = element_blank(),
        axis.text.y=element_blank(),  plot.margin = margin(t = 5,  # Top margin
                                                                 r = 10,  # Right margin
                                                                 b = 5,  # Bottom margin
                                                                 l = 5, unit='mm'))  # Left margin)


mar_july_plot<-grid.arrange(mar_jul_DRP,mar_jul_TP,nrow=1,widths=c(1.5,1))

setwd(scenario_dir)
ggsave(paste0('March_July_RCH',run_key,'.png'),mar_july_plot,height=150,width=510,units='mm')

####### TN ########
max_y<-mar_jul_MAW %>% 
  filter(variable=='TOT Nkg') %>% 
  filter(value_corrected==max(value_corrected)) %>% 
  select(value_corrected) %>% 
  pull

mar_jul_TN<-mar_jul_MAW %>% 
  filter(variable=='TOT Nkg') %>% 
  mutate(value_corrected=value_corrected/1000) %>% # convert to tons
  ggplot(.,aes(x=scenario,y=value_corrected)) + geom_bar(stat='identity',colour='black',fill='olivedrab4')+xlab("")+ylab('Metric tons TN')+ggtitle('Average March-July TN load')+
  scale_y_continuous(expand=c(0,0),limits=c(0,max_y*1.15/1000))+
  # geom_hline(yintercept = 860,linetype='dashed')+
  geom_text(aes(label=paste0(round(percent_change,0),'%')),nudge_y=2500,size=8)+
  coord_flip()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(),text = element_text(size = 24),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        legend.title = element_blank(),
        plot.margin = margin(t = 5,  # Top margin
                                                           r = 10,  # Right margin
                                                           b = 5,  # Bottom margin
                                                           l = 5, unit='mm'))  # Left margin)

ggsave(paste0('March_July_RCH_TN',run_key,'.png'),last_plot(),height=150,width=300,units='mm')


#### discharge ####
max_y<-mar_jul_MAW %>% 
  filter(variable=='FLOW_OUTcms') %>% 
  filter(value_corrected==max(value_corrected)) %>% 
  select(value_corrected) %>% 
  pull

mar_jul_discharge<-mar_jul_MAW %>% 
  filter(variable=='FLOW_OUTcms') %>% 
  ggplot(.,aes(x=scenario,y=value_corrected)) + geom_bar(stat='identity',colour='black',fill='olivedrab4')+xlab("")+ylab('Discharge (sum cms)')+ggtitle('Average March-July discharge')+
  scale_y_continuous(expand=c(0,0),limits=c(0,max_y*1.15))+
  # geom_hline(yintercept = 860,linetype='dashed')+
  geom_text(aes(label=paste0(round(percent_change,0),'%')),nudge_y=120,size=10)+
  coord_flip()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(),text = element_text(size = 24),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        legend.title = element_blank(),
        plot.margin = margin(t = 5,  # Top margin
                                                           r = 10,  # Right margin
                                                           b = 5,  # Bottom margin
                                                           l = 5, unit='mm'))  # Left margin)

ggsave(paste0('March_July_RCH_discharge_',run_key,'.png'),last_plot(),height=150,width=300,units='mm')
# mar_jul_TP


### save csv excel format #####

mar_jul_MAW<-mar_jul_MAW %>% 
  pivot_wider(names_from=c(scenario),values_from=c(value,value_corrected,percent_change))


write.csv(mar_jul_MAW,paste0('MarJulRCH_',run_key,'.csv'),row.names=F)



### Annual plots ###

max_y<-annual_MAW %>% 
  filter(variable=='MINP_OUTkg') %>% 
  filter(value_corrected==max(value_corrected)) %>% 
  select(value_corrected) %>% 
  pull


annual_DRP<-annual_MAW %>% 
  filter(variable=='MINP_OUTkg') %>% 
  mutate(value_corrected=value_corrected/1000) %>% # convert to tons
  ggplot(.,aes(x=scenario,y=value_corrected)) + geom_bar(stat='identity',colour='black',fill='olivedrab4')+xlab("")+ylab('Metric tons DRP')+ggtitle('Average annual DRP load')+
  scale_y_continuous(expand=c(0,0),limits=c(0,max_y*1.15/1000))+
  geom_hline(yintercept = 401,linetype='dashed')+
  geom_text(aes(label=paste0(round(percent_change,0),'%')),nudge_y=50,size=10)+
  coord_flip()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(),text = element_text(size = 24),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        legend.title = element_blank(),plot.margin = margin(t = 5,  # Top margin
                                                            r = 5,  # Right margin
                                                            b = 5,  # Bottom margin
                                                            l = 5, unit='mm'))
# annual_DRP

max_y<-annual_MAW %>% 
  filter(variable=='TOT Pkg') %>% 
  filter(value_corrected==max(value_corrected)) %>% 
  select(value_corrected) %>% 
  pull

annual_TP<-annual_MAW %>% 
  filter(variable=='TOT Pkg') %>% 
  mutate(value_corrected=value_corrected/1000) %>% # convert to tons
  ggplot(.,aes(x=scenario,y=value_corrected)) + geom_bar(stat='identity',colour='black',fill='olivedrab4')+xlab("")+ylab('Metric tons TP')+ggtitle('Average annual TP load')+
  scale_y_continuous(expand=c(0,0),limits=c(0,max_y*1.15/1000))+
  geom_hline(yintercept = 1897,linetype='dashed')+
  geom_text(aes(label=paste0(round(percent_change,0),'%')),nudge_y=190,size=10)+
  coord_flip()+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(),text = element_text(size = 24),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        legend.title = element_blank(),
        axis.text.y=element_blank(),  plot.margin = margin(t = 5,  # Top margin
                                                           r = 10,  # Right margin
                                                           b = 5,  # Bottom margin
                                                           l = 5, unit='mm'))  # Left margin)


annual_plot<-grid.arrange(annual_DRP,annual_TP,nrow=1,widths=c(1.5,1))

setwd(scenario_dir)
ggsave(paste0('annual_RCH', run_key,'.png'),annual_plot,height=150,width=510,units='mm')

# excel format
annual_MAW<-annual_MAW %>% 
  pivot_wider(names_from=c(scenario),values_from=c(value,value_corrected,percent_change))

write.csv(annual_MAW,paste0('annualRCH_',run_key,'.csv'),row.names=F)


write.csv(obs_lookup,'bias_c_factors.csv')
}

#### Additional notes to change anything in the script #####

# TO CHANGE HRU VARIABLES
# add variable to selection of columns in line 198 and 260
# add variable to naming convention in lines 352, 353
# add variable to filter function and factor list for each HRU graphs, eg 359-360

# CHANGE FUNCTION THAT FINDS HRU TABLE / ASSUMES IS THE ONLY TXT FILE
# 179 and 235

# BIAS CORRECTION FOR RCH OUTPUTS (lines 527-624)
# Bias correction factors are calculated for each variable as:
# bias_c = mean(obs average annual)/mean(sim baseline average annual)

# This is applied as follows:
# baseline = baseline average annual * bias_c = obs average annual
# scenario_1 (bias corrected) = scenario_1 average annual * bias_c 

# This factor is calculated for each variable -- discharge, tp, solp, tn
# headers should contain at least 'year, month, day, variable values'
# 'year' and 'month' must be labeled exactly (all lowercase), and the value column must be the 4th column (doesn't need specific label)
# This data could be daily or monthly timeseries. If it is daily it will be summarized to monthly values.
# Additional variables can be added to this data by adding them to the 'lookup.csv' file
# The SWAT_OUT variable should match the header in the reach file and the obs should be the name used to label the file
# The file name should be 'variablename_BOA.csv'

