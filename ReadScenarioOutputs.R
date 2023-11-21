########### Read Maumee SWAT model HRU outputs ################
rm(list=ls())
library(tidyverse)
library(gridExtra)
library(here)

# Add total DRP (tile + surface)

# assume HRU table in each folder containing a binary column with changed hrus called 'changed_hru'

ReadHRUoutputs<-'no'
ReadRCHoutputs<-'yes'

#### HABRI OLEC inputs ####
#testing
# scenario_dir<-'D:\\Maumee model\\HABRI_OLEC_Scenarios'
# yrs<-c(2007:2021)
# 
# scenario_list<-c('Baseline',
#                  '1.6. Cover Crops')
# 
# 
# 
# levels_hru_plots<-c('1.6. Cover Crops')
# 
# 
# levels_rch_plots<-c('1.6. Cover Crops',
#                     'Baseline')


#### HABRI OLEC inputs ####
 scenario_dir<-'D:\\Maumee model\\HABRI_OLEC_Scenarios'
 yrs<-c(2007:2021)

 scenario_list<-c('Baseline',
                  # '1.1. Soil Testing',
                  '1.2. Variable Rate Fertilizer',
                  '1.3. Subsurface Nutrient Application',
                  '1.4. Manure Incorporation',
                  '1.6. Cover Crops')
 
 # scenario_list<-c('Baseline',
 #                  '1.3. Subsurface Nutrient Application')



 levels_hru_plots<-c('1.6. Cover Crops',
   '1.4. Manure Incorporation',
   '1.3. Subsurface Nutrient Application',
   '1.2. Variable Rate Fertilizer',
   '1.1. Soil Testing')


 levels_rch_plots<-c('1.6. Cover Crops',
                     '1.4. Manure Incorporation',
                     '1.3. Subsurface Nutrient Application',
                     '1.2. Variable Rate Fertilizer',
                     '1.1. Soil Testing',
                     'Baseline')

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
if (ReadHRUoutputs == 'yes'){
###### Combine output.hru from all baseline and scenario folders ############
hru_output<-c()

for (scen in scenario_list){
  
  wd<-paste0(scenario_dir,'//',scen)
  
  # if it is a scenario, read hru table to filter only changed HRUs
  
  if (!grepl('Baseline',scen)){
    
  hru_table_name<-list.files(wd)[grep('.txt',list.files(wd))] # assume hru table is only txt in folder
    
  hru_table<-read.csv(paste0(wd,'//',hru_table_name),sep="\t") %>% 
    select(HRU_GIS,colnames(.)[grep('scen_change_HRU',colnames(.),ignore.case=T)]) %>% 
    rename('Scen_Change_HRU'=2)
  
  add_df<-readSWATtxt(wd,headers_hru,'output.hru') %>% 
    
    # Filter unchanged HRUs
    mutate(across(!LULC, as.numeric)) %>% # convert everything except lulc to numeric
    left_join(.,hru_table,by=c("GIS"="HRU_GIS")) %>% 
    
    
    filter(Scen_Change_HRU==1) %>% 
    select(-Scen_Change_HRU) %>% 
    
    #add year column, assume monthly data
    mutate(YR=rep(yrs,each=length(unique(GIS))*12)) %>% 
    
    # total P and N
    mutate(totp=`SOLPkg/ha`+`ORGPkg/ha`+`SEDPkg/ha`+`TILEPkg/ha`+`P_GWkg/ha`,
           totsolp=`SOLPkg/ha`+`TILEPkg/ha`,
           totn=`TNO3kg/ha`+`ORGNkg/ha`+`NSURQkg/ha`) %>% # extra slide 
    
    # remove unneeded variables
    select(-c(SW_ENDmm, LATQGENmm , GW_Qmm, LATQCNTmm,`P_GWkg/ha`,`YLDt/ha`,`P_STRS`,`PUPkg/ha`)) %>% 
    
    gather(variable,value,-LULC,-HRU,-GIS,-SUB,-MGT,-MON,-AREAkm2,-YR) %>% 
    mutate(scenario=scen)
  
  
  hru_output<-rbind(hru_output,add_df)
  
  rm(add_df,hru_table) # clear memory 
  
  }else{
    
    # baseline data
    
    add_df<-readSWATtxt(wd,headers_hru,'output.hru') %>% 
      mutate(across(!LULC, as.numeric)) %>% # convert everything except lulc to numeric
      
      #add year column, assume monthly data
      mutate(YR=rep(yrs,each=length(unique(GIS))*12)) %>% 
      
      # total P and N
      mutate(totp=`SOLPkg/ha`+`ORGPkg/ha`+`SEDPkg/ha`+`TILEPkg/ha`+`P_GWkg/ha`,
             totsolp=`SOLPkg/ha`+`TILEPkg/ha`,
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
             `TILEPkg/ha`=`TILEPkg/ha`*AREAkm2*100) %>% 
      ungroup() %>% 
      
      gather(variable,value,-LULC,-HRU,-GIS,-SUB,-MGT,-MON,-AREAkm2,-YR) %>% 
      mutate(scenario=scen)
      
      
    
    hru_output<-rbind(hru_output,add_df)
    
    rm(add_df)
    
    
  }
  
  print(paste(scen, 'loaded'))
}


###### Calculate percent change for each scenario as compared to baseline #######

hru_output_yearly <- hru_output %>% 
  mutate(scenario=factor(scenario)) %>% 
  group_by(GIS,YR,variable,scenario) %>%
  summarize(value=sum(value,na.rm=T)) %>% # yearly average of monthly values
  mutate(percent_change=(value-value[scenario=="Baseline"])*100/value[scenario=="Baseline"]) %>%
  mutate(abs_change=(value-value[scenario=="Baseline"])) %>%
  ungroup() %>% 
  group_by(GIS,variable,scenario) %>% 
  mutate(percent_change=mean(percent_change)) %>%  # average change kg/yr across all years
  mutate(abs_change=mean(abs_change)) # average change % across all years

# Make this March-July summaries
hru_output_marjul <- hru_output %>% 
  filter(MON %in% c(3:7)) %>% 
  mutate(scenario=factor(scenario)) %>% 
  group_by(GIS,YR,variable,scenario) %>%
  summarize(value=sum(value,na.rm=T)) %>% # yearly average of monthly values
  mutate(percent_change=(value-value[scenario=="Baseline"])*100/value[scenario=="Baseline"]) %>%
  mutate(abs_change=(value-value[scenario=="Baseline"])) %>%
  ungroup() %>% 
  group_by(GIS,variable,scenario) %>% 
  mutate(percent_change=mean(percent_change)) %>%  # average change kg/yr across all years
  mutate(abs_change=mean(abs_change)) # average change % across all years


###### HRU plots #####

variable_labs<-c('Tile discharge','Surface runoff','Surface DRP','Tile DRP','Total DRP','TP','TN')
names(variable_labs)<-c('QTILEmm','SURQ_CNTmm','SOLPkg/ha','TILEPkg/ha','totsolp','totp','totn')

##### percent change #########

# Annual
hru_output_yearly %>% 
  filter(!(scenario %in% 'Baseline'),  
         variable %in% c('QTILEmm','SURQ_CNTmm','SOLPkg/ha','TILEPkg/ha','totsolp','totp','totn')) %>%
  mutate(variable=factor(variable,ordered=T, levels=c('QTILEmm','SURQ_CNTmm','SOLPkg/ha','TILEPkg/ha','totsolp','totp','totn'))) %>% 
  mutate(scenario=factor(scenario,ordered=T,levels=levels_hru_plots)) %>% 
  ggplot(.,aes(x=scenario,y=percent_change))+geom_boxplot()+facet_wrap(~variable,labeller=labeller(variable=variable_labs),scales='free_x')+
  xlab("")+ylab("Change from baseline (%)")+
    coord_flip()+
  # scale_fill_manual(values=c('baseline (2013-2020)'='white', 'land management scenario (2013-2020)'='grey'))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(),text = element_text(size = 16),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        legend.title = element_blank())

setwd(scenario_dir)
ggsave('hru_per_change_annual.png',last_plot(),height=150,width=300,units='mm')

# March-July
hru_output_marjul %>% 
  filter(!(scenario %in% 'Baseline'),  
         variable %in% c('QTILEmm','SURQ_CNTmm','SOLPkg/ha','TILEPkg/ha','totsolp','totp','totn')) %>%
  mutate(variable=factor(variable,ordered=T, levels=c('QTILEmm','SURQ_CNTmm','SOLPkg/ha','TILEPkg/ha','totsolp','totp','totn'))) %>% 
  mutate(scenario=factor(scenario,ordered=T,levels=levels_hru_plots)) %>% 
  ggplot(.,aes(x=scenario,y=percent_change))+geom_boxplot()+facet_wrap(~variable,labeller=labeller(variable=variable_labs),scales='free_x')+
  xlab("")+ylab("Change from baseline (%)")+
  coord_flip()+
  # scale_fill_manual(values=c('baseline (2013-2020)'='white', 'land management scenario (2013-2020)'='grey'))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(),text = element_text(size = 16),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        legend.title = element_blank())

setwd(scenario_dir)
ggsave('hru_per_change_marjul.png',last_plot(),height=150,width=300,units='mm')

##### absolute change #########

variable_labs<-c('Tile discharge (m3)','Surface runoff (m3)','Surface DRP (kg)','Tile DRP (kg)','Total DRP (kg)','TP (kg)','TN (kg)')
names(variable_labs)<-c('QTILEmm','SURQ_CNTmm','SOLPkg/ha','TILEPkg/ha','totsolp','totp','totn')

# Annual
hru_output_yearly %>% 
  filter(!(scenario %in% 'Baseline'),  
         variable %in% c('QTILEmm','SURQ_CNTmm','SOLPkg/ha','TILEPkg/ha','totsolp','totp','totn')) %>%
  mutate(variable=factor(variable,ordered=T, levels=c('QTILEmm','SURQ_CNTmm','SOLPkg/ha','TILEPkg/ha','totsolp','totp','totn'))) %>% 
  mutate(scenario=factor(scenario,ordered=T,levels=levels_hru_plots)) %>% 
  ggplot(.,aes(x=scenario,y=abs_change))+geom_boxplot()+facet_wrap(~variable,labeller=labeller(variable=variable_labs),scales='free_x')+
  xlab("")+ylab("Change from baseline")+
  coord_flip()+
  # scale_fill_manual(values=c('baseline (2013-2020)'='white', 'land management scenario (2013-2020)'='grey'))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(),text = element_text(size = 16),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        legend.title = element_blank())

setwd(scenario_dir)
ggsave('hru_abs_change_annual.png',last_plot(),height=150,width=300,units='mm')

# March-July
hru_output_marjul %>% 
  filter(!(scenario %in% 'Baseline'),  
         variable %in% c('QTILEmm','SURQ_CNTmm','SOLPkg/ha','TILEPkg/ha','totsolp','totp','totn')) %>%
  mutate(variable=factor(variable,ordered=T, levels=c('QTILEmm','SURQ_CNTmm','SOLPkg/ha','TILEPkg/ha','totsolp','totp','totn'))) %>% 
  mutate(scenario=factor(scenario,ordered=T,levels=levels_hru_plots)) %>% 
  ggplot(.,aes(x=scenario,y=abs_change))+geom_boxplot()+facet_wrap(~variable,labeller=labeller(variable=variable_labs),scales='free_x')+
  xlab("")+ylab("Change from baseline")+
  coord_flip()+
  # scale_fill_manual(values=c('baseline (2013-2020)'='white', 'land management scenario (2013-2020)'='grey'))+
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.background = element_blank(),text = element_text(size = 16),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        legend.title = element_blank())

setwd(scenario_dir)
ggsave('hru_abs_change_marjul.png',last_plot(),height=150,width=300,units='mm')

hru_output_marjul %>% 
  filter(scenario %in% c('Baseline','1.3. Subsurface Nutrient Application'))
write.csv(hru_output_marjul,'hru_output_marjul.csv',row.names=F)
  
}


##################### RCH OUTPUTS ##########################################################


rch_headers<-c('RCH'      ,'GIS',   'MON',     'AREAkm2',  'FLOW_INcms', 'FLOW_OUTcms',     'EVAPcms',    
               'TLOSScms',  'SED_INtons', 'SED_OUTtons', 'SEDCONCmg/kg',   'ORGN_INkg',  'ORGN_OUTkg',   'ORGP_INkg',  'ORGP_OUTkg',    
               'NO3_INkg',   'NO3_OUTkg',    'NH4_INkg',   'NH4_OUTkg',    'NO2_INkg',   'NO2_OUTkg',   'MINP_INkg',  
               'MINP_OUTkg',   'CHLA_INkg',  'CHLA_OUTkg',   'CBOD_INkg',  'CBOD_OUTkg',  'DISOX_INkg',
               'DISOX_OUTkg', 'SOLPST_INmg', 'SOLPST_OUTmg', 'SORPST_INmg', 'SORPST_OUTmg',  'REACTPSTmg',    'VOLPSTmg',  'SETTLPSTmg',
               'RESUSP_PSTmg', 'DIFFUSEPSTmg', 'REACBEDPSTmg',   'BURYPSTmg',   'BED_PSTmg', 'BACTP_OUTct', 'BACTLP_OUTct',  'CMETAL#1kg',  
               'CMETAL#2kg',  'CMETAL#3kg',     'TOT Nkg',     'TOT Pkg', 'NO3ConcMg/l',    'WTMPdegc')

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
  write.csv(.,'annual_RCH_allYrs.csv',row.names=F)

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
ggsave('March_July_RCH.png',mar_july_plot,height=150,width=510,units='mm')

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

ggsave('March_July_RCH_TN.png',last_plot(),height=150,width=300,units='mm')


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

ggsave('March_July_RCH_discharge.png',last_plot(),height=150,width=300,units='mm')
# mar_jul_TP


### save csv excel format #####

mar_jul_MAW<-mar_jul_MAW %>% 
  pivot_wider(names_from=c(scenario),values_from=c(value,value_corrected,percent_change))


write.csv(mar_jul_MAW,'MarJulRCH.csv',row.names=F)



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
annual_TP


annual_plot<-grid.arrange(annual_DRP,annual_TP,nrow=1,widths=c(1.5,1))

setwd(scenario_dir)
ggsave('annual_RCH.png',annual_plot,height=150,width=510,units='mm')

# excel format
annual_MAW<-annual_MAW %>% 
  pivot_wider(names_from=c(scenario),values_from=c(value,value_corrected,percent_change))

write.csv(annual_MAW,'annualRCH.csv',row.names=F)


write.csv(obs_lookup,'bias_c_factors.csv')