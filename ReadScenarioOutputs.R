########### Read Maumee SWAT model HRU outputs ################
library(here)
library(tidyverse)

# assume HRU table in each folder containing a binary column with changed hrus called 'changed_hru'

# Place folder that contains all scenario folders below:
scenario_dir<-'D:\\Maumee model\\MaumeeScenarios' 

# Place list of scenarios here
scenario_list<-c('scenario 1','scenario 2')

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

setwd(paste0(scenario_dir,'\\baseline'))

# HRU output structure
# line 9 = header column

tmp<-file('output.hru')

hru_output<-readLines(tmp, n = -1)
close(tmp)

hru_file_heading<-hru_output[9] # hru file headings

hru_txt<-hru_output[10:length(hru_output)] # hru data text, remove non-data lines

headers<-c("LULC",  "HRU"     ,  "GIS"  ,"SUB",  "MGT" , "MON"  , "AREAkm2"      ,"ETmm",  
           "SW_ENDmm"    ,"PERCmm","SURQ_CNTmm" ,"LATQGENmm"    ,"GW_Qmm"    ,"WYLDmm"   ,
           "QTILEmm" ,"SOLPkg/ha" ,"LATQCNTmm" ,"ORGPkg/ha" ,"SEDPkg/ha", "TNO3kg/ha", 
           "ORGNkg/ha","NSURQkg/ha",  "PUPkg/ha" ,"P_GWkg/ha",    "P_STRS",   "YLDt/ha","TILEPkg/ha")

hru_output<-tibble(.rows=length(hru_txt))

#### FUNCTION TO EXTRACT COLUMNS AND ADD THEM TO A DATA FRAME #####

# column name as defined as input in code
# text in out file heading (with SWAT output spacing), 
# readLines output of ONLY data columns (crop anything headers and above)
# data frame to be filled

extract_columns<-function(col_input,file_heading,txt_in,df){

location<-gregexpr(pattern =col_input,file_heading)
start<-unlist(location[1])
stop<-start+nchar(col_input)-1

new_col<-substr(txt_in,start,stop)

df<-df %>% 
  mutate({{col_input}} := new_col)

df

return(df)

}



# I'd rather apply this as a function but don't know how to do so right now
for (cols in headers){
  print(cols)
  hru_output<-extract_columns(cols,hru_file_heading,hru_txt,hru_output)
  
}