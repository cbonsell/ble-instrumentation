#TCM data load and munging####
#Bonsell
# Oct 2019

#gets everything in YYYY-MM-DD HH:MM:SS time format with timezone set to Alaska


locale(tz="US/Alaska")

library(tidyverse)
library(lubridate)

########################
###TCM Temperature###
########################

filelist =list.files(path = ".", recursive = TRUE,
                     pattern = "\\Temperature.csv$", 
                     full.names = TRUE)
TCM_Temp <- filelist %>%
  set_names(.) %>%
  map_df(read_csv, .id = "FileName")

TCM_Temp<-TCM_Temp%>%mutate(Station=str_sub(FileName,21,25))%>%
  select(Station, DT=`ISO 8601 Time`, Temperature=`Temperature (C)`) 

########################
###TCM Currents###
########################
filelist =list.files(path = ".", recursive = TRUE,
                     pattern = "\\Current.csv$", 
                     full.names = TRUE)
TCM_Curr <- filelist %>%
  set_names(.) %>%
  map_df(read_csv, .id = "FileName")

TCM_Curr<-TCM_Curr%>%mutate(Station=str_sub(FileName,21,25))%>%
  select(Station, DT=`ISO 8601 Time`, Speed_cm_s=`Speed (cm/s)`, Heading= `Heading (degrees)`,
         Vel_N=`Velocity-N (cm/s)`, Vel_E=`Velocity-E (cm/s)`)

# Settings from Lowell software data conversion:
# TCMs: 0 ballast, Salt water
# Declination (WMM, 1 April 2019) set for TCMS: 
#   EEL and EWL: 13.86
#   KAL and JAL:19.07 