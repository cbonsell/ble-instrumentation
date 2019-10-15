#TCM data load and munging####
#Bonsell
# Oct 2019

library(tidyverse)
library(lubridate)

filelist =list.files(path = ".", recursive = TRUE,
                     pattern = "\\Temperature.csv$", 
                     full.names = TRUE)
TCM_Temp <- filelist %>%
  set_names(.) %>%
  map_df(read_csv, .id = "FileName")

TCM_Temp<-TCM_Temp%>%mutate(Station=str_sub(FileName,21,25))%>%
  select(Station, DT=`ISO 8601 Time`, Temperature=`Temperature (C)`)

filelist =list.files(path = ".", recursive = TRUE,
                     pattern = "\\Current.csv$", 
                     full.names = TRUE)
TCM_Curr <- filelist %>%
  set_names(.) %>%
  map_df(read_csv, .id = "FileName")

TCM_Curr<-TCM_Curr%>%mutate(Station=str_sub(FileName,21,25))%>%
  select(Station, DT=`ISO 8601 Time`, Speed_cm_s=`Speed (cm/s)`, Heading= `Heading (degrees)`,
         Vel_N=`Velocity-N (cm/s)`, Vel_E=`Velocity-E (cm/s)`)
