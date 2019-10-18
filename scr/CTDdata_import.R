##CTD data Import and munging####
##Bonsell Sept 2019
library(tidyverse)
library(lubridate)
library(readxl)

#StarOddi load and munging####
filelist =list.files(path = ".", recursive = TRUE,
                     pattern = "\\.txt$", 
                     full.names = TRUE)
StarOddi <- filelist %>%
  set_names(.) %>%
  map_df(read_table2, col_names = FALSE, .id = "FileName",
         col_types = cols(X4 = col_character(),
                          X5 = col_character(), 
                          X6 = col_character(), 
                          X7 = col_character()))


StarOddi$X4<-as.numeric(sub(",", ".", StarOddi$X4, fixed = TRUE))
StarOddi$X5<-as.numeric(sub(",", ".", StarOddi$X5, fixed = TRUE))
StarOddi$X6<-as.numeric(sub(",", ".", StarOddi$X6, fixed = TRUE))
StarOddi$X7<-as.numeric(sub(",", ".", StarOddi$X7, fixed = TRUE))

StarOddi<-StarOddi%>%mutate(Station=str_sub(FileName,15,19))%>%
                              select(Station, Date=X2, Time=X3, Temp_C=X4, Sal=X5, Cond_mspercm=X6, SoundVel_mpers=X7)

StarOddi$DT<-as_datetime(paste(as.Date(StarOddi$Date, format="%d.%m.%Y"), StarOddi$Time))


#RBR load and munging####
filelist =list.files(path = ".", recursive = TRUE,
                     pattern = "\\.xls$", 
                     full.names = TRUE)
RBR <- filelist %>%
  set_names(.) %>%
  map_df(read_excel, col_names = T, .id = "FileName")
#Don't know what cols 11(temp?) and 12 are

RBR<-RBR%>%mutate(Station=str_sub(FileName,29,33), DT=as.POSIXct(Timestamp, format="%d/%m/%Y %H:%M:%OS"))%>%
  select(-Timestamp, -FileName, -...11, -...12)
RBR<-RBR[,c(10,11,1:9)]# reorder columns



#TCM Temp load and munging####
filelist =list.files(path = ".", recursive = TRUE,
                     pattern = "\\Temperature.csv$", 
                     full.names = TRUE)
TCM_Temp <- filelist %>%
  set_names(.) %>%
  map_df(read_csv, .id = "FileName")

TCM_Temp<-TCM_Temp%>%mutate(Station=str_sub(FileName,21,25))



TCM_Temp$Instrument<-rep("TCM")
RBR$Instrument<-rep("RBRCTD")
StarOddi$Instrument<-rep("StarOddiCST")

