####edit extra long YSI data to only have data needed for instrument calibration

YSI<- read_csv("data/YSI 2018to2019.csv")%>%
  filter(DepthCat=="depth",Parameter %in% c("Temp.C","Sal","Cond"),
         !Station %in% c("SILD1","SILD2","STLD1","STLD2"))%>%
  select(Station,Parameter,Value,Datetime)%>%
  mutate(DT=round_date(Datetime, unit="hour"))%>%
  spread(Parameter, Value)%>%
  mutate(Conductivity=Cond/1000)%>%
  select(Station,DT,Salinity=Sal,Temperature=Temp.C, Conductivity) %>% 
  filter(!is.na(DT))


write.csv(YSI, "YSI_for_calibration.csv")
