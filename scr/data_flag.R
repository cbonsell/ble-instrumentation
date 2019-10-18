####Flagging CTD data from 2018-2019 BLE LTER deployments
##also chooses "best" data set between RBR and Star Oddi
## data chosen via "identify" funtion, where you can choose by point and click on plot (currently commented out)
##C Bonsell Oct 2019
library(RColorBrewer)

############
###RBR####
############

# RBR2$Salinity[RBR2$anom==T]<-NA
# RBR2$Conductivity[RBR2$anom==T]<-NA
# RBR2$`Specific conductivity`[RBR2$anom==T]<-NA
x<-RBR2%>%mutate(flag=F)%>%group_split(Station)

EELD1<-as.data.frame(x[1])
EELD1$flag[c(482:nrow(EELD1))]<-T

# IDoutlier(as.data.frame(x[2]), "DT", "Salinity", "EWLD2")
# IDoutlier2(as.data.frame(x[2]), "Temperature", "Salinity", "EWLD2")
EWLD2<-as.data.frame(x[2])
EWLD2$flag[c(2901,4935:5108,5793:5962,7179,8217:8244)]<-T


# IDoutlier(as.data.frame(x[3]), "DT", "Salinity", "KALD2")
# IDoutlier2(as.data.frame(x[3]), "Temperature", "Salinity", "KALD2")
#looks ok
JALD2<-as.data.frame(x[3])

# IDoutlier(as.data.frame(x[4]), "DT", "Salinity", "KALD2")
# IDoutlier2(as.data.frame(x[4]), "Temperature", "Salinity", "KALD2")
#looks ok
KALD2<-as.data.frame(x[4])

RBR_flagged<-bind_rows(EWLD2,JALD2,KALD2) %>%
  select(Station, DT, Conductivity, Temperature, Pressure, 
         Sea.pressure, Depth, Salinity, Speed.of.sound, 
         Specific.conductivity, Density.anomaly, anom, flag)

############
###StarOddi####
############

#SO2$Sal[SO2$anom==T]<-NA
#SO2$Cond_mspercm[SO2$anom==T]<-NA
x<-SO3%>%mutate(flag=F)%>%group_split(Station)


# IDoutlier(as.data.frame(x[1]), "DT", "Sal", "EELD1")
# IDoutlier2(as.data.frame(x[1]), "Temp_C", "Sal", "EELD1")
EELD1<-as.data.frame(x[1])
EELD1$flag[c(482:nrow(EELD1))]<-T
#throw out


# IDoutlier(as.data.frame(x[2]), "DT", "Sal", "EELD2")
# IDoutlier2(as.data.frame(x[2]), "Temp_C", "Sal", "EELD2")
EELD2<-as.data.frame(x[2])
EELD2$flag[c(1:15,7127:7519)]<-T
# IDoutlier(EELD2, "DT", "Sal", "EELD2")


# IDoutlier(as.data.frame(x[3]), "DT", "Sal", "EWLD1")
# IDoutlier2(as.data.frame(x[3]), "Temp_C", "Sal", "EWLD1")
EWLD1<-as.data.frame(x[3])
EWLD1$flag[7524]<-T
#looks ok

# IDoutlier(as.data.frame(x[4]), "DT", "Sal", "EWLD2")
# IDoutlier2(as.data.frame(x[4]), "Temp_C", "Sal", "EWLD2")
EWLD2<-as.data.frame(x[4])
EWLD2$flag[c(1:2)]<-T
#throw this one out?


# IDoutlier(as.data.frame(x[5]), "DT", "Sal", "JALD1")
# IDoutlier2(as.data.frame(x[5]), "Temp_C", "Sal", "JALD1")
JALD1<-as.data.frame(x[5])
JALD1$flag[c(2984,3387,7676:7891)]<-T
# IDoutlier(JALD1, "DT", "Sal", "JALD1")

# IDoutlier(as.data.frame(x[6]), "DT", "Sal", "JALD2")
# IDoutlier2(as.data.frame(x[6]), "Temp_C", "Sal", "JALD2")
JALD2<-as.data.frame(x[6])
JALD2$flag[c(2478:2720)]<-T
# IDoutlier(JALD2, "DT", "Sal", "JALD2")


# IDoutlier(as.data.frame(x[7]), "DT", "Sal", "KALD1")
# IDoutlier2(as.data.frame(x[7]), "Temp_C", "Sal", "KALD1")
KALD1<-as.data.frame(x[7])
KALD1$flag[c(8:194,5293,7641:8333)]<-T
# IDoutlier(KALD1, "DT", "Sal", "KALD1")

# IDoutlier(as.data.frame(x[8]), "DT", "Sal", "KALD2")
# IDoutlier2(as.data.frame(x[8]), "Temp_C", "Sal", "KALD2")
KALD2<-as.data.frame(x[8])
KALD2$flag[c(112:171,7715:8327)]<-T
# IDoutlier(KALD1, "DT", "Sal", "KALD2")

SO_flagged<-bind_rows(EELD1,EELD2,EWLD1,JALD1,KALD1)[,c(1,4:9,19:21)]
