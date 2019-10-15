####TS data calibration functions
####Christina Bonsell
####Oct 2019

TS.adj<-function (raw, calib, rowfirst, rowlast, rownums){
  #raw: df column of raw data
  #calib: df column that is blank except for the calibration values
  #rowfirst: the rownumber of the first calibration value
  #rowlast: the rownumber of the second (and last) calibration value
  #rownums: a column of row numbers
  #out: vector where values will be filled in
  x1<-calib[rowfirst]-raw[rowfirst]
  x2<-calib[rowlast]-raw[rowlast]
  xslope<-(x2-x1)/(rowlast-rowfirst)
  calib<-raw+x1+((rownums-1)*xslope)
  
  invisible(calib)
}

TScal_SO<-function(dat){
  #dat is a dataframe with rows:Cond_mspercm, Temp_C, fixedCond, fixedTemp
  dat$row<-c(1:nrow(dat))
  dat$fixedTemp<-TS.adj(dat$Temp_C,dat$fixedTemp,1,nrow(dat),dat$row)
  dat$fixedCond<-TS.adj(dat$Cond_mspercm,dat$fixedCond,1,nrow(dat),dat$row)
  
  dat
}

TScal_RBR<-function(dat){
  #dat is a dataframe with rows:Conductivity, Temperature, fixedCond, fixedTemp
  dat$row<-c(1:nrow(dat))
  dat$fixedTemp<-TS.adj(dat$Temperature,dat$fixedTemp,1,nrow(dat),dat$row)
  dat$fixedCond<-TS.adj(dat$Conductivity,dat$fixedCond,1,nrow(dat),dat$row)
  
  dat
}

TSmidcal_SO<-function(dat,midpoint){
  #dat is a dataframe with rows:Cond_mspercm, Temp_C, fixedCond, fixedTemp
  #midpoint is the row number of the midpoint calibration
  ln<-nrow(dat)
  
  dat$row<-c(1:nrow(dat))
  dat$fixedCond[1:midpoint]<-TS.adj(dat$Cond_mspercm,dat$fixedCond,1,midpoint,dat$row)
  dat$fixedTemp[1:midpoint]<-TS.adj(dat$Temp_C,dat$fixedTemp,1,midpoint,dat$row)
  lrow<-ln-midpoint+1
  dat$row[midpoint:ln]<-c(1:lrow)
  dat$fixedCond[midpoint:ln]<-TS.adj(dat$Cond_mspercm[midpoint:ln],dat$fixedCond[midpoint:ln],1,lrow,dat$row)
  dat$fixedTemp[midpoint:ln]<-TS.adj(dat$Temp_C[midpoint:ln],dat$fixedTemp[midpoint:ln],1,lrow,dat$row)
  
  dat
}

TSmidcal_RBR<-function(dat,midpoint){
  #dat is a dataframe with rows:Conductivity, Temperature, fixedCond, fixedTemp
  #midpoint is the row number of the midpoint calibration
  ln<-nrow(dat)
  
  dat$row<-c(1:nrow(dat))
  dat$fixedCond[1:midpoint]<-TS.adj(dat$Conductivity,dat$fixedCond,1,midpoint,dat$row)
  dat$fixedTemp[1:midpoint]<-TS.adj(dat$Temperature,dat$fixedTemp,1,midpoint,dat$row)
  lrow<-ln-midpoint+1
  dat$row[midpoint:ln]<-c(1:lrow)
  dat$fixedCond[midpoint:ln]<-TS.adj(dat$Conductivity[midpoint:ln],dat$fixedCond[midpoint:ln],1,lrow,dat$row)
  dat$fixedTemp[midpoint:ln]<-TS.adj(dat$Temperature[midpoint:ln],dat$fixedTemp[midpoint:ln],1,lrow,dat$row)
  
  dat
}