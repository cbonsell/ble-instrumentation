####determine anomolous TS data using instrument error
####made for BP TempSal workup fall 2017.R
####adapted for BLE LTER
####Christina Bonsell
####last edit 11 Sept 2019
source('scr/CondtoSal_miliS.R')

print("SO error: T=0.1, Cond=1.5 mS/cm, RBR error: T=0.002, Cond=0.01 mS/cm")

TSanom_workup<-function(dat,tempcol,condcol, Terror, Cerror){

  dat$Sal<-Cond.to.Sal(dat[[condcol]], dat[[tempcol]])
  
  #the next part calculates the salinity error based on the innate loggger cond and temp error margins (Terror and Cerror)
  #this can be used in TS plots do determine if dataset's error margins fall within an acceptable range
  dat$posTerror<-dat[[tempcol]]+Terror
  dat$negTerror<-dat[[tempcol]]-Terror
  
  dat$posCerror<-dat[[condcol]]+Cerror
  dat$negCerror<-dat[[condcol]]-Cerror
  
  dat$pCpTSalerror<-Cond.to.Sal(dat$posCerror, dat$posTerror)
  dat$pCnTSalerror<-Cond.to.Sal(dat$posCerror, dat$negTerror)
  dat$nCpTSalerror<-Cond.to.Sal(dat$negCerror, dat$posTerror)
  dat$nCnTSalerror<-Cond.to.Sal(dat$negCerror, dat$negTerror)
  
  #data is "anomylous" if data +C+T error is below the freezing line 
  dat$anom<-dat$posTerror<(-0.0575*dat$pCpTSalerror) + ((dat$pCpTSalerror^(3/2))*1.710523E-3) - (2.154996E-4 * dat$pCpTSalerror^2)- 7.53E-4
  
  rownames(dat) <- seq(length=nrow(dat))
  dat$row<-seq(length=nrow(dat))
  
  dat
}


###Visulaize the outliers (black) in plot over time

outlierTS<-function(dat,tempcol,name){ 
  
  tks<-seq(dat$Date[1], dat$Date[nrow(dat)], by = "months")
  tklb<-format(tks, format="%b%d")
  
  my.col <- colorRampPalette(brewer.pal(11, "Spectral"))(diff(range(dat$row)))
  
  win.graph(30,35)
  par(las=2)
  par(mfrow=c(2,1))
  plot(dat$Date,dat[[tempcol]],col= ifelse(dat$anom==F,my.col,'black'), pch="o", cex=.3, main=name, xaxt="n")
  axis.POSIXct(1, at = tks, "%m %d", labels=tklb)
  legend("bottomleft",legend=("Temp"))
  plot(dat$Date,dat$Sal,col=ifelse(dat$anom==F,my.col,'black'), pch="o", cex=.3, xaxt="n")
  axis.POSIXct(1, at = tks, "%m %d", labels=tklb)
  legend("bottomleft",legend=("Sal"))
  
}

########identify one off outliers

IDoutlier<-function(dat,datename, salname, dfname){
  my.col <- colorRampPalette(brewer.pal(11, "Spectral"))(diff(range(dat$row)))
  df<-as.data.frame(dat)
  win.graph(50,35)
  plot(df[[datename]],df[[salname]],
       col=ifelse(dat$flag==F,my.col,'black'),
       cex=.8, main=dfname,
       ylim=c(0,45))
  badpts<-identify(df[[datename]],df[[salname]], labels=row.names(df))
  badpts
}

IDoutlier2<-function(dat,tempname, salname, dfname){
  my.col <- colorRampPalette(brewer.pal(11, "Spectral"))(diff(range(dat$row)))
  df<-as.data.frame(dat)
  win.graph(50,35)
  plot(df[[salname]],df[[tempname]],
       col=my.col, 
       cex=.8, main=dfname)
  badpts<-identify(df[[salname]],df[[tempname]], labels=row.names(df))
  badpts
}

