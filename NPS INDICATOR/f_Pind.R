##############################Phosphorus indicator

f.Pind <- function(year,lu,Pferfn,countybd,lvstockfn, eccfn,areafn,perc.areafn,DailyQ,AnnualQ,dem,Soisand,Soisilt,Soiclay,om,rainsite,dailyR,Mask,param,Plot){
  
  ###Inputs needing digitalization
  Pfer.r <- f.Pfer(year,lu,Pferfn,countybd)
  Pman.r <- f.Pman(lvstockfn, eccfn,areafn,perc.areafn,lu,year,countybd)
  Runoff <- as.data.frame(f.runoff(DailyQ,AnnualQ,year))

  ##############DP calcualtion
  
  ###(1)Calculation of soil soluble P loss considering soil P conversion and DP delivery efficiency.
  DPsoi_sur <- OlsenP *prod(Runoff$RDsur,param[7],param[8],0.01)
  DPsoi_sub <- OlsenP *prod(Runoff$RDsub,param[7],param[8],0.01,param[9])
  
  ###(2)Caculation of Mannure DP considering losable percentage of manure P,DP delivery efficiency and P concentration ratio between diffrent runoff pahtways. 
  
  DPman_sur <- Pman.r*prod(Runoff$TFranks,Runoff$DRsur,param[11],param[10],param[8])
  DPman_sub <- Pman.r*prod(Runoff$TFranks,Runoff$DRsub,param[11],param[10],param[8],param[9])
  
  ###(3)Caculation of fertiluzer DP considering fertilizer P utilization rates,DP delivery efficiency and P concentration ratio between diffrent runoff pahtways. 
  
  DPfer_sur <- Pfer.r*prod(Runoff$TFranks,Runoff$DRsur,param[12],param[8])
  DPfer_sub <- Pfer.r*prod(Runoff$TFranks,Runoff$DRsub,param[12],param[8],param[9])
  
 
  TDPsur <- sum(DPsoi_sur,DPman_sur,DPfer_sur)
  TDPsub <- sum(DPsoi_sub,DPman_sub,DPfer_sub)
  TDP <- TDPsur + TDPsub
  
  
  ##############PP calcualtion
 
  A <-100*f.usle(year,dem,Soisand,Soisilt,Soiclay,om,lu,rainsite,dailyR,param[1],param[2],countybd,Mask)### ton/km2
  
  SDR <- 1/(((113000*f.dis(Mask,y=liying))^param[3])*((113000*f.dis(Mask,y=river))^param[4]))
  
  PER<-exp(param[5]-param[6]*log(10*A))
  
  Aoutlet <- A*SDR*Runoff$TFranks ### ton/km2
  
  PPsoi <- Aoutlet*TPsoi*PER*0.00001 #### kg/ha
  
  PPman <- (1-param[10])*Pman.r*SDR*Runoff$TFranks #### kg/ha
  
  TPP <- sum(PPsoi, PPman)
  
  TP <- sum(TDPsur,TDPsub,TPP)
  
  
  
  ###plot
  
  plot.ls <- list (DPsoi_sur,DPsoi_sub,DPman_sur,DPman_sub,DPfer_sur,DPfer_sub,
                   TDPsur,TDPsub,A,Aoutlet,PPsoi,PPman,TPP,TP)
  
  plot.ls.names <- c("DPsoi_sur","DPsoi_sub","DPman_sur","DPman_sub","DPfer_sur","DPfer_sub",
                      "TDPsur","TDPsub","A","Aoutlet","PPsoi","PPman","TPP","TP")
  
  if (Plot==TRUE) return (plot(stack(plot.ls),main=plot.ls.names))
  
  else {
    f.summary <- function (x){return(cellStats(x,"mean",na.rm=T))}
    Ploss <- sapply(plot.ls,FUN=f.summary)
    Ploss <- as.matrix(round(Ploss,digits=3))
    row.names(Ploss) <- plot.ls.names

    return(Ploss)}###the annual average loads of above mentioned outcomes.

}





