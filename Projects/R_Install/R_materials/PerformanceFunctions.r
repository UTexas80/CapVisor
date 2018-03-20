
CapVisorPerformance <- function(portfolio,index,RiskFree=0,myPres,portfolioname="",indexname=""){

  performance.version <<-  "1.0.3"
  performance.ModDate <<-  as.Date("2015-01-16") 

    #Initilization
    portfolio = na.omit(portfolio)
    index = na.omit(index)
    lstdate = strftime(substr(max(index(portfolio)),1,10),format="%d %B %Y")
    timeseq = index(portfolio)
    endquarter = as.numeric(substr(max(index(portfolio)),6,7))/3
    numQuarters = length(portfolio)
    Periods = c(1,3,5)
  
    #Table of Returns
    rtnData = cbind(portfolio,index)  
    colnames(rtnData)=c(portfolioname,indexname)

    RtnTable = Return.cumulative(rtnData[seq(numQuarters-1+1,numQuarters),]) 		#1 Quarter - There is always 1 Quarter
    rNames = "1 Quarter"
    
    if (numQuarters >= endquarter) {
      RtnTable = rbind(RtnTable,Return.cumulative(rtnData[seq(numQuarters-endquarter+1,numQuarters)],geometric=T))
    } else {
      RtnTable = rbind(RtnTable,Return.cumulative(rtnData[seq(numQuarters-numQuarters+1,numQuarters)],geometric=T))      
    }
    
    rNames = c(rNames,"Ytd")
    
   if (numQuarters >= 4) {																			# 1 Year
      RtnTable = rbind(RtnTable,Return.cumulative(rtnData[seq(numQuarters-4+1,numQuarters)]))
      rNames = c(rNames,"1 Year")
    }                 
   
    if (numQuarters >= 12) {																			# 3 Years
      RtnTable = rbind(RtnTable,Return.annualized(rtnData[seq(numQuarters-12+1,numQuarters)],scale=4))
      rNames = c(rNames,"3 Years")
    }                 
    if (numQuarters >= 20) {																			# 5 Years
      RtnTable = rbind(RtnTable,Return.annualized(rtnData[seq(numQuarters-20+1,numQuarters)],scale=4))
      rNames = c(rNames,"5 Years")
    }                 
    
    if (numQuarters >= 4) {																			# Inception
      RtnTable = rbind(RtnTable,Return.annualized(rtnData, scale = 4))} else {
        RtnTable = rbind(RtnTable,Return.cumulative(rtnData))  
      }
    rNames = c(rNames,"Inception")
    rownames(RtnTable) = rNames
    
    ylimMax = 1.5*max(RtnTable)*100
    ylimMin = min(0,1.5*min(RtnTable)*100)
    
    #Trailing Performance
    layout(c(1,1)) 
   
    x = as.data.frame(RtnTable)
    x$period = rownames(x)
    x = melt(x, id = 'period')
    

    #FIX ORDER TO: Quarter, YTD, I year, 3 year, 5 year (if available) and inception.
    target = c("1 Quarter", "Ytd", "1 Year", "3 Years", "5 Years", "Inception")
    x$period = factor(x$period, levels=target)
    x = x[order(x$period),]
    
    p = ggplot(x, aes(x = period, fill = variable)) +
      geom_bar(stat="identity", aes(y=value, ymax=value), position="dodge") +
      geom_text(aes(y=value, label = paste(round(value*100,2), "%"), 
                    vjust=ifelse(sign(value)>0, -.5, 0)),
                position = position_dodge(width=1))  +
      scale_y_continuous(labels = percent_format()) +
      ylab("% Return") + xlab("") +
      theme.noframe + colorscheme
    print(p)
    #p = captureplot()
    
    if(PPT && chart["IPPA1"]){
      pages <<-pages + 1
         
      addSlideFunction(doc = mydoc, slideType="Title and Content",
                       slideTitle = "Trailing Performance", 
                       slideVisual = p,
                       addType = "plot")
    }
   
    #Trailing Peformance Table
    layout(c(1,1)) 
    PlotTable = cbind(RtnTable,(RtnTable[,1]-RtnTable[,2]))
    colnames(PlotTable) = c(colnames(RtnTable),"Excess Return")
    PlotTable = as.data.frame(PlotTable * 100)
    PlotTable = format.data.frame(PlotTable,digits=4,justify="right")
    textplot(PlotTable)
    title(main="",sub="")
    p = captureplot()
    
    if(PPT && chart["IPPA2"]){
      pages <<-pages + 1

      addSlideFunction(doc = mydoc, slideType="Title and Content",
                       slideTitle = "Trailing Performance", 
                       slideVisual = p,
                       addType = "plot")
    }

  #Information Ratio (1,3 & 5 yr)
  for(Plot in 1:length(Periods)){
    period = Periods[Plot]*4
    
    if (numQuarters > period){
      IR = NULL
      
      for (i in 1:(numQuarters-period+1)){
        IR = c(IR,InformationRatio(rtnData[i:(i+period-1),1],rtnData[i:(i+period-1),2],scale=4))
      }
      
      d =dates[(length(dates)-numQuarters+period):length(dates)]
      
      ir = as.data.frame(cbind(d,IR))
      colnames(ir) = c("Date","IR")
      ir$Date = as.Date(ir$Date)
      
      date.seq = ir$Date[seq(1,length(ir$Date),by=max(1,round(length(ir$Date)/5)))]
      
      p = ggplot(ir,aes(x=Date,y=IR))+
        geom_line() + scale_x_date( labels=date_format("%b %y"), breaks = date.seq ) +
        ylab("IR") + xlab("") +
        geom_hline(aes(yintercept = 0)) +
        theme.noframe
      print(p)
      
      if(PPT&& chart["IPPA3"]) {
        pages <<- pages + 1
        
        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle =paste(Periods[Plot],"Year Trailing Information Ratio"), 
                         slideVisual = p,
                         addType = "plot")
      }
    } 
  }
  
  #Rolling Risk Return (1,3, & 5yr)
  for (Plot in 1:length(Periods)){
    period = Periods[Plot]*4
    
    if (numQuarters > period){
      layout(c(1,1))
      x = num.quarters - period + 1
      y = num.quarters
      rf = as.numeric((1+Return.annualized(risk.free.rate[x:y,]))^0.25-1)  ##RISK##
      chart.SnailTrail(rtnData,add.names ="none",Rf = rf,width=period,stepsize=1,legend.loc="topleft",main="")
      #p = captureplot()
      
      if(PPT && chart["IPPA4"]){
        pages <<-pages + 1

        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = paste("Rolling ",Periods[Plot],"yr Risk-Return",sep=""), 
                         slideVisual = chart.SnailTrail(rtnData,add.names ="none",Rf = rf,width=period,stepsize=1,legend.loc="topleft",main="",
                                                        cex.axis = 1.5, cex.lab = 1.5),
                         addType = "plot")
      }
    }
        
  }
  
  #Rolling Active Risk Return (1,3 & 5yr)
  for (Plot in 1:length(Periods)){
    period = Periods[Plot]*4
    
    if (numQuarters > period){
      alpha = rollapply(portfolio,period,Return.annualized,align="right",scale=4) - rollapply(index,period,Return.annualized,align="right",scale=4)
      std = rollapply(portfolio,period,StdDev.annualized,align="right",scale=4) - rollapply(index,period,StdDev.annualized,align="right",scale=4)
      Alpha = as.vector(na.omit(alpha))
      ActiveRisk = as.vector(na.omit(std))    
      
      layout(c(1,1))
      plot(ActiveRisk,Alpha, main="",xlab="Active Risk",ylab = "Active Return", sub="", 
           cex.axis = 1.5, cex.lab = 1.5) # ??
      lines(ActiveRisk,Alpha)
      points(ActiveRisk[length(ActiveRisk)],Alpha[length(Alpha)],col="red",pch=24,bg="red")
      abline(h=0,v=0,col="red")
      text(ActiveRisk[seq(1,length(ActiveRisk),3)],Alpha[seq(1,length(Alpha),3)],timeseq[seq(length(timeseq) - length(Alpha)+1,length(timeseq),3)],cex=0.5)
      
      p = captureplot()
      
      if(PPT && chart["IPPA5"]){
        pages <<-pages + 1
 
        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle =paste("Rolling ",Periods[Plot],"yr Active Risk-Return",sep=""), 
                         slideVisual = p,
                         addType = "plot")
      }
      
    }
    
  }
  
 
if (numQuarters >= 4){

  # Cumulative Return
  layout(c(1,1)) 
  chart.CumReturns(rtnData,legend.loc="topleft", main="",sub = "",wealth.index=TRUE,ylab="", 
                   cex.axis = 1.5, cex.lab = 1.5)
  #p = captureplot()
  
  if(PPT && chart["IPPA6"]) {
    pages <<-pages + 1

    addSlideFunction(doc = mydoc, slideType="Title and Content",
                     slideTitle = "Cumulative Returns", 
                     slideVisual =  chart.CumReturns(rtnData,legend.loc="topleft", main="",sub = "",wealth.index=TRUE,ylab="",
                                                     cex.axis = 1.5, cex.lab = 1.5),
                     addType = "plot")
  }
  
  #Historical Performance
  layout(c(1,1))
  charts.BarVaR(rtnData,main="",
                cex.axis = 1.5, cex.lab = 1.5) #,methods="ModifiedVaR"
  p = captureplot()
    
  if(PPT && chart["IPPA7"]){
    pages <<-pages + 1

    addSlideFunction(doc = mydoc, slideType="Title and Content",
                     slideTitle = "Historical Quarterly Performance", 
                     slideVisual = p,
                     addType = "plot")
  }
  
  #Drawdown
  layout(c(1,1))
  chart.Drawdown(rtnData,legend.loc="bottomleft",main="",ylab="percentage", cex.axis = 1.5, cex.lab = 1.5)
  #p = captureplot()
  if(PPT && chart["IPPA8"]){
    pages <<-pages + 1

    addSlideFunction(doc = mydoc, slideType="Title and Content",
                     slideTitle = "Drawdown", 
                     slideVisual =  chart.Drawdown(rtnData,legend.loc="bottomleft",main="",ylab="percentage",
                                                   cex.axis = 1.5, cex.lab = 1.5),
                     addType = "plot")
   }

  #Rolling Performance
  layout(c(1,1))
  charts.RollingPerformance(rtnData,width=4,Rf=RiskFree,legend.loc="left",sub = "",main="",
                            cex.axis = 1.5, cex.lab = 1)
  p = captureplot()
  
  if(PPT && chart["IPPA9"]){
    pages <<-pages + 1

    addSlideFunction(doc = mydoc, slideType="Title and Content",
                     slideTitle = "Rolling 1yr Performance", 
                     slideVisual = charts.RollingPerformance(rtnData,width=4,Rf=RiskFree,legend.loc="left",sub = "",main="",
                                                             cex.axis = 1.5, cex.lab = 1),
                     addType = "plot")
    
  }
  
  #Return Distribution
  layout(c(1,1))
  chart.Boxplot(rtnData,main="",sub = "since inception", 
                cex.axis = 1.5, cex.lab = 1.5)
  p = captureplot()
  
  if(PPT && chart["IPPA10"]){
    pages <<-pages + 1

    addSlideFunction(doc = mydoc, slideType="Title and Content",
                     slideTitle = "Return Distribution", 
                     slideVisual = p,
                     addType = "plot")
    
  }
  
  #Relative Performance
  layout(c(1,1))
  chart.RelativePerformance(portfolio,index,main="", 
                            cex.axis = 1.5, cex.lab = 1.5)
  p = captureplot()
  
  if(PPT  && chart["IPPA11"]){
    pages <<- pages + 1

    addSlideFunction(doc = mydoc, slideType="Title and Content",
                     slideTitle = "Relative Performance", 
                     slideVisual = p,
                     addType = "plot")
    
    
  }  
  
}

if (numQuarters >= 12){   
     
    # Sometimes Correlatin & VaR can produce errors even with valid data
    # Trap using graph selector b/c editing data wont' be useufl
  
    if (chart["IPPA12"]){
      #Correlation
      layout(c(1,1))
      chart.Correlation(rtnData,sub="since inception",
                        cex.axis = 1.5, cex.lab = 1.5)
      mtext("since inception", side = 1, line = -1.5, outer = TRUE)
      p = captureplot()
      
      if(PPT && chart["IPPA12"]){
        pages <<-pages + 1

        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = "Correlation", 
                         slideVisual =  p,
                         addType = "plot")
      }
    }
    
    if (chart["IPPA13"]){
      #VaR Sensitivity
      layout(c(1,1))
      chart.VaRSensitivity(portfolio,
                           methods=c("HistoricalVaR", "ModifiedVaR", "GaussianVaR"),
                           colorset=bluefocus, lwd=2,main="",sub="since inception",
                           cex.axis = 1.5, cex.lab = 1.5)
      mtext("since inception", side = 1, line = -1, outer = TRUE)
      p = captureplot()
      
      if(PPT && chart["IPPA13"]){
        pages <<-pages + 1

        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = "VaR Sensitivity", 
                         slideVisual =  p,
                         addType = "plot")
      }    
    }
    
    # Distribution graphs for portfolio & index
    layout(rbind(c(1,2),c(3,4))) 
    chart.Histogram(portfolio, main = "Plain", methods = NULL, cex.axis = 1.5, cex.lab = 1.5)
    chart.Histogram(portfolio, main = "Density", breaks=40,
                    methods = c("add.density", "add.normal"),cex.axis = 1.5, cex.lab = 1.5)
    chart.Histogram(portfolio, main = "Skew and Kurt", methods = c
                    ("add.centered", "add.rug"), cex.axis = 1.5, cex.lab = 1.5)
    chart.Histogram(portfolio, main = "Risk Measures", methods = c
                    ("add.risk"), cex.axis = 1.5, cex.lab = 1.5)
    mtext("since inception", side = 1, line = -1, outer = TRUE)
   
    p = captureplot()
    layout(c(1,1))
    
    if(PPT && chart["IPPA14"]){
      pages <<- pages + 1

      addSlideFunction(doc = mydoc, slideType="Title and Content",
                       slideTitle = "Portfolio Distribution", 
                       slideVisual =  p,
                       addType = "plot") 
    }
    
    layout(rbind(c(1,2),c(3,4))) 
    chart.Histogram(index, main = "Plain", methods = NULL, cex.axis = 1.5, cex.lab = 1.5)
    chart.Histogram(index, main = "Density", breaks=40,
                    methods = c("add.density", "add.normal"), cex.axis = 1.5, cex.lab = 1.5)
    chart.Histogram(index, main = "Skew and Kurt", methods = c
                    ("add.centered", "add.rug"), cex.axis = 1.5, cex.lab = 1.5)
    chart.Histogram(index, main = "Risk Measures", methods = c
                    ("add.risk"), cex.axis = 1.5, cex.lab = 1.5)
    mtext("since inception", side = 1, line = -1, outer = TRUE)
    p = captureplot()
    layout(c(1,1))  
    
    if(PPT && chart["IPPA15"]){
      pages <<-pages + 1
 
      addSlideFunction(doc = mydoc, slideType="Title and Content",
                       slideTitle = "Index Distribution", 
                       slideVisual =  p,
                       addType = "plot") 
    }
}   
    
}

# VERSION HISTORY
# 2015.01.09 - v.1.0.0
#  1st Release
# 2015.01.11 - v.1.0.1 
#  Fix risk free rate calculation
# 2015.01.15 - v.1.0.2
#  Edited Rf rate used in graphs
# 2015.01.16 - v.1.0.3
#  Fixed graph error when length is exactly 1,3 or 5yr
#  Trapped VaR & Correlation using GraphSelection
# 2016.01.16 - v.1.0.5 (Opex Analytics)
#  Changed Presentation package to ReporteRs (editable graphics in PPT)
#  Updated some aesthetics (font sizes)