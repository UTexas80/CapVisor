

source("setup.R")

if (AUM.GRAPHS){
  string = "AUM Graphs"
  string = paste(string,spacer(string),pages)
  toc = c(toc,string)
  
  #Total AUM
  df = ddply(melt(AUM.Mod, id.var="Date"),~Date,summarise,AUM=sum(value))
  layout(c(1,1))
  p = ggplot(df,aes(x=Date,y=AUM/1000000))+
    geom_bar(stat="identity",fill="grey") + 
    geom_text(aes(label = round(AUM/1000000,2)), vjust = -.5) +
    ylab("AUM (Millions)") + xlab("") +
    colorscheme +
    theme.noframe
  print(p)
  
  if (PPT  && chart["AUM1"]){
    pages = pages + 1

    addSlideFunction(doc = mydoc,
                     slideType="Title and Content", 
                     slideTitle = "Total AUM", 
                     slideVisual =  p, 
                     addType = "plot")
  }
  
  df = melt(AUM.Mod,id.var="Date",variable.name="Manager")
  df$Strategy = mgr$Strategy[match(df$Manager,mgr$Manager)]
  df$value = as.numeric(df$value)
  
  
  #Historical AUM by Manager 
  layout(c(1,1))
  p = ggplot(df, aes(x=Date, y=value/1000000, fill=Manager)) + 
    geom_bar(stat="identity") + xlab("") +
    geom_text(data=subset(df, value>0), position = "stack", aes(label = round(value/1000000,2)), vjust = -.5) + # LABEL
    ylab("AUM (Millions)") + colorscheme +
    scale_y_continuous(labels = comma) +
    theme.noframe
  print(p)
  
  if (PPT && chart["AUM2"]){
    pages = pages + 1
 
    addSlideFunction(doc = mydoc, slideType="Title and Content", slideTitle = "Assets by Manager", 
                     slideVisual =  p, addType = "plot")
  }

  date.seq = dates[seq(1,length(dates),by=max(1,round(length(dates)/5)))]  ## MAGIC NUMBER - TRIAL & ERROR
  
  #Normalized
  layout(c(1,1))
  p = ggplot(df,aes(x=Date,y=value,group=Manager,fill=Manager))+
    geom_area(position="fill") + colorscheme +
    scale_x_date( labels=date_format("%b %y"), breaks = date.seq ) +
    ylab("% AUM") + xlab("") + scale_y_continuous(labels = percent_format())+
    theme.noframe
  print(p)
  
  if (PPT && chart["AUM3"]){
    pages = pages + 1

    addSlideFunction(doc = mydoc, slideType="Title and Content", slideTitle = "Assets by Manager", 
                     slideVisual =  p, addType = "plot")
  }
  
  # PIE CHART BY MANAGER
  dfpie = df[df$Date==tail(AUM.Mod,1)$Date,]  #get last entries in df
  
  layout(c(1,1))
  p = graph.pie(dfpie,dfpie$value,dfpie$Manager)
  
  if (PPT && chart["AUM4"]) {
    pages = pages + 1

    addSlideFunction(doc = mydoc, slideType="Title and Content", slideTitle = "Assets by Manager", 
                     slideVisual =  p, addType = "plot")
  }
  
  #AUM by Strategy
  
  df = ddply(df,c("Date","Strategy"),numcolwise(sum))
  dfpie = df[df$Date==tail(AUM.Mod,1)$Date,]
  
  #Actual Values
  layout(c(1,1))
  p = ggplot(df, aes(x=Date, y=value/1000000, fill=Strategy)) + 
    geom_bar(stat="identity") +
    geom_text(data=subset(df, value>0), position = "stack", aes(label = round(value/1000000,2)), vjust = -.5) + # LABEL
    ylab("AUM (Millions)") + xlab("") + colorscheme +
    scale_y_continuous(labels = comma) +
    theme.noframe
  print(p)
  
  if(PPT && chart["AUM5"]){
    pages = pages + 1

    addSlideFunction(doc = mydoc, slideType="Title and Content", slideTitle = "Assets by Strategy", 
                     slideVisual =  p, addType = "plot")
  }
  
  #Normalized
  layout(c(1,1))
  p = ggplot(df,aes(x=Date,y=value,group=Strategy,fill=Strategy))+
    geom_area(position="fill") + xlab("") +
    scale_x_date( labels=date_format("%b %y"), breaks = date.seq ) +
    ylab("% AUM") + colorscheme +  scale_y_continuous(labels = percent_format())+
    theme.noframe
  print(p)
  
  if (PPT && chart["AUM6"]){
    pages = pages + 1

    addSlideFunction(doc = mydoc, slideType="Title and Content", slideTitle = "Assets by Strategy", 
                     slideVisual =  p, addType = "plot")
  }

  
  #PIE CHART by Strategy
  layout(c(1,1))
  p = graph.pie(dfpie,dfpie$value,dfpie$Strategy)
  
  if (PPT && chart["AUM7"]) {
    pages = pages + 1

    addSlideFunction(doc = mydoc, slideType="Title and Content", slideTitle = "Assets by Strategy", 
                     slideVisual =  p, addType = "plot")
  }

}


### Portfolio Analysis (slides 9-22, 13 graphs)
if(TOTAL.PORTFOLIO){
  
  string = "Total Portfolio Analysis"
  string = paste(string,spacer(string),pages)
  toc = c(toc,string)
  
  #Risk Return Scatter Plots (1,3 & 5yr)
  for(Plot in 1:length(Periods)){
    
    Period.Qtr = Periods[Plot]*4
    portfolio.list = Portfolios[[Plot]]
    
    if(length(portfolio.list) > 0){
      x = num.quarters - Period.Qtr + 1
      y = num.quarters
      rf = as.numeric((1+Return.annualized(risk.free.rate[x:y,]))^0.25-1)
      layout(c(1,1))
      chart.RiskReturnScatter(performance[x:y,portfolio.list],Rf=rf,main="", 
                              cex.axis = 1.5, cex.lab = 1.5) ##RISK##

      p = captureplot()
      
      if(PPT && chart["TPPA1"]) {
        pages = pages + 1

        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = paste(Periods[Plot],"Year Risk-Return Comparison"), 
                         slideVisual =  p, 
                         addType = "plot")
      }
      
    }  
    
  }
  
  #Cumulative return
  layout(c(1,1))
  chart.CumReturns(performance[,port.col],wealth.index=T,begin="axis",legend.loc="topleft", main="",ylab="",
                   cex.axis = 1.5, cex.lab = 1.5)
  
  if (PPT && chart["TPPA2"]){
    pages = pages + 1

    addSlideFunction(doc = mydoc, slideType="Title and Content",
                     slideTitle = "Cumulative Return",
                     slideVisual =   chart.CumReturns(performance[,port.col],wealth.index=T,begin="axis",legend.loc="topleft", main="",ylab="",
                                                      cex.axis = 1.5, cex.lab = 1.5), 
                     addType = "plot")
  }
  
  #Historical Performance
  layout(c(1,1))
  charts.BarVaR(performance[,port.col],main="", cex.axis = 1.5, cex.lab = 1.5)
  
  p = captureplot()
  if(PPT && chart["TPPA3"]){
    pages = pages + 1

    addSlideFunction(doc = mydoc, slideType="Title and Content",
                     slideTitle = "Historical Quarterly Performance",
                     slideVisual = charts.BarVaR(performance[,port.col],main="",  cex.axis = 1.5, cex.lab = 1.5) , 
                     addType = "plot")
    
  }
  
  #Drawdown
  layout(c(1,1))
  chart.Drawdown(performance[,port.col],legend.loc="bottomleft",main="",ylab="percentage", cex.axis = 1.5, cex.lab = 1.5)
  
  if(PPT && chart["TPPA4"]){
    pages = pages + 1

    addSlideFunction(doc = mydoc, slideType="Title and Content",
                     slideTitle = "Drawdown",
                     slideVisual = chart.Drawdown(performance[,port.col],legend.loc="bottomleft",main="",ylab="percentage",
                                                  cex.axis = 1.5, cex.lab = 1.5), 
                     addType = "plot")
    
  }
  
  #Trailing Information Ratio (1,3 & 5yr)
  for(Plot in 1:length(Periods)){
    
    Total = as.data.frame(dates)
    Mgr.w.data = 1
    
    for (i in port.col){
      
      manager = (i-1)/2 + 1
      portfolio = performance[,i]
      index = performance[,i+1]
      portfolioname = colnames(performance)[i]
      indexname = colnames(performance)[i+1]
      portfolio = na.omit(portfolio)
      index = na.omit(index)
      numQuarters = length(portfolio)
      rtnData = cbind(portfolio,index)  
      colnames(rtnData)=c(portfolioname,indexname)
      
      period = Periods[Plot]*4
      
      if (numQuarters > period){
        IR = NULL
        Mgr.w.data = Mgr.w.data + 1
        
        for (k in 1:(numQuarters-period+1)){
          IR = c(IR,InformationRatio(rtnData[k:(k+period-1),portfolioname],rtnData[k:(k+period-1),indexname],scale=4))
        }
        
        tmp = rep(0,(nrow(Total) - length(IR)))
        IR = c(tmp,IR)
        
        Total[,Mgr.w.data] = IR
        colnames(Total)[Mgr.w.data] = portfolioname
      }
    }
    
    if(ncol(Total) > 1){
      Total = Total[rowSums(Total[-1]) != 0,]
      date.seq = Total$dates[seq(1,length(Total$dates),by=max(1,round(length(Total$dates)/5)))]
      
      p = ggplot(melt(Total,id="dates"),aes(x=dates,y=value,group=variable, color=variable))+
        geom_line() + scale_x_date( labels=date_format("%b %y"), breaks = date.seq ) +
        ylab("IR") + xlab("") + geom_point(aes(shape=variable)) +
        geom_hline(aes(yintercept = 0)) +
        theme.noframe
      print(p)
      
      if(PPT && chart["TPPA5"]) {
        pages = pages + 1
 
        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = paste(Periods[Plot],"Year Trailing Information Ratio"),
                         slideVisual = p, 
                         addType = "plot")
        
      } 
    }
  }
  
  #Rolling Risk-Return (1,3, & 5yr)
  for (Plot in 1:length(Periods)){
    
    Period.Qtr = Periods[Plot]*4
    portfolio.list = Portfolios[[Plot]]
    
    if ((length(portfolio.list) > 0) && (num.quarters > Period.Qtr)){
      layout(c(1,1))
      x = num.quarters - Period.Qtr + 1
      y = num.quarters
      rf = as.numeric((1+Return.annualized(risk.free.rate[x:y,]))^0.25-1)  ##RISK##
      chart.SnailTrail(performance[,portfolio.list],Rf=rf,main="",add.names ="none",width=Period.Qtr,stepsize=1,legend.loc="topleft", cex.axis = 1.5, cex.lab = 1.5)
      
      if(PPT && chart["TPPA6"]){
        pages = pages + 1

        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = paste("Rolling ",Periods[Plot],"yr Risk-Return",sep=""),
                         slideVisual = chart.SnailTrail(performance[,portfolio.list],Rf=rf,main="",add.names ="none",width=Period.Qtr,stepsize=1,legend.loc="topleft",
                                                        cex.axis = 1.5, cex.lab = 1.5), 
                         addType = "plot")
        
      }
      
    }
  }
  
  if (length(one.year) > 0 ){
    #Rolling Performance
    layout(c(1,1))
    charts.RollingPerformance(performance[,one.year],width=4,Rf=risk.free.rate,legend.loc="left",main="", 
                              cex.axis = 1.5, cex.lab = 1)
    
    if (PPT && chart["TPPA7"]){
      pages = pages + 1

      addSlideFunction(doc = mydoc, slideType="Title and Content",
                       slideTitle = "Rolling 1yr Performance",
                       slideVisual =  charts.RollingPerformance(performance[,one.year],width=4,Rf=risk.free.rate,legend.loc="left",main="",
                                                                cex.axis = 1.5, cex.lab = 1), 
                       addType = "plot")
    }
    
    #Return Distribution
    layout(c(1,1))
    chart.Boxplot(performance[,one.year],main="",sub="since portfolio inception",  
                  cex.lab = 1.5)
    
    p = captureplot()
    
    if (PPT && chart["TPPA8"]){
      pages = pages + 1
 
      addSlideFunction(doc = mydoc, slideType="Title and Content",
                       slideTitle = "Return Distribution",
                       slideVisual = p, 
                       addType = "plot")
      
    }
    
  }
  
  if ((length(three.year) > 0) && (length(three.year.mgr) > 1)){
    
      #Sometimes correlation can give errors even with valid data
      #Trap using graph setting because correcting data is not possible
      if (chart["TPPA9"]){
        #Correlation
        layout(c(1,1))
        chart.Correlation(performance[,three.year.mgr],sub="since portfolio inception")
        mtext("since portfolio inception", side = 1, line = -1.5, outer = TRUE)
        
        if(PPT && chart["TPPA9"]){
          pages = pages + 1

          addSlideFunction(doc = mydoc, slideType="Title and Content",
                           slideTitle = "Correlation",
                           slideVisual = chart.Correlation(performance[,three.year.mgr],sub="since portfolio inception"), 
                           addType = "plot")
        }
      }
  }
  
  if (length(one.year)>0){
    #CAPM
    if(PPT  && chart["TPPA10"]){
      CAPM.table = table.CAPM(performance[,1],performance[,1+1],Rf=risk.free.rate)
      if (length(one.year) > 1){
        for(i in one.year[2:(length(one.year))]){
          CAPM.table = cbind(CAPM.table,table.CAPM(performance[,i],performance[,i+1],Rf=risk.free.rate))
        }      
      }
      colnames(CAPM.table)= colnames(performance[,one.year])
      CAPM.table = format.data.frame(CAPM.table,digits=4,justify="right")
      layout(c(1,1)) 
      textplot(CAPM.table)
      title(main="",sub="versus individual portfolio benchmark and since portfolio inception")
      
      p = captureplot()   
      pages = pages + 1
      
      addSlideFunction(doc = mydoc, slideType="Title and Content",
                       slideTitle = "CAPM statistics",
                       slideVisual = p,
                       addType = "plot")
      
    }
    
    #Downside Risk Summary
    layout(c(1,1))
    PlotTable = table.DownsideRisk(performance[,one.year],Rf=risk.free.rate)
    PlotTable = format.data.frame(PlotTable,digits=4,justify="right", fontsize = 14)
    textplot(PlotTable)
    title(main="",sub="since portfolio inception")
    
    p = captureplot()
    
    if(PPT && chart["TPPA11"]){
      pages = pages + 1

      addSlideFunction(doc = mydoc, slideType="Title and Content",
                       slideTitle = "Downside Risk Summary",
                       slideVisual =  textplot(PlotTable), #p,
                       addType = "plot")
    }
    
    #Downside Risk Ratio Summary
    layout(c(1,1)) 
    PlotTable = table.DownsideRiskRatio(performance[,one.year])
    PlotTable = format.data.frame(PlotTable,digits=4,justify="right")
    textplot(PlotTable)
    title(main="",sub="since portfolio inception")
    
    if(PPT && chart["TPPA12"]){
      pages = pages + 1

      addSlideFunction(doc = mydoc, slideType="Title and Content",
                       slideTitle = "Downside Risk Ratios",
                       slideVisual =  textplot(PlotTable),
                       addType = "plot")
    }
    
  }
  
  
  #Portfolio Statistics
  layout(c(1,1))
  x = format.data.frame(table.Stats(performance[,port.col]))
  textplot(x)
  title(main="",sub="since portfolio inception")
  
  p = captureplot()
  if(PPT  && chart["TPPA13"]){
    pages = pages + 1

     addSlideFunction(doc = mydoc, slideType="Title and Content",
                     slideTitle = "Portfolio Statistics",
                     slideVisual = p,
                     addType = "plot")
  }
} #TOTAL.PORTFOLIO


### PORTFOLIO SPECIFIC ANALYSIS 
if(INDIVIDUAL.PORTFOLIO){
  
  if (num.mgr == 1) {
    
    string = paste(mgr[1,"Manager"],"Portfolio Analysis")
    string = paste(string,spacer(string),pages)
    toc = c(toc,string)
    
    MgrTbl = paste("Manager",spacer2("Manager"),mgr[1,"Manager"])
    MgrTbl = c(MgrTbl,paste("Strategy",spacer2("Strategy"),mgr[1,"Strategy"]))
    MgrTbl = c(MgrTbl,paste("Benchmark",spacer2("Benchmark"),mgr[1,"Benchmark"]))
    MgrTbl = c(MgrTbl,paste("Inception",spacer2("Inception"),strftime(mgr[1,"Inception"],format("%d %B %Y"))))
    textplot(MgrTbl,valign="top",halign="left",cex=1.5)
    
    if(PPT){
      pages = pages + 1

      addSlideFunction(doc = mydoc, slideType="Title and Content",
                       slideTitle = paste("Portfolio: ",colnames(performance)[1]), 
                       slideVisual = textplot(MgrTbl,valign="top",halign="left",cex=1.5), 
                       addType = "plot")
    }

    CapVisorPerformance(performance[,1],performance[,2],RiskFree=risk.free.rate,myPres,portfolioname=colnames(performance)[1],indexname=colnames(performance)[2])    
   } else {
      
    string = "Individual Portfolio Analysis"
    string = paste(string," "," ")
    toc = c(toc,string)
    
    if (INCLUDE.COMPOSITE  && COMPOSITE.PORTFOLIO){
      string = "  Composite Portfolio"
      string = paste(string,spacer(string),pages)
      toc = c(toc,string)
      
      if(PPT){
        pages = pages + 1

        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = paste("Portfolio: ","Composite Portfolio"), 
                         slideVisual = "",
                         addType = "")      
      }
      
      j = num.mgr*2 + 1

      CapVisorPerformance(performance[,j],performance[,j+1],RiskFree=risk.free.rate,myPres,portfolioname=colnames(performance)[j],indexname=colnames(performance)[j+1])  
    }
    
    if (SINGLE.PORTFOLIO){
      #Loop for each manager
      for (i in 1:num.mgr){
        if(Manager.active){
          
          j = 2*(i-1)+1
          MgrTbl = paste("Manager",spacer2("Manager"),mgr[i,"Manager"])
          MgrTbl = c(MgrTbl,paste("Strategy",spacer2("Strategy"),mgr[i,"Strategy"]))
          MgrTbl = c(MgrTbl,paste("Benchmark",spacer2("Benchmark"),mgr[i,"Benchmark"]))
          MgrTbl = c(MgrTbl,paste("Inception",spacer2("Inception"),strftime(mgr[i,"Inception"],format("%d %B %Y"))))
          textplot(MgrTbl,valign="top",halign="left",cex=1.5)
          p = captureplot()
          
          string = paste("  ",mgr[i,"Manager"],sep="")
          string = paste(string,spacer(string),pages)
          toc = c(toc,string)
          
          if(PPT){
            pages = pages + 1

            addSlideFunction(doc = mydoc, slideType="Title and Content",
                             slideTitle = paste("Portfolio: ",colnames(performance)[j]), 
                             slideVisual = p,
                             addType = "plot")     
            
          }
          CapVisorPerformance(performance[,j],performance[,j+1],RiskFree=risk.free.rate,myPres,portfolioname=colnames(performance)[j],indexname=colnames(performance)[j+1])
          
        }#Manager Active
      } #Manager loop
    } #SINGLE.PORTFOLIO
    
  } #num.mgr > 1
  
} #INDIVIDUAL.PORTFOLIO

### FIXED INCOME ANALYSIS 
if(exists("fixeddata") && FIXED.ANALYSIS){
  
  #Read Rating Map File
  ratingmap.read = xlsxToR("ratingmap.xlsx",header=TRUE)
  ratingmap = data.frame(ratingmap.read)
  
  #Rename Columns
  ## MAGIC NUMBER ##  File format shoudn't change!  See header.fi
  colnames(fixeddata)[4] = "MV"
  colnames(fixeddata)[9] = "Sector"
  
  
  fixeddata.original = fixeddata
  fixeddata_intermediate = sqldf("SELECT     fixeddata.Manager as Manager,
                    fixeddata.Identifier as Identifier,
                    fixeddata.Par as Par,
                    fixeddata.MV as MV,
                    ratingmap.Mapped as Rating,
                    fixeddata.Duration as Duration,
                    100*fixeddata.Yield as Yield,
                    100*fixeddata.Coupon as Coupon,
                    fixeddata.Sector as Sector,
                    ratingmap.Draw as Draw
                    FROM fixeddata
                    INNER JOIN ratingmap
                    ON fixeddata.Rating = ratingmap.Original")
  
  string = "Fixed Income Analysis"
  string = paste(string,spacer(string),pages)
  toc = c(toc,string)
  
  #Add title slide
  if(PPT){
    pages = pages + 1

    addSlideFunction(doc = mydoc, slideType="Title and Content",
                     slideTitle = "Fixed Income Analysis", 
                     slideVisual = "",
                     addType = "")      
  }
  
  #Only do fixed income analysis if mapping works
  if (nrow(fixeddata_intermediate) == nrow(fixeddata.original)){
    
    fixeddata = fixeddata_intermediate  # Analysis for total portfolio

    source("fixedincomeanalysis.r")
    
    Total.Avg.Coupon = Coupon.avg
    Total.Avg.Duration = Duration.avg
    Total.Avg.Yield = Yield.avg
    
    fi.mgrs = sqldf("select Manager from fixeddata_intermediate GROUP BY Manager")
    num.fi.mgrs = nrow(fi.mgrs)
    
    group.fixed.rating = list()
    group.fixed.sector = list()
    
    if (num.fi.mgrs > 1){
      
      for (fi.mgr in 1:num.fi.mgrs){
        
        string = paste(" ",fi.mgrs[fi.mgr,1])
        string = paste(string,spacer(string),pages)
        toc = c(toc,string)
        
        if(PPT){
          pages = pages + 1

          addSlideFunction(doc = mydoc, slideType="Title and Content",
                           slideTitle = fi.mgrs[fi.mgr,1], 
                           slideVisual = "",
                           addType = "")      
        }
        
        fixeddata = sqldf(paste("SELECT * from fixeddata_intermediate WHERE Manager = '",fi.mgrs[fi.mgr,1],"'",sep=""))
        
        source("fixedincomeanalysis.r")  # Analysis for each individual manager
        group.fixed.rating[[fi.mgr]] = fixed.rating
        group.fixed.sector[[fi.mgr]] = fixed.sector
        
        # # Save results to a list
        # if (fi.mgr == 1){
        #   group.fixed.rating = fixed.rating
        #   group.fixed.sector = fixed.sector
        #   
        # } else {
        #   group.fixed.rating = list(group.fixed.rating,fixed.rating)
        #   group.fixed.sector = list(group.fixed.sector,fixed.sector)
        # }
        
      } # Fixed manager loop
      
      # Cross Manager Fixed Income Analysis
      string = "  Cross Manager Analysis"
      string = paste(string,spacer(string),pages)
      toc = c(toc,string)
      
      if(PPT){
        pages = pages + 1

        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = "Cross Manager Fixed Income Analysis", 
                         slideVisual = "",
                         addType = "")      
      }
      
      for (i in 1:num.fi.mgrs){
        group.fixed.sector[[i]]$Manager = rep(fi.mgrs[i,1],nrow(group.fixed.sector[[i]]))
        group.fixed.sector[[i]]$MVP = group.fixed.sector[[i]]$MV/sum(group.fixed.sector[[i]]$MV)
        group.fixed.sector[[i]]$DurationCP = group.fixed.sector[[i]]$DurationC/sum(group.fixed.sector[[i]]$DurationC)
        group.fixed.sector[[i]]$YieldCP = group.fixed.sector[[i]]$YieldC/sum(group.fixed.sector[[i]]$YieldC)
        group.fixed.sector[[i]]$CouponCP = group.fixed.sector[[i]]$CouponC/sum(group.fixed.sector[[i]]$CouponC)
      }
      
      
      fi.mv = group.fixed.sector[[1]][,c("Manager","Sector","MV")]
      fi.mvp = group.fixed.sector[[1]][,c("Manager","Sector","MVP")]
      fi.durc = group.fixed.sector[[1]][,c("Manager","Sector","DurationCP")]
      fi.yldc = group.fixed.sector[[1]][,c("Manager","Sector","YieldCP")]
      fi.cpnc = group.fixed.sector[[1]][,c("Manager","Sector","CouponCP")]
      fi.dur = group.fixed.sector[[1]][,c("Manager","Sector","DurationC")]
      fi.yield = group.fixed.sector[[1]][,c("Manager","Sector","YieldC")]
      fi.coupon = group.fixed.sector[[1]][,c("Manager","Sector","CouponC")]
      fi.dur.label = sum(group.fixed.sector[[1]]$DurationC)
      fi.yld.label = sum(group.fixed.sector[[1]]$YieldC)
      fi.cpn.label = sum(group.fixed.sector[[1]]$CouponC)
      
      for (i in 2:num.fi.mgrs){
        fi.mv = rbind(fi.mv,group.fixed.sector[[i]][,c("Manager","Sector","MV")])
        fi.mvp = rbind(fi.mvp,group.fixed.sector[[i]][,c("Manager","Sector","MVP")])
        fi.durc = rbind(fi.durc,group.fixed.sector[[i]][,c("Manager","Sector","DurationCP")])
        fi.yldc = rbind(fi.yldc,group.fixed.sector[[i]][,c("Manager","Sector","YieldCP")])
        fi.cpnc = rbind(fi.cpnc,group.fixed.sector[[i]][,c("Manager","Sector","CouponCP")])
        fi.dur = rbind(fi.dur,group.fixed.sector[[i]][,c("Manager","Sector","DurationC")])
        fi.yield = rbind(fi.yield,group.fixed.sector[[i]][,c("Manager","Sector","YieldC")])
        fi.coupon = rbind(fi.coupon,group.fixed.sector[[i]][,c("Manager","Sector","CouponC")])
        fi.dur.label = c(fi.dur.label,sum(group.fixed.sector[[i]]$DurationC))
        fi.yld.label = c(fi.yld.label,sum(group.fixed.sector[[i]]$YieldC))
        fi.cpn.label = c(fi.cpn.label,sum(group.fixed.sector[[i]]$CouponC))
      }
      
      fi.dur.label.max = max(fi.dur.label)
      fi.yld.label.max = max(fi.yld.label)
      fi.cpn.label.max = max(fi.cpn.label)
      
      # need to cast and melt data frames to match lengths
      fi.mv = dcast(fi.mv,Sector~Manager)
      fi.mv[is.na(fi.mv)] = 0
      fi.mv = melt(fi.mv,id="Sector")
      
      fi.mvp = dcast(fi.mvp,Sector~Manager)
      fi.mvp[is.na(fi.mvp)] = 0
      fi.mvp = melt(fi.mvp,id="Sector")
      
      fi.durc = dcast(fi.durc,Sector~Manager)
      fi.durc[is.na(fi.durc)] = 0
      fi.durc = melt(fi.durc,id="Sector")
      
      fi.yldc = dcast(fi.yldc,Sector~Manager)
      fi.yldc[is.na(fi.yldc)] = 0
      fi.yldc = melt(fi.yldc,id="Sector")
      
      fi.cpnc = dcast(fi.cpnc,Sector~Manager)
      fi.cpnc[is.na(fi.cpnc)] = 0
      fi.cpnc = melt(fi.cpnc,id="Sector")
      
      fi.dur = dcast(fi.dur,Sector~Manager)
      fi.dur[is.na(fi.dur)] = 0
      fi.dur = melt(fi.dur,id="Sector")
      
      fi.yield = dcast(fi.yield,Sector~Manager)
      fi.yield[is.na(fi.yield)] = 0
      fi.yield = melt(fi.yield,id="Sector")
      
      fi.coupon = dcast(fi.coupon,Sector~Manager)
      fi.coupon[is.na(fi.coupon)] = 0
      fi.coupon = melt(fi.coupon,id="Sector")
      
      # Allocation by manager
      print("Allocation by manager")
      layout(c(1,1))
      p = ggplot(fi.mv,aes(x=variable,y=value/1000000,fill=Sector)) + 
        geom_bar(stat="identity") + colorscheme +
        ylab("MV (Millions)") + xlab("") +
        theme.noframe
      print(p)
      p = captureplot()
      
      if(PPT && chart["FIA36"]){
        pages = pages + 1

        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = "Allocation by Manager", 
                         slideVisual = p,
                         addType = "plot")      
      }
      
      #Allocation by Manager
      layout(c(1,1))
      p = ggplot(fi.mvp,aes(x=variable,y=value,fill=Sector)) + 
        geom_bar(stat="identity") + colorscheme +
        ylab("Percent")+ scale_y_continuous(labels = percent_format()) +
        xlab("") +
        theme.noframe
      print(p)
      p = captureplot()
      
      if(PPT && chart["FIA37"]){
        pages = pages + 1

        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = "Allocation by Manager", 
                         slideVisual = p,
                         addType = "plot")  
      }
      
      #Allocation by Sector
      layout(c(1,1))
      p = ggplot(fi.mv,aes(x=Sector,y=value/1000000,fill=variable)) + 
        geom_bar(stat="identity",position = "dodge") + colorscheme +
        ylab("MV (Millions)")+ xlab("") + 
        theme.noframe
      print(p)
      p = captureplot()
      
      if(PPT && chart["FIA38"]){
        pages = pages + 1

        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = "Allocation by Sector", 
                         slideVisual = p,
                         addType = "plot")  
      }
      
      #Allocation by Sector
      layout(c(1,1))
      p = ggplot(fi.mvp,aes(x=Sector,y=value,fill=variable)) + 
        geom_bar(stat="identity",position = "dodge") + colorscheme +
        ylab("Percent")+ xlab("") + 
        scale_y_continuous(labels = percent_format()) +
        theme.noframe
      print(p)
      p = captureplot()
      
      if(PPT && chart["FIA39"]){
        pages = pages + 1

        
        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = "Allocation by Sector", 
                         slideVisual = p,
                         addType = "plot")  
      }
      
      #Duration by Manager
      layout(c(1,1))
      p = ggplot(fi.dur,aes(x=variable,y=value,fill=Sector)) + 
        geom_bar(stat="identity") + colorscheme +
        geom_hline(aes(yintercept = Total.Avg.Duration,color="red")) +
        annotate("text",x=seq(1,num.fi.mgrs),y=fi.dur.label.max + 0.1,label=sprintf("%.3f",fi.dur.label)) +
        annotate("text", x = (1+num.fi.mgrs)/2, y = Total.Avg.Duration+0.1 ,label = paste("Avg  Dur = ",sprintf("%.3f", Total.Avg.Duration))) +      
        ylab("Duration") + xlab("") +
        theme.noframe
      print(p)
      p = captureplot()
      
      if(PPT && chart["FIA40"]){
        pages = pages + 1

        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = "Duration by Manager", 
                         slideVisual = p,
                         addType = "plot")  
      }
      
      #Duration by Manager
      layout(c(1,1))
      p = ggplot(fi.durc,aes(x=variable,y=value,fill=Sector)) + 
        geom_bar(stat="identity") + colorscheme +
        ylab("Percent") + xlab("") +
        scale_y_continuous(labels = percent_format()) +
        theme.noframe
      print(p)
      p = captureplot()
      
      if(PPT && chart["FIA41"]){
        pages = pages + 1

        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = "Duration by Manager", 
                         slideVisual = p,
                         addType = "plot")  
      }
      
      #Yield by Manager
      layout(c(1,1))
      p = ggplot(fi.yield,aes(x=variable,y=value,fill=Sector)) + 
        geom_bar(stat="identity") + colorscheme +
        geom_hline(aes(yintercept = Total.Avg.Yield,color="red")) +
        annotate("text",x=seq(1,num.fi.mgrs),y=fi.yld.label.max + 0.1,label=sprintf("%.3f",fi.yld.label)) +
        annotate("text", x = (1+num.fi.mgrs)/2, y = Total.Avg.Yield+0.1 ,label = paste("Avg  Yld = ",sprintf("%.3f", Total.Avg.Yield))) +      
        ylab("Yield") + xlab("") +
        theme.noframe
      print(p)
      p = captureplot()
      
      if(PPT && chart["FIA42"]){
        pages = pages + 1

        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = "Yield by Manager", 
                         slideVisual = p,
                         addType = "plot")  
      }
      
      #Yield by Manager
      layout(c(1,1))
      p = ggplot(fi.yldc,aes(x=variable,y=value,fill=Sector)) + 
        geom_bar(stat="identity") + colorscheme +
        ylab("Percent") + xlab("") +
        scale_y_continuous(labels = percent_format()) +
        theme.noframe
      print(p)
      p = captureplot()
      
      if(PPT && chart["FIA43"]){
        pages = pages + 1

        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = "Yield by Manager", 
                         slideVisual = p,
                         addType = "plot")
      }

      #Coupon by Manager
      layout(c(1,1))
      p = ggplot(fi.coupon,aes(x=variable,y=value,fill=Sector)) + 
        geom_bar(stat="identity") + colorscheme +
        geom_hline(aes(yintercept = Total.Avg.Coupon,color="red")) +
        annotate("text",x=seq(1,num.fi.mgrs),y=fi.cpn.label.max + 0.1,label=sprintf("%.3f",fi.cpn.label)) +
        annotate("text", x = (1+num.fi.mgrs)/2, y = Total.Avg.Coupon+0.1 ,label = paste("Avg  Cpn = ",sprintf("%.3f", Total.Avg.Coupon))) +      
        ylab("Coupon") + xlab("") +
        theme.noframe
      print(p)
      p = captureplot()
      
      if(PPT && chart["FIA44"]){
        pages = pages + 1

        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = "Coupon by Manager", 
                         slideVisual = p,
                         addType = "plot")
      }
      
      #Coupon by Manager
      layout(c(1,1))
      p = ggplot(fi.cpnc,aes(x=variable,y=value,fill=Sector)) + 
        geom_bar(stat="identity") + colorscheme +
        ylab("Percent") + xlab("") +
        scale_y_continuous(labels = percent_format()) +
        theme.noframe
      print(p)
      p = captureplot()
      
      if(PPT && chart["FIA45"]){
        pages = pages + 1
 
        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = "Coupon by Manager", 
                         slideVisual = p,
                         addType = "plot")
      }
      
      # Once again but using ratings 
      for (i in 1:num.fi.mgrs){
        group.fixed.rating[[i]]$Manager = rep(fi.mgrs[i,1],nrow(group.fixed.rating[[i]]))
        group.fixed.rating[[i]]$MVP = group.fixed.rating[[i]]$MV/sum(group.fixed.rating[[i]]$MV)
        group.fixed.rating[[i]]$DurationCP = group.fixed.rating[[i]]$DurationC/sum(group.fixed.rating[[i]]$DurationC)
        group.fixed.rating[[i]]$YieldCP = group.fixed.rating[[i]]$YieldC/sum(group.fixed.rating[[i]]$YieldC)
        group.fixed.rating[[i]]$CouponCP = group.fixed.rating[[i]]$CouponC/sum(group.fixed.rating[[i]]$CouponC)
      }
      
      fi.mv = group.fixed.rating[[1]][,c("Manager","R2","MV")]
      fi.mvp = group.fixed.rating[[1]][,c("Manager","R2","MVP")]
      fi.durc = group.fixed.rating[[1]][,c("Manager","R2","DurationCP")]
      fi.yldc = group.fixed.rating[[1]][,c("Manager","R2","YieldCP")]
      fi.cpnc = group.fixed.rating[[1]][,c("Manager","R2","CouponCP")]
      fi.dur = group.fixed.rating[[1]][,c("Manager","R2","DurationC")]
      fi.yield = group.fixed.rating[[1]][,c("Manager","R2","YieldC")]
      fi.coupon = group.fixed.rating[[1]][,c("Manager","R2","CouponC")]
      fi.dur.label = sum(group.fixed.rating[[1]]$DurationC)
      fi.yld.label = sum(group.fixed.rating[[1]]$YieldC)
      fi.cpn.label = sum(group.fixed.rating[[1]]$CouponC)
      
      for (i in 2:num.fi.mgrs){
        fi.mv = rbind(fi.mv,group.fixed.rating[[i]][,c("Manager","R2","MV")])
        fi.mvp = rbind(fi.mvp,group.fixed.rating[[i]][,c("Manager","R2","MVP")])
        fi.durc = rbind(fi.durc,group.fixed.rating[[i]][,c("Manager","R2","DurationCP")])
        fi.yldc = rbind(fi.yldc,group.fixed.rating[[i]][,c("Manager","R2","YieldCP")])
        fi.cpnc = rbind(fi.cpnc,group.fixed.rating[[i]][,c("Manager","R2","CouponCP")])
        fi.dur = rbind(fi.dur,group.fixed.rating[[i]][,c("Manager","R2","DurationC")])
        fi.yield = rbind(fi.yield,group.fixed.rating[[i]][,c("Manager","R2","YieldC")])
        fi.coupon = rbind(fi.coupon,group.fixed.rating[[i]][,c("Manager","R2","CouponC")])
        fi.dur.label = c(fi.dur.label,sum(group.fixed.rating[[i]]$DurationC))
        fi.yld.label = c(fi.yld.label,sum(group.fixed.rating[[i]]$YieldC))
        fi.cpn.label = c(fi.cpn.label,sum(group.fixed.rating[[i]]$CouponC))
      }
      
      fi.dur.label.max = max(fi.dur.label)
      fi.yld.label.max = max(fi.yld.label)
      fi.cpn.label.max = max(fi.cpn.label)
      
      # need to cast and melt data frames to match lengths
      fi.mv = dcast(fi.mv,R2~Manager)
      fi.mv[is.na(fi.mv)] = 0
      fi.mv = melt(fi.mv,id="R2")
      
      fi.mvp = dcast(fi.mvp,R2~Manager)
      fi.mvp[is.na(fi.mvp)] = 0
      fi.mvp = melt(fi.mvp,id="R2")
      
      fi.durc = dcast(fi.durc,R2~Manager)
      fi.durc[is.na(fi.durc)] = 0
      fi.durc = melt(fi.durc,id="R2")
      
      fi.yldc = dcast(fi.yldc,R2~Manager)
      fi.yldc[is.na(fi.yldc)] = 0
      fi.yldc = melt(fi.yldc,id="R2")
      
      fi.cpnc = dcast(fi.cpnc,R2~Manager)
      fi.cpnc[is.na(fi.cpnc)] = 0
      fi.cpnc = melt(fi.cpnc,id="R2")
      
      fi.dur = dcast(fi.dur,R2~Manager)
      fi.dur[is.na(fi.dur)] = 0
      fi.dur = melt(fi.dur,id="R2")
      
      fi.yield = dcast(fi.yield,R2~Manager)
      fi.yield[is.na(fi.yield)] = 0
      fi.yield = melt(fi.yield,id="R2")
      
      fi.coupon = dcast(fi.coupon,R2~Manager)
      fi.coupon[is.na(fi.coupon)] = 0
      fi.coupon = melt(fi.coupon,id="R2")
      
      #Allocation by Manager
      layout(c(1,1))
      p = ggplot(fi.mv,aes(x=variable,y=value/1000000,fill=R2)) + 
        geom_bar(stat="identity") + colorscheme +
        ylab("MV (Millions)") + xlab("") +
        theme.noframe
      print(p)
      p = captureplot()
      
      if(PPT && chart["FIA46"]){
        pages = pages + 1

        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = "Allocation by Manager", 
                         slideVisual = p,
                         addType = "plot")
      }
      
      #Allocation by Manager
      layout(c(1,1))
      p = ggplot(fi.mvp,aes(x=variable,y=value,fill=R2)) + 
        geom_bar(stat="identity") + colorscheme +
        ylab("Percent")+ scale_y_continuous(labels = percent_format()) +
        xlab("") +
        theme.noframe
      print(p)
      p = captureplot()
      
      if(PPT && chart["FIA47"]){
        pages = pages + 1

        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = "Allocation by Manager", 
                         slideVisual = p,
                         addType = "plot")
      }
      
      #Allocation by Rating
      layout(c(1,1))
      p = ggplot(fi.mv,aes(x=R2,y=value/1000000,fill=variable)) + 
        geom_bar(stat="identity",position = "dodge") + colorscheme +
        ylab("MV (Millions)")+ xlab("") + 
        theme.noframe
      print(p)
      p = captureplot()
      
      if(PPT && chart["FIA48"]){
        pages = pages + 1

        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = "Allocation by Rating", 
                         slideVisual = p,
                         addType = "plot")
      }
      
      #Allocation by Rating
      layout(c(1,1))
      p = ggplot(fi.mvp,aes(x=R2,y=value,fill=variable)) + 
        geom_bar(stat="identity",position = "dodge") + colorscheme +
        ylab("Percent")+ xlab("") + 
        scale_y_continuous(labels = percent_format()) +
        theme.noframe
      print(p)
      p = captureplot()
      
      if(PPT && chart["FIA49"]){
        pages = pages + 1

        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = "Allocation by Rating", 
                         slideVisual = p,
                         addType = "plot")
      }
      
      #Duration by Manager
      layout(c(1,1))
      p = ggplot(fi.dur,aes(x=variable,y=value,fill=R2)) + 
        geom_bar(stat="identity") + colorscheme +
        geom_hline(aes(yintercept = Total.Avg.Duration,color="red")) +
        annotate("text",x=seq(1,num.fi.mgrs),y=fi.dur.label.max + 0.1,label=sprintf("%.3f",fi.dur.label)) +
        annotate("text", x = (1+num.fi.mgrs)/2, y = Total.Avg.Duration+0.1 ,label = paste("Avg  Dur = ",sprintf("%.3f", Total.Avg.Duration))) +      
        ylab("Duration") + xlab("") +
        theme.noframe
      print(p)
      p = captureplot()
      
      if(PPT && chart["FIA50"]){
        pages = pages + 1

        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = "Duration by Manager", 
                         slideVisual = p,
                         addType = "plot")
      }
      
      #Duration by manager
      layout(c(1,1))
      p = ggplot(fi.durc,aes(x=variable,y=value,fill=R2)) + 
        geom_bar(stat="identity") + colorscheme +
        ylab("Percent") + xlab("") +
        scale_y_continuous(labels = percent_format()) +
        theme.noframe
      print(p)
      p = captureplot()
      
      if(PPT && chart["FIA51"]){
        pages = pages + 1

        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = "Duration by Manager", 
                         slideVisual = p,
                         addType = "plot")
      }
      
      #Yield by Manager
      layout(c(1,1))
      p = ggplot(fi.yield,aes(x=variable,y=value,fill=R2)) + 
        geom_bar(stat="identity") + colorscheme +
        geom_hline(aes(yintercept = Total.Avg.Yield,color="red")) +
        annotate("text",x=seq(1,num.fi.mgrs),y=fi.yld.label.max + 0.1,label=sprintf("%.3f",fi.yld.label)) +
        annotate("text", x = (1+num.fi.mgrs)/2, y = Total.Avg.Yield+0.1 ,label = paste("Avg  Yld = ",sprintf("%.3f", Total.Avg.Yield))) +      
        ylab("Yield") + xlab("") +
        theme.noframe
      print(p)
      p = captureplot()
      
      if(PPT && chart["FIA52"]){
        pages = pages + 1

        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = "Yield by Manager", 
                         slideVisual = p,
                         addType = "plot")
      }
      
      #Yield by Manager
      layout(c(1,1))
      p = ggplot(fi.yldc,aes(x=variable,y=value,fill=R2)) + 
        geom_bar(stat="identity") + colorscheme +
        ylab("Percent") + xlab("") +
        scale_y_continuous(labels = percent_format()) +
        theme.noframe
      print(p)
      p = captureplot()
      
      if(PPT && chart["FIA53"]){
        pages = pages + 1

        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = "Yield by Manager", 
                         slideVisual = p,
                         addType = "plot")
      }
      
      #Coupon by Manager
      layout(c(1,1))
      p = ggplot(fi.coupon,aes(x=variable,y=value,fill=R2)) + 
        geom_bar(stat="identity") + colorscheme +
        geom_hline(aes(yintercept = Total.Avg.Coupon,color="red")) +
        annotate("text",x=seq(1,num.fi.mgrs),y=fi.cpn.label.max + 0.1,label=sprintf("%.3f",fi.cpn.label)) +
        annotate("text", x = (1+num.fi.mgrs)/2, y = Total.Avg.Coupon+0.1 ,label = paste("Avg  Cpn = ",sprintf("%.3f", Total.Avg.Coupon))) +      
        ylab("Coupon") + xlab("") +
        theme.noframe
      print(p)
      p = captureplot()
      
      if(PPT && chart["FIA54"]){
        pages = pages + 1

        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = "Coupon by Manager", 
                         slideVisual = p,
                         addType = "plot")
      }
      
      #Coupon by Manager
      layout(c(1,1))
      p = ggplot(fi.cpnc,aes(x=variable,y=value,fill=R2)) + 
        geom_bar(stat="identity") + colorscheme +
        ylab("Percent") + xlab("") +
        scale_y_continuous(labels = percent_format()) +
        theme.noframe
      print(p)
      p = captureplot()
      
      if(PPT && chart["FIA55"]){
        pages = pages + 1

        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = "Coupon by Manager", 
                         slideVisual = p,
                         addType = "plot")
      }
      
      
    } # num.fi.mgrs > 1
    
  } else {
    
    x = c("Mapping of Ratings didn't work correctly","Please check holding file to see if there is a new rating","Compare with ratingmap.xlsx")
    textplot(x)
    p = captureplot()
    
    if(PPT){
      pages = pages + 1

      addSlideFunction(doc = mydoc, slideType="Title and Content",
                       slideTitle = "Fixed Income Analysis", 
                       slideVisual = p,
                       addType = "plot")
    }
    
  }
  
} #Fixed Income exists


##### EQUITY ANALYSIS 
if(exists("equitydata") && EQUITY.ANALYSIS){
  
  #change name for sql use - Sql won't accept "."
  ## MAGIC NUMBER ## - Data format shouldn't change!  See header.eq
  colnames(equitydata)[5] = "MV"
  colnames(equitydata)[6] = "DY"
  
  equitydata_original = equitydata  #Save original
  eq.mgrs = sqldf("select Manager from equitydata GROUP BY Manager")
  num.eq.mgrs = nrow(eq.mgrs)
    
  string = "Equity Analysis"
  string = paste(string,spacer(string),pages)
  toc = c(toc,string)

  if(PPT){
    pages = pages + 1

    addSlideFunction(doc = mydoc, slideType="Title and Content",
                     slideTitle = "Equity Analysis", 
                     slideVisual = "",
                     addType = "")    
  }
  
  source("equityanalysis.r")  # For total portfolio
  Total.AvgDivYield = AvgDivYield
  
  if (num.eq.mgrs > 1) {
    
    group.equitydf = list()
    
    for (eq.mgr in 1:num.eq.mgrs){
      
      string = paste(" ",eq.mgrs[eq.mgr,1])
      string = paste(string,spacer(string),pages)
      toc = c(toc,string)
      
      if(PPT){
        pages = pages + 1

        addSlideFunction(doc = mydoc, slideType="Title and Content",
                         slideTitle = eq.mgrs[eq.mgr,1], 
                         slideVisual = "",
                         addType = "")    
      }
      
      equitydata = sqldf(paste("SELECT * FROM equitydata_original WHERE Manager = '",
                                   eq.mgrs[eq.mgr,1],"'",sep=""))
           
      source("equityanalysis.r")  # for individual manager
      
      #Save data frames in list
      group.equitydf[[eq.mgr]] = equitydf
      
      # if (eq.mgr == 1) {
      #   group.equitydf = equitydf
      # } else {
      #   group.equitydf = list(group.equitydf,equitydf)
      # }
      
    }  
    
  } else {
    group.equitydf = list(equitydf)
  }
  
  #Cross manager analysis
  if(num.eq.mgrs > 1) {string = "  Cross Manager Analysis"} 
    else {string = "  Benchmark Analysis"}
  string = paste(string,spacer(string),pages)
  toc = c(toc,string)
  
  if(PPT){
    pages = pages + 1
    if (num.eq.mgrs > 1) {

      addSlideFunction(doc = mydoc, slideType="Title and Content",
                       slideTitle ="Cross Manager Equity Analysis", 
                       slideVisual = "",
                       addType = "")   
    } else{

      addSlideFunction(doc = mydoc, slideType="Title and Content",
                       slideTitle = "Benchmark Analysis", 
                       slideVisual = "",
                       addType = "")   
    }
  }
  
  for (i in 1:num.eq.mgrs){
    group.equitydf[[i]]$MVP = group.equitydf[[i]]$MV/sum(group.equitydf[[i]]$MV)
    group.equitydf[[i]]$Manager = rep(eq.mgrs[i,1],nrow(group.equitydf[[i]]))
    group.equitydf[[i]]$DivCP = group.equitydf[[i]]$DividendC/sum(group.equitydf[[i]]$DividendC)
  }
  

  eq.mv = group.equitydf[[1]][,c("Manager","Sector","MV")]
  eq.mvp = group.equitydf[[1]][,c("Manager","Sector","MVP")]
  eq.div = group.equitydf[[1]][,c("Manager","Sector","DividendC")]
  eq.divC = group.equitydf[[1]][,c("Manager","Sector","DivCP")]
  div.label = sum(group.equitydf[[1]]$DividendC)
  
  if (num.eq.mgrs > 1){
    
    for (i in 2:num.eq.mgrs){
      eq.mv = rbind(eq.mv,group.equitydf[[i]][,c("Manager","Sector","MV")])
      eq.mvp = rbind(eq.mvp,group.equitydf[[i]][,c("Manager","Sector","MVP")])
      eq.div = rbind(eq.div,group.equitydf[[i]][,c("Manager","Sector","DividendC")])
      eq.divC = rbind(eq.divC,group.equitydf[[i]][,c("Manager","Sector","DivCP")])
      div.label = c(div.label,sum(group.equitydf[[i]]$DividendC))
    }    
    
    lab.max = max(div.label)      
  
  }
  
  # Read and format SPX Sector data
  spx = xlsxToR("spx-sector.xlsx",header=TRUE)
  spx$Date = as.Date(spx$Date,origin="1899-12-30")
  spx = spx[nrow(spx),]  #Get last row
  spx = melt(spx,id="Date")
  spx$Date = rep("SPX",nrow(spx))
  colnames(spx) = colnames(eq.mvp)
  spx$MVP = as.numeric(spx$MVP) 
  eq.mvp = rbind(eq.mvp,spx) # Add to MV as percentage
  
  #cast and melt data frames
  eq.mv = dcast(eq.mv, Sector ~ Manager)
  eq.mv[is.na(eq.mv)] = 0
  eq.mv = melt(eq.mv,id="Sector")
  
  eq.mvp.cast = dcast(eq.mvp, Sector ~ Manager)
  eq.mvp.cast[is.na(eq.mvp.cast)] = 0
  eq.mvp = melt(eq.mvp.cast,id="Sector")
  
  if (num.eq.mgrs > 1) {
    
    #Alloction by Manger
    layout(c(1,1))
    p = ggplot(eq.mv,aes(x=variable,y=value/1000000,fill=Sector)) + 
      geom_bar(stat="identity") + colorscheme +
      ylab("MV (Millions)") + xlab("") +
      theme.noframe
    print(p)
    
    if(PPT && chart["EA8"]){
      pages = pages + 1

      addSlideFunction(doc = mydoc, slideType="Title and Content",
                       slideTitle = "Allocation by Manager" , 
                       slideVisual = p,
                       addType = "plot")   
      
    }
  }
  
  #Allocation by Manager
  layout(c(1,1))
  p = ggplot(eq.mvp,aes(x=variable,y=value,fill=Sector)) + 
    geom_bar(stat="identity") + colorscheme +
    ylab("Percent")+ scale_y_continuous(labels = percent_format()) +
    xlab("") +
    theme.noframe
  print(p)
  
  if(PPT && chart["EA9"]){
    pages = pages + 1

    addSlideFunction(doc = mydoc, slideType="Title and Content",
                     slideTitle = "Allocation by Manager", 
                     slideVisual = p,
                     addType = "plot")   
  }
  
  #Allocation by Sector
  layout(c(1,1))
  p = ggplot(eq.mvp,aes(x=Sector,y=value,fill=variable)) + 
    geom_bar(stat="identity",position = "dodge") + colorscheme +
    ylab("Percent")+ scale_y_continuous(labels = percent_format()) +
    theme.noframe.xrotate
  print(p)
  
  if(PPT && chart["EA10"]){
    pages = pages + 1

    addSlideFunction(doc = mydoc, slideType="Title and Content",
                     slideTitle = "Allocation by Sector", 
                     slideVisual = p,
                     addType = "plot")   
  }
  
  #Allocation relative to S&P 500
  for (i in 2:(num.eq.mgrs+1)){  #Start at 2 b/c sectors are in column 1
    eq.mvp.cast[,i] = eq.mvp.cast[,i] - eq.mvp.cast[,ncol(eq.mvp.cast)]
  }
  eq.mvp.cast = melt(eq.mvp.cast[,-ncol(eq.mvp.cast)],id="Sector")
  
  layout(c(1,1))
  p = ggplot(eq.mvp.cast,aes(x=Sector,y=value,fill=variable)) + 
    geom_bar(stat="identity",position = "dodge") + colorscheme +
    geom_text(aes(label = round(value,2)), vjust = -.5) +
    ylab("Percent")+ scale_y_continuous(labels = percent_format()) +
    theme.noframe.xrotate
  print(p)
  
  if(PPT && chart["EA11"]){
    pages = pages + 1

    addSlideFunction(doc = mydoc, slideType="Title and Content",
                     slideTitle = "Allocation Relative to S&P 500", 
                     slideVisual = p,
                     addType = "plot")   
  }
  
  
  if (num.eq.mgrs > 1){
    #Cast and melt data frames 
    eq.div = dcast(eq.div, Sector ~ Manager)
    eq.div[is.na(eq.div)] = 0
    eq.div = melt(eq.div,id="Sector")
    
    eq.divC = dcast(eq.divC, Sector ~ Manager)
    eq.divC[is.na(eq.divC)] = 0
    eq.divC = melt(eq.divC,id="Sector")
    
    #Dividend yield by Manaager
    layout(c(1,1))
    p = ggplot(eq.div,aes(x=variable,y=value,fill=Sector)) + 
      geom_bar(stat="identity") + colorscheme +
      geom_hline(aes(yintercept = Total.AvgDivYield,color="red")) +
      annotate("text",x=seq(1,num.eq.mgrs),y=lab.max + 0.1, label=sprintf("%.3f",div.label)) +
      annotate("text", x = (1+num.eq.mgrs)/2, y = Total.AvgDivYield + 0.1 ,label = paste("Avg  Div Yld = ",sprintf("%.3f", Total.AvgDivYield))) +      
      ylab("Dividend Yield") + xlab("") +
      theme.noframe
    print(p)
    
    if(PPT && chart["EA12"]){
      pages = pages + 1

      addSlideFunction(doc = mydoc, slideType="Title and Content",
                       slideTitle = "Dividend Yield by Manager", 
                       slideVisual = p,
                       addType = "plot")   
    }
    
    #Dividend Yield by Manager
    layout(c(1,1))
    p = ggplot(eq.divC,aes(x=variable,y=value,fill=Sector)) + 
      geom_bar(stat="identity") + colorscheme +
      ylab("Percent")+ scale_y_continuous(labels = percent_format()) +
      xlab("") +
      theme.noframe
    print(p)
    
    if(PPT && chart["EA13"]){
      pages = pages + 1

      addSlideFunction(doc = mydoc, slideType="Title and Content",
                       slideTitle ="Dividend Yield by Manager" , 
                       slideVisual = p,
                       addType = "plot")   
    }
  }
} # Equity exists


## DIAGNOSTIC PAGE
s.info = sessionInfo()
diagnostic = data.frame("Version","Date")
diagnostic[,1]=as.character(diagnostic[,1])
diagnostic[,2]=as.character(diagnostic[,2])
diagnostic.names = NULL

## MAGIC NUMBER ## Strings have not member names - Depends on sessionInfo()
ver =strsplit(s.info[["R.version"]][["version.string"]][1]," ")[[1]][3]
dat = as.character(substr( strsplit(s.info[["R.version"]][["version.string"]][1]," ")[[1]][4],2,11))
diagnostic = rbind(diagnostic,c(ver,dat))

ver = s.info[["platform"]][1]
dat = ""
diagnostic = rbind(diagnostic,c(ver,dat))

diagnostic.names = c(diagnostic.names,"R Version","platform")


if (length(s.info[["otherPkgs"]])> 0){
  for(i in 1:length(s.info[["otherPkgs"]])){
    ver = s.info[["otherPkgs"]][[i]]$Version
    dat = as.character(s.info[["otherPkgs"]][[i]]$Date)
    if(length(dat)==0){dat = " "}
    diagnostic = rbind(diagnostic,c(ver,dat))
    
    diagnostic.names = c(diagnostic.names,s.info[["otherPkgs"]][[i]]$Package)
  }
}

if (length(s.info[["loadedOnly"]])> 0){
  for(i in 1:length(s.info[["loadedOnly"]])){
    ver = s.info[["loadedOnly"]][[i]]$Version
    dat = as.character(s.info[["loadedOnly"]][[i]]$Date)
    if(length(dat)==0){dat = " "}
    diagnostic = rbind(diagnostic,c(ver,dat))
    
    diagnostic.names = c(diagnostic.names,s.info[["loadedOnly"]][[i]]$Package)
  }
}

#Add code diagnostic information
diagnostic = rbind(diagnostic,c(code.version,as.character(code.ModDate)))
diagnostic = rbind(diagnostic,c(performance.version,as.character(performance.ModDate)))
diagnostic = rbind(diagnostic,c(fixedincome.version,as.character(fixedincome.ModDate)))
diagnostic = rbind(diagnostic,c(equity.version,as.character(equity.ModDate)))
diagnostic.names = c(diagnostic.names,"Base Code","Performance Code","Fixed Code","Equity Code")

diagnostic = diagnostic[-1,]
colnames(diagnostic) = c("Version","Date")
rownames(diagnostic) = diagnostic.names

last.diagnostic = 1
diagnostic.rows = 19   #MAGIC NUMBER - TRIAL & ERROR

while (last.diagnostic <= nrow(diagnostic)){
  tmp.diagnostic = diagnostic[last.diagnostic:min(nrow(diagnostic),last.diagnostic+diagnostic.rows),]
  layout(c(1,1))
  textplot(cbind(tmp.diagnostic),valign="top")
  p = captureplot()
  
  if(PPT & chart["SD1"]){

    addSlideFunction(doc = mydoc, slideType="Title and Content",
                     slideTitle = "System Diagnostics ", 
                     slideVisual = p,
                     addType = "plot")   
  }
  
  last.diagnostic = last.diagnostic + diagnostic.rows + 1
}

if (PPT) {
  #Add TOC
  toc = toc[-1] # Remove 1st empty entry
  
  #Number of rows that show in a column
  TOC.rows = 19  #MAGIC NUMBER - TRIAL & ERROR
  lasttoc = 1
  
  toc.pages = ceiling(ceiling(length(toc)/(TOC.rows+1))/2)
  #if there is more than 1 TOC page, change page #s
  if (toc.pages > 1){
    x = substr(toc,0,38)
    y = substr(toc,39,99)
    y = as.numeric(y)
    y = y + toc.pages - 1
    y = as.character(y)
    y[is.na(y)] = ""
    toc = paste(x,y)
  }
  
  while (lasttoc <= length(toc)){

    toc1 = toc[lasttoc:min(lasttoc+TOC.rows,length(toc))]
    textplot(toc1,halign="left",valign="top",cex=1.5)
    p = captureplot()

    lasttoc = lasttoc + TOC.rows + 1
    addSlideFunction(doc = mydoc, slideType="Title and Content",
                     slideTitle = "Table of Contents ", 
                     slideVisual = p,
                     addType = "plot")   
    
    if (lasttoc <= length(toc)) {
      toc1 = toc[lasttoc:min(lasttoc+TOC.rows,length(toc))]
      textplot(toc1,halign="left",valign="top",cex=1.5)
      p = captureplot()
 
      lasttoc = lasttoc + TOC.rows + 1
      addSlideFunction(doc = mydoc, slideType="Title and Content",
                       slideTitle = "Table of Contents ", 
                       slideVisual = p,
                       addType = "plot")   
    }
    
  }
 
  #Save File
  if (SAVE.FILE){
    filename = paste(strftime(max(index(performance)),format="%Y.%m.%d"),"-",client[1,"Short Name"],sep = "")
 
    writeDoc(mydoc, paste(filename, ".pptx", sep=""))
    
  }

  #Close Powerpoint
  rm(mydoc)
  PPT = FALSE
}

finish.time = Sys.time()
time = finish.time = start.time

# VERSION HISTORY
# 2015.01.09 - v.1.0.0
#  1st release
#
# 2015.01.09 - v.1.0.1
#  Fix error in sharpe lines not showing up in certain graphs
#
# 2015.01.11 - v.1.0.2
#  Fixed Magic numbers
#  Added all risk free rates to analysis
#  Consolidated Functions in one section
#  Added analysis options in spreadsheet
#  Fixed so that it works for single manager
#
# 2015.01.15 - v.1.0.3
#  Added otherPkgs info to diagnostic
#  Added ability to include/exclude composite
#  Converted all data to same decimal format
#  Edited Rf rate used in graphs
#
# 2015.01.16 - v.1.0.4
#  Fixed manager list selection (n.year.mgr)
#  Fixed graph error when length is exactly 1,3 or 5yr
#  Trap correlation using chartselection
#
# 2016.01.16 - v.1.0.5 (Opex Analytics)
#  Changed Presentation package to ReporteRs (editable graphics in PPT)
#  Updated some aesthetics (font sizes)
