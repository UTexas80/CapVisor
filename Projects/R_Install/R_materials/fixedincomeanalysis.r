#Diagnostics
fixedincome.version = "1.0.0"
fixedincome.ModDate = "2015-01-09"

#Add Contribution Columns
fixeddata=cbind(fixeddata,(fixeddata$MV*fixeddata$Duration)/sum(fixeddata$MV))
fixeddata=cbind(fixeddata,(fixeddata$MV*fixeddata$Yield)/sum(fixeddata$MV))
fixeddata=cbind(fixeddata,(fixeddata$MV*fixeddata$Coupon)/sum(fixeddata$MV))

colnames(fixeddata)[11:13] = c("DurationC","YieldC","CouponC")  #MAGIC NUMBER - File format shouldn't change

#Calculate Average values
Duration.avg = sum(fixeddata$DurationC)
Yield.avg = sum(fixeddata$YieldC)
Coupon.avg = sum(fixeddata$CouponC)

#Summarize by ratings & by sector
fixed.rating = sqldf("SELECT Rating,
                     sum(MV) as MV,
                     sum(MV*Duration) as Duration,
                     sum(MV*Yield) as Yield,
                     sum(MV*Coupon) as Coupon,
                     sum(Durationc) as DurationC,
                     sum(YieldC) as YieldC,
                     sum(CouponC) as CouponC,
                     Draw as Draw
                     FROM fixeddata
                     GROUP BY Rating")
fixed.rating[,c("Duration","Yield","Coupon")] = fixed.rating[,c("Duration","Yield","Coupon")]/fixed.rating$MV #Avg by rating
fixed.rating$R2 = reorder(fixed.rating$Rating,fixed.rating$Draw)
fixed.rating = fixed.rating[with(fixed.rating,order(R2)),]


fixed.sector = sqldf("SELECT Sector,
                     sum(MV) as MV,
                     sum(MV*Duration) as Duration,
                     sum(MV*Yield) as Yield,
                     sum(MV*Coupon) as Coupon,
                     sum(Durationc) as DurationC,
                     sum(YieldC) as YieldC,
                     sum(CouponC) as CouponC
                     FROM fixeddata
                     GROUP BY Sector")
fixed.sector[,c("Duration","Yield","Coupon")] = fixed.sector[,c("Duration","Yield","Coupon")]/fixed.sector$MV  #Avg by sector

#For sorting ratings graphs
rating.draw = fixed.rating[with(fixed.rating,order(Draw)),]$Rating
colorscheme2 = scale_fill_brewer(breaks=rating.draw,type="qual",palette = 3)


#Allocation Graphs

#Allocation by sector
layout(c(1,1))
p = ggplot(fixed.sector,aes(x=Sector,y=MV/1000000,fill=Sector)) + 
      geom_bar(stat="identity") + colorscheme +
      geom_text(aes(label = round(MV/1000000,2)), vjust = -.5) +
      ylab("MV (Millions)")+
      theme.noframe.nolegend.notitle
print(p)

if(PPT && chart["FIA1"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Allocation by Sector", 
                   slideVisual = p,
                   addType = "plot")    
}

#Allocation by Sector
layout(c(1,1))
graph.pie(fixed.sector,fixed.sector$MV,fixed.sector$Sector)
p = captureplot()

if(PPT && chart["FIA2"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Allocation by Sector" , 
                   slideVisual = p,
                   addType = "plot")    
}

#Allocation by rating
layout(c(1,1))
p = ggplot(fixed.rating,aes(x=R2,y=MV/1000000,fill=Rating)) + 
      geom_bar(stat="identity") + colorscheme +
      geom_text(aes(label = round(MV/1000000,2)), vjust = -.5) +
      ylab("MV (Millions)")+
      theme.noframe.nolegend.notitle
print(p)

if(PPT && chart["FIA3"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Allocation by Rating", 
                   slideVisual = p,
                   addType = "plot")    
}

#Allocation by Rating
fixed.rating = fixed.rating[with(fixed.rating,order(Draw)),]
colorscheme = colorscheme2
layout(c(1,1))
graph.pie(fixed.rating,fixed.rating$MV,fixed.rating$Rating)
colorscheme = colorscheme1
fixed.rating = fixed.rating[with(fixed.rating,order(Rating)),]
p = captureplot()

if(PPT && chart["FIA4"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Allocation by Rating", 
                   slideVisual = p,
                   addType = "plot")    
}


#Duration Graphs

#Histogram
x = weighted.hist(fixeddata$Duration,fixeddata$MV,plot=F)
Duration.hist = as.data.frame(cbind(x$mids,x$density))

p = ggplot(Duration.hist,aes(x=V1,y=V2))+
  geom_bar(stat="identity",fill="grey") + 
  geom_text(aes(label = paste(round(V2*100,2), "%")), vjust = -.5) +
  ylab("Percentage") +
  xlab("Duration") + colorscheme + scale_y_continuous(labels = percent_format()) +
  geom_vline(aes(xintercept = Duration.avg,color="red")) +
  annotate("text", x = Duration.avg, y = 0.9*max(Duration.hist$V2),label = paste("Avg Dur = ",sprintf("%.3f", Duration.avg))) +
  theme.noframe.nolegend
print(p)

if(PPT && chart["FIA5"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Distribution of Duration", 
                   slideVisual = p,
                   addType = "plot")    
}

#Duration by Sector
layout(c(1,1))
p = ggplot(fixed.sector,aes(x=Sector,y=Duration,fill=Sector))+
      geom_bar(stat="identity") +
      geom_text(data = subset(fixed.sector, Duration>0),aes(label = round(Duration,2)), vjust = -.5) +
      ylab("Duration") + colorscheme +
      geom_hline(aes(yintercept = Duration.avg,color="red")) +
      annotate("text", x = nrow(fixed.sector)/2, y = Duration.avg+0.1,label = paste("Avg Dur = ",sprintf("%.3f", Duration.avg))) +
      theme.noframe.nolegend.notitle
print(p)

if(PPT && chart["FIA6"]){
  pages = pages + 1
 
  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Duration by Sector", 
                   slideVisual = p,
                   addType = "plot")    
}


#Duration contribution by sector
layout(c(1,1))
p = ggplot(fixed.sector,aes(x=Sector,y=DurationC,fill=Sector))+
      geom_bar(stat="identity") +  colorscheme +
      geom_text(data = subset(fixed.sector, DurationC>0),aes(label = round(DurationC,2)), vjust = -.5) +
      ylab("Duration Contribution") +
      theme.noframe.nolegend.notitle
print(p)

if(PPT && chart["FIA7"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Duration Contribution by Sector", 
                   slideVisual = p,
                   addType = "plot")    
}

#Duration contribution by Sector
layout(c(1,1))
graph.pie(fixed.sector,fixed.sector$DurationC,fixed.sector$Sector)
p = captureplot()

if(PPT && chart["FIA8"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Duration Contribution by Sector", 
                   slideVisual = p,
                   addType = "plot")    
}

#Duration contribution by Sector
layout(c(1,1))
fixed.sector = fixed.sector[with(fixed.sector,order(-DurationC)),]
graph.contribution(fixed.sector,fixed.sector$DurationC,fixed.sector$Sector)
fixed.sector = fixed.sector[with(fixed.sector,order(Sector)),]
p = captureplot()

if(PPT && chart["FIA9"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Duration Contribution by Sector", 
                   slideVisual = p,
                   addType = "plot")    
}


#Duration by rating
layout(c(1,1))
p = ggplot(fixed.rating,aes(x=R2,y=Duration,fill=Rating))+
      geom_bar(stat="identity") +  colorscheme +
      geom_text(data = subset(fixed.rating, Duration>0),aes(label = round(Duration,2)), vjust = -.5) +
      ylab("Duration") +
      geom_hline(aes(yintercept = Duration.avg,color="red")) +
      annotate("text", x = nrow(fixed.rating)/2, y = Duration.avg+0.1,label = paste("Avg Dur = ",sprintf("%.3f", Duration.avg))) +
      theme.noframe.nolegend.notitle
print(p)

if(PPT && chart["FIA10"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Duration by Rating", 
                   slideVisual = p,
                   addType = "plot")    
}

#Duration contribution by rating
layout(c(1,1))
p = ggplot(fixed.rating,aes(x=R2,y=DurationC,fill=Rating))+
      geom_bar(stat="identity") +  colorscheme +
      geom_text(data = subset(fixed.rating, DurationC>0),aes(label = round(DurationC,2)), vjust = -.5) +
      ylab("Duration Contribution") +
      theme.noframe.nolegend.notitle
print(p)

if(PPT && chart["FIA11"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Duration Contribution by Rating", 
                   slideVisual = p,
                   addType = "plot")    
}

#Duration by Rating
fixed.rating = fixed.rating[with(fixed.rating,order(Draw)),]
colorscheme = colorscheme2
layout(c(1,1))
graph.pie(fixed.rating,fixed.rating$DurationC,fixed.rating$Rating)
colorscheme = colorscheme1
fixed.rating = fixed.rating[with(fixed.rating,order(Rating)),]
p = captureplot()

if(PPT && chart["FIA12"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Duration Contribution by Rating", 
                   slideVisual = p,
                   addType = "plot")    
}

#Duration by Rating
layout(c(1,1))
fixed.rating = fixed.rating[with(fixed.rating,order(-DurationC)),]
colorscheme = colorscheme2
graph.contribution(fixed.rating,fixed.rating$DurationC,fixed.rating$Rating)
colorscheme = colorscheme1
fixed.rating = fixed.rating[with(fixed.rating,order(Rating)),]
p = captureplot()

if(PPT && chart["FIA13"]){
  pages = pages + 1
 
  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Duration Contribution by Rating", 
                   slideVisual = p,
                   addType = "plot")    
}

# Yield Graphs
#Histogram
x = weighted.hist(fixeddata$Yield,fixeddata$MV,plot=F)
Yield.hist = as.data.frame(cbind(x$mids,x$density))

p = ggplot(Yield.hist,aes(x=V1,y=V2))+
  geom_bar(stat="identity",fill="grey") + 
  geom_text(data = subset(Yield.hist, V2>0), aes(label = paste(round(V2*100,2),"%")), vjust = -.5) +
  ylab("Percentage") +
  xlab("Yield") + colorscheme + scale_y_continuous(labels = percent_format()) +
  geom_vline(aes(xintercept = Yield.avg,color="red")) +
  annotate("text", x = Yield.avg, y = 0.9*max(Yield.hist$V2),label = paste("Avg Yld = ",sprintf("%.3f", Yield.avg))) +
  theme.noframe.nolegend
print(p)

if(PPT && chart["FIA14"]){
  pages = pages + 1
 
  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Distribution of Yield", 
                   slideVisual = p,
                   addType = "plot")    
}

#Yield by Sector
layout(c(1,1))
p = ggplot(fixed.sector,aes(x=Sector,y=Yield,fill=Sector))+
  geom_bar(stat="identity") + 
  geom_text(data = subset(fixed.sector, Yield>0), aes(label =round(Yield,2)), vjust = -.5) +
  ylab("Yield") + colorscheme +
  geom_hline(aes(yintercept = Yield.avg,color="red")) +
  annotate("text", x = nrow(fixed.sector)/2, y = Yield.avg+0.1,label = paste("Avg Yld = ",sprintf("%.3f", Yield.avg))) +
  theme.noframe.nolegend.notitle
print(p)

if(PPT && chart["FIA15"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Yield by Sector", 
                   slideVisual = p,
                   addType = "plot")    
}


#Yield contribution by sector
layout(c(1,1))
p = ggplot(fixed.sector,aes(x=Sector,y=YieldC,fill=Sector))+
  geom_bar(stat="identity") +  colorscheme +
  geom_text(data = subset(fixed.sector, YieldC>0), aes(label =round(YieldC,2)), vjust = -.5) +
  ylab("Yield Contribution") +
  theme.noframe.nolegend.notitle
print(p)

if(PPT && chart["FIA16"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Yield Contribution by Sector", 
                   slideVisual = p,
                   addType = "plot")    
}

#Yield distribution by sector
layout(c(1,1))
graph.pie(fixed.sector,fixed.sector$YieldC,fixed.sector$Sector)
p = captureplot()

if(PPT && chart["FIA17"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Yield Contribution by Sector", 
                   slideVisual = p,
                   addType = "plot")    
}

#Yield distribution by sector
layout(c(1,1))
fixed.sector = fixed.sector[with(fixed.sector,order(-YieldC)),]
graph.contribution(fixed.sector,fixed.sector$YieldC,fixed.sector$Sector)
fixed.sector = fixed.sector[with(fixed.sector,order(Sector)),]
p = captureplot()

if(PPT && chart["FIA18"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Yield Contribution by Sector", 
                   slideVisual = p,
                   addType = "plot")    
}

#Yield by rating
layout(c(1,1))
p = ggplot(fixed.rating,aes(x=R2,y=Yield,fill=Rating))+
  geom_bar(stat="identity") +  colorscheme +
  geom_text(data = subset(fixed.rating, Yield>0), aes(label =round(Yield,2)), vjust = -.5) +
  ylab("Yield") +
  geom_hline(aes(yintercept = Yield.avg,color="red")) +
  annotate("text", x = nrow(fixed.rating)/2, y = Yield.avg+0.1,label = paste("Avg Yld = ",sprintf("%.3f", Yield.avg))) +
  theme.noframe.nolegend.notitle
print(p)

if(PPT && chart["FIA19"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Yield by Rating", 
                   slideVisual = p,
                   addType = "plot")    
}

#Yield contribution by rating
layout(c(1,1))
p = ggplot(fixed.rating,aes(x=R2,y=YieldC,fill=Rating))+
  geom_bar(stat="identity") +  colorscheme +
  geom_text(data = subset(fixed.rating, YieldC>0), aes(label =round(YieldC,2)), vjust = -.5) +
  ylab("Yield Contribution") +
  theme.noframe.nolegend.notitle
print(p)

if(PPT && chart["FIA20"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Yield Contribution by Rating", 
                   slideVisual = p,
                   addType = "plot")    
}


#Yield contribution by Rating
fixed.rating = fixed.rating[with(fixed.rating,order(Draw)),]
colorscheme = colorscheme2
layout(c(1,1))
graph.pie(fixed.rating,fixed.rating$YieldC,fixed.rating$Rating)
colorscheme = colorscheme1
fixed.rating = fixed.rating[with(fixed.rating,order(Rating)),]
p = captureplot()

if(PPT && chart["FIA21"]){
  pages = pages + 1
 
  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Yield Contribution by Rating" , 
                   slideVisual = p,
                   addType = "plot")    
}

#Yield contribution by Rating
layout(c(1,1))
fixed.rating = fixed.rating[with(fixed.rating,order(-YieldC)),]
colorscheme = colorscheme2
graph.contribution(fixed.rating,fixed.rating$YieldC,fixed.rating$Rating)
colorscheme = colorscheme1
fixed.rating = fixed.rating[with(fixed.rating,order(Rating)),]
p = captureplot()

if(PPT && chart["FIA22"]){
  pages = pages + 1
  
  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Yield Contribution by Rating", 
                   slideVisual = p,
                   addType = "plot")    
}



#Coupon Graphs
#Histogram
x = weighted.hist(fixeddata$Coupon,fixeddata$MV,plot=F)
Coupon.hist = as.data.frame(cbind(x$mids,x$density))

p = ggplot(Coupon.hist,aes(x=V1,y=V2))+
  geom_bar(stat="identity",fill="grey") + 
  geom_text(data = subset(Coupon.hist, V2>0), aes(label =paste(round(V2*100,2),"%")), vjust = -.5) +
  ylab("Percentage") +
  xlab("Coupon") + colorscheme + scale_y_continuous(labels = percent_format()) +
  geom_vline(aes(xintercept = Coupon.avg,color="red")) +
  annotate("text", x = Coupon.avg, y = 0.9*max(Coupon.hist$V2),label = paste("Avg Cpn = ",sprintf("%.3f", Coupon.avg))) +
  theme.noframe.nolegend
print(p)

if(PPT && chart["FIA23"]){
  pages = pages + 1
 
  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Distribution of Coupon", 
                   slideVisual = p,
                   addType = "plot")    
}

#Coupon by Sector
layout(c(1,1))
p = ggplot(fixed.sector,aes(x=Sector,y=Coupon,fill=Sector))+
  geom_bar(stat="identity") + 
  geom_text(data = subset(fixed.sector, Coupon>0), aes(label =round(Coupon,2)), vjust = -.5) +
  ylab("Coupon") + colorscheme +
  geom_hline(aes(yintercept = Coupon.avg,color="red")) +
  annotate("text", x = nrow(fixed.sector)/2, y = Coupon.avg+0.1,label = paste("Avg Cpn = ",sprintf("%.3f", Coupon.avg))) +
  theme.noframe.nolegend.notitle
print(p)

if(PPT && chart["FIA24"]){
  pages = pages + 1
 
  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Coupon by Sector", 
                   slideVisual = p,
                   addType = "plot")    
}


#Coupon contribution by sector
layout(c(1,1))
p = ggplot(fixed.sector,aes(x=Sector,y=CouponC,fill=Sector))+
  geom_bar(stat="identity") +  colorscheme +
  geom_text(data = subset(fixed.sector, CouponC>0), aes(label =round(CouponC,2)), vjust = -.5) +
  ylab("Coupon Contribution") +
  theme.noframe.nolegend.notitle
print(p)

if(PPT && chart["FIA25"]){
  pages = pages + 1
  
  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Coupon Contribution by Sector", 
                   slideVisual = p,
                   addType = "plot")    
}

#Coupon contribution by Sector
layout(c(1,1))
graph.pie(fixed.sector,fixed.sector$CouponC,fixed.sector$Sector)
p = captureplot()

if(PPT && chart["FIA26"]){
  pages = pages + 1
   
  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Coupon Contribution by Sector", 
                   slideVisual = p,
                   addType = "plot")    
}

#coupon contribution by sector
layout(c(1,1))
fixed.sector = fixed.sector[with(fixed.sector,order(-CouponC)),]
graph.contribution(fixed.sector,fixed.sector$CouponC,fixed.sector$Sector)
fixed.sector = fixed.sector[with(fixed.sector,order(Sector)),]
p = captureplot()

if(PPT && chart["FIA27"]){
  pages = pages + 1
 
  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Coupon Contribution by Sector", 
                   slideVisual = p,
                   addType = "plot")    
}



#Coupon by rating
layout(c(1,1))
p = ggplot(fixed.rating,aes(x=R2,y=Coupon,fill=Rating))+
  geom_bar(stat="identity") +  colorscheme +
  geom_text(data = subset(fixed.rating, Coupon>0), aes(label =round(Coupon,2)), vjust = -.5) +
  ylab("Coupon") +
  geom_hline(aes(yintercept = Coupon.avg,color="red")) +
  annotate("text", x = nrow(fixed.rating)/2, y = Coupon.avg+0.1,label = paste("Avg Cpn = ",sprintf("%.3f", Coupon.avg))) +
  theme.noframe.nolegend.notitle
print(p)

if(PPT && chart["FIA28"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Coupon by Rating", 
                   slideVisual = p,
                   addType = "plot")    
}

#Coupon contribution by rating
layout(c(1,1))
p = ggplot(fixed.rating,aes(x=R2,y=CouponC,fill=Rating))+
  geom_bar(stat="identity") +  colorscheme +
  geom_text(data = subset(fixed.rating, CouponC>0), aes(label =round(CouponC,2)), vjust = -.5) +
  ylab("Coupon Contribution") +
  theme.noframe.nolegend.notitle
print(p)

if(PPT && chart["FIA29"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Coupon Contribution by Rating", 
                   slideVisual = p,
                   addType = "plot")    
}

#Coupon contribution by rating
fixed.rating = fixed.rating[with(fixed.rating,order(Draw)),]
colorscheme = colorscheme2
layout(c(1,1))
graph.pie(fixed.rating,fixed.rating$CouponC,fixed.rating$Rating)
colorscheme = colorscheme1
fixed.rating = fixed.rating[with(fixed.rating,order(Rating)),]
p = captureplot()

if(PPT && chart["FIA30"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Coupon Contribution by Rating", 
                   slideVisual = p,
                   addType = "plot")    
}

#coupon contribution by ratin
layout(c(1,1))
fixed.rating = fixed.rating[with(fixed.rating,order(-CouponC)),]
colorscheme = colorscheme2
graph.contribution(fixed.rating,fixed.rating$CouponC,fixed.rating$Rating)
colorscheme = colorscheme1
fixed.rating = fixed.rating[with(fixed.rating,order(Rating)),]
p = captureplot()

if(PPT && chart["FIA31"]){
  pages = pages + 1
 
  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Coupon Contribution by Rating" , 
                   slideVisual = p,
                   addType = "plot")    
}


#Crosstabs

#MV
CrossTab = melt(fixeddata[,c(4,5,9,10)],id.var=2:4)
CrossTab = dcast(CrossTab,Rating+Draw~Sector,sum)
CrossTab = CrossTab[with(CrossTab,order(Draw)),]
CrossTab = CrossTab[,-2]
CrossTab[,-1] = 100*(CrossTab[,-1])/sum(CrossTab[,-1])
rownames(CrossTab) = CrossTab[,1]
CrossTab = CrossTab[,-1]
CrossTab["Total" ,] <- colSums(CrossTab)
CrossTab[,"Total"] <- rowSums(CrossTab)

layout(c(1,1))
textplot(format(round(CrossTab,digits=2),digits=2))

if(PPT && chart["FIA32"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Market Value Distribution", 
                   slideVisual = textplot(format(round(CrossTab,digits=2),digits=2)), 
                   addType = "plot")    
}


#Duration Contribution
CrossTab = melt(fixeddata[,c(11,5,9,10)],id.var=2:4)
CrossTab = dcast(CrossTab,Rating+Draw~Sector,sum)
CrossTab = CrossTab[with(CrossTab,order(Draw)),]
CrossTab = CrossTab[,-2]
rownames(CrossTab) = CrossTab[,1]
CrossTab = CrossTab[,-1]
CrossTab["Total" ,] <- colSums(CrossTab)
CrossTab[,"Total"] <- rowSums(CrossTab)

layout(c(1,1))
textplot(format(round(CrossTab,digits=3),digits=3))

if(PPT && chart["FIA33"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Duration Distribution", 
                   slideVisual = textplot(format(round(CrossTab,digits=3),digits=3)),
                   addType = "plot")    
}

#Yield Contribution
CrossTab = melt(fixeddata[,c(12,5,9,10)],id.var=2:4)
CrossTab = dcast(CrossTab,Rating+Draw~Sector,sum)
CrossTab = CrossTab[with(CrossTab,order(Draw)),]
CrossTab = CrossTab[,-2]
rownames(CrossTab) = CrossTab[,1]
CrossTab = CrossTab[,-1]
CrossTab["Total" ,] <- colSums(CrossTab)
CrossTab[,"Total"] <- rowSums(CrossTab)

layout(c(1,1))
textplot(format(round(CrossTab,digits=3),digits=3))

if(PPT && chart["FIA34"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Yield Distribution", 
                   slideVisual = textplot(format(round(CrossTab,digits=3),digits=3)), 
                   addType = "plot")    
}

#Coupon Contribution
CrossTab = melt(fixeddata[,c(13,5,9,10)],id.var=2:4)
CrossTab = dcast(CrossTab,Rating+Draw~Sector,sum)
CrossTab = CrossTab[with(CrossTab,order(Draw)),]
CrossTab = CrossTab[,-2]
rownames(CrossTab) = CrossTab[,1]
CrossTab = CrossTab[,-1]
CrossTab["Total" ,] <- colSums(CrossTab)
CrossTab[,"Total"] <- rowSums(CrossTab)

layout(c(1,1))
textplot(format(round(CrossTab,digits=3),digits=3))


if(PPT && chart["FIA35"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Coupon Distribution", 
                   slideVisual = textplot(format(round(CrossTab,digits=3),digits=3)),
                   addType = "plot")    
}
