#Diagnostics
equity.version = "1.0.0"
equity.ModDate = "2011-01-09"

equitydata$DividendC = (100*equitydata$MV*equitydata$DY)/sum(equitydata$MV)
equitydf = sqldf("SELECT Sector,
                   sum(MV) as MV,
                   sum(100*MV*DY) as DY,
                   sum(DividendC) as DividendC
                   FROM equitydata
                   GROUP BY Sector")

equitydf$DY = equitydf$DY/equitydf$MV
AvgDivYield = sum(equitydata$DividendC)  


#Allocation
layout(c(1,1))
p = ggplot(equitydf,aes(x=Sector,y=MV/1000000,fill=Sector)) + 
  geom_bar(stat="identity") + colorscheme +
  geom_text(aes(label = round(MV/1000000,2)), vjust = -.5) +
  ylab("MV (Millions)") +
  theme.noframe.noticks.notitle
print(p)

if(PPT && chart["EA1"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Allocation by Sector", 
                   slideVisual = p,
                   addType = "plot")   
}

# Allocation by Sector
layout(c(1,1))
graph.pie(equitydf,equitydf$MV,equitydf$Sector)
p = captureplot()

if(PPT && chart["EA2"]){
  pages = pages + 1
 
  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Allocation by Sector", 
                   slideVisual = p,
                   addType = "plot")   
}

#Histogram
x = weighted.hist(100*equitydata$DY,equitydata$MV,plot=F)
Dividend.hist = as.data.frame(cbind(x$mids,x$density))

layout(c(1,1))
p = ggplot(Dividend.hist,aes(x=V1,y=V2))+
  geom_bar(stat="identity",fill="grey") + 
  geom_text(aes(label = round(V2,2)), vjust = -.5) +
  ylab("Percentage") +
  xlab("Dividend Yield") + colorscheme + scale_y_continuous(labels = percent_format())+
  geom_vline(aes(xintercept = AvgDivYield,color="red")) +
  annotate("text", x = AvgDivYield, y = 0.9*max(Dividend.hist$V2) ,label = paste("Avg Div Yld = ",sprintf("%.3f", AvgDivYield))) +
  theme.noframe.nolegend
print(p)

if(PPT && chart["EA3"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Distribution of Dividend Yield", 
                   slideVisual = p,
                   addType = "plot")   
}

#Dividend Yield by Sector
layout(c(1,1))
p = ggplot(equitydf,aes(x=Sector,y=DY,fill=Sector))+
  geom_bar(stat="identity") + 
  geom_text(aes(label = round(DY,2)), vjust = -.5) +
  ylab("Dividend Yield") + colorscheme +
  geom_hline(aes(yintercept = AvgDivYield,color="red")) +
  annotate("text", x = nrow(equitydf)/2, y = AvgDivYield+0.1 ,label = paste("Avg Div Yld = ",sprintf("%.3f", AvgDivYield))) +
  theme.noframe.noticks.notitle
print(p)

if(PPT && chart["EA4"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Dividend Yield by Sector", 
                   slideVisual = p,
                   addType = "plot")   
}

#Dividend contribution by sector
layout(c(1,1))
p = ggplot(equitydf,aes(x=Sector,y=DividendC,fill=Sector))+
  geom_bar(stat="identity") + colorscheme +
  geom_text(aes(label = round(DividendC,2)), vjust = -.5) +
  ylab("Dividend Yield Contribution") +
  theme.noframe.noticks.notitle
print(p)

if(PPT && chart["EA5"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Dividend Contribution by Sector", 
                   slideVisual = p,
                   addType = "plot")   
}

#Dividend Yield Contributio by Sector
layout(c(1,1))
graph.pie(equitydf,equitydf$MV,equitydf$Sector)
graph.pie(equitydf,equitydf$DividendC,equitydf$Sector)
p = captureplot()

if(PPT && chart["EA6"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Dividend Contribution by Sector", 
                   slideVisual = p,
                   addType = "plot")   
}

#Dividend Yield Contribution by sector
equitydf = equitydf[with(equitydf,order(-DividendC)),]
graph.contribution(equitydf,equitydf$DividendC,equitydf$Sector)
equitydf = equitydf[with(equitydf,order(Sector)),]
p = captureplot()

if(PPT && chart["EA7"]){
  pages = pages + 1

  addSlideFunction(doc = mydoc, slideType="Title and Content",
                   slideTitle = "Dividend Contribution by Sector", 
                   slideVisual = p,
                   addType = "plot")   
}


