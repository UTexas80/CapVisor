#Clear Enviornment
rm(list = ls(all = TRUE)) 
start.time = Sys.time()
# Required packages
require("ggplot2")
require("pbapply")
require("PerformanceAnalytics")
require("plotrix")
require("plyr")
require("R2PPT")
require("RDCOMClient")
require("reshape2")
require("scales")
require("sqldf")
require("tcltk2")
require("XML")
require("ReporteRs") # NEW 

#Set up diagnostics
code.version = "1.0.5"
code.ModDate = as.Date("2015-07-20")
fixedincome.version = "Not Called"
fixedincome.ModDate = "Not Called"
equity.version = "Not Called"
equity.ModDate = "Not Called"
performance.version = "Not Called"
performance.ModDate = "Not Called"

#File Information
EXISTS.FIXED.INCOME = FALSE
EXISTS.EQUITY = FALSE
header.fi = c("Manager","Identifier","Par","MV + Accrued","Rating","Duration","Yield","Coupon","Security Type")
header.eq = c("Manager","Identifier","Description","Quantity","Market Value","Dividend Yield","Sector")

# FUNCTIONS
#Load function to read Excel
source("https://gist.github.com/schaunwheeler/5825002/raw/3526a15b032c06392740e20b6c9a179add2cee49/xlsxToR.r")

#load Performance Functions
source("PerformanceFunctions.r")

#Pie function
pie.min = 1
pie.size = 6

lbl = NULL
lbl.y = NULL
lbl.fill = NULL
lbls.pct = NULL
lbls.y = NULL
lbls.show = NULL
lbls.fill = NULL

graph.pie = function(lbl,lbl.y,lbl.fill){
  
  #must set global variable as ggplot can't work in fucntion
  lbl <<- lbl
  lbl.y <<- lbl.y
  lbl.fill <<- lbl.fill
  
  lbls.pct <<- 100*lbl.y/sum(lbl.y)
  lbls.y <<- cumsum(lbl.y) - 0.5*lbl.y
  lbls.show <<- lbls.pct > pie.min
  lbls.pct <<- sprintf("%1.1f%%",lbls.pct)
  lbls.fill <<- lbl.fill
  
  if (sum(lbls.show) < length(lbl.fill)){
    lbls.fill[!lbls.show] <<- " "
    lbls.pct[!lbls.show] <<- " "
  }
  
  p = ggplot(lbl,aes(x=1,y=lbl.y,fill=lbl.fill))+
    geom_bar(stat="identity") + colorscheme + coord_polar(theta="y") +
    geom_text(show_guide=F,aes(label = lbls.pct, x = 1.25, y = lbls.y),size=pie.size) +
    geom_text(show_guide=F,aes(label = lbls.fill,x=1.5,y=lbls.y),size=pie.size) +
    theme.pie
  print(p)
  
}

#Stacked column
graph.contribution = function(lbl,lbl.y,lbl.fill){
  
  #must set global variable as ggplot can't work in fucntion
  lbl <<- lbl
  lbl.y <<- lbl.y
  lbl.fill <<- lbl.fill
  
  lbls.pct <<- 100*lbl.y/sum(lbl.y)
  lbls.y <<- cumsum(lbl.y) - 0.5*lbl.y
  lbls.show <<- lbls.pct > pie.min
  lbls.pct <<- sprintf("%1.3f",lbl.y)
  lbls.fill <<- lbl.fill
  
  if (sum(lbls.show) < length(lbl.fill)){
    lbls.fill[!lbls.show] <<- " "
    lbls.pct[!lbls.show] <<- " "
  }
  
  p = ggplot(lbl,aes(x=1,y=lbl.y,fill=lbl.fill))+
    geom_bar(stat="identity") + colorscheme  +
    ylab("")+
    geom_text(show_guide=F,aes(label = lbls.pct, x = 1.35, y = lbls.y),size=pie.size) +
    geom_text(show_guide=F,aes(label = lbls.fill, x = 0.9, y = lbls.y),size=pie.size) +
    theme.noframe.noticks.nolegend.notitle
  print(p)
  
}

# Spacer function for Table of Contents Function
# Adds appropriate .......
# FOR TOC
spacer = function(x){
  l = 35 - nchar(x)  #MAGIC NUMBER based on asthetics & trial/error
  tmp = "."
  for (i in 1:l){tmp = paste(tmp,".",sep="")}
  spacer = tmp
  spacer
}

#FOR Manager page
spacer2 = function(x){
  l = 12 - nchar(x)  # MAGIC NUMBER - Based on asthetics and trial&error
  tmp = ":"
  for (i in 1:l){tmp = paste(tmp," ",sep="")}
  spacer = tmp
  spacer
}


#Graph settings
# 72pts per Inch
# left, top, Width, height"
g.standard = c(50, 100, 650, 350)
g.pie = c(174,92,396,396)
g.text = c(50,100,350,350)
g.text1 = c(12,95,350,350)
g.text2 = c(365,95,350,350)

colorscheme1 = scale_fill_brewer(type="qual",palette = 3) # HERE
colorscheme = colorscheme1

# Themes for ggplot
# theme.noframe
# theme.noframe.xrotate
# theme.noframe.nolegend
# theme.noframe.nolegend.notitle
# theme.noframe.noticks.notitle
# theme.noframe.noticks.nolegend.notitle
# theme.pie

theme.noframe = theme(panel.background = element_rect(fill = 'transparent'),
                      legend.key=element_rect(fill='transparent'),
                      legend.title=element_blank(),
                      axis.text.x = element_text(color = 'black'), 
                      axis.text.y = element_text(color='black'),
                      axis.title.x = element_text(colour = 'black'),
                      axis.title.y = element_text(colour = 'black'))

theme.noframe.xrotate = theme(panel.background = element_rect(fill = 'transparent'),
                              legend.title=element_blank(),
                              legend.key=element_rect(fill='transparent'),
                              axis.text.x = element_text(color = 'black',angle=90), 
                              axis.text.y = element_text(color='black'),
                              #axis.title.x = element_blank(), --ET 1/16/18 Commented out to remove error, unsure of ramifications of change
                              axis.title.x = element_text(colour = 'black'),
                              axis.title.y = element_text(colour = 'black'))

theme.noframe.nolegend = theme(panel.background = element_rect(fill = 'transparent'),
                               legend.key=element_rect(fill='transparent'),
                               legend.title=element_blank(),
                               legend.position = 'none',   
                               axis.text.x = element_text(color = 'black'), 
                               axis.text.y = element_text(color='black'),
                               axis.title.x = element_text(colour = 'black'),
                               axis.title.y = element_text(colour = 'black'))

theme.noframe.nolegend.notitle = theme(panel.background = element_rect(fill = 'transparent'),
                                       legend.key=element_rect(fill='transparent'),
                                       legend.title=element_blank(),
                                       legend.position = 'none',   
                                       #axis.title.x=element_blank(), --ET 1/16/18 Commented out to remove error, unsure of ramifications of change
                                       axis.text.x = element_text(color = 'black'), 
                                       axis.text.y = element_text(color='black'),
                                       axis.title.x = element_text(colour = 'black'),
                                       axis.title.y = element_text(colour = 'black'))

theme.noframe.noticks.notitle = theme(panel.background = element_rect(fill = 'transparent'),
                                      legend.title=element_blank(),
                                      axis.title.x=element_blank(),
                                      axis.text.x=element_blank(),
                                      axis.ticks.x=element_blank(),
                                      axis.text.y = element_text(color='black'),
                                      axis.title.y = element_text(colour = 'black'))

theme.noframe.noticks.nolegend.notitle = theme(panel.background = element_rect(fill = 'transparent'),
                                               legend.title=element_blank(),
                                               legend.position = 'none',   
                                               axis.title.x=element_blank(),
                                               axis.text.x=element_blank(),
                                               axis.ticks.x=element_blank(),
                                               axis.text.y = element_text(color='black'),
                                               axis.title.y = element_text(colour = 'black'))

theme.pie = theme(panel.background = element_rect(fill = 'transparent'),
                  legend.title=element_blank(),legend.position = 'none', 
                  axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks=element_blank(),
                  axis.text.y = element_blank(),
                  axis.title.y = element_blank())

#Load Data
## MAGIC NUMBERS ## 
## File format shouldn't change !

data = xlsxToR("performance.xlsx",header=TRUE)
client = data[[1]]
mgr = data[[2]]
AUM = data[[3]]
performance = data[[4]]
risk.free.rate = data[[5]]
ChartSelection = data[[6]]

if (length(data) >= 7){
  if( length(colnames(data[[7]])) == length(header.fi)){
    fixeddata = data[[7]]
    EXISTS.FIXED.INCOME = TRUE
  }
  
  if(length(colnames(data[[7]])) == length(header.eq)){
    equitydata = data[[7]]
    EXISTS.EQUITY = TRUE
  }
}

if (length(data) >= 8){
  if( length(colnames(data[[8]])) == length(header.fi)){
    fixeddata = data[[8]]
    EXISTS.FIXED.INCOME = TRUE
  }
  
  if(length(colnames(data[[8]])) == length(header.eq)){
    equitydata = data[[8]]
    EXISTS.EQUITY = TRUE
  }
}

#error checking
if(nrow(performance) != nrow(AUM)){ stop("AUM & Performance have different lengths")}
if(nrow(risk.free.rate) != nrow(performance)){stop("Risk Free Rate & Performance have different lengths")}
if(nrow(mgr) != ncol(AUM)-1){stop("Number of Managers in AUM is different from number of Managers in Manager tab")}
if(nrow(mgr) != (ncol(performance)-1)/2){stop("Number of Managers in Performance is different from number of Managers in Manager tab")}

#manager names
mgr.name = mgr["Manager"]
num.mgr = nrow(mgr.name)

#Change date to standard r
risk.free.rate$Date = as.Date(risk.free.rate$Date,origin="1899-12-30")
AUM$Date = as.Date(AUM$Date,origin="1899-12-30")       
performance$Date = as.Date(performance$Date,origin="1899-12-30")
mgr$Inception = as.Date(mgr$Inception, origin="1899-12-30")

#Pull out dates
dates = AUM$Date     

# Replace NA with 0
# Need version of AUM & Performance w/ and w/o NA
AUM.Mod = AUM
AUM.Mod[is.na(AUM.Mod)] = 0                                   
performance.Mod = performance
performance.Mod[is.na(performance.Mod)] = 0

#Convert to numeric 
#Column 1 is date
AUM[-1] = sapply(AUM[-1],as.numeric)
AUM.Mod[-1] = sapply(AUM.Mod[-1],as.numeric)
performance[-1] = sapply(performance[-1],as.numeric)
performance.Mod[-1] = sapply(performance.Mod[-1],as.numeric)
risk.free.rate[-1] = sapply(risk.free.rate[-1],as.numeric)

#Make risk free a percentage
risk.free.rate[-1] = risk.free.rate[-1]/100

if (EXISTS.FIXED.INCOME){
  for (i in c(3,4,6,7,8)){  # MAGIC NUMBER - File format shouldn't change! See header.fi
    fixeddata[,i]=as.numeric(fixeddata[,i])
  }  
}

if (EXISTS.EQUITY){
  for (i in c(4,5,6)){  # MAGIC NUMBER - File format shouldn't change! See header.eq
    equitydata[,i]=as.numeric(equitydata[,i])
  }  
}

# Analysis & Charts to create
chart = ChartSelection[,7] #MAGIC NUMBER - FILE FORMAT SHOULDN'T CHANGE
chart = chart[-1] #Remove 1st row which is hidden is spreadsheet
chart = chart[!is.na(chart)]
chart = chart == "Print"

names = ChartSelection[,5] #MAGIC NUMBER - FILE FORMAT SHOULDN'T CHANGE
names = names[-1]
names = names[!is.na(names)]

chart = as.array(chart)
rownames(chart) = names

## MAGIC NUMBERS - FILE FORMAT SHOULDN'T CHANGE
analysis=ChartSelection[,3]
analysis = analysis[!is.na(analysis)]

names=ChartSelection[,1]
names = names[!is.na(names)]

analysis = as.array(analysis)
rownames(analysis) = names

if (analysis["AUM"] == "Yes") {AUM.GRAPHS = TRUE} else {AUM.GRAPHS = FALSE}
if (analysis["TPPA"] == "Yes") {TOTAL.PORTFOLIO = TRUE} else {TOTAL.PORTFOLIO = FALSE}
if (analysis["IPPA"] == "No") {
  INDIVIDUAL.PORTFOLIO = FALSE
  COMPOSITE.PORTFOLIO = FALSE
  SINGLE.PORTFOLIO = FALSE
} 
if (analysis["IPPA"] == "Composite Only") {
  INDIVIDUAL.PORTFOLIO = TRUE
  COMPOSITE.PORTFOLIO = TRUE
  SINGLE.PORTFOLIO = FALSE
} 
if (analysis["IPPA"] == "Individual Portfolios Only") {
  INDIVIDUAL.PORTFOLIO = TRUE
  COMPOSITE.PORTFOLIO = FALSE
  SINGLE.PORTFOLIO = TRUE
} 
if (analysis["IPPA"] == "Composite & Individual Portfolios") {
  INDIVIDUAL.PORTFOLIO = TRUE
  COMPOSITE.PORTFOLIO = TRUE
  SINGLE.PORTFOLIO = TRUE
} 
if (analysis["FIA"] == "Yes") {FIXED.ANALYSIS = TRUE} else {FIXED.ANALYSIS = FALSE}
if (analysis["EA"] == "Yes") {EQUITY.ANALYSIS = TRUE} else {EQUITY.ANALYSIS = FALSE}
if (analysis["PPT"] == "Yes") {PPT = TRUE} else {PPT = FALSE}
if (analysis["SAVE"] == "Yes") {SAVE.FILE = TRUE} else {SAVE.FILE = FALSE}
if (is.na(analysis["ICA"])) {INCLUDE.COMPOSITE = TRUE} else {  #for backward compatibility
  if(analysis["ICA"] == "Yes") {INCLUDE.COMPOSITE = TRUE} else {INCLUDE.COMPOSITE = FALSE}
}

#Append Performance w/ Compsoite Performance if there are multiple managers
if ((num.mgr > 1) && INCLUDE.COMPOSITE){
  
  TotalAUM = rowSums(AUM.Mod[-1])
  TotalAUM = data.frame(dates,TotalAUM)
  
  #first manager is column 2
  Composite_Portfolio =  AUM.Mod[,2]*performance.Mod[,2]
  Composite_Benchmark =  AUM.Mod[,2]*performance.Mod[,3]
  
  for (i in 2:num.mgr){
    Composite_Portfolio = Composite_Portfolio + AUM.Mod[,i+1]*performance.Mod[,i*2]
    Composite_Benchmark = Composite_Benchmark + AUM.Mod[,i+1]*performance.Mod[,i*2+1]  
  }  
  
  Composite_Portfolio = Composite_Portfolio/TotalAUM[,2]
  Composite_Benchmark = Composite_Benchmark/TotalAUM[,2]
  performance = data.frame(performance,Composite_Portfolio,Composite_Benchmark)
}


#Convert to zoo class
performance = read.zoo(performance)
risk.free.rate = read.zoo(risk.free.rate)

#Make XTS
performance = as.xts(performance)
risk.free.rate = as.xts(risk.free.rate)

#Active managers
Manager.active = AUM.Mod[nrow(AUM.Mod),-1] > 0

names = c(mgr[1,"Manager"],mgr[1,"Benchmark"])

if (num.mgr > 1) {
  for(i in 2:num.mgr){
    names = c(names,mgr[i,"Manager"],mgr[i,"Benchmark"])
  }
}

if ((num.mgr > 1) && INCLUDE.COMPOSITE) {
  names = c(names,"Composite Portfolio","Composite Benchmark")
}
colnames(performance) = names
colnames(risk.free.rate) = "RiskFree"

#Column selectors
mgr.col = NULL
for (i in 1:num.mgr){
  if(Manager.active[i]){mgr.col = c(mgr.col,(i-1)*2+1)}
}
if ((num.mgr > 1) && INCLUDE.COMPOSITE) {
  port.col = c(mgr.col,num.mgr*2+1)
} else {
  port.col = mgr.col
}
index.col = port.col + 1

#Create sets based on length of portfolio
num.quarters = nrow(performance)
one.year = NULL
three.year = NULL
five.year = NULL

for (i in port.col){
  if ((length(na.omit(performance[,i]))) >= 4) { one.year = c(one.year,i)}
  if ((length(na.omit(performance[,i]))) >= 12) { three.year = c(three.year,i)}
  if ((length(na.omit(performance[,i]))) >= 20) { five.year = c(five.year,i)}  
}

#Manager only list
if ((num.mgr > 1) && INCLUDE.COMPOSITE) {
  one.year.mgr = one.year[1:length(one.year)-1]
  three.year.mgr = three.year[1:length(three.year)-1]
  five.year.mgr = five.year[1:length(five.year)-1]
  
} else {
  one.year.mgr = one.year
  three.year.mgr = three.year
  five.year.mgr = five.year
}

Periods = c(1,3,5)
Portfolios = list(one.year,three.year,five.year)

#Initialize Table of Contents
toc = ""

### SETUP POWERPOINT
library(ReporteRs)

addSlideFunction = function(doc, slideType, slideTitle, slideVisual, addType){
  doc = addSlide(doc, slideType)
  doc = addTitle(doc, slideTitle)
  if(addType == "plot") doc = addPlot(doc, x = slideVisual, fun = print)
  if(addType == "flextable") doc = addFlexTable(doc, slideVisual)
}

captureplot = function() {p = invisible(recordPlot()); return(p)}

if (PPT) {
  mydoc <- pptx(template = 'template.pptx')
}

#Add title slide
pages = 3
if (PPT) { 

  mydoc = addSlide(mydoc, "Title Slide")
  mydoc = addTitle(mydoc, as.vector(client["Full Name"]$`Full Name`))
  mydoc = addSubtitle(mydoc, paste("as of",strftime(max(dates),format("%d %B %Y")),sep=" "))
 
  # set theme size for ggplot
  theme_set(theme_gray(base_size = 20))
}

