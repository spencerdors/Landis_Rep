'''
author: Spencer Dorsey
Replication of "Temperature seasonality and violent conflict:
The inconsistencies of a warming planet" by Steven T Landis,
started: 27 March 2015
'''

rm(list=ls()) 
setwd("~/OneDrive/MLE/Replication/")
set.seed(61384)

duke_blue = '#001A57'
duke_lightBlue = '#0736A4'
duke_lighterBlue = '#0680CD'
green_light = '#728302'
green_dark = '#A1B70D'
brown = '#7B4500'
orange = '#F09905'
yellow = '#FFD960'
purple = '#4D005E'
dark_gray = '#666666'
light_gray = '#B5B5B5'




# Function to load packages
loadPkg=function(toLoad){
  for(lib in toLoad){
	if(! lib %in% installed.packages()[,1])
	  { install.packages(lib, repos='http://cran.rstudio.com/') }
	suppressMessages( library(lib, character.only=TRUE) ) }
}

# Load libraries
packs=c('foreign', 'MASS', 'ggplot2', 'reshape2', 'countrycode',
 'Zelig', 'multcomp', 'scales', 'sbgcop', 'pscl', 'CRISP')
loadPkg(packs)

d_ICEWS = read.csv('DATA.csv')

d_ICEWS$X = NULL

dv='icews_violence'
ivs=c('icews_lag', 'temp_mean', 'posDev', 'posDev_sq', 'negDev', 'negDev_sq', 
	'precip_mean', 'posPreDev', 'posPreDev_sq', 'negPreDev', 'negPreDev_sq', 
	'DEMOC_lag', 'gdp_pc_lag', 'population_lag', 'El_Nino')
modForm=formula(paste0( dv, ' ~ ', paste(ivs, collapse=' + ') ))

########################################################################
#Simulation setup
tempRange = seq(min(d_ICEWS$temp_mean), max(d_ICEWS$temp_mean), length.out=100)
tempRange = quantile(d_ICEWS$temp_mean, probs=c(0.25, 0.75))
scen = with(data=d_ICEWS, cbind(1,
mean(icews_lag)
,tempRange
,mean(posDev)     
,mean(posDev_sq)         
,mean(negDev)
,mean(negDev_sq)
,mean(precip_mean)
,mean(posPreDev)         
,mean(posPreDev_sq)
,mean(negPreDev)  
,mean(negPreDev_sq)      
,mean(DEMOC_lag)
,mean(gdp_pc_lag) 
,mean(population_lag)    
,median(El_Nino) #this one is median because it is binary
))


mod = glm.nb(modForm, data = d_ICEWS)



##### Negative Binomial
### Varying Democratic Sponsors
# Simulate additional parameter estimates from multivariate normal
sims=1000
draws = mvrnorm(sims, coef(mod), vcov(mod))
preds = draws %*% t(scen)
preds = exp(preds)
# preds = append(preds[,1], preds[,2])
# preds = as.data.frame(cbind(preds, c(rep(1, 1000), rep(2, 1000))))
# colnames(preds) = c('predicted', 'scen')

colnames(preds)=1:ncol(preds)
modelExp2=melt(preds)[,-1]
ggMeans = ddply(modelExp2, .(Var2), summarise, sMean=mean(value))
ggDensity = ddply(modelExp2, .(Var2), .fun=function(x){
  tmp = density(x$value); x1 = tmp$x; y1 = tmp$y
  q95 = x1 >= quantile(x$value,0.025) & x1 <= quantile(x$value,0.975)
  q90 = x1 >= quantile(x$value,0.05) & x1 <= quantile(x$value,0.95)
  data.frame(x=x1,y=y1,q95=q95, q90=q90) } )

ggMeans$Var2 = as.factor(ggMeans$Var2)
ggDensity$Var2 = as.factor(ggDensity$Var2)

temp = ggplot()
temp = temp + geom_line(data=ggDensity, aes(x=x,y=y,color=Var2))
temp = temp + geom_vline(data=ggMeans,
  aes(xintercept=sMean, color=Var2),linetype='solid',size=1)
temp = temp + geom_ribbon(data=subset(ggDensity,q95),
  aes(x=x,ymax=y,fill=Var2),ymin=0,alpha=0.5)
temp = temp + geom_ribbon(data=subset(ggDensity,q90),
  aes(x=x,ymax=y,fill=Var2),ymin=0,alpha=0.9)
temp = temp + theme(legend.position='none')
temp = temp  + scale_fill_manual(breaks = c(1,2), values = c(orange, duke_lighterBlue))
temp = temp + scale_colour_manual(breaks = c(1,2), values = c(orange, duke_lighterBlue))
temp = temp + theme(panel.background=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
temp = temp + xlab('') + ylab('')
temp
ggsave('substantive.png', temp, height = 8, width = 9)



########################################################################
#time-series visualization

dat = d_ICEWS[ , which(names(d_ICEWS) %in% c('date', 'icews_violence', 'country'))]
dat$year = lubridate::year(dat$date)
dat$date = NULL

sumDat = data.frame(matrix(ncol = 2, nrow = length(unique(dat$country))*length(unique(dat$year))))
colnames(sumDat) <- c('country', 'year')
sumDat$year = rep(unique(dat$year), length(unique(dat$country)))
sumDat$country = rep(unique(dat$country), length(unique(dat$year)))

hold = aggregate(dat$icews_violence, by = list(dat$country, dat$year), FUN=sum)
colnames(hold) = c('country', 'year', 'Violence')

sumDat = merge(sumDat, hold, by=c('country', 'year'))

sumDatW <- reshape(sumDat, 
  timevar = "country",
  idvar = c("year"),
  direction = "wide")

names = sort(c('Russian Federation',
'China',
'Mongolia',
'Korea, Dem. Rep.',
'Korea, Rep.',   
'Japan', 
'India',
'Bangladesh',
'Myanmar',
'Sri Lanka',
'Nepal',
'Thailand',
'Cambodia',
'Lao PDR',
'Malaysia',
'Philippines',
'Indonesia',
'Australia',
'Papua New Guinea',
'New Zealand'
))

UN = countrycode(names, 'country.name', 'iso3c')

colnames(sumDatW) = c('step', names)

sumDatW2 = sumDatW[1:8,]
sumDatW3 = sumDatW[9:13,]

colnames(sumDatW2) = c('step', UN)
colnames(sumDatW3) = c('step', UN)

require(devtools)
source_gist('5281518')
 
par(mar=numeric(4),family='serif')
plot.qual(sumDatW2,rs.ln=c(3,15))
plot.qual(sumDatW3,rs.ln=c(3,15))


########################################################################
#Cross-validation and Prediction

#Creating d_ICEWS_2 for scaling small coefficients 
d_ICEWS_2 = d_ICEWS
d_ICEWS_2$temp_mean = d_ICEWS_2$temp_mean/50
d_ICEWS_2$precip_mean = d_ICEWS_2$precip_mean/50
d_ICEWS_2$icews_lag = d_ICEWS_2$icews_lag/50
d_ICEWS_2$gdp_pc_lag = d_ICEWS_2$gdp_pc_lag/25
d_ICEWS_2$DEMOC_lag = d_ICEWS_2$DEMOC_lag/50

for(ii in unique(d_ICEWS_2$fold)){
  #set to train and test
  train = d_ICEWS_2[d_ICEWS_2$fold != ii,]
  test= d_ICEWS_2[d_ICEWS_2$fold == ii,]
  #get coefficients
  model=glm.nb(data=train, modForm)
  trainRes=cbind(c('(Intercept)', ivs), summary (model )$'coefficients'[,1:2], ii)
  coefCrossVal=rbind(coefCrossVal, trainRes)
  #get performance
  # preds=exp(trainRes[,1] %*% t(data.matrix(cbind(1, test[,ivs]))) )
  preds = predict.glm(model, newdata=test, type='response')
  perf=c(perf, rmse(preds, test$icews_violence) )
}

#organize data:
ggData=data.frame(coefCrossVal, row.names=NULL)
colnames(ggData)=c('Variable','Coefficient','SE','modelName')
ggData$Coefficient = as.numeric(levels(ggData$Coefficient))[ggData$Coefficient]
ggData$SE = as.numeric(levels(ggData$SE))[ggData$SE]

#test getting rid of intercept
ggData = ggData[ggData$Variable != '(Intercept)', ]

# Specify the width of your confidence intervals
interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

zp2 <- ggplot(ggData, aes(colour = modelName))
zp2 <- zp2 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
zp2 <- zp2 + geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                                ymax = Coefficient + SE*interval1),
                            lwd = 1, position = position_dodge(width = 1/2))
zp2 <- zp2 + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                                 ymax = Coefficient + SE*interval2),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE")
zp2 <- zp2 + coord_flip() + theme_bw() + scale_x_discrete(limits = rev(ivs), breaks = ivs, labels = labels2)
zp2 <- zp2 + scale_colour_manual(values = c(orange, light_gray, yellow, duke_lighterBlue), name = "Fold", labels = c('2002-2004', '2005-2007', '2008-2010', '2011-2014'))
zp2 <- zp2 + theme(legend.position = 'top',  panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
ggsave("~/OneDrive/MLE/Replication/Poster/coefPlot.png", zp2, width = 8, height = 7, dpi = 300)

#################################################
#predictions for each country
duke_blue = '#001A57'
duke_lightBlue = '#0736A4'
duke_lighterBlue = '#0680CD'
green_light = '#728302'
green_dark = '#A1B70D'
brown = '#7B4500'
orange2 = '#F09905'
yellow = '#FFD960'
purple = '#4D005E'
dark_gray = '#666666'
light_gray = '#B5B5B5'


countrycodes = unique(d_ICEWS$ccode)
countrycodes = countrycodes[countrycodes != 710]
countrycodes = countrycodes[countrycodes != 775] #These  wouldn't graph
countrycodes = countrycodes[countrycodes != 812]
countrycodes = countrycodes[countrycodes != 811]



for(ii in countrycodes){
  dat = d_ICEWS[d_ICEWS$ccode == ii, ]
  train = dat[which(dat$year < 2010),]
  test = dat[which(dat$year >= 2010),]
  mod = glm.nb(modForm , data=train)
  mod2 = glm.nb(modForm2, data = train)
  # Generated predicted probabilites from train set
  predIn <- predict.glm(mod, type='response')
  predIn2 = predict.glm(mod2, tpye = 'response')

  # Generate predicted probabilities for test set
  predOut <- predict.glm(mod, newdata=test, type='response')
  predOut2 = predict.glm(mod2, newdata=test, type = 'response')

  # Plot train predicted versus actual
  ggData=cbind( train[,c('date','icews_violence')], predIn, predIn2)
  ggData=melt(ggData, id='date')
  maxVal = max(ggData$value)
  ggData$date = as.Date(ggData$date)
  tmp=ggplot(ggData, aes(x=date, y=value, color=variable, size = variable, group = variable)) + geom_line()
  tmp = tmp + scale_colour_manual(breaks = c('icews_violence', 'predIn', 'predIn2'),
   labels=c('Actual Violence', 'In-Sample Predictions', 'Predictions w/out Climate'),
                  values = c(duke_lighterBlue, orange2, yellow))
  tmp = tmp + scale_size_manual(breaks = c('icews_violence', 'predIn', 'predIn2'), 
    values = c(.5,1, .5), guide=FALSE)
  tmp=tmp+ylab('Violent Events')+xlab('Date')  + theme_bw()  + theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
  tmp=tmp + scale_y_continuous(limits=c(0, maxVal))
  ggsave(filename=paste0( ii, 'train', '.png'), plot=tmp)
  print('after 1')

  # Plot test predicted versus actual
  # Plot test predicted versus actual
  ggData=cbind( test[,c('date','icews_violence')], predOut, predOut2)
  ggData=melt(ggData, id='date')
  ggData$date = as.Date(ggData$date)
  ymp=ggplot(ggData, aes(x=date, y=value, color=variable, size = variable, group = variable)) + geom_line()
  ymp = ymp + scale_colour_manual(breaks = c('icews_violence', 'predOut', 'predOut2'),
   labels=c('Actual Violence', 'Out-Sample Predictions', 'Predictions w/out Climate'),
                  values = c(duke_lighterBlue, orange2, yellow), guide=FALSE)
  ymp = ymp + scale_size_manual(breaks = c('icews_violence', 'predOut', 'predOut2'),
   values = c(.5,1, .5))
  ymp=ymp+ylab('Violent Events')+xlab('Date') +theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
  ymp=ymp + scale_y_continuous(limits=c(0,maxVal))
  ymp
  ggsave(filename=paste0( ii, 'test,', '.png'), plot=ymp) #This builds the predictive images
  print('end')
}