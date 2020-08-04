## sp and rgdal are used to assign and transform Coordinate Reference Systems in R
library(rgdal)
library(sp)


## In R, the notation used to describe the CRS is proj4string from the
## PROJ.4 library.
## Some spatial data files have associated projection data, such as
## ESRI shapefiles.  When readOGRis used to import these data this
## information is automatically linked to the R spatial object. To
## retrieve the CRS for a spatial object use proj4string

## Map of scotland from shapefiles and its CRS
## Note: make sure shapefiles are all in working directory
scotland     <- readOGR("District_16_1.shp",verbose=TRUE)  #or street postcode shapefile
coordinates(scotland)
plot(scotland)
proj4string(scotland)


## There are various attributes of the CRS, such as the projection,
## datum, and ellipsoid. Some of the options for each variable can be
## obtained in R with projInfo
projInfo(type =  "proj")
projInfo(type = "datum")
projInfo(type  = "ellps")



##Defining a coordinate system can be done explicitly through function CRS in R package sp, for example
## 
new.CRS      <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs +ellps=airy
                +towgs84=446.448,-125.157,542.060,0.1502,0.2470,0.8421,-20.4894")
new.CRS

## To assign a known CRS to spatial data (say x)
ukgrid  <- "+init=epsg:27700"
longlat <- "+init=epsg:4326"
proj4string(scotland) <- CRS(ukgrid)


## To transform from one CRS to another 
scotland_latlong <- spTransform(scotland, latlong)
coordinates(scotland_latlong)

ukgrid  <- "+init=epsg:27700"
longlat <- "+init=epsg:4326"
coordinates(leadpipes)<-c("Easting","Northing")
proj4string(leadpipes)<-ukgrid
data_longlat<-spTransform(leadpipes,longlat)

plot(scotland_latlong,axes=TRUE)
plot(data_longlat,pch=20,col=2,add=TRUE)

leadpipes<-read.csv("C:/Users/xuh/Desktop/毕业论文/苏格兰水/LeadPipes.csv")
df<-as.data.frame(leadpipes)
df[,c("long","lat")] <- coordinates(data_longlat)
#draw the plot based on leadproportion

library(leaflet)
pal <- colorBin("viridis", bins = c(0, 0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
leaflet(df) %>%  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(lng = ~long, lat = ~lat, color = ~pal(LeadProportion)) %>%
  addLegend("bottomright", pal = pal, values = ~LeadProportion, title = "Prevalence") %>%
  addScaleBar(position = c("bottomleft"))

#EDA
#missing data check
#showing the number of missings
library(zCompositions)
library('naniar')
vis_miss(df)
#there is no missing data
#correlation check
library(corrplot)
library(ggplot2)
cov_df=df[c('BuiltAfter1970','CensusHouseholdCount2011'
                                        ,'Phosphorus','MeanHouseAge'
                                        ,'BuildYearRounded','MedianHouseAge'
                                        ,'LeadMeasurement','LeadPresence','TotalPoints','LeadProportion')]

corr_df=cor(cov_df,use='complete.obs')
corrplot(corr =corr_df,order="AOE",type="upper",tl.pos="tp")
corrplot(corr = corr_df,add=TRUE, type="lower", method="number",order="AOE", col="black",diag=FALSE,tl.pos="n", cl.pos="n")
#mean house age, median house age, build after 1970, build year rounded 
#censushouseholdcount2011 and total points
#As a result, we choose buildafter1970 out of the four variables, and remove censushouseholdcount2011 from the model
corr_df1<-as.data.frame(corr_df)
dist1=ggplot(corr_df1, aes(x=unlist(corr_df1[,1]))) + 
  geom_histogram(aes(y=..density..),      
                 binwidth=0.1,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")+
  theme_bw()+
  labs(x='BuiltAfter1970',y="")
#for categorical data ReplacedPipes,WOAName,UrbanRural,Region
cate_cov=df[c('ReplacedPipes','WOAName','UrbanRural','Region')]
df2<-df
df2$ReplacedPipes<-as.factor(df2$ReplacedPipes)
df2$UrbanRural<-as.factor(df2$UrbanRural)
#boxplot and one way ANOVA to see the correlation between lead proportion and categorical variables
ggplot(df2, aes(x=ReplacedPipes, y=LeadProportion)) + 
  geom_boxplot()
ggplot(df2, aes(x=Region, y=LeadProportion)) + 
  geom_boxplot()
#test the correlation between ReplacedPipes and UrbanRural
#anova can not be used since 
tbl = matrix(data=c(20, 35, 19,14,8,45,44,89,0,4,1,4,0,9,12,4), nrow=2, ncol=8, byrow=T) 
dimnames(tbl) = list(ReplacedPipes=c('0', '1'), UrbanRural=c('1', '2','3','4','5','6','7','8')) 
chi2 = chisq.test(tbl, correct=F) 
c(chi2$statistic, chi2$p.value)
sqrt(chi2$statistic / sum(tbl)) 
#Since the p-value is small, we can see that the correlation is not that great
tbl = matrix(data=c(1, 85, 36,80,32,40,0,7,2,25,0,0), nrow=2, ncol=6, byrow=T) 
dimnames(tbl) = list(ReplacedPipes=c('0', '1'), Region=c('AYR', 'DON','FORTH','NITH','TAY','TWEED')) 
chi2 = chisq.test(tbl, correct=F) 
c(chi2$statistic, chi2$p.value)
sqrt(chi2$statistic / sum(tbl)) 
#3
tbl = matrix(data=c(0,0,0,0,6,14,0,3,6,21,3,6,0,3,0,7,3,7,0,0,0,15,1,2,0,8,0,0,0,0,0,5,1,35,4,9
                    ,0,6,13,27,8,2,
                    1,67,18,0,7,0), nrow=8, ncol=6, byrow=T) 
dimnames(tbl) = list(ReplacedPipes=c('1', '2','3','4','5','6','7','8'), Region=c('AYR', 'DON','FORTH','NITH','TAY','TWEED')) 
chi2 = chisq.test(tbl, correct=F) 
c(chi2$statistic, chi2$p.value)
sqrt(chi2$statistic / sum(tbl)) 
#reject the null hypothesis, leadproportion varies between different levels of replacedpipes
#similarly, we do the following analysis.
ggplot(df2, aes(x=UrbanRural, y=LeadProportion)) + 
  geom_boxplot()

# Based on the data exploration above, we choose phosphorus,builtafter1970,ReplacedPipes
#as fixed effects, and build spde model. 

#build mesh
library(INLA)
library(geoR)

coo <- cbind(df$long, df$lat)
mesh <- inla.mesh.2d(loc = coo, max.edge = c(0.1, 5), cutoff = 0.1)
plot(mesh)
points(coo, col = "red")

#build the SPDE model on mesh
spde <- inla.spde2.matern(mesh = mesh, alpha = 2)
indexs <- inla.spde.make.index("s", spde$n.spde)
lengths(indexs)
#build projector matrix
A <- inla.spde.make.A(mesh = mesh, loc = coo)
library(raster)
#use the full model first
#model check——choose the true distribution
#stack for estimation stk.e
stk.e <- inla.stack(tag = "est",
                    data = list(y = df$LeadPresence, numtrials = df$TotalPoints),
                    A = list(1, A),
                    effects = list(data.frame(b0 = 1, cov1 = df$Phosphorus, 
                                              cov2=df$BuiltAfter1970,cov3=df$ReplacedPipes), s = indexs))

#stack for prediction stk.p
stk.p <- inla.stack(tag = "pred",
                    data = list(y = NA, numtrials = NA),
                    A = list(1, A),
                    effects = list(data.frame(b0 = 1, cov1 = df$Phosphorus, 
                                              cov2=df$BuiltAfter1970,cov3=df$ReplacedPipes), s = indexs))

#stk.full has stk.e and stk.p
stk.full <- inla.stack(stk.e, stk.p)
#run inla
formula <- y ~ 0 + b0 + cov1 +cov2+ cov3+ f(s, model = spde)
res <- inla(formula, family = "binomial", Ntrials = numtrials,
            control.family = list(link = "logit"),
            data = inla.stack.data(stk.full),
            control.predictor = list(compute = TRUE, link = 1, A = inla.stack.A(stk.full)))
#results,注意inla不支持factor
summary(res)

res$summary.fitted.values
#画参数的后验density
##draw the density of alpha
library(ggplot2)
alpha <- res$marginals.fixed[[1]]
ggplot(data.frame(inla.smarginal(alpha)), aes(x, y)) +
  geom_line() +
  theme_bw()
##draw the density of beta1
beta1 <- res$marginals.fixed[[2]]
ggplot(data.frame(inla.smarginal(beta1)), aes(x, y)) +
  geom_line() +
  theme_bw()
#it shows that cov2 is not that fittable.
#show the cpo for each datapoints
n = nrow(df)
plot(1:n,na.omit(res1$cpo$cpo), ylab="CPO",type="n")
text(1:n,na.omit(res1$cpo$cpo), 1:n)
#可以用来分析的图形
plot(res1,1)

#the pits against predictor variables
pit<-na.omit(res1$cpo$pit)
plot(df$LeadProportion, pit, xlab="LeadProportion", ylab="PIT")

#map the predict data
index <- inla.stack.index(stack = stk.full, tag = "pred")$data
prev_mean <- res$summary.fitted.values[index, "mean"]
prev_ll <- res$summary.fitted.values[index, "0.025quant"]
prev_ul <- res$summary.fitted.values[index, "0.975quant"]

df1<-df
df1[,"prev_mean"]<-prev_mean


leaflet(df1) %>%  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(lng = ~long, lat = ~lat, color = ~pal(prev_mean)) %>%
  addLegend("bottomright", pal = pal, values = ~prev_mean, title = "Prevalence") %>%
  addScaleBar(position = c("bottomleft"))
#measurement
formula <- y ~ 0 + b0 + cov1 +cov2+ cov3+ f(s, model = spde)
res1 <- inla(formula, family = "binomial", Ntrials = numtrials,
            control.family = list(link = "logit"),
            data = inla.stack.data(stk.full),
            control.predictor = list(compute = TRUE, link = 1, A = inla.stack.A(stk.full)),
            control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE))
formula1<-y ~ 0 + b0 +cov2+ cov3+ f(s, model = spde)
res1.1<-inla(formula1,family = "binomial", Ntrials = numtrials,
           control.family = list(link = "logit"),
           data = inla.stack.data(stk.full),
           control.predictor = list(compute = TRUE, link = 1, A = inla.stack.A(stk.full)),
           control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE))
##In model3, we use lead presence as response variable.
#stack for estimation stk.e
stk.e1 <- inla.stack(tag = "est",
                    data = list(y = df$LeadPresence),
                    A = list(1, A),
                    effects = list(data.frame(b0 = 1, cov1 = df$Phosphorus, 
                                              cov2=df$BuiltAfter1970,cov3=df$ReplacedPipes,cov4=df$TotalPoints), s = indexs))

#stack for prediction stk.p
stk.p1 <- inla.stack(tag = "pred",
                    data = list(y = NA),
                    A = list(1, A),
                    effects = list(data.frame(b0 = 1, cov1 = df$Phosphorus, 
                                              cov2=df$BuiltAfter1970,cov3=df$ReplacedPipes,cov4=df$TotalPoints), s = indexs))
stk.full1 <- inla.stack(stk.e1, stk.p1)
formula3<-y ~ 0 + b0 +cov1+cov2+ cov3+ offset(log(cov4))+f(s, model = spde)
res2<-inla(formula3,family = "poisson",
           control.family = list(link = "log"),
           data = inla.stack.data(stk.full1),
           control.predictor = list(compute = TRUE, link = 1, A = inla.stack.A(stk.full1)),
           control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE))
summary(res2)
beta1 <- res2$marginals.fixed[[2]]
ggplot(data.frame(inla.smarginal(beta1)), aes(x, y)) +
  geom_line() +
  theme_bw()
formula4<-y ~ 0 + b0 +cov2+ cov3+ offset(log(cov4))+f(s, model = spde)
res2.1<-inla(formula4,family = "poisson",
             control.family = list(link = "log"),
             data = inla.stack.data(stk.full1),
             control.predictor = list(compute = TRUE, link = 1, A = inla.stack.A(stk.full1)),
             control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE))
#利用region进行建模
res3<-inla(LeadPresence~0+1+Phosphorus+BuiltAfter1970+ReplacedPipes+offset(log(TotalPoints))+f(Region,model="iid"), family="poisson",data=df,control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE))
res3.1<-inla(LeadPresence~0+1+BuiltAfter1970+ReplacedPipes+offset(log(TotalPoints))+f(Region,model="iid"), family="poisson",data=df,control.compute = list(cpo = TRUE, dic = TRUE, waic = TRUE))
summary(res3)
beta1 <- res3$marginals.fixed[[2]]
ggplot(data.frame(inla.smarginal(beta1)), aes(x, y)) +
  geom_line() +
  theme_bw()
# we summarise the cpo
measure1<--sum(log(na.omit(res1$cpo$cpo)))#the smaller the value, the better the model fits
measure1.1<--sum(log(na.omit(res1.1$cpo$cpo)))
measure2<--sum(log(na.omit(res2$cpo$cpo)))
measure2.1<--sum(log(na.omit(res2.1$cpo$cpo)))
measure3<--sum(log(na.omit(res3$cpo$cpo)))
measure3.1<--sum(log(na.omit(res3.1$cpo$cpo)))
#model 1 performs the best

plot(ypostmean1[,1],ypostmean2[,1], xlab="Binomial", ylab="LM fitted values")
abline(0,1)
#model select method,use cpo to select
cpo.vec<-(c(measure1,measure1.1,measure2,measure2.1,measure3,measure3.1))
plot(cpo.vec, ylab="cpo", xlab="model")
which.min(cpo.vec) 
#show DICs
DIC1<-492.52
DIC1.1<-491.61
DIC2<-501.33
DIC2.1<-500.38
DIC3<-539.39
DIC3.1<-540.13
#show WAICs
WAIC1<-504.97
WAIC1.1<-503.92
WAIC2<-506.02
WAIC2.1<-504.76
WAIC3<-543.67
WAIC3.1<-543.80

dic.vec<-(c(DIC1,DIC1.1,DIC2,DIC2.1,DIC3,DIC3.1))
plot(dic.vec, ylab="dic", xlab="model")
which.min(dic.vec) 

waic.vec<-(c(WAIC1,WAIC1.1,WAIC2,WAIC2.1,WAIC3,WAIC3.1))
plot(waic.vec, ylab="waic", xlab="model")
which.min(waic.vec) 
#model analysis
beta1 <- res1.1$marginals.fixed[[2]]
ggplot(data.frame(inla.smarginal(beta1)), aes(x, y)) +
  geom_line() +
  theme_bw()
beta2 <- res1.1$marginals.fixed[[3]]
ggplot(data.frame(inla.smarginal(beta2)), aes(x, y)) +
  geom_line() +
  theme_bw()
#show the predicted results of true model
#map the predict data
index <- inla.stack.index(stack = stk.full, tag = "pred")$data
prev_mean <- res1.1$summary.fitted.values[index, "mean"]
prev_ll <- res1.1$summary.fitted.values[index, "0.025quant"]
prev_ul <- res1.1$summary.fitted.values[index, "0.975quant"]

#mean prevalence
df1<-df
df1[,"prev_mean"]<-prev_mean

pal <- colorBin("viridis", bins = c(0, 0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
leaflet(df1) %>%  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(lng = ~long, lat = ~lat, color = ~pal(prev_mean)) %>%
  addLegend("bottomright", pal = pal, values = ~prev_mean, title = "Prevalence") %>%
  addScaleBar(position = c("bottomleft"))
#0.025 prevalence
df1<-df
df1[,"prev_ll"]<-prev_ll

pal <- colorBin("viridis", bins = c(0, 0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
leaflet(df1) %>%  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(lng = ~long, lat = ~lat, color = ~pal(prev_ll)) %>%
  addLegend("bottomright", pal = pal, values = ~prev_ll, title = "Prevalence") %>%
  addScaleBar(position = c("bottomleft"))
#0.975 prevalence
df1<-df
df1[,"prev_ul"]<-prev_ul

pal <- colorBin("viridis", bins = c(0, 0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
leaflet(df1) %>%  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(lng = ~long, lat = ~lat, color = ~pal(prev_ul)) %>%
  addLegend("bottomright", pal = pal, values = ~prev_ul, title = "Prevalence") %>%
  addScaleBar(position = c("bottomleft"))
#plot density
fitted <- res1.1$summary.fitted.values[1,]