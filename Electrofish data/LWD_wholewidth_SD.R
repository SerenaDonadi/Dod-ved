rm(list=ls())
setwd("C:/Users/sedi0002/Google Drive/Dod ved/Electrofish data")
setwd("C:/Users/serena/Google Drive/Dod ved/Electrofish data")
my<-read.table("LWD_wholewidth_SD3.txt",header=T)

head(my)
str(my)
summary(my)


# Libraries ---------------------------------------------------------------
library(ggplot2)
library(lattice)
library(nlme)
library(MASS)
library(piecewiseSEM)
library(lme4)
library(car)

# Defining variables type: -----------------------------------------------------

# convert to categorical variables:my$DateYYYYMMDD<-as.factor(my$DateYYYYMMDD)

my$Catchment_number<-as.factor(my$Catchment_number)
my$Whole_width_fished<-as.factor(my$Whole_width_fished)
my$DateYYYYMMDD<-as.factor(my$DateYYYYMMDD)

# binary variables, to convert or not into factors:
my$Abbor_KLASS<-as.factor(my$Abbor_KLASS)
my$Elrit_KLASS<-as.factor(my$Elrit_KLASS)
my$GEdda_KLASS<-as.factor(my$GEdda_KLASS)
my$Harr_KLASS<-as.factor(my$Harr_KLASS)
my$Lake_KLASS<-as.factor(my$Lake_KLASS)
my$Lax_KLASS<-as.factor(my$Lax_KLASS)
my$Eel_KLASS<-as.factor(my$Eel_KLASS)
my$Mort_KLASS<-as.factor(my$Mort_KLASS)
my$Voltage<-as.factor(my$Voltage)

# ordinal variables, to conver or not:
my$Catchment_area_class<-as.factor(my$Catchment_area_class)
my$SUB1<-as.factor(my$SUB1)
my$Site_habitat_index<-as.factor(my$Site_habitat_index)
my$SUB1<-as.factor(my$SUB1)
my$Velocity<-as.factor(my$Velocity)
my$Month<-as.factor(my$Month)
my$VIX_klass<-as.factor(my$VIX_klass)


# Temporal and spatial variation: what level of replication to consider --------
#If I consider river as my replicate, shall I average away:
#1)the spatial variation: take avg of sites for different years, to obtain one value per 
#river and year combination. This would solve the problem of having differen spot per site 
#and NAs in site's labels (which could represent more than one site)
#2)the temporal variation: calculate avg of year per each site. It would not solve the 
# problem of NAs in site's label, but lat-long show that NAs corrispond to one site most of 
#the time rather than mutliple site, so as far as Site is nested in River (site/river) 
# it can be okeysh. 

# Not to loose information and homogenize everything, I should average where the variability 
# is lowest: from year to year or between sites of the same river? Test by plotting response 
# variables (LWD and fish) vs  site (boxplot) to see temporal variation, or vs river per year
# (xyplot) to see spatial variation between sites of the same river.

#temporal variation between replicates in the same site:
plot(my$LWD~my$Site_name)
plot(my$OringTOT~my$Site_name)

# consider 500 data to avoid crashing:
my1<-my[1:500,]
plot(my1$LWD~my1$Site_name)
plot(my1$OringTOT~my1$Site_name)
my1<-my[1:400,]
ggplot(my1, aes(x = Site_name, y = LWD)) +
  geom_point(aes(colour = Site_name), size = 2)
ggplot(my1, aes(x = Site_name, y = LWD)) +
  geom_point(aes(colour = River_name), size = 2)

#spatial variation between sites of a river for specific years:
ggplot(my1, aes(x = Year, y = LWD)) +
  geom_point(aes(colour = River_name), size = 2)
ggplot(my1, aes(x = Year, y = OringTOT)) +
  geom_point(aes(colour = River_name), size = 2)


# Extracting averages per river and year -----------------------------------
### extract means per river and year: you can not do it for factors (nor binary variables)
# I think: include only factors in the list(groups), while calculate later binary variables or  
# inlcude also those binary variables and keep them as numeric, and convert them later all no-zero numbers into ones 
# Also, keep only variables of interest, you can always add later
AV<-aggregate(cbind(my$Altitude,my$ddlat,my$ddlong,my$LWD,my$exaktarea,my$Wetted_width,my$Site_length
                    ,my$Site_area,my$Maxdepth,my$Av_depth,my$Water_temperature,my$Average_air_temperature
                    ,my$SUB1,my$Site_habitat_index,my$Velocity,my$Slope_percent,my$Distance_to_sea,my$Month,my$Julian_date
                    ,my$Abbor,my$BEcrOTOT,my$Elrit,my$GEdda,my$HarrTOT,my$Lake,my$LaxFIXTO,my$LaxOrtot,my$LaxTOT,my$Eel,my$MOrt,my$OringTOT
                    ,my$RegnbTOT,my$ROdinTOT,my$Cottus_spp,my$Lampetra,my$Sticklebacks,my$VIX,my$VIX_klass,my$Number_of_fish_species
                    ),list(my$River_name,my$Catchment_number,my$Year),mean)
names(AV)<-c("River_name", "Catchment_number","Year", 
             "Altitude","Lat","Long","LWD","exaktarea","Wetted_width","Site_length","Site_area",
             "Maxdepth","Av_depth","Water_temperature","Average_air_temperature","SUB1","Site_habitat_index",
             "Velocity","Slope_percent","Distance_to_sea","Month","Julian_date","Abbor","BEcrOTOT","Elrit","GEdda",
             "HarrTOT","Lake","LaxFIXTO","LaxOrtot","LaxTOT","Eel","MOrt","OringTOT","RegnbTOT","ROdinTOT","Cottus_spp",
             "Lampetra","Sticklebacks","VIX","VIX_klass","Number_of_fish_species")
head(AV)

#check what happens to NAs
AV$Wetted_width
max(AV$Wetted_width)
min(AV$Wetted_width)
summary(AV)
#check with subset:
AV1<-AV[AV$River_name=="Sennan",]
AV1
# se cé un NA non calcola la media ma riporta NA. Perderei dati solo se ho piu' repliche per site AND YEAR e solo uno
# dei valori e´ NA. # controlla alla fine con summary(AV) quanti NA hai accumulato per response variable

summary(AV)
# For some of the environmental variables det finns många NAs but LWD and fish are good.
# suggest: start analyses, and see key drivers. For those you can always recover more values later

# Add binary variables for absence/presence of fish -----------------------
######### add to the dataset binary variables of presence/absence for fish (as numerical variables):
AV$GEdda_KLASS <- ifelse(AV$GEdda > 0, c(1), c(0)) 
head(AV)
str(AV)
AV$OringTOT_KLASS <- ifelse(AV$OringTOT > 0, c(1), c(0)) 
AV$LaxTOT_KLASS <- ifelse(AV$LaxTOT > 0, c(1), c(0)) 

# or use (from Zuur 2010):
DeerEcervi$Ecervi.01 <- DeerEcervi$Ecervi
DeerEcervi$Ecervi.01[DeerEcervi$Ecervi >0 ] <- 1


# subsets of data ---------------------------------------------------------

#choose a specific year. 2009 is that with more observation:
AV2009<-AV[AV$Year=="2009",]

#take the average of years:
AVyear<-aggregate(cbind(AV$Altitude,AV$Lat,AV$Long,AV$LWD,AV$exaktarea,AV$Wetted_width,AV$Site_length
                        ,AV$Site_area,AV$Maxdepth,AV$Av_depth,AV$Water_temperature,AV$Average_air_temperature
                        ,AV$SUB1,AV$Site_habitat_index,AV$Velocity,AV$Slope_percent,AV$Distance_to_sea,AV$Month,AV$Julian_date
                        ,AV$Abbor,AV$BEcrOTOT,AV$Elrit,AV$GEdda,AV$HarrTOT,AV$Lake,AV$LaxFIXTO,AV$LaxOrtot,AV$LaxTOT,AV$Eel,AV$MOrt,AV$OringTOT
                        ,AV$RegnbTOT,AV$ROdinTOT,AV$Cottus_spp,AV$Lampetra,AV$Sticklebacks,AV$VIX,AV$VIX_klass,AV$Number_of_fish_species
),list(AV$River_name,AV$Catchment_number),mean)
names(AVyear)<-c("River_name", "Catchment_number", 
                 "Altitude","Lat","Long","LWD","exaktarea","Wetted_width","Site_length","Site_area",
                 "Maxdepth","Av_depth","Water_temperature","Average_air_temperature","SUB1","Site_habitat_index",
                 "Velocity","Slope_percent","Distance_to_sea","Month","Julian_date","Abbor","BEcrOTOT","Elrit","GEdda",
                 "HarrTOT","Lake","LaxFIXTO","LaxOrtot","LaxTOT","Eel","MOrt","OringTOT","RegnbTOT","ROdinTOT","Cottus_spp",
                 "Lampetra","Sticklebacks","VIX","VIX_klass","Number_of_fish_species")
head(AVyear) # add binary variables:
AVyear$OringTOT_KLASS <- AVyear$OringTOT
AVyear$OringTOT_KLASS[AVyear$OringTOT >0 ] <- 1
AVyear$GEdda_KLASS <- ifelse(AVyear$GEdda > 0, c(1), c(0)) 
# and remove NAs:
AVyear2<-na.omit(AVyear)



#tutti i fiumi il cui nome ricorre piu'  di una volta
AVOC<-AV[AV$River_name %in% n_occur$Var1[n_occur$Freq > 1],]




# SPATIAL AUTOCORRELATION -------------------------------------------------
# One catchment contain several rivers. But also, sometimes, one same (long!) river belong to 
# more catchments, which is not a problem for visual exploration (averages were calculated separetely
# for each river*catchment combination) but it may be a problem with the model, not much with the 
# spatial correlation, but with the  temporal 
# autocorrelation: I use a correrlation structure between years of the same river (e.g.
# CompSymm(form=~Year|River)), which may belong to more catchments. It 
# is still the same river, true, but the problem is that I may have more values for the same year (in 
# different catchments)! To avoid that I should nest river within catchment, maybe it works even if
# the name of the river is sometimes the same in differnet catchments: 
# try e.g. CompSymm(form=~Year|River/catchment)

# 1) visual exploration:
plot(AV$OringTOT~AV$Catchment_number)
plot(AV$LWD~AV$Catchment_number)


# modeling:
M1<-lme(OringTOT~LWD, random =~1|Catchment_number, data=AV)
summary(M1)
M2<-lm(OringTOT~LWD, data=AV)
anova(M1,M2)
# significant. but is linear model ok? maybe not, and maybe I should rather go for binary variables


# Temporal autocorrelation ------------------------------------------------
# 1) visual exploration:
# I should check within groups, that means within each river: I need to nest year within river
# (or catchment maybe?)
# 1.1) plots: plots fish~year for subsets corresponding to one river or catchmnet at a time. Also,
# 1.2) use acf for that same subset

# 1.1) not easy to disentangle:
plot(AV$OringTOT~AV$Year,subset=(AV$Catchment_number=="48"), 
     col=as.numeric(AV$River_name))
plot(AV$OringTOT~AV$Year,subset=(AV$Catchment_number=="1"), 
     col=as.numeric(AV$River_name))
plot(AV$OringTOT~AV$Year,subset=(AV$River_name=="SvartOn"))
# nicer layout but still hard to interpret
# 1.2) use acf: should be run for univariate time series, i.e. for a certain site across years
AV1<-AV[AV$River_name=="SvartOn",]# check now whether it belongs to more catchments: yes, so:
AV2<-AV[(AV$River_name=="SvartOn")&(AV$Catchment_number=="18"),]
acf(AV2$OringTOT) # ok :)

AV1<-AV[AV$River_name=="KitkiOjoki",] # check now whether it belongs to more catchments. no
acf(AV1$OringTOT) # ok :)

AV1<-AV[AV$River_name=="AkkarjOkkO",] # check now whether it belongs to more catchments. no
acf(AV1$OringTOT) # little, after the 1 year lag only

AV1<-AV[AV$River_name=="MalbEcken",] # check now whether it belongs to more catchments: yes, so:
AV2<-AV[(AV$River_name=="MalbEcken")&(AV$Catchment_number=="17"),]
acf(AV2$OringTOT) # ok

############# do for some more examples, take sites with many years!! 
#altre opzioni ma troppo pesanti
# xyplot(OringTOT ~ Year|River_name, data=AV,  occchio a plottare, rischi il crash
#panel = function (x,y){
#  panel.grid(h=-1,v=2)
#  panel.points(x, y, col = 1)
#  panel.loess(x, y, span = 0.5, col =1, lwd =2)})
#coplot(OringTOT~Year|Catchment_number,panel = panel.smooth)
############# cmq a occhio non mi sembra ci sia corr temporale

# 2) modeling temporal autocorrelation:  
# try first with the simplest correlation structure. these should be quivalent:
# corCompSymm(form=~Year|River) and random=~Year|River 
# Explora also random=~Year|River/catchment

#### only temporal correlation:
# simple correlation structure:
M1<-gls(OringTOT~LWD, corCompSymm(form=~Year|River_name), data=AV)
summary(M1) # rho = 0.5438009 
M1<-lme(OringTOT~LWD, random =~Year|River_name, data=AV)
summary(M1) # should be the same as above, confirm? yes, very small differences

# nesting: to be more correct, as the same river can belong to differnet cathment:
M1<-lme(OringTOT~LWD, random =~Year|River_name/Catchment_number, data=AV)
summary(M1) # does it run? yesss
# is the same as: 
M2<-gls(OringTOT~LWD, corCompSymm(form=~Year|River_name/Catchment_number), data=AV) # rho=0.591894 
# yes, and this last one run muuuuuch faster!!! 

# or was it: check the right script for nested random factors (they are not the same)
M1<-gls(OringTOT~LWD, corCompSymm(form=~Year|Catchment_number/River_name), data=AV)

# other correlation structures: corAR:
M3<-gls(OringTOT~LWD, corAR1(form=~Year|River_name/Catchment_number), data=AV) # phi = 0.69 
summary(M1)
# or using spatial correaltion function, it copes better with missing values and irregularly 
# spaced data, such as: corLin(form=~Year|River, nugget=T), corGaus, corExp, corSpher
M1<-gls(OringTOT~LWD, corLin(form=~Year|River_name/Catchment_number, nugget=T),data=AV)
# try also the others!

#compare:
M0<-gls(OringTOT~LWD, data=AV) 
M2<-gls(OringTOT~LWD, corCompSymm(form=~Year|River_name/Catchment_number), data=AV) # rho=0.591894 
M3<-gls(OringTOT~LWD, corAR1(form=~Year|River_name/Catchment_number), data=AV) # phi = 0.69 
M1<-gls(OringTOT~LWD, corLin(form=~Year|River_name/Catchment_number, nugget=T),data=AV)
AIC(M1,M2,M3,M0)
anova(M0,M1) # M1 wins



# Both spatial and temporal correlation: ----------------------------------

#Spatial and temporal correlation are modelled separately, i.e. I don't need to include the spatial random factor 
#in the temporal correlation structure. But in the temporal correlation I need to make sure that the values for 
#which I model the temporal  correlation come from the same spot, i.e. they can differ in year and river but not 
# other spatial variables. E.g.:

# for öring continuous
# temp corr:
M0<-lme(OringTOT~LWD,random=~1|River_name/Catchment_number, data=AV)
M1<-lme(OringTOT~LWD,random=~1|River_name/Catchment_number, corCompSymm(form=~Year), data=AV)
M2<-lme(OringTOT~LWD,random=~1|River_name/Catchment_number, corAR1(form=~Year), data=AV)
M3<-lme(OringTOT~LWD,random=~1|River_name/Catchment_number, corLin(form=~Year), data=AV)
M4<-lme(OringTOT~LWD,random=~1|River_name/Catchment_number, correlation=corExp(form=~ Year), data=AV)
AIC(M0,M1,M2,M3,M4) #M2 and M4 are the same, but M2's script takes 50% time less to run :)
anova(M0,M2) #M2 wins

# spatial corr:
M2<-lme(OringTOT~LWD,random=~1|River_name/Catchment_number, corAR1(form=~Year), data=AV)
M5<-gls(OringTOT~LWD, corAR1(form=~Year|River_name/Catchment_number), data=AV)
anova(M2,M5) #M2 wins

# for öring binary:
# temp corr:
M0<-lme(OringTOT_KLASS~LWD,random=~1|River_name/Catchment_number, data=AV)
M1<-lme(OringTOT_KLASS~LWD,random=~1|River_name/Catchment_number, corCompSymm(form=~Year), data=AV)
M2<-lme(OringTOT_KLASS~LWD,random=~1|River_name/Catchment_number, corAR1(form=~Year), data=AV)
M3<-lme(OringTOT_KLASS~LWD,random=~1|River_name/Catchment_number, corLin(form=~Year), data=AV)
AIC(M0,M1,M2,M3) #
anova(M0,M2) #M2 wins

#spatial corr:
M2<-lme(OringTOT_KLASS~LWD,random=~1|River_name/Catchment_number, corAR1(form=~Year), data=AV)
M5<-gls(OringTOT_KLASS~LWD, corAR1(form=~Year|River_name/Catchment_number), data=AV)
anova(M2,M5) #M2 wins

#NB: given that one river belong to more than 1 catchemnt, may be enough to include only river as random,
# instead of a nested random factor, i.e.:
M2<-lme(OringTOT~LWD,random=~1|River_name, corAR1(form=~Year), data=AV) #nope



# !!! remember to check the assumption that the time series are not correlated,
# by looking at correlation between residuals of the model coming from each time serie

# explore collinearity of predictors ------------------------------------

#take dataframe with only environmental data and calculate model:
pgd<-AV[,4:22] # or pgd<-AV[,3:22] to include year but it does not explain much (actually tot explained variation declines)
# maybe need to convert categorical variables into dummy variables.  But are there any?
# No, the problem is caused by NAs: 
summary(pgd)
# exclude all NAs fpor now:
d<-na.omit(pgd)
summary(d)
# variables should be scaled (subtractm mean abnd divide by stand dev) as were taken with different unit of measurements
model.pca<-prcomp(d,scale=T)
summary(model.pca)

# decline of the explained variation on x axis=scree plot
plot(model.pca,xlab="ordination axis")

print(model.pca)               #     give environment scores
predict(model.pca)[,1]         #     give samples scores! axis 1
predict(model.pca)[,2]         #     give samples scores! axis 2

#biplot:
biplot(model.pca)
# biplot tells that points close to each other have the similar environmental conditions
# direction of arrow is the magnitude of the increase of the environm. factor
#two units of measurements on the sides, maybe standardized and unstandardized scores

# Make a better plot:
envir_scores<-printc(model.pca)  #For arrows - 
d$scores_x<-predict(model.pca)[,1]         #     give samples scores! axis 1
d$scores_y<-predict(model.pca)[,2]         #     give samples scores! axis 2

plot(d$scores_x,d$scores_y)
#arrows(0,0, -0.3969727,0.29611532, col="red", length=0.05) #Arrows - dont work

# check linear vs non linear repsonse of env factors vs PCA scores:
dev.off()
par(mfrow=c(2,3))
plot(d$scores_x,d$LWD)
plot(d$scores_x,d$Average_air_temperature)
plot(d$scores_x,d$Wetted_width)
plot(d$scores_y,d$LWD)
plot(d$scores_y,d$Average_air_temperature)
plot(d$scores_y,d$Wetted_width)

# check with DCA:
# rule of thumb: if the lengths of DCA axes 1 and 2 are both lower than 3, we can use PCA,
# which assumes linear responses of species to environmental gradients(ter Braak and Smilauer 2002).
# in this case, my species are actually the dam attributes - better say: linear responses of
# original variables to the components (the PCA axes) 
library(vegan)
d<-na.omit(pgd)
# exclude  negative values in Water_temperature and air temperature, decorana can not handle those:
d1<-d[!d$Water_temperature<0,]
d2<-d1[!d1$Average_air_temperature<0,]
summary(d2)
ord <- decorana(d2)
ord


# PCA with fish data ------------------------------------------------------

pgd<-AV[,23:39]
summary(pgd)
d<-na.omit(pgd)
model.pca<-prcomp(d,scale=T)
summary(model.pca)
plot(model.pca,xlab="ordination axis")
print(model.pca)               #     give environment scores
predict(model.pca)[,1]         #     give samples scores! axis 1
predict(model.pca)[,2]         #     give samples scores! axis 2
biplot(model.pca)

# DCA:
library(vegan)
ord <- decorana(d) # I should get community weighted values, now there are too many zeros

# correlation matrix:
scatterplot.matrix(~Abbor+BEcrOTOT+Elrit+GEdda+HarrTOT
                   +Lake+OringTOT,data=d)

pairs(d,panel=panel.smooth)

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))                                    # these are person correlation. or : r <- cor(x, y, method="spearman")
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(d, lower.panel=panel.smooth, upper.panel=panel.cor)


# graphs öring -----------------------------------------------------
dotchart(AV$OringTOT)

# explore sampling artefacts:
plot(AV$OringTOT~AV$Site_length) 
plot(AV$OringTOT~AV$Site_area) # ok, no sign of incxreased abundances for higher values of legnths or area
plot(AV$OringTOT~AV$Water_temperature) # hump shape

# explore relationships with potential explanatory factors:
# LWD
plot(AV$OringTOT~AV$LWD)
plot(AV$OringTOT_KLASS~AV$LWD)# more promising with binarian varirables indeed...
plot(AV$OringTOT~AV$LWD, cex=AV$GEdda/5)
plot(AV$OringTOT~AV$LWD, cex=AV$GEdda/4, xlim=c(0,150), ylim=c(0,350))
plot(AV$OringTOT_KLASS~AV$LWD,cex=AV$GEdda/4,xlim=c(0,50),)# more promising with binarian varirables indeed...

# seasonality and year-to-year variation
plot(AV$OringTOT~AV$Julian_date)
plot(AV$OringTOT_KLASS~AV$Julian_date)
plot(AV$OringTOT~AV$Month)
plot(AV$OringTOT_KLASS~AV$Month)
plot(AV$OringTOT~AV$Year)
plot(AV$OringTOT_KLASS~AV$Year)

# climate and geography
plot(AV$OringTOT~AV$Lat)
plot(AV$OringTOT_KLASS~AV$Lat)
plot(AV$OringTOT~AV$Altitude)
plot(AV$OringTOT_KLASS~AV$Altitude)
plot(AV$OringTOT~AV$Average_air_temperature)
plot(AV$OringTOT_KLASS~AV$Average_air_temperature)
plot(AV$OringTOT~AV$Distance_to_sea)
plot(AV$OringTOT_KLASS~AV$Distance_to_sea)

# stream size
plot(AV$OringTOT~AV$Wetted_width)
plot(AV$OringTOT_KLASS~AV$Wetted_width)
plot(AV$OringTOT~AV$exaktarea)
plot(AV$OringTOT_KLASS~AV$exaktarea)
plot(AV$OringTOT~AV$Av_depth)
plot(AV$OringTOT_KLASS~AV$Av_depth)
plot(AV$OringTOT~AV$Maxdepth)
plot(AV$OringTOT_KLASS~AV$Maxdepth)

# stream local features
plot(AV$OringTOT~AV$Slope_percent)
plot(AV$OringTOT_KLASS~AV$Slope_percent)
plot(AV$OringTOT~AV$Velocity)
plot(AV$OringTOT_KLASS~AV$Velocity)
# substrate
plot(AV$OringTOT~AV$SUB1)
plot(AV$OringTOT_KLASS~AV$SUB1)

# predators and competitors:
plot(AV$OringTOT~AV$GEdda)
plot(AV$OringTOT_KLASS~AV$GEdda)
plot(AV$OringTOT~AV$Lampetra)
plot(AV$OringTOT_KLASS~AV$Lampetra)
plot(AV$OringTOT~AV$BEcrOTOT)
plot(AV$OringTOT_KLASS~AV$BEcrOTOT)

# VIX:
plot(AV$OringTOT~AV$VIX)

# graphs LWD ---------------------------------------------------------------------
dotchart(AV$LWD)

# explore sampling artefacts:
plot(AV$LWD~AV$Site_length) 
plot(AV$LWD~AV$Site_area) # ok, no sign of incxreased abundances for higher values of legnths or area
plot(AV$LWD~AV$Water_temperature) # hump shape

# seasonality and year-to-year variation
plot(AV$LWD~AV$Julian_date)
plot(AV$LWD~AV$Month)
plot(AV$LWD~AV$Year)

# climate and geography
plot(AV$LWD~AV$Lat)
plot(AV$LWD~AV$Altitude)
plot(AV$LWD~AV$Average_air_temperature)
plot(AV$LWD~AV$Distance_to_sea)

# stream size
plot(AV$LWD~AV$Wetted_width)
plot(AV$LWD~AV$exaktarea)
plot(AV$LWD~AV$Av_depth)
plot(AV$LWD~AV$Maxdepth)

# stream local features
plot(AV$LWD~AV$Slope_percent)
plot(AV$LWD~AV$Velocity)


# predictors to transform? ------------------------------------------------
###########predictors to transform?
hist(AV$LWD)
hist(AV$Altitude)
hist(AV$Wetted_width)
hist(AV$Av_depth)
hist(AV$Average_air_temperature)
hist(AV$Slope_percent)
hist(AV$Distance_to_sea)
hist(AV$Velocity)



# explore single models for Öring binary ---------------------------------------------------------------------

# single models, check convergence, random part, interactions and collinearity among predictors:
# maybe center predictors so that intercept represent avg values rather than 0s

############### Öring binary: exploratory
# random part
# what to use: glmer, glmmPQL,glmmML. Which one can incorporate my complex spatial-temporal correlation?
#random=~1|River_name/Catchment_number, corAR1(form=~Year)
M1<-glmer(OringTOT_KLASS~LWD+(1|River_name),family=binomial,data=AV) #ok
M1<-glmer(OringTOT_KLASS~LWD+(1|Catchment_number),family=binomial,data=AV)#ok
M1<-glmer(OringTOT_KLASS~LWD+(1|River_name/Catchment_number),family=binomial,data=AV)#no. 
## maybe  bc we some rivers belong to more than 1 catchment? or maybe is the wrong order in random?
M1<-glmer(OringTOT_KLASS~LWD+(1|Catchment_number/River_name),family=binomial,data=AV)# yes! Checked, order is now correct
summary(M1)
M1<-glmer(OringTOT_KLASS~LWD+(Year|Catchment_number/River_name),family=binomial,data=AV)# no
# with a different script:
M1<-glmmPQL(OringTOT_KLASS~LWD,random =~ 1 |River_name,family=binomial,data=AV)#ok
M1<-glmmPQL(OringTOT_KLASS~LWD,random =~ 1 |Catchment_number,family=binomial,data=AV)#ok
M1<-glmmPQL(OringTOT_KLASS~LWD,random =~ 1 |River_name/Catchment_number,family=binomial,data=AV)#yes
M1<-glmmPQL(OringTOT_KLASS~LWD,random =~ Year |River_name/Catchment_number,family=binomial,data=AV)#no
summary(M1)
# implement temporal correlation with either script:
M1<-glmmPQL(OringTOT_KLASS~LWD,random =~1|Catchment_number+corAR1(form=~Year),family=binomial,data=AV)#no
M1<-glmer(OringTOT_KLASS~LWD+(1|Catchment_number/River_name)+corAR1(form=~Year),family=binomial,data=AV)# no
# conclusion: 
#1)either I skip temporal correlation, and check residuals at the end,or
#2)I choose 1 year, or I take the avg of all years
#3)I try Bayesian
#4) I use continuous instead of binarian

### more effort to model temporal correlation in glmer:
# remove rivers that have been sampled only once and try to model temporal correlation:
AVdup<-AV[duplicated(AV$River_name), ]
AV[1:20,1:3]
AVdup[1:8,1:3] # estraggo tutti i river ripetuti, but 1) perdo il primo della serie temporale per ogni fiume, e 2) per
# quei fiumi per cui ho solo 2 date, mi ritrovo con una sola
n_occur <- data.frame(table(AV$River_name))
n_occur[n_occur$Freq > 1,]
n_occur[n_occur$Freq == 1,]
AVOC<-AV[AV$River_name %in% n_occur$Var1[n_occur$Freq > 1],] #Ok! ottoengo tutti i fiumi il cui nome ricorre piu'  di una volta,
# e poiche'  ho clacolato le medie per anno, ottengo tutti i fiumi sampled in more than one year
# check:
head(AVOC)
AV[AV$River_name=="Vettasjoki",]
AVOC[AVOC$River_name=="Vettasjoki",]

M1<-glmer(OringTOT_KLASS~LWD+(1|Catchment_number/River_name)+corAR1(form=~Year),family=binomial,data=AVOC) #still nope
# is than bc of NAs? No
M1<-glmer(OringTOT_KLASS~LWD+(1|Catchment_number/River_name)+corAR1(form=~Year),family=binomial,data=AV2)
# is it because of the specific correlation strc (which is exponential?) nope
M1<-glmer(OringTOT_KLASS~LWD+(1|Catchment_number/River_name)+corCompSymm(form=~Year),family=binomial,data=AV2)
# the correlation worked with lme so my guess is that it doesn't cope well with glmer (or lmer)
M2<-lme(OringTOT_KLASS~LWD,random=~1|River_name/Catchment_number, corAR1(form=~Year), data=AV)
summary(M2)
M1<-lmer(OringTOT_KLASS~LWD+(1|Catchment_number/River_name)+corAR1(form=~Year),family=binomial,data=AV2)

### if I skip temporal correlation: beyond optimal model:
M1<-glm(OringTOT_KLASS~Wetted_width+LWD+Julian_date+Year+Average_air_temperature+Distance_to_sea
        +SUB1+GEdda,family=binomial,data=AV)
summary(M1)
vif(M1)
# add random:
M2<-glmer(OringTOT_KLASS~Wetted_width+LWD+Julian_date+Year+Average_air_temperature+Distance_to_sea
          +SUB1+GEdda+(1|Catchment_number/River_name),family=binomial,data=AV)# problems:
# try to add 1 predictor each time, based on PCA. maybe even use the PCA axis?
M2<-glmer(OringTOT_KLASS~LWD+Distance_to_sea+Average_air_temperature+Av_depth
          +(1|Catchment_number/River_name),family=binomial,data=AV)
summary(M2) #hard to add more now, check whether random is significant:
M1<-glm(OringTOT_KLASS~LWD+Distance_to_sea+Average_air_temperature+Av_depth,family=binomial,data=AV)
AIC(M1,M2) #better M2. with a simpler random:better the one above
M2<-glmer(OringTOT_KLASS~LWD+Distance_to_sea+Average_air_temperature+Av_depth
          +(1|River_name),family=binomial,data=AV)
M1<-glm(OringTOT_KLASS~LWD+Distance_to_sea+Average_air_temperature+Av_depth,family=binomial,data=AV)
# anyways I should be sure about the order when nesting the random
# go on with this:
M2<-glmer(OringTOT_KLASS~LWD+Distance_to_sea+Average_air_temperature+Av_depth
          +(1|Catchment_number/River_name),family=binomial,data=AV)

### skip temporal correlation but choose a specific year:
n_occur <- data.frame(table(AV$Year))
n_occur[n_occur$Freq > 1,]
# 2009 is that with more observation:
AV2009<-AV[AV$Year=="2009",]
M1<-glmer(OringTOT_KLASS~LWD+(1|Catchment_number/River_name),family=binomial,data=AV2009)

### or take the average of years:
AVyear<-aggregate(cbind(AV$Altitude,AV$Lat,AV$Long,AV$LWD,AV$exaktarea,AV$Wetted_width,AV$Site_length
                        ,AV$Site_area,AV$Maxdepth,AV$Av_depth,AV$Water_temperature,AV$Average_air_temperature
                        ,AV$SUB1,AV$Site_habitat_index,AV$Velocity,AV$Slope_percent,AV$Distance_to_sea,AV$Month,AV$Julian_date
                        ,AV$Abbor,AV$BEcrOTOT,AV$Elrit,AV$GEdda,AV$HarrTOT,AV$Lake,AV$LaxFIXTO,AV$LaxOrtot,AV$LaxTOT,AV$Eel,AV$MOrt,AV$OringTOT
                        ,AV$RegnbTOT,AV$ROdinTOT,AV$Cottus_spp,AV$Lampetra,AV$Sticklebacks,AV$VIX,AV$VIX_klass,AV$Number_of_fish_species
),list(AV$River_name,AV$Catchment_number),mean)
names(AVyear)<-c("River_name", "Catchment_number", 
                 "Altitude","Lat","Long","LWD","exaktarea","Wetted_width","Site_length","Site_area",
                 "Maxdepth","Av_depth","Water_temperature","Average_air_temperature","SUB1","Site_habitat_index",
                 "Velocity","Slope_percent","Distance_to_sea","Month","Julian_date","Abbor","BEcrOTOT","Elrit","GEdda",
                 "HarrTOT","Lake","LaxFIXTO","LaxOrtot","LaxTOT","Eel","MOrt","OringTOT","RegnbTOT","ROdinTOT","Cottus_spp",
                 "Lampetra","Sticklebacks","VIX","VIX_klass","Number_of_fish_species")
head(AVyear)
AVyear$OringTOT_KLASS <- AVyear$OringTOT
AVyear$OringTOT_KLASS[AVyear$OringTOT >0 ] <- 1

M1<-glmer(OringTOT_KLASS~LWD+(1|Catchment_number/River_name),family=binomial,data=AVyear)
summary(M1)




# # explore single models for Öring continuous ----------------------------

###############  Öring continuous: exploratory
M2<-lme(OringTOT~LWD+Average_air_temperature,random=~1|River_name/Catchment_number, 
        corAR1(form=~Year), data=AV)
#NAs give problems:
AV2<-na.omit(AV) # un po' drastico, ma giusto per vedere se risolvo..
M2<-lme(OringTOT~LWD+Distance_to_sea+Average_air_temperature+Av_depth+Wetted_width+Julian_date+Year+SUB1+GEdda,
        random=~1|River_name/Catchment_number, corAR1(form=~Year), method="ML",data=AV2)
summary(M2)
vif(M2)
M3<-update(M2, .~. -SUB1)
anova(M2,M3)
# better to transform?
M1<-lme(OringTOT~LWD+Distance_to_sea+Average_air_temperature+Av_depth+Wetted_width+Julian_date+Year+SUB1+GEdda,
        random=~1|River_name/Catchment_number, corAR1(form=~Year), method="ML",data=AV2)
M2<-lme(log(OringTOT+1)~LWD+Distance_to_sea+Average_air_temperature+Av_depth+Wetted_width+Julian_date+Year+SUB1+GEdda,
        random=~1|River_name/Catchment_number, corAR1(form=~Year), method="ML",data=AV2)
M3<-lme(sqrt(OringTOT+1)~LWD+Distance_to_sea+Average_air_temperature+Av_depth+Wetted_width+Julian_date+Year+SUB1+GEdda,
        random=~1|River_name/Catchment_number, corAR1(form=~Year), method="ML",data=AV2)
AIC(M1,M2,M3) #log is way better



# explore single model for LWD --------------------------------------------
# LWD:
# trasnform?
hist(AV$LWD) #trasform seems better

M1<-lme(LWD~Distance_to_sea+Average_air_temperature+Av_depth+Wetted_width+Julian_date+Year,
        random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2)
M2<-lme(log(LWD+1)~Distance_to_sea+Average_air_temperature+Av_depth+Wetted_width+Julian_date+Year,
        random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2)
M3<-lme(sqrt(LWD)~Distance_to_sea+Average_air_temperature+Av_depth+Wetted_width+Julian_date+Year,
        random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2)
M4<-glmer(LWD~Distance_to_sea+Average_air_temperature+Av_depth+Wetted_width+Julian_date+Year+
            (1|Catchment_number/River_name),family=poisson,data=AV2) #don't run take forever
AIC(M1,M2,M3) #meglio log
summary(M2)


# piecewise SEM  ----------------------------------------------------------

#BINARY
# the best so far is:
### SEM: on AV2 (no NAs) and binary:
M2 = list(
  glmer(OringTOT_KLASS~log(LWD+1)+Av_depth+Wetted_width+Year
        +(1|Catchment_number/River_name),family=binomial,data=AV2),
  lme(log(LWD+1)~Distance_to_sea+Average_air_temperature+Av_depth+Wetted_width+Year,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2))
sem.fit(M2,AV2)
sem.coefs(M2,AV2)
sem.model.fits(M2)
sem.plot(M2, AV)
# if I trasnform some predictors? boh..
M2 = list(
  glmer(OringTOT_KLASS~log(LWD+1)+Av_depth+log(Wetted_width)+log(Distance_to_sea)
        +(1|Catchment_number/River_name),family=binomial,data=AV2),
  lme(log(LWD+1)~Average_air_temperature+Av_depth+log(Wetted_width)+Year,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2))
sem.fit(M2,AV2)
sem.coefs(M2,AV2)
sem.model.fits(M2)
sem.plot(M2, AV)

# other attempts, less succeful:
# SEM: on AV and binary:(BUT no temp correlation for öring): run again
M2 = list(
  glmer(OringTOT_KLASS~log(LWD+1)+Av_depth+Wetted_width+Average_air_temperature+SUB1
        +(1|Catchment_number/River_name),family=binomial,data=AV),
  lme(log(LWD+1)~Distance_to_sea+Average_air_temperature+Av_depth+Wetted_width+Year,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV))
sem.fit(M2,AV)
sem.coefs(M2,AV)
sem.model.fits(M2)
sem.plot(M2, AV)

# SEM: on AVyear (averages of years) and binary:
M2 = list(
  glmer(OringTOT_KLASS~log(LWD+1)+Av_depth+Wetted_width+Average_air_temperature+SUB1
        +(1|Catchment_number/River_name),family=binomial,data=AVyear),
  lme(log(LWD+1)~Distance_to_sea+Average_air_temperature+Av_depth+Wetted_width,
      random=~1|River_name/Catchment_number, data=AVyear))
sem.fit(M2,AVyear)
#  remove NAs:
AVyear2<-na.omit(AVyear)
M2 = list(
  glmer(OringTOT_KLASS~Av_depth+SUB1
        +(1|Catchment_number/River_name),family=binomial,data=AVyear2),
  lme(log(LWD+1)~Distance_to_sea+Average_air_temperature+Av_depth+Wetted_width,
      random=~1|River_name/Catchment_number, data=AVyear2))
sem.fit(M2,AVyear2)
sem.coefs(M2,AVyear2)
sem.model.fits(AVyear2)
sem.plot(M2, AVyear2)

# only with 2009 values:
M2 = list(
  glmer(OringTOT_KLASS~log(LWD+1)+Av_depth+SUB1+ GEdda_KLASS
        +(1|Catchment_number/River_name),family=binomial,data=AV2009),
  lme(log(LWD+1)~Distance_to_sea+Average_air_temperature+Av_depth+Wetted_width,
      random=~1|River_name/Catchment_number, data=AV2009))
sem.fit(M2,AVyear)
# remove NAs
AV2009_2<-na.omit(AV2009)
#  does not converge


################## CONTINUOUS
# SEM: on AV2 and continuous:
M2 = list(
  lme(log(OringTOT+1)~log(LWD+1)+Av_depth+Wetted_width+Distance_to_sea+Average_air_temperature+SUB1+GEdda,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2),
  lme(log(LWD+1)~Distance_to_sea+Average_air_temperature+Av_depth+Wetted_width+Year,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2))
sem.fit(M2,AV2)
sem.coefs(M2,AV2)
sem.model.fits(M2)
sem.plot(M2, AV)

# continue with AV2 and continous: trying different predictors:
####### move this part where you ahve PCA
# 1) CLIMATIC: lat, altitude, avg air temp
M1<-lm(log(OringTOT+1)~Lat+Altitude+Average_air_temperature, data=AV2)
vif(M1)
# try all but choose 1

# 2) GEOGRAPHIC:
M1<-lm(log(OringTOT+1)~Distance_to_sea+Long, data=AV2)
vif(M1)
# I would ignore long

# 3) STREAM SIZE:exaktarea+Wetted_width+Av_depth+ Maxdepth
M1<-lm(log(OringTOT+1)~exaktarea+Wetted_width+Av_depth, data=AV2)
vif(M1)
M1<-lm(log(OringTOT+1)~exaktarea+Wetted_width+Maxdepth, data=AV2)
vif(M1)
# choose between Max or avg depth

# 4) LOCAL FEATURES:
M1<-lm(log(OringTOT+1)~Velocity+Slope_percent+SUB1, data=AV2)
vif(M1)
#can include all

# 5) SEASONALITY
M1<-lm(log(OringTOT+1)~Month+Julian_date, data=AV2)
vif(M1)
# choose 1

#6) YEAR-TO_YEAR variation
Year

# 7) BIOTIC INTERACTIONS:
M1<-lm(log(OringTOT+1)~GEdda+Lampetra+Sticklebacks+LaxTOT+Abbor+BEcrOTOT+Elrit+HarrTOT+Lake+LaxFIXTO+LaxOrtot+Eel+
         MOrt+RegnbTOT+ROdinTOT+Cottus_spp, data=AV2)
vif(M1)
#better to run a PCA..But there are no obvious correlation

# 1)Climati factors: avg air temp OR lat are the best
# 3)Stream size: exact area is signif?NO. better avg or max depth?
# 4) inlcude all local features: velocity for LWD:no. Slope_percent for LWD:link to Öring is also suggested,
# links are positive in both cases but is supported by theory? ask Erik, meanwhile go on without
#5) add month or julian date:
# 6) biotic interactions: +GEdda+Lampetra+Sticklebacks+LaxTOT+Abbor+Lake+Cottus_spp were signif, but overall fit not good
# talk to Erik to know what makes sense. for now I keep only Gedda

### Final for AV2 for now..:
M2 = list(
  lme(log(OringTOT+1)~Average_air_temperature+Distance_to_sea+Wetted_width+Av_depth+log(LWD+1)+SUB1+Julian_date+GEdda,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2),
  lme(log(LWD+1)~Average_air_temperature+Distance_to_sea+Av_depth+Wetted_width+Year+Julian_date,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2))
sem.fit(M2,AV2)
sem.coefs(M2,AV2)
sem.model.fits(M2)
sem.plot(M2, AV2)

# SEM: with AV too many NAs, it fails
M2 = list(
  lme(log(OringTOT+1)~log(LWD+1)+Av_depth+Wetted_width+Distance_to_sea+Average_air_temperature+SUB1+GEdda,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV),
  lme(log(LWD+1)~Distance_to_sea+Average_air_temperature+Av_depth+Wetted_width+Year,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV))
sem.fit(M2,AV)
sem.coefs(M2,AV)
sem.model.fits(M2)
sem.plot(M2, AV)

# taking away gtemporal variability:
# With AVyear (averages of years) without NAs:
M2 = list(
  lme(log(OringTOT+1)~log(LWD+1)+Av_depth+Wetted_width+Distance_to_sea+Average_air_temperature+SUB1+GEdda,
      random=~1|River_name/Catchment_number, na.action=na.omit, data=AVyear),
  lme(log(LWD+1)~Distance_to_sea+Average_air_temperature+Av_depth+Wetted_width,
      random=~1|River_name/Catchment_number, na.action=na.omit, data=AVyear))
sem.fit(M2,AVyear)
sem.coefs(M2,AVyear)
sem.model.fits(M2)
sem.plot(M2, AVyear)
# With AV2009 without NAs:
M2 = list(
  lme(log(OringTOT+1)~Av_depth+Wetted_width+Distance_to_sea+Average_air_temperature+GEdda,
      random=~1|River_name/Catchment_number, na.action=na.omit, data=AV2009),
  lme(log(LWD+1)~Distance_to_sea+Average_air_temperature+Wetted_width,
      random=~1|River_name/Catchment_number, na.action=na.omit, data=AV2009))
sem.fit(M2,AV2009)
sem.coefs(M2,AV2009)
sem.model.fits(M2)
sem.plot(M2, AV2009)


# With AVOC(exclude sites sampled only once) without NAs:
M2 = list(
  lme(log(OringTOT+1)~log(LWD+1)+Av_depth+Wetted_width+Distance_to_sea+Average_air_temperature+SUB1+GEdda+Year,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),na.action=na.omit, data=AVOC),
  lme(log(LWD+1)~Distance_to_sea+Average_air_temperature+Av_depth+Wetted_width+Year,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),na.action=na.omit, data=AVOC))
sem.fit(M2,AVOC)
sem.coefs(M2,AVOC)
sem.model.fits(M2)
sem.plot(M2, AVOC)

# include interaction predators*LWD: not signif
# include VIX?
