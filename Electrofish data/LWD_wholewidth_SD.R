rm(list=ls())
setwd("C:/Users/sedi0002/Google Drive/Dod ved/Electrofish data")
setwd("C:/Users/serena/Google Drive/Dod ved/Electrofish data")
my<-read.table("LWD_wholewidth_SD6.txt",header=T)

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
library(visreg)

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
                    ,my$SUB1,my$Site_habitat_index,my$Velocity,my$Slope_percent,my$Distance_to_sea,my$Month,my$Julian_date,my$Typ_of_migration_numerical
                    ,my$Abbor,my$BEcrOTOT,my$Elrit,my$GEdda,my$HarrTOT,my$Lake,my$LaxFIXTO,my$LaxOrtot,my$LaxTOT,my$Eel,my$MOrt,my$OringTOT
                    ,my$RegnbTOT,my$ROdinTOT,my$Cottus_spp,my$Lampetra,my$Sticklebacks,my$VIX,my$VIX_klass,my$Number_of_fish_species
                    ),list(my$River_name,my$Catchment_number,my$Year),mean)
names(AV)<-c("River_name", "Catchment_number","Year", 
             "Altitude","Lat","Long","LWD","exaktarea","Wetted_width","Site_length","Site_area",
             "Maxdepth","Av_depth","Water_temperature","Average_air_temperature","SUB1","Site_habitat_index",
             "Velocity","Slope_percent","Distance_to_sea","Month","Julian_date","Type_migration_continuous","Abbor","BEcrOTOT","Elrit","GEdda",
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


# not to lose too much info, I delete NA at the level of site, so that I can still get an average per river
# (if not all sites have NAs for that river):
summary(my)
my_Migration_NAremoved<-my[!is.na(my$Typ_of_migration_numerical),]
my_Migration_0<-my_Migration_NAremoved[my_Migration_NAremoved$Typ_of_migration_numerical=="0",]
my_Migration_1<-my_Migration_NAremoved[my_Migration_NAremoved$Typ_of_migration_numerical=="1",]

# than take the averages:
AV_Migration_NAremoved<-aggregate(cbind(my_Migration_NAremoved$Altitude,my_Migration_NAremoved$ddlat,my_Migration_NAremoved$ddlong,my_Migration_NAremoved$LWD,my_Migration_NAremoved$exaktarea,my_Migration_NAremoved$Wetted_width,my_Migration_NAremoved$Site_length
                    ,my_Migration_NAremoved$Site_area,my_Migration_NAremoved$Maxdepth,my_Migration_NAremoved$Av_depth,my_Migration_NAremoved$Water_temperature,my_Migration_NAremoved$Average_air_temperature
                    ,my_Migration_NAremoved$SUB1,my_Migration_NAremoved$Site_habitat_index,my_Migration_NAremoved$Velocity,my_Migration_NAremoved$Slope_percent,my_Migration_NAremoved$Distance_to_sea,my_Migration_NAremoved$Month,my_Migration_NAremoved$Julian_date,my_Migration_NAremoved$Typ_of_migration_numerical
                    ,my_Migration_NAremoved$Abbor,my_Migration_NAremoved$BEcrOTOT,my_Migration_NAremoved$Elrit,my_Migration_NAremoved$GEdda,my_Migration_NAremoved$HarrTOT,my_Migration_NAremoved$Lake,my_Migration_NAremoved$LaxFIXTO,my_Migration_NAremoved$LaxOrtot,my_Migration_NAremoved$LaxTOT,my_Migration_NAremoved$Eel,my_Migration_NAremoved$MOrt,my_Migration_NAremoved$OringTOT
                    ,my_Migration_NAremoved$RegnbTOT,my_Migration_NAremoved$ROdinTOT,my_Migration_NAremoved$Cottus_spp,my_Migration_NAremoved$Lampetra,my_Migration_NAremoved$Sticklebacks,my_Migration_NAremoved$VIX,my_Migration_NAremoved$VIX_klass,my_Migration_NAremoved$Number_of_fish_species
),list(my_Migration_NAremoved$River_name,my_Migration_NAremoved$Catchment_number,my_Migration_NAremoved$Year),mean)
names(AV_Migration_NAremoved)<-c("River_name", "Catchment_number","Year", 
             "Altitude","Lat","Long","LWD","exaktarea","Wetted_width","Site_length","Site_area",
             "Maxdepth","Av_depth","Water_temperature","Average_air_temperature","SUB1","Site_habitat_index",
             "Velocity","Slope_percent","Distance_to_sea","Month","Julian_date","Type_migration_continuous","Abbor","BEcrOTOT","Elrit","GEdda",
             "HarrTOT","Lake","LaxFIXTO","LaxOrtot","LaxTOT","Eel","MOrt","OringTOT","RegnbTOT","ROdinTOT","Cottus_spp",
             "Lampetra","Sticklebacks","VIX","VIX_klass","Number_of_fish_species")


# Add binary and transformed variables -----------------------
######### add to the dataset binary variables of presence/absence for fish (as numerical variables):

# into binary:
AV$OringTOT_KLASS <- ifelse(AV$OringTOT > 0, c(1), c(0)) 
# potential predators
AV$GEdda_KLASS <- ifelse(AV$GEdda > 0, c(1), c(0)) 
AV$Lake_KLASS <- ifelse(AV$Lake > 0, c(1), c(0)) 
# potential competitors
AV$Cottus_spp_KLASS <- ifelse(AV$Cottus_spp > 0, c(1), c(0)) 
AV$BEcrOTOT_KLASS <- ifelse(AV$BEcrOTOT > 0, c(1), c(0)) 
AV$HarrTOT_KLASS <- ifelse(AV$HarrTOT > 0, c(1), c(0)) 
AV$LaxTOT_KLASS <- ifelse(AV$LaxTOT > 0, c(1), c(0)) 

#Log transformation:
AV$log_OringTOT <- log(AV$OringTOT+1)
AV$log_LWD <- log(AV$LWD+1)
AV$log_GEdda<- log(AV$GEdda+1)
AV$log_Lake<- log(AV$Lake+1)
AV$log_Cottus_spp<- log(AV$Cottus_spp+1)
AV$log_BEcrOTOT<- log(AV$BEcrOTOT+1)
AV$log_HarrTOT<- log(AV$HarrTOT+1)
AV$log_LaxTOT<- log(AV$LaxTOT+1)

# to the other dataset:
# into binary:
AV_Migration_NAremoved$OringTOT_KLASS <- ifelse(AV_Migration_NAremoved$OringTOT > 0, c(1), c(0)) 
# potential predators
AV_Migration_NAremoved$GEdda_KLASS <- ifelse(AV_Migration_NAremoved$GEdda > 0, c(1), c(0)) 
AV_Migration_NAremoved$Lake_KLASS <- ifelse(AV_Migration_NAremoved$Lake > 0, c(1), c(0)) 
# potential competitors
AV_Migration_NAremoved$Cottus_spp_KLASS <- ifelse(AV_Migration_NAremoved$Cottus_spp > 0, c(1), c(0)) 
AV_Migration_NAremoved$BEcrOTOT_KLASS <- ifelse(AV_Migration_NAremoved$BEcrOTOT > 0, c(1), c(0)) 
AV_Migration_NAremoved$HarrTOT_KLASS <- ifelse(AV_Migration_NAremoved$HarrTOT > 0, c(1), c(0)) 
AV_Migration_NAremoved$LaxTOT_KLASS <- ifelse(AV_Migration_NAremoved$LaxTOT > 0, c(1), c(0)) 

#Log transformation:
AV_Migration_NAremoved$log_OringTOT <- log(AV_Migration_NAremoved$OringTOT+1)
AV_Migration_NAremoved$log_LWD <- log(AV_Migration_NAremoved$LWD+1)
AV_Migration_NAremoved$log_GEdda<- log(AV_Migration_NAremoved$GEdda+1)
AV_Migration_NAremoved$log_Lake<- log(AV_Migration_NAremoved$Lake+1)
AV_Migration_NAremoved$log_Cottus_spp<- log(AV_Migration_NAremoved$Cottus_spp+1)
AV_Migration_NAremoved$log_BEcrOTOT<- log(AV_Migration_NAremoved$BEcrOTOT+1)
AV_Migration_NAremoved$log_HarrTOT<- log(AV_Migration_NAremoved$HarrTOT+1)
AV_Migration_NAremoved$log_LaxTOT<- log(AV_Migration_NAremoved$LaxTOT+1)


# or use (from Zuur 2010):
DeerEcervi$Ecervi.01 <- DeerEcervi$Ecervi
DeerEcervi$Ecervi.01[DeerEcervi$Ecervi >0 ] <- 1



# Subsets of data ---------------------------------------------------------

# remove NAs from full dataset
AV2<-na.omit(AV)

# or from AV_Migration_NAremoved:
AV_Migration_NAremoved2<-na.omit(AV_Migration_NAremoved)


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
n_occur <- data.frame(table(AV$River_name))
n_occur[n_occur$Freq > 1,]
n_occur[n_occur$Freq == 1,]
AVOC<-AV[AV$River_name %in% n_occur$Var1[n_occur$Freq > 1],]
AVOC2<-na.omit(AVOC)

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




# explore single models for Öring continuous ----------------------------

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



# SEM Öring binary  ----------------------------------------------------------

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




# SEM Öring CONTINUOUS ----------------------------------------------------

# on AV2 (whole dataset without NAs)

# when trying different predictors:
# 1)Climatic factors: avg air temp OR lat are the best
# 3)Stream size: exact area is signif?NO. better avg or max depth?
# 4) inlcude all local features: velocity for LWD:no. Slope_percent for LWD:link to Öring is also suggested,
# links are positive in both cases but is supported by theory? ask Erik, meanwhile go on without
#5) add month or julian date:
# 6) biotic interactions: potenatial predatrors are GEdda and Lake. Potential competitors are:LaxTOT,Becro, Harr,Cottus_spp 
# include interaction predators*LWD: not signif
# include VIX? not for now, preliminary results not promising

### Final using fish spp as exogenous and continuous:
# Lake and Gedda show negative relationships. marginal R2= 11 and 11
M2 = list(
  lme(log_OringTOT~Average_air_temperature+Distance_to_sea+Wetted_width+Av_depth+log_LWD+SUB1+Julian_date+Slope_percent
      +GEdda+Lake,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2),
  lme(log_LWD~Average_air_temperature+Distance_to_sea+Av_depth+Wetted_width+Year+Julian_date+Slope_percent,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2))
sem.fit(M2,AV2)
sem.coefs(M2,AV2)
sem.model.fits(M2)
sem.plot(M2, AV2)
sem.coefs(M2,AV2,standardize = "scale") 
sem.coefs(M2,AV2,standardize = "range")

# brook trout (becro) and grayling (Harr) (competitiors) as explanatory factors: not signif
# LAxTOT or Cottus seems to have a weak positive effect on Öring, which does not make sense
# including a correlation between SUB1 and slope doesn't change anything
# including SUB as endogenous explained by slope doesn't work smoothly, I d need to add many other links
# include interactions: 
# a) between predators (Gädda or lake) and LWD (on öring): not significant
# b) between stream width and LWD (on öring): not significant
# are there abiotics that can potentially be influenced by LWD? depth or width (LWD can create pools), but link is negative, 
# so causal link has to go from depth to LWD
# test interaction between air temperature or latitude (instället air temp) and pike on öring: both not signif
# interaction Julian date*distance to sea on LWD and on öring:nope
# interaction between competitors (LaxTOT,BEcrOTOT, HarrTOT,Cottus_spp) and environmental conditions 
# (slope, depth, width, temp): none of the possible conbination is signif
# test effects of number fish spp on öring:  signif but has a positive effects! Skip it?


### including migration type as continuous. 
# I use dataset where I excluded NA for migration type at site level. The above model works fine, explained 
# variation is now 12% for both öring and LWD (without including migration type). When I add:
# a)migration type: signif and positive, where do I end up?
M2 = list(
  lme(log_OringTOT~Average_air_temperature+Wetted_width+Av_depth+log_LWD+SUB1+Julian_date+Slope_percent
      +GEdda+Lake+Type_migration_continuous,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV_Migration_NAremoved2),
  lme(log_LWD~Average_air_temperature+Distance_to_sea+Av_depth+Wetted_width+Year+Julian_date+Slope_percent,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV_Migration_NAremoved2))
sem.fit(M2,AV_Migration_NAremoved2)
sem.coefs(M2,AV_Migration_NAremoved2)
sem.model.fits(M2)
sem.plot(M2, AV_Migration_NAremoved2)

#b)interaction type of migration and number of spp? dist to sea and year not signif for öring but necessary 
# for good model fit, I use correlation. However, interaction has negative effects on öring, but n. of spp is 
# positively linked to öring. Does it make sense?
M2 = list(
  lme(log_OringTOT~Average_air_temperature+Wetted_width+Av_depth+log_LWD+SUB1+Julian_date+Slope_percent
      +GEdda+Lake+Type_migration_continuous*Number_of_fish_species,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV_Migration_NAremoved2),
  lme(log_LWD~Average_air_temperature+Distance_to_sea+Av_depth+Wetted_width+Year+Julian_date+Slope_percent,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV_Migration_NAremoved2))
sem.coefs(M2,AV_Migration_NAremoved2)
sem.fit(M2,AV_Migration_NAremoved2, corr.errors = c("Distance_to_sea~~log_OringTOT","Year~~log_OringTOT"))
sem.model.fits(M2)

# c)interaction migration type as continuous*depth or migration type as continuous*width : signif! 
# In both cases: dist to sea and year not signif for öring but necessary for good model fit
# deleting link but using correlation:
M2 = list(
  lme(log_OringTOT~Average_air_temperature+Wetted_width+log_LWD+SUB1+Julian_date+Slope_percent
      +GEdda+Lake+Type_migration_continuous*Av_depth,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV_Migration_NAremoved2),
  lme(log_LWD~Average_air_temperature+Distance_to_sea+Av_depth+Wetted_width+Year+Julian_date+Slope_percent,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV_Migration_NAremoved2))
sem.fit(M2,AV_Migration_NAremoved2, corr.errors = c("Distance_to_sea~~log_OringTOT","Year~~log_OringTOT"))
sem.coefs(M2,AV_Migration_NAremoved2)
sem.model.fits(M2)

M2 = list(
  lme(log_OringTOT~Average_air_temperature+Av_depth+log_LWD+SUB1+Julian_date+Slope_percent
      +GEdda+Lake+Type_migration_continuous*Wetted_width,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV_Migration_NAremoved2),
  lme(log_LWD~Average_air_temperature+Distance_to_sea+Av_depth+Wetted_width+Year+Julian_date+Slope_percent,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV_Migration_NAremoved2))
sem.fit(M2,AV_Migration_NAremoved2, corr.errors = c("Distance_to_sea~~log_OringTOT","Year~~log_OringTOT"))
sem.coefs(M2,AV_Migration_NAremoved2)
sem.plot(M2, AV_Migration_NAremoved2)
sem.model.fits(M2)

# including link from depth or width to type of migration:
M2 = list(
  lme(log_OringTOT~Average_air_temperature+Wetted_width+Av_depth+log_LWD+SUB1+Julian_date+Slope_percent
      +GEdda+Lake+Type_migration_continuous,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV_Migration_NAremoved2),
  lme(log_LWD~Average_air_temperature+Distance_to_sea+Av_depth+Wetted_width+Year+Julian_date+Slope_percent,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV_Migration_NAremoved2))
sem.fit(M2,AV_Migration_NAremoved2)
sem.coefs(M2,AV_Migration_NAremoved2)
sem.model.fits(M2)
sem.plot(M2, AV_Migration_NAremoved2)


plot(AV_Migration_NAremoved2$Wetted_width,AV_Migration_NAremoved2$Type_migration_continuous)
plot(AV_Migration_NAremoved2$Av_depth,AV_Migration_NAremoved2$Type_migration_continuous)
cor.test(AV_Migration_NAremoved2$Wetted_width,AV_Migration_NAremoved2$Type_migration_continuous, method="spearman")
cor.test(AV_Migration_NAremoved2$Av_depth,AV_Migration_NAremoved2$Type_migration_continuous, method="spearman")

### using fish spp as exogenous and binary: brecro have negative effects.Marginal R=10 and 11
M2 = list(
  lme(log_OringTOT~Average_air_temperature+Distance_to_sea+Wetted_width+Av_depth+log_LWD+SUB1+Julian_date+Slope_percent
      +BEcrOTOT_KLASS,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2),
  lme(log_LWD~Average_air_temperature+Distance_to_sea+Av_depth+Wetted_width+Year+Julian_date+Slope_percent,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2))
sem.fit(M2,AV2)
sem.coefs(M2,AV2)
sem.model.fits(M2)
sem.plot(M2, AV2)
sem.coefs(M2,AV2,standardize = "scale") 
sem.coefs(M2,AV2,standardize = "range")
# lake and herr are not signif. LAx and cottus have a positive effects which does not seem correct
# there is a negative relationship from gedda to LWD..if modeled as correlation, AIC decreases of ca 6 units 



#######partial correlation plots:
#plot partial residuals plots:
partial.resid(.formula = log_OringTOT ~ Average_air_temperature, M2, AV2)
partial.resid(.formula = log_OringTOT ~ Distance_to_sea, M2, AV2)
partial.resid(.formula = log_OringTOT ~ Wetted_width, M2, AV2)
partial.resid(.formula = log_OringTOT ~ Av_depth, M2, AV2)
partial.resid(.formula = log_OringTOT ~ log_LWD, M2, AV2)
partial.resid(.formula = log_OringTOT ~ Julian_date, M2, AV2)
partial.resid(.formula = log_OringTOT ~ GEdda, M2, AV2)
partial.resid(.formula = log_LWD ~ Average_air_temperature, M2, AV2)
partial.resid(.formula = log_LWD ~ Distance_to_sea, M2, AV2)
partial.resid(.formula = log_LWD ~ Wetted_width, M2, AV2)
partial.resid(.formula = log_LWD ~ Av_depth, M2, AV2)
partial.resid(.formula = log_LWD ~ Julian_date, M2, AV2)
partial.resid(.formula = log_LWD ~ Year, M2, AV2)
# to calculate manually partial regression plots have a look at point level analysis PF
# or with visreg. maybe difference in dot's position and axis labels are due to the fact that piecewise account for all 
# models in the basis set. however, if that was the reason, endogenous facxtors should look the same, but they don't
M1<-lme(log_OringTOT~Average_air_temperature+Distance_to_sea+Wetted_width+Av_depth+log_LWD+SUB1+Julian_date+GEdda,
        random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2)
visreg(M1, "log_LWD")
#or better layout:
visreg(M1,"log_LWD",type="conditional",line=list(col="red"),points=list(cex=1, pch=16),xlab="Average_air_temperature")
visreg(M1,"Average_air_temperature",type="contrast",line=list(col="red"),points=list(cex=1, pch=16),xlab="Average_air_temperature")

# using fish spp (inlcuding only predators: gedda and lake) as endogenous: logtranform them
M2 = list(
  lme(log_OringTOT~Average_air_temperature+Distance_to_sea+Wetted_width+Av_depth+log_LWD+SUB1+Julian_date+Slope_percent
      +log_GEdda+log_Lake,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2),
  lme(log_GEdda~Distance_to_sea+Wetted_width+Av_depth+log_LWD+Year+Slope_percent,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2),
  lme(log_Lake~Distance_to_sea+Slope_percent+log_GEdda,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2),
  lme(log_LWD~Average_air_temperature+Distance_to_sea+Av_depth+Wetted_width+Year+Julian_date+Slope_percent,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2))
sem.fit(M2,AV2)
sem.coefs(M2,AV2)
sem.model.fits(M2)
sem.plot(M2, AV2)
sem.coefs(M2,AV2,standardize = "scale") 
sem.coefs(M2,AV2,standardize = "range")
# adding competitors:
# interaction lax or harr or cottus with slope explaining öring: does not converge for Harr, not signif for lax and cottus

# if I use VIX:
# 1)on the top of the factors that are already in the model: vix has positive link to öring, negative to gädda and lake,
# which are not affecting öring any longer, but they are still required in the model to have a Fisher C with p>0.05 (!)
M2 = list(
  lme(log_OringTOT~Average_air_temperature+Distance_to_sea+Wetted_width+Av_depth+log_LWD+SUB1+Julian_date+Slope_percent
      +VIX,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2),
  lme(log_GEdda~Distance_to_sea+Wetted_width+Av_depth+log_LWD+Year+Slope_percent+VIX,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2),
  lme(log_Lake~Distance_to_sea+Slope_percent+log_GEdda+VIX,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2),
  lme(log_LWD~Average_air_temperature+Distance_to_sea+Av_depth+Wetted_width+Year+Julian_date+Slope_percent,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2))
sem.fit(M2,AV2)
sem.coefs(M2,AV2)
sem.model.fits(M2)
sem.plot(M2, AV2)
# 2) but delete factors included in the estimation of VIX such as air temp, width, slope as explanatory for only fish:
# it is not enough to explain them.If those factors are removed also as explanatory for LWD:bad, R2 for LWD decrease a lot
M2 = list(
  lme(log_OringTOT~Distance_to_sea+Av_depth+log_LWD+SUB1+Julian_date+log_GEdda+log_Lake+VIX,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2),
  lme(log_GEdda~Av_depth+log_LWD+Year+VIX,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2),
  lme(log_Lake~Distance_to_sea+log_LWD+log_GEdda+VIX,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2),
  lme(log_LWD~Distance_to_sea+Av_depth+Year+Julian_date,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2))
sem.fit(M2,AV2)
sem.coefs(M2,AV2)
sem.model.fits(M2)

# compare with the simple mixed model öring vs VIX: check how to compare fit!
M1<-lme(log_OringTOT~VIX,random=~1|River_name/Catchment_number, corAR1(form=~Year), method="ML",data=AV2)
summary(M1)
anova(M1)
M0<-lme(log_OringTOT~1,random=~1|River_name/Catchment_number, corAR1(form=~Year),method="ML", data=AV2)
anova(M1,M0)

M1<-lme(log_OringTOT~VIX,random=~1|River_name/Catchment_number, corAR1(form=~Year), method="REML",data=AV2)
M0<-gls(log_OringTOT~VIX, corAR1(form=~Year|River_name/Catchment_number),method="REML", data=AV2)
M0<-lme(log_OringTOT~VIX, random=~1|River_name/Catchment_number,method="REML", data=AV2)
anova(M1,M0)

plot(AV2$VIX,AV2$log_OringTOT)
sem.model.fits(M1)

# using pike and lake as endogenous and binary:
M2 = list(
  lme(log_OringTOT~Average_air_temperature+Distance_to_sea+Wetted_width+Av_depth+log_LWD+SUB1+Julian_date+Slope_percent
     ,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2),
  lme(GEdda_KLASS~Wetted_width+log_LWD+Year+Slope_percent,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2),
  lme(Lake_KLASS~Average_air_temperature+Wetted_width+Distance_to_sea+Slope_percent+SUB1+GEdda_KLASS,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2),
  lme(log_LWD~Average_air_temperature+Distance_to_sea+Av_depth+Wetted_width+Year+Julian_date+Slope_percent,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV2))
sem.fit(M2,AV2)
sem.coefs(M2,AV2)
sem.model.fits(M2)
sem.plot(M2, AV2)
sem.coefs(M2,AV2,standardize = "scale") 
sem.coefs(M2,AV2,standardize = "range")




########## on other datsets:
# on AV: too many NAs, it fails
M2 = list(
  lme(log(OringTOT+1)~log(LWD+1)+Av_depth+Wetted_width+Distance_to_sea+Average_air_temperature+SUB1+GEdda,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV),
  lme(log(LWD+1)~Distance_to_sea+Average_air_temperature+Av_depth+Wetted_width+Year,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),data=AV))
sem.fit(M2,AV)
sem.coefs(M2,AV)
sem.model.fits(M2)
sem.plot(M2, AV)

# taking away temporal variability:
# On AVyear (averages of years) without NAs:
M2 = list(
  lme(log(OringTOT+1)~log(LWD+1)+Av_depth+Wetted_width+Distance_to_sea+Average_air_temperature+SUB1+GEdda,
      random=~1|River_name/Catchment_number, na.action=na.omit, data=AVyear),
  lme(log(LWD+1)~Distance_to_sea+Average_air_temperature+Av_depth+Wetted_width,
      random=~1|River_name/Catchment_number, na.action=na.omit, data=AVyear))
sem.fit(M2,AVyear)
sem.coefs(M2,AVyear)
sem.model.fits(M2)
sem.plot(M2, AVyear)
# On AV2009 without NAs:
M2 = list(
  lme(log(OringTOT+1)~Av_depth+Wetted_width+Distance_to_sea+Average_air_temperature+GEdda,
      random=~1|River_name/Catchment_number, na.action=na.omit, data=AV2009),
  lme(log(LWD+1)~Distance_to_sea+Average_air_temperature+Wetted_width,
      random=~1|River_name/Catchment_number, na.action=na.omit, data=AV2009))
sem.fit(M2,AV2009)
sem.coefs(M2,AV2009)
sem.model.fits(M2)
sem.plot(M2, AV2009)

# On AVOC(exclude sites sampled only once) without NAs:
M2 = list(
  lme(log(OringTOT+1)~log(LWD+1)+Av_depth+Wetted_width+Distance_to_sea+Average_air_temperature+SUB1+GEdda+Year,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),na.action=na.omit, data=AVOC),
  lme(log(LWD+1)~Distance_to_sea+Average_air_temperature+Av_depth+Wetted_width+Year,
      random=~1|River_name/Catchment_number, corAR1(form=~Year),na.action=na.omit, data=AVOC))
sem.fit(M2,AVOC)
sem.coefs(M2,AVOC)
sem.model.fits(M2)
sem.plot(M2, AVOC)


