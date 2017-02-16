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
                    ),list(my$River_name,my$Year,my$Catchment_number),mean)
names(AV)<-c("River_name", "Year", "Catchment_number",
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

# Exploring and plotting ---------------------------------------------------------------
##### The fun starts now:

###wild exploration:
dotchart(AV$OringTOT)
dotchart(AV$BEcrOTOT)
dotchart(AV$Abbor)
dotchart(AV$Eel)
dotchart(AV$LaxTOT )
dotchart(AV$Sticklebacks)

plot(AV$OringTOT~AV$LWD)
plot(AV$OringTOT_KLASS~AV$LWD)# more promising with binarian varirables indeed...
plot(AV$LaxTOT~AV$LWD)
plot(AV$LaxTOT_KLASS~AV$LWD)

plot(AV$OringTOT~AV$Catchment_number)

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

# temo corr:
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


# !!! remember to check the assumption that the time series are not correlated,
# by looking at correlation between residuals of the model coming from each time serie


# next step is to consider binary vs not binary variables and start serious modeling
# also, need to check collinearity of explanatory factors, maybe use a PCA or matrix (found online)


# explore collinearity of predictors ------------------------------------

#take dataframe with only environmental data and calculate model:
pgd<-AV[,4:22]
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

################################# still to do:


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

