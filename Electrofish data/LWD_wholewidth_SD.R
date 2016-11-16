rm(list=ls())
setwd("C:/Users/sedi0002/Google Drive/Dod ved/Electrofish data")
my<-read.table("LWD_wholewidth_SD3.txt",header=T)


head(my)
str(my)
summary(my)


# Libraries ---------------------------------------------------------------
library(ggplots2)
library(lattice)

# Defining variables type: -----------------------------------------------------

# convert to categorical variables:
my$Catchment_number<-as.factor(my$Catchment_number)
my$Whole_width_fished<-as.factor(my$Whole_width_fished)
my$DateYYYYMMDD<-as.factor(my$DateYYYYMMDD)

# binary variables, to convert or not into factors:
my$Abbor_KLASS<-as.factor(my$Abbor_KLASS)
my$Elrit_KLASS<-as.factor(my$Elrit_KLASS)
my$GÄKLASS<-as.factor(my$GÄKLASS)
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

# Exracting averages per river and year -----------------------------------
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


#### explore temporal autocorrelation:

plot(AV$OringTOT~AV$Year)

# I should do it within groups, that means within each river: I need to nest year within river
# (or catchment maybe?)
# 1) plots: plots fish~year for subsets corresponding to one river at a time. Also,
# use acf for that same subset

plot(AV$OringTOT~AV$Year,subset=(AV$Catchment_number=="48"), 
     col=as.numeric(AV$River_name))
plot(AV$OringTOT~AV$Year,subset=(AV$Catchment_number=="1"), 
     col=as.numeric(AV$River_name))
plot(AV$OringTOT~AV$Year,subset=(AV$River_name=="SvartOn"))

acf(AV$OringTOT,subset=(AV$River_name=="SvartOn"))
acf(AV$OringTOT,subset=(AV$Catchment_number=="1"))

#altre opzioni ma troppo pesanti
# xyplot(OringTOT ~ Year|River_name, data=AV,  occchio a plottare, rischi il crash
#panel = function (x,y){
#  panel.grid(h=-1,v=2)
#  panel.points(x, y, col = 1)
#  panel.loess(x, y, span = 0.5, col =1, lwd =2)})
#coplot(OringTOT~Year|Catchment_number,panel = panel.smooth)

# need to get the library, det funkar inte
ggplot(AV, aes(x = Year, y = OringTOT)) +
  geom_point(aes(colour = Catchment_number), size = 2)
plot(AV$OringTOT~AV$Year, col=as.numeric(AV$Catchment_number))

############# cmq a occhio non mi sembra ci sia corr temporale, ma esplora more



# 2) model:  corCompSymm(form=~Year|River) or corAR1(form=~Year|River) or
# use a spatial correaltion function, it copes better with missing values and irregularly 
# spaced data, such as: corLin(form=~Year|River, nugget=T), corGaus, corExp, corSpher
# check moe details on the notes, e.g. need to check the assumption that the time series are not correlated,
# by looking at correlation between residuals of the model coming from each time serie

# try first with the simplest correlation structure. these should be quivalent:
# corCompSymm(form=~Year|River) and random=~Year|River .
#Credo. Explora also random=~Year|River/catchment



# if I don't manage to model temporal correlation, consider averages


