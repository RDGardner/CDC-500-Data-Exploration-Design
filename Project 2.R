#Ricca Callis
#EN 605.662 Data Visualization
#Project 2 - Data Exploration and Design
#Data from CDC 500 Cities Project: https://chronicdata.cdc.gov/browse?category=500+Cities

#Project Instructions:

#I. Purpose: The purpose of this assignment is to introduce you to the process of exploring 
#and visually analyzing data without even having to develop a visualization tool. You will 
#pick a domain and dataset that you are interested in. The data should have at least 10 variables
#(i.e. columns) and 1,000 records (i.e. rows). The purpose of this assignment is to design different 
#visualizations to illustrate individual aspects of the dataset under consideration.

#II. Task:Find a dataset of interest to you. Minimum dataset: 10 variables x 1000 records
#Explore and analyze the data using Excel, R, Python, Matlab, SPSS, Google Spreadsheets, or a calculator. 
#To do this, please create a table that describes each of the data elements, list their category (nominal, ordinal, quantitative, etc…),
#and provide some statistics about each of the data elements (e.g. min, max, std, mode, categories, etc…)
#List five analytical questions that users examining the data might be wondering.
#By using markers, pen and/or pencils, please sketch different visualizations that can be used to address some of the analytical questions. 
#Specifically, of the five analytical questions, select three of them and sketch three different visualizations for each of them. (i.e. 3 
#analytical questions x 3 visualizations = 9 visualizations). No programming needed.
#Write one paragraph for each of the 9 drawings / sketches explaining the design, the purpose, and the logic behind your design.

#IV. What to submit
#Document (2 - N pages) describing the data, analytical questions, and the 9 visualizations designed for three of the analytical questions. 
#Document must include screenshots of the drawings, visualizations, or illustrations.
#Submit document through Blackboard. Please use the following file format: your_lastname_project02.docx or your_lastname_project02.pdf

#V. Grading
#Students will be evaluated based on the quality of the work, logic, clarity, and effort put into the design. 
#Specifically,What dataset was used? There are no right or wrong datasets, but put effort in finding something of interest to you while meets 
#the data requirements.
#How well the data was explored and described? Mean, standard deviation, median, max, etc…? Is there a table listing the elements?
#Quality of the five analytical questions that users examining the data might be wondering.
#Logic of the 9 designs, sketches, pictures, or visualizations proposed to explore the dataset
#Write-up and overall justification.

#Variables:

#ACCESS2_AdjPrev : prevalence estimate of current lack of health insurance
#ARTHRITIS_AdjPrev : estimate for age-adjusted prevalence of arthritis
#BINGE_AdjPrev : estimate for age-adjusted prevalence of binge drinking
#BPHIGH_AdjPrev : estimate for age-adjusted prevalence of high blood pressure
#BPMED_AdjPrev : estimate for age-adjusted prevalence of taking medicine for high blood pressure control [prevention]
#CANCER_AdjPrev : estimate for age-adjusted prevalence of cancer (excluding skin cancer) among adults aged >=18 years
#CASTHMA_AdjPrev : estimate for age-adjusted prevalence of current asthma
#CHD_AdjPrev : estimate for age-adjusted prevalence of coronary heart disease
#CHECKUP_AdjPrev : estimate for age-adjusted prevalence of visits to doctor for routine checkup within the past year [prevention]
#CHOLSCREEN_AdjPrev : estimate for age-adjusted prevalence of cholesterol screening [prevention]
#COLON_SCREEN_AdjPrev : estimate for age-adjusted prevalence of fecal occult blood test, sigmoidoscopy, or colonoscopy
#COPD_AdjPrev : estimate for age-adjusted prevalence of chronic obstructive pulmonary disease
#COREM_AdjPrev : estimate for age-adjusted prevalence of older adult men aged >=65 years who are up to date on a core set of clinical preventive services: Flu shot past year, PPV shot ever, Colorectal cancer screening [prevention]
#COREW_AdjPrev : same as above, for women [prevention]
#CSMOKING_AdjPrev : estimate for age-adjusted prevalence of current smoking
#DENTAL_AdjPrev : estimate for age-adjusted prevalence of visits to dentist [prevention]
#DIABETES_AdjPrev : estimate for age-adjusted prevalence of diagnosed diabetes
#HIGHCHOL_AdjPrev : estimate for age-adjusted prevalence of high cholesterol
#KIDNEY_AdjPrev : estimate for age-adjusted prevalence of chronic kidney disease
#LPA_AdjPrev : estimate for age-adjusted prevalence of no leisure-time physical activity
#MAMMOUSE_AdjPrev : estimate for age-adjusted prevalence of mammography
#MHLTH_AdjPrev : stimate for age-adjusted prevalence of mental health not good for >=14 days
#OBESITY_AdjPrev : estimate for age-adjusted prevalence of obesity
#PHLTH_AdjPrev : estimate for age-adjusted prevalence of physical health not good for >=14 days
#SLEEP_AdjPrev : estimate for age-adjusted prevalence of sleeping less than 7 hours
#STROKE_AdjPrev : estimate for age-adjusted prevalence of stroke

#Standard libraries
library("ggplot2")
library(readxl)
library (MPV)
library (olsrr)
library("ggpubr")
library("Hmisc")
library(caret)
library (lattice)
library(leaps)
library(MASS)
library(alr3)
library("ggplot2")
library(readxl)
library(dplyr)
library(gridExtra)
library(RColorBrewer)
library(ggthemes)
library(viridis)
library("plotly")
library(corrplot)
library(maps)
library(alr3)
library(rms)
library(ppcor)
library(ggthemes)
library(data.table)
library(ISLR)
library(tibble)
library(aod)
library(tidyverse)
library(modelr)
library(broom)
library(WVPlots)
library(lmtest)
library("dummies")
library(lpSolve) #lp_solve #linprog #solveLP() function
library(lpSolveAPI)
library(Rglpk) #GLPK
#library(Rsymphony) #Symphony
library(ROI)
library(CVXR)
library(ompr)
library(fPortfolio)
library(pastecs)
library(summarytools)

# Seg  working directory
#setwd("/Users/riccacallis/Desktop/JHU/Data Science/Data Visualization/Project 2/input")

df<- read.csv(file ="/Users/riccacallis/Desktop/JHU/Data Science/Data Visualization/Project 2/input/500_Cities_CDC.csv",sep=',',stringsAsFactors=F)
features_1<-c("StateAbbr","PlaceName","PlaceFIPS","Population2010","Geolocation")
# Look at the first six rows
head(df)
# Look at all data & attach it
View(df)
attach(df)

#select all adjusted measurements
df2<-data.frame(cbind(df %>% select(.dots = features_1), df %>% select(grep("_AdjPrev", colnames(df)))))
colnames(df2)[1:5]<-features_1
View(df2)

#get longitude and latitude
df2$long<-sapply(df2$Geolocation, function(x) as.double(gsub("\\)","",strsplit(x,",")[[1]][2])))
df2$lat<-sapply(df2$Geolocation, function(x) as.double(gsub("\\(","",strsplit(x,",")[[1]][1])))
View(df2)

#Descriptive Statistics
#Summary Statistics
summary(df)
describe(df)
stat.desc(df)

library(psych)
vars<-select(df2, 6:33)
describe(vars)

#Individual Variables: Stroke
fivenum(STROKE_AdjPrev)
basicStats(STROKE_AdjPrev)
mean(STROKE_AdjPrev)
var(STROKE_AdjPrev)
stdev(STROKE_AdjPrev)

#Coefficient of Variation: Stroke
sd(df$STROKE_AdjPrev) / mean(df$STROKE_AdjPrev)

#Histogram: Stroke
hist(df$STROKE_AdjPrev)

ggplot(df) +
  aes(x = STROKE_AdjPrev) +
  geom_histogram()

#Boxplot: Stroke

boxplot(df$STROKE_AdjPrev)

boxplot(df$STROKE_AdjPrev ~ df$StateAbbr)
ggplot(df) +
  aes(x = StateAbbr, y = STROKE_AdjPrev) +
  geom_boxplot()

#Scatterplot: Stroke
plot(df$STROKE_AdjPrev, df$STROKE_AdjPrev)
ggplot(df) +
  aes(x = StateAbbr, y = STROKE_AdjPrev) +
  geom_point()

#Draw points on the qq-plot:
qqnorm(df$STROKE_AdjPrev)
#Draw the reference line:
qqline(df$STROKE_AdjPrev)
qqPlot(df$STROKE_AdjPrev, groups = df$StateAbbr)

#Density: Stroke
plot(density(df$STROKE_AdjPrev))

ggplot(df) +
  aes(x = STROKE_AdjPrev) +
  geom_density()

#Individual Variables: Access
fivenum(ACCESS2_AdjPrev)
basicStats(ACCESS2_AdjPrev)
mean(ACCESS2_AdjPrev)
var(ACCESS2_AdjPrev)
stdev(ACCESS2_AdjPrev)

#Coefficient of Variation: Access
sd(df$ACCESS2_AdjPrev) / mean(df$ACCESS2_AdjPrev)

#Histogram: Access
hist(df$ACCESS2_AdjPrev)

ggplot(df) +
  aes(x = ACCESS2_AdjPrev) +
  geom_histogram()

#Boxplot: Access
boxplot(df$ACCESS2_AdjPrev)
boxplot(df$ACCESS2_AdjPrev ~ df$StateAbbr)
ggplot(df) +
  aes(x = StateAbbr, y = ACCESS2_AdjPrev) +
  geom_boxplot()

#Scatterplot: Access
plot(df$ACCESS2_AdjPrev, df$ACCESS2_AdjPrev)
ggplot(df) +
  aes(x = StateAbbr, y = ACCESS2_AdjPrev) +
  geom_point()

#Draw points on the qq-plot:
qqnorm(df$ACCESS2_AdjPrev)
#Draw the reference line:
qqline(df$ACCESS2_AdjPrev)
qqPlot(df$ACCESS2_AdjPrev, groups = df$StateAbbr)

#Density: Access
plot(density(df$ACCESS2_AdjPrev))

ggplot(df) +
  aes(x = ACCESS2_AdjPrev) +
  geom_density()


#Aggregate the data per State
perState <- df2 %>% group_by(StateAbbr) %>% summarize(meanRisk = mean(STROKE_AdjPrev))
colnames(perState)<-c('state.abb','value')
states_map<-map_data("state")
statesNaming<-data.frame('state.name' = tolower(state.name), 'state.abb' = state.abb )

#Viridis palette
vir = viridis::inferno(n=256)
perState<-merge(perState,statesNaming,by='state.abb')
colnames(perState)[3]<-'region'
RES<-merge(perState,states_map,by='region')
p <- RES %>% ggplot(aes(map_id = region)) + 
  geom_map(aes(fill = value), map = states_map,color='white',size=.25) + 
  expand_limits(x = states_map$long, y = states_map$lat) + 
  theme_fivethirtyeight() + 
  scale_fill_gradientn(name='risk',colors = vir) +
  theme(panel.grid.major = element_blank(),
        axis.text=element_blank(),axis.ticks=element_blank(),
        legend.position = c(0.9, 0.02))

p<-p + labs(title="Stroke risk factor among adults aged ≥18 years in the U.S, 2015",
            subtitle="Alaska, Hawaii territories not represented on the map\nMin/Max values indicated") + 
  theme(
    plot.title=element_text(face="bold",hjust=.012,vjust=.8,colour="#3C3C3C",size=20),
    plot.subtitle=element_text(size=10, hjust=0, face="italic", color="black"))
p<-p + 
  annotate("text", x = -69.5,y = 26.5, label = "AK, Anchorage, risk : 2.6", size=3, colour="black") + 
  annotate("text", x = -70, y = 26.0, label = "HI, Honolulu, risk : 2.5", size=3, colour="black") +
  annotate("text", x = -69, y = 37, label = "Maryland(4.5)", size=3, colour="#F21A00") + 
  annotate("segment", x = -72, xend = -75, y = 37.2, yend = 38.2, colour = "#F21A00") +
  annotate("text", x = -108, y = 29, label = "Colorado(2.43)", size=3, colour="#46ACC8") + 
  annotate("segment", x = -108, xend = -108, y = 29.5, yend = 38.2, colour = "#46ACC8")
histo <- ggplotGrob(
  ggplot(data=perState,aes(x=value)) + 
    geom_histogram(bins=25,alpha=.75) + 
    theme_fivethirtyeight() + geom_vline(xintercept = mean(perState$value),color='black',lty=2) + 
    ggtitle('Stroke risk across all States') + 
    theme(plot.title=element_text(size=8))
)
p + annotation_custom(grob = histo, xmin = -130, xmax = -113, ymin = 23, ymax = 32)

#Counties
usMap<-ggplot() + 
  geom_map(data = states_map, map = states_map,aes(x = long, y = lat, map_id = region, group = group),fill = "white", color = "black", size = 0.1) + 
  theme_fivethirtyeight() + 
  theme(panel.grid.major = element_blank(),
        axis.text=element_blank(),axis.ticks=element_blank(),
        legend.position = c(0.9, 0.02))
p<-usMap + 
  geom_point(data=filter(df2,
                         StateAbbr!='AK' & StateAbbr!='HI'),
             aes(x=long,y=lat,color=STROKE_AdjPrev),alpha=.75,size=2) + 
  scale_color_gradientn(name='risk',colors = vir)
p<-p + labs(title="Stroke risk factor among adults aged ≥18 years in the U.S, 2019",
            subtitle="Alaska, Hawaii territories not represented on the map") + 
  theme(plot.title=element_text(face="bold",hjust=.012,vjust=.8,colour="#3C3C3C",size=20),
        plot.subtitle=element_text(size=10, hjust=0, face="italic", color="black"))
p<-p + 
  annotate("text", x = -69.5, y = 26.5, label = "AK, Anchorage, risk : 2.6", size=3, colour="black") + 
  annotate("text", x = -70, y = 26.0, label = "HI, Honolulu, risk : 2.5", size=3, colour="black")

histo<-ggplotGrob(ggplot(data=df2,aes(x= STROKE_AdjPrev)) + geom_histogram(bins=30,alpha=.75) + theme_fivethirtyeight() + geom_vline(xintercept = mean(df2$STROKE_AdjPrev),color='black',lty=2) + ggtitle('Stroke risk across all Cities') + theme(plot.title=element_text(size=8)))

p + annotation_custom(grob = histo, xmin = -130, xmax = -113, ymin = 23, ymax = 32)

#Correlation
corrplot(cor(df2 %>% select(-c(StateAbbr,PlaceName,PlaceFIPS, Population2010, Geolocation, long, lat, TEETHLOST_AdjPrev, PAPTEST_AdjPrev, MAMMOUSE_AdjPrev))),
         method="ellipse", 
         col=vir, 
         tl.cex=.8, 
         tl.col="black",
         addgrid.col="black",order="AOE")

#Risk Factor Stroke Correlations
cor(df2 %>% select(-c(StateAbbr,PlaceName,PlaceFIPS, Population2010, Geolocation, long, lat, TEETHLOST_AdjPrev, PAPTEST_AdjPrev, MAMMOUSE_AdjPrev)))[25,]

#Scatterplot for positive correlation
features<-c('BINGE_AdjPrev','CHOLSCREEN_AdjPrev','COREM_AdjPrev','COREW_AdjPrev','DENTAL_AdjPrev')
featuresPlot<-list()
for (i in 1:length(features)){
  x<-features[i]
  y<-'STROKE_AdjPrev'
  fact<-'CHECKUP_AdjPrev'
  featuresPlot[[i]]<-df2 %>% ggplot(aes_string(x=x,y=y,color=fact)) + 
    geom_point() + theme_fivethirtyeight() + 
    scale_color_gradientn(name='Check-Up ratio',colors = vir) + 
    ggtitle(paste0('Risk of Stroke vs. ',gsub("_AdjPrev","",x))) + 
    geom_smooth(method='lm',color='#46ACC8',alpha=.2,size=.5) + 
    theme(legend.position='right',legend.direction="vertical",plot.title=element_text(size=10))
}
do.call(grid.arrange,c(featuresPlot,ncol=2))

#Scatterplot for negative correlation
features<-c('BPHIGH_AdjPrev','CHD_AdjPrev','COPD_AdjPrev','CSMOKING_AdjPrev','DIABETES_AdjPrev','LPA_AdjPrev','OBESITY_AdjPrev','PHLTH_AdjPrev','SLEEP_AdjPrev')
featuresPlot<-list()
for (i in 1:length(features)){
  x<-features[i]
  y<-'STROKE_AdjPrev'
  fact<-'CHECKUP_AdjPrev'
  featuresPlot[[i]]<-df2 %>% ggplot(aes_string(x=x,y=y,color=fact)) + 
    geom_point() + theme_fivethirtyeight() + 
    scale_color_gradientn(name='Check-Up ratio',colors = vir) + 
    ggtitle(paste0('Risk of Stroke vs. ',gsub("_AdjPrev","",x))) + 
    geom_smooth(method='lm',color='#46ACC8',alpha=.2,size=.5) + 
    theme(legend.position='right',legend.direction="vertical",plot.title=element_text(size=10))
}
do.call(grid.arrange,c(featuresPlot,ncol=2))

#Lack of Insurance
p<-usMap + 
  geom_point(data=filter(df2,
                         StateAbbr!='AK' & StateAbbr!='HI'),
             aes(x=long,y=lat,color=STROKE_AdjPrev,size=ACCESS2_AdjPrev),alpha=.75) + 
  scale_color_gradientn(name='risk',colors = vir) + scale_size_continuous(breaks =seq(4,50,1)) + guides(size=F)
p<-p + labs(title="Stroke risk factor among adults aged ≥18 years in the U.S, 2015\nas a function of lack of insurance",
            subtitle="Alaska, Hawaii territories not represented on the map\nlack of insurance proportional to area of circles, the bigger the higher is the lack") + 
  theme(plot.title=element_text(face="bold",hjust=.012,vjust=.8,colour="#3C3C3C",size=20),
        plot.subtitle=element_text(size=10, hjust=0, face="italic", color="black"))

histo<-ggplotGrob(ggplot(data=df2,aes(x=ACCESS2_AdjPrev )) + geom_histogram(bins=30,alpha=.75) + theme_fivethirtyeight() + geom_vline(xintercept = mean(df2$ACCESS2_AdjPrev),color='black',lty=2) + ggtitle('Lack of insurance factor across all Cities') + theme(plot.title=element_text(size=8)))

p + annotation_custom(grob = histo, xmin = -130, xmax = -113, ymin = 23, ymax = 32)

#Geographic Considerations
df2$PopLevel<-ifelse(df2$Population2010<1e5,'low','high')
ggplot(data=df2,aes(x=ACCESS2_AdjPrev,y= STROKE_AdjPrev, color= CHECKUP_AdjPrev)) + 
  geom_point() + 
  theme_fivethirtyeight() + 
  scale_color_gradientn(name='Check-Up ratio',colors = vir) + 
  geom_smooth(method='lm',color='#46ACC8',alpha=.2,size=.5) + 
  facet_wrap(~PopLevel,ncol=2) + 
  ggtitle('STROKE_AdjPrev vs. ACCESS2_AdjPrev')

#HeatMap
df3 <- scale(df2 %>% dplyr::select(-c(StateAbbr,PlaceName,PlaceFIPS, Population2010, Geolocation, long, lat, PopLevel)))
heatmap(as.matrix(df3), col = vir,main = "",cexCol=1, margins=c(12,12))

