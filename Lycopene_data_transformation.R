
#Cell metrics project, lycopene data, analysis notebook

#This notebook reads in the data from the lycopene cell metrics experiments (e26-28, e30-32, e37-38, e44-48),
#and outputs the summary tab file csv file, which is the tidy data frame with all factors in columns and all 256 observations in rows
#original file name "180213 cm e26-48 lycopene analysis.R"

dirnm<-"/Users/ahh1/Documents/My SugarSync/Cell Metrics/Experimental Data/R analysis files/R files for upload"
setwd(dirnm)

library(tidyverse)
library(readxl)
source("Lycopene_analysis.R")
source("DCM_analysis.R")
#source("Lycopene_background_correction.R")
source("Main_effects_calculator.R")

#=====================
#READ IN DATA:
#=====================

OD_raw<-read_excel("Supplementary Data File S4.xlsx",sheet = 1, col_names = T)
lycopene_raw<-read_excel("Supplementary Data File S4.xlsx",sheet = 2, col_names = T) 
key<-read_excel("Supplementary Data File S4.xlsx",sheet = 4, col_names = T)  


#=====================
#CALIBRATION VALUES:
#=====================

#Dry cell calibration curve values. x-axis: OD700, y-axis: Dry cell mass per volume of culture (g/L)
#Calibration values from cm-e42, averaging the runs in cm-e10 and cm-e41
DCM_OD700_slope <- 1.343289
DCM_OD700_intercept <- (-0.016317)
dcm_calibration<-c(DCM_OD700_slope,DCM_OD700_intercept)

#Lycopene calibration curve values. x-axis: Lycopene concentration (mg/L), y-axis: Absorbance (AU)
#Calibration values from cm-e41, averaging the runs in cm-e18, and cm-e41
Lyc_449_slope <- 0.08366689
Lyc_449_intercept <- (-0.008434176)
Lyc_475_slope <- 0.121680
Lyc_475_intercept <- 0.002021
Lyc_507_slope <- 0.108991
Lyc_507_intercept <- 0.001693


lycopene_calibration<-c(Lyc_449_slope,Lyc_449_intercept,
                        Lyc_475_slope,Lyc_475_intercept,
                        Lyc_507_slope,Lyc_507_intercept)


#=====================
#DATA PROCESSING STEPS
#=====================

#Convert the raw lycopene absorbance to lycopene concentration. Need the raw lycopene absorbance, 
#the master key (to know which wells are samples and which are blanks), and the calibration values.
lycopene_analyzed<-Lycopene_analysis(lycopene_raw,key,lycopene_calibration)

#Do the same for the OD values.
od_analyzed<-DCM_analysis(OD_raw,key,dcm_calibration)

#Merge the calculated lycopene concentration with dry cell mass.
#Calculate yield, the mass of lycopene produced per biomass. Result has units of, mg/g. (titer in mg/L, dcm in g/L).
data_merge<-merge(lycopene_analyzed,select(od_analyzed,Well_id,dcm),by="Well_id")%>%
  mutate(yield = titer/dcm)%>%
  filter(Category!="Blank")

#============================================
#Create a summary table, summarizing key metrics by well and by type.
#============================================
#Summary_tab is the full experimental design table. Columns are factor levels and responses, rows are runs
#Recode 
summary_tab<-data_merge%>%
  group_by(Well_id)%>%
  summarize_at(c('titer','dcm','yield'), mean, na.rm=T)%>%
  merge(key, by="Well_id")%>%
  arrange(desc(Category),Exp_number,Run_number)%>%
  rename(Well_bottom=MTP_bottom, 
         Well_cover=MTP_cover, 
         Well_fill_volume=MTP_fill_volume, 
         Well_volume=MTP_volume,
         Shake_speed=MTP_shake_speed)%>%
  write_excel_csv("Lycopene experimental data table.csv")



