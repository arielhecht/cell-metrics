Lycopene_analysis<-function(plate,key,Lycopene_calibration){
  
  #This function takes in a set of lycopene absorbance measurements (at 400, 447, 474, 505 and 600 nm), 
  #from which it calculates the concentration of lycopene at the three absorbance peaks (447, 474 and 505 nm) in units of mg/L. 
  #This requires that the sample volume used to calculate the calibration curve is the same as the sample volume analyzed during the assay.
  
  plate<-lycopene_raw  #FOR TESTING ONLY

  #STEP ONE: Merge the experimental data with the master key.
  plate_melt<-plate %>%
    merge(key,by=c("Exp_number","Plate","Well")) %>%
    select(Exp_number:Assay_vol,Category,Well_id) %>%
    gather('nm400','nm449','nm475','nm507','nm600',key="id",value="Absorbance")
  plate_melt$Wavelength<-as.numeric(substr(plate_melt$id,3,5))
  
  
  #STEP TWO: Subtract acetone background:
  background.df<-data.frame(unique(plate_melt[plate_melt$Category=="Blank",]$Wavelength),
                            tapply(plate_melt[plate_melt$Category=="Blank",]$Absorbance,
                                   plate_melt[plate_melt$Category=="Blank",]$Wavelength,mean))
  colnames(background.df)<-c("Wavelength","Back_absorbance")
  
  plate_melt<-merge(plate_melt,background.df)
  plate_melt$Absorbance_bs<-plate_melt$Absorbance-plate_melt$Back_absorbance
  
  plate_melt$Lycopene<-TRUE
  for(j in 1:dim(plate_melt)[1]) if(plate_melt$Category[j]=="Blank") plate_melt$Lycopene[j]<-FALSE
  
  #plate_melt$Absorbance_corr<-plate_melt$Absorbance_bs

  
  #STEP THREE: Correct for background noise:

  unique_wells<-unique(plate_melt$Well_id)
  plate_melt$Absorbance_corr<-NA
  
  for (j in 1:length(unique_wells)){
    id_var<-unique_wells[j]
    A600<-filter(plate_melt, Well_id==id_var, id=="nm600")$Absorbance_bs
    for(k in 1:nrow(plate_melt)){
      if(plate_melt$Well_id[k]==id_var){
        plate_melt$Absorbance_corr[k]<-plate_melt$Absorbance_bs[k]-A600
      }
    }
  }

   
  #STEP FOUR: Calculate lycopene concentration in units of mg/L from the calibration values:
  cal.vals<-data.frame(c(400,449,475,507,600),
                       c(NA,NA,-Lycopene_calibration[4],-Lycopene_calibration[6],NA),
                       c(NA,NA,Lycopene_calibration[3],Lycopene_calibration[5],NA))
  colnames(cal.vals)<-c("Wavelength","Intercept","Slope")
  plate_melt<-merge(plate_melt,cal.vals)

  plate_melt$titer<-(plate_melt$Absorbance_corr-plate_melt$Intercept)/plate_melt$Slope*
    plate_melt$Assay_vol/plate_melt$Culture_vol

  return(plate_melt)

}