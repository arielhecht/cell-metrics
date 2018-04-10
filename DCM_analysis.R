DCM_analysis<-function(plate,key,DCM_calibration){
  
  #This function calibrates measurements of OD700 to dry cell mass, in units of mg/L.
  plate<-merge(plate,key,by=c("Exp_number","Plate","Well"))
  plate<-select(plate,Exp_number:Dilution,Category,Well_id)
  
  #Background subtraction
  background_OD<-mean(plate[plate$Category=="Blank",]$OD700)
  plate$OD700<-plate$OD700-background_OD
  
  DCM_OD700_slope<-DCM_calibration[1]
  DCM_OD700_intercept<-DCM_calibration[2]
  
  #Application of the calibration curve, calculating dry cell mass in mg/L. Assume that all OD measurements are made at a volume of 200 uL.
  plate$dcm<-(plate$OD700*DCM_OD700_slope-DCM_OD700_intercept)*plate$Dilution
  
  return(plate)
  
}
