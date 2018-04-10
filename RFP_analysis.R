RFP_analysis<-function(plate,key){
  
  #This function takes in raw measurements of fluorescence, and calculates the RFP titer, based on user-supplied values for dilution and volume.
  plate<-merge(plate,key,by=c("Exp_number","Plate","Well"))
  plate<-select(plate,Exp_number:Dilution,Category,Well_id,Culture_vol)
  
  #Background subtraction
  background_AFU<-mean(plate[plate$Category=="Blank",]$Fluorescence)
  plate$Fluorescence<-plate$Fluorescence-background_AFU
  
  #Take background-subtracted fluorescence, multiply by the dilution factor, divide by the sample volume (in uL), then convert from uL to L
  #Titer is fluorescence, in AFU, per L. I divide it by 1e12 to make the units more tractable.
  plate$titer<-plate$Fluorescence*plate$Dilution/200*1000000/1e12
  
  return(plate)
  
}
