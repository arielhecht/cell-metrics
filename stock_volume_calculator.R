stock_volume_calculator<-function(design_table, stock_list, media_volume, starter_OD){
  #For every entry in stock_list that matches a column name in design_table, calculate the volume of stock solution of that media component
  #needed to mix to generate a media stock of media_volume (all volume units in microliters)
  
  total_volume=0
  
  for (j in 1:length(stock_list)){
    if (names(stock_list)[j] %in% names(design_table)){
      
      varname1<-names(stock_list[j])
      varname2<-paste(names(stock_list[j]),"volume",sep="_")
      
      design_table[[varname2]]<- signif ( as.numeric(as.character(design_table[[varname1]])) * media_volume / stock_list[[j]], 3 )
      
      total_volume = total_volume + design_table[[varname2]]
      
    }
  }
  
  #Calculate the amount of water needed to achieve the desired volume of media, by subtracting out the volume of all the other media components.
  design_table$Water_vol = signif ( media_volume - total_volume , 3)
  
  
  #Determine the volume of starter culture needed to inoculate the experimental culture. The Inoculum_amount column has the OD of the experimental culture upon inoculation
  #If we multiply the volume of the container by the fill volume fraction, and then multiply by the Inoculum amount, and divide by the OD of the starter culture,
  #we are left with the inoculum volume, which is the amount, in uL, that needs to used as the incoulum volume. 
  
  if(!all(is.na(design_table$MTP_volume))){
    
    design_table$Inoculum_vol = as.numeric(as.character(design_table$MTP_volume)) * as.numeric(as.character(design_table$MTP_fill_volume)) * 
      as.numeric(as.character(design_table$Inoculum_amount)) / starter_OD * 1000
    
  }
  
  if(!all(is.na(design_table$Flask_volume))) {
    
    design_table$Inoculum_vol_ = as.numeric(as.character(design_table$Flask_volume)) * as.numeric(as.character(design_table$Flask_fill_volume)) * 
      as.numeric(as.character(design_table$Inoculum_amount)) / starter_OD * 1000
    
  }
  
  return(design_table)
}