osmolality_calculator<-function(design_table, salt_list, osmotic_pressure_list){
  #This function calculates the osmotic pressure of the composed media, and, if that differs from the desired osmotic pressure defined in design_table,
  #calculates the concentration of salt needed to be added to the media to achieve the desired osmotic pressure.
  
  #The input salt list defines the identity, molar mass, and osmotic strength of the salt.
  #The osmolality list includes the osmotic pressure divided by the molar mass of each component of the media. Since media component concentrations are in units of g/L,
  #dividing g/L by g/mol produces units of mol/L. We then assume that 1 L = 1 kg of media. For yeast extract and tryptone, we use the value of 6 mmol/kg for their osmolality. 
  
  #First, in a for loop, we calculate the total_osmolality of all the media components combined, in units of mol/kg. 
  
  #Then, we take the desired final osmolality of the solution, as given in the design table, convert it to units of mol/kg, subtract from it the osmolality of the other remaining media 
  #components, to determine the amount of osmolality that will need to be provided by the added salt. Then, we multiply by the molar mass of the salt, and divide by its ionic strength,
  #to obtain the amount, in g/L, of added salt that we will need to achieve our desired osmotic pressure
  
  total_osmolality=0
  
  for (j in 1:length(osmolality_list)){
    if (names(osmolality_list)[j] %in% names(design_table)){
      
      total_osmolality = total_osmolality + as.numeric(as.character(design_table[[names(osmolality_list[j])]])) * osmolality_list[[j]]
      
    }
  }
  
  design_table[[salt_list$name]] = signif( (as.numeric(as.character(design_table$Osmolality))/1000 - total_osmolality) * salt_list$mm / salt_list$ionic_strength , 3)
  
  return(design_table)
}
