exp_table_generator<-function(ff_design_table=NULL, all_factors, num_centerpoints, num_blanks){
  #This function takes a fractional factorial design table, genereated by FrF2, and adds centerpoints and blank rows
  #The fractional factorial table argument is optional. If omitted, function returns an experimental design table with only centerpoints and blanks.
  
  #Create a table of centerpoints with num_centerpoints rows
  design_table<-data.frame(matrix(unlist(all_factors),nrow=1))%>%
    mutate(Category="Centerpoint")%>%
    mutate(Replicate=1)
  colnames(design_table)<-c(names(all_factors),"Category","Replicate")
  for (j in 1:(num_centerpoints-1)){
    design_table<-rbind(design_table,design_table[1,])
    design_table$Replicate[j+1]=j+1
  } 
  
  if (length(ff_design_table)>0){
    
    #Check that all of the elements in the factorial factor design table are also present in the all factors list
    if(!all(colnames(ff_design_table) %in% names(all_factors))){
      stop("All of the factors in the fractional factorial design table are not included in all_factors.")
    }
    
    #Add the Category factor to the ff_design_table, to allow for distinguishing between fractional factorial runs and centerpoint runs
    ff_design_table<-mutate(ff_design_table, Category="Exp")
    
    #Bind the fractional factorial design table to the centerpoints table, and back-fill the NAs in the ff table with values from the centerpoints table
    design_table<-suppressWarnings(na.locf( bind_rows(ff_design_table,design_table), fromLast=T)) 
  }
  
  #Add in the rows for blanks
  for (j in 1:(num_blanks)) design_table<-add_row(design_table, Category="Blank")
  
  return(design_table)
}
