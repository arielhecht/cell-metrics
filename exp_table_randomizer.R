exp_table_randomizer<-function(design_table, top_left, bottom_right){
  
  #This function randomizes the run order of a design table, and assigns wells (based on a 96-well format) for running and analyzing the experiment.
  #The assignment of wells to the 96 well plate is based on the rectangular area defined by the top_left and bottom_right well values. 
  #The 96-well plate area, defined as the number of wells included in the area, must be equal to the number of rows in the design table.
  #The parameters exp_number and exp_date allow the user to assign an experiment id number and an experiment date to the design table
  
  tl_row<-as.numeric(match(substr(top_left,1,1),  LETTERS[1:26]))
  tl_col<-as.numeric(substr(top_left,2,3))
  br_row<-as.numeric(match(substr(bottom_right,1,1), LETTERS[1:26]))
  br_col<-as.numeric(substr(bottom_right,2,3))
  
  plate_area_dim<-(br_row - tl_row+1) * (br_col-tl_col+1) 
  
  if (plate_area_dim != dim(design_table)[1]){
    stop(paste("Number of wells in plate area, ", plate_area_dim, " is not equal to the number of rows in design table, ", dim(design_table)[1], "." , sep=""))
  }
  
  design_table<-design_table%>%
    mutate(Run_number=1:n())
  
  #Assign each row a random number
  design_table<-design_table[sample(nrow(design_table)),]%>%
    mutate(Random=1:n())%>%
    mutate(Row=rep(LETTERS[seq(from=tl_row,to=br_row)],each=(br_col-tl_col+1)))%>%
    mutate(Column=rep(tl_col:br_col,times=(br_row-tl_row+1)))%>%
    mutate(Well=paste(Row,Column,sep=""))%>%
    mutate(Plate=1)%>%
    mutate(Exp_plate=1)
  
  return(design_table)
}
