exp_table_metadata<-function(design_table, exp_number=NULL, starter_culture=NULL, exp_date=NULL, plate_streak_date=NULL, well_id=NULL){
  #Add optional metadata columns to the end of the design_table
  
  if (!is.null(exp_number)) design_table$Exp_number = exp_number
  if (!is.null(starter_culture)) design_table$Starter_culture = as.numeric(starter_culture)
  if (!is.null(exp_date)) design_table$Exp_date = as.Date(exp_date,format='%m/%d/%Y')
  if (!is.null(plate_streak_date)) design_table$Plate_streak_date = as.Date(plate_streak_date,format='%m/%d/%Y')
  if (isTRUE(well_id)) design_table$Well_id=paste(exp_number, design_table$Plate, design_table$Well, sep="-")
  
  return(design_table)
  
}