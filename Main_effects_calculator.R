Main_effects_calculator<-function(exp_table, exp_factors_list, metrics_list){
  
  #The purpose of this function is to calculate the main effects and interaction of every factor in the experimental factor list
  #This function calculates both the main effect and the relative effect (which is the main effect divided by the response mean)
  
  #Calculate the mean response of each metric
  metric_mean<-exp_table%>%
    filter(Exp_number %in% names(exp_factors_list))%>%
    filter(Category=="Exp")%>%
    gather(unlist(metrics_list),key="metric",value="response")%>%
    select(metric,response)%>%
    group_by(metric)%>%
    summarize(mean_metric=mean(response))
  
  
  #Initialize the effects table data frame
  effects_table<-data.frame() 
  
  
  for (exp in 1:length(exp_factors_list)){
    for (met in 1:length(metrics_list)){
      
      #Create a tibble for each experiment and for each metric
      tib_sub<-exp_table%>%
        filter(Exp_number==names(exp_factors_list[exp]))%>%
        filter(Category=="Exp")%>%
        select(metrics_list[[met]], exp_factors_list[[exp]])
      
      #Generate a linear model with all 2-factor interactions
      colnames(tib_sub)[1]<-"y"
      model<-lm(y~(.)^2, data=tib_sub)
      
      #Create a DanielPlot, from the FrF2 package, which calculates the effects of each factor and 2-factor interaction
      exp_effects<-DanielPlot(model,half=F)%>%
        select(effect,x,y)%>%
        rename("factor_raw"=effect, "effect"=x, "norm_prob"=y)%>%
        mutate(Exp_number = names(exp_factors_list[exp]),
               metric = metrics_list[[met]],
               factor_one = lapply(strsplit(as.character(factor_raw),":"), "[[", 1))
      
      effects_table<-rbind(effects_table,exp_effects)
    }
  }
  
  
  #Merge in the mean response for each metric
  effects_table<-full_join(effects_table, metric_mean, by='metric')
  
  #Use the mean response for each metric to calculate the relative effect of ecah metric
  effects_table<-mutate(effects_table, relative_effect = effect / mean_metric)
  
  
  
  #For clean-up purposes, split the interaction factor into its two components
  for (j in 1:dim(effects_table)[1]){
    effects_table$factor_two[j]<-strsplit(as.character(effects_table$factor_raw[j]),":")[[1]][2]
  }
  
  #Rename each factor level with a cleaned up label for the purpose of plotting
  effects_table<-effects_table%>%
    mutate(factor_one = fct_recode(as.character(factor_one),
                                   "Well volume"="Well_volume",
                                   "Flask baffles"="Flask_baffles",
                                   "Flask cover"="Flask_cover",
                                   "Shake speed"="Shake_speed",
                                   "Yeast extract source"="Yeast_extract_source",
                                   "Well fill volume"="Well_fill_volume",
                                   "Magnesium sulfate"="Magnesium_sulfate",
                                   "Flask volume"="Flask_volume",
                                   "Yeast extract"="Yeast_extract",
                                   "Well bottom"="Well_bottom",
                                   "Well cover"="Well_cover",
                                   "Flask fill volume"="Flask_fill_volume",
                                   "Buffer capacity"="Buffer_capacity",
                                   "Antibiotic concentration"="Antibiotic_conc",
                                   "Inoculum age"="Inoculum_age",
                                   "Inoculum amount"="Inoculum_amount"))%>%
    mutate(factor_two = fct_recode(as.character(factor_two),
                                   "Well volume"="Well_volume",
                                   "Flask baffles"="Flask_baffles",
                                   "Flask cover"="Flask_cover",
                                   "Shake speed"="Shake_speed",
                                   "Yeast extract source"="Yeast_extract_source",
                                   "Well fill volume"="Well_fill_volume",
                                   "Magnesium sulfate"="Magnesium_sulfate",
                                   "Flask volume"="Flask_volume",
                                   "Yeast extract"="Yeast_extract",
                                   "Well bottom"="Well_bottom",
                                   "Well cover"="Well_cover",
                                   "Flask fill volume"="Flask_fill_volume",
                                   "Buffer capacity"="Buffer_capacity",
                                   "Antibiotic concentration"="Antibiotic_conc",
                                   "Inoculum age"="Inoculum_age",
                                   "Inoculum amount"="Inoculum_amount"))%>%
    mutate(factor_raw = fct_recode(as.character(factor_raw),
                                   "Well volume"="Well_volume",
                                   "Flask baffles"="Flask_baffles",
                                   "Flask cover"="Flask_cover",
                                   "Shake speed"="Shake_speed",
                                   "Yeast extract source"="Yeast_extract_source",
                                   "Well fill volume"="Well_fill_volume",
                                   "Magnesium sulfate"="Magnesium_sulfate",
                                   "Flask volume"="Flask_volume",
                                   "Yeast extract"="Yeast_extract",
                                   "Well bottom"="Well_bottom",
                                   "Well cover"="Well_cover",
                                   "Flask fill volume"="Flask_fill_volume",
                                   "Buffer capacity"="Buffer_capacity",
                                   "Antibiotic concentration"="Antibiotic_conc",
                                   "Inoculum age"="Inoculum_age",
                                   "Inoculum amount"="Inoculum_amount"))%>%
    mutate(metric = fct_recode(metric,
                               "DCM"="dcm",
                               "Titer"="titer",
                               "Yield"="yield"))%>%
    mutate(main_effect=F)
  
  
  #Paste together the two interacting factors to create a nice label for plotting purposes
  for (j in 1:dim(effects_table)[1]){
    if (is.na(effects_table$factor_two[j])){
      effects_table$factor[j]<-as.character(effects_table$factor_raw[j])
      effects_table$main_effect[j]<-T
    } else {
      effects_table$factor[j]<-paste(effects_table$factor_one[j], effects_table$factor_two[j], sep=" : ")
    }
  }
  
  return(effects_table)
  
}
