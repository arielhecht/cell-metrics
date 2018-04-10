pH_calculator<-function(design_table, pH_list){
  
  #This function calculates the concentration of phosphate salts needed to achieve the desired pH
  #Relies on knowing the desired pH and Buffer capacity (supplied via the design_table), 
  #the identity and molar mass of the conjugate base and acids, and the pKa of the buffer.
  
  conj_base = pH_list$conj_base
  conj_acid = pH_list$conj_acid
  cb_mm = pH_list$cb_mm
  ca_mm = pH_list$ca_mm
  pKa = pH_list$pKa
  
  design_table[[conj_base]]<-NA
  design_table[[conj_acid]]<-NA
  
  #Henderson-Hasselbach equation:
  #pH = pKa + log([A-]/[HA])
  #raise both sides to the power of 10, and rearrange, and we end up with a linear equation for [A-] and   [HA]:
  # 0 = [A-]*10^pKA - [HA]*10^pH
  
  #Equation for buffer capacity:
  #Buffer.cap = [A-] + [HA]
  
  #Combining the two, we can create two matricies, a and b:
  # a =[[  1       1  ]
  #     [10^pka -10^pH]]
  #
  #b = [[Buffer.cap]
  #     [     0    ]]
  #
  #c =[[A-]
  #    [HA]]
  #
  # a * c = b
  #Solve for a and b:
  #c[1] is the concentration of A-, in mM. c[2] is the concentration of HA, in mM.
  
  for (j in 1:dim(design_table)[1]){
    a<-matrix(c(1,10^pKa,1,-10^as.numeric(as.character(design_table$pH[j]))),nrow=2,ncol=2)
    b<-matrix(c(as.numeric(as.character(design_table$Buffer_capacity[j])),0),ncol=1)
    c<-solve(a,b)
    #Convert molarity of phosphate salts into concentration in g/L
    design_table[[conj_base]][j]<-signif(c[1]/1000*cb_mm,3)
    design_table[[conj_acid]][j]<-signif(c[2]/1000*ca_mm,3)
  }
  
  return(design_table)
  
}
