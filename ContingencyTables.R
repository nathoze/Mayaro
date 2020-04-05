ContingencyTables <- function(Inf.Prob  = Inf.Prob){
  
  RSN = read.csv('SeroNeutralisation.csv', sep=';')
  
  w= which(!is.na(RSN$SN.Chikungunya.conclusion))
  RSN = RSN[w, ]
  
  indices =  match(RSN$num_indiv,Inf.Prob$num_indiv)
  
  RSN$May.Model =Inf.Prob$mayinf[indices]
  RSN$Chik.Model =Inf.Prob$chikinf[indices]
  
  RSN$May.Lum = RSN$may_ratio>MayThreshold
  RSN$Chik.Lum = RSN$chik_ratio>ChikThreshold
  
  
  
  print('Contingency table Luminex and Seroneutralization, CHIKV:')
  t=(table(RSN$Chik.Lum,RSN$SN.Chikungunya.conclusion))
  print(t)
  specificity = t[1]/(t[1]+t[2])
  sensitivity = t[4]/(t[3]+t[4])
  print(sensitivity)
  print(specificity)
  
  print('Contingency table Model and Seroneutralization, CHIKV:')
  t=table(RSN$Chik.Model,RSN$SN.Chikungunya.conclusion)
  print(t)
  specificity = t[1]/(t[1]+t[2])
  sensitivity = t[4]/(t[3]+t[4])
  print(sensitivity)
  print(specificity)
  
  print('Contingency table Luminex and Seroneutralization, MAYV:')
  t=(table(RSN$May.Lum,RSN$SN.Mayaro.conclusion))
  print(t)
  specificity = t[1]/(t[1]+t[2])
  sensitivity = t[4]/(t[3]+t[4])
  print(sensitivity)
  print(specificity)
  
  
  print('Contingency table Model and Seroneutralization, MAYV:')
  t=table(RSN$May.Model,RSN$SN.Mayaro.conclusion)
  print(t)
  specificity = t[1]/(t[1]+t[2])
  sensitivity = t[4]/(t[3]+t[4])
  print(sensitivity)
  print(specificity)
} 