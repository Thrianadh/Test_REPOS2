install.packages("insuranceData",dependencies = TRUE)
library(insuranceData)
data("AutoBi")
AutoBi
View(AutoBi)
  
claimant=AutoBi
claimant$CASENUM=NULL

#data Recoding
claimant$ATTORNEY[claimant$ATTORNEY==2]==0
claimant$CLMSEX[claimant$CLMSEX==2]==0
claimant$CLMINSUR[claimant$CLMINSUR==2]=0
claimant$CLMINSUR[claimant$CLMINSUR==3]=NA
claimant$SEATBELT[claimant$SEATBELT==2]=0
claimant$SEATBELT[claimant$SEATBELT==3]=NA

