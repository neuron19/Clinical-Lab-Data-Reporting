### Clinical Labs Data and Reporting Abnormal Values ###

library(tidyr)
library(ggplot2)

#1. Generate a Random Data Set for 1000 Patients with various Clinical Labs

  #1.1 Prepare Random 4-letter random ID for  1000 Patients 
  n = 1000 # No. of Patients
  PatientID = NULL
  for(i in 1:n){
        ID = paste(sample(LETTERS, 4, replace=TRUE),collapse="")
  PatientID = c(PatientID, ID)
  }

  #1.2 Assign, randomly, Sex for PatientID
  Sex  = sample(c("Male", "Female"), n, prob = c(5,3), replace=T)
  Labs = cbind(PatientID, Sex)

  #1.3 Lets generate age for Patients (between 18 to 75 years)
  Age  = sample(18:75, n, replace = T)
  Labs = cbind(Labs, Age)

  #1.4 Generate random Clinical Lab Values for various Clincal Labs(32 tests) based on reference ranges 
  Albumin       = round(runif(n, 3.5,5.1), 1)
  AlkaPhos      = sample(50:160, n, replace = T )
  BunCreatRatio = sample(5.5:24, n, replace = T)
  BUN           = sample(7.5:25.5, n, replace = T)
  Calcium       = round(runif(n, 8.3, 10.8), 1)
  Cholesterol   = sample(150:215, n, replace = T)
  Chloride      = sample(95:111, n, replace = T)
  Creatinine    = round(runif(n, 0.6, 1.4),2)
  Glucose       = sample(60:145, n, replace = T)
  Iron          = sample(50:180, n, replace = T)
  Potassium     = round(runif(n, 3.4, 5.5), 1)
  Sodium        = sample(136:147, n, replace = T)
  Phosphorus    = round(runif(n, 2.2, 5), 1)
  AST           = sample(7:50, n, replace = T)
  ALT           = sample(30:60, n, replace = T)
  Bilirubin     = round(runif(n, 0.2, 1.5), 1)
  Protein       = round(runif(n, 5.4, 8.0), 1)
  Triglycerides = sample(135:155, n, replace = T)
  UricAcid      = round(runif(n, 3, 7.2), 1)
  WBC   = round(runif(n, 3.5, 12), 1)
  RBC   = round(runif(n, 3.5, 5.7), 1)
  HGB   = round(runif(n, 11, 16.5), 1)
  HCT   = round(runif(n, 33, 47.5), 1)
  MCV   = sample(77:102, n, replace = T)
  RDW   = round(runif(n, 11.5, 15.5), 1)
  Platelets  = sample(130:500, n, replace = T)
  PMN        = round(runif(n, 1.4, 8), 1)
  LYMPHS     = round(runif(n, 0.9, 5), 1)
  MONOS      = round(runif(n, 0.3, 1.1), 1)
  EOS        = round(runif(n, 0, 0.6), 1)
  BASOS      = round(runif(n, 0, 0.3), 1)
  PHUrine    = round(runif(n, 4.9, 8.5), 1)
 
  Labs = as.data.frame(cbind(Labs, Albumin, AlkaPhos, BunCreatRatio,
                                   BUN, Calcium, Cholesterol, Chloride, Creatinine,
                                   Glucose, Iron, Potassium, Sodium, Phosphorus, 
                                   AST, ALT, Bilirubin, Protein, Triglycerides,
                                   UricAcid, WBC, RBC, HGB, HCT, MCV, RDW, Platelets,
                                   PMN, LYMPHS, MONOS, EOS, BASOS, PHUrine), stringsAsFactors=F)

  #1.5 Converting data format  from  Wide to Long 
  Labs        = gather(Labs, Test, Result, -PatientID, -Sex, -Age)
  Labs$Result = as.numeric(Labs$Result)
  Labs$Age    = as.numeric(Labs$Age)

#2. Automatically flag -Out of Range(OoR)- Clinical Labs and save in a separate data frame
  Labs.subset = function(x){Labs[Labs$Test == x,]}

  i.labs = Labs.subset("Albumin") 
  OoR    = i.labs[ifelse(i.labs$Age < 60, (i.labs$Result < 3.5 | i.labs$Result > 5.0), 
                                          (i.labs$Result < 3.4 | i.labs$Result > 4.8)),]
  
  i.labs    = Labs.subset("AlkaPhos") 
  i.labs.M  = i.labs[i.labs$Sex == "Male",]
  i.labs.F  = i.labs[i.labs$Sex == "Female",]
  OoR       = rbind(OoR, i.labs.M[ifelse(i.labs.M$Age >= 20, (i.labs.M$Result >= 150), (i.labs.M$Result >= 750)),],
                         i.labs.F[ifelse(i.labs.F$Age >= 20, (i.labs.F$Result >= 150), (i.labs.F$Result >= 500)),])

  i.labs = Labs.subset("BunCreatRatio") 
  OoR    = rbind(OoR, i.labs[i.labs$Result < 6.0 | i.labs$Result > 22.0,])

  i.labs = Labs.subset("BUN")
  OoR    = rbind(OoR, i.labs[i.labs$Result < 8.0 | i.labs$Result > 25.0,])

  i.labs = Labs.subset("Calcium")
  OoR    = rbind(OoR, i.labs[i.labs$Result < 8.5 | i.labs$Result > 10.4,])

  i.labs = Labs.subset("Cholesterol")
  OoR    = rbind(OoR, i.labs[i.labs$Result >= 200.0,])

  i.labs = Labs.subset("Chloride")
  OoR    = rbind(OoR, i.labs[i.labs$Result < 96.0 | i.labs$Result > 111.0,])
 
  i.labs = Labs.subset("Creatinine")
  OoR    = rbind(OoR,i.labs[ifelse(i.labs$Sex == "Male", (i.labs$Result < 0.62 | i.labs$Result > 1.27), 
                                                         (i.labs$Result < 0.49 | i.labs$Result > 1.10)),])
   
  i.labs = Labs.subset("Glucose")
  OoR    = rbind(OoR, i.labs[i.labs$Result < 65.0 | i.labs$Result > 139.0,])

  i.labs = Labs.subset("Iron")
  OoR    = rbind(OoR,i.labs[ifelse(i.labs$Sex == "Male", (i.labs$Result < 55.0 | i.labs$Result > 175.0), 
                                                         (i.labs$Result < 45.0 | i.labs$Result > 170.0)),])
  i.labs = Labs.subset("Potassium")
  OoR    = rbind(OoR, i.labs[i.labs$Result < 3.5 | i.labs$Result > 5.1,])

  i.labs = Labs.subset("Sodium")
  OoR    = rbind(OoR, i.labs[i.labs$Result < 136.0 | i.labs$Result > 145.0,])

  i.labs = Labs.subset("Phosphorus") 
  OoR    = rbind(OoR, i.labs[ifelse(i.labs$Age < 60, (i.labs$Result < 2.4 | i.labs$Result > 4.7), 
                                                     (i.labs$Result < 2.3 | i.labs$Result > 4.0)),])

  i.labs = Labs.subset("AST")
  OoR    = rbind(OoR, i.labs[i.labs$Result < 8.0 | i.labs$Result > 45.0,])

  i.labs = Labs.subset("ALT")
  OoR    = rbind(OoR, i.labs[i.labs$Result >= 55.0,])

  i.labs = Labs.subset("Bilirubin")
  OoR    = rbind(OoR, i.labs[i.labs$Result < 0.3 | i.labs$Result > 1.3,])

  i.labs = Labs.subset("Protein") 
  OoR    = rbind(OoR, i.labs[ifelse(i.labs$Age < 60, (i.labs$Result < 6.0 | i.labs$Result > 7.9), 
                                                     (i.labs$Result < 5.6 | i.labs$Result > 7.6)),])

  i.labs = Labs.subset("Triglycerides")
  OoR    = rbind(OoR, i.labs[i.labs$Result >= 150.0,])

  i.labs    = Labs.subset("UricAcid") 
  i.labs.M  = i.labs[i.labs$Sex == "Male",]
  i.labs.F  = i.labs[i.labs$Sex == "Female",]
  OoR       = rbind(OoR, i.labs.M[ifelse(i.labs.M$Age >= 20, (i.labs.M$Result < 3.5 | i.labs.M$Result > 7.0), 
                                                             (i.labs.M$Result < 4.0 | i.labs.M$Result > 8.7)),],
                         i.labs.F[ifelse(i.labs.F$Age >= 20, (i.labs.M$Result < 3.2 | i.labs.M$Result > 6.0), 
                                                             (i.labs.M$Result < 3.0 | i.labs.M$Result > 5.8)),])

  i.labs = Labs.subset("WBC")
  OoR    = rbind(OoR, i.labs[i.labs$Result < 3.5 | i.labs$Result > 11.0,])

  i.labs = Labs.subset("RBC")
  OoR    = rbind(OoR,i.labs[ifelse(i.labs$Sex == "Male", (i.labs$Result < 4.06 | i.labs$Result > 5.63), 
                                                         (i.labs$Result < 3.63 | i.labs$Result > 4.92)),])

  i.labs = Labs.subset("HGB")
  OoR    = rbind(OoR,i.labs[ifelse(i.labs$Sex == "Male", (i.labs$Result < 12.5 | i.labs$Result > 16.3), 
                                                         (i.labs$Result < 11.2 | i.labs$Result > 15.2)),])

  i.labs = Labs.subset("HCT")
  OoR    = rbind(OoR,i.labs[ifelse(i.labs$Sex == "Male", (i.labs$Result < 36.7 | i.labs$Result > 47.0), 
                                                         (i.labs$Result < 33.5 | i.labs$Result > 45.2)),])

  i.labs = Labs.subset("MCV")
  OoR    = rbind(OoR, i.labs[i.labs$Result < 78.0 | i.labs$Result > 100.0,])

  i.labs = Labs.subset("RDW")
  OoR    = rbind(OoR, i.labs[i.labs$Result < 12.0 | i.labs$Result > 15.0,])

  i.labs = Labs.subset("Platelets")
  OoR    = rbind(OoR, i.labs[i.labs$Result < 140.0 | i.labs$Result > 450.0,])

  i.labs = Labs.subset("LYMPHS")
  OoR    = rbind(OoR, i.labs[i.labs$Result < 1.0 | i.labs$Result > 4.8,])

  i.labs = Labs.subset("MONOS")
  OoR    = rbind(OoR, i.labs[i.labs$Result < 0.3 | i.labs$Result > 1.0,])

  i.labs = Labs.subset("PMN")
  OoR    = rbind(OoR, i.labs[i.labs$Result < 1.5 | i.labs$Result > 7.7,])

  i.labs = Labs.subset("EOS")
  OoR    = rbind(OoR, i.labs[i.labs$Result > 0.5,])

  i.labs = Labs.subset("BASOS")
  OoR    = rbind(OoR, i.labs[i.labs$Result > 0.2,])

  i.labs = Labs.subset("PHUrine")
  OoR    = rbind(OoR, i.labs[i.labs$Result < 5.0 | i.labs$Result > 8.0,])

  OoR = OoR[order(OoR$PatientID),]
  rownames(OoR) = c(1:length(OoR$PatientID))

#3. Plots - Check to see the clinical lab with most abnormal values   
  t.test = data.frame(table(OoR$Test))

  png("plot1.png", width=900, height=1200, units="px")

  plot = ggplot(t.test, aes(factor(Var1), Freq))+geom_bar(stat="identity", fill = "orangered3")+coord_flip()+
       labs(x = "Clinical Lab", y = "Frequency")+
       labs(title = "Out of Range(OoR) Clinical Labs for Patients")   

  print(plot)
  dev.off()










