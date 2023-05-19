###############
#Load Packages#
###############

packages = c('dplyr')
for (package in packages){
  if(!require(package, character.only = T)){
    install.packages(package)
    library(package, character.only = T)
  }
}


###########
#Load Data#
###########
IPDE = read.csv('../Data/PSU HiTOP IPDE.csv', header = T)
IPDE = apply(IPDE, 2, as.character) 

IPDE = as.data.frame(apply(IPDE, 2, as.numeric)) 
#Some columns were read as factors. 
#This coerces all of them to be numeric.You can ignore the warning message. 

str(IPDE)

############################
#IPDE Diagnosis Arrangement#
############################

#Function for threshold detection
#E.g., if [threshold] or more than [threshold] items equal to "2", 
#it is considered as passing the threshold (coded as 1, otherwise coded as 0)

diagnosis = function(data, threshold){
  cellEquals2 = ifelse(data == 2, 1, 0 )
  numbersOfSymptoms = rowSums(cellEquals2, na.rm = T)
  passThreshold = ifelse(numbersOfSymptoms >= threshold, 1, 0)
  return(passThreshold)
}

#Paranoid PD 
#Criterion A: 4 or more items equal to “2” (7 total): IPDE42, IPDE43, IPDE44, IPDE45, IPDE46, IPDE47, IPDE6
symptoms_paranoidPD = 
  IPDE[, c('IPDE42', 'IPDE43', 'IPDE44', 'IPDE45', 'IPDE46', 'IPDE47', 'IPDE64')]
paranoidPD = diagnosis(symptoms_paranoidPD, 4)

#Schizoid PD
#Criterion A: 4 or more item equal to “2” (7 total): IPDE23, IPDE24, IPDE25, IPDE50, IPDE51, IPDE61, IPDE96
symptoms_schizoidPD = 
  IPDE[ ,c('IPDE23', 'IPDE24', 'IPDE25', 'IPDE50', 'IPDE51', 'IPDE61', 'IPDE96')]
schizoidPD = diagnosis(symptoms_schizoidPD, 4)

#Schizotypal PD
#Criterion A: 5 or more facets equal to “2” (9 total): 
#IPDE23, IPDE26, IPDE48, IPDE66, IPDE67, IPDE93, IPDE95, IPDE97, IPDE98
symptoms_schizotypalPD = 
  IPDE[ ,c('IPDE23', 'IPDE26', 'IPDE48', 'IPDE66', 'IPDE67', 'IPDE93', 'IPDE95', 'IPDE97', 'IPDE98')]
schizotypalPD = diagnosis(symptoms_schizotypalPD, 5)

#Antisocial PD
#Criterion A: 3 or more facets equal to “2” (7 total): A6, IPDE69, IPDE71, IPDE72, IPDE73, IPDE74, IPDE75
symptoms_antisocialPD = 
  IPDE[ ,c('IPDE69', 'IPDE71', 'IPDE72', 'IPDE73', 'IPDE74', 'IPDE75')]

#If either IPDE05 or IPDE70 = 2, then Criterion A6 = 2
#If either IPDE05 or IPDE70 = 1 and niether of them = 2, then Criterion A6 = 1
#If both IPDE05 amd IPDE70 = 0, then Criterion A6 = 0
#In consistent with the logic, the ifelse function needs to go from 0 to 2. 
antpd_a6 = ifelse(IPDE$IPDE05 == 0 & IPDE$IPDE70 == 0, 0, NA)
antpd_a6 = ifelse(IPDE$IPDE05 == 1 | IPDE$IPDE70 == 1, 1, antpd_a6)
antpd_a6 = ifelse(IPDE$IPDE05 == 2 | IPDE$IPDE70 == 2, 2, antpd_a6)

symptoms_antisocialPD$a6 = antpd_a6
antisocialPD = diagnosis(symptoms_antisocialPD, 3)

#Criterion C: evidence of conduct disorder
#Conduct Disorder: 3 or more facets equal to “2” (15 total): IPDE78 to IPDE92
symptoms_cd = IPDE[,grep('IPDE78|IPDE79|IPDE8[0-9]|IPDE90|IPDE91|IPDE92', colnames(IPDE))]
cd = diagnosis(symptoms_cd, 3)
antisocialPD = ifelse(cd == 1, antisocialPD, 0) #if there is no evidence of CD, then recode antisocialPD to 0

#Borderline PD
#Criterion A: 5 or more facets equal to “2” (9 total)
#IPDE33, IPDE52, IPDE56, IPDE58, IPDE59,IPDE68, IPDE77
#C3(IPDE06*, IPDE07*, IPDE08*, IPDE31* IPDE65*), 
#C4(IPDE63*, IPDE73*, IPDE76*)

symptoms_borderlinePD = 
  IPDE[ ,c('IPDE33', 'IPDE52', 'IPDE56', 'IPDE58', 'IPDE59', 'IPDE68','IPDE77')]

#If 2 or more of IPDE06, IPDE07, IPDE08, IPDE31, and IPDE65 = 2, then Criterion 3 = 2
#If only 1 of IPDE06, IPDE07, IPDE08, IPDE31, and IPDE65 = 2, then Criterion 3 = 1
#If none of IPDE06, IPDE07, IPDE08, IPDE31, and IPDE65 = 2 and sum of items is equal to or great than 3, then Criterion 3 = 1

bpd_c3_symptoms = IPDE[, c('IPDE06', 'IPDE07', 'IPDE08', 'IPDE31', 'IPDE65')]
bpd_c3 = diagnosis(bpd_c3_symptoms, 2) 
ifelse(bpd_c3 == 1, 2, 0) 
# I did not differentiate those have 1 from 0, because 1 and 0 have no difference in making a diagnosis

#If IPDE76 = 2, then Criterion 4 = 2 -> This criterion goes first because the variable contains NA
bpd_c4 = ifelse(IPDE$IPDE76 == 2, 2, 0)
bpd_c4[is.na(bpd_c4)] = 0
#If both IPDE63 and IPDE73 = 2, then Criterion 4 = 2
bpd_c4 = ifelse(IPDE$IPDE63 == 2 & IPDE$IPDE73 == 2, 2, bpd_c4)
#If sum of IPDE63, IPDE73, and IPDE76 is equal to or greater than 5, then Criterion 4 = 2
bpd_c4 = ifelse(rowSums(IPDE[,c('IPDE63', 'IPDE73', 'IPDE76')], na.rm = T) >= 5, 2, bpd_c4)

symptoms_borderlinePD$c3 = bpd_c3
symptoms_borderlinePD$c4 = bpd_c4
borderlinePD = diagnosis(symptoms_borderlinePD, 5) 

#Histrionic PD
#Criterion A: 5 or more facets equal to “2” (8 total:
#IPDE12, IPDE18, IPDE19, IPDE32, IPDE49, IPDE57, IPDE62, IPDE94
symptoms_histrionicPD = 
  IPDE[ ,c('IPDE12', 'IPDE18', 'IPDE19', 'IPDE32', 'IPDE49', 'IPDE57', 'IPDE62', 'IPDE94')]
histrionicPD = diagnosis(symptoms_histrionicPD, 5)

#Narcissistic PD
#Criterion A: 5 or more facets equal to “2” (9 total)
#IPDE17, IPDE20, IDPE21, IPDE22, IDPE38, IPDE39, IPDE40, IPDE53, IPDE99
symptoms_narcissisticPD = 
  IPDE[ ,c('IPDE17', 'IPDE20', 'IPDE21', 'IPDE22', 'IPDE38', 'IPDE39', 'IPDE40', 'IPDE53', 'IPDE99')]
narcissisticPD = diagnosis(symptoms_narcissisticPD, 5)

#Avoidant PD
#Criterion A: 4 or more facets equal to “2” (7 total)
#IPDE04, IPDE13, IPDE27, IPDE28, IPDE29, IPDE30, IDPE60
symptoms_avoidantPD = 
  IPDE[ ,c('IPDE04', 'IPDE13', 'IPDE27', 'IPDE28', 'IPDE29', 'IPDE30', 'IPDE60')]
avoidantPD = diagnosis(symptoms_avoidantPD, 4)

#Dependent PD
#Criterion A: 5 or more facets equal to “2’” (8 total)
#IPDE09, IPDE10, IPDE11, IPDE34, IPDE35, IPDE41, IPDE54, IPDE55
symptoms_dependentPD = 
  IPDE[ ,c('IPDE09', 'IPDE10', 'IPDE11', 'IPDE34', 'IPDE35', 'IPDE41', 'IPDE54', 'IPDE55')]
dependentPD = diagnosis(symptoms_dependentPD, 5)

#Obsessive-Compulsive PD
#Criterion A: 4 or more facets equal to “2” (8 total)
#IPDE01, IPDE02, IDPE03, IDPE14, IPDE15, IDPE16, IDPE36, IDPE37
symptoms_obsessive_compulsivePD = 
  IPDE[ ,c('IPDE01', 'IPDE02', 'IPDE03', 'IPDE14', 'IPDE15', 'IPDE16', 'IPDE36', 'IPDE37')]
obsessive_compulsivePD = diagnosis(symptoms_obsessive_compulsivePD, 4)

IPDE_dx = 
  data.frame(
    ID = IPDE$ID,
    paranoidPD,
    schizoidPD,
    schizotypalPD,
    antisocialPD,
    borderlinePD,
    histrionicPD,
    narcissisticPD,
    avoidantPD,
    dependentPD,
    obsessive_compulsivePD
  )

write.csv(IPDE_dx, '../Data/IPDE_Dx.csv', row.names = F)

