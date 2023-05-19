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


##########################
#IPDE Symptom Arrangement#
##########################

#Paranoid PD 
#Criterion A: 4 or more items equal to “2” (7 total): IPDE42, IPDE43, IPDE44, IPDE45, IPDE46, IPDE47, IPDE6
symptoms_paranoidPD = 
  IPDE[, c('IPDE42', 'IPDE43', 'IPDE44', 'IPDE45', 'IPDE46', 'IPDE47', 'IPDE64')]
colnames(symptoms_paranoidPD) = paste0('paranoid', seq(1:7))

#Schizoid PD
#Criterion A: 4 or more item equal to “2” (7 total): IPDE23, IPDE24, IPDE25, IPDE50, IPDE51, IPDE61, IPDE96
symptoms_schizoidPD = 
  IPDE[ ,c('IPDE23', 'IPDE24', 'IPDE25', 'IPDE50', 'IPDE51', 'IPDE61', 'IPDE96')]
colnames(symptoms_schizoidPD) = paste0('schizoid', seq(1:7))

#Schizotypal PD
#Criterion A: 5 or more facets equal to “2” (9 total): 
#IPDE23, IPDE26, IPDE48, IPDE66, IPDE67, IPDE93, IPDE95, IPDE97, IPDE98
symptoms_schizotypalPD = 
  IPDE[ ,c('IPDE26', 'IPDE48', 'IPDE66', 'IPDE67', 'IPDE93', 'IPDE95', 'IPDE97', 'IPDE98')]
colnames(symptoms_schizotypalPD) = paste0('schizotypal', seq(1:8))

#Antisocial PD
#Criterion A: 3 or more facets equal to “2” (7 total): A6, IPDE69, IPDE71, IPDE72, IPDE73, IPDE74, IPDE75
symptoms_antisocialPD = 
  IPDE[ ,c('IPDE05','IPDE69', 'IPDE70','IPDE71', 'IPDE72', 'IPDE73', 'IPDE74', 'IPDE75')]
colnames(symptoms_antisocialPD) = paste0('antisocial', seq(1:8))


#Criterion C: evidence of conduct disorder
#Conduct Disorder: 3 or more facets equal to “2” (15 total): IPDE78 to IPDE92
symptoms_cd = IPDE[,grep('IPDE78|IPDE79|IPDE8[0-9]|IPDE90|IPDE91|IPDE92', colnames(IPDE))]
colnames(symptoms_cd) = paste0('cd', seq(1:15))

#Borderline PD
symptoms_borderlinePD = 
  IPDE[ ,c('IPDE06', 'IPDE07', 'IPDE08', 'IPDE31','IPDE33', 
           'IPDE52', 'IPDE56', 'IPDE58', 'IPDE59', 'IPDE63',
           'IPDE65', 'IPDE68', 'IPDE76', 'IPDE77')]
colnames(symptoms_borderlinePD) = paste0('borderline', seq(1:14))

#Histrionic PD
#Criterion A: 5 or more facets equal to “2” (8 total:
#IPDE12, IPDE18, IPDE19, IPDE32, IPDE49, IPDE57, IPDE62, IPDE94
symptoms_histrionicPD = 
  IPDE[ ,c('IPDE12', 'IPDE18', 'IPDE19', 'IPDE32', 'IPDE49', 'IPDE57', 'IPDE62', 'IPDE94')]
colnames(symptoms_histrionicPD) = paste0('histrionic', seq(1:8))

#Narcissistic PD
#Criterion A: 5 or more facets equal to “2” (9 total)
#IPDE17, IPDE20, IDPE21, IPDE22, IDPE38, IPDE39, IPDE40, IPDE53, IPDE99
symptoms_narcissisticPD = 
  IPDE[ ,c('IPDE17', 'IPDE20', 'IPDE21', 'IPDE22', 'IPDE38', 'IPDE39', 'IPDE40', 'IPDE53', 'IPDE99')]
colnames(symptoms_narcissisticPD ) = paste0('narcissistic', seq(1:9))

#Avoidant PD
#Criterion A: 4 or more facets equal to “2” (7 total)
#IPDE04, IPDE13, IPDE27, IPDE28, IPDE29, IPDE30, IDPE60
symptoms_avoidantPD = 
  IPDE[ ,c('IPDE04', 'IPDE13', 'IPDE27', 'IPDE28', 'IPDE29', 'IPDE30', 'IPDE60')]
colnames(symptoms_avoidantPD) = paste0('avoidant', seq(1:7))

#Dependent PD
symptoms_dependentPD = 
  IPDE[ ,c('IPDE09', 'IPDE10', 'IPDE11', 'IPDE34', 'IPDE35', 'IPDE41', 'IPDE54', 'IPDE55')]
colnames(symptoms_dependentPD) = paste0('dependent', seq(1:8))

#Obsessive-Compulsive PD
symptoms_obsessive_compulsivePD = 
  IPDE[ ,c('IPDE01', 'IPDE02', 'IPDE03', 'IPDE14', 'IPDE15', 'IPDE16', 'IPDE36', 'IPDE37')]
colnames(symptoms_obsessive_compulsivePD) = paste0('oc', seq(1:8))


IPDE_Sx = 
  data.frame(
    ID = IPDE$ID,
    symptoms_paranoidPD,
    symptoms_schizoidPD,
    symptoms_schizotypalPD,
    symptoms_antisocialPD,
    symptoms_cd, 
    symptoms_borderlinePD,
    symptoms_histrionicPD,
    symptoms_narcissisticPD,
    symptoms_avoidantPD,
    symptoms_dependentPD,
    symptoms_obsessive_compulsivePD
  )

str(IPDE_Sx)
write.csv(IPDE_Sx, '../Data/IPDE_Sx.csv', row.names = F)

