###############
#Load Packages#
###############

packages = c('psych', 'dplyr', 'DiagrammeR', 'lavaan', 
             'DiagrammeR', 'DiagrammeRsvg', 'magrittr', 'rsvg', 'GPArotation')
for (package in packages){
  if(!require(package, character.only = T)){
    install.packages(package)
    library(package, character.only = T)
  }
}


###########
#Load Data#
###########
ipdeSx = read.csv('../Data/IPDE_Sx.csv', header = T)
ipdeSx = ipdeSx[!duplicated(ipdeSx$ID),]


############################################
#Dichotimized IPDE Symptom Data Preparation#
############################################

#Dichotomies function 
dichotomoize = function(x, cutoff){
  return(ifelse(x> cutoff, 1, 0))
}

ipdeSxD = as.data.frame(lapply(ipdeSx, dichotomoize, 0)) #cutoff = 0
ipdeSxD[is.na(ipdeSxD)] = 0
cd = ifelse(rowSums(ipdeSxD[grep('cd', colnames(ipdeSxD))]) >= 3, 1, 0)
ipdeSxD[,grep('cd', colnames(ipdeSxD))] = NULL
ipdeSxD$antisocial_cd = cd
str(ipdeSxD)
ipdeSxD$ID = ipdeSx$ID


##############################################
#Examine the Goodness of Fit of PD Categories#
##############################################

#Goodness of fit
sx = colnames(ipdeSxD)
efaByPD = data.frame(Sx = NA, Chi2 = NA, df = NA, pVal = NA, RMSEA = NA, TLI = NA)
cfaCByPD = data.frame(Sx = NA, Chi2 = NA, df = NA, pVal = NA, RMSEA = NA, TLI = NA, SRMR = NA)

for(x in c('paranoid', 'schizoid', 'schizotypal', 'antisocial', 'borderline', 'histrionic',
           'narcissistic', 'avoidant', 'dependent', 'oc')){
  assign(paste0('efa_',x), 
         fa(ipdeSxD[,grep(paste0('^',x), sx)], fa = 'RMSEA'))
  efa = get(paste0('efa_',x))
  if (is.null(efa$RMSEA)){efa$RMSEA = NA}
  
  efaByPD =  rbind(efaByPD, 
                     c(x, efa$dof, efa$chi, efa$PVAL, efa$RMSEA[1], efa$TLI))
  
  model = paste0('x =~ ', paste(sx[grep(paste0('^',x), sx)], collapse = ' + '))
  assign(paste0('cfa_',x), tryCatch(cfa(model, ipdeSxD,  std.lv = T, 
                                        ordered = sx, mimic = 'Mplus'),
                                    error = function(c) NULL))

  cfa = get(paste0('cfa_',x))
  if(is.null(cfa)){
    cfaCByPD = rbind(cfaCByPD,  c(x, NA, NA, NA, NA, NA, NA))
    }
  else{
    cfaCByPD = rbind(cfaCByPD,  c(x, fitmeasures(cfa, c('chisq.scaled','df.scaled','pvalue.scaled', 
                                                        'rmsea.scaled' ,'cfi', 'srmr'))))}
 
  
}

write.csv(efaByPD[-1,], '../Results/efaByPD.csv', row.names = F)
write.csv(cfaCByPD[-1,], '../Results/cfaByPD.csv', row.names = F)


###################################
#EFA on Dichotimized IPDE Symptoms#
###################################

fa(ipdeSxD[,-1], nfactors = 5, rotate = 'oblimin')


ipdeSxD4FA  = ipdeSxD[,-1]
ipdeSxD4FA = ipdeSxD4FA[,-which(KMO(cor(ipdeSxD4FA))$MSAi < 0.6)]
desc = read.csv('../Data/IPDE_Sx_Description.csv')

fa.parallel(ipdeSxD4FA, fm = 'minres') #2-9
ipdeVss = vss(ipdeSxD4FA, n = 10, rotation = 'oblimin')

efaFN = data.frame(num = NA, df = NA, cih2 = NA, pVal = NA, RMSEA = NA, TLI = NA, SRMR = NA, BIC = NA)

for(i in 1:9){
  efa =  fa(ipdeSxD4FA, nfactors = i, rotate = 'oblimin')
  assign(paste0('efa',i), efa)
  efaFN = rbind(efaFN, c(i, efa$dof, efa$chi, efa$PVAL, efa$RMSEA[1], efa$TLI, efa$rms, efa$BIC))
  loadings = as.data.frame(efa$loadings[,])
  loadings$Sx_Code = rownames(loadings)
  res = merge(loadings, desc, by = 'Sx_Code', x.all = T)
  res = cbind(res[,c('Sx_Code','Sx_Description')], res[,2:(i+1)])
  cor = efa$Phi
  
  write.csv(res, paste0('../Results/ipdeEfa',i,'.csv'), row.names = F)
  write.csv(cor, paste0('../Results/ipdeEfa',i,'_factorCor.csv'), row.names = F)
}


efaFN
write.csv(efaFN[-1,], '../Results/ipdeEfaRes1_9.csv', row.names = F)


anova(efa1, efa2, efa3, efa4, efa5, efa6, efa7, efa8, efa9)


########################################
#Bass-Ackward Approach on IPDE Symptoms#
########################################

set.seed(123)
Sx_Code = data.frame(Sx_Code = colnames(ipdeSxD4FA))
Sx_Desc = read.csv('../Data/IPDE_Sx_Description.csv')
ipdeSxDesc4FA = merge(Sx_Code, Sx_Desc, by ='Sx_Code')
str(ipdeSxDesc4FA )

ba2 = bassAckward(ipdeSxD4FA, nfactors = c(1:2), sort = T)
ba3 = bassAckward(ipdeSxD4FA, nfactors = c(2:3), sort = T)
ba4 = bassAckward(ipdeSxD4FA, nfactors = c(3:4), sort = T)
ba5 = bassAckward(ipdeSxD4FA, nfactors = c(1:5), sort = T)
ba5$bass.ack

ba5_plot = 
grViz("digraph {
  graph [layout = dot, rankdir = TB]
  
  node [shape = rectangle]        
  F1_1 [label = 'General\nFactor']
  F1_2 [label = 'Internalizing/\nExternalizing/\nThought']
  F2_2 [label = 'Paranoid\nAttachment']
  F1_3 [label = 'Paranoid\nAttachment']
  F2_3 [label = 'Emotion-related\nImpulsivity']
  F3_3 [label = 'Egocentrism/Oddity']
  F1_4 [label = 'Negative\nAffectivity']
  F2_4 [label = 'Paranoid\nAttachment']
  F3_4 [label = 'Egocentrism/Oddity']
  F4_4 [label = 'Disinhibition']
  F1_5 [label = 'Negative\nAffectivity']
  F2_5 [label = 'Detachment']
  F3_5 [label = 'Disinhibition']
  F4_5 [label = 'Antagonism']
  F5_5 [label = 'Psychoticism']
  
  # edge definitions with the node IDs
  F1_1 -> F1_2 [label= '0.90'] 
  F1_1 -> F2_2 [label= '0.51'] 
  
  F1_2 -> F1_3 [label='0.31']
  F1_2 -> F2_3 [label= '0.86'] 
  F1_2 -> F3_3 [label= '0.80'] 
  F2_2 -> F1_3 [label= '0.99']
  
  F1_3 -> F1_4 [label= '0.56']
  F1_3 -> F2_4 [label= '0.93']
  F2_3 -> F1_4 [label= '0.66']
  F2_3 -> F4_4 [label= '0.91']
  F3_3 -> F1_4 [label= '0.52']
  F3_3 -> F3_4 [label= '0.96']
  
  F1_4 -> F1_5 [label= '1.00']
  F2_4 -> F2_5 [label= '0.97']
  F2_4 -> F5_5 [label= '0.42']
  F3_4 -> F4_5 [label= '0.96']
  F3_4 -> F5_5 [label= '0.45']
  F4_4 -> F3_5 [label= '0.98']
  F4_4 -> F5_5 [label= '0.36']
  
  }")%>%
  export_svg %>% 
  charToRaw %>% 
  rsvg_pdf("../Results/BassAck_IPDE5.pdf") 


efa5Score = factor.scores(ipdeSxD4FA, f = efa5, method = 'tenBerge')
pid5FactorScore = data.frame(ID = c(1:nrow(ipdeSx)), 
                           efa5Score$scores)
colnames(pid5FactorScore) = c('ID', 'NEG', 'DET', 'DIS', 'ANT', 'PSY')
write.csv(pid5FactorScore, '../Data/pid5FactorScore.csv', row.names = F)
