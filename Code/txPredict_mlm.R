###############
#Load Packages#
###############

packages = c('dplyr', 'ggplot2', 'DescTools', 'lmvar', 'DescTools', 'lme4')
for (package in packages){
  if(!require(package, character.only = T)){
    install.packages(package)
    library(package, character.only = T)
  }
}


###########
#Load Data#
###########
df = read.csv('../Data/pid5PDDemoTop_valid.csv')
selectModelBIC = read.csv('../Results/selectModelBIC.csv')

colnames(df)[grep('PD', colnames(df))] = 
  StrCap(substring(colnames(df)[grep('PD', colnames(df))], 1, 
                   nchar(colnames(df)[grep('PD', colnames(df))])-2))

write.csv(df[,c('ID', 'NEG', 'DET', 'ANT', 'DIS', 'PSY', 
                'Paranoid', 'Schizoid', 'Schizotypal', 'Antisocial', 'Borderline', 
                'Histrionic', 'Narcissistic', 'Avoidant', 'Dependent', 'Obsessive_compulsive', 
                'DEPRS', 'LIFEQ', 'MANIC', 'PANIC', 'PSYCS', 'SA', 
                'SCONF', 'SEXFN', 'SLEEP', 'SUICD', 'VIOLN', 'WORKF', 'SESSION')], 
          'TxOutcomePrediction/pid5TOP_valid.csv')

##################################################
#PID5 vs. PD in Predicting Treatment Trajectories#
##################################################

outcomeName = c('DEPRS', 'LIFEQ', 'MANIC', 'PANIC', 'PSYCS', 'SA', 
                'SCONF', 'SEXFN', 'SLEEP', 'SUICD', 'VIOLN', 'WORKF')

PID5vsPD = data.frame(
  Outcome = outcomeName,
  R2_PID5 = NA,
  R2_PD = NA,
  R2Adj_PID5 = NA,
  R2Adj_PD = NA,
  BIC_PID5 = NA,
  BIC_PD = NA,
  CV_MAE_M_PID5 = NA, 
  CV_MAE_SD_PID5 = NA, 
  CV_MAE_M_SD_PID5 = NA,
  CV_MAE_M_PD = NA, 
  CV_MAE_SD_PD = NA, 
  CV_MAE_M_SD_PD = NA, 
  CV_MAE_pvalue = NA, 
  
  CV_RMSE_M_PID5 = NA,
  CV_RMSE_SD_PID5 = NA,
  CV_RMSE_M_SD_PID5 = NA, 
  CV_RMSE_M_PD = NA,
  CV_RMSE_SD_PD = NA,
  CV_RMSE_M_SD_PD = NA, 
  CV_RMSE_pvalue = NA,
  
  SigPer_PID5 = NA,
  SigPer_PD = NA
)

dir.create('../Results/predictionModels_ml')



for (y in outcomeName){
  
  mlmdata = data.frame(ID = df$ID, 
                       SESSION = df$SESSION, 
                       df[,c('NEG','DET','ANT','DIS','PSY')],
                       Y = df[,y])
  mlmdata =
    mlmdata%>%
    mutate(SESSION = SESSION-1)
  
  if (grepl('ucgL', selectModelBIC[selectModelBIC$Outcome == y, 'Model'],  fixed = F)){
    assign(paste0('mlm_pid5_rd_',y), 
           lmer(Y ~ SESSION + (NEG+DET+ANT+DIS+PSY) + (SESSION|ID), data = mlmdata,
                control = lmerControl(optimizer = "bobyqa")))
    
    assign(paste0('mlm_pid5_',y), 
           lmer(Y ~ SESSION*(NEG+DET+ANT+DIS+PSY) + (SESSION|ID), data = mlmdata,
                control = lmerControl(optimizer = "bobyqa")))
    
  } else if (grepl('ucgQ', selectModelBIC[selectModelBIC$Outcome == y, 'Model'],  fixed = F)){
   
     q_model_rd = function(SESSION, NEG, DET, ANT, DIS, PSY, 
                          b0, b1, b2, b3, b4, b5, b6, b7)(b0 + b1*SESSION + b2*SESSION^2 + 
         b3*NEG + b4*DET + b5*ANT + b6*DIS + b7*PSY)
    
     q_modelg_rd = deriv(body(q_model_rd), 
                      namevec = c('b0', 'b1', 'b2',
                                  'b3', 'b4', 'b5', 'b6', 'b7'), 
                      function.arg = q_model_rd) 
     
     startsite_rd <- c(b0 = 0, b1 = 0,  b2 = 0, b3 = 0, b4 = 0,
                    b5 = 0, b6 = 0, b7 = 0)
     
     assign(paste0('mlm_pid5_rd_',y), 
            nlmer(Y ~ q_modelg_rd(SESSION, NEG, DET, ANT, DIS, PSY, 
                               b0, b1, b2, b3, b4, b5, b6, b7) ~ 
                    (b0+b1+b2|ID), data = mlmdata, start = startsite_rd, 
                  control = glmerControl(optimizer = "bobyqa",
                                         optCtrl = list(maxfun=2e5))))
     
    q_model = function(SESSION, NEG, DET, ANT, DIS, PSY, 
                       b0, b1, b2, b3, b4, b5, b6, b7, 
                       b8, b9, b10, b11, b12, b13, b14, b15, b16, b17) (b0 + b1*SESSION + b2*SESSION^2 + 
         b3*NEG + b4*DET + b5*ANT + b6*DIS + b7*PSY +
         b8*SESSION*NEG + b9*SESSION*DET + b10*SESSION*ANT + 
         b11*SESSION*DIS + b12*SESSION*PSY +
         b13*SESSION^2*NEG + b14*SESSION^2*DET + b15*SESSION^2*ANT + 
         b16*SESSION^2*DIS + b17*SESSION^2*PSY)
    
    q_modelg = deriv(body(q_model), 
                     namevec = c('b0', 'b1', 'b2',
                                 'b3', 'b4', 'b5', 'b6', 'b7', 
                                 'b8', 'b9', 'b10', 'b11', 'b12', 'b13', 'b14', 
                                 'b15', 'b16', 'b17'), 
                     function.arg = q_model) 
    
    startsite <- c(b0 = 0, b1 = 0,  b2 = 0, b3 = 0, b4 = 0,
                   b5 = 0, b6 = 0, b7 = 0, b8 = 0, b9 = 0, 
                   b10 = 0, b11 = 0, b12 = 0, b13 = 0, b14 = 0, b15 = 0, b16 = 0, 
                   b17 = 0)
    
    assign(paste0('mlm_pid5_',y), 
           nlmer(Y ~ q_modelg(SESSION, NEG, DET, ANT, DIS, PSY, 
                              b0, b1, b2, b3, b4, b5, b6, b7, 
                              b8, b9, b10, b11, b12, b13, b14, b15, b16, b17) ~ 
                   (b0+b1+b2|ID), data = mlmdata, start = startsite, 
                 control = glmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun=2e5))))
    
  }
  
}



#######################
#Personality Disorders#
#######################

#contrast_pd = matrix(0, ncol = 10, nrow = 11)
#diag(contrast_pd) = 1
#sampleList2 = c()

PDName = c('Paranoid', 'Schizoid', 'Schizotypal', 'Antisocial', 'Borderline', 
           'Histrionic', 'Narcissistic', 'Avoidant', 'Dependent', 'Obsessive_compulsive')
for (y in outcomeName){
  
  mlmdata = data.frame(ID = df$ID, 
                       SESSION = df$SESSION, 
                       df[,PDName],
                       Y = df[,y])
  
  mlmdata = 
    mlmdata %>%
    mutate(SESSION = SESSION-1) 
  
  if (grepl('ucgL', selectModelBIC[selectModelBIC$Outcome == y, 'Model'],  fixed = F)){
    
    assign(paste0('mlm_pd_rd_',y), 
           lmer(Y ~ SESSION+(Paranoid + Schizoid + Schizotypal + Antisocial + Borderline + 
                               Histrionic + Narcissistic + Avoidant + Dependent + Obsessive_compulsive) + 
                  (SESSION|ID), data = mlmdata,
                control = lmerControl(optimizer = "bobyqa")))
     assign(paste0('mlm_pd_',y), 
           lmer(Y ~ SESSION*(Paranoid + Schizoid + Schizotypal + Antisocial + Borderline + 
                  Histrionic + Narcissistic + Avoidant + Dependent + Obsessive_compulsive) + 
                  (SESSION|ID), data = mlmdata,
                control = lmerControl(optimizer = "bobyqa")))
    
  } else if (grepl('ucgQ', selectModelBIC[selectModelBIC$Outcome == y, 'Model'],  fixed = F)){
   
     q_model_rd = function(SESSION, Paranoid, Schizoid, Schizotypal, Antisocial, 
                       Borderline, Histrionic, Narcissistic, Avoidant, 
                       Dependent, Obsessive_compulsive, 
                       b0, b1, b2, 
                       b3, b4, b5, b6, b7, b8, b9, b10, b11, b12) 
      (b0 + b1*SESSION + b2*SESSION^2 + 
         b3*Paranoid + b4*Schizoid + b5*Schizotypal + b6*Antisocial +
         b7*Borderline + b8*Histrionic + b9*Narcissistic + b10*Avoidant +
         b11*Dependent + b12*Obsessive_compulsive)
    
    q_modelg_rd = deriv(body(q_model_rd), 
                     namevec = c('b0', 'b1', 'b2','b3', 'b4', 'b5', 'b6', 'b7', 'b8', 'b9',
                                 'b10', 'b11', 'b12'), 
                     function.arg = q_model_rd) 
    
    startsite_rd <- c(b0 = 0, b1 = 0,  b2 = 0, b3 = 0, b4 = 0,
                   b5 = 0, b6 = 0, b7 = 0, b8 = 0, b9 = 0, 
                   b10 = 0, b11 = 0, b12 = 0)
    
    assign(paste0('mlm_pd_rd_',y), 
           nlmer(Y ~ q_modelg_rd(SESSION, Paranoid, Schizoid, Schizotypal, Antisocial, 
                              Borderline, Histrionic, Narcissistic, Avoidant, 
                              Dependent, Obsessive_compulsive, 
                              b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12) ~ 
                   (b0+b1+b2|ID), data = mlmdata, start = startsite_rd,
                 control = glmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun=2e5))))
    
    
    q_model = function(SESSION, Paranoid, Schizoid, Schizotypal, Antisocial, 
                       Borderline, Histrionic, Narcissistic, Avoidant, 
                       Dependent, Obsessive_compulsive, 
                       b0, b1, b2, 
                       b3, b4, b5, b6, b7, b8, b9, b10, b11, b12,
                       b13, b14, b15, b16, b17, b18, b19, b20, b21, b22,
                       b23, b24, b25, b26, b27, b28, b29, b30, b31, b32) 
      (b0 + b1*SESSION + b2*SESSION^2 + 
         b3*Paranoid + b4*Schizoid + b5*Schizotypal + b6*Antisocial +
         b7*Borderline + b8*Histrionic + b9*Narcissistic + b10*Avoidant +
         b11*Dependent + b12*Obsessive_compulsive + 
         b13*SESSION*Paranoid +b14*SESSION*Schizoid + b15*SESSION*Schizotypal +
         b16*SESSION*Antisocial +b17*SESSION*Borderline + b18*SESSION*Histrionic +
         b19*SESSION*Narcissistic + b20*SESSION*Avoidant +
         b21*SESSION*Obsessive_compulsive + b22*SESSION*Dependent + 
         b23*SESSION^2*Paranoid + b24*SESSION^2*Schizoid + 
         b25*SESSION^2*Schizotypal + b26*SESSION^2*Antisocial + 
         b27*SESSION^2*Borderline + b28*SESSION^2*Histrionic + 
         b29*SESSION^2*Narcissistic + b30*SESSION^2*Avoidant + 
         b31*SESSION^2*Dependent + b32*SESSION^2*Obsessive_compulsive)
    
    q_modelg = deriv(body(q_model), 
                     namevec = c('b0', 'b1', 'b2','b3', 'b4', 'b5', 'b6', 'b7', 'b8', 'b9',
                                 'b10', 'b11', 'b12', 'b13', 'b14', 'b15', 'b16', 'b17', 
                                 'b18', 'b19', 'b20', 'b21', 'b22', 'b23','b24', 'b25',
                                 'b26', 'b27', 'b28', 'b29', 'b30', 'b31', 'b32'), 
                     function.arg = q_model) 
    
    startsite <- c(b0 = 0, b1 = 0,  b2 = 0, b3 = 0, b4 = 0,
                   b5 = 0, b6 = 0, b7 = 0, b8 = 0, b9 = 0, 
                   b10 = 0, b11 = 0, b12 = 0, 
                   b13 = 0, b14 = 0, b15 = 0, b16 = 0, b17 = 0, 
                   b18 = 0, b19 = 0, b20 = 0, b21 = 0, b22 = 0,
                   b23 = 0, b24 = 0, b25 = 0, b26 = 0, b27 = 0, 
                   b28 = 0, b29 = 0, b30 = 0, b31 = 0, b32 = 0)
    
    assign(paste0('mlm_pd_',y), 
           nlmer(Y ~ q_modelg(SESSION, Paranoid, Schizoid, Schizotypal, Antisocial, 
                              Borderline, Histrionic, Narcissistic, Avoidant, 
                              Dependent, Obsessive_compulsive, 
                              b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, 
                              b12, b13, b14, b15, b16, b17, b18, b19, b20, b21, b22,
                              b23, b24, b25, b26, b27, b28, b29, b30, b31, b32) ~ 
                   (b0+b1+b2|ID), data = mlmdata, start = startsite,
                 control = glmerControl(optimizer = "bobyqa",
                                        optCtrl = list(maxfun=2e5))))
    
  }
  }
  

BIC_comp = data.frame(
  outcome = NA, 
  BIC_D = NA,
  BIC_C = NA,
  Winner = NA
)

adjR2_comp = data.frame(
  outcome = NA, 
  adjR2_D = NA,
  adjR2_C = NA,
  Winner = NA
)

r2 = data.frame(
  outcome = NA, 
  R2_base = NA,
  R2_rd_D = NA,
  R2_D = NA, 
  R2_rd_C = NA,
  R2_C = NA
)

i = 0
for (y in outcomeName){
  i = i+1
  model_d = get(paste0('mlm_pid5_',y))
  model_c = get(paste0('mlm_pd_',y))
  BIC_comp[i,1] = y
  BIC_comp[i,2] = BIC(model_d)
  BIC_comp[i,3] = BIC(model_c)
  BIC_comp[i,4] = ifelse(BIC_comp[i,2] <= BIC_comp[i,3], 'D', 'C') 
  
  n_d = nrow(model_d@frame)
  n_c = nrow(model_c@frame)
  adjR2_comp[i,1] = y
  adjR2_comp[i,2] = 1 - (1-rsquared(model_d)[1])*(n_d-1)/(n_d-17-1)
  adjR2_comp[i,3] = 1 - (1-rsquared(model_c)[1])*(n_c-1)/(n_d-32-1)
  adjR2_comp[i,4] = ifelse(adjR2_comp[i,2] >= adjR2_comp[i,3], 'D', 'C') 
  
  r2[i,1] = y
  r2[i,2] = rsquared(get(paste0(selectModelBIC[selectModelBIC$Outcome == y,2],'_',y)))[1]
  r2[i,3] = rsquared(get(paste0('mlm_pid5_rd_',y)))[1]
  r2[i,4] = rsquared(model_d)[1]
  r2[i,5] = rsquared(get(paste0('mlm_pd_rd_',y)))[1]
  r2[i,6] = rsquared(model_c)[1]
  }


r2
BIC_comp
adjR2_comp


adjustedR2 = 1 - (1-R2)*(n-1)/(n-k-1)
for (y in outcomeName){
  i = i+1
  length(mlm_pid5_DEPRS@resp$weights)
  BIC_comp[i,1] = y
  BIC_comp[i,2] = BIC(get(paste0('mlm_pid5_',y)))
  BIC_comp[i,3] = BIC(get(paste0('mlm_pd_',y)))
  BIC_comp[i,4] = ifelse(BIC_comp[i,2] <= BIC_comp[i,3], 'D', 'C') 
}


BIC(mlm_pid5_DEPRS, mlm_pd_DEPRS)

  capture.output(summary(m_pd), 
                 file = paste0('../Results/predictionModels/pd_', y))
  
  sum_pd = summary(m_pd)
  pdCoefP = sum_pd$coefficients[grep('Paranoid|Schizoid|Schizotypal|Antisocial|Borderline|
                                     Histrionic|Narcissistic|Avoidant|Dependent|Obsessive_compulsive', 
                                     rownames(sum_pd$coefficients)),'Pr(>|t|)']
  sigPer_pd = length(which(pdCoefP < 0.05))/length(pdCoefP)
  
  
  i = i + 1
  PID5vsPD[i, 3] = summary(m_pd)$r.squared
  PID5vsPD[i, 5] = summary(m_pd)$adj.r.squared
  PID5vsPD[i, 7] = BIC(m_pd)
  cv = cv.lm(m_pd)
  PID5vsPD[i, 11] = cv$MAE$mean
  PID5vsPD[i, 12] = cv$MAE$sd
  PID5vsPD[i, 13] = paste0(round(cv$MAE$mean,2),' (', 
                           round(cv$MAE$sd,2),')')  
  
  PID5vsPD[i, 18] = cv$MSE_sqrt$mean
  PID5vsPD[i, 19] = cv$MSE_sqrt$sd
  PID5vsPD[i, 20] = paste0(round(cv$MSE_sqrt$mean,2),' (', 
                           round(cv$MSE_sqrt$sd,2),')') 
  
  
  PID5vsPD[i, 23] = sigPer_pd
  
  Ma = PID5vsPD$CV_MAE_M_PID5[i]
  Mb = PID5vsPD$CV_MAE_M_PD[i]
  Sa = PID5vsPD$CV_MAE_SD_PID5[i]
  Sb = PID5vsPD$CV_MAE_SD_PD[i]
  Sp = sqrt((Sa^2 + Sb^2)/2)
  d = (Ma-Mb)/Sp/sqrt(0.2)
  PID5vsPD[i,14] = 2*pt(q=abs(d), df=18, lower.tail=F)
  
  Ma = PID5vsPD$CV_RMSE_M_PID5[i]
  Mb = PID5vsPD$CV_RMSE_M_PD[i]
  Sa = PID5vsPD$CV_RMSE_SD_PID5[i]
  Sb = PID5vsPD$CV_RMSE_SD_PD[i]
  Sp = sqrt((Sa^2 + Sb^2)/2)
  d = (Ma-Mb)/Sp/sqrt(0.2)
  PID5vsPD[i,21] = 2*pt(q=abs(d), df=18, lower.tail=F)
  
}
mdl = mlm_pid5_DEPRS
sum((as.vector(lme4::fixef(mdl) %*% t(≈) - mdl@resp$y)^2)
as.vector(lme4::fixef(mdl) %*% t(mdl@pp$V))[1:10]
mdl@resp$y[1:10]
mdl@frame
sum((predict(lm(Y ~ (SESSION + I(SESSION^2) )*(NEG+DET+ANT+DIS+ PSY), mlmdata)) -mdl@resp$y)^2) 
sum((predict(ucgLInt_DEPRS, re.form = NA)-mdl@resp$y)^2)


predict(mlm_pd_DEPRS, mlmdata)


round(fixef(mdl),2)

str(mdl@pp)

mdl@resp$y - as.vector(lme4::fixef(mdl) %*% t(mdl@pp$V)

write.csv(PID5vsPD, '../Results/PID5vsPDpredTOP.csv', row.names = F) 



1-sum((predict(mlm_pid5_DEPRS)-mlm_pid5_DEPRS@frame$Y)^2)
  sum((mlm_pid5_DEPRS@frame$Y - mean(mlm_pid5_DEPRS@frame$Y))^2)

  
preddata = mdl@pp$V
preddata = as.data.frame(preddata)
str(preddata)
predcol = c()
for(i in 0:17){
  predcol = c(predcol, paste0('b',i))
}
colnames(preddata) = predcol
str(preddata)

preddata$ID = c(mlm_pid5_DEPRS@frame$ID+1000)

head(predict(mlm_pid5_DEPRS, 
        newdata = as.data.frame(preddata),
        allow.new.levels = TRUE))

mlm_pid5_DEPRS@frame
?r
