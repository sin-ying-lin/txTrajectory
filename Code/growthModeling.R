###############
#Load Packages#
###############

packages = c('lme4', 'lmerTest', 'ggplot2', 'grid', 'gridExtra', 'cowplot',  'MuMIn', 'dplyr')
for (package in packages){
  if(!require(package, character.only = T)){
    install.packages(package)
    library(package, character.only = T)
  }
}

source('rsquared_mlm.R')

###########
#Load Data#
###########

df = read.csv('../Data/pid5PDDemoTop_valid.csv')
outcomeTypes = c('DEPRS', 'MANIC', 'PANIC', 'PSYCS', 'SA', 'SLEEP', 
                 'SUICD', 'VIOLN',
                 'SCONF', 'WORKF', 'SEXFN', 'LIFEQ') 


###################
#Plot Trajectories#
###################

cols = c("Linear" = "#E69F00", "Quadratic" = "#56B4E9", "Loess" = "#009E73")

for (y in outcomeTypes){
  
  plotdata = data.frame(SESSION = df$SESSION, 
                        Y = df[,y])
  assign(paste0('plot_',y),  
  ggplot(data  = plotdata,
         aes(x = SESSION,
             y = Y))+
    geom_point(size = 1.2,
               alpha = .8,
               position = "jitter") +# to add some random noise for plotting purposes
    theme_classic() +
    theme(legend.position = 'none') +
    ylab(y) +
    stat_smooth(method = lm,
                se= F,
                formula = y~x, 
                size   = 2, 
                aes(colour = 'Linear')) + 
    stat_smooth(method = lm,
                formula = y~poly(x,2), 
                se = F,
                size   = 2,
                aes(colour = 'Quadratic')) + 
    stat_smooth(method = 'loess',
                formula = y~x, 
                se = F,
                size   = 2, 
                aes( colour = 'Loess')) +
    scale_colour_manual(name="Legend", 
                        breaks = c("Linear", 'Quadratic', 'Loess'),
                        values = cols) 
  )
}

sharedLegend = get_legend(plot_DEPRS + theme(legend.position= 'right'))

pdf('../Results/combinedPlot.pdf')
grid.arrange(plot_DEPRS, plot_MANIC, plot_PANIC, 
             plot_PSYCS,plot_SA, plot_SLEEP, plot_SUICD, plot_VIOLN, 
            plot_SCONF, plot_WORKF, plot_SEXFN, plot_LIFEQ,  sharedLegend, nrow = 5)
dev.off()


#####################
#Multilevel Modeling#
#####################

modelR2M =
  modelR2C = 
  modelBIC = 
  data.frame(Outcome = outcomeTypes, 
             ICC = NA,
             base = NA,
             ucgLInt = NA, 
             ucgLSlp = NA, 
             ucgQInt = NA, 
             ucgQSlp = NA, 
             ucgCInt = NA, 
             ucgCSlp = NA
             )

dir.create('../Results/growthModels')
             
for (y in outcomeTypes){
  
  mlmdata = data.frame(ID = df$ID, 
                       SESSION = df$SESSION, 
                       df[,c('NEG','DET','ANT','DIS','PSY')],
                       df[,49:58],
                       Y = df[,y])
  mlmdata = mlmdata %>%
    mutate(SESSION = SESSION-1)
  
  
  
  ################
  #Baseline Model#
  ################
  
  assign(paste0('base_', y), lmer(Y ~ 1 + (1|ID), data = mlmdata, REML = F))
  sumBase = summary(get(paste0('base_', y)))
  capture.output(sumBase, file = paste0('../Results/growthModels/base_', y))
  
  ##ICC
  modelR2M[modelR2M$Outcome == y, 2]  = 
    modelR2C[modelR2C$Outcome == y, 2]  = 
    modelBIC[modelBIC$Outcome == y, 2] = 
    as.data.frame(sumBase$varcor)[1,'vcov']/sum(as.data.frame(sumBase$varcor)[,'vcov'])
  
  ##R2 and BIC
  modelR2M[modelR2M$Outcome == y, 3] = rsquared(get(paste0('base_', y)))[1]
  modelR2C[modelR2C$Outcome == y, 3] = rsquared(get(paste0('base_', y)))[2]
  modelBIC[modelBIC$Outcome == y, 3] = BIC(get(paste0('base_', y)))
  
  ##########################################################
  #Unconditional Linear Growth Model with Random Intercepts#
  ##########################################################

  assign(paste0('ucgLInt_', y), lmer(Y ~ SESSION + (1|ID), data = mlmdata, REML = F))
  assign(paste0('sumUcgLInt_', y), summary(get(paste0('ucgLInt_', y))))
  capture.output(get(paste0('sumUcgLInt_', y)), 
                 file = paste0('../Results/growthModels/ucgLInt_', y))
  
  ##R2 and BIC
  #if (is.null(get(paste0('sumUcgLInt_', y))$optinfo$conv$lme4[1][[1]])){
    modelR2M[modelR2M$Outcome == y, 4] = rsquared(get(paste0('ucgLInt_', y)))[1]
    modelR2C[modelR2C$Outcome == y, 4] = rsquared(get(paste0('ucgLInt_', y)))[2]
    modelBIC[modelBIC$Outcome == y, 4] = BIC(get(paste0('ucgLInt_', y)))
  #}
  
  #####################################################################
  #Unconditional Linear Growth Model with Random Intercepts and Slopes#
  #####################################################################
  assign(paste0('ucgLSlp_', y),
         lmer(Y ~ SESSION + (SESSION|ID), data = mlmdata, REML = F))
  assign(paste0('sumUcgLSlp_', y), summary(get(paste0('ucgLSlp_', y))))
  
  capture.output(get(paste0('sumUcgLSlp_', y)), 
                 file = paste0('../Results/growthModels/ucgLSlp_', y))
  
  ##R2 and BIC
  #if (is.null(get(paste0('sumUcgLSlp_', y))$optinfo$conv$lme4[1][[1]])){
    modelR2M[modelR2M$Outcome == y, 5] = rsquared(get(paste0('ucgLSlp_', y)))[1]
    modelR2C[modelR2C$Outcome == y, 5] = rsquared(get(paste0('ucgLSlp_', y)))[2]
    modelBIC[modelBIC$Outcome == y, 5] = BIC(get(paste0('ucgLSlp_', y)))
   # }
    
  #############################################################  
  #Unconditional Quadratic Growth Model with Random Intercepts#
  #############################################################
  q_model = function(SESSION, b0, b1, b2) (b0 + b1*SESSION + b2*SESSION^2)
  q_modelg = deriv(body(q_model), 
                   namevec = c('b0', 'b1', 'b2'), 
                   function.arg = q_model) 
  startsite <- c(b0 = 0, b1 = 0,  b2 = 0)
  
  assign(paste0('ucgQInt_', y),
         nlmer(Y ~ q_modelg(SESSION, b0, b1, b2) ~ (b0|ID), data = mlmdata,
               start = startsite))
  assign(paste0('sumUcgQInt_', y), summary(get(paste0('ucgQInt_', y))))
  capture.output(get(paste0('sumUcgQInt_', y)), 
                 file = paste0('../Results/growthModels/ucgQInt_', y))
  
  ##R2 and BIC
  #if (is.null(get(paste0('sumUcgQInt_', y))$optinfo$conv$lme4[1][[1]])){
    modelR2M[modelR2M$Outcome == y, 6] = rsquared(get(paste0('ucgQInt_', y)))[1]
    modelR2C[modelR2C$Outcome == y, 6] = rsquared(get(paste0('ucgQInt_', y)))[2]
    modelBIC[modelBIC$Outcome == y, 6] = BIC(get(paste0('ucgQInt_', y)))
  #}
  
  ######################################################################  
  #Unconditional Quadratic Growth Model with Random Intercepts & Slopes#
  ######################################################################
  
  assign(paste0('ucgQSlp_', y),
         nlmer(Y ~ q_modelg(SESSION, b0, b1, b2) ~ (b0+b1+b2|ID), data = mlmdata,
               start = startsite))
  assign(paste0('sumUcgQSlp_', y), summary(get(paste0('ucgQSlp_', y))))
  capture.output(get(paste0('sumUcgQSlp_', y)), 
                 file = paste0('../Results/growthModels/ucgQSlp_', y))

  
  ##R2 and BIC
  if (is.null(get(paste0('sumUcgQSlp_', y))$optinfo$conv$lme4[1][[1]])){
    modelR2M[modelR2M$Outcome == y, 7] = rsquared(get(paste0('ucgQSlp_', y)))[1]
    modelR2C[modelR2C$Outcome == y, 7] = rsquared(get(paste0('ucgQSlp_', y)))[2]
    modelBIC[modelBIC$Outcome == y, 7] = BIC(get(paste0('ucgQSlp_', y)))
  }
  
  
  #########################################################
  #Unconditional Cubic Growth Model with Random Intercepts#
  #########################################################
  c_model = function(SESSION, b0, b1, b2, b3) (b0 + b1*SESSION + b2*SESSION^2 + 
                                                 b3*SESSION^3)
  c_modelg = deriv(body(c_model), 
                   namevec = c('b0', 'b1', 'b2', 'b3'), 
                   function.arg = c_model) 
  c_startsite <- c(b0 = 0, b1 = 0,  b2 = 0, b3 = 0)
  assign(paste0('ucgCInt_', y),
         nlmer(Y ~ c_modelg(SESSION, b0, b1, b2, b3) ~ (b0|ID), data = mlmdata,
               start = c_startsite))
  assign(paste0('sumUcgCInt_', y), summary(get(paste0('ucgCInt_', y))))
  capture.output(get(paste0('sumUcgCInt_', y)), 
                 file = paste0('../Results/growthModels/ucgCInt_', y))
  
  ##R2 and BIC
  if (is.null(get(paste0('sumUcgCInt_', y))$optinfo$conv$lme4[1][[1]])){
    modelR2M[modelR2M$Outcome == y, 8] = rsquared(get(paste0('ucgCInt_', y)))[1]
    modelR2C[modelR2C$Outcome == y, 8] = rsquared(get(paste0('ucgCInt_', y)))[2]
    modelBIC[modelBIC$Outcome == y, 8] = BIC(get(paste0('ucgCInt_', y)))
  }
  
  ######################################################################  
  #Unconditional Quadratic Growth Model with Random Intercepts & Slopes#
  ######################################################################
  
  assign(paste0('ucgCSlp_', y),
         nlmer(Y ~ c_modelg(SESSION, b0, b1, b2, b3) ~ (b0+b1+b2+b3|ID), data = mlmdata,
               start = c_startsite))
  assign(paste0('sumUcgCSlp_', y), summary(get(paste0('ucgCSlp_', y))))
  capture.output(get(paste0('sumUcgCSlp_', y)), 
                 file = paste0('../Results/growthModels/ucgCSlp_', y))
  
  
  ##R2 and BIC
  if (is.null(get(paste0('sumUcgCSlp_', y))$optinfo$conv$lme4[1][[1]])){
    modelR2M[modelR2M$Outcome == y, 9] = rsquared(get(paste0('ucgCSlp_', y)))[1]
    modelR2C[modelR2C$Outcome == y, 9] = rsquared(get(paste0('ucgCSlp_', y)))[2]
    modelBIC[modelBIC$Outcome == y, 9] = BIC(get(paste0('ucgCSlp_', y)))
  }
  
  
}

write.csv(modelR2M, '../Results/modelR2M.csv', row.names = F)
write.csv(modelR2C, '../Results/modelR2C.csv', row.names = F)
write.csv(modelBIC, '../Results/modelBIC.csv', row.names = F)


findLowestBIC = function(x){
  outcome = x[1]
  x = x[-c(1:2)]
  return(c(outcome,
           Model = names(x)[which(x == min(x, na.rm = T))]))
}

findLargestR2 = function(x){
  outcome = x[1]
  x = x[-c(1:2)]
  return(c(outcome,
           Model = names(x)[which(x == max(x, na.rm = T))]))
}

    
selectModelBIC = apply(modelBIC, 1, findLowestBIC)
selectModelR2M = apply(modelR2M, 1, findLargestR2)
selectModelR2C = apply(modelR2C, 1, findLargestR2)

write.csv(t(selectModelBIC), '../Results/selectModelBIC.csv', row.names = F)
write.csv(t(selectModelR2M), '../Results/selectModelR2M.csv', row.names = F)
write.csv(t(selectModelR2C), '../Results/selectModelR2C.csv', row.names = F)
