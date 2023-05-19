rsquared = function(mdl){
  VarF = var(as.vector(lme4::fixef(mdl) %*% t(mdl@pp$V)))
  VarRand <- sum(
    sapply(
      VarCorr(mdl)[!sapply(unique(unlist(strsplit(names(ranef(mdl)),":|/"))), 
                           function(l) length(unique(mdl@frame[,l])) == nrow(mdl@frame))],
      function(Sigma) {
        X <- mdl@pp$V
        colnames(X) = colnames(model.matrix(mdl))
        Z <- X[,rownames(Sigma)]
        sum(diag(Z %*% Sigma %*% t(Z)))/nrow(X) } ) )
  VarResid = attr(lme4::VarCorr(mdl), "sc")^2
  R2m = VarF/(VarF + VarRand + VarResid)
  R2c = (VarF + VarRand)/(VarF + VarRand + VarResid)
  
  return(data.frame(R2m = R2m, R2c = R2c))
  
} 

