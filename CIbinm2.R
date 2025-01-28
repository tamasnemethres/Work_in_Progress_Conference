################################################################################
#CIbinm2 function
################################################################################
CIbinm2<-function(binglm){
  CI<-sapply(c(-1,0,1),function(x) round(exp(binglm$coefficients+x*qnorm(0.975)*sqrt(diag(vcov(binglm)))),3))
  CI<-matrix(CI,ncol=3)
  rownames(CI) <- names(binglm$coefficients)
  colnames(CI) <- c("lower limit","estimate","upper limit"); print(CI)
}