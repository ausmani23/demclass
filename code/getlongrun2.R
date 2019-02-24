#HELPER FUNCTION

getlongrun2<-function(
  m,
  vcov,
  dv,
  iv,
  ivsd
) {
  ####
  # m<-m.tmp
  # vcov<-m.tmp$vcov
  # dv<-thisdv
  # iv<-"beopct_all"
  # ivsd<-1
  ####
  #get the coefficients
  tmpclass<-class(m)[1]
  if(tmpclass=="plm") {
    coefs<-m$coefficients
  } else if(tmpclass=="lmerMod") {
    coefs<-coef(summary(m))[,'Estimate']
  } else {
    stop("Not written for this kind of mod.")
  }
  ###
  #get dv terms
  tmpregex<-paste0(dv,"$")
  tmprows<-str_detect(names(coefs),tmpregex)
  lagdv.terms<-names(coefs)[tmprows]
  #get iv terms
  tmpregex<-paste0(iv,"$")
  tmprows<-str_detect(names(coefs),tmpregex)
  iv.terms<-names(coefs)[tmprows]
  means<-c(
    coefs[lagdv.terms],
    coefs[iv.terms]
  )
  #subset vcov
  rows<-row.names(vcov)%in%c(lagdv.terms,iv.terms)
  cols<-colnames(vcov)%in%c(lagdv.terms,iv.terms)
  vcov.useme<-vcov[rows,cols]
  new.order<-match(names(means),row.names(vcov.useme))
  vcov.useme<-vcov.useme[new.order,new.order]
  #draw from multivatriate normal dist
  draws<-MASS::mvrnorm(n=reps,mu=means,Sigma=vcov.useme)
  numerator<-apply(draws[,iv.terms] %>% as.matrix,1,sum) #sum iv terms
  denominator<- 1 - apply(draws[,lagdv.terms] %>% as.matrix,1,sum) #sum dv terms
  lrm.distribution<-numerator/denominator
  #put this distribution in meaningful units
  lrm.distribution<-lrm.distribution * ivsd
  returnrow<-summarize.distribution2(lrm.distribution)
  return(returnrow)
}