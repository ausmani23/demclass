#this is a predict function for plm objects
#written to fix the problem of new levels 
#w/ option to pass it a beta vector, 
#for predictions w/ reps

predict2 <- function (object, newdata, mybeta=NULL) {
  
  ###
  #DEBUGGING
  # object<-thismod
  # newdata<-thisdf[1,]
  # mybeta<-thisbeta
  
  ###
  #should be no NA's,
  #else this won't work
  hasNA<-apply(
    newdata,
    1,
    function(x) {
      sum(is.na(x))>0
    }
  )
  if(hasNA)
    stop('You cant pass me NAs')
  
  ####
  #MODEL MATRIX
  
  #model matrix w/o response var wanted
  tt <- terms(object)
  Terms <- delete.response(tt)
  #get the xlevels from the object matrix
  obj.model<-object$model
  tmp<-names(object$model)
  tmp<-tmp[str_detect(tmp,"factor")]
  xlevels<-lapply(tmp,function(v) {
    levels(obj.model[,v])
  })
  names(xlevels)<-tmp
  #get the model frame
  m <- model.frame(
    Terms, 
    newdata,
    xlev = xlevels
  )
  #make the model matrix
  X <- model.matrix(
    Terms, 
    m,
    contrasts.arg = object$contrasts
  )
  
  ###
  #ADJUST BETAS
  
  #get coefs
  if(is.null(mybeta))
    mybeta <- coef(object)

  #do by dataframe, to be sure
  myX<-as.matrix(X[,])
  Xdf<-data.frame(
    covar=row.names(myX),
    val=unname(myX)
  )
  betadf<-data.frame(
    covar=names(mybeta),
    beta=unname(mybeta)
  )
  tmpdf<-merge(
    Xdf,
    betadf
  )
  
  ###
  #COMPUTE!
  sum(tmpdf$val*tmpdf$beta)
  
}


