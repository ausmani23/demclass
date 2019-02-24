loopdf<-expand.grid(
  dv=c(
    "lngdpcap",
    "highcapratio",
    "landlords",
    "gini.generous",
    "years.all",
    "urban02"
  ),
  iv=c(
    "year",
    "lngdpcap"
  ),
  subset=c(
    "advanced",
    "developing"
  ),
  stringsAsFactors=F
)

tmpseq.i<-1:nrow(loopdf)
loess.mods<-lapply(tmpseq.i,function(i) {
  tmpvars<-c(
    loopdf$dv[i],
    loopdf$iv[i]
  )
  if(loopdf$subset[i]=="advanced") {
    tmprows<-demdfs$one$advanced
  } else {
    tmprows<-!demdfs$one$advanced
  }
  tmpdf<-demdfs$one[tmprows,tmpvars]
  tmpdf<-tmpdf[complete.cases(tmpdf),]
  m.gdp<-loess(
    tmpdf[[loopdf$dv[i]]] ~ 
      tmpdf[[loopdf$iv[i]]]
  )
  list(
    mod=m.gdp,
    minval=min(tmpdf[[ loopdf$iv[i] ]]),
    maxval=max(tmpdf[[ loopdf$iv[i] ]])
  )
})
names(loess.mods) <- apply(
  loopdf[,c("dv","iv","subset")],
  1,paste0,collapse="."
)

#using one of these smooths, predict!
psmooth<-function(x,dv,iv,subset) {
  #dv<-"dcap"
  #x<-c(5,6,7)
  #change any values below or above min/max,
  name<-paste0(
    dv,
    ".",
    iv,
    ".",
    subset
  )
  thismod<-loess.mods[[name]]
  #to minmax
  x[x<thismod$minval]<-thismod$minval
  x[x>thismod$maxval]<-thismod$maxval
  #now predict
  y<-predict(thismod$mod,x)
  return(y)
}

# x.tmp<-c(1875:2000)
# y.adv<-psmooth(
#   x=x.tmp,
#   dv="urban02",
#   iv="year",
#   subset="advanced"
# )
# y.dev<-psmooth(
#   x=x.tmp,
#   dv="urban02",
#   iv="year",
#   subset="developing"
# )
# 
# plot(x.tmp,y.dev,ylim=c(0,400),col='dark red')
# points(x.tmp,y.adv,col='blue')
# 
# summary(demdfs$one$urban02)
