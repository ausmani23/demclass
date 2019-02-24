#we generate a DGP 
#that looks like Polity2 and DCAP
#following model of Keele and Kelly 2006

#y_t = alpha * y_t-1 + beta * x_t + u_t
#x_t = rho * x_t-1 + e_1t
#u_t = phi * u_t-1 + e_2t

###

#check: how autocorrelated is the error structure?
tmpname<-'polity2.one.preferred'
tmpdf<-samps[[tmpname]]
tmpdf$residuals<-mods[[tmpname]]$residuals
tmporder<-order(
  tmpdf$cowcode.num,
  tmpdf$year
)
tmpdf<-tmpdf[tmporder,]
tmpdf$L.residuals<-tapply(
  tmpdf$residuals,
  tmpdf$cowcode.num,
  function(x)
    dplyr::lag(x,1)
) %>% unlist
m.tmp<-lm(data=tmpdf,residuals ~ L.residuals); summary(m.tmp)
phi_observed<-coef(m.tmp)[names(coef(m.tmp))=='L.residuals'] %>% 
  unname #about 0.06

#also check: standardized impact of dcap
tmp<-finaldf$iv=="highcapratio" & 
  finaldf$ivspec=="preferred" & 
  finaldf$type=="shortrun" &
  finaldf$df=="one"
beta_observed<-finaldf$musd[tmp] %>% mean #about 0.02

#phi_observed and beta_observed help set reasonable bounds
#i.e., they delimit the sample space of the simulation

#if I want to run simulation again?
runfull<-T
if(runfull) {
  
  #extra pack
  require(prais)
  ###
  
  #set some global params
  #and loop through these params
  set.seed(23)
  Ns<-c(20,75,130,250)
  reps<-1:100
  phi<-seq(0,0.10,by=0.01)
  beta<-seq(0.01,0.1,by=0.01)
  loopdf<-expand.grid(
    phi=phi,
    beta=beta,
    N=Ns,
    rep=reps
  )
  loopdf$k<-1:nrow(loopdf)
  
  #get alpha, from data
  tmpdf<-demdfs$one
  #tmpdf<-samps$polity2.one.preferred
  m.tmp<-lm(data=tmpdf,polity2 ~ L.polity2 + factor(cowcode.num))
  tmpsum<-summary(m.tmp)
  alpha<-coef(m.tmp)[names(coef(m.tmp))=='L.polity2'] %>% unname
  #highly autocorrelated
  
  #get rho, from data
  #we also want a autocorrelated X value,
  #which has the autocorrelation of DCAP
  tmpdf<-demdfs$one
  m.tmp<-lm(data=tmpdf,highcapratio ~ L.highcapratio + factor(cowcode.num))
  rho<-coef(m.tmp)[names(coef(m.tmp))=='L.highcapratio'] %>% unname
  #highly autocorrelated
  
  ###
  
  #loop through
  
  biasdf<-lapply(loopdf$k,function(k) {
    
    #k<-12
    
    #track progress
    print(
      paste(
        k,"of",max(loopdf$k)
      )
    )
    
    #get params
    thisphi<-loopdf$phi[k]
    thisbeta<-loopdf$beta[k]
    thisrep<-loopdf$rep[k]
    thisN<-loopdf$N[k]
    
    #x generated here;
    #random vals w/ this serial correlation
    e1<-rnorm(thisN)
    x<-rep(NA,thisN)
    x[1]<-rnorm(1)
    for(i in 2:thisN)
      x[i]<-rho * x[i-1] + e1[i]
    #check
    # tmpdf<-data.frame(x=x)
    # tmpdf$L.x<-dplyr::lag(x,1)
    # lm(data=tmpdf,formula=x ~ L.x) #roughly
    
    #u generated here
    e2<-rnorm(thisN)
    u<-rep(NA,thisN)
    u[1]<-rnorm(1)
    for(i in 2:thisN)
      u[i]<-thisphi * u[i-1] + e2[i]
    
    #now, generate y
    y<-rep(NA,thisN)
    y[1]<-rnorm(1)
    for(i in 2:thisN)
      y[i]<-alpha * y[i-1] + thisbeta * x[i-1] + u[i]
    
    ###
    
    #estimate, calculate bias
    estdf<-data.frame(
      y=y,
      x=x
    )
    estdf$L.y<-dplyr::lag(estdf$y,1)
    
    #estimate different models
    m.ols<-lm(data=estdf,formula=y ~ x)
    require(prais)
    m.prais<-prais.winsten(data=estdf,formula=y~x)
    m.adl<-lm(data=estdf,formula=y ~ L.y + x)
    
    modlist<-list(
      ols=m.ols,
      prais=m.prais,
      adl=m.adl
    )
    tmpseq.i<-seq_along(modlist)
    returndf<-lapply(tmpseq.i,function(i) {
      #i<-3
      mod<-modlist[[i]]
      modname<-names(modlist)[i]
      if(modname=="prais") {
        mubeta<-coef(mod[[1]])[2,1]
      } else {
        mubeta<-coef(mod)[names(coef(mod))=='x']
      }
      if(modname=="adl") {
        mualpha<-coef(mod)[names(coef(mod))=="L.y"]
      } else {
        mualpha<-NA
      }
      #get the coefficient
      data.frame(
        modname=modname,
        mubeta=mubeta,
        mualpha=mualpha,
        stringsAsFactors=F
      )
    }) %>% rbind.fill
    returndf$bias<-100 * (returndf$mubeta - thisbeta)/thisbeta
    tmp<-returndf$modname=="adl"
    est_lrm<-returndf$mubeta[tmp]/(1-returndf$mualpha[tmp])
    act_lrm<-thisbeta/(1-alpha)
    returndf$bias_lrm<-NA
    returndf$bias_lrm[tmp]<-100 * (est_lrm - act_lrm)/act_lrm
    returndf$k<-k
    returndf
    
  }) %>% rbind.fill
  
  ###
  
  #merge
  biasdf<-merge(
    loopdf,
    biasdf
  )
  head(biasdf)
  tail(biasdf)
  
  ###
  
  #w/ lots of reps,
  #this takes time,
  #so: option to run it w/o
  #running all simulations
  #on repeat
  setwd(outputdir); dir()
  write.csv(
    biasdf,
    "biasdf.csv",
    row.names=F
  )
  
} else {
  
  #if not running from scratch,
  #just load old results
  setwd(outputdir)
  biasdf<-read.csv(
    'biasdf.csv',
    stringsAsFactors=F
  )
  
}


#########################################################
#########################################################

#we will really be interested in bias2
#which is the bias comparing LRM from ADL
#to mubeta from prais, OLS
tmp<-biasdf$modname=="adl"
biasdf$bias2[!tmp]<-biasdf$bias[!tmp]
biasdf$bias2[tmp]<-biasdf$bias_lrm[tmp]

#focus on this
tmpvars<-c("modname","rep","phi","beta","N","bias2")
tmpdf<-biasdf[,tmpvars]
tmpdf<-spread(
  tmpdf,
  modname,
  bias2
)

#obvi, don't care about direction of bias
#just the magnitude; so take abs value
tmpdf$prais<-abs(tmpdf$prais)
tmpdf$adl<-abs(tmpdf$adl)
tmpdf$ols<-abs(tmpdf$ols)

#when is adl better than prais
tmp<-abs(tmpdf$adl)<abs(tmpdf$prais)
sum(tmp)/nrow(tmpdf); sum(!tmp)/nrow(tmpdf)

#calculate improvement from using ADL vs. prais
tmpdf$pctimp_adl<-100 * 
  (tmpdf$prais - tmpdf$adl) / 
  (tmpdf$prais)

#also calc improvement from using prais vs. OLS
#this is for the purpose of comparison
tmpdf$pctimp_prais<-100 * 
  (tmpdf$ols - tmpdf$prais) / 
  (tmpdf$prais)


###

#manipulating this is easiest when long
gathcols<-c(
  "adl",
  "ols",
  "prais",
  "pctimp_adl",
  "pctimp_prais"
)
tmpdf<-tidyr::gather_(
  tmpdf,
  "var",
  "val",
  gathcols
)

#for writing, report median bias from each
tmpoutput<-tapply(
  tmpdf$val,
  list(tmpdf$var,tmpdf$N),
  quantile,
  c(0.5)
)
print(tmpoutput)

tmpoutput<-tapply(
  tmpdf$val,
  list(tmpdf$var,tmpdf$phi),
  quantile,
  c(0.5)
)
print(tmpoutput)

tmpoutput<-tapply(
  tmpdf$val,
  list(tmpdf$var,tmpdf$beta),
  quantile,
  c(0.5)
)
print(tmpoutput)

#calculate: dist of bias from each,
#dist of pct improvement from ADL
require(data.table)
tmpdf<-data.table(tmpdf)
avgdf<-tmpdf[
  ,
  .(
    mu=median(val),
    mu.min=quantile(val,0.025),
    mu.max=quantile(val,0.975)
  )
  ,
  by=c(
    "phi",
    "beta",
    "N",
    "var"
  )
  ]

#for plotting, we plot median bias of adl and ecm
#and the median reduction in bias
levelsdf<-avgdf[avgdf$var%in%c("adl","ols","prais")]
impdf<-avgdf[avgdf$var=="pctimp_adl"]
mean(impdf$mu)
median(impdf$mu)

#we can also take a look at whether
#the 95\% CI crosses 0, though this result
#is unstable in shorter T, which 
#is something future work could investigate
#earlier simulations suggest that as T -> infinity
#this becomes 'significant', in this sense
tmp<-impdf$mu.min>0 & impdf$mu.max>0
impdf$sig<-F
impdf$sig[tmp]<-T
sum(impdf$sig)
sum(!impdf$sig)


#for writing, also check improvement from prais
tmpdf<-avgdf[avgdf$var=="pctimp_prais"]
mean(tmpdf$mu)
median(tmpdf$mu)

#########################################################
#########################################################

#plotting prelims
require(ggplot2)
require(ggthemes)
require(extrafont)
require(RColorBrewer)
require(scales)
#load fonts
loadfonts(quiet=T) #register w/ pdf
loadfonts(device = "win",quiet=T) #register w/ windows
#fonts()
#get ghostscript, for tex output
gsdir<-file.path(
  "c:",
  "Program Files",
  "gs"
)
gsdir_full<-file.path(
  gsdir,
  dir(gsdir),
  "bin",
  "gswin64c.exe"
)
Sys.setenv(
  R_GSCMD = gsdir_full
)
#initialize graphlist
gs.list<-list()

#########################################################
#########################################################

#plot #1 - plot levels of bias
tmp<-levelsdf$var!="ols"
plotdf<-levelsdf[tmp,]

tmplevels<-c(
  "ols",
  "adl",
  "prais"
)
tmplabels<-c(
  "OLS",
  "LDV",
  "Prais Winsten"
)
plotdf$var<-factor(
  plotdf$var,
  tmplevels,
  tmplabels
)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=phi,
    y=beta,
    fill=log(mu)
  )
) +
  geom_tile() +
  facet_wrap(
    ~ N + var,
    ncol=1
  ) +
  geom_hline(
    yintercept=beta_observed,
    linetype='dashed'
  ) +
  scale_fill_continuous(
    #limits=c(0,100),
    #breaks=seq(0,100,by=20),
    name="Log % Bias"
  ) +
  xlab(expression(phi)) +
  ylab(expression(beta)) +
  theme_bw(
    base_family="CM Roman",
    base_size=14
  )
g.tmp

tmpname<-"fig_montecarlo_lev.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8,
  height=6
)


#plot #2 - plot improvement from ADL vs. Prais
plotdf<-impdf

tmplevels<-unique(plotdf$N) %>% sort
tmplabels<-paste0("N=",tmplevels)
plotdf$N<-factor(
  plotdf$N,
  tmplevels,
  tmplabels
)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=phi,
    y=beta,
    fill=mu
  )
) + 
  geom_tile() +
  geom_hline(
    yintercept=beta_observed,
    linetype='dashed'
  ) +
  scale_fill_continuous(
    limits=c(0,100),
    breaks=seq(0,100,by=20),
    name="% Less Bias"
  ) +
  facet_wrap(
    ~ N,
    ncol=1
  ) +
  xlab(expression(phi)) +
  ylab(expression(beta)) +
  theme_bw(
    base_family="CM Roman",
    base_size=14
  )
g.tmp
tmpname<-"fig_montecarlo.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=6,
  height=8
)

#########################################################
#########################################################

#OUTPUT
#output graphlist
setwd(outputdir)
this.sequence<-seq_along(gs.list)
for(i in this.sequence) {
  print(
    paste0(
      "saving ",i," of ",length(this.sequence)
    )
  )
  thiselement<-gs.list[[i]]
  ggsave(
    filename="tmp.pdf",
    plot=thiselement$graph,
    width=thiselement$width,
    height=thiselement$height
  )
  #embed font
  embed_fonts(
    file="tmp.pdf",
    outfile=thiselement$filename
  )
  file.remove(
    "tmp.pdf"
  )
  Sys.sleep(1)
}


