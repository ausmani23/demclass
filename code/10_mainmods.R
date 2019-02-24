#########################################################
#########################################################

#clear workspace
rm(list=ls())

#load packages
require(stringr)
require(plyr)
require(dplyr)
require(zoo)
require(plm)
require(tidyr)
require(rprojroot)

#set dirs
rootdir<-find_root(
  criterion=has_file('_demclass.RProj')
)
codedir<-file.path(rootdir,"code")
setwd(codedir); dir()
source('dirs.R')

setwd(datadir)
load('cowcodes.RData')
#back to homedir
setwd(homedir)

#set params
reps<-10000
set.seed(23)

#load functions
setwd(codedir)
source('demfunctions.R')

#########################################################
#########################################################

#LOAD
setwd(datadir); dir()
demdfs<-readRDS(
  "demdfs.RDS"
)

#########################################################
#########################################################

#SET UP REG

#dv
dvs<-varsdf$varname[varsdf$type=="demdv"]

#dfs
dfs<-c(
  "one",
  "five",
  "ten",
  "fifteen",
  "twenty"
)

#spec
ivspecs<-c(
  "disruptive",
  "landlords",
  "socialforces",
  "modernization",
  "inequality",
  "preferred"
)

specslist<-lapply(ivspecs,function(x) {
  varsdf$varname[varsdf[[x]]==T]
})
names(specslist)<-ivspecs

#########################################################
#########################################################

#GET FORMS

modsdf<-expand.grid(
  dv=dvs,
  df=dfs,
  ivspec=ivspecs,
  stringsAsFactors=F
)
modsdf$mname<-apply(
  modsdf,1,paste0,collapse="."
)

# #tmp trimmer
# tmp<-modsdf$ivspec=="preferred" &
#   modsdf$df=="one"
# modsdf<-modsdf[tmp,]

tmpseq.i<-1:nrow(modsdf)
forms<-lapply(tmpseq.i,function(i) {
  #i<-25
  #get params
  thisrow<-modsdf[i,]
  thisdv<-thisrow$dv
  ####FORMULA
  ##LHS
  lhs<-thisdv
  ##RHS
  #year fe
  thisfe.yr<-"factor(year)"
  #lagdv
  thislagdv<-
    paste0("L.",thisdv)
  #regavg
  #(only add if this is pref model)
  thisregavg<-ifelse(
    thisrow$ivspec=="preferred",
    paste0("L.",thisdv,"_regavg"),
    ""
  )
  #ivs
  ivs<-
    paste0("L.",specslist[[thisrow$ivspec]]) %>%
    paste0(collapse=" + ")
  #TOGETHER
  rhs<-paste(
    thisfe.yr,
    thislagdv,
    thisregavg,
    ivs,
    sep=" + "
  )
  #get rid of extra + signs
  rhs<-str_replace_all(rhs,"\\+\\s+\\+","+ ") %>%
    str_replace("\\s+\\+\\s+$","") %>%
    str_replace("^\\s+\\+\\s+","")
  #RETURN
  #put the formula togeehter
  thisform<-paste(
    lhs,"~",rhs
  ) %>% as.formula
  thisform
})

names(forms)<-modsdf$mname

#########################################################
#########################################################

#GET SAMPS
#each dv/df/spec has a sample

sampsdf<-expand.grid(
  dv=unique(modsdf$dv),
  df=unique(modsdf$df),
  ivspec=unique(modsdf$ivspec),
  stringsAsFactors=F
)
sampsdf$sampname<-apply(
  sampsdf,1,paste0,collapse="."
)

sampsdf$minN<-NA
sampsdf$minN[sampsdf$df=="one"]<-20
sampsdf$minN[sampsdf$df=="five"]<-4
sampsdf$minN[sampsdf$df=="ten"]<-2
sampsdf$minN[sampsdf$df=="fifteen"]<-1
sampsdf$minN[sampsdf$df=="twenty"]<-1

tmpseq.i<-1:nrow(sampsdf)
samps<-lapply(tmpseq.i,function(i) {
  #i<-3
  #print("####")
  #print(i)
  thisdv<-sampsdf$dv[i]
  thisdf<-sampsdf$df[i]
  this.spec<-sampsdf$ivspec[i]
  this.minN<-sampsdf$minN[i]
  #this is the df
  fulldf<-demdfs[[sampsdf$df[i]]]
  #these are the cols we want
  allforms<-forms[modsdf$dv==thisdv & modsdf$ivspec==this.spec]
  allvars<-lapply(allforms,all.vars) %>%
    unlist %>%
    unique
  #this gives us rows
  tmprows<-by(
    fulldf[,c("cowcode.num",allvars)],
    fulldf$cowcode.num,
    function(df) {
      #print(unique(df$cowcode.num))
      #df<-fulldf[fulldf$cowcode.num==385,allvars]
      #grab longest run of consecutive obs
      tmpcases<-complete.cases(df)
      # tmpobs<-as.numeric(tmpcases)
      # #sometimes, nothing to consider
      # if(sum(tmpobs)==0) {
      #   returnrows<-rep(F,nrow(df))
      # } else {
      #   #where it changes?
      #   diffs <- tmpobs[-1] != tmpobs[-length(tmpobs)]
      #   #identify each run
      #   ids <- c(0,cumsum(diffs))
      #   #how many missing in each run
      #   tmptab<-table(ids)
      #   #these are runs to consider
      #   goodids<-unique(ids[tmpobs==1])
      #   tmp<-names(tmptab)%in%goodids
      #   tmptab<-tmptab[tmp]
      #   #pick the largest noe
      #   tmp<-which(tmptab==max(tmptab))
      #   thisid<-names(tmptab)[tmp] %>% as.numeric
      #   #if there is a tie, pick the earlier one
      #   if(length(thisid)>1) 
      #     thisid<-thisid[1]
      #   #this is longest run of non-missing ints
      #   goodrows<-ids==thisid
      #   #if more than min, we're happy
      #   if(sum(goodrows)>=this.minN) {
      #     returnrows<-goodrows
      #   } else {
      #     returnrows<-rep(F,nrow(df))
      #   }
      # }
      if(sum(tmpcases)>=this.minN) {
        returnrows<-tmpcases
      } else {
        returnrows<-rep(F,nrow(df))
      }
      #return
      returnrows
    }) %>% unlist
  #these are extra vars
  idvars<-c(
    "cowcode.num",
    "year"
  )
  extravars<-c(
    #####
  )
  tmpcols<-c(
    idvars,
    extravars,
    allvars
  ) %>% unique
  tmpcols<-tmpcols[tmpcols%in%names(fulldf)]
  #so this is the df
  thisdf<-fulldf[tmprows,tmpcols]
  #return
  thisdf
})
names(samps)<-sampsdf$sampname

sampinfodf<-lapply(tmpseq.i,function(i) {
  #i<-1
  tmpdf<-samps[[i]]
  data.frame(
    N=nrow(tmpdf),
    N.states=length(unique(tmpdf$cowcode.num)),
    range=paste0(
      min(tmpdf$year),"-",max(tmpdf$year)
    )
  )
}) %>% rbind.fill

#sampsdf
sampsdf<-cbind(
  sampsdf,
  sampinfodf
)
sampsdf

#########################################################
#########################################################

#GET SDS
#for each iv and dv
#in the full sample each df
#which means, one SD per var per dv/df
ivs<-specslist %>%
  unlist %>%
  unname %>%
  unique
allvars<-c(
  dvs,
  paste0(dvs,"_regavg"),
  ivs
)

tmp<-str_detect(
  sampsdf$sampname,
  "preferred$"
)
prefsamps<-sampsdf$sampname[tmp]

loopdf<-expand.grid(
  var=allvars,
  sampname=prefsamps,
  stringsAsFactors=F
)

#remove rows that don't apply
tmp<-str_detect(
  loopdf$sampname,
  "polity2\\."
) & str_detect(
  loopdf$var,
  "v2x_polyarchy"
)
loopdf<-loopdf[!tmp,]

tmp<-str_detect(
  loopdf$sampname,
  "v2x_polyarchy\\."
) & str_detect(
  loopdf$var,
  "polity2"
)
loopdf<-loopdf[!tmp,]

tmpseq.i<-1:nrow(loopdf)
sdsdf<-lapply(tmpseq.i,function(i) {
  #i<-81
  #print(i)
  ###
  thisvar<-loopdf$var[i]
  this.sampname<-loopdf$sampname[i]
  thisdf<-samps[[this.sampname]]
  if(thisvar%in%names(thisdf)) {
    myvar<-thisvar
  } else {
    tmp<-str_detect(names(thisdf),thisvar)
    if(sum(tmp)!=1)
      stop()
    myvar<-names(thisdf)[tmp]
  }
  ###
  isdummy<-F
  avg<-mean(thisdf[[myvar]],na.rm=T)
  if(isdummy) {
    sd<-rng<-1
  } else {
    sd<-tapply(
      thisdf[[myvar]],
      thisdf$cowcode.num,
      sd,na.rm=T
    ) %>% mean(na.rm=T) 
    rng<-tapply(
      thisdf[[myvar]],
      thisdf$cowcode.num,
      function(x) {
        diff(
          quantile(x,c(0.2,0.8),na.rm=T)
        ) %>% abs
      } 
    ) %>% mean(na.rm=T)
  }
  #add meta info
  tmp<-sampsdf$sampname==this.sampname
  thisdv<-sampsdf$dv[tmp]
  thisdf<-sampsdf$df[tmp]
  data.frame(
    var=thisvar,
    df=thisdf,
    dv=thisdv,
    avg=avg,
    sd=sd,
    range=rng,
    stringsAsFactors=F
  )
}) %>% rbind.fill

#########################################################
#########################################################

#RUN REGS
this.sequence<-seq_along(forms)
mods<-lapply(this.sequence,function(i) {
  #i<-2
  #progress
  print(
    paste(
      "Estimating model",
      i,"of",max(this.sequence)
    )
  )
  #get params
  thisform<-forms[[i]]
  thisdv<-modsdf$dv[i]
  thisdf<-modsdf$df[i]
  thisivspec<-modsdf$ivspec[i]
  tmp<-sampsdf$dv==thisdv & 
    sampsdf$df==thisdf &
    sampsdf$ivspec==thisivspec
  this.sampname<-sampsdf$sampname[tmp]
  thisdf<-samps[[this.sampname]]
  #estimate
  m.tmp<-plm(
    data=thisdf,
    formula=thisform,
    index=c(
      "cowcode.num",
      "year"
    ),
    model="within"
  )
})
names(mods)<-modsdf$mname

#########################################################
#########################################################

#GET ESTS

#load lmtest
require(lmtest)

this.sequence<-seq_along(mods)
tmpoutput<-lapply(this.sequence,function(i) {
  #i<-1
  #progress
  print(
    paste(
      "Getting results from model",
      i,"of",max(this.sequence)
    )
  )
  #get params
  m<-mods[[i]]
  thismname<-modsdf$mname[i]
  thisdv<-modsdf$dv[i]
  thisdf<-modsdf$df[i]
  thisivspec<-modsdf$ivspec[i]
  tmp<-sampsdf$dv==thisdv & 
    sampsdf$df==thisdf &
    sampsdf$ivspec==thisivspec
  this.sampname<-sampsdf$sampname[tmp]
  
  #get robustvcov coefs
  coefs<-m$coefficients
  thisvcov<-vcovHC(
    m,
    type="HC1",
    cluster="group"
  )
  coefs.tested<-coeftest(m,thisvcov)
  
  #SHORT-RUN
  #all ivs, and lag dvs
  print("SR")
  shortrunvars<-c(
    thisdv,
    paste0(thisdv,"_regavg"),
    ivs
  )
  sr.sequence<-seq_along(shortrunvars)
  shortrundf<-lapply(sr.sequence,function(j) {
    #j<-2
    #print(j)
    #get params
    thisiv<-shortrunvars[j]
    # #if this is the dv, no multiply
    if(thisiv==thisdv) {
      thisiv.sd<-1
    } else {
      tmp<-sdsdf$var==thisiv & 
        sdsdf$df==thisdf & 
        sdsdf$dv==thisdv
      if(sum(tmp)>1)
        stop()
      thisiv.sd<-sdsdf$sd[tmp]
    }
    if(length(thisiv.sd)!=1) stop("SD missing.")
    #get the var(s)
    thevar.regex<-paste0(
      "^(L([0-9]+)?\\.)?(D([0-9]+)?\\.)?",
      thisiv,
      "$"
    )
    thisrow<-str_detect(row.names(coefs.tested),thevar.regex)
    notvar.regex<-paste0("X",thisiv)
    thisrow<-thisrow & !str_detect(row.names(coefs.tested),notvar.regex)
    #if(sum(thisrow)>1)
    #stop(paste(thisiv,"is matching >1 terms"))
    #but don't match square terms
    #thisrow<-thisrow & !str_detect(row.names(coefs.tested),paste0(thisiv,"2"))
    #this gives term
    term<-row.names(coefs.tested)[thisrow]
    est<-coefs.tested[thisrow,"Estimate"]
    se<-coefs.tested[thisrow,"Std. Error"]
    tval.col<-str_detect(colnames(coefs.tested),"t.value")
    t<-coefs.tested[thisrow,tval.col]
    pval<-coefs.tested[thisrow,"Pr(>|t|)"]
    #multiply by iv.sds
    est<-est*thisiv.sd
    se<-se*thisiv.sd
    #now compute
    est.min<-est-1.96*se
    est.max<-est+1.96*se
    #return if was in the model
    if(sum(thisrow)>0) {
      returnrow<-data.frame(
        iv=thisiv,
        term,
        mu=est,
        mu.min=est.min,
        mu.max=est.max,
        se,
        pval,
        t,
        stringsAsFactors=F
      )
    } else {
      returnrow<-data.frame(
        iv=thisiv,
        mu=NA
      )
    }
    #return
    return(returnrow)
  }) %>% rbind.fill
  #identify
  shortrundf$type<-"shortrun"
  #chuck all nas
  shortrundf<-shortrundf[!is.na(shortrundf$mu),]
  
  ##########################################
  
  #LONG-RUN
  print("LR")
  longrunvars<-c(
    paste0(thisdv,"_regavg"),
    ivs
  )
  lroutput<-lapply(seq_along(longrunvars),function(j) {
    #j<-2
    #print(j)
    #get params
    thisiv<-longrunvars[j]
    # #get sd
    #thisiv.sd<-1
    tmp<-sdsdf$var==thisiv & 
      sdsdf$df==thisdf & 
      sdsdf$dv==thisdv
    if(sum(tmp)>1) stop()
    thisiv.sd<-sdsdf$sd[tmp]
    if(length(thisiv.sd)!=1) stop("SD missing.")
    #did we detect var?
    thevar.regex<-paste0(
      "^(L([0-9]+)?\\.)?(D([0-9]+)?\\.)?",
      thisiv,
      "$"
    )
    tmprows<-str_detect(names(coefs),thisiv)
    if(sum(tmprows)==0) {
      returnrow<-data.frame(
        mu=NA
      )
    } else {
      #all ests involving this var
      iv.terms<-names(coefs)[tmprows]
      #all ests with dv
      thisdv.regex<-paste0(
        "^(L([0-9]+)?\\.)?(D([0-9]+)?\\.)?",
        thisdv,
        "$"
      )
      tmprows<-str_detect(names(coefs),thisdv.regex)
      lagdv.terms<-names(coefs)[tmprows]
      #get the longrun estimate
      means<-c(
        coefs[lagdv.terms],
        coefs[iv.terms]
      )
      #get the vcov matrix
      new.vcov<-thisvcov #from above
      rows<-row.names(new.vcov)%in%c(lagdv.terms,iv.terms)
      cols<-colnames(new.vcov)%in%c(lagdv.terms,iv.terms)
      vcov.useme<-new.vcov[rows,cols]
      #vcov needs to be ordered in the same way as the means
      new.order<-match(names(means),row.names(vcov.useme))
      vcov.useme<-vcov.useme[new.order,new.order]
      #sample from the multivariate distribution defined here
      ###to replicate pref results, we need exact random seed
      if(thisivspec=="preferred" & thisdf=="one") {
        setwd(filesdir)
        tmpseed<-.Random.seed
        saveRDS(tmpseed,paste0(thismname,"_",thisiv,"_seed.RDS"))
      }
      ###
      draws<-MASS::mvrnorm(n=reps,mu=means,Sigma=vcov.useme)
      numerator<-apply(draws[,iv.terms] %>% as.matrix,1,sum) #sum iv terms
      denominator<- 1 - apply(draws[,lagdv.terms] %>% as.matrix,1,sum) #sum dv terms
      lrm.distribution<-numerator/denominator
      #put this distribution in meaningful units
      lrm.distribution<-lrm.distribution * thisiv.sd
      returnrow<-summarize.distribution2(lrm.distribution)
      #return this info (but not w/ lag)
      returnrow$iv<-thisiv
    }
    return(returnrow)
  })
  longrundf<-rbind.fill(lroutput)
  #identify as longrun
  longrundf$type<-"longrun"
  #get rid of this when not applicable
  longrundf<-longrundf[!is.na(longrundf$mu),]
  
  ##########################################
  
  #finalize
  thism.estsdf<-rbind.fill(shortrundf,longrundf)
  thism.estsdf$mname<-thismname
  thism.estsdf$seq<-i
  return(thism.estsdf)
})

#put together in df
estsdf<-rbind.fill(tmpoutput)
estsdf

#classify the shortrun pvals into pval class
tmp<-estsdf$type=="shortrun"
estsdf$pval.class[estsdf$pval<0.01 & tmp]<-"at alpha=0.01"
estsdf$pval.class[estsdf$pval>=0.01 & estsdf$pval<0.05 & tmp]<-"at alpha=0.05"
estsdf$pval.class[estsdf$pval>=0.05 & estsdf$pval<0.10 & tmp]<-"at alpha=0.10"
estsdf$pval.class[estsdf$pval>=0.10 & tmp]<-"not sig"
tmp<-is.na(estsdf$pval.class)
if(sum(tmp)>0)
  stop()

#########################################################
#########################################################

#GET FIT

this.sequence<-seq_along(mods)
fitdf<-lapply(this.sequence,function(i) {
  #i<-10
  #get params
  m<-mods[[i]]
  #track progress
  print(paste("Calc fit for model",i,"of",max(this.sequence)))
  thisrow<-calcfits(m)
}) %>% rbind.fill
fitdf$mname<-modsdf$mname

#########################################################
#########################################################

#PUT TOGETHER

#put model info (from specs)
#together with estimates
#and fitdf

mergelist<-list(modsdf,estsdf,fitdf)
finaldf<-Reduce(
  function(...)
    merge(..., by="mname", all=T),
  mergelist
)

#add sample info
finaldf<-merge(
  finaldf,
  sampsdf,
  by=c("dv","df","ivspec")
)
head(finaldf)

#########################################################
#########################################################

#IDENTIFY PREFESTS
#this is only valid if est on consistent sample

# tmplist<-list(
#   finaldf$dv,
#   finaldf$df
# )
# finaldf<-by(finaldf,tmplist,function(df) {
#   #df<-finaldf[finaldf$dv=="incrt_t_jur",]
#   #trim
#   rankvars<-c("mname","aic","bic","r2","adjr2")
#   tmpdf<-unique(df[,rankvars])
#   #rank
#   tmpdf$aic.rank<-rank(tmpdf$aic)
#   tmpdf$bic.rank<-rank(tmpdf$bic)
#   tmpdf$r2.rank<-length(tmpdf$r2) + 1 - rank(tmpdf$r2)
#   tmpdf$adjr2.rank<-length(tmpdf$adjr2) + 1 - rank(tmpdf$adjr2)
#   #pref by AIC
#   tmpdf$pref<-ifelse(tmpdf$aic.rank==1,T,F)
#   #pref by BIC
#   tmpdf$prefbic<-ifelse(tmpdf$bic.rank==1,T,F)
#   #merge back in
#   mergevars<-c("mname","pref","prefbic")
#   merge(
#     df,
#     tmpdf[,mergevars],
#     by="mname"
#   )
# }) %>% rbind.fill

#########################################################
#########################################################

#ADD DVSDS

#merge in dvsd information
tmprows<-sdsdf$var%in%dvs
tmpnames<-c("var","df","sd")
tmpdf<-sdsdf[tmprows,tmpnames]
names(tmpdf)<-c("dv","df","dvsd")
intersect(
  names(tmpdf),
  names(finaldf)
)
finaldf<-merge(
  finaldf,
  tmpdf,
  by=c(
    "dv",
    "df"
  )
)
finaldf$musd<-finaldf$mu/finaldf$dvsd
finaldf$musd.min<-finaldf$mu.min/finaldf$dvsd
finaldf$musd.max<-finaldf$mu.max/finaldf$dvsd

#########################################################
#########################################################

#save out
setwd(filesdir); dir()
save.image(
  "mainmods.RData"
)

setwd(outputdir); dir()
write.csv(
  finaldf,
  'mainests.csv',
  row.names=F
)
