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

#load dfs again
setwd(datadir); dir()
demdfs<-readRDS(
  'demdfs.RDS'
)

#########################################################
#########################################################

#SET UP REG

#dv
dvs<-varsdf$varname[varsdf$type=="uniondv"]

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
  "dcapbvrt",
  "dcapfull"
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
modsdf$seq<-1:nrow(modsdf)

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
  #ivs
  ivs<-
    paste0("L.",specslist[[thisrow$ivspec]]) %>%
    paste0(collapse=" + ")
  #TOGETHER
  rhs<-paste(
    thisfe.yr,
    thislagdv,
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
#each dv/df has a sample

sampsdf<-expand.grid(
  dv=unique(modsdf$dv),
  df=unique(modsdf$df),
  stringsAsFactors=F
)
sampsdf$sampname<-apply(
  sampsdf,1,paste0,collapse="."
)

sampsdf$minN<-1
sampsdf$minN[sampsdf$df=="ten"]<-2
sampsdf$minN[sampsdf$df=="five"]<-4
sampsdf$minN[sampsdf$df=="one"]<-20

tmpseq.i<-1:nrow(sampsdf)
samps<-lapply(tmpseq.i,function(i) {
  #i<-1
  thisdv<-sampsdf$dv[i]
  thisdf<-sampsdf$df[i]
  this.minN<-sampsdf$minN[i]
  #this is the df
  fulldf<-demdfs[[sampsdf$df[i]]]
  #these are the cols we want
  allforms<-forms[modsdf$dv==thisdv]
  allvars<-lapply(allforms,all.vars) %>%
    unlist %>%
    unique
  #this gives us rows
  tmp<-by(
    fulldf[,allvars],
    fulldf$cowcode.num,
    function(df) {
      sum(complete.cases(df))>=this.minN
    }
  ) 
  goodcows<-names(which(tmp))
  tmprows<-complete.cases(fulldf[,allvars]) &
    fulldf$cowcode.num%in%goodcows 
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
#and for each sample

ivs<-specslist %>%
  unlist %>%
  unname %>%
  unique
allvars<-c(
  dvs,
  ivs
)

#get sds of all ivs
loopdf<-expand.grid(
  var=ivs,
  sampname=sampsdf$sampname,
  stringsAsFactors=F
)
tmpseq.i<-1:nrow(loopdf)
sdsdf<-lapply(tmpseq.i,function(i) {
  #i<-2
  #print(i)
  ###
  thisvar<-loopdf$var[i]
  this.sampname<-loopdf$sampname[i]
  thisdf<-samps[[this.sampname]]
  ###
  if(thisvar%in%names(thisdf)) {
    myvar<-thisvar
  } else {
    tmp<-str_detect(names(thisdf),thisvar)
    if(sum(tmp)!=1) {
      stop()
    }
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
  data.frame(
    var=thisvar,
    sampname=this.sampname,
    avg=avg,
    sd=sd,
    range=rng,
    stringsAsFactors=F
  )
}) %>% rbind.fill

#DV's SDS
tmpseq.i<-seq_along(dvs)
dvsdf<-lapply(tmpseq.i,function(i) {
  #i<-1
  thisdv<-dvs[i]
  #get all samps involved in
  tmp<-sampsdf$dv==thisdv
  sampnames<-sampsdf$sampname[tmp]
  #loop through each sampname
  returndf<-lapply(sampnames,function(this.sampname) {
    thisdf<-samps[[this.sampname]]
    avg<-mean(thisdf[[thisdv]],na.rm=T) 
    sd<-tapply(
      thisdf[[thisdv]],
      thisdf$cowcode.num,
      sd,na.rm=T
    ) %>% mean(na.rm=T) 
    rng<-tapply(
      thisdf[[thisdv]],
      thisdf$cowcode.num,
      function(x) {
        diff(
          quantile(x,c(0.2,0.8),na.rm=T)
        ) %>% abs
      } 
    ) %>% mean(na.rm=T)
    data.frame(
      var=thisdv,
      sampname=this.sampname,
      avg=avg,
      sd=sd,
      range=rng,
      stringsAsFactors=F
    )
  }) %>% rbind.fill
  returndf
}) %>% rbind.fill



#########################################################
#########################################################

#RUN REGS
this.sequence<-seq_along(forms)
mods<-lapply(this.sequence,function(i) {
  #i<-1
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
  tmp<-sampsdf$dv==thisdv & 
    sampsdf$df==thisdf
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

#LOOP THROUGH

tmpseq.i<-seq_along(mods)
dcapestsdf<-lapply(tmpseq.i,function(i) {
  
  #i<-1
  print(i)
  #get mod and vcov
  m.tmp<-mods[[i]]
  vcov.tmp<-vcovHC(
    m.tmp,
    type="HC1",
    cluster="group"
  )
  
  #get sd and deets
  thisdv<-modsdf$dv[i]
  thisdf<-modsdf$df[i]
  tmp<-sampsdf$dv==thisdv & 
    sampsdf$df==thisdf
  this.sampname<-sampsdf$sampname[tmp]
  keyvar<-c("highcapratio")
  tmp<-sdsdf$var==keyvar &
    sdsdf$sampname==this.sampname
  keyvar.sd<-sdsdf$sd[tmp]
  
  #get the coefficients
  coefs<-m.tmp$coefficients
  ###
  #get dv terms
  tmpregex<-paste0(thisdv,"$")
  tmprows<-str_detect(names(coefs),tmpregex)
  lagdv.terms<-names(coefs)[tmprows]
  #get iv terms (includes int term)
  tmpregex<-paste0("highcapratio")
  tmprows<-str_detect(names(coefs),tmpregex)
  iv.terms<-names(coefs)[tmprows]
  all.terms<-c(
    lagdv.terms,
    iv.terms
  )
  means<-coefs[all.terms]
  
  #subset vcov
  rows<-row.names(vcov.tmp)%in%c(all.terms)
  cols<-colnames(vcov.tmp)%in%c(all.terms)
  vcov.useme<-vcov.tmp[rows,cols]
  new.order<-match(names(means),row.names(vcov.useme))
  vcov.useme<-vcov.useme[new.order,new.order]
  #draw from multivatriate normal dist
  draws<-MASS::mvrnorm(n=reps,mu=means,Sigma=vcov.useme)
  denominator<- 1 - apply(draws[,lagdv.terms] %>% as.matrix,1,sum) #sum dv terms
  numerator<-apply(
    (draws[,iv.terms]*keyvar.sd) %>% 
      as.matrix,
    1,sum
  )
  lrm.distribution<-numerator/denominator
  returndf<-summarize.distribution2(lrm.distribution)
  returndf$iv<-"highcapratio"
  returndf$seq<-i
  returndf
}) %>% rbind.fill

#########################################################
#########################################################

#OUTPUT

#merge in model info
intersect(
  names(dcapestsdf),
  names(modsdf)
)
dcapestsdf<-merge(
  dcapestsdf,
  modsdf,
  by="seq"
)

#merge in samp info
intersect(
  names(dcapestsdf),
  names(sampsdf)
)
dcapestsdf<-merge(
  dcapestsdf,
  sampsdf,
  by=c(
    "dv",
    "df"
  )
)

#merge in dv sd info
tmpdf<-dvsdf[,c("sampname","sd")]
names(tmpdf)[2]<-"dvsd"
intersect(
  names(dcapestsdf),
  names(tmpdf)
)
dcapestsdf<-merge(
  dcapestsdf,
  tmpdf,
  by="sampname"
)

#output
setwd(filesdir)
save.image(
  "dcapmods.RData"
)

