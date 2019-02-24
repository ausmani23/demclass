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

#load image
setwd(filesdir); dir()
load(
  'mainmods.RData'
)

#load functions
setwd(codedir)
source('demfunctions.R')

#########################################################
#########################################################

#CREATE TABLESLIST

#formatted for list of regtables
#though here I want only one set

regtables<-list()
finaldf$mname %>% unique
regtables[["politytable"]]<-c(
  "polity2.one.disruptive",
  "polity2.one.landlords",
  "polity2.one.socialforces",
  "polity2.one.modernization",
  "polity2.one.inequality",
  "polity2.one.preferred"
)

regtables[["polytable"]]<-c(
  "v2x_polyarchy.one.disruptive",
  "v2x_polyarchy.one.landlords",
  "v2x_polyarchy.one.socialforces",
  "v2x_polyarchy.one.modernization",
  "v2x_polyarchy.one.inequality",
  "v2x_polyarchy.one.preferred"
)

regtables[["politytable_yrs"]]<-c(
  "polity2.one.preferred",
  "polity2.five.preferred",
  "polity2.ten.preferred",
  "polity2.fifteen.preferred",
  "polity2.twenty.preferred"
)

regtables[["polytable_yrs"]]<-c(
  "v2x_polyarchy.one.preferred",
  "v2x_polyarchy.five.preferred",
  "v2x_polyarchy.ten.preferred",
  "v2x_polyarchy.fifteen.preferred",
  "v2x_polyarchy.twenty.preferred"
)

#########################################################
#########################################################

#LOOP THROUGH, PRODUCE TABLES

#sequence to loop through
tmpseq.i<-seq_along(regtables)
for(i in tmpseq.i) {
  #i<-1
  
  #######################################
  #######################################
  
  #get params
  thistab<-regtables[[i]]
  thistabname<-names(regtables)[i]
  
  #track progress
  print("#####")
  print(i)
  print(thistabname)
  
  #subset estimates df
  #subset big df w/ these models
  ssdf<-finaldf[finaldf$mname%in%thistab,]
  thisdv<-unique(ssdf$dv)
  is.ecm<-F #ADL estimated
  
  #######################################
  #######################################
  
  ###GET SHORT RUN
  print("SR")
  tmpdf<-ssdf[ssdf$type=="shortrun",]
  #these are the terms which will appear on the left
  
  #note: here we loop through terms and not vars,
  #as for the long run vars
  terms<-tmpdf$term %>% unique
  #these are models which will be columns
  mods<-thistab
  #already ordered, no ordering her
  
  #loop through and get
  tmpseq.j<-seq_along(terms)
  srests<-lapply(seq_along(terms),function(j) {
    #j<-1
    #print(j)
    thisterm<-terms[j]
    #print(thisiv)
    #get each estimate
    ivrows<-lapply(seq_along(mods),function(k) {
      #k<-4
      thismod<-mods[k]
      thisrow<-tmpdf$term==thisterm & 
        tmpdf$mname==thismod
      #get estimate
      estdf<-tmpdf[thisrow,]
      #use gimmie.est to get the display
      tmp<-apply(estdf[,c("mu","pval","se")],1,function(x)
        gimmie.est(x[1],x[2],x[3],nrow=2))
      tmp<-matrix(tmp,ncol=1)
      return(tmp)
    })
    #adjust all these rows to be equal
    maxrows<-max(sapply(ivrows,nrow))
    ivrows<-lapply(ivrows,function(r) {
      if(nrow(r)<maxrows) {
        x<-rep("",maxrows-nrow(r))
        y<-matrix(x,ncol=1)
        r<-rbind(r,y)
      }
      return(r)
    })
    ivrows<-Reduce(cbind,ivrows)
    ivrows<-data.frame(ivrows,stringsAsFactors=F)
    names(ivrows)<-mods
    ivrows$type<-c("est","se")
    ivrows$term<-thisterm
    
    return(ivrows)
  }) %>% rbind.fill
  
  ###get good names and order the ests
  
  #goodnames will be propername
  #plus suffix indicating lag and within/between
  
  #order should be
  #dv
  #betweens
  #ivorder
  #withins
  #ivorder
  
  #get type of var
  srests$type<-sapply(
    srests$term,
    getvarcode,F,
    "varname",
    "type",
    varsdf
  )
  srests$isdv<-srests$type=="demdv"
  #get the base iv name
  srests$iv.base<-sapply(
    srests$term,
    getvarcode,
    F,"varname",
    "varname",
    varsdf 
  )
  srests$propername<-sapply(
    srests$iv.base,
    getvarcode,F,
    "varname",
    "propername",
    varsdf
  )
  srests$order<-sapply(
    srests$term,
    getvarcode,F,
    "varname",
    "order",
    varsdf
  )
  srests$lag.numbers<-sapply(srests$term,getlag)
  #srests$diff.numbers<-sapply(srests$term,getdiff)
  
  ###make labels
  srests$prefix<-""
  srests$suffix<-sapply(srests$lag.numbers,function(x) {
    if(is.na(x)) {
      y<-""
    } else {
      y<-paste0("$_{t-",x,"}$")
    }
  })
  srests$label<-paste0(
    srests$prefix,
    srests$propername,
    srests$suffix
  )
  
  ###set the order
  termorder<-order(
    srests$order,
    srests$lag.numbers
  )
  srests<-srests[termorder,]
  
  ###finalize
  #make label sparse
  srests$label[1:nrow(srests)%%2!=1]<-""
  #return lagdvs and ivs separately
  splitter<-which(diff(srests$isdv)!=0)
  tmpvars<-c("label",mods)
  srests.ldvs<-srests[1:splitter,c("label",mods)]
  tmprows<-c(1:nrow(srests))%in%c(splitter+1):nrow(srests)
  srests.ivs<-srests[tmprows,tmpvars]
  
  #######################################
  #######################################
  
  ##GET LONG RUN
  print("LR")
  tmpdf<-ssdf[ssdf$type=="longrun",]
  
  #adjust, if necessary
  # #a quick tweak will make interactions/squared easier
  # tmp<-!is.na(tmpdf$atvals)
  # tmpdf$iv[tmp]<-paste0(
  #   tmpdf$iv[tmp],
  #   ".",
  #   tmpdf$atvals.with[tmp],
  #   "=",
  #   tmpdf$atvals[tmp]
  # )
  
  #ivs, here
  ivs.root<-tmpdf$iv %>% 
    unique
  tmpseq.j<-seq_along(ivs.root)
  lrests<-lapply(tmpseq.j,function(j) {
    #j<-5
    thisiv<-ivs.root[j]
    #tmpdf[tmpdf$iv.root==thisiv,]
    #print(thisiv)
    #get each estimate
    tmpseq.k<-seq_along(mods)
    ivrows<-lapply(tmpseq.k,function(k) {
      #k<-1
      #print(j)
      #print(k)
      thismod<-mods[k]
      thisrow<-tmpdf$iv==thisiv & 
        tmpdf$mname==thismod
      if(sum(thisrow)>1)
        stop()
      #get estimate
      estdf<-tmpdf[thisrow,]
      #use gimmie.est to get the display
      tmp<-gimmie.est2(
        mu=estdf$mu,
        pval.class=estdf$pval.class,
        mu.min=estdf$mu.min,
        mu.max=estdf$mu.max,
        nrow=2
      )
      tmp<-matrix(tmp,ncol=1)
      return(tmp)
    }) 
    #print(thisiv)
    #adjust all these rows to be equal
    maxrows<-max(sapply(ivrows,nrow))
    ivrows<-lapply(ivrows,function(r) {
      if(nrow(r)<maxrows) {
        x<-rep("",maxrows-nrow(r))
        y<-matrix(x,ncol=1)
        r<-rbind(r,y)
      }
      return(r)
    })
    #print(thisiv)
    ivrows<-Reduce(cbind,ivrows)
    ivrows<-data.frame(ivrows,stringsAsFactors=F)
    names(ivrows)<-mods
    #add some identifying information
    ivrows$type<-c("est","se")
    ivrows$iv.root<-thisiv
    return(ivrows)
  }) %>% rbind.fill
  
  ###get label
  #get the base iv name
  lrests$iv.base<-sapply(
    lrests$iv,
    getvarcode,F,
    "varname",
    "varname",
    varsdf
  )
  lrests$propername<-sapply(
    lrests$iv.base,
    getvarcode,F,
    "varname",
    "propername",
    varsdf
  )
  lrests$order<-sapply(
    lrests$iv.base,
    getvarcode,F,
    "varname",
    "order",
    varsdf
  )
  
  #this is the label
  lrests$prefix<-""
  lrests$label<-paste0(
    lrests$prefix,
    lrests$propername
  )
  
  ###set the order
  termorder<-order(
    lrests$order
  )
  lrests<-lrests[termorder,]
  
  #order cols
  colorder<-mods
  #make sparse
  lrests$label[1:nrow(lrests)%%2!=1]<-""
  
  #split
  tmpvars<-c("label",mods)
  tmprows<-1:nrow(lrests)
  lrests.overall<-lrests[tmprows,tmpvars]
  
  #######################################
  #######################################
  
  ###GET MOD INFO
  tmpseq.j<-seq_along(mods)
  modelinfo<-lapply(tmpseq.j,function(j) {
    #j<-1
    #print(j)
    thismod<-mods[j]
    thismod.row<-modsdf$mname==thismod
    thisdv<-modsdf$dv[thismod.row]
    thisdf<-modsdf$df[thismod.row]
    thisivspec<-modsdf$ivspec[thismod.row]
    tmp<-sampsdf$dv==thisdv & 
      sampsdf$df==thisdf &
      sampsdf$ivspec==thisivspec
    fulldf<-samps[[sampsdf$sampname[tmp]]]
    #info
    N.obs<-format(nrow(fulldf),big.mark=",",scientific=F)
    N.states<-fulldf$cowcode.num %>% unique %>% length
    range<-paste0(range(unique(fulldf$year)),collapse="-")
    avg.N<-tapply(fulldf$year,fulldf$cowcode.num,length)
    avg.N<-round(mean(avg.N),1)
    #industry-level RE? 
    cowfe<-"FE"
    yearfe<-"FE"
    #model stats
    thismod.df<-finaldf[finaldf$mname==thismod,]
    adjr2<-thismod.df$adjr2 %>% unique
    adjr2<-sprintf("%.3f",adjr2)
    bic<-thismod.df$bic %>% unique
    bic<-sprintf("%.2f",bic)
    thiscol<-data.frame(
      c(
        N.obs,
        N.states,
        range,
        avg.N,
        cowfe,
        yearfe,
        adjr2#,
        #bic
      ),
      stringsAsFactors = F
    )
    names(thiscol)<-thismod
    return(thiscol)
  }) 
  modelinfo<-Reduce(cbind,modelinfo)
  modelinfo$label<-c(
    "Observations",
    "Countries",
    "Range",
    "Avg. $N_{i}$",
    "Country-Level",
    "Year-Level",
    "Adj. $R^{2}$"#
    #"BIC"
  )
  
  #######################################
  #######################################
  
  #PUT ALL TOGETHER
  
  if(is.ecm) {
    ldvfiller<-data.frame(label=c("","\\textit{Error Correction Rate}",""))
  } else {
    ldvfiller<-data.frame(label=c("","\\textit{Lagged Dep. Var}",""))
  }
  srfiller<-data.frame(label=c("","\\textit{Short-Run Impact}",""))
  lrfiller<-data.frame(label=c("","\\textit{Long-Run Multiplier}",""))
  infofiller<-data.frame(label=c("","\\textit{Model Info}",""))
  
  regtable<-rbind.fill(
    ldvfiller,
    srests.ldvs,
    srfiller,
    srests.ivs,
    lrfiller,
    lrests.overall,
    infofiller,
    modelinfo
  )
  #make NA's blank
  regtable<-apply(regtable,2,function(x) {
    x[is.na(x)]<-""
    return(x)
  })
  #write out each of these
  setwd(outputdir)
  filename<-paste0(
    "tab_reg.",
    thistabname,
    ".csv"
  )
  write.csv(
    regtable,
    filename,
    row.names=F
  )
  
}





