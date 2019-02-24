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
require(data.table)
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
source('getlongrun2.R')
source('predict2.R')

#########################################################
#########################################################

#load mods
setwd(filesdir); dir()
load('mainmods.RData')

#load data
setwd(datadir); dir()
demdfs<-readRDS(
  "demdfs.RDS"
)

#########################################################
#########################################################

#unit root tests

#set params

#vars
tmpvars<-mods$polity2.one.preferred$formula %>%
  all.vars
tmpvars2<-mods$v2x_polyarchy.one.preferred$formula %>%
  all.vars
vars<-c(
  tmpvars,
  tmpvars2
) %>% unique

#omit some
badvars<-c(
  "year",
  "L.polity2",
  "L.v2x_polyarchy"
)
tmp<-!vars%in%badvars
vars<-vars[tmp]

#misc parameters
maxlags<-3

#########################################################
#########################################################

#BALANCED TEST

#do the test at different thresholds

loopdf<-expand.grid(
  var=vars,
  min.N=seq(10,100,10),
  stringsAsFactors=F
)
loopdf$i<-1:nrow(loopdf)

#put together a balanced dataset for each variable
#i will want at least N.countries, and so I pick the
#T for each variable that gives me this many N.countires
#in a balanced dataset

tmpseq.i<-1:nrow(loopdf)
baldf<-lapply(tmpseq.i,function(i) {


  #i<-85
  print(
    paste(
      i,"of",length(tmpseq.i)
    )
  )

  ####

  thisvar<-loopdf$var[i]
  thisN<-loopdf$min.N[i]

  ###

  #get countries desired, given N countries
  thisdf<-demdfs$one
  tmpcows<-tapply(
    thisdf[[thisvar]],
    thisdf$cowcode.num,
    function(x) sum(!is.na(x))
  ) %>% sort
  mycows<-tail(
    tmpcows,
    thisN
  )
  thesecows<-names(mycows)
  minN<-mycows[1]

  ###

  #create a tmpdf w/ this many obs,
  #using thesecows
  tmpvars<-c("cowcode.num","year",thisvar)
  tmprows<-thisdf$cowcode.num%in%thesecows
  tmpdf<-thisdf[tmprows,tmpvars]
  testdf<-by(tmpdf,tmpdf$cowcode.num,function(df) {
    #df<-tmpdf[tmpdf$cowcode.num==2,]
    df<-df[!is.na(df[[thisvar]]),]
    data.frame(
      thisvar=tail(df[[thisvar]],minN),
      time=1:minN,
      cowcode.num=unique(df$cowcode.num),
      stringsAsFactors=F
    )
  }) %>% rbind.fill

  ###

  #make this wide rather than long
  is.na(testdf$thisvar) %>% sum
  is.nan(testdf$thisvar) %>% sum
  sum(!is.finite(testdf$thisvar))

  #here's my dataframe
  testdf<-spread(
    testdf,
    cowcode.num,
    thisvar
  )

  #but remove inel cols (this which don't change)
  badcols<-apply(
    testdf,2,
    function(x) length(unique(x))==1
  )
  testdf<-testdf[,!badcols]

  #and badcols, which are countries in which there isn't sufficient variation
  #I don't know how to fix purtest error w/o throwing out these countries
  #and I don't have chops to peer under the hood. fortunately, not many countries
  badcols<-names(testdf)%in%c(
    "20",
    "920",
    "390",
    "305",
    "375"
  )
  testdf<-testdf[,!badcols]

  ###

  #run the tests,get restuls
  tests<-c(
    "levinlin",
    "ips",
    "madwu",
    "hadri"
  )
  returndf<-lapply(tests,function(mytest) {
    #mytest<-"levinlin"
    #print(mytest)
    tmptest<-purtest(
      testdf,
      test=mytest,
      exo='intercept',
      lags='AIC',
      pmax=maxlags
    )
    returndf<-data.frame(
      #test=tmptest$statistic$method,
      test=mytest,
      pval=tmptest$statistic$p.value
    )
    if(mytest=="hadri") {
      returndf$unitroot<-ifelse(
        returndf$pval>0.05,"No","Yes"
      )
    } else {
      returndf$unitroot<-ifelse(
        returndf$pval<0.05,"No","Yes"
      )
    }
    returndf
  }) %>% rbind.fill

  ###

  returndf$i<-i
  returndf

}) %>% rbind.fill


#merge loopdf and returndf
intersect(
  names(loopdf),
  names(baldf)
)

baldf<-merge(
  loopdf,
  baldf,
  by="i",
  all=T
)

########################################################
########################################################

#UNBALANCED TESTS

loopdf<-expand.grid(
  var=vars,
  min.T=seq(20,100,10),
  stringsAsFactors=F
)
loopdf$i<-1:nrow(loopdf)

tmpseq.i<-1:nrow(loopdf)
ubaldf<-lapply(tmpseq.i,function(i) {
  
  #i<-5
  
  #i<-85
  print(
    paste(
      i,"of",length(tmpseq.i)
    )
  )
  
  ####
  
  thisvar<-loopdf$var[i]
  thisT<-loopdf$min.T[i]
  
  ####
  
  #get countries which have at least 
  #this many observations
  thisdf<-demdfs$one
  tmpcows<-tapply(
    thisdf[[thisvar]],
    thisdf$cowcode.num,
    function(x) sum(!is.na(x))
  ) %>% sort
  mycows<-tmpcows[tmpcows>=thisT]
  thesecows<-names(mycows)

  ###
  
  tmpvars<-c("cowcode.num","year",thisvar)
  tmprows<-thisdf$cowcode.num%in%thesecows &
    !is.na(thisdf[[thisvar]])
  testdf<-thisdf[tmprows,tmpvars]
  
  ####
  
  #now, split this by cowcode.num
  testdfs<-split(testdf[[thisvar]],testdf$cowcode.num)
  #remove countries that don't change at all
  testdfs<-testdfs[!sapply(testdfs,function(x) length(unique(x))==1)]
  #now run the fisher-type purtest on the unbalanced data
  output<-purtest_fisher(testdfs)
  #return information based on this test
  returndf<-data.frame(
    test="fishertype",
    pval=output$logtest,
    stringsAsFactors=F
  )
  returndf$unitroot<-ifelse(
    returndf$pval<0.05,"No","Yes"
  )
  
  ###
  
  returndf$i<-i
  returndf
}) %>% rbind.fill

#merge loopdf and returndf
intersect(
  names(loopdf),
  names(ubaldf)
)

ubaldf<-merge(
  loopdf,
  ubaldf,
  by="i",
  all=T
)

########################################################
########################################################

#save out
setwd(filesdir); dir()
save.image(
  'unitroots.RData'
)

