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
# gsdir<-file.path(
#   "c:",
#   "Program Files",
#   "gs"
# )
# gsdir_full<-file.path(
#   gsdir,
#   dir(gsdir),
#   "bin",
#   "gswin64c.exe"
# )
Sys.setenv(
  R_GSCMD = gsdir_full
)
#initialize graphlist
gs.list<-list()

#########################################################
#########################################################

#CAPACITY INDICATORS

#load
setwd(datadir); dir()
demdfs<-readRDS(
  "demdfs.RDS"
)

demdf<-demdfs$one
laborvars<-c(
  "highcapratio",
  "laborrightspos",
  "plp",
  "manu_valadd",
  "union.density",
  "union.ratio",
  "union.pcap",
  "volume",
  "volume.ratio",
  "volume.pcap",
  "frequency",
  "frequency.ratio",
  "frequency.pcap"
)

labordf<-lapply(laborvars,function(var) {
  #var<-"highcapratio"
  print(var)
  tmp<-!is.na(demdf[[var]])
  N<-sum(tmp)
  N.polity2<-sum(tmp & !is.na(demdf$polity2))
  avgpolity2<-mean(demdf$polity2[tmp],na.rm=T)
  polity2diff<-avgpolity2 - 
    mean(demdf$polity2,na.rm=T)
  N.countries<-tapply(
    demdf[[var]],
    demdf$cowcode.num,
    function(x) 
      sum(!is.na(x))>0
  ) %>% sum
  years<-range(
    demdf$year[tmp]
  ) %>% paste0(collapse="-")
  data.frame(
    var,
    N,
    N.polity2,
    avgpolity2,
    polity2diff,
    N.countries,
    years
  )
}) %>% rbind.fill

#fix varnames
labordf$varname<-sapply(
  labordf$var,
  getvarname
)

#get roworder
roworder<-order(
  -labordf$N.polity2
)

#prettify    
labordf$N<-formatC(
  labordf$N,
  format='f',
  digits=0,
  big.mark=","
)
labordf$N.polity2<-formatC(
  labordf$N.polity2,
  format='f',
  digits=0,
  big.mark=","
)
labordf$polity2diff<-round(
  labordf$polity2diff,0
) %>% format(nsmall=0)
labordf$polity2diff<-paste0(
  "+",
  labordf$polity2diff
)

#rowcolorder
colorder<-c(
  "varname",
  "N",
  "N.polity2",
  "polity2diff",
  "N.countries",
  "years"
)
names(labordf)
labordf<-labordf[roworder,colorder]

setwd(outputdir)
write.csv(
  labordf,
  "tab_labor.csv",
  row.names=F
)

#########################################################
#########################################################

#DESCRIPTIVE STATISTICS
#take all vars used/mentioned
#generate simple descritpive stats table

#load
setwd(filesdir); dir()
load(
  "mainmods.RData"
)

#for vars in the main regressions
prefmodnames<-c(
  "polity2.one.preferred",
  "v2x_polyarchy.one.preferred"
)
prefmods<-mods[prefmodnames]
sumvars<-lapply(prefmods,function(x) all.vars(x$formula)) %>%
  unlist %>% unique

#remove lagged dvs, regavg dvs, non-main dvs
tmp<-sapply(sumvars,getvartype)=="demdv"
tmp<-tmp & str_detect(sumvars,"^L\\.")
tmp<-tmp | str_detect(sumvars,"regavg")
sumvars<-sumvars[!tmp]

#remove id vars
idvars<-c(
  "year"
)
sumvars<-sumvars[!sumvars%in%idvars]

tmpdf<-lapply(sumvars,function(v) {
  #v<-"netmigrate"
  #get nice name
  varname<-getvarname(v)
  #from polity2 sample, unless polyarchy var
  if(v=="v2x_polyarchy") {
    mysampname<-"v2x_polyarchy.one.preferred"
  } else {
    mysampname<-"polity2.one.preferred"
  }
  tmpdf<-samps[[mysampname]]
  #get within-sample mean, within-country SD  
  avg<-mean(tmpdf[[v]],na.rm=T) %>%
    round(2) %>%
    format(nsmall=2)
  sd.all<-sd(tmpdf[[v]],na.rm=T) %>%
    round(2) %>%
    format(nsmall=2)
  sd.within<-tapply(tmpdf[[v]],tmpdf$cowcode.num,sd,na.rm=T) %>% 
    mean(na.rm=T) %>%
    round(2) %>%
    format(nsmall=2)
  
  #bigdf$countryname[tmp] %>% unique
  #return
  data.frame(
    v,
    varname,
    avg,
    sd.all,
    sd.within,
    stringsAsFactors=F
  )
}) %>% rbind.fill
tmpdf

#add some hepful info from varnames
mergevars<-c("propername","order","type")
mergedf<-varsdf[,mergevars]
names(mergedf)[names(mergedf)=="propername"]<-"varname"
datadf<-merge(tmpdf,mergedf)

#row order,colorder
roworder<-order(
  datadf$order
)
colorder<-c(
  #these make no sense in same table as reg stats
  "varname",
  "type",
  "avg",
  "sd.all",
  "sd.within"
)
datadf<-datadf[roworder,colorder]
datadf

#save out
setwd(outputdir)
write.csv(
  datadf,
  "tab_descriptive.csv",
  row.names=F
)

#########################################################
#########################################################

#SAMPLE TILE PLOTS

#POLITY/POLYARCHY
#social forces and pref smaple
tmp<-sampsdf$df=="one" &
  sampsdf$ivspec%in%c("preferred","socialforces")
keysamps<-sampsdf$sampname[tmp]

#fill in all years, all cows
#across both dfs into both dfs
bothdfs<-samps[keysamps]
allcows<-sapply(
  bothdfs,
  function(df) 
    df$cowcode.num
) %>% unlist %>% unique
allyears<-sapply(
  bothdfs,
  function(df) 
    df$year
) %>% unlist
allyears<-min(allyears):
  max(allyears)

#extradf
extradf<-expand.grid(
  cowcode.num=allcows,
  year=allyears,
  insamp=F,
  src=2,
  stringsAsFactors=F
)

#loop through
tmpseq.i<-seq_along(keysamps)
plotdf<-lapply(tmpseq.i,function(i) {
  #i<-1
  mysamp<-keysamps[i]
  tmpdf<-samps[[mysamp]]
  keepvars<-c(
    "cowcode.num",
    "year"
  )
  tmpdf<-tmpdf[,keepvars]
  tmpdf$insamp<-T
  tmpdf$src<-1
  #add extra years
  tmpdf<-rbind.fill(
    tmpdf,
    extradf
  )
  roworder<-order(
    tmpdf$cowcode.num,
    tmpdf$year,
    tmpdf$src
  )
  tmpdf<-tmpdf[roworder,]
  #drop dups
  tmpdf$id<-paste0(
    tmpdf$cowcode.num,
    "-",
    tmpdf$year
  )
  tmpdf$N<-tapply(
    1:nrow(tmpdf),
    tmpdf$id,
    function(x) 
      1:length(x)
  ) %>% unlist
  tmp<-tmpdf$N==1
  tmpdf<-tmpdf[tmp,]
  if(nrow(tmpdf)!=nrow(extradf))
    stop()
  #return
  tmpdf$src<-tmpdf$id<-tmpdf$N<-NULL
  tmpdf$samp<-mysamp
  #return other invo
  tmp<-sampsdf$sampname==mysamp  
  tmpdf$dv<-sampsdf$dv[tmp]
  tmpdf$ivspec<-sampsdf$ivspec[tmp]
  tmpdf
}) %>% rbind.fill

plotdf$samp<-NULL
plotdf<-spread(
  plotdf,
  ivspec,
  insamp
)
plotdf$insamp<-"none"
tmp<-plotdf$socialforces &
  !plotdf$preferred
plotdf$insamp[tmp]<-"socialforces"
tmp<-plotdf$preferred
plotdf$insamp[tmp]<-"preferred"

plotdf$insamp<-factor(
  plotdf$insamp,
  levels=c(
    "none",
    "socialforces",
    "preferred"
  ),
  labels=c(
    "Out-of-Sample",
    "Social Forces",
    "Preferred"
  )
)
brewer.pal.info
tmpcolors<-brewer.pal(3,'Greys')
names(tmpcolors)<-levels(plotdf$insamp)

#fix samp
plotdf$dv<-factor(
  plotdf$dv,
  levels=c(
    "polity2",
    "v2x_polyarchy"
  ),
  labels=c(
    "Polity2",
    "Electoral Democracy"
  )
)

#fix country
tmplevels<-plotdf$cowcode.num %>%
  unique %>% sort %>% rev 
tmp<-str_extract(tmplevels,"[0-9]+") %>%
  as.numeric
tmplevels<-tmplevels[order(tmp)]
tmplabels<-sapply(
  tmplevels,
  getcountryname
)
plotdf$countryname<-factor(
  plotdf$cowcode.num,
  levels=tmplevels,
  labels=tmplabels
)

#graph
g.tmp<-ggplot(
  plotdf,
  aes(
    x=year,
    y=countryname,
    fill=insamp
  )
) + 
  geom_tile() +
  scale_fill_manual(
    name="",
    values=tmpcolors
  ) +
  facet_wrap(
    ~ dv
  ) +
  xlab("") +
  ylab("") + 
  theme_bw(
    base_family="CM Roman",
    base_size=14
  )
g.tmp

tmpname<-"fig_sample.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=9,
  height=14
)





#########################################################
#########################################################

# #DCAP MODS
# 
# setwd(filesdir); dir()
# load(
#   'dcapmods.RData'
# )
# 
# #three main samples
# tmp<-sampsdf$df=="one" & 
#   str_detect(
#     sampsdf$dv,
#     "ratio"
#   )
# keysamps<-sampsdf$sampname[tmp]
# 
# #fill in all years, all cows
# #across both dfs into both dfs
# bothdfs<-samps[keysamps]
# allcows<-sapply(
#   bothdfs,
#   function(df) 
#     df$cowcode.num
# ) %>% unlist %>% unique
# allyears<-sapply(
#   bothdfs,
#   function(df) 
#     df$year
# ) %>% unlist
# allyears<-min(allyears):
#   max(allyears)
# 
# #extradf
# extradf<-expand.grid(
#   cowcode.num=allcows,
#   year=allyears,
#   insamp=F,
#   src=2,
#   stringsAsFactors=F
# )
# 
# #loop through
# tmpseq.i<-seq_along(keysamps)
# plotdf<-lapply(tmpseq.i,function(i) {
#   #i<-1
#   mysamp<-keysamps[i]
#   tmpdf<-samps[[mysamp]]
#   keepvars<-c(
#     "cowcode.num",
#     "year"
#   )
#   tmpdf<-tmpdf[,keepvars]
#   tmpdf$insamp<-T
#   tmpdf$src<-1
#   #add extra years
#   tmpdf<-rbind.fill(
#     tmpdf,
#     extradf
#   )
#   roworder<-order(
#     tmpdf$cowcode.num,
#     tmpdf$year,
#     tmpdf$src
#   )
#   tmpdf<-tmpdf[roworder,]
#   #drop dups
#   tmpdf$id<-paste0(
#     tmpdf$cowcode.num,
#     "-",
#     tmpdf$year
#   )
#   tmpdf$N<-tapply(
#     1:nrow(tmpdf),
#     tmpdf$id,
#     function(x) 
#       1:length(x)
#   ) %>% unlist
#   tmp<-tmpdf$N==1
#   tmpdf<-tmpdf[tmp,]
#   if(nrow(tmpdf)!=nrow(extradf))
#     stop()
#   #return
#   tmpdf$src<-tmpdf$id<-tmpdf$N<-NULL
#   tmpdf$samp<-mysamp
#   tmpdf
# }) %>% rbind.fill
# 
# 
# #fix tile
# tmpcolors<-brewer.pal(3,'Greys')[c(1,3)]
# names(tmpcolors)<-levels(plotdf$insamp)
# 
# #fix samp
# plotdf$samp<-factor(
#   plotdf$samp,
#   levels=c(
#     "union.ratio.one",
#     "frequency.ratio.one",
#     "volume.ratio.one"
#   ),
#   labels=c(
#     "Union Membership",
#     "Strike Frequency",
#     "Strike Volume"
#   )
# )
# 
# #fix country
# tmplevels<-plotdf$cowcode.num %>%
#   unique %>% sort %>% rev 
# tmp<-str_extract(tmplevels,"[0-9]+") %>%
#   as.numeric
# tmplevels<-tmplevels[order(tmp)]
# tmplabels<-sapply(
#   tmplevels,
#   getcountryname
# )
# plotdf$countryname<-factor(
#   plotdf$cowcode.num,
#   levels=tmplevels,
#   labels=tmplabels
# )
# 
# #graph
# g.tmp<-ggplot(
#   plotdf,
#   aes(
#     x=year,
#     y=countryname,
#     fill=insamp
#   )
# ) + 
#   geom_tile() +
#   scale_fill_manual(
#     name="",
#     values=tmpcolors
#   ) +
#   facet_wrap(
#     ~ samp
#   ) +
#   xlab("") +
#   ylab("") + 
#   theme_bw(
#     base_family="CM Roman",
#     base_size=12
#   )
# g.tmp
# 
# tmpname<-"fig_dcapsample.pdf"
# gs.list[[tmpname]]<-list(
#   graph=g.tmp,
#   filename=tmpname,
#   width=8,
#   height=12
# )

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
}


