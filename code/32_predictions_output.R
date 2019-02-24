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

#load cfactuals output
setwd(filesdir); dir()
load('cfactuals_stats.RData')
setwd(datadir); dir()
demdfs<-readRDS('demdfs.RDS')

#########################################################
#########################################################

#plotting prelims
require(ggplot2)
require(ggthemes)
require(extrafont)
require(RColorBrewer)
require(scales)
require(data.table)
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

#WRITEUP STATS

#for brazil, we want gain
#expressed in pct,sd
gaindf[
  cow==140 & 
    betatype=="fixed" & 
    stat=="diffraw" &
    dv=="polity2"
  ]

#comparison to predicted/observed, does it matter? 
# gaindf$diff.max<-gaindf$diff.min<-NULL
# tmpdf<-spread(
#   gaindf,
#   comparison,
#   diff
# )
# tmpdf<-data.table(tmpdf)
# tmpdf[
#   ,
#   cor(observed,predicted)
#   ,
#   by=c(
#     "dv","stat","cfactual"
#   )
#   ] 
# #highgly correlated in the key cfactuals
# #in gdp, not so much.. 

#for devcow, we want gain
#as pct of gap to advanced world
#under a couple of permutations..
loopdf<-expand.grid(
  cows=c("loess","avg"),
  baseline=c("predicted","observed"),
  stringsAsFactors=F
)
loopdf$seq<-1:nrow(loopdf)
tmpseq.i<-1:nrow(loopdf)
gapdf<-lapply(tmpseq.i,function(i) {
  #i<-3
  print(i)
  devcow<-paste0("developing.",loopdf$cows[i])
  advcow<-paste0("advanced.",loopdf$cows[i])
  tmp<-predictdf$cow==devcow &
    predictdf$cfactual%in%c(
      loopdf$baseline[i],
      "gini",
      "modernization",
      "socialforces",
      "full"
    )
  tmp<-tmp | (
    predictdf$cow==advcow &
      predictdf$cfactual==loopdf$baseline[i]
  )
  tmpdf<-predictdf[tmp,]
  tmpdf$group<-paste0(
    tmpdf$cow,
    "_",
    tmpdf$cfactual
  )
  tmpdf$cow<-tmpdf$cfactual<-NULL
  tmpdf<-tmpdf[
    ,
    .(
      yhat=getarea(
        year,
        yhat
      )/100
    )
    ,
    by=c("group","dv","rep","betatype")
    ]
  tmpdf<-spread(
    tmpdf,
    group,
    yhat
  )
  
  #put the predicted columns first
  idcols<-c(
    "dv",
    "rep",
    "betatype"
  )
  tmp<-str_detect(
    names(tmpdf),
    "predicted|observed"
  ) | names(tmpdf)%in%idcols
  idcols<-names(tmpdf)[tmp]
  othcols<-names(tmpdf)[!tmp]
  tmpdf<-tmpdf[,c(idcols,othcols),with=F]
  #tweak names
  names(tmpdf)[4:5]<-c("adv_p","dev_p")
  names(tmpdf)<-str_replace(
    names(tmpdf),
    "developing\\.loess\\_",
    ""
  )
  #now, make wide
  idcols<-names(tmpdf)[1:5]
  gathcols<-names(tmpdf)[!names(tmpdf)%in%idcols]
  tmpdf<-gather_(
    tmpdf,
    "cfactual",
    "yhat",
    gathcols
  ) %>% as.data.table
  tmpf<-function(advp,devp,yhat) {
    gap<-advp -
      devp
    closed<-yhat -
      devp
    100 * closed/gap
  }
  returndf<-tmpdf[
    ,
    .(
      gapclosed=tmpf(adv_p,dev_p,yhat)
    )
    ,
    by=c(
      "dv",
      "rep",
      "betatype",
      "cfactual"
    )
    ]
  returndf$seq<-i
  returndf
}) %>% rbind.fill

gapdf<-merge(
  gapdf,
  loopdf
)

#summarize
gapdf<-data.table(gapdf)
gapdf<-gapdf[
  ,
  .(
    gapclosed=mean(gapclosed),
    gapclosed.max=quantile(gapclosed,0.975),
    gapclosed.min=quantile(gapclosed,0.025)
  )
  ,
  by=c(
    "dv",
    "betatype",
    "cows",
    "cfactual",
    "baseline"
  )
  ]
gapdf

tmp<-gapdf$cfactual=="socialforces"
gapdf[tmp,]
#OTHER STATS? 

#compare diff between advaned and developing in observed
loopdf<-expand.grid(
  dv=unique(meansdf$dv),
  baseline=c("observed","predicted"),
  stringsAsFactors=F
)
tmpseq.i<-1:nrow(loopdf)
loopdf$gap<-sapply(tmpseq.i,function(i) {
  #x<-c("observed")
  y1<-meansdf[
    meansdf$cow=="advanced.loess" &
      meansdf$dv==loopdf$dv[i] &
      meansdf$cfactual==loopdf$baseline[i]
    ,
    yhat
    ]
  y2<-meansdf[
    meansdf$cow=="developing.loess" &
      meansdf$dv==loopdf$dv[i] &
      meansdf$cfactual==loopdf$baseline[i]
    ,
    yhat
    ]
  abs(y1-y2)
})

#########################################################
#########################################################

#BRAZIL 

#get summary gains
gaindf[
  cow==140 & 
    betatype=="fixed" & 
    comparison=="predicted" & 
    cfactual=="modernization"
  ]

gaindf[
  cow==140 & 
    betatype=="fixed" & 
    comparison=="predicted" & 
    cfactual=="socialforces"
  ]

#how much due to land, how much to dcap

dcap<-gaindf[
  cow==140 & 
    betatype=="fixed" & 
    comparison=="predicted" & 
    cfactual=="dcapyear" &
    dv=="polity2" &
    stat=="diffraw"
  ,
  diff
  ]
land<-gaindf[
  cow==140 & 
    betatype=="fixed" & 
    comparison=="predicted" & 
    cfactual=="landyear" &
    dv=="polity2" &
    stat=="diffraw"
  ,
  diff
  ]

round(100 * dcap/(dcap+land))
round(100 * land/(land+dcap))

#subset
tmp<-predictdf$cow==140 &
  predictdf$cfactual%in%c(
    "predicted",
    "socialforces"#,
    #"modernization",
    #"gini"
  ) &
  predictdf$dv=="polity2" &
  predictdf$betatype=="fixed"
plotdf<-predictdf[tmp,] %>%
  as.data.frame

#for shading of main plot
filldf<-data.frame(
  year=unique(plotdf$year),
  ymin=plotdf$yhat[plotdf$cfactual=="predicted"],
  ymax=plotdf$yhat[plotdf$cfactual=="socialforces"],
  stringsAsFactors=F
)

#for extra plots
#hcap,gdp,land
tmpcols<-c(
  "year",
  "cfactual",
  "L.highcapratio",
  #"L.lngdpcap",
  "L.landlords"
)
tmprows<-plotdf$cfactual%in%c("predicted","socialforces")
extradf<-plotdf[tmprows,tmpcols]
extradf<-gather(
  extradf,
  var,
  val,
  L.landlords:L.highcapratio
)
# #gini
# tmpcols<-c("year","cfactual","L.gini.generous")
# tmprows<-plotdf$cfactual%in%c("predicted","gini")
# extradf2<-plotdf[tmprows,tmpcols]
# extradf2<-gather(
#   extradf2,
#   var,
#   val,
#   L.gini.generous
# )
# #put together
# extradf<-rbind.fill(
#   extradf,
#   extradf2
# )

#finalize
tmp<-extradf$cfactual!="predicted"
extradf$cfactual[tmp]<-"cfactual"

#fix levels
tmplevels<-c(
  "socialforces",
  "predicted",
  "modernization",
  "gini"
)
tmplabels<-c(  
  "DCAP, LAND",
  "As Observed",
  "GDP, ED, URB",
  "GINI"
)
plotdf$cfactual<-factor(
  plotdf$cfactual,
  tmplevels,
  tmplabels
)

#main plot
g.main<-ggplot(
  data=plotdf
) + 
  geom_line(
    aes(
      x=year,
      y=yhat,
      group=cfactual,
      linetype=cfactual
    )
  ) +
  scale_linetype_discrete(
    name=""
  ) +
  geom_ribbon(
    data=filldf,
    aes(
      x=year,
      ymin=ymin,
      ymax=ymax
    ),
    fill='grey',
    alpha=0.1
  ) +
  xlab("") + 
  ylab("") +
  theme_bw(
    base_family="CM Roman",
    base_size=14
  )

#fix levels
tmplevels<-c(
  "cfactual",
  "predicted"
)
extradf$cfactual<-factor(
  extradf$cfactual,
  tmplevels,
  tmplevels
)
tmp<-extradf$var %>% unique
tmplevels<-sapply(tmp,getvarorder) %>%
  sort %>% names
tmplabels<-sapply(tmplevels,getvarname)
extradf$var<-factor(
  extradf$var,
  tmplevels,
  tmplabels
)

#covariate plots
g.extra<-ggplot(
  extradf,
  aes(
    x=year,
    y=val,
    linetype=cfactual
  )
) +
  geom_line() +
  facet_wrap(
    ~ var,
    scales="free"
  ) +
  scale_linetype(
    guide=F
  ) +
  xlab("") +
  ylab("") +
  theme_bw(
    base_family="CM Roman",
    base_size=14
  )


#combine
library(gridExtra)
tmplay <- rbind(
  c(1,1,1,1,1,1,1,1),
  c(2,2,2,2,2,2,2,NA)
)
g.tmp<-grid.arrange(
  g.main,
  g.extra,
  layout_matrix=tmplay,
  heights=c(2,1),
  ncol=1
)

tmpname<-"fig_brazilgains.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=11,
  height=8
)

#########################################################
#########################################################

#ALL, SIMPLE

#subset
tmp<-predictdf$cow=="developing.loess" &
  predictdf$cfactual%in%c(
    "predicted",
    "socialforces"
  )
tmp<-tmp | (
  predictdf$cow=="advanced.loess" &
    predictdf$cfactual=="predicted"
)
tmp<-tmp & predictdf$betatype=="fixed"
plotdf<-predictdf[tmp,] %>%
  as.data.frame
plotdf$group<-paste0(
  plotdf$cow,
  plotdf$cfactual
)

#fix levels
unique(plotdf$group)
tmplevels<-c(
  "advanced.loesspredicted",
  "developing.loesssocialforces",
  "developing.loesspredicted"
)
tmplabels<-c(
  "Advanced (As Observed)",
  "Developing (DCAP, LAND)",
  "Developing (As Observed)"
)
plotdf$group<-factor(
  plotdf$group,
  tmplevels,
  tmplabels
)

tmplevels<-sapply(
  unique(plotdf$dv),
  getvarorder
) %>% sort %>% names
tmplabels<-sapply(
  tmplevels,
  getvarname
)
plotdf$dv<-factor(
  plotdf$dv,
  tmplevels,
  tmplabels
)

#add filldf
tmpcols<-c(
  "year",
  "dv",
  "group",
  "yhat"
)
filldf<-plotdf[,tmpcols]
filldf<-spread(
  filldf,
  group,
  yhat
)
filldf$`Advanced (As Observed)` <- NULL
names(filldf)<-c(
  "year",
  "dv",
  "ymax",
  "ymin"
)

#plot
g.tmp<-ggplot(
  plotdf
) +
  geom_line(
    aes(
      x=year,
      y=yhat,
      group=group,
      linetype=group
    )
  ) +
  scale_linetype_discrete(
    name=""
  ) +
  geom_ribbon(
    data=filldf,
    aes(
      x=year,
      ymin=ymin,
      ymax=ymax
    ),
    fill='grey',
    alpha=0.1
  ) +
  xlab("") +
  ylab("") +
  facet_wrap(
    ~ dv,
    ncol=1
  ) +
  theme_bw(
    base_family="CM Roman",
    base_size=14
  )

tmpname<-"fig_devgains.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8,
  height=8
)


#########################################################
#########################################################

# #ALL, COMPLICATED

#get stats for wrietup
#gains
tmp<-gaindf$cow=="developing.loess" & 
  gaindf$cfactual=="socialforces" &
  gaindf$stat%in%c("diffraw","diffpct")
gaindf[tmp,]
#gap


#comparing each endogenous
#and also the full

#subset
tmp<-predictdf$cow=="developing.loess" &
  predictdf$cfactual%in%c(
    "predicted",
    "full",
    "modernization",
    "gini",
    "socialforces"
  )
tmp<-tmp | (
  predictdf$cow=="advanced.loess" &
    predictdf$cfactual=="predicted"
)
tmp<-tmp & predictdf$betatype=="fixed"
plotdf<-predictdf[tmp,] %>%
  as.data.frame
plotdf$group<-paste0(
  plotdf$cow,
  plotdf$cfactual
)
plotvars<-c(
  "year",
  "dv",
  "group",
  "yhat"
)
plotdf<-plotdf[,plotvars]

#reshape
#spread
plotdf<-spread(
  plotdf,
  group,
  yhat
)
idcols<-c(
  "year",
  "dv"
)
tmp<-str_detect(
  names(plotdf),
  "predicted"
)
idcols<-c(
  idcols,
  names(plotdf)[tmp]
)
gathcols<-names(plotdf)[!names(plotdf)%in%idcols]

plotdf<-gather_(
  plotdf,
  "group",
  "yhat",
  gathcols
)

#reorder cols
idcols<-c(
  "dv",
  "year",
  "group"
)
othcols<-c(
  "developing.loesspredicted",
  "yhat",
  "advanced.loesspredicted"
)
plotdf<-plotdf[,c(idcols,othcols)]
names(plotdf)<-c(
  "dv",
  "year",
  "facet",
  "dev",
  "cfactual",
  "adv"
)

plotdf<-gather(
  plotdf,
  group,
  yhat,
  dev:adv
)

#i need to generate filldfs
#one for each facet

tmplist<-list(
  plotdf$facet,
  plotdf$dv
)
filldf<-by(plotdf,tmplist,function(df) {
  #df<-plotdf[plotdf$facet=="developing.loessfull" & plotdf$dv=="polity2",]
  df<-spread(
    df,
    group,
    yhat
  )
  keepcols<-c(
    "dv",
    "facet",
    "year",
    "dev",
    "cfactual"
  )
  df<-df[,keepcols]
  names(df)[(ncol(df)-1):ncol(df)]<-c(
    "ymin","ymax"
  )
  df
}) %>% rbind.fill


#drop where any ymin/ymax are NA
tmp<-is.na(filldf$ymin) | is.na(filldf$ymax)
filldf<-filldf[!tmp,]
#drop any where ymin>ymax
tmp<-filldf$ymin>filldf$ymax 
filldf<-filldf[!tmp,]


#fix levels
unique(plotdf$facet)
tmplevels<-c(
  "developing.loessmodernization",
  "developing.loessgini",
  "developing.loesssocialforces",
  "developing.loessfull"
)
tmplabels<-c(
  "Modernization",
  "Inequality-Based",
  "Social Forces",
  "All Three"
)
plotdf$facet<-factor(
  plotdf$facet,
  tmplevels,
  tmplabels
)
filldf$facet<-factor(
  filldf$facet,
  tmplevels,
  tmplabels
)

tmplevels<-sapply(
  unique(plotdf$dv),
  getvarorder
) %>% sort %>% names
tmplabels<-sapply(
  tmplevels,
  getvarname
)
plotdf$dv<-factor(
  plotdf$dv,
  tmplevels,
  tmplabels
)
filldf$dv<-factor(
  filldf$dv,
  tmplevels,
  tmplabels
)

unique(plotdf$group)
tmplevels<-c(
  "adv",
  "cfactual",
  "dev"
)
tmplabels<-c(
  "Advanced",
  "Counterfactual",
  "Developing"
)
plotdf$group<-factor(
  plotdf$group,
  tmplevels,
  tmplabels
)

g.tmp<-ggplot(
  plotdf
) +
  geom_line(
    aes(
      x=year,
      y=yhat,
      group=group,
      linetype=group
    )
  ) +
  scale_linetype_discrete(
    name=""
  ) +
  scale_alpha_continuous(
    guide=F,
    range=c(0.25,1)
  ) +
  geom_ribbon(
    data=filldf,
    aes(
      x=year,
      ymin=ymin,
      ymax=ymax,
      fill=facet
    ),
    alpha=0.1
  ) +
  scale_fill_discrete(
    guide=F
  ) +
  xlab("") +
  ylab("") +
  facet_wrap(
    ~ facet + dv,
    ncol=2
  ) +
  theme_bw(
    base_family="CM Roman",
    base_size=14
  )

tmpname<-"fig_allgains.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8,
  height=12
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

#output for prez
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
    width=thiselement$width/1.5,
    height=thiselement$height/1.5
  )
  #embed font
  newfilename<-str_replace(
    thiselement$filename,
    "\\.pdf",
    "_PREZ.pdf"
  )
  embed_fonts(
    file="tmp.pdf",
    outfile=newfilename
  )
  file.remove(
    "tmp.pdf"
  )
  Sys.sleep(1)
}
