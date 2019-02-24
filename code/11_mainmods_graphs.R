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

#plotting prelims
require(ggplot2)
require(ggthemes)
require(extrafont)
require(RColorBrewer)
#load fonts
loadfonts(quiet=T) #register w/ pdf
loadfonts(device = "win",quiet=T) #register w/ windows
#get ghostscript, for tex output
Sys.setenv(
  R_GSCMD = gsdir_full
)
#initialize graphlist
gs.list<-list()

#########################################################
#########################################################

#confirm that where a LRM is significant,
#so is the corresponding short-run estimate

tmp<-!finaldf$iv%in%c("polity2","v2x_polyarchy")
tmpdf<-finaldf[tmp,]
tmplist<-list(
  tmpdf$mname,
  tmpdf$iv
)
tmpdf<-by(tmpdf,tmplist,function(df) {
  print(unique(df$mname))
  print(unique(df$iv))
  #df<-finaldf[finaldf$mname=="polity2.fifteen.disruptive" & finaldf$iv=="highcapratio",]
  tmplevels<-c(
    "not sig",
    "at alpha=0.10",
    "at alpha=0.05",
    "at alpha=0.01"
  )
  df$pval.num<-factor(
    df$pval.class,
    tmplevels,
    tmplevels 
  ) %>% as.numeric
  sr<-df$pval.num[df$type=="shortrun"]
  lr<-df$pval.num[df$type=="longrun"]
  if(sr<lr) {
    df$checker<-"srless" 
  } else if (sr==lr) {
    df$checker<-"equal"
  } else if( sr> lr) {
    df$checker<-"srmore"
  }
  df
}) %>% rbind.fill
tmptab<-table(tmpdf$checker)
100 * tmptab/sum(tmptab)

tmpdf[tmpdf$checker=="srless",]
tmpdf[tmpdf$checker=="srmore",]


#########################################################
#########################################################

#SETUP FINALDF

#get pval fill, for tile
finaldf$pval.fill<-NA
finaldf$pval.fill[finaldf$pval.class=="at alpha=0.01"]<-4
finaldf$pval.fill[finaldf$pval.class=="at alpha=0.05"]<-3
finaldf$pval.fill[finaldf$pval.class=="at alpha=0.10"]<-2
finaldf$pval.fill[finaldf$pval.class=="not sig"]<-1
negmu<-ifelse(finaldf$mu<0,-1,1)
finaldf$pval.fill<-finaldf$pval.fill * negmu
pval.labels<-c("at alpha=0.01","at alpha=0.05","at alpha=0.10","")
tmplabels<-c(
  paste0("- /",pval.labels),
  paste0("+ /",rev(pval.labels))
)
#assign levels,colors
finaldf$pval.fill<-factor(
  finaldf$pval.fill,
  levels=c(-4,-3,-2,-1,1,2,3,4),
  labels=tmplabels
)
#for black/white graphs, binary colors
finaldf$sign<-'neg'
tmp<-finaldf$mu>0
finaldf$sign[tmp]<-'pos'
finaldf$sign<-factor(
  finaldf$sign,
  levels=c('neg','pos'),
  labels=c('neg','pos')
)

#for colors, consult brewer
tmpcolors<-brewer.pal(8,"RdYlGn")
names(tmpcolors)<-levels(finaldf$pval.fill)
fill.labels<-c(
  expression(paste(alpha==0.01,", ",beta<0)),
  expression(paste(alpha==0.05,", ",beta<0)),
  expression(paste(alpha==0.10,", ",beta<0)),
  expression(paste(beta<0)),
  expression(paste(beta>0)),
  expression(paste(alpha==0.10,", ",beta>0)),
  expression(paste(alpha==0.05,", ",beta>0)),
  expression(paste(alpha==0.01,", ",beta>0))
)

#b/w option
tmpcolors_bw<-brewer.pal(8,"Greys")
names(tmpcolors_bw)<-levels(finaldf$pval.fill)
fill.labels<-c(
  expression(paste(alpha==0.01,", ",beta<0)),
  expression(paste(alpha==0.05,", ",beta<0)),
  expression(paste(alpha==0.10,", ",beta<0)),
  expression(paste(beta<0)),
  expression(paste(beta>0)),
  expression(paste(alpha==0.10,", ",beta>0)),
  expression(paste(alpha==0.05,", ",beta>0)),
  expression(paste(alpha==0.01,", ",beta>0))
)

#binary option
tmpcolors_bin<-c("grey","white")
names(tmpcolors_bin)<-levels(finaldf$sign)
fill.labels_bin<-c(
  expression(paste(beta<0)),
  expression(paste(beta>0))
)


#add shape
finaldf$pval.shp<-NA
finaldf$pval.shp[finaldf$pval.class=="at alpha=0.01"]<-1
finaldf$pval.shp[finaldf$pval.class=="at alpha=0.05"]<-2
finaldf$pval.shp[finaldf$pval.class=="at alpha=0.10"]<-3
finaldf$pval.shp[finaldf$pval.class=="not sig"]<-4
finaldf$pval.shp<-factor(
  finaldf$pval.shp,
  levels=c(1,2,3,4),
  labels=c("at alpha=0.01","at alpha=0.05","at alpha=0.10","not sig")
)
tmpshapes<-c(8,4,16,1)
names(tmpshapes)<-levels(finaldf$pval.shp)
tmpshapes_bin<-c(8,4,16,32)
names(tmpshapes_bin)<-levels(finaldf$pval.shp)
shp.labels<-c(
  bquote(alpha == 0.01),
  bquote(alpha == 0.05),
  bquote(alpha == 0.10)
)

#########################################################
#########################################################

#TILE PLOTS
tmp<-finaldf$iv%in%c(
  "highcapratio",
  "lngdpcap",
  "gdpgr",
  "polity2_regavg",
  "v2x_polyarchy_regavg",
  "landlords",
  "gini.generous",
  "years.all",
  "urban02"
) &
  finaldf$type=="longrun" &
  finaldf$ivspec=="preferred"
plotdf<-finaldf[tmp,]

#order
tmplevels<-c(
  "one",
  "five",
  "ten",
  "fifteen",
  "twenty"
)
tmplabels<-c(
  "1",
  "5",
  "10",
  "15",
  "20"
)
plotdf$df<-factor(
  plotdf$df,
  levels=tmplevels,
  labels=tmplabels
)

# #terms
# tmp<-plotdf$ivspec%in%c("modernization","disruptive","landlords")
# plotdf$ivspec[tmp]<-"bivariate"
# tmplevels<-c(
#   "bivariate",
#   "main3",
#   "preferred"
# )
# tmplabels<-c(
#   "Bivariate",
#   "Main",
#   "Preferred"
# )
# plotdf$ivspec<-factor(
#   plotdf$ivspec,
#   levels=rev(tmplevels),
#   labels=rev(tmplabels)
# )

tmplevels<-sapply(
  plotdf$dv,
  getvarorder
) %>% sort %>%
  names %>%
  unique
tmplabels<-sapply(
  tmplevels,
  getvarname
)
plotdf$dv<-factor(
  plotdf$dv,
  levels=tmplevels,
  labels=tmplabels
)

#plot

tmplevels<-sapply(
  plotdf$iv,
  getvarorder
) %>% sort %>%
  names %>% unique
tmplabels<-sapply(
  tmplevels,
  getvarname
)
plotdf$iv<-factor(
  plotdf$iv,
  levels=rev(tmplevels),
  labels=rev(tmplabels)
)

#color version
g.tmp<-ggplot(
  plotdf,
  aes(
    x=df,
    y=iv,
    fill=pval.fill
  )
) +
  geom_tile() +
  scale_fill_manual(
    name="",
    values=tmpcolors,
    labels=fill.labels,
    drop=F
  ) +
  xlab("\nInterval Between Observations") + 
  ylab("") +
  facet_wrap(
    ~ dv,
    ncol=3
  ) + 
  theme_bw(
    base_family="CM Roman",
    base_size=14
  )
g.tmp

gs.list[["fig_prefests_color"]]<-list(
  graph=g.tmp,
  filename="fig_prefests_color.pdf",
  width=14/1.25,
  height=8/1.25
)

#bin version
g.tmp<-ggplot(
  plotdf,
  aes(
    x=df,
    y=iv,
    fill=sign,
    shape=pval.shp
  )
) +
  geom_tile() +
  geom_point() +
  scale_fill_manual(
    name="",
    values=tmpcolors_bin,
    labels=fill.labels_bin,
    drop=F
  ) +
  scale_shape_manual(
    name="",
    values=tmpshapes_bin,
    labels=shp.labels,
    drop=F
  ) +
  guides(
    fill=guide_legend(override.aes=list(shape=32))
  ) +
  xlab("\nInterval Between Observations") + 
  ylab("") +
  facet_wrap(
    ~ dv,
    ncol=3
  ) + 
  theme_bw(
    base_family="CM Roman",
    base_size=14
  )
g.tmp

gs.list[["fig_prefests"]]<-list(
  graph=g.tmp,
  filename="fig_prefests.pdf",
  width=14/1.25,
  height=8/1.25
)

#bw version
g.tmp<-ggplot(
  plotdf,
  aes(
    x=df,
    y=iv,
    fill=pval.fill
  )
) +
  geom_tile() +
  scale_fill_manual(
    name="",
    values=tmpcolors_bw,
    labels=fill.labels,
    drop=F
  ) +
  xlab("\nInterval Between Observations") + 
  ylab("") +
  facet_wrap(
    ~ dv,
    ncol=3
  ) + 
  theme_bw(
    base_family="CM Roman",
    base_size=14
  )
g.tmp

gs.list[["fig_prefests_bw"]]<-list(
  graph=g.tmp,
  filename="fig_prefests_bw.pdf",
  width=14/1.25,
  height=8/1.25
)



#########################################################
#########################################################

#PREFESTS 
tmp<-finaldf$iv%in%c(
  "highcapratio",
  "lngdpcap",
  "landlords"
) &
  finaldf$type=="longrun"
tmp<-tmp & str_detect(
  finaldf$mname,
  "preferred"
)
plotdf<-finaldf[tmp,]

#order
tmplevels<-c(
  "one",
  "five",
  "ten",
  "fifteen",
  "twenty"
)
tmplabels<-c(
  "1",
  "5",
  "10",
  "15",
  "20"
)
plotdf$df<-factor(
  plotdf$df,
  levels=rev(tmplevels),
  labels=rev(tmplabels)
)

tmplevels<-sapply(
  plotdf$dv,
  getvarorder
) %>% sort %>%
  names %>%
  unique
tmplabels<-sapply(
  tmplevels,
  getvarname
)
plotdf$dv<-factor(
  plotdf$dv,
  levels=tmplevels,
  labels=tmplabels
)

tmplevels<-sapply(
  plotdf$iv,
  getvarorder
) %>% sort %>%
  names %>% unique
tmplabels<-sapply(
  tmplevels,
  getvarname
)
plotdf$iv<-factor(
  plotdf$iv,
  levels=tmplevels,
  labels=tmplabels
)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=df,
    y=musd,
    ymin=musd.min,
    ymax=musd.max,
    shape=pval.shp
  )
) +
  geom_errorbar(
    size=0.3,
    width=0.2
  ) +
  geom_point() +
  geom_hline(
    yintercept=0,
    linetype='dashed',
    color='darkred'
  ) +
  scale_shape_manual(
    name="",
    values=tmpshapes,
    labels=shp.labels,
    drop=F
  ) +
  facet_wrap(
    ~ dv + iv
  ) +
  coord_flip() +
  xlab("") +
  ylab("") +
  theme_bw(
    base_family="CM Roman",
    base_size=14
  )
g.tmp

tmpname<-"fig_prefcoefs.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=10,
  height=8
)

#########################################################
#########################################################

#KEY ESTS ONLY
#across specs
tmp<-finaldf$iv%in%c(
  "highcapratio",
  "landlords"
) &
  finaldf$type=="longrun"
tmp<-tmp & finaldf$df=="one"
tmp<-tmp & finaldf$ivspec%in%c(
  # "disruptive",
  # "landlords",
  "preferred"
)
plotdf<-finaldf[tmp,]

tmplevels<-sapply(
  plotdf$iv,
  getvarorder
) %>% sort %>%
  names %>% unique
tmplabels<-sapply(
  tmplevels,
  getvarname
)
plotdf$iv<-factor(
  plotdf$iv,
  levels=tmplevels,
  labels=tmplabels
)

tmplevels<-c(
  "disruptive",
  "landlords",
  "preferred"
)
tmplabels<-c(
  "DCAP",
  "LAND",
  "Pref"
)
plotdf$ivspec<-factor(
  plotdf$ivspec,
  tmplevels,
  tmplabels
)

tmplevels<-sapply(
  plotdf$dv,
  getvarorder
) %>% sort %>%
  names %>%
  unique
tmplabels<-sapply(
  tmplevels,
  getvarname
)
plotdf$dv<-factor(
  plotdf$dv,
  levels=tmplevels,
  labels=tmplabels
)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=ivspec,
    y=iv,
    fill=pval.fill
  )
) +
  geom_tile() +
  scale_fill_manual(
    name="",
    values=tmpcolors,
    labels=fill.labels,
    drop=F
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

tmpname<-"fig_keycoefs.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=12,
  height=8
)

#########################################################
#########################################################

#KEY ESTS,
#pref, across lags

tmp<-finaldf$iv%in%c(
  "highcapratio",
  "landlords"
) &
  finaldf$type=="longrun" 
tmp<-tmp & finaldf$ivspec%in%c(
  "preferred"
)
plotdf<-finaldf[tmp,]

#order
tmplevels<-c(
  "one",
  "five",
  "ten",
  "fifteen",
  "twenty"
)
tmplabels<-c(
  "1",
  "5",
  "10",
  "15",
  "20"
)
plotdf$df<-factor(
  plotdf$df,
  levels=tmplevels,
  labels=tmplabels
)

tmplevels<-sapply(
  plotdf$iv,
  getvarorder
) %>% sort %>%
  names %>% unique
tmplabels<-sapply(
  tmplevels,
  getvarname
)
plotdf$iv<-factor(
  plotdf$iv,
  levels=tmplevels,
  labels=tmplabels
)

tmplevels<-sapply(
  plotdf$dv,
  getvarorder
) %>% sort %>%
  names %>%
  unique
tmplabels<-sapply(
  tmplevels,
  getvarname
)
plotdf$dv<-factor(
  plotdf$dv,
  levels=tmplevels,
  labels=tmplabels
)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=df,
    y=iv,
    fill=pval.fill
  )
) +
  geom_tile() +
  scale_fill_manual(
    name="",
    values=tmpcolors,
    labels=fill.labels,
    drop=F
  ) +
  facet_wrap(
    ~ dv
  ) +
  xlab("\nLag") + 
  ylab("") + 
  theme_bw(
    base_family="CM Roman",
    base_size=14
  )
g.tmp


tmpname<-"fig_keycoefs_lags.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=12,
  height=4
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

# #output for prez
# #output graphlist
# setwd(outputdir)
# this.sequence<-seq_along(gs.list)
# for(i in this.sequence) {
#   print(
#     paste0(
#       "saving ",i," of ",length(this.sequence)
#     )
#   )
#   thiselement<-gs.list[[i]]
#   ggsave(
#     filename="tmp.pdf",
#     plot=thiselement$graph,
#     width=thiselement$width/1.5,
#     height=thiselement$height/1.5
#   )
#   #embed font
#   newfilename<-str_replace(
#     thiselement$filename,
#     "\\.pdf",
#     "_PREZ.pdf"
#   )
#   embed_fonts(
#     file="tmp.pdf",
#     outfile=newfilename
#   )
#   file.remove(
#     "tmp.pdf"
#   )
#   Sys.sleep(1)
# }