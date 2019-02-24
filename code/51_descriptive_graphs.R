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

#DESCRIPTIVE GRAPH OF DV'S
setwd(datadir); dir()
demdfs<-readRDS('demdfs.RDS')
demdf<-demdfs$one

names(demdf)
keyvars<-c(
  "advanced",
  "year",
  "polity2",
  "v2x_polyarchy",
  "democracy"
)
tmpdf<-demdf[,keyvars]
tmpdf$democracy<-tmpdf$democracy * 100 #same scale

#make long
tmpdf<-gather(
  tmpdf,
  dv,
  val,
  polity2:democracy
)

#keep non-missing
tmprows<-complete.cases(tmpdf)
tmpdf<-tmpdf[tmprows,]

#########################################################
#########################################################

#GRAPH 1
#over time, all countries
tmplist<-list(
  tmpdf$year,
  tmpdf$dv
)
plotdf<-by(tmpdf,tmplist,function(df) {
  data.frame(
    year=unique(df$year),
    dv=unique(df$dv),
    val=mean(df$val),
    stringsAsFactors=F
  )
}) %>% rbind.fill

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

g.tmp<-ggplot(
  plotdf,
  aes(
    x=year,
    y=val,
    group=dv
  )
) +
  geom_line() +
  # geom_line(
  #   stat='smooth',
  #   method='lm',
  #   color='red',
  #   se=F,
  #   alpha=0.4
  # ) +
  facet_wrap(
    ~ dv,
    ncol=2,
    nrow=2
  ) +
  xlab("") +
  ylab("Average\n") +
  theme_bw(
    base_family="CM Roman",
    base_size=14
  )

tmpname<-"fig_globalavg.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=10,
  height=10
)

#########################################################
#########################################################


#GRAPH 2
#over time, the democracy gap

tmplist<-list(
  tmpdf$year,
  tmpdf$dv,
  tmpdf$advanced
)
plotdf<-by(tmpdf,tmplist,function(df) {
  data.frame(
    year=unique(df$year),
    dv=unique(df$dv),
    advanced=unique(df$advanced),
    val=mean(df$val),
    stringsAsFactors=F
  )
}) %>% rbind.fill

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

tmplevels<-c(
  T,
  F
)
tmplabels<-c(
  "Advanced",
  "Developing"
)
plotdf$advanced<-factor(
  plotdf$advanced,
  tmplevels,
  tmplabels
)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=year,
    y=val,
    group=advanced,
    color=advanced
  )
) +
  geom_line() +
  # geom_line(
  #   stat='smooth',
  #   method='lm',
  #   color='red',
  #   se=F,
  #   alpha=0.4
  # ) +
  scale_color_discrete(
    name=""#,
    #guide=F
  ) +
  facet_wrap(
    ~ dv,
    ncol=2,
    nrow=2
  ) +
  xlab("") +
  ylab("Average\n") +
  theme_black(
    base_family="CM Roman",
    base_size=14
  ) +
  theme(
    legend.position = c(0.75,0.3)
  )

tmpname<-"fig_splitavg.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=10,
  height=10
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
    plot=thiselement$graph + theme_black(),
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