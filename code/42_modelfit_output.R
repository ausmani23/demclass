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
load('modelfit_stats.RData')

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

#PLOT FIT IMPROVEMENT
#amongst models

fitsumdf

tmp<-!fitsumdf$perm%in%c(
  "global",
  "regional",
  "initial",
  "pooled",
  "nolag"
)
plotdf<-fitsumdf[tmp,]

#order mods
tmplevels<-c(
  "fe",
  "disruptive",
  "landlords",
  "socialforces",
  "modernization",
  "inequality",
  "preferred"
)
tmplabels<-c(
  "FE Only",
  "DCAP Only",
  "LAND Only",
  "Social Forces",
  "Modernization",
  "Inequality",
  "Preferred"
)
plotdf$perm<-factor(
  plotdf$perm,
  levels=rev(tmplevels),
  labels=rev(tmplabels)
)

#order stats
tmplevels<-c(
  "mae",
  "rmse"
)
tmplabels<-c(
  "Mean Absolute Error",
  "Root Mean Square Error"
)
plotdf$stat<-factor(
  plotdf$stat,
  levels=tmplevels,
  labels=tmplabels
)

#order dv
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

#add hlines
extradf<-by(
  plotdf,
  list(plotdf$dv,plotdf$stat),
  function(df) {
    data.frame(
      dv=unique(df$dv),
      stat=unique(df$stat),
      prefit=df$pctchg[df$perm=="Preferred"],
      stringsAsFactors=F
    )
  }) %>% rbind.fill

g.tmp<-ggplot(
  plotdf,
  aes(
    x=perm,
    y=pctchg
  )
) +
  geom_bar(
    stat='identity'
  ) +
  geom_hline(
    data=extradf,
    aes(
      yintercept=prefit
    ),
    color='red',
    linetype='dashed'
  ) + 
  facet_wrap(
    ~ dv + stat
  ) +
  coord_flip() +
  xlab("") +
  ylab("\n% Less Error Relative to FE Only") +
  theme_bw(
    base_family="CM Roman",
    base_size=14
  )
g.tmp

tmpname<-"fig_mfit.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8,
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
}
