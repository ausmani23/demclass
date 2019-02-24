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

#load dfs again
setwd(filesdir); dir()
load('unitroots.RData')

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

#BALANCED GRAPH

plotdf<-baldf

#omit hadri, since it's undiscerning and fails simulation
tmp<-plotdf$test!="hadri"
plotdf<-plotdf[tmp,]

tmplevels<-c(
  "levinlin",
  "ips",
  "madwu",
  "hadri"
)
tmplabels<-c(
  "LLC",
  "IPS",
  "Maddala",
  "Hadri"
)
plotdf$test<-factor(
  plotdf$test,
  tmplevels,
  tmplabels
)

tmp<-sapply(
  plotdf$var,
  getvarorder
) %>% sort
tmplevels<-names(tmp) %>% 
  unique
tmplabels<-sapply(
  tmplevels,
  getvarname
)
plotdf$var<-factor(
  plotdf$var,
  levels=tmplevels,
  labels=tmplabels
)

tmpcolors<-brewer.pal(8,"RdYlGn")
tmpfill<-c(
  tmpcolors[8],
  tmpcolors[1]
)
names(tmpfill)<-c(
  "No",
  "Yes"
)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=min.N,
    y=var,
    fill=unitroot
  )
) + 
  geom_tile() +
  scale_fill_manual(
    name="Unit Root?",
    values=tmpfill
  ) +
  facet_wrap(
    ~ test,
    ncol=1
  ) + 
  xlab("\nMinimum N for Balanced Panel") +
  ylab("") +
  theme_bw(
    base_family="CM Roman",
    base_size=14
  )
tmpname<-"fig_balur.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8,
  height=10
)

#could add unbalanced later,
#but in simulations it does much worse.. 

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
  Sys.sleep(0.5)
}

