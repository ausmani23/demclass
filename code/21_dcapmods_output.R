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
setwd(filesdir); dir()
load('dcapmods.RData')

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

#COEFPLOT
#PREFESTS
#union.ratio,frequency.ratio,volume.ratio
tmp<-dcapestsdf$dv%in%c(
  "union.ratio",
  "frequency.ratio",
  "volume.ratio"
)
tmp<-tmp & dcapestsdf$df=="one"
tmp<-tmp & str_detect(
  dcapestsdf$mname,
  "full"
)
plotdf<-dcapestsdf[tmp,]

#standardize
plotdf$mu<-plotdf$mu/plotdf$dvsd
plotdf$mu.max<-plotdf$mu.max/plotdf$dvsd
plotdf$mu.min<-plotdf$mu.min/plotdf$dvsd

#add pval info to shape of point
plotdf$pval.shp<-NA
plotdf$pval.shp[plotdf$pval.class=="at alpha=0.01"]<-1
plotdf$pval.shp[plotdf$pval.class=="at alpha=0.05"]<-2
plotdf$pval.shp[plotdf$pval.class=="at alpha=0.10"]<-3
plotdf$pval.shp[plotdf$pval.class=="not sig"]<-4
plotdf$pval.shp<-factor(
  plotdf$pval.shp,
  levels=c(1,2,3,4),
  labels=c("at alpha=0.01","at alpha=0.05","at alpha=0.10","not sig")
)
tmpshapes<-c(8,4,16,1)
names(tmpshapes)<-levels(plotdf$pval.shp)
shp.labels<-c(
  bquote(alpha == 0.01),
  bquote(alpha == 0.05),
  bquote(alpha == 0.10)
)

#dv
tmplevels<-sapply(
  unique(plotdf$dv),
  getvarcode,
  F,"varname",
  "order",
  varsdf
) %>% 
  sort %>% 
  names
tmplabels<-sapply(
  tmplevels,
  getvarcode,
  F,"varname",
  "propername",
  varsdf
)
plotdf$dv<-factor(
  plotdf$dv,
  levels=rev(tmplevels),
  labels=rev(tmplabels)
)

#mod
tmp<-str_detect(
  plotdf$mname,
  "bvrt"
)
plotdf$mname[tmp]<-"Bivariate"
tmp<-str_detect(
  plotdf$mname,
  "full"
)
plotdf$mname[tmp]<-"Full"
plotdf$mname<-factor(
  plotdf$mname,
  levels=c(
    "Full",
    "Bivariate"
  ),
  labels=c(
    "Full",
    "Bivariate"
  )
)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=dv,
    y=mu,
    ymin=mu.min,
    ymax=mu.max,
    shape=pval.shp
  )
) +
  geom_point(
    size=2
  ) +
  geom_errorbar(
    size=0.4,
    width=0.2
  ) +
  geom_hline(
    yintercept=0,
    #color='red',
    linetype='dashed'
  ) +
  scale_shape_manual(
    name="",
    values=tmpshapes,
    labels=shp.labels,
    drop=F
  ) +
  coord_flip() +
  # facet_wrap(
  #   ~ dv,
  #   ncol=1
  # ) +
  xlab("") +
  ylab("\nLong-Run Impact of Disruptive Capacity (in SDs)") +
  theme_bw(
    base_family="CM Roman",
    base_size=14
  )

tmpname<-"fig_dcapests.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8,
  height=3
)

#########################################################
#########################################################

#TILEPLOT
#ROBUSTS
plotdf<-dcapestsdf

#get pval fill, for tile
plotdf$pval.fill<-NA
plotdf$pval.fill[plotdf$pval.class=="at alpha=0.01"]<-4
plotdf$pval.fill[plotdf$pval.class=="at alpha=0.05"]<-3
plotdf$pval.fill[plotdf$pval.class=="at alpha=0.10"]<-2
plotdf$pval.fill[plotdf$pval.class=="not sig"]<-1
negmu<-ifelse(plotdf$mu<0,-1,1)
plotdf$pval.fill<-plotdf$pval.fill * negmu
pval.labels<-c("at alpha=0.01","at alpha=0.05","at alpha=0.10","")
tmplabels<-c(
  paste0("- /",pval.labels),
  paste0("+ /",rev(pval.labels))
)
#assign levels,colors
plotdf$pval.fill<-factor(
  plotdf$pval.fill,
  levels=c(-4,-3,-2,-1,1,2,3,4),
  labels=tmplabels
)
#for colors, consult brewer
tmpcolors<-brewer.pal(8,"RdYlGn")
names(tmpcolors)<-levels(plotdf$pval.fill)
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

#models
tmplevels<-c(
  "one",
  "five",
  "ten",
  "fifteen",
  "twenty"
)
tmplabels<-c(
  "Annual",
  "Five-Year",
  "Ten-Year",
  "Fifteen-Year",
  "Twenty-Year"
)
plotdf$df<-factor(
  plotdf$df,
  levels=tmplevels,
  labels=tmplabels
)

plotdf$dvtype<-str_extract(
  plotdf$dv,
  "pcap|ratio"
)
plotdf$dvtype[is.na(plotdf$dvtype)]<-
  "nonaglf"
tmplevels<-c(
  "ratio",
  "nonaglf",
  "pcap"
)
tmplabels<-c(
  "Preferred",
  "% of Non-Ag Labor Force",
  "% of Population"
)
plotdf$dvtype<-factor(
  plotdf$dvtype,
  levels=rev(tmplevels),
  labels=rev(tmplabels)
)

plotdf$dvbase<-str_extract(
  plotdf$dv,
  "union|frequency|volume"
)
tmplevels<-c(
  "union",
  "frequency",
  "volume"
)
tmplabels<-c(
  "Union Membership",
  "Strike Frequency",
  "Strike Volume"
)
plotdf$dvbase<-factor(
  plotdf$dvbase,
  levels=tmplevels,
  labels=tmplabels
)

plotdf$ivspec<-factor(
  plotdf$ivspec,
  levels=c(
    "dcapbvrt",
    "dcapfull"
  ),
  labels=c(
    "Bivariate",
    "Full"
  )
)


#make plot #1
##to show dfs
g.tmp<-ggplot(
  plotdf[plotdf$ivspec=="Full",],
  aes(
    x=df,
    y=dvtype,
    fill=pval.fill
  )
) +
  geom_tile() +
  facet_wrap(
   ~ dvbase,
   ncol=1
  ) +
  scale_fill_manual(
    name="",
    values=tmpcolors,
    labels=fill.labels,
    drop=F
  ) +
  xlab("") + 
  ylab("") +
  theme_bw(
    base_family="CM Roman",
    base_size=14
  ) 
g.tmp

tmpname<-"fig_dcaprobs.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8,
  height=11
)

#make plot #1
##to show specs
g.tmp<-ggplot(
  plotdf[plotdf$df=="Annual",],
  aes(
    x=ivspec,
    y=dvtype,
    fill=pval.fill
  )
) +
  geom_tile() +
  facet_wrap(
    ~ dvbase,
    ncol=1
  ) +
  scale_fill_manual(
    name="",
    values=tmpcolors,
    labels=fill.labels,
    drop=F
  ) +
  xlab("") + 
  ylab("") +
  theme_bw(
    base_family="CM Roman",
    base_size=14
  ) 
g.tmp

tmpname<-"fig_dcaprobs2.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8,
  height=11
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
