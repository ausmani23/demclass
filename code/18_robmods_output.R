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

#ROBMODS OUTPUT
setwd(filesdir); dir()
load('robmods.RData')

setwd(datadir)
demdfs<-readRDS('demdfs.RDS')

#add groups to robests
robestsdf$group<-
  sapply(
    robestsdf$mod,
    getvarcode,
    append=F,
    from="mname",
    to="group",
    rawrobdf
  )

#get display
robestsdf$display<-
  sapply(
    robestsdf$mod,
    getvarcode,
    append=F,
    from="mname",
    to="maindisplay",
    rawrobdf
  )

#view eff size of nolag
tmp<-robestsdf$mod=="nolag" &
  robestsdf$iv%in%c('highcapratio','landlords')
robestsdf[tmp,]
tmp<-robestsdf$mod=="pref" &
  robestsdf$iv%in%c("highcapratio",'landlords')
robestsdf[tmp,]

#TILE PLOT
#model,iv faceted by dv/type
tmp<-robestsdf$mod!="pref" &
  robestsdf$iv%in%c(
    "highcapratio",
    "landlords"
  ) &
  robestsdf$display #ones toshow
plotdf<-robestsdf[tmp,]

#no need to repeat vdem var display
tmp<-plotdf$dv=="polity2" & 
  plotdf$mod%in%c(
    "liberal",
    "deliberative",
    "participatory",
    "egalitarian"
  ) 
plotdf<-plotdf[!tmp,]

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
brewer.pal.info
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

#groups
unique(plotdf$group)
tmplevels<-c(
  "restricted",
  "threshold",
  "spec",
  "DCAP",
  "altdv"
)
tmplabels<-c(
  "Consistent Sample",
  "Cutoff",
  "Specification",
  "Alternative DCAP",
  "Alternative DV"
)
plotdf$group<-factor(
  plotdf$group,
  levels=tmplevels,
  labels=tmplabels
)

#mods
tmp<-sapply(
  plotdf$mod,
  getmodorder
) %>% sort
tmplevels<-names(tmp) %>% 
  unique
tmplabels<-sapply(
  tmplevels,
  getmodname
)
plotdf$mod<-factor(
  plotdf$mod,
  levels=rev(tmplevels),
  labels=rev(tmplabels)
)

#dv
tmp<-sapply(
  plotdf$iv,
  getvarorder
)
tmplevels<-names(tmp) %>% 
  unique
tmplabels<-sapply(
  tmplevels,
  getvarname
)
tmplabels<-c(
  #"GDP",
  "DCAP",
  "LAND"
)
plotdf$iv<-factor(
  plotdf$iv,
  levels=tmplevels,
  labels=tmplabels
)

#dv
tmp<-sapply(
  plotdf$dv,
  getvarorder
)
tmplevels<-names(tmp) %>% 
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
    x=iv,
    y=mod,
    fill=pval.fill
  )
) + 
  geom_tile() +
  facet_grid(
    group~dv,
    scales='free'
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
tmpname<-"fig_robests.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8*1.1,
  height=10*1.1
)

#########################################################
#########################################################

#TILE PLOT
#intests
#take a quick look at ests
head(intestsdf)
tmp<-intestsdf$mainvar=="L.highcapratio" &
  intestsdf$intvar=="L.highcapratio"
intestsdf[tmp,]
tmp<-intestsdf$mainvar=="L.highcapratio" &
  intestsdf$intvar=="L.landlords"
intestsdf[tmp,]

tmp<-intestsdf$mainvar=="L.highcapratio" & 
  intestsdf$intvar=="L.wars_interstate"
intestsdf[tmp,]

#get pval fill, for tile
intestsdf$pval.fill<-NA
intestsdf$pval.fill[intestsdf$pval.class=="at alpha=0.01"]<-4
intestsdf$pval.fill[intestsdf$pval.class=="at alpha=0.05"]<-3
intestsdf$pval.fill[intestsdf$pval.class=="at alpha=0.10"]<-2
intestsdf$pval.fill[intestsdf$pval.class=="not sig"]<-1
negmu<-ifelse(intestsdf$mu<0,-1,1)
intestsdf$pval.fill<-intestsdf$pval.fill * negmu
pval.labels<-c("at alpha=0.01","at alpha=0.05","at alpha=0.10","")
tmplabels<-c(
  paste0("- /",pval.labels),
  paste0("+ /",rev(pval.labels))
)
#assign levels,colors
intestsdf$pval.fill<-factor(
  intestsdf$pval.fill,
  levels=c(-4,-3,-2,-1,1,2,3,4),
  labels=tmplabels
)
#for colors, consult brewer
tmpcolors<-brewer.pal(8,"RdYlGn")
names(tmpcolors)<-levels(intestsdf$pval.fill)
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

#add pval info to shape of point
intestsdf$pval.shp<-NA
intestsdf$pval.shp[intestsdf$pval.class=="at alpha=0.01"]<-1
intestsdf$pval.shp[intestsdf$pval.class=="at alpha=0.05"]<-2
intestsdf$pval.shp[intestsdf$pval.class=="at alpha=0.10"]<-3
intestsdf$pval.shp[intestsdf$pval.class=="not sig"]<-4
intestsdf$pval.shp<-factor(
  intestsdf$pval.shp,
  levels=c(1,2,3,4),
  labels=c("at alpha=0.01","at alpha=0.05","at alpha=0.10","not sig")
)
tmpshapes<-c(8,4,16,1)
names(tmpshapes)<-levels(intestsdf$pval.shp)
shp.labels<-c(
  bquote(alpha == 0.01),
  bquote(alpha == 0.05),
  bquote(alpha == 0.10)
)

#get coefs for some key ints

#hcapXland
tmp<-intestsdf$mainvar=="L.highcapratio" &
  intestsdf$intvar=="L.landlords"
intestsdf[tmp,]
#landXhcap
tmp<-intestsdf$mainvar=="L.landlords" &
  intestsdf$intvar=="L.highcapratio"
intestsdf[tmp,]
#hcapXwar
tmp<-intestsdf$mainvar=="L.highcapratio" &
  intestsdf$intvar=="L.wars_interstate"
intestsdf[tmp,]

#TILE PLOT
#limit to hcap,land
tmp<-intestsdf$mainvar%in%c(
  "L.highcapratio",
  "L.landlords"
) & intestsdf$intvar!="L.wars_interstate"
plotdf<-intestsdf[tmp,]

#round the pctile var
plotdf$pctile<-factor(
  100 * 
    plotdf$atpctile %>% 
    as.numeric %>% 
    round(2)
)

#fix vars
tmplevels<-sapply(
  unique(plotdf$mainvar),
  getvarorder
) %>% sort %>% names
tmplabels<-sapply(
  tmplevels,
  getvarname
)
plotdf$mainvar<-factor(
  plotdf$mainvar,
  levels=tmplevels,
  labels=tmplabels
)

tmplevels<-sapply(
  unique(plotdf$intvar),
  getvarorder
) %>% sort %>% names
tmplabels<-sapply(
  tmplevels,
  getvarname
)
plotdf$intvar<-factor(
  plotdf$intvar,
  levels=rev(tmplevels),
  labels=rev(tmplabels)
)

tmplevels<-sapply(
  unique(plotdf$dv),
  getvarorder
) %>% sort %>% names
tmplabels<-sapply(
  tmplevels,
  getvarname
)
plotdf$dv <- factor(
  plotdf$dv,
  levels=tmplevels,
  labels=tmplabels
)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=pctile,
    y=intvar,
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
    ~ mainvar + dv,
    ncol=2
  ) +
  xlab("\nPercentile") +
  ylab("Interacted With\n") +
  theme_bw(
    base_family="CM Roman",
    base_size=14
  )
tmpname<-"fig_interactions.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=6*1.5,
  height=6*1.5
)

#########################################################
#########################################################

#TILE PLOT
#jacknife

#get pval fill, for tile
jkdf$pval.fill<-NA
jkdf$pval.fill[jkdf$pval.class=="at alpha=0.01"]<-4
jkdf$pval.fill[jkdf$pval.class=="at alpha=0.05"]<-3
jkdf$pval.fill[jkdf$pval.class=="at alpha=0.10"]<-2
jkdf$pval.fill[jkdf$pval.class=="not sig"]<-1
negmu<-ifelse(jkdf$mu<0,-1,1)
jkdf$pval.fill<-jkdf$pval.fill * negmu
pval.labels<-c("at alpha=0.01","at alpha=0.05","at alpha=0.10","")
tmplabels<-c(
  paste0("- /",pval.labels),
  paste0("+ /",rev(pval.labels))
)
#assign levels,colors
jkdf$pval.fill<-factor(
  jkdf$pval.fill,
  levels=c(-4,-3,-2,-1,1,2,3,4),
  labels=tmplabels
)
#for colors, consult brewer
tmpcolors<-brewer.pal(8,"RdYlGn")
names(tmpcolors)<-levels(jkdf$pval.fill)
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

#add pval info to shape of point
jkdf$pval.shp<-NA
jkdf$pval.shp[jkdf$pval.class=="at alpha=0.01"]<-1
jkdf$pval.shp[jkdf$pval.class=="at alpha=0.05"]<-2
jkdf$pval.shp[jkdf$pval.class=="at alpha=0.10"]<-3
jkdf$pval.shp[jkdf$pval.class=="not sig"]<-4
jkdf$pval.shp<-factor(
  jkdf$pval.shp,
  levels=c(1,2,3,4),
  labels=c("at alpha=0.01","at alpha=0.05","at alpha=0.10","not sig")
)
tmpshapes<-c(8,4,16,1)
names(tmpshapes)<-levels(jkdf$pval.shp)
shp.labels<-c(
  bquote(alpha == 0.01),
  bquote(alpha == 0.05),
  bquote(alpha == 0.10)
)

#dv
tmp<-sapply(
  jkdf$iv,
  getvarorder
)
tmplevels<-names(tmp) %>%
  unique
tmplabels<-sapply(
  tmplevels,
  getvarname
)
tmplabels<-c(
  "GDP",
  "DCAP",
  "LAND"
)
jkdf$iv<-factor(
  jkdf$iv,
  levels=tmplevels,
  labels=tmplabels
)

#dv
tmp<-sapply(
  jkdf$dv,
  getvarorder
)
tmplevels<-names(tmp) %>%
  unique
tmplabels<-sapply(
  tmplevels,
  getvarname
)
jkdf$dv<-factor(
  jkdf$dv,
  levels=tmplevels,
  labels=tmplabels
)

#SUBSET!

#COWCODE
tmp<-jkdf$omitter=="cowcode.num" &
  jkdf$iv%in%c(
    "DCAP",
    "LAND"
  )
plotdf<-jkdf[tmp,]

#omitted
tmplevels<-unique(plotdf$omitted)
tmp<-str_extract(tmplevels,"[0-9]+") %>%
  as.numeric
tmplevels<-tmplevels[order(tmp)]
tmplabels<-sapply(
  tmplevels,
  function(x) {
    if(x=="none") {
      x<-"None Omitted"
    } else {
      #x<-42
      tmp<-demdfs$one$cowcode.num==x &
        !is.na(demdfs$one$countryname)
      tail(demdfs$one$countryname[tmp],1)
    }
  }
)
plotdf$omitted<-factor(
  plotdf$omitted,
  levels=tmplevels,
  labels=tmplabels
)

#tile
g.tmp<-ggplot(
  plotdf,
  aes(
    x=iv,
    y=omitted,
    fill=pval.fill
  )
) +
  geom_tile() +
  facet_wrap(
    ~ dv
  ) +
  scale_fill_manual(
    name="",
    values=tmpcolors,
    labels=fill.labels,
    drop=F
  ) +
  xlab("") +
  ylab("Country Omitted\n") +
  theme_bw(
    base_family="CM Roman",
    base_size=14
  )
tmpname<-"fig_jktile.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8,
  height=12
)

#########################################################
#########################################################

#BINMODS OUTPUT
setwd(filesdir); dir()
load('robmods_bin.RData')

#ESTS W/ STAT SIG
tmp<-binfinaldf$disp &
  binfinaldf$iv%in%c(
    "highcapratio",
    "highcapratio2",
    "landlords"
  )
plotdf<-binfinaldf[tmp,]

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

plotdf$specname<-""
tmp<-plotdf$ivspec=="preferred" & 
  plotdf$dv=="ttd"
plotdf$specname[tmp]<-"Preferred"
tmp<-plotdf$ivspec=="extra" & 
  plotdf$dv=="ttd"
plotdf$specname[tmp]<-"+ DCAP Sq."
tmp<-plotdf$ivspec=="preferred" & 
  plotdf$dv=="tta"
plotdf$specname[tmp]<-"- DCAP Sq."
tmp<-plotdf$ivspec=="extra" & 
  plotdf$dv=="tta"
plotdf$specname[tmp]<-"Preferred"

plotdf$specname<-factor(
  plotdf$specname,
  levels=c(
    "Preferred",
    "+ DCAP Sq.",
    "- DCAP Sq."
  )
)

plotdf$dv<-factor(
  plotdf$dv,
  levels=c(
    "ttd",
    "tta"
  ),
  labels=c(
    "Transition",
    "Rollback"
  )
)

# 
# 
# plotdf$ivspec<-factor(
#   plotdf$ivspec,
#   levels=c(
#     "preferred",
#     "extra"
#   ),
#   labels=c(
#     "Preferred",
#     "+ DCAP Sq."
#   )
# )

tmp<-sapply(
  unique(plotdf$iv),
  getvarorder
) %>% sort
tmplevels<-names(tmp)
tmplabels<-sapply(
  tmplevels,
  getvarname
)
plotdf$iv<-factor(
  plotdf$iv,
  rev(tmplevels),
  rev(tmplabels)
)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=specname,
    y=iv,
    fill=pval.fill
  )
) +
  geom_tile() +
  facet_wrap(
    ~ dv,
    scales="free"
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
tmpname<-"fig_binmodests.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=12,
  height=4
)

#PREDICTED PROBS
tmp<-phatdf$disp
plotdf<-phatdf[tmp,]

plotdf$phatnum<-prettyNum(
  plotdf$p10,
  digits=2,
  drop0trailing=F
)

plotdf$dv<-factor(
  plotdf$dv,
  levels=c(
    "ttd",
    "tta"
  ),
  labels=c(
    "Transition",
    "Rollback"
  )
)

plotdf$ivspec<-factor(
  plotdf$ivspec,
  levels=c(
    "preferred",
    "extra"
  ),
  labels=c(
    "Preferred",
    "+ DCAP Sq."
  )
)

g.tmp<-ggplot(
  data=plotdf,
  aes(
    x=L.landlords,
    y=L.highcapratio,
    fill=p10#,
    #label=phatnum
  )
) +
  geom_tile() +
  #geom_text(
  #  color="white",
  #  family="CM Roman"
  #) +
  xlab("\nLandlord Power") +
  ylab("Disruptive Capacity\n") +
  scale_fill_continuous(
    name="P(Outcome)",
    low=c("darkblue"),
    high=c("red")
  ) +
  facet_wrap(~ dv + ivspec) +
  theme_bw(
    base_family="CM Roman",
    base_size=14
  )

tmpname<-"fig_binmodpreds.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=12/1.25,
  height=8/1.25
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
  Sys.sleep(0.5)
}
