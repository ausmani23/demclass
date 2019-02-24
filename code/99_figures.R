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
setwd(codedir)
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
source('footnotes.R') #fn's
setwd(homedir)

#########################################################
#########################################################

#this function does the work
writefigure<-function(
  texfile,
  imagefile,
  label,
  caption,
  note="",
  width="4.5in"
) 
{
  opening<-"\\begin{figure} \n \\centering \n"
  caption<-paste0("\\caption{",caption,"} \n")
  label<-paste0("\\label{",label,"} \n")
  note<-paste0("\\floatfoot{",note,"} \n")
  if(str_count(imagefile,"\\.")>1) {
    imagefile<-
      str_replace(
        imagefile,
        "^(.*)(\\.pdf)$",
        "{\\1}\\2"
      ) #mask other periods from LaTex
  }
  graphic<-paste0(
    "\\includegraphics[width=",
    width,
    ",keepaspectratio]{"
    ,imagefile,
    "} \n"
  )
  closing<-"\\end{figure} \n"
  content<-ifelse(
    note=="",
    paste(caption,label,graphic,collapse=" "),
    paste(caption,label,note,graphic,collapse="")
  )
  tmpoutput<-paste(opening,content,closing,collapse=" ")
  write(
    tmpoutput,
    file=texfile,
    append=T
  )
}

#########################################################
#########################################################

#OUTPUT TEX FILES

require(xtable)
#file header
setwd(metadir); dir()
myheader<-readLines("header.txt") %>%
  paste0(collapse=" \n ") %>%
  str_replace("0.8in","1.0in")
#loop through and generate all four
setwd(outputdir)
tmpdf<-expand.grid(
  loc=c("main","app"),
  type=c("bit","full"),
  stringsAsFactors=F
)
tmpseq<-1:nrow(tmpdf) 
for(i in tmpseq) {
  #i<-2
  print("###")
  print(i)
  #get params
  thisrow<-tmpdf[i,]
  thisloc<-thisrow$loc
  thistype<-thisrow$type
  isfull<-thistype=="full"
  #get filename
  filename<-paste0(
    thisloc,
    "figures",
    ifelse(isfull,"_full",""),
    ".tex"
  )
  #write header
  opening<-ifelse(isfull,myheader,"")
  write(opening,file=filename)
  #loop through figs from main, order
  setwd(metadir)
  figinfo<-read.csv(
    "figures.csv",
    colClasses="character"
  )
  tmp<-!is.na(figinfo$order)
  tmp<-tmp & figinfo$loc==thisloc
  figinfo<-figinfo[tmp,]
  figinfo$order<-as.numeric(figinfo$order)
  roworder<-order(figinfo$order)
  figinfo<-figinfo[roworder,]
  #loop through
  setwd(outputdir)
  fig.sequence<-1:nrow(figinfo)
  for(j in fig.sequence) {
    #i<-1
    print(j)
    thisrow<-figinfo[j,]    
    mywidth<-ifelse(
      is.na(thisrow$width),
      "4.5in",
      thisrow$width
      )
    #write out using writefigure()
    writefigure(
      texfile=filename,
      imagefile=thisrow$imagefile,
      label=thisrow$label,
      caption=thisrow$caption,
      note=fns[[thisrow$label]],
      width=mywidth
    )
  }
  closing<-ifelse(isfull,"\\end{document}","")
  write(closing,file=filename,append=T)
}

#########################################################
#########################################################

#UPLOAD
#make copies of all images in uplaod folder, 
#for quick upload
tmpdir<-file.path(
  outputdir,
  "upload"
)
#create if it doesn't exist
dir.create(tmpdir,showWarnings=F)

setwd(metadir)
figinfo<-read.csv(
  "figures.csv",
  stringsAsFactors=F
)  
tmp<-!is.na(figinfo$order)
figinfo<-figinfo[tmp,]
finalfiles<-figinfo$imagefile
setwd(outputdir)
file.copy(finalfiles,to=tmpdir,overwrite=T)