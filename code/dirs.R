codedir<-getwd()
setwd('..')
homedir<-getwd()
###
#these dirs hold inputs
datadir<-file.path(
  homedir,
  "data"
)
metadir<-file.path(
  homedir,
  "meta"
)
#these dirs hold outputs
filesdir<-file.path(
  homedir,
  "files"
)
outputdir<-file.path(
  homedir,
  "output"
)
#if they haven't already been created,
#make sure these are created
dir.create(filesdir,showWarnings=F)
dir.create(outputdir,showWarnings=F)
setwd(homedir)
#also set the gsdir
#this is the location of your 
#ghostscript executable
#I have written it here so it is 
#flexible wrt the version #, 
#but that is not necessary
gsdir<-file.path(
  "c:",
  "Program Files",
  "gs"
)
gsdir_full<-file.path(
  gsdir,
  dir(gsdir), 
  "bin",
  "gswin64c.exe"
)
setwd(codedir)