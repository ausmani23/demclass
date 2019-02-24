require(stringr)
require(rprojroot)

#time this process
rootdir<-find_root(
  criterion=has_file('_demclass.RProj')
)
codedir<-file.path(rootdir,"code")
setwd(codedir); dir()
source('dirs.R')

#for timing
setwd(filesdir)
t0 <- proc.time()
write(t0[3],"t0.txt")

#run through
setwd(codedir)
tmp<-str_detect(dir(),"[0-9]{2}\\_") &
  !str_detect(dir(),"!!|00|01|02")
myfiles<-dir()[tmp]

#subset these?
myfiles<-myfiles[16:19]

for(myfile in myfiles) {
  print("######")
  print("Running: ")
  print(myfile)
  rootdir<-find_root(
    criterion=has_file('_demclass.RProj')
  )
  codedir<-file.path(rootdir,"code")
  setwd(codedir)
  source(myfile)
}

##########

#print final time
rootdir<-find_root(
  criterion=has_file('_demclass.RProj')
)
codedir<-file.path(rootdir,"code")
setwd(codedir)
source('dirs.R')
setwd(filesdir)
tf <- proc.time()
t0<-readLines("t0.txt") %>%
  as.numeric
print(
  paste(
    round((tf[3] - t0),3),
    "seconds"
  )
) #seconds
print(
  paste(
    round((tf[3] - t0) / 60,3),
    "minutes"
  )
) #minutes
print(
  paste(
    round((tf[3] - t0) / (60^2),3),
    "hours"
  )
) #hours
#delete to.txt
file.remove("t0.txt")
