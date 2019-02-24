######
#make sure you have set the working directory
#to the directory containing all the .R scripts
######
tmp<-str_detect(dir(),"[0-9]{2}\\_") &
  dir()!="00_runall.R"
files<-dir()
tmpfiles<-lapply(files,readLines)
packs<-lapply(tmpfiles,function(x) {
  #x<-tmpfiles[[1]]
  tmp<-str_detect(x,"require\\(")
  tmp_packs<-str_replace(x[tmp],"require\\(([A-z\\.0-9]+)\\)","\\1")
  str_replace(
    tmp_packs,"^\\s+|\\s+$",""
  )
}) %>% unlist %>% unique %>% sort
packs
#lapply(packs,install.packages)