old.dir<-getwd()
setwd(metadir)
footnotes<-readLines("footnotes.txt")
tmp<-str_detect(footnotes,"###")
st.labels<-which(tmp)
end.labels<-st.labels-1
end.labels<-end.labels[end.labels!=0]
end.labels<-c(end.labels,length(footnotes))
looper<-matrix(
  sort(c(st.labels,end.labels)),
  byrow=T,
  ncol=2
)
tmp.seq<-1:nrow(looper)
tmplist<-lapply(tmp.seq,function(i) {
  #i<-1
  thisrow<-looper[i,]
  labpos<-thisrow[1]
  fnpos<-(labpos+1):thisrow[2]
  tmplab<-footnotes[labpos] %>%
    str_replace("^###","")
  tmpfn<-footnotes[fnpos] %>%
    paste0(collapse=" \n ")
  #return
  list(
    lab=tmplab,
    fn=tmpfn
  )
})
fns<-lapply(tmplist,function(x) x$fn)
names(fns)<-sapply(tmplist,function(x) x$lab)
setwd(old.dir)