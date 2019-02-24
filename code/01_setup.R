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

#EDITS?
# setwd(filesdir)
# demdf<-read.csv(
#   'demdf.csv',
#   stringsAsFactors=F
# )
# 
# #remove this, which duplicates
# tmp<-demdf$countryname=="Cyprus: Greek Sector"
# demdf<-demdf[!tmp,]
# 
# #replace countryname
# tmp<-demdf$countryname=="South Africa (Early)"
# demdf$countryname[tmp]<-"South Africa"
# tmp<-demdf$countryname=="Egypt (Early)"
# demdf$countryname[tmp]<-"Egypt"
# tmp<-demdf$countryname=="Croatia (Early)"
# demdf$countryname[tmp]<-"Croatia"
# 
# #get names
# cowsoutput<-getcows(
#   demdf$countryname,
#   demdf$year
# )
# cowsdf<-cowsoutput$cows
# cowsinfo<-cowsoutput$info
# 
# #add the new id variables and rename old
# names(demdf)[names(demdf)=="countryname"]<-"countryname.old"
# demdf$cowcode<-cowsdf$cowcodes
# demdf$countryname<-cowsdf$newnames
# 
# #NA countries
# if (sum(length(unique(cowsinfo$na$originalnames)))>0)
#   stop("NA cowcodes") #nothing NA, as expected
# 
# #Invalid Countries
# #check for invalid cowcodes
# unique(cowsinfo$invalid.names$originalnames)
# 
# #drop these invalid cowcodes
# invalidobs<-str_detect(demdf$cowcode,"INVALID")
# demdf$countryname.old[invalidobs] %>% unique
# demdf<-demdf[!invalidobs,]
# 
# #Out-of-range countries
# range.countries<-unique(cowsinfo$outofrange$originalnames)
# checkrange<-function(x) {
#   outofrange<-demdf$year[demdf$countryname.old==x & str_detect(demdf$cowcode,"OUT")]
#   outofrangeyears<-paste0(c(min(outofrange),max(outofrange)),collapse="-")
#   inrange<-demdf$year[demdf$countryname.old==x & (!str_detect(demdf$cowcode,"OUT"))]
#   inrangeyears<-paste0(c(min(inrange),max(inrange)),collapse="-")
#   return(c(outofrangeyears,inrangeyears))
# }
# range.matrix<-t(sapply(range.countries,checkrange))
# range.matrix
# 
# #rest can be dropped
# notrangeobs<-str_detect(
#   demdf$cowcode,
#   "OUT OF RANGE"
# )
# sum(notrangeobs)
# demdf<-demdf[!notrangeobs,]
# 
# #Countries in parts
# unique(cowsinfo$parts$newnames)
# 
# #Colonized countries
# unique(cowsinfo$occupations$newnames)
# 
# #Countries in spells
# unique(cowsinfo$spells$newnames)
# 
# #regenerate cownums
# demdf$cowcode.num<-getcownums(demdf$cowcode)
# 
# #any dups?
# indvars<-c("cowcode","cowcode.num")
# for(var in indvars) {
#   print("####")
#   print(var)
#   tmpind<-paste0(
#     demdf[[var]],
#     "-",
#     demdf$year
#   )
#   tmptab<-table(tmpind)
#   tmp<-tmptab>1
#   if(sum(tmp)>0) {
#     print(tmptab[tmp])
#     stop()
#   }
# }
# 
# setwd(filesdir); dir()
# write.csv(
#   demdf,
#   'demdf_st.csv',
#   row.names=F
# )

#LOAD DATA
setwd(filesdir)
demdf<-read.csv(
  'demdf_st.csv',
  stringsAsFactors=F
)

#########################################################
#########################################################

#MERGE

#v-dem
#add v-dem dataset in, w/ extra DV's
tmpdir<-file.path(
  "c:",
  "users",
  "adaner",
  "dropbox",
  "data",
  "starting datasets",
  "V-Dem, 6.2"
)
setwd(tmpdir); dir()
tmpdf<-read.csv(
  'vdem.csv',
  stringsAsFactors=F
)

#how many countries? what span?
# tmpnums<-getcownums(tmpdf$cowcode)
# unique(tmpnums) %>% length
# range(tmpdf$year)
# sum(!is.na(tmpdf$v2x_polyarchy))
# sum(!is.na(demdf$polity2.resc))

#v-dem vars are scaled between 0 and 1
#coefficients easier to read if scaled to 100
tmp<-str_detect(names(tmpdf),"v2")
vdemvars<-names(tmpdf)[tmp]
for(vdemvar in vdemvars)
  tmpdf[[vdemvar]]<-100 * tmpdf[[vdemvar]]

tmpdf$X<-tmpdf$countryname.old<-NULL
tmp<-names(tmpdf)%in%names(demdf) &
  !names(tmpdf)%in%c("countryname","cowcode","ccode","year")
if(sum(tmp)>0) {
  print(names(tmpdf)[tmp])
  stop("Rename necessary.")
}

#make merge happen, on cowcode basis
demdf<-merge(
  demdf,
  tmpdf,
  by=c(
    "cowcode",
    "year"
  ),
  all=T
)

#fix countryname and cowcode
names(demdf)[str_detect(names(demdf),"\\.x$|\\.y$")]
demdf$countryname<-ifelse(
  is.na(demdf$countryname.x),
  demdf$countryname.y,
  demdf$countryname.x
)
demdf$countryname.x<-demdf$countryname.y<-NULL

#na codes?
sum(is.na(demdf$ccode))
sum(is.na(demdf$year))
sum(is.na(demdf$cowcode))
sum(is.na(demdf$countryname))

######################

#TRADE/GDP
tmpdir<-file.path(
  "c:",
  "users",
  "adaner",
  "dropbox",
  "data",
  "starting datasets",
  "Trade Ratios, Usmani"
)
setwd(tmpdir); dir()
tmpdf<-read.csv(
  "tradegdp_FINAL.csv",
  stringsAsFactors=F
)

#there is a Macao mistake, here
tmp<-tmpdf$cowcode==2004.5 & tmpdf$year==1999
tmpdf$countryname[tmp]<-getcountryname(2004)
tmpdf$cowcode[tmp]<-2004

tmpdf$X<-tmpdf$countryname.old<-NULL
tmp<-names(tmpdf)%in%names(demdf) &
  !names(tmpdf)%in%c("countryname","cowcode","year")
if(sum(tmp)>0)
  stop("Rename necessary.")

demdf<-merge(
  demdf,
  tmpdf,
  by=c(
    "cowcode",
    "year"
  ),
  all=T
)

names(demdf)[str_detect(names(demdf),"\\.x$|\\.y$")]
demdf$countryname<-ifelse(
  is.na(demdf$countryname.x),
  demdf$countryname.y,
  demdf$countryname.x
)
demdf$countryname.x<-demdf$countryname.y<-NULL

######################

#COLONIAL RULER
tmpdir<-file.path(
  "c:",
  "users",
  "adaner",
  "dropbox",
  "data",
  "starting datasets",
  "Colonial History, Usmani"
)
setwd(tmpdir); dir()
tmpdf<-read.csv(
  "colonialhist.csv",
  stringsAsFactors=F
)

tmpdf$X<-tmpdf$countryname<-NULL
tmp<-names(tmpdf)%in%names(demdf) &
  !names(tmpdf)%in%c("countryname","cowcode","year")
if(sum(tmp)>0)
  stop("Rename necessary.")

tmp<-tmpdf$cowcode%in%demdf$cowcode
tmpdf$cowcode[!tmp] %>% unique
tmp<-demdf$cowcode%in%tmpdf$cowcode
demdf$cowcode[!tmp] %>% unique

demdf<-merge(
  demdf,
  tmpdf,
  by=c(
    "cowcode"
  ),
  all.x=T
)

#########################################################
#########################################################

#WARS
tmpdir<-file.path(
  "c:",
  "users",
  "adaner",
  "dropbox",
  "data",
  "starting datasets",
  "Wars, Correlates of War"
)
setwd(tmpdir); dir()
tmpdf<-read.csv(
  "warsdf.csv",
  stringsAsFactors = F
)

tmpdf$X<-tmpdf$countryname<-NULL
tmp<-names(tmpdf)%in%names(demdf) &
  !names(tmpdf)%in%c("countryname","cowcode","year")
if(sum(tmp)>0)
  stop("Rename necessary.")

demdf<-merge(
  demdf,
  tmpdf,
  by=c(
    "cowcode",
    "year"
  ),
  all=T
)

#for countries which didn't match,
#i make warvars 0; this is safe, 
#on assmpution that COW warslist
#includes all wars
warvars<-c(
  "war",
  "wars_total",
  "wars_interstate",
  "wars_other",
  "deaths_total",
  "deaths_interstate"
)
misswars<-is.na(demdf$war)
demdf[misswars,warvars]<-0

#########################################################
#########################################################

#MANU VAL ADD

tmpdir<-file.path(
  "c:",
  "users",
  "adaner",
  "dropbox",
  "data",
  "starting datasets",
  "Manu share of GDP, World Bank"
)
setwd(tmpdir); dir()
tmpdf<-read.csv(
  "manu_valadded.csv",
  stringsAsFactors = F
)

tmpdf$X<-tmpdf$countryname<-NULL
tmp<-names(tmpdf)%in%names(demdf) &
  !names(tmpdf)%in%c("countryname","cowcode","year")
if(sum(tmp)>0)
  stop("Rename necessary.")

demdf<-merge(
  demdf,
  tmpdf,
  by=c(
    "cowcode",
    "year"
  ),
  all=T
)

#########################################################
#########################################################

#GET/CREATE VARS

#check dups
tmpind<-paste0(
  demdf$cowcode,
  "-",
  demdf$year
)
tmptab<-table(tmpind)
tmp<-tmptab>1
if(sum(tmp)>0) {
  print(tmptab[tmp])
  stop()
}

#a little bit of cleaning
#NA cowcodes?
tmp<-is.na(demdf$cowcode)
if(sum(tmp)>0)
  stop()
#NA years? 
tmp<-is.na(demdf$year)
if(sum(tmp)>0)
  stop()

#regenerate cownums
demdf$cowcode.num<-getcownums(demdf$cowcode)

#any dups?
indvars<-c("cowcode","cowcode.num")
for(var in indvars) {
  print("####")
  print(var)
  tmpind<-paste0(
    demdf[[var]],
    "-",
    demdf$year
  )
  tmptab<-table(tmpind)
  tmp<-tmptab>1
  if(sum(tmp)>0) {
    print(tmptab[tmp])
    stop()
  }
}

#order
roworder<-order(
  demdf$cowcode.num,
  demdf$year
)
demdf<-demdf[roworder,]

#rename polity2
names(demdf)[names(demdf)=="polity2.resc"]<-"polity2"

#dvs multiplied by 100
demdf$polity2 <- 100 * demdf$polity2

#regenrate region
demdf$region<-sapply(
  demdf$cowcode,
  getregionname2
)

#decade dummies
demdf$decade<-10*round((demdf$year-5)/10)

#regenerate advanced/developing
demdf$advanced<-F
tmp<-demdf$region=="N. America and W. Europe" |
  demdf$countryname=="Japan"
demdf$advanced[tmp]<-T

#landlords has three strange gaps 
#in Austria, Lebanon, and Myanmar
#i interpolate across
demdf$landlords<-tapply(
  demdf$landlords,
  demdf$cowcode.num,
  na.approx,na.rm=F
) %>% unlist
demdf$lnlandlords<-log(
  demdf$landlords + 1
)

#regnerate hcapalts
demdf$highcaplf<- 100 *
  demdf$highcapacity/demdf$lftotal
summary(demdf$highcaplf)
demdf$highcap_pcap<-100 * 
  demdf$highcapacity/demdf$totalpop
summary(demdf$highcap_pcap)
demdf$highcapratio.gov<- 100 * 
  demdf$highcapacity.gov/demdf$workingagepop
summary(demdf$highcapratio.gov)
cor(
  demdf$highcapratio,
  demdf$highcapratio.gov,
  use="complete.obs"
)

#highcapratio2
demdf$highcapratio2<-
  demdf$highcapratio * 
  demdf$highcapratio

#land2
demdf$landlords2<-
  demdf$landlords * 
  demdf$landlords

#gdp2
demdf$lngdpcap2<-
  demdf$lngdpcap * 
  demdf$lngdpcap


#spread colruler
roworder<-order(
  demdf$cowcode.num,
  demdf$year
)
demdf<-demdf[roworder,]

demdf$colrulername<-by(
  demdf,
  demdf$cowcode.num,
  #x<-demdf$colrulername[demdf$cowcode.num=="770"]
  function(df) {
    #print(df$cowcode.num %>% unique)
    #df<-demdf[demdf$cowcode.num=="365-2",]
    x<-df$colrulername
    print(unique(x))
    tmp<-!is.na(x)
    if(sum(tmp)>0) {
      x[!tmp]<-tail(unique(x[tmp]),1) #take most recent
    }
    x
  }
) %>% unlist

#make colruler numeric
demdf$colruler<-factor(
  demdf$colrulername
) %>% as.numeric

#which is which
table(
  demdf$colruler,
  demdf$colrulername
)

#make region numeric
tmplevels<-c(
  "Eastern Europe",
  "Latin America",
  "MENA",
  "Sub-Saharan Africa",
  "N. America and W. Europe",
  "E. Asia",
  "South E. Asia",
  "South Asia",
  "Oceania",
  "Caribbean",
  "MISSING REGION"
)
demdf$region_num<-factor(
  demdf$region,
  tmplevels,
  tmplevels
) %>% as.numeric

#loop through var and cow
#generate regional avg by year
#only possible for those assigned to regions
dvs<-varsdf$varname[varsdf$type%in%c("robdv","demdv")]
tmp<-demdf$region!="MISSING REGION"
mycows<-demdf$cowcode.num[tmp] %>% unique
loopdf<-expand.grid(
  dv=dvs,
  cowcode.num=mycows,
  stringsAsFactors=F
)
tmpseq.i<-1:nrow(loopdf)

regavgsdf<-lapply(tmpseq.i,function(i) {
  #i<-1
  #i<-which(loopdf$cowcode.num==740)[1]
  ###
  print(
    paste(i,"of",length(tmpseq.i))
  )
  thisdv<-loopdf$dv[i]
  thiscow<-loopdf$cowcode.num[i]
  tmp<-demdf$cowcode.num==thiscow &
    demdf$region!="MISSING REGION" 
  thisregion<-unique(demdf$region[tmp])
  #####
  #we should have just one region
  if(length(thisregion)>1) 
    stop()
  #####
  #regional avg, excluding this country
  tmp<-demdf$cowcode.num!=thiscow & 
    demdf$region==thisregion 
  tmpdf<-demdf[tmp,c("year",thisdv)]
  tmpsum<-tapply(
    tmpdf[[thisdv]],
    tmpdf$year,
    mean,na.rm=T
  )
  returndf<-data.frame(
    cowcode.num=thiscow,
    year=names(tmpsum),
    dv=paste0(thisdv,"_regavg"),
    val=tmpsum
  )
  #interpolate over missing years
  returndf$val<-na.approx(
    returndf$val,
    na.rm=F
  )
  #return
  returndf
}) %>% rbind.fill

#make this wide
require(tidyr)
regavgsdf<-spread(
  regavgsdf,
  dv,
  val
)

#merge this in
demdf<-merge(
  demdf,
  regavgsdf,
  all.x=T,
  by=c(
    "cowcode.num",
    "year"
  )
)

#########################################################
#########################################################

#TRIM

#trim to keepervars
colorder<-varsdf$varname
tmp<-colorder%in%names(demdf)
if(sum(!tmp)>0) {
  print(colorder[!tmp])
  stop()
}

#order the df
roworder<-order(
  demdf$cowcode.num,
  demdf$year
)
demdf<-demdf[roworder,colorder]

#check order
roworder<-order(
  demdf$cowcode.num,
  demdf$year
)
tmp<-identical(
  demdf,
  demdf[roworder,]
)
if(!tmp) {
  stop()
}

#########################################################
#########################################################

#DATASETS OF DIFFERENT LENGTHS

#check dups for both index vars
indvars<-c("cowcode","cowcode.num")
for(var in indvars) {
  print("####")
  print(var)
  tmpind<-paste0(
    demdf[[var]],
    "-",
    demdf$year
  )
  tmptab<-table(tmpind)
  tmp<-tmptab>1
  if(sum(tmp)>0) {
    print(tmptab[tmp])
    stop()
  }
}

demdf5<-demdf[demdf$year%%5==2,]
demdf10<-demdf[demdf$year%%10==2,]
demdf15<-demdf[demdf$year%%15==2,]
demdf20<-demdf[demdf$year%%20==2,]
demdfs<-list(
  one=demdf,
  five=demdf5,
  ten=demdf10,
  fifteen=demdf15,
  twenty=demdf20
)

#########################################################
#########################################################

#LAGS AND DIFFS

#check order
roworder<-order(
  demdf$cowcode.num,
  demdf$year
)
tmp<-identical(
  demdf,
  demdf[roworder,]
)
if(!tmp) {
  stop('')
}

#get all keyvars
tmp<-varsdf$type!="group"
keyvars<-varsdf$varname[tmp]

#add first differences of all vars
tmpseq.i<-seq_along(demdfs)
oldnames<-names(demdfs)
demdfs<-lapply(tmpseq.i,function(i) {
  #i<-1
  print("######")
  print(names(demdfs)[i])
  tmpdf<-demdfs[[i]]
  for(var in keyvars) {
    #var<-"polity2"
    print(var)
    newname<-paste0("D.",var) #first diff
    tmpdf[[newname]]<-by(
      tmpdf,
      tmpdf$cowcode.num,
      function(df) {
        #df<-tmpdf[tmpdf$cowcode==100,]
        y<-c(NA,diff(df[[var]],1))
        if(length(y)!=nrow(df)) {
          print(unique(df$cowcode.num))
          stop()
        }
        y
      }) %>% unlist
  }
  tmpdf
})
names(demdfs)<-oldnames

#update the list of vars
keyvars<-c(keyvars,paste0("D.",keyvars)) %>% 
  unique
keyvars<-keyvars[keyvars%in%names(demdfs[[1]])]

#check order
roworder<-order(
  demdf$cowcode.num,
  demdf$year
)
tmp<-identical(
  demdf,
  demdf[roworder,]
)
if(!tmp) {
  stop()
}

#generate lags 
#loop through each of these vars
#and add four lags to the dataset
tmpseq.i<-seq_along(demdfs)
oldnames<-names(demdfs)
demdfs<-lapply(tmpseq.i,function(i) {
  #i<-1
  print("######")
  print(names(demdfs)[i])
  tmpdf<-demdfs[[i]]
  #get lags and first diffs
  for(var in keyvars) {
    print(var)
    tmpdf[[paste0("L.",var)]]<-
      lag.apply(tmpdf[[var]],tmpdf$cowcode.num,1)
    tmpdf[[paste0("L2.",var)]]<-
      lag.apply(tmpdf[[var]],tmpdf$cowcode.num,2)
    tmpdf[[paste0("L3.",var)]]<-
      lag.apply(tmpdf[[var]],tmpdf$cowcode.num,3)
    tmpdf[[paste0("L4.",var)]]<-
      lag.apply(tmpdf[[var]],tmpdf$cowcode.num,4)
  }
  tmpdf
})
names(demdfs)<-oldnames

#########################################################
#########################################################

#DUMMIES MARKING DEMOCRACY/AUTH

#now, for each of these datasets we need to generate dummies
#noting which observations should form part of our analysis of transitions,
#and which should form part of our analysis of consolidations

#easy to do, now that we have lags of the democracy variable
#for transition, keep all instances where
#democracy is 0, or where democracy is 1 but lag democracy is 0

#basic idea is that lagged variables of interest should be higher
#in these 1/0 years than in 0 years..

#for each dataset we will make six dummies (maybe, a list of dummies)
#transitions/consolidation for the composite democracy variable,
#transitions/consolidation for the polity variable
#transitions/consolidation for the exacting version of the polity variable

#we also need duration dependence terms
#these mark time elapsed since the last event
#in transitions analysis, this is time since last dem
#in consol analysis, this is time since last relapse

demdfs<-lapply(demdfs,function(thisdf) {
  
  #thisdf<-demdfs[[1]]
  ###
  
  democracy<-thisdf$democracy==1 & 
    !is.na(thisdf$democracy)
  dictatorship<-thisdf$democracy==0 & 
    !is.na(thisdf$democracy)
  #mark transition years: 
  #dictatorship, or recent democracy
  thisdf$trans <- F
  thisdf$trans <- (
    dictatorship & 
      !is.na(dictatorship)
  ) | ( #OR
    thisdf$L.democracy==0 & 
      thisdf$democracy==1 &
      !is.na(thisdf$L.democracy) &
      !is.na(thisdf$democracy)
  )
  #mark consolidation years: 
  #democracy, or recent dictatorship
  thisdf$consol <- F
  thisdf$consol <- (
    democracy & 
      !is.na(democracy)
  ) | ( #OR
    thisdf$L.democracy==1 & 
      thisdf$democracy==0 &
      !is.na(thisdf$L.democracy) &
      !is.na(thisdf$democracy)
  )
  
  #######
  #######
  
  thisdf
  
})

#########################################################
#########################################################

#INSPECT DUPS

#check dups
tmpseq.i<-seq_along(demdfs)
for (i in tmpseq.i) {
  print("####")
  print(i)
  df<-demdfs[[i]]
  indvars<-c("cowcode","cowcode.num")
  for(var in indvars) {
    print(var)
    tmpind<-paste0(
      df[[var]],
      "-",
      df$year
    )
    tmptab<-table(tmpind)
    tmp<-tmptab>1
    if(sum(tmp)>0) {
      print(tmptab[tmp])
      stop()
    }
  }
}

#########################################################
#########################################################

#save out
setwd(datadir)
saveRDS(
  demdfs,
  "demdfs.RDS"
)