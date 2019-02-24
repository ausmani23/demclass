#stared with this directory
olddir<-getwd()

########################################################################
########################################################################

#read in varnames df

setwd(metadir); dir()
varsdf<-read.csv(
  "vars.csv",
  stringsAsFactors=F
)

regionsdf<-read.csv(
  'regions_EDITED.csv',
  stringsAsFactors=F
)


########################################################################
########################################################################

#this is a generic conversion function
#given value, from/to, and a df
###(should be written to output 2 possiiblities, etc.)

getcode<-function(x,from,to,df,appx=T,allowdups=F) {
  
  # x<-"McCollum Amendment";
  # from<-"shortname";
  # to<-"amendtitle";
  # df<-plotdf
  # x<-"Labor Intensity"
  # from<-"propername"
  # to<-"order"
  # df<-varsdf
  
  ##############
  
  #get the row
  #if you pass me NA
  #return NA
  if(is.na(x)) {
    ###can't do anything
    y<-NA
  } else {
    ###else we can do something
    #convert a;; to lower, 
    #in case these are character matches
    match.from<-tolower(df[[from]])
    x<-tolower(x)
    #get matching rows
    tmp<-match.from==x
    if(sum(tmp)==1) {
      #if just one row, easy
      y<-df[[to]][tmp]
    } else if(sum(tmp)==0) {
      #if nothing matched,
      #this is probably NA
      y<-NA
      #if approx is on
      if(appx) {
        #but we can try converting 
        #the matching column to numeric
        #for appx matches of numbers
        tmpnum<-suppressWarnings(
          as.numeric(match.from)
        )
        tmp<-tmpnum==x & !is.na(tmpnum)
        if(sum(tmp)==1) 
          y<-df[[to]][tmp]
      }
    } else if(sum(tmp)>1) {
      #if more than 1 matches,
      #still return if they're all the same
      y<-df[[to]][tmp] %>%
        unique
      if(length(y)>1) {
        #if more than one matches
        #check if allow dups is on,
        #then we can do something
        #otherwise, don't match more than 1
        if(allowdups) {
          #return a vector
          #this can't be put in df w/o examination
          #because it won't be right length
          y<-df[[to]][tmp]
        } else {
          y<-NA #set back to NA
        }
      }
    }
  }
  
  ##############
  
  return(y)
}

########################################################################
########################################################################

getvarcode<-function(x,append=T,...) {
  #x<-"Derrick A"
  
  #######
  
  #check for std suffixes or prefixes
  
  prefsufs<-c(
    "L[0-9]?.",
    "D[0-9]?."
  )
  prefsufs.propername<-c(
    "Lag",
    "Diff"
  )
  prefsufs.regex<-paste0("(",paste0(prefsufs,collapse="|"),")")
  tmp<-str_detect(x,prefsufs.regex)
  if(tmp) {
    #extract suffix
    x.new<-str_replace_all(x,prefsufs.regex,"")
    y<-getcode(x.new,...)
    #y<-getcode(x.new,"varname","propername",varsdf)
    if(append) {
      myprefsufs<-str_extract_all(x,prefsufs.regex)[[1]]
      tmp<-lapply(myprefsufs,function(z) str_detect(z,prefsufs))
      myappender<-prefsufs.propername[sapply(tmp,which)] %>%
        paste(collapse=", ")
      y<-paste0(y," (",myappender,")")
    } 
  } else {
    y<-getcode(x,...) #nothing to do if no suffix
  }
  y
}

#additional wrappers
getvarname<-function(x) {
  getvarcode(x,append=F,"varname","propername",varsdf)
}
getvarorder<-function(x) {
  getvarcode(x,append=F,"varname","order",varsdf)
}
getvartype<-function(x) {
  getvarcode(x,append=F,"varname","type",varsdf)
}
getvar<-function(x) {
  getvarcode(x,append=F,"varname","varname",varsdf)
}

getregion<-function(x) {
  getvarcode(x,append=F,"cowcode","region",regionsdf)
}
getregionname2<-function(x) {
  getregion(x) %>% getregionname
}
getregion('2')
getregionname2('2')

########################################################################
########################################################################

summarize.distribution2<-function(ests.distribution) {
  #ests.distribution<-lrm.distribution
  #get quantiles
  quantiles<-quantile(
    ests.distribution,
    c(
      0.01,
      0.025,
      0.05,
      0.5,
      0.95,
      0.975,
      0.99
    )
  )
  #return mu, mu.min, mu.max
  mu<-quantiles["50%"]
  mu.min<-quantiles["2.5%"]
  mu.max<-quantiles["97.5%"]
  #and also a pval classification
  if(mu>0) {
    if(quantiles["1%"]>0) {
      pval.class<-'at alpha=0.01'
    } else if(quantiles["2.5%"]>0) {
      pval.class<-'at alpha=0.05'
    } else if(quantiles["5%"]>0) {
      pval.class<-'at alpha=0.10'
    } else {
      pval.class<-'not sig'
    }
  } else if(mu<0) {
    if(quantiles["99%"]<0) {
      pval.class<-'at alpha=0.01'
    } else if(quantiles["97.5%"]<0) {
      pval.class<-'at alpha=0.05'
    } else if(quantiles["95%"]<0) {
      pval.class<-'at alpha=0.10'
    } else {
      pval.class<-'not sig'
    }
  }
  se<-NA 
  pval<-NA
  #return me
  data.frame(
    mu,
    mu.min,
    mu.max,
    se=se,
    #se.q=se.q,
    pval=pval,
    pval.class=pval.class,
    stringsAsFactors=F
  )
}

########################################################################
########################################################################

#a function to add Latex stars to estimates, based on pval
#obviously, will return a string and not a number
apply.pvals<-function(ests,pvals,markers=c("**","*","+")) {
  if(class(ests)=="numeric")
    ests<-sprintf("%.3f",ests)
  ests[pvals<0.1 & pvals>=0.05]<-
    paste0(
      ests[pvals<0.1 & pvals>=0.05],
      paste0("\\textsuperscript{",markers[3],"}")
    )
  ests[pvals<0.05 & pvals>=0.01]<-
    paste0(
      ests[pvals<0.05 & pvals>=0.01],
      paste0("\\textsuperscript{",markers[2],"}")
    )  
  ests[pvals<0.01]<-
    paste0(
      ests[pvals<0.01],
      paste0("\\textsuperscript{",markers[1],"}")
    )
  return(ests)
}

#test
apply.pvals(ests=rnorm(30),pvals=rep(c(0.11,0.06,0.01),10))
apply.pvals(ests=sprintf("%.2f",rnorm(30)),pvals=rep(c(0.11,0.06,0.01),10))

#get 
get.pvals.class<-function(pvals) {
  y<-NA
  y[pvals<0.01]<-"at alpha=0.01"
  y[pvals>0.01 & pvals<0.05]<-"at alpha=0.05"
  y[pvals>0.05 & pvals<0.10]<-"at alpha=0.10"
  y[pvals>0.10]<-"not sig"
  return(y)
}

#this function uses class rather than numeric pval
apply.pvals.class<-function(ests,pvals.class,markers=c("**","*","+")) {
  if(class(ests)=="numeric")
    ests<-sprintf("%.3f",ests)
  ests[pvals.class=="at alpha=0.10"]<-
    paste0(
      ests[pvals.class=="at alpha=0.10"],
      paste0("\\textsuperscript{",markers[3],"}")
    )
  ests[pvals.class=="at alpha=0.05"]<-
    paste0(
      ests[pvals.class=="at alpha=0.05"],
      paste0("\\textsuperscript{",markers[2],"}")
    )  
  ests[pvals.class=="at alpha=0.01"]<-
    paste0(
      ests[pvals.class=="at alpha=0.01"],
      paste0("\\textsuperscript{",markers[1],"}")
    )
  return(ests)
}

#this function takes an est, pval, and se, 
#and gives something for a regtable
gimmie.est<-function(mu,pval,se,nrow=4) {
  tmp1<-apply.pvals(mu,pval)
  tmp2<-paste0("(",sprintf("%.3f",se),")")
  matrix(c(tmp1,tmp2),nrow=nrow)
}

#this function takes an est, a pval class and CI
#and gives something for a regtable
#to be used with simulated long run multipliers
gimmie.est2<-function(mu,pval.class=NULL,mu.min,mu.max,nrow=4) {
  # mu<-estdf$mu
  # pval.class<-estdf$pval.class
  # mu.min<-estdf$mu.min
  # mu.max<-estdf$mu.max
  # nrow<-2
  if(length(mu)==0) {
    tmp1<-tmp2<-""
  } else {
    if(!is.null(pval.class)) {
      tmp1<-apply.pvals.class(mu,pval.class)
    } else {
      tmp1<-format(round(mu,2),2)
    }
    tmp2<-paste0(
      "[",
      format(round(mu.min,2),2),
      ",",
      format(round(mu.max,2),2),
      "]"
    )
  }
  matrix(
    c(tmp1,tmp2),
    nrow=nrow
  )
}

########################################################################
########################################################################

#summarize runs
sumruns<-function(x,sep.me=":") {
  diffs <- c(1, diff(x))
  start_indexes <- c(1, which(diffs > 1))
  end_indexes <- c(start_indexes - 1, length(x))
  coloned <- paste(x[start_indexes], x[end_indexes], sep=sep.me)
  paste0(coloned, collapse=", ")
}

########################################################################
########################################################################

#helper functions
normalize<-function(x,na.rm=T)
  return((x-min(x,na.rm=na.rm))/(max(x,na.rm=na.rm)-min(x,na.rm=na.rm)))
diff.apply<-function(var,index,n) #t+1 - t
  return(unlist(tapply(var,index,function(x) c(diff(x,n),rep(NA,n)))))
diff.apply2<-function(var,index,n) #t - t-1
  return(unlist(tapply(var,index,function(x) c(rep(NA,n),diff(x,n)))))
lag.apply<-function(var,index,n) 
  return(unlist(tapply(var,index,function(x) c(dplyr::lag(x,n))))) #lag auto generates NA's
lead.apply<-function(var,index,n) 
  return(unlist(tapply(var,index,function(x) lead(x,n)))) #lead auto generates NA's
unskew<-function(x) {
  sign<-ifelse(x<0,-1,1)
  sqrt(abs(x))*sign
}

#########################################################

#this is a helper function
#calculates the BIC for PLM objects
#and for LM objects, since my function
#doesn't seem to match the built-in function

calcfits<-function(m) {
  #m<-m.test
  #m<-m
  #is it a plm model?
  plm.fit<-class(m)[1]=="plm"
  #m<-m.cube
  #get residual sum of squares
  res<-m$residuals
  rss<-sum(res^2)
  #get n
  n<-nobs(m)
  #get k
  if(plm.fit) {
    k<-n-m$df.residual
  } else {
    k<-m$rank
  }
  #the stats
  df<-n-k #degrees of freedom
  w<-rep(1,n) #weights
  #log likelihood
  ll<-0.5 * (sum(log(w)) - n * 
               (log(2 * pi) + 1 - log(n) + log(sum(w * res^2))))
  df.ll<-k+1
  bic<- -2 * ll + log(n) * df.ll
  aic<- -2 * ll + 2 * df.ll 
  #bic<- n*log(rss/n)+k*log(n) #old way
  #aic<-2*k+n*log(rss) #old way
  #get summary object
  summary.m<-tryCatch(summary(m),error=function(e) "error")
  #temp fix for the plm problem
  if(summary.m[1]=="error") {
    print("summary error")
    r2<-adjr2<-NA
  } else {
    if(plm.fit) {
      r2<-summary.m$r.squared['rsq']
      adjr2<-summary.m$r.squared['adjrsq']
    } else {
      r2<-summary.m$r.squared
      adjr2<-summary.m$adj.r.squared
    }
  }
  #return
  thisrow<-data.frame(bic,aic,r2,adjr2,
                      stringsAsFactors=F)
  return(thisrow)  
}

#test
# y<-rnorm(100)
# x<-rnorm(100)
# m.test<-lm(y ~ x)
# BIC(m.test)
# calcfits(m.test)

#########################################################
#########################################################

#tells us how many lags are in this var
getlag<-function(varname) {
  #varname<-"L4.D.beopct_all"
  laginfo<-str_replace(varname,"^(L([0-9]+)?)\\..*$","\\1")
  if(laginfo=="L") {
    y<-1
  } else if (str_detect(laginfo,"L")) {
    y<-str_replace(laginfo,"L","") %>% 
      as.numeric
  } else {
    y<-NA
  }
  return(y)
}
getlag("L2.D.beopct_all")

#########################################################
#########################################################

#lifted from: https://stackoverflow.com/questions/18332463/convert-written-number-to-number-in-r
word2num <- function(word){
  wsplit <- strsplit(tolower(word)," ")[[1]]
  one_digits <- list(zero=0, one=1, two=2, three=3, four=4, five=5,
                     six=6, seven=7, eight=8, nine=9)
  teens <- list(eleven=11, twelve=12, thirteen=13, fourteen=14, fifteen=15,
                sixteen=16, seventeen=17, eighteen=18, nineteen=19)
  ten_digits <- list(ten=10, twenty=20, thirty=30, forty=40, fifty=50,
                     sixty=60, seventy=70, eighty=80, ninety=90)
  doubles <- c(teens,ten_digits)
  out <- 0
  i <- 1
  while(i <= length(wsplit)){
    j <- 1
    if(i==1 && wsplit[i]=="hundred")
      temp <- 100
    else if(i==1 && wsplit[i]=="thousand")
      temp <- 1000
    else if(wsplit[i] %in% names(one_digits))
      temp <- as.numeric(one_digits[wsplit[i]])
    else if(wsplit[i] %in% names(teens))
      temp <- as.numeric(teens[wsplit[i]])
    else if(wsplit[i] %in% names(ten_digits))
      temp <- (as.numeric(ten_digits[wsplit[i]]))
    if(i < length(wsplit) && wsplit[i+1]=="hundred"){
      if(i>1 && wsplit[i-1] %in% c("hundred","thousand"))
        out <- out + 100*temp
      else
        out <- 100*(out + temp)
      j <- 2
    }
    else if(i < length(wsplit) && wsplit[i+1]=="thousand"){
      if(i>1 && wsplit[i-1] %in% c("hundred","thousand"))
        out <- out + 1000*temp
      else
        out <- 1000*(out + temp)
      j <- 2
    }
    else if(i < length(wsplit) && wsplit[i+1] %in% names(doubles)){
      temp <- temp*100
      out <- out + temp
    }
    else{
      out <- out + temp
    }
    i <- i + j
  }
  return(list(word,out))
}

#########################################################
#########################################################

#this function uses sintegral to get area
#it will be necessary where we have missing values
require(Bolstad2)
getarea<-function(years,values) {
  #years<-pred.year
  #values<-polityhat.normal
  if(max(diff(years))==1) { #if no missing years, no biggie
    return(sintegral(years,values)$int)
  } else { #if there are some missing years, need to adjust
    #need to split this up into running intervals
    #grab each instance of missing year, and the last year
    cutpoints<-which(c(diff(years)>1,NA) | years==max(years))
    cuts<-lapply(seq_along(cutpoints),function(i) {
      if(i==1) {
        return(1:cutpoints[i])
      } else {
        return((cutpoints[i-1]+1):cutpoints[i])
      }
    })
    #calculate area of each cut, and return
    area<-sapply(cuts,function(indices) {
      sintegral(years[indices],values[indices])$int
    }) %>% sum
    return(area)
  }
}

#########################################################
#########################################################

#this function takes a list of residuals (unbalanced), 
#and implements fisher-type tests on them

require(metap)
require(tseries)

purtest_fisher<-function(xs,lags=3) {
  # xs<-testdfs
  # lags<-3
  # 
  
  tmpseq.i<-seq_along(xs)
  pvals<-sapply(tmpseq.i,function(i) {
    #i<-57
    #print(i)

    x<-xs[[i]]
    output<-tseries::adf.test(
      x,
      alternative='stationary',
      k=lags
    )
    pval<-output$p.value
  })
  pvals<-pvals[!is.na(pvals)]
  fisher<-metap::sumlog(pvals)
  returnlist<-list(
    logtest=fisher$p,
    allpvals=pvals
  )
  returnlist
}

# #test with some simulated data
# df<-expand.grid(units=letters,t=1:50,x=1)
# df<-df[order(df$units,df$t,df$x),]
# gamma<-0.8
# #make each series unit root with some randomness
# df$x<-tapply(df$x,df$units,function(x) {
#   x[1]<-rnorm(n=1)
#   for(i in 2:length(x))  
#     x[i]<-x[i-1]*gamma+rnorm(n=1)
#   return(x)
# }) %>% unlist
# splitxs<-split(df$x,df$units)
# purtest(data.frame(splitxs),test="levinlin",exo="intercept",lags="AIC",pmax=3)
# purtest_fisher(splitxs)

#########################################################
#########################################################

#this creates black images, which I use for presentations

theme_black = function(base_size = 14, base_family = "CM Roman") {
  
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    
    theme(
      # Specify axis options
      axis.line = element_blank(),  
      axis.text.x = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.text.y = element_text(size = base_size*0.8, color = "white", lineheight = 0.9),  
      axis.ticks = element_line(color = "white", size  =  0.2),  
      axis.title.x = element_text(size = base_size, color = "white", margin = margin(0, 10, 0, 0)),  
      axis.title.y = element_text(size = base_size, color = "white", angle = 90, margin = margin(0, 10, 0, 0)),  
      axis.ticks.length = unit(0.3, "lines"),   
      # Specify legend options
      legend.background = element_rect(color = NA, fill = "black"),  
      legend.key = element_rect(color = "white",  fill = "black"),  
      legend.key.size = unit(1.2, "lines"),  
      legend.key.height = NULL,  
      legend.key.width = NULL,      
      legend.text = element_text(size = base_size*0.8, color = "white"),  
      legend.title = element_text(size = base_size*0.8, face = "bold", hjust = 0, color = "white"),  
      legend.position = "right",  
      legend.text.align = NULL,  
      legend.title.align = NULL,  
      legend.direction = "vertical",  
      legend.box = NULL, 
      # Specify panel options
      panel.background = element_rect(fill = "black", color  =  NA),  
      panel.border = element_rect(fill = NA, color = "white"),  
      panel.grid.major = element_line(color = "grey35"),  
      panel.grid.minor = element_line(color = "grey20"),  
      #panel.margin = unit(0.5, "lines"),   
      # Specify facetting options
      strip.background = element_rect(fill = "grey30", color = "grey10"),  
      strip.text.x = element_text(size = base_size*0.8, color = "white"),  
      strip.text.y = element_text(size = base_size*0.8, color = "white",angle = -90),  
      # Specify plot options
      plot.background = element_rect(color = "black", fill = "black"),  
      plot.title = element_text(size = base_size*1.2, color = "white"),  
      plot.margin = unit(rep(1, 4), "lines")
      
    )
  
}

#########################################################
#########################################################

setwd(olddir)