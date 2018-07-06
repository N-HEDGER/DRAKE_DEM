---
title: "EYETRACK_PIPE"
output: 
  html_document: 
    keep_md: yes
---


# Index

| Section | Description | Status |
| --- | --- | --- |
| [Description and Requirements](#Rules) | A brief readme |
| [Imports](#import) | Load the required packages |
| [Inputs](#inputs) | Set the variables required for analysis |
| [Functions](#functions) | Functions for the pipeline |
| [Define Workflow](#workflow) | Define pipeline |
| [Run Workflow](#run) | Run pipeline |

***

<a id='Rules'></a>

# Description

Pipeline for analysing eyetracking data output from MATLAB experiment.

1. The eventables ('summary.txt' files describing the events on each trial) output by MATLAB are read in.
2. These are then put alongside the corresponding cleaned eye-movement data (output from Grafix).
3. The demographic variables (EQ, AQ etc) are appended to the data for each participant.
4. The data for NT and ASC subjects are then put into one big dataframe.
5. The gaze data are then coded in terms of which AOI they fall into.
6. Data are then re-formatted into eyetrackingR format.
7. The data are then cleaned according to the degree of trackloss you allow.
8. Three analysis are performed i) Window analysis ii) Time series/divergence analysis iii) Switching analysis. In each case, the demographic variables are defined as predictors.
9. A report is generated: 'report.html' including the output of the models and some plots.

***

## Requirements

1. You will need to install all the packages listed in the [following](#import) section.
2. A directory of 'summary files' - the 'summary.txt' files output from matlab - these should be named as follows e.g. NT_001, NT_002 etc for neurotypical and A_001 for ASC.
3. A directory of 'Grafix files' - these are the cleaned .csv files output from Grafix. Grafix only allows numeric names, so 001 for NT and 101 for ASC etc
4. An excel file containing demographic info. Rows are individual subjects, ordered in the same way as i). Columns are different demographic variables (EQ, AQ etc).

***

## Instructions 

1. First set the [inputs](#inputs). This includes things like the path to your files, prefixes for files, AOI definitions, cleaning parameters and analysis parameters. 
2. Then read in all the functions [functions](#functions)
3. Then define the [pipeline](#workflow)
4. Then [run the pipeline](#run)

***

<a id='import'></a>
# Imports and Variables

## Load in packages


```r
require(drake)
require(readr)
require(pracma)
require(xlsx)
require(ggplot2)
require(eyetrackingR)
require(lme4)
require(afex)
require(phia)
require(nlme)
require(effects)
require(stringr)
require(webshot)
```

***
<a id='inputs'></a>
# Set Variables

### Create some environments


```r
clean()
const=new.env()
AOIdef=new.env()
formatdef=new.env()
cleandef=new.env()
analysis=new.env()
```


### Set all the required variables and put them into their environments.

Paths and Prefixes


```r
# Path to the Grafix files.
assign("Grafpath","/Users/nicholashedger/Google\ Drive/Grafix",envir=const)

# Path to the summary files
assign("Sumpath","/Users/nicholashedger/Google\ Drive/EYETRACK_DATA/FREEVIEW",envir=const)

# Path to the demographic data.
assign("Demopath","/Users/nicholashedger/Google\ Drive/EYETRACK_DEMO/",envir=const)

# Base path.
assign("basepath",'/Users/nicholashedger/Documents/DRAKE_DEM',envir=const)

# Prefix given to the NT summary files.
assign("PrefixsumNT","P",envir=const)

# Prefix given to the ASC summary files.
assign("PrefixsumASC","A",envir=const)

# Prefix given to the xls file of demographic data: NT
assign("PrefixdemoNT","NT",envir=const)

# Prefix given to the xls file of demographic data: ASC
assign("PrefixdemoASC","ASC",envir=const)

# Prefix given to the Grafix files: NT
assign("PrefixgrafNT","smooth_0",envir=const)

# Prefix given to the Grafix files: ASC
assign("PrefixgrafASC","smooth_1",envir=const)
```

Column names and list of demographic variables.


```r
assign("colnames",c("Trial","Timestamp","X","Y","side","sc","model","ps",'Group'),envir=const)

assign("DEMOVARS",c(c("EQ","AQ")),envir=const)
```


AOI definitions.


```r
# The left AOI
assign("rectlxmin",303,envir=AOIdef)
assign("rectlxmax",542,envir=AOIdef)
assign("rectlymin",424,envir=AOIdef)
assign("rectlymax",601,envir=AOIdef)

# The right AOI
assign("rectrxmin",739,envir=AOIdef)
assign("rectrxmax",978,envir=AOIdef)
assign("rectrymin",424,envir=AOIdef)
assign("rectrymax",601,envir=AOIdef)

# The resolution of the monitor
assign("resx",1280,envir=AOIdef)
assign("resy",1024,envir=AOIdef)
```

Formatting definitions.


```r
# Whether or not to treat non-AOI looks as missing data.
assign("missing_dat",TRUE,envir=formatdef)

# The earliest timestamp you are interested in.
assign("TRIML",1000,envir=formatdef)

# The latest timestamp you are interested in.
assign("TRIMU",6000,envir=formatdef)
```

Cleaning definitions.


```r
# The maximum permitted proportion trackloss per trial
assign("trial_prop",.6,envir=cleandef)

# The maximum permitted proportion trackloss per participant.
assign("part_prop",1,envir=cleandef)
```

Analysis definitions


```r
# The size of the timebins for all timeseries analyses.
assign("binlength",100,envir=analysis)

# The repetitions for the bootsrappping
assign("nreps",100,envir=analysis)

# The onset time for switching analysis
assign("onset_time",1100,envir=analysis)

# The rolling window to use for switching analysis.
assign("windowlength",100,envir=analysis)
```

***
<a id='functions'></a>
# Functions

Multiplot function


```r
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
```

These functions return a list of files given a path and a prefix. They read in the list of files and bind them into one matrix.


```r
GET_FILEVEC <- function(prefixsum,pathsum) {
  filevec=list.files(path=pathsum,pattern=prefixsum,full.names = TRUE)
  filevec=filevec[order(parse_number(list.files(path=pathsum,pattern=prefixsum,full.names = TRUE)))]
  return(filevec)
}


GET_NT_SUM <- function(prefixsum,pathsum) {
  fprintf('Loading NT trial matrix...\n',file='log.txt')
  filevec=list.files(path=pathsum,pattern=prefixsum,full.names = TRUE)
  filevec=filevec[order(parse_number(filevec))]
  fprintf('Found %i files \n',length(filevec),file='log.txt', append = TRUE)
  print(filevec)
  DATA=data.frame()
  for (i in 1:length(filevec)){
    DATA=rbind(cbind(read.table(file=filevec[i], header = FALSE,blank.lines.skip=FALSE,sep=",",fill=TRUE),i,1),DATA)
    fprintf('Loading file %s \n',filevec[i],file='log.txt', append = TRUE)
  
  }
  return(DATA)
}


GET_ASC_SUM <- function(prefixsum,pathsum) {
  fprintf('Loading ASC trial matrix... \n',file='log.txt', append = TRUE)
  filevec=list.files(path=pathsum,pattern=prefixsum,full.names = TRUE)
  filevec=filevec[order(parse_number(filevec))]
  fprintf('Found %i files \n',length(filevec),file='log.txt', append = TRUE)
  print(filevec)
  DATA=data.frame()
  for (i in 1:length(filevec)){
    DATA=rbind(cbind(read.table(file=filevec[i], header = FALSE,blank.lines.skip=FALSE,sep=",",fill=TRUE),i,2),DATA)
    fprintf('Loading file %s \n',filevec[i],file='log.txt', append = TRUE)
  }
  return(DATA)
}


GET_NT_GRAF <- function(prefixgraf,pathgraf) {
  fprintf('Loading cleaned NT eye-tracking data..',file='log.txt', append = TRUE)
  filevec=list.files(path=pathgraf,pattern=prefixgraf,full.names = TRUE)
  filevec=filevec[order(parse_number(filevec))]
  fprintf('Found %i files \n',length(filevec),file='log.txt', append = TRUE)
  print(filevec)
  DATA=data.frame()
  for (i in 1:length(filevec)){
    DATA=rbind(cbind(read.table(file=filevec[i], header = FALSE,blank.lines.skip=FALSE,sep=",",fill=TRUE),i,1),DATA)
    fprintf('Loading file %s \n',filevec[i],file='log.txt', append = TRUE)
  }
  return(DATA)
}


GET_ASC_GRAF<- function(prefixgraf,pathgraf) {
  fprintf('Loading cleaned ASC eye-tracking data.. \n', append = TRUE)
  filevec=list.files(path=pathgraf,pattern=prefixgraf,full.names = TRUE)
  filevec=filevec[order(parse_number(filevec))]
  fprintf('Found %i files \n',length(filevec),file='log.txt', append = TRUE)
  print(filevec)
  DATA=data.frame()
  for (i in 1:length(filevec)){
    DATA=rbind(cbind(read.table(file=filevec[i], header = FALSE,blank.lines.skip=FALSE,sep=",",fill=TRUE),i,2),DATA)
    fprintf('Loading file %s \n ',filevec[i],file='log.txt', append = TRUE)
  }
  return(DATA)
}
```

Binds the cleaned eye-tracking data to the event table.


```r
BIND_VARS=function(condmat,smoothmat,colnames) {
  fprintf('Binding NT data to the trial matrix.. \n',file='log.txt', append = TRUE)
  colnames(condmat)=colnames
  condmat$XSMOOTH=smoothmat$V3
  condmat$YSMOOTH=smoothmat$V4
  condmat$interp=smoothmat$V7
  return(condmat)
}
```


Reads in a xls file containing demographic information and binds the demographic variables to the corresponding participant.


```r
GET_DEMO=function(demopath,demoprefix,frame,filevec,DEMONAMES) {
  fprintf('Collecting demographic data.. \n',file='log.txt', append = TRUE)
  
  DEMOFRAME=read.xlsx2(strcat(demopath,strcat(demoprefix,'.xlsx')),sheetIndex = 1)
  
  frame2=cbind(frame,repmat(0,nrow(frame),length(DEMONAMES)))
  
  colnames(frame2)=c(colnames(frame),DEMONAMES)
  
  
  DEMOMAT=data.frame()
  for (i in 1:length(DEMONAMES)){
    
    if (DEMONAMES[i]=="Gender"){
      vec=as.character(get(DEMONAMES[i],DEMOFRAME))[order(parse_number(filevec))]
    }
    else{
      vec=as.numeric(as.character(get(DEMONAMES[i],DEMOFRAME)))[sort(parse_number(filevec))]
    }
    
    for (j in 1:length(unique(frame2$ps))){
      frame2[frame2$ps==j,][,length(colnames(frame))+i]=rep(vec[j])
      
    }
  
  }
  return(frame2)
  }
```

Binds the NT and ASC groups (if applicable) and recodes the participant ID.


```r
APPEND_DATA=function(NTFRAME,ASCFRAME,filevec) {
  fprintf('Binding the NT and ASC frames together \n',file='log.txt', append = TRUE)
  ASCFRAME$ps=ASCFRAME$ps+length(filevec)
  DATA=rbind(NTFRAME,ASCFRAME)
  print(table(DATA$ps,DATA$Group))
  return(DATA)
}
```

Trims of redundant data (before the images appear) and defines the AOIs.


```r
DEFINE_AOI=function(FRAME,rectlxmin,rectlxmax,rectlymin,rectlymax,rectrxmin,rectrxmax,rectrymin,rectrymax,triml,trimu) {

  FRAME=FRAME[FRAME$Timestamp>triml,]
  FRAME=FRAME[FRAME$Timestamp<trimu,]
  
  fprintf('Trimming off the first %f msec of data \n',triml,file='log.txt', append = TRUE)
  fprintf('Trimming off anything after %f msec \n',trimu,file='log.txt', append = TRUE)
  
  FRAME$isinL=as.logical(ifelse(FRAME$XSMOOTH<rectlxmax & FRAME$XSMOOTH>rectlxmin & FRAME$YSMOOTH<rectlymax & FRAME$YSMOOTH>rectlymin ,1,0))
  FRAME$isinR=as.logical(ifelse(FRAME$XSMOOTH<rectrxmax & FRAME$XSMOOTH>rectrxmin & FRAME$YSMOOTH<rectrymax & FRAME$YSMOOTH>rectrymin ,1,0))
  
  FRAME$AOI=rep(0,nrow(FRAME))
  
  fprintf('Defining AOIs',file='log.txt', append = TRUE)
  
  FRAME[FRAME$XSMOOTH<AOIdef$rectlxmax & FRAME$XSMOOTH>AOIdef$rectlxmin & FRAME$YSMOOTH<AOIdef$rectlymax & FRAME$YSMOOTH>AOIdef$rectlymin,]$AOI=1
  FRAME[FRAME$XSMOOTH<AOIdef$rectrxmax & FRAME$XSMOOTH>AOIdef$rectrxmin & FRAME$YSMOOTH<AOIdef$rectrymax & FRAME$YSMOOTH>AOIdef$rectrymin,]$AOI=2
  FRAME[FRAME$AOI!=1 & FRAME$AOI!=2,]$AOI=0
  FRAME$AOI=factor(FRAME$AOI)
  return(FRAME)
}
```

Prepares the data for EyetrackingR format.


```r
FORMAT <- function(FRAME,missing_dat) {
  fprintf('Formatting for eyetrackingR\n',file='log.txt', append = TRUE)
  FRAME$isN=ifelse(FRAME$X=="NaN" & FRAME$interp==0,1,0)
  FRAME$side=factor(FRAME$side,levels=c(1,2),labels=c("Social Left","Social Right"))
  
  FRAME$sc=factor(FRAME$sc,levels=c(1,2),labels=c("Intact","Scrambled"))
  
  
  FRAME$trackloss=ifelse(FRAME$X=="NaN",1,2)
  FRAME$trackloss=factor(FRAME$trackloss,levels=c(1,2),labels=c("NA","Data"))
  
  
  if("Group" %in% colnames(FRAME))
  {
    FRAME$Group=factor(FRAME$Group,levels=c(1,2),labels=c("NT","ASC"))
  }
  
  
  
  FRAME$SOCIAL=as.logical(ifelse(as.numeric(FRAME$side)==FRAME$AOI,1,0))
  FRAME$NONSOCIAL=as.logical(ifelse(as.numeric(FRAME$side)!=FRAME$AOI & FRAME$AOI!=0 ,1,0))
  FRAME$trackloss=as.logical(ifelse(FRAME$X=="NaN" & FRAME$interp!=1,1,0))
  
  ET_DATA <- make_eyetrackingr_data(FRAME, 
                                    participant_column = "ps",
                                    trial_column = "Trial",
                                    time_column = "Timestamp",
                                    aoi_columns = c('isinL','isinR',"SOCIAL","NONSOCIAL"),
                                    treat_non_aoi_looks_as_missing = TRUE,trackloss_column="trackloss"
  )
  return(ET_DATA)
  
}
```

Makes a plot of the first observers data.


```r
MAKE_PLOT <- function(FRAME,rectlxmin,rectlxmax,rectlymin,rectlymax,rectrxmin,rectrxmax,rectrymin,rectrymax,resx,resy) {
  ggplot(FRAME[FRAME$ps==1 & FRAME$sc=="Intact",],aes(x=Timestamp,y=XSMOOTH))+geom_vline(aes(xintercept=Timestamp,colour=trackloss),alpha=.1)+geom_point(aes(colour=side))+facet_wrap(~Trial,ncol=5)+theme_classic()+ scale_colour_manual(values = c("white","springgreen3","steelblue2","pink"))
}
```

Cleans the data according to trackloss criteria


```r
CLEAN_TRACK=function(FRAME,partprop,trialprop){
  trackenv=new.env()
  fprintf('Cleaning according to trackloss criteria.. \n',file='log.txt', append = TRUE)
  
  assign('trackloss_summary',trackloss_analysis(FRAME),envir=trackenv)
  
  bef=nrow(FRAME)
  FRAME <- clean_by_trackloss(data = FRAME, trial_prop_thresh = trialprop,participant_prop_thresh=partprop)
  af=nrow(FRAME)
  
  assign('criteria',c(partprop*100,trialprop*100),envir=trackenv)
  
  removed=(bef-af)/bef*100
  
  assign('perc_removed',removed,envir=trackenv)
  assign('CLEANED',FRAME,envir=trackenv)

  return(trackenv)
}
```

Performs time-window analyses


```r
TIME_WINDOW=function(FRAME,DEMOVARS){
  FRAME=FRAME$CLEANED
  window_env=new.env()
  response_window_agg_by_sub <- make_time_window_data(FRAME, aois=c("NONSOCIAL","SOCIAL"),summarize_by = "ps",predictor_columns = c(c("sc","Group"),DEMOVARS))
  response_window_agg_by_sub$AOI=factor(response_window_agg_by_sub$AOI)
  
  assign('FRAME',response_window_agg_by_sub,envir=window_env)
  
  WINDOW_PLOT=ggplot(response_window_agg_by_sub,aes(x=AOI,y=Prop))+facet_grid(.~sc)+  stat_summary(fun.y=mean,position=position_dodge(width=0.95),geom="bar",aes(fill=AOI),size=2,alpha=.5,colour="black")+theme_classic(base_size = 11)+ylab("Gaze Proportion")+geom_point(colour="red",position = position_jitter(w=0.2),alpha=.5,size=1)+
   scale_colour_discrete(guide=FALSE)+ theme(axis.text.x = element_text(angle = 25, hjust = 1))+ scale_fill_manual(values = c("springgreen3","steelblue2"))+theme(legend.position = "none")+geom_hline(yintercept=.5,linetype='dashed')
  
  if (length(unique(FRAME$sc))==2){
  model_window <- lmer(Prop ~ AOI*sc+(1|ps), data = response_window_agg_by_sub, REML = FALSE)
  model_window_p = mixed(Prop ~ AOI*sc + (1|ps), data=response_window_agg_by_sub,method="LRT")
  interactions= testInteractions(model_window, fixed=c("sc"), pairwise=c("AOI"),adjustment="holm")
  
  assign('PLOT',WINDOW_PLOT,envir=window_env)
  assign('model',model_window,envir=window_env)
  assign('modelp',model_window_p,envir=window_env)
  assign('interactions',interactions,envir=window_env)
  
  formula1= "Prop ~ AOI*sc"
  for (i in 1:length(DEMOVARS)){
    fixedf=strcat(formula1,strcat('*',DEMOVARS[i]))
  print(i)
    
  demmod=lmer(formula=formula(paste0(fixedf,"+","(1|ps)")), data = window_env$FRAME)
  assign(strcat('modeld',DEMOVARS[i]), demmod,envir=window_env)
  assign(strcat('modelp',DEMOVARS[i]), mixed(formula=formula(paste0(fixedf,"+","(1|ps)")), data = window_env$FRAME,method="LRT"),envir=window_env)
    
  }
  
  }
  else if (length(unique(FRAME$sc))==1){
    model_window <- lmer(Prop ~ AOI+(1|ps), data = response_window_agg_by_sub, REML = FALSE)
    model_window_p = mixed(Prop ~ AOI + (1|ps), data=response_window_agg_by_sub,method="LRT")

    assign('PLOT',WINDOW_PLOT,envir=window_env)
    assign('model',model_window,envir=window_env)
    assign('modelp',model_window_p,envir=window_env)

    formula1= "Prop ~ AOI"
    for (i in 1:length(DEMOVARS)){
      fixedf=strcat(formula1,strcat('*',DEMOVARS[i]))
      print(i)
      
      demmod=lmer(formula=formula(paste0(fixedf,"+","(1|ps)")), data = window_env$FRAME)
      assign(strcat('modeld',DEMOVARS[i]), demmod,envir=window_env)
      assign(strcat('modelp',DEMOVARS[i]), mixed(formula=formula(paste0(fixedf,"+","(1|ps)")), data = window_env$FRAME,method="LRT"),envir=window_env)
      
    }
    
  }
  
  return(window_env)
  
}
```

Performs global time-series analyses.


```r
TS_ANALYSIS=function(FRAME,binlength){
  FRAME=FRAME$CLEANED
  ts_env=new.env()
  
  if (length(unique(FRAME$sc))==2){
  response_time <- make_time_sequence_data(FRAME, time_bin_size = binlength,aois = c("isinL","isinR"),summarize_by = "ps",predictor_columns = c("sc","side"))
  response_time2 <- make_time_sequence_data(FRAME, time_bin_size = binlength,aois = c("SOCIAL","NONSOCIAL"),summarize_by = "ps",predictor_columns = c("sc","side"))
  
  tb_analysis_INT <- analyze_time_bins(data = response_time[response_time$sc=="Intact",],test = "t.test",aoi=c("isinL"),predictor_column =c("side"),  alpha = .05,p_adjust_method = 'holm')
  tb_analysis_SC <- analyze_time_bins(data = response_time[response_time$sc=="Scrambled",],test = "t.test",aoi=c("isinL"),predictor_column =c("side"),  alpha = .05,p_adjust_method = 'holm')
  
  
  x=plot(response_time2,predictor_column = "sc")+theme_classic()+ scale_colour_manual(values = c("springgreen3","steelblue2","springgreen3","steelblue2"))+ scale_fill_manual(values = c("springgreen3","steelblue2","springgreen3","steelblue2"))
  x2=plot(tb_analysis_INT)+theme_classic()+ scale_colour_manual(values = c("springgreen3","steelblue2","springgreen3","steelblue2"))+ggtitle("Intact images")
  x3=plot(tb_analysis_SC)+theme_classic()+ scale_colour_manual(values = c("springgreen3","steelblue2","springgreen3","steelblue2"))+ggtitle("Scrambled images")
  
  
  assign('TBA_1',tb_analysis_INT,envir = ts_env)
  assign('TBA_2',tb_analysis_SC,envir = ts_env)
  assign('TSPLOT',x,envir = ts_env)
  assign('TBPLOT1',x2,envir = ts_env)
  assign('TBPLOT2',x3,envir = ts_env)
  }
  
  else if (length(unique(FRAME$sc))==1){
    response_time <- make_time_sequence_data(FRAME, time_bin_size = binlength,aois = c("isinL","isinR"),summarize_by = "ps",predictor_columns = c("side"))
    response_time2 <- make_time_sequence_data(FRAME, time_bin_size = binlength,aois = c("SOCIAL","NONSOCIAL"),summarize_by = "ps",predictor_columns = c("side"))
    
    tb_analysis_INT <- analyze_time_bins(data = response_time,test = "t.test",aoi=c("isinL"),predictor_column =c("side"),  alpha = .05,p_adjust_method = 'holm')
    
    x=plot(response_time2)+theme_classic()+ scale_colour_manual(values = c("springgreen3","steelblue2","springgreen3","steelblue2"))+ scale_fill_manual(values = c("springgreen3","steelblue2","springgreen3","steelblue2"))
    x2=plot(tb_analysis_INT)+theme_classic()+ scale_colour_manual(values = c("springgreen3","steelblue2","springgreen3","steelblue2"))+ggtitle("Intact images")

    assign('TBA_1',tb_analysis_INT,envir = ts_env)
    assign('TSPLOT',x,envir = ts_env)
    assign('TBPLOT1',x2,envir = ts_env)
  }
  
  return(ts_env)
  
}
```

Look at the effects of demographic variables on time series.


```r
TS_ANALYSIS_DEMO=function(FRAME,binlength,DEMOVARS,nreps){
  FRAME=FRAME$CLEANED
  ts_env_demo=new.env()
  
  num_sub = length(unique((FRAME$ps)))
  threshold_t = qt(p = 1 - .05/2, df = num_sub-1)
  
  response_time_new <- make_time_sequence_data(FRAME, time_bin_size = binlength,aois = c("SOCIAL"),predictor_columns = c("sc",DEMOVARS),summarize_by="ps")

  for (i in 1:length(DEMOVARS)){
  df_timeclust_between <- make_time_cluster_data(response_time_new[response_time_new$sc=="Intact",], test= "lm",predictor_column = DEMOVARS[i], threshold = threshold_t)
  
  plot=plot(df_timeclust_between)+ scale_fill_manual(values = c("springgreen3","steelblue2","springgreen3","steelblue2"))+theme_classic()+xlab("Time in Trial (ms)")+ggtitle(DEMOVARS[i])
  
  clust_analysis_between <- analyze_time_clusters(df_timeclust_between, within_subj = FALSE, samples=nreps)
  clust_analysis_plot <- plot(clust_analysis_between)+ggtitle(DEMOVARS[i])+theme_classic()
  
  assign(strcat('plot',DEMOVARS[i]),plot,envir=ts_env_demo)
  assign(strcat('fit',DEMOVARS[i]),df_timeclust_between,envir=ts_env_demo)
  
  assign(strcat('clustplot',DEMOVARS[i]),clust_analysis_plot,envir=ts_env_demo)
  assign(strcat('clustfit',DEMOVARS[i]), clust_analysis_between,envir=ts_env_demo)
  
  
  }
  return(ts_env_demo)
  
}
```

Perform analysis of switch latencies


```r
TS_ANALYSIS_SWITCH=function(FRAME,onset_time,window_length,DEMOVARS){
  FRAME=FRAME$CLEANED
  ts_env_switch=new.env()
  
  onsets <- make_onset_data(FRAME[FRAME$sc=="Intact",], onset_time = onset_time, fixation_window_length=window_length,target_aoi='SOCIAL',distractor_aoi = 'NONSOCIAL')
  
  assign('plotswitchall',plot(onsets)+theme_classic()+ scale_colour_manual(values = c("springgreen3","steelblue2")),envir=ts_env_switch)
  
  onset_switchesall <- make_switch_data(onsets)
  
  assign('plotonseswitchall', plot(onset_switchesall)+theme_classic()+ scale_colour_manual(values = c("springgreen3","steelblue2")),envir=ts_env_switch)
  
  assign('model',lmer(FirstSwitch ~ FirstAOI + (1 | ps), data=onset_switchesall,REML=FALSE),envir=ts_env_switch)
  assign('modelp',mixed(FirstSwitch ~ FirstAOI + (1 | ps), data=onset_switchesall,method="LRT"),envir=ts_env_switch)
  
  
  formula1= "FirstSwitch ~ FirstAOI"

  
  for (i in 1:length(DEMOVARS)){
  
  onset_switches <- make_switch_data(onsets, predictor_columns = DEMOVARS[i])
  
  assign(strcat('dplotswitch',DEMOVARS[i]),plot(onsets,predictor_columns=DEMOVARS[i])+ guides(linetype=guide_legend(title="Initial AOI"))+scale_colour_manual(values = c("springgreen3","steelblue2"))+ theme_classic()+ggtitle(DEMOVARS[i])
,envir=ts_env_switch)
  assign(strcat('dplotonsetswitch',DEMOVARS[i]), plot(onset_switches, predictor_columns = DEMOVARS[i])+ scale_colour_manual(values = c("springgreen3","steelblue2"))+ theme_classic()+ggtitle(DEMOVARS[i]),envir=ts_env_switch)
  
  
  fixedf=strcat(formula1,strcat('*',DEMOVARS[i]))
  print(i)
  
  demmod=lmer(formula=formula(paste0(fixedf,"+","(1|ps)")), data = onset_switches)
  assign(strcat('dmodeld',DEMOVARS[i]), demmod,envir=ts_env_switch)
  assign(strcat('dmodelp',DEMOVARS[i]), mixed(formula=formula(paste0(fixedf,"+","(1|ps)")), data = onset_switches,method="LRT"),envir=ts_env_switch)
  
  
  
  }
  
  
  return(ts_env_switch)
  
}
```

***

<a id='Workflow'></a>
# Define pipeline


```r
setwd(const$basepath)

plan <- drake_plan(
  filevec_NT=GET_FILEVEC(const$PrefixsumNT,const$Sumpath),
  filevec_ASC=GET_FILEVEC(const$PrefixsumASC,const$Sumpath),
  raw_data_NT = GET_NT_SUM(const$PrefixsumNT,const$Sumpath),
  raw_data_ASC = GET_ASC_SUM(const$PrefixsumASC,const$Sumpath),
  raw_data_NT_GRAF = GET_NT_GRAF(const$PrefixgrafNT,const$Grafpath),
  raw_data_ASC_GRAF = GET_ASC_GRAF(const$PrefixgrafASC,const$Grafpath),
  bound_data_NT=BIND_VARS(raw_data_NT,raw_data_NT_GRAF,const$colnames),
  bound_data_ASC=BIND_VARS(raw_data_ASC,raw_data_ASC_GRAF,const$colnames),
  DEMOFRAME_NT=GET_DEMO(const$Demopath,const$PrefixdemoNT,bound_data_NT,filevec_NT,const$DEMOVARS),
  DEMOFRAME_ASC=GET_DEMO(const$Demopath,const$PrefixdemoASC,bound_data_ASC,filevec_ASC,const$DEMOVARS),
  FRAME=APPEND_DATA(DEMOFRAME_NT,DEMOFRAME_ASC,filevec_NT),
  AOI_FRAME=DEFINE_AOI(FRAME,AOIdef$rectlxmin,AOIdef$rectlxmax,AOIdef$rectlymin,AOIdef$rectlymax,AOIdef$rectrxmin,AOIdef$rectrxmax,AOIdef$rectrymin,AOIdef$rectrymax,formatdef$TRIML,formatdef$TRIMU),
  EYETRACK_FRAME=FORMAT(AOI_FRAME,formatdef$missing_dat),
  PLOT=MAKE_PLOT(EYETRACK_FRAME,AOIdef$rectlxmin,AOIdef$rectlxmax,AOIdef$rectlymin,AOIdef$rectlymax,AOIdef$rectrxmin,AOIdef$rectrxmax,AOIdef$rectrymin,AOIdef$rectrymax,AOIdef$resx,AOIdef$resy),
  CLEANED_FRAME=CLEAN_TRACK(EYETRACK_FRAME,cleandef$part_prop,cleandef$trial_prop),
  WINDOW_FRAME=TIME_WINDOW(CLEANED_FRAME,const$DEMOVARS),
  TS_FRAME=TS_ANALYSIS(CLEANED_FRAME,analysis$binlength),
  TS_DEMO_FRAME=TS_ANALYSIS_DEMO(CLEANED_FRAME,analysis$binlength,const$DEMOVARS,analysis$nreps),
  TS_SWITCH_FRAME=TS_ANALYSIS_SWITCH(CLEANED_FRAME,analysis$onset_time,analysis$windowlength,const$DEMOVARS),
  
  rmarkdown::render(
    knitr_in("report.Rmd"),
    output_file = file_out("report.html"),
    quiet = TRUE
  )
  )

config <- drake_config(plan)
vis_drake_graph(config,layout='layout_with_sugiyama',direction='UD',targets_only = 'TRUE',build_times = "none")
```

<!--html_preserve--><div id="htmlwidget-ec897849c5c50a6f7846" style="width:672px;height:480px;" class="visNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-ec897849c5c50a6f7846">{"x":{"nodes":{"id":["raw_data_NT","raw_data_NT_GRAF","raw_data_ASC","raw_data_ASC_GRAF","bound_data_NT","filevec_NT","bound_data_ASC","filevec_ASC","DEMOFRAME_ASC","DEMOFRAME_NT","FRAME","AOI_FRAME","EYETRACK_FRAME","CLEANED_FRAME","PLOT","TS_DEMO_FRAME","TS_FRAME","TS_SWITCH_FRAME","WINDOW_FRAME","\"report.html\""],"label":["raw_data_NT","raw_data_NT_GRAF","raw_data_ASC","raw_data_ASC_GRAF","bound_data_NT","filevec_NT","bound_data_ASC","filevec_ASC","DEMOFRAME_ASC","DEMOFRAME_NT","FRAME","AOI_FRAME","EYETRACK_FRAME","CLEANED_FRAME","PLOT","TS_DEMO_FRAME","TS_FRAME","TS_SWITCH_FRAME","WINDOW_FRAME","\"report.html\""],"status":["outdated","outdated","outdated","outdated","outdated","outdated","outdated","outdated","outdated","outdated","outdated","outdated","outdated","outdated","outdated","outdated","outdated","outdated","outdated","outdated"],"type":["object","object","object","object","object","object","object","object","object","object","object","object","object","object","object","object","object","object","object","file"],"font.size":[20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20],"color":["#000000","#000000","#000000","#000000","#000000","#000000","#000000","#000000","#000000","#000000","#000000","#000000","#000000","#000000","#000000","#000000","#000000","#000000","#000000","#000000"],"shape":["dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","dot","square"],"level":[1,1,1,1,2,1,2,1,3,3,4,5,6,7,7,8,8,8,8,9],"hover_label":["GET_NT_SUM(const$PrefixsumNT, const$Sumpath)","GET_NT_GRAF(const$PrefixgrafNT, const$Grafpath)","GET_ASC_SUM(const$PrefixsumASC, const$Sumpath)","GET_ASC_GRAF(const$PrefixgrafASC, const$Grafpath)","BIND_VARS(raw_data_NT, raw_data_NT_GRAF, const$colnames)","GET_FILEVEC(const$PrefixsumNT, const$Sumpath)","BIND_VARS(raw_data_ASC, raw_data_ASC_GRAF, const$colnames)","GET_FILEVEC(const$PrefixsumASC, const$Sumpath)","GET_DEMO(const$Demopath, const$PrefixdemoASC, bound_data_ASC,\nfilevec_ASC, const$DEMOVARS)","GET_DEMO(const$Demopath, const$PrefixdemoNT, bound_data_NT,\nfilevec_NT, const$DEMOVARS)","APPEND_DATA(DEMOFRAME_NT, DEMOFRAME_ASC, filevec_NT)","DEFINE_AOI(FRAME, AOIdef$rectlxmin, AOIdef$rectlxmax,\nAOIdef$rectlymin, AOIdef$rectlymax, AOIdef$rectrxmin,\nAOIdef$rectrxmax, AOIdef$rectrymin, AOIdef$rectrymax,\nformatdef$TRIML, formatdef$TRIMU)","FORMAT(AOI_FRAME, formatdef$missing_dat)","CLEAN_TRACK(EYETRACK_FRAME, cleandef$part_prop,\ncleandef$trial_prop)","MAKE_PLOT(EYETRACK_FRAME, AOIdef$rectlxmin, AOIdef$rectlxmax,\nAOIdef$rectlymin, AOIdef$rectlymax, AOIdef$rectrxmin,\nAOIdef$rectrxmax, AOIdef$rectrymin, AOIdef$rectrymax, AOIdef$resx,\nAOIdef$resy)","TS_ANALYSIS_DEMO(CLEANED_FRAME, analysis$binlength,\nconst$DEMOVARS, analysis$nreps)","TS_ANALYSIS(CLEANED_FRAME, analysis$binlength)","TS_ANALYSIS_SWITCH(CLEANED_FRAME, analysis$onset_time,\nanalysis$windowlength, const$DEMOVARS)","TIME_WINDOW(CLEANED_FRAME, const$DEMOVARS)","rmarkdown::render(knitr_in(\"report.Rmd\"), output_file =\nfile_out(\"report.html\"), quiet = TRUE)"],"x":[-1,-0.666666666666667,-0.333333333333333,0,-0.666666666666667,0.5,0.333333333333333,1,0.5,-0.333333333333333,0.5,0.5,0.5,0.166666666666667,0.833333333333333,-0.833333333333333,-0.5,-0.166666666666667,0.166666666666667,0.5],"y":[-1,-1,-1,-1,-0.75,-1,-0.75,-1,-0.5,-0.5,-0.25,0,0.25,0.5,0.5,0.75,0.75,0.75,0.75,1]},"edges":{"from":["raw_data_NT","raw_data_NT_GRAF","raw_data_ASC","raw_data_ASC_GRAF","bound_data_NT","filevec_NT","filevec_NT","bound_data_ASC","filevec_ASC","DEMOFRAME_ASC","DEMOFRAME_NT","FRAME","AOI_FRAME","EYETRACK_FRAME","EYETRACK_FRAME","CLEANED_FRAME","CLEANED_FRAME","CLEANED_FRAME","CLEANED_FRAME","CLEANED_FRAME","PLOT","TS_DEMO_FRAME","TS_FRAME","TS_SWITCH_FRAME","WINDOW_FRAME"],"to":["bound_data_NT","bound_data_NT","bound_data_ASC","bound_data_ASC","DEMOFRAME_NT","DEMOFRAME_NT","FRAME","DEMOFRAME_ASC","DEMOFRAME_ASC","FRAME","FRAME","AOI_FRAME","EYETRACK_FRAME","CLEANED_FRAME","PLOT","TS_DEMO_FRAME","TS_FRAME","TS_SWITCH_FRAME","WINDOW_FRAME","\"report.html\"","\"report.html\"","\"report.html\"","\"report.html\"","\"report.html\"","\"report.html\""],"arrows":["to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to","to"]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot","physics":false},"manipulation":{"enabled":false},"layout":{"hierarchical":{"enabled":true,"direction":"UD"}},"edges":{"smooth":false},"physics":{"stabilization":false},"interaction":{"navigationButtons":true,"hover":true}},"groups":null,"width":null,"height":null,"idselection":{"enabled":false},"byselection":{"enabled":false},"main":{"text":"Dependency graph","style":"font-family:Georgia, Times New Roman, Times, serif;font-weight:bold;font-size:20px;text-align:center;"},"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)","legend":{"width":0.2,"useGroups":false,"position":"left","ncol":1,"stepX":100,"stepY":100,"zoom":true,"nodes":{"label":["Up to date","Outdated","In progress","Failed","Imported","Missing","Object","Function","File"],"color":["#228B22","#000000","#FF7221","#AA0000","#1874CD","#9A32CD","#888888","#888888","#888888"],"shape":["dot","dot","dot","dot","dot","dot","dot","triangle","square"],"font.color":["black","black","black","black","black","black","black","black","black"],"font.size":[20,20,20,20,20,20,20,20,20],"id":[1,2,3,4,5,6,7,8,9]},"nodesToDataframe":true},"igraphlayout":{"type":"square"},"tooltipStay":300,"tooltipStyle":"position: fixed;visibility:hidden;padding: 5px;white-space: nowrap;font-family: verdana;font-size:14px;font-color:#000000;background-color: #f5f4ed;-moz-border-radius: 3px;-webkit-border-radius: 3px;border-radius: 3px;border: 1px solid #808074;box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);","events":{"hoverNode":"function(e){\n        var label_info = this.body.data.nodes.get({\n          fields: ['label', 'hover_label'],\n          filter: function (item) {\n            return item.id === e.node\n          },\n          returnType :'Array'\n        });\n        this.body.data.nodes.update({\n          id: e.node,\n          label : label_info[0].hover_label,\n          hover_label : label_info[0].label\n        });\n      }","blurNode":"function(e){\n        var label_info = this.body.data.nodes.get({\n          fields: ['label', 'hover_label'],\n          filter: function (item) {\n            return item.id === e.node\n          },\n          returnType :'Array'\n        });\n        this.body.data.nodes.update({\n          id: e.node,\n          label : label_info[0].hover_label,\n          hover_label : label_info[0].label\n        });\n      }"}},"evals":["events.hoverNode","events.blurNode"],"jsHooks":[]}</script><!--/html_preserve-->




```r
make(plan)
```

```
##  [1] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P1_summary.txt"   
##  [2] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P2_summary.txt"   
##  [3] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P3_summary.txt"   
##  [4] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P_4_summary.txt"  
##  [5] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P_5_summary.txt"  
##  [6] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P_6_summary.txt"  
##  [7] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P_7_summary.txt"  
##  [8] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P_8_summary.txt"  
##  [9] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P_9_summary.txt"  
## [10] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P_10_summary.txt" 
## [11] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P_11_summary.txt" 
## [12] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P_12b_summary.txt"
## [13] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P_13_summary.txt" 
## [14] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P_14_summary.txt" 
## [15] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P_15_summary.txt" 
## [16] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P_17_summary.txt" 
## [17] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P_18_summary.txt" 
## [18] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P_19_summary.txt" 
## [19] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P_20_summary.txt" 
## [20] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P_21_summary.txt" 
## [21] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P_22_summary.txt" 
## [22] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P_23_summary.txt" 
## [23] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P_24_summary.txt" 
## [24] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P_25_summary.txt" 
## [25] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P_26b_summary.txt"
## [26] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P_27_summary.txt" 
## [27] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P_28_summary.txt" 
## [28] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P_29_summary.txt" 
## [29] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P_30_summary.txt" 
## [30] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/P_31_summary.txt" 
##  [1] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/A_1_summary.txt"  
##  [2] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/A_2_summary.txt"  
##  [3] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/A_3_summary.txt"  
##  [4] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/A_4_summary.txt"  
##  [5] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/A_5_summary.txt"  
##  [6] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/A_6_summary.txt"  
##  [7] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/A_7_summary.txt"  
##  [8] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/A_8_summary.txt"  
##  [9] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/A_9_summary.txt"  
## [10] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/A_10b_summary.txt"
## [11] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/A_11_summary.txt" 
## [12] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/A_12_summary.txt" 
## [13] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/A_13_summary.txt" 
## [14] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/A_14_summary.txt" 
## [15] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/A_15_summary.txt" 
## [16] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/A_16_summary.txt" 
## [17] "/Users/nicholashedger/Google Drive/EYETRACK_DATA/FREEVIEW/A_17_summary.txt" 
##  [1] "/Users/nicholashedger/Google Drive/Grafix/smooth_001.csv"
##  [2] "/Users/nicholashedger/Google Drive/Grafix/smooth_002.csv"
##  [3] "/Users/nicholashedger/Google Drive/Grafix/smooth_003.csv"
##  [4] "/Users/nicholashedger/Google Drive/Grafix/smooth_004.csv"
##  [5] "/Users/nicholashedger/Google Drive/Grafix/smooth_005.csv"
##  [6] "/Users/nicholashedger/Google Drive/Grafix/smooth_006.csv"
##  [7] "/Users/nicholashedger/Google Drive/Grafix/smooth_007.csv"
##  [8] "/Users/nicholashedger/Google Drive/Grafix/smooth_008.csv"
##  [9] "/Users/nicholashedger/Google Drive/Grafix/smooth_009.csv"
## [10] "/Users/nicholashedger/Google Drive/Grafix/smooth_010.csv"
## [11] "/Users/nicholashedger/Google Drive/Grafix/smooth_011.csv"
## [12] "/Users/nicholashedger/Google Drive/Grafix/smooth_012.csv"
## [13] "/Users/nicholashedger/Google Drive/Grafix/smooth_013.csv"
## [14] "/Users/nicholashedger/Google Drive/Grafix/smooth_014.csv"
## [15] "/Users/nicholashedger/Google Drive/Grafix/smooth_015.csv"
## [16] "/Users/nicholashedger/Google Drive/Grafix/smooth_017.csv"
## [17] "/Users/nicholashedger/Google Drive/Grafix/smooth_018.csv"
## [18] "/Users/nicholashedger/Google Drive/Grafix/smooth_019.csv"
## [19] "/Users/nicholashedger/Google Drive/Grafix/smooth_020.csv"
## [20] "/Users/nicholashedger/Google Drive/Grafix/smooth_021.csv"
## [21] "/Users/nicholashedger/Google Drive/Grafix/smooth_022.csv"
## [22] "/Users/nicholashedger/Google Drive/Grafix/smooth_023.csv"
## [23] "/Users/nicholashedger/Google Drive/Grafix/smooth_024.csv"
## [24] "/Users/nicholashedger/Google Drive/Grafix/smooth_025.csv"
## [25] "/Users/nicholashedger/Google Drive/Grafix/smooth_026.csv"
## [26] "/Users/nicholashedger/Google Drive/Grafix/smooth_027.csv"
## [27] "/Users/nicholashedger/Google Drive/Grafix/smooth_028.csv"
## [28] "/Users/nicholashedger/Google Drive/Grafix/smooth_029.csv"
## [29] "/Users/nicholashedger/Google Drive/Grafix/smooth_030.csv"
## [30] "/Users/nicholashedger/Google Drive/Grafix/smooth_031.csv"
## Loading cleaned ASC eye-tracking data.. 
##  [1] "/Users/nicholashedger/Google Drive/Grafix/smooth_101.csv"
##  [2] "/Users/nicholashedger/Google Drive/Grafix/smooth_102.csv"
##  [3] "/Users/nicholashedger/Google Drive/Grafix/smooth_103.csv"
##  [4] "/Users/nicholashedger/Google Drive/Grafix/smooth_104.csv"
##  [5] "/Users/nicholashedger/Google Drive/Grafix/smooth_105.csv"
##  [6] "/Users/nicholashedger/Google Drive/Grafix/smooth_106.csv"
##  [7] "/Users/nicholashedger/Google Drive/Grafix/smooth_107.csv"
##  [8] "/Users/nicholashedger/Google Drive/Grafix/smooth_108.csv"
##  [9] "/Users/nicholashedger/Google Drive/Grafix/smooth_109.csv"
## [10] "/Users/nicholashedger/Google Drive/Grafix/smooth_110.csv"
## [11] "/Users/nicholashedger/Google Drive/Grafix/smooth_111.csv"
## [12] "/Users/nicholashedger/Google Drive/Grafix/smooth_112.csv"
## [13] "/Users/nicholashedger/Google Drive/Grafix/smooth_113.csv"
## [14] "/Users/nicholashedger/Google Drive/Grafix/smooth_114.csv"
## [15] "/Users/nicholashedger/Google Drive/Grafix/smooth_115.csv"
## [16] "/Users/nicholashedger/Google Drive/Grafix/smooth_116.csv"
## [17] "/Users/nicholashedger/Google Drive/Grafix/smooth_117.csv"
##     
##          1     2
##   1  21598     0
##   2  21560     0
##   3  21591     0
##   4  21605     0
##   5  21615     0
##   6  21599     0
##   7  21588     0
##   8  21600     0
##   9  21586     0
##   10 21591     0
##   11 21596     0
##   12 21580     0
##   13 21599     0
##   14 21607     0
##   15 21600     0
##   16 21596     0
##   17 21600     0
##   18 21583     0
##   19 21611     0
##   20 21610     0
##   21 21622     0
##   22 21589     0
##   23 21595     0
##   24 21601     0
##   25 21590     0
##   26 21606     0
##   27 21604     0
##   28 21599     0
##   29 21600     0
##   30 21589     0
##   31     0 21601
##   32     0 21596
##   33     0 21613
##   34     0 21606
##   35     0 21611
##   36     0 21594
##   37     0 21587
##   38     0 21597
##   39     0 21609
##   40     0 21606
##   41     0 21596
##   42     0 21606
##   43     0 21574
##   44     0 21595
##   45     0 21582
##   46     0 21594
##   47     0 21580
## Fitting 2 (g)lmer() models:
## [..]
## [1] 1
## Fitting 4 (g)lmer() models:
## [....]
## [1] 2
## Fitting 4 (g)lmer() models:
## [....]
## Fitting 4 (g)lmer() models:
## [....]
## [1] 1
## Fitting 8 (g)lmer() models:
## [........]
## [1] 2
## Fitting 8 (g)lmer() models:
## [........]
```

