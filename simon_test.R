
GET_FILEVEC <- function(prefixsum,pathsum) {
  filevec=list.files(path=pathsum,pattern=prefixsum,full.names = TRUE)
  filevec=filevec[order(parse_number(list.files(path=pathsum,pattern=prefixsum,full.names = TRUE)))]
  return(filevec)
}


GET_NT_SUM <- function(prefixsum,pathsum) {
  filevec=list.files(path=pathsum,pattern=prefixsum,full.names = TRUE)
  filevec=filevec[order(parse_number(filevec))]
  DATA=data.frame()
  
  for (i in 1:length(filevec)){
    print(i)
    print(filevec[i])
    DATA=rbind(cbind(read.table(file=filevec[i], header = FALSE,blank.lines.skip=FALSE,sep=",",fill=TRUE),i,1),DATA)

  }
  return(DATA)
}


GET_ASC_SUM <- function(prefixsum,pathsum) {
  fprintf('Loading ASC trial matrix... \n',file='log.txt', append = TRUE)
  filevec=list.files(path=pathsum,pattern=prefixsum,full.names = TRUE)
  filevec=filevec[order(parse_number(filevec))]
  fprintf('Found %i files \n',length(filevec),file='log.txt', append = TRUE)
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
  DATA=data.frame()
  for (i in 1:length(filevec)){
    DATA=rbind(cbind(read.table(file=filevec[i], header = FALSE,blank.lines.skip=FALSE,sep=",",fill=TRUE),i,2),DATA)
    fprintf('Loading file %s \n ',filevec[i],file='log.txt', append = TRUE)
  }
  return(DATA)
}


const=new.env()
# Path to the Grafix files.
assign("Grafpath","/Users/nicholashedger/Google\ Drive/SIMON_DATA/FREEVIEW/GRAFIX",envir=const)

# Path to the summary files
assign("Sumpath","/Users/nicholashedger/Google\ Drive/SIMON_DATA/FREEVIEW/SUMMARY",envir=const)

# Path to the demographic data.
assign("Demopath","/Users/nicholashedger/Google\ Drive/EYETRACK_DEMO/",envir=const)

# Base path.
assign("basepath",'/Users/nicholashedger/Documents/DRAKE_DEM',envir=const)

# Prefix given to the NT summary files.
assign("PrefixsumNT","1001",envir=const)


# Prefix given to the xls file of demographic data: NT
assign("PrefixdemoNT","NT",envir=const)

# Prefix given to the Grafix files: NT
assign("PrefixgrafNT","smooth_0",envir=const)




legit=GET_NT_SUM(const$Sumpath,const$PrefixsumNT) 



filevec=list.files(path=const$Sumpath,pattern=const$PrefixsumNT,full.names = TRUE)
filevec=filevec[order(parse_number(filevec))]

read.table(filevec[8], header = FALSE,blank.lines.skip=FALSE,sep=",",fill=TRUE)




setwd(const$basepath)

plan <- drake_plan(
  filevec_NT=GET_FILEVEC(const$PrefixsumNT,const$Sumpath),
  raw_data_NT = GET_NT_SUM(const$PrefixsumNT,const$Sumpath),
  raw_data_NT_GRAF = GET_NT_GRAF(const$PrefixgrafNT,const$Grafpath),
  bound_data_NT=BIND_VARS(raw_data_NT,raw_data_NT_GRAF,const$colnames),
  DEMOFRAME_NT=GET_DEMO(const$Demopath,const$PrefixdemoNT,bound_data_NT,filevec_NT,const$DEMOVARS),
  AOI_FRAME=DEFINE_AOI(DEMOFRAME_NT,AOIdef$rectlxmin,AOIdef$rectlxmax,AOIdef$rectlymin,AOIdef$rectlymax,AOIdef$rectrxmin,AOIdef$rectrxmax,AOIdef$rectrymin,AOIdef$rectrymax,formatdef$TRIML,formatdef$TRIMU),
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