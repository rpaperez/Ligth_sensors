# SCRITP TO READ AND VISUALIZE THE DATA OF PAR SENSORS FROM THE PH --------
### R. PEREZ, 16 Mai 2024


# Load packages -----------------------------------------------------------
packs <- c("lubridate", "stringr", 'tidyverse','viridis','data.table','cowplot','plotly')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack)) }
lapply(packs, InstIfNec)


# inputs ------------------------------------------------------------------

### Apogee sensors coefficients

coefPAR1=71.0 #micromol.m-2.s-1 per mV
coefFR1=51.45 #micromol.m-2.s-1 per mV
coefPAR2=62.57 #micromol.m-2.s-1 per mV
coefFR2=33.16 #micromol.m-2.s-1 per mV
coefPAR=10.0 #micromol.m-2.s-1 per mV

tableCoeff=data.frame(variable=c('Battery','PAR','PAR1','PAR2','FR1','FR2'),
                      Coeff=c(1,coefPAR,coefPAR1,coefPAR2,coefFR1,coefFR2))

## vector of info names in the RawData column
nbSlots=6
names=paste0(c('slot','value'),rep(x = c(1:nbSlots),each=2))

vecName= c('Logger',names)

tableSlot_C1=data.frame(Logger='C1',
                        sensor=c("T0","T1","T2","T3","T4","T5"),
                        variable=c('Battery','PAR','PAR','PAR','PAR','PAR'))

tableSlot_C2=data.frame(Logger='C2',
                        sensor=c("T0","T1","T2","T3","T4","T5"),
                        variable=c('Battery','PAR','PAR','PAR','PAR','PAR'))

tableSlot=rbind(tableSlot_C1,tableSlot_C2)



# load the data -----------------------------------------------------------

allDat=NULL #init
files=list.files(path = '0-data/',pattern = 'LORA')

for (file in files){
  # file=files[1]
  

  don_raw=data.table::fread(input = paste0('0-data/',file)) %>% 
    data.frame() %>% 
    mutate(Date=dmy(Date),
           time=ymd_hms(paste(Date,Heure)),
           ref=str_sub(COM,start=1,end=str_length(COM)-1)) %>% 
    tidyr::separate(col = ref,into =vecName ,sep='#') 
     
   don=don_raw %>% select(Date,Heure,time,COM)

  for (i in 1: nbSlots){
    dsub=don_raw %>%
      mutate(info=paste(get(paste0('slot',i)),get(paste0('value',i))))
    
    colnames(dsub)[colnames(dsub)=='info']=paste0('info',i)
    
    don=cbind(don,dsub %>% 
                select(paste0('info',i)))
    
  }
  
  
  donF=don %>%
    tidyr::gather(key = 'info',value = 'value',contains('info')) %>% 
    mutate(value=str_remove(value,'_A11')) %>% 
    tidyr::separate(col = value,into = c('sensor','tension'),sep = '_') %>% 
    mutate(tension=as.numeric(tension),
           DATA=file,
           Logger=str_sub(COM,1,2),
           sensor=str_remove(sensor,' S1'),
           sensor=str_remove(sensor,' S4'))
  
  
  donF=merge(donF,tableSlot)
  donF=merge(donF,tableCoeff)
  allDat=rbind(allDat,donF) %>% 
    mutate(value=tension*Coeff)
  
}



# plots -------------------------------------------------------------------
VAR='Battery'
Sensors='T0'
Loggers='C1'

ggplotly(allDat %>% 
filter(variable %in% VAR) %>%
  filter(sensor %in% Sensors) %>%
  filter(Logger %in% Loggers) %>%
  ggplot(aes(x=time,y=value,col=sensor,group=paste(sensor,Logger,DATA)))+
  geom_line()+
  facet_grid(Logger~sensor,scale='free_y')+
  scale_x_datetime())
