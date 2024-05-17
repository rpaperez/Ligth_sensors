# SCRITP TO READ AND VISUALIZE THE DATA OF PAR SENSORS FROM THE PH --------
### R. PEREZ, 16 Mai 2024


# Load packages -----------------------------------------------------------
packs <- c("lubridate", "stringr", 'tidyverse','viridis','data.table','cowplot')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack)) }
lapply(packs, InstIfNec)


# inputs ------------------------------------------------------------------

### Apogee sensors coefficients

coefPAR=71.0 #micromol.m-2.s-1 per mV
coefFR=51.45 #micromol.m-2.s-1 per mV

## vector of info names in the RawData column
nbSlots=7
names=paste0(c('slot','value'),rep(x = c(1:nbSlots),each=2))

vecName= c('station',names,'E')

tableSlot=data.frame(sensor=c("T0 S2","T1 S3","T2 S1","T3 S1","T4 S1","T5 S1","T6 S1"),
                     variable=c('Battery','Temperature','PAR','FR','-','--','---'))


## start time

start=ymd_hms('2024-05-03 15:01:00')
# load the data -----------------------------------------------------------

don_raw=data.table::fread(input = '0-data/DATA.CSV') %>% 
  mutate(time=start+Timestamp,
           ref=str_sub(RawData,start=4,end=str_length(RawData)-1)) %>% 
  tidyr::separate(col = ref,into =vecName ,sep='#') 

don=don_raw %>% select(ID,Timestamp,Days,Hours,Minutes,time,RawData)
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
mutate(tension=as.numeric(tension))


donF=merge(donF,tableSlot)

donF %>%
  filter(variable %in% c('PAR','FR','Battery','Temperature')) %>% 
  mutate(tension=ifelse(sensor=='T3 S1',tension*coefFR,tension)) %>%
  mutate(tension=ifelse(sensor=='T2 S1',tension*coefPAR,tension)) %>%
  ggplot(aes(x=time,y=tension,col=sensor))+
  geom_line()+
  facet_wrap(~variable,scale='free_y')+
  scale_x_datetime()+
  labs(y='')

