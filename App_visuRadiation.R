# Load packages -----------------------------------------------------------


packs <- c('shiny','shinythemes','datasets',"lubridate", "stringr", "ggplot2",'dplyr','viridis','plotly','tidyr')
InstIfNec<-function (pack) {
  if (!do.call(require,as.list(pack))) {
    do.call(install.packages,as.list(pack))  }
  do.call(require,as.list(pack)) }
lapply(packs, InstIfNec)



# ui ----------------------------------------------------------------------
ui<-fluidPage(
  navbarPage(theme = shinytheme("united"),
             title="RadiationLog Viewer",
             sidebarLayout( 
               sidebarPanel(
                 fileInput('file1', 'Import data file(s) (.txt)',
                           accept=c('text/csv',
                                    'text/comma-separated-values,text/plain',
                                    '.csv'),multiple = TRUE),
                 
                 uiOutput("Dates"),
                 
                 selectInput("variable", "Select variable(s):",
                             list('PAR'='PAR',
                                  'Far red'='FR',
                                  "Temperature" = "Temperature", 
                                  'Battery'='Battery'))
                 
               ),
               mainPanel(
                 tableOutput('contents'),
                 plotlyOutput("graph")
               )
             )
  )
)






# server ------------------------------------------------------------------

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
donT=data.table::fread(input = '0-data/PAR Time.csv') %>%
  filter(File!='') %>% 
  mutate(start=dmy_hms(paste0(`Day of setting`,'_',`Setting Time`,':00')),
         end=dmy_hms(paste0(`Day of taking`,'_',`Taking time`,':00'))) %>% 
  data.frame() 


server<-function(input, output) {
  output$contents <- reactive({
    
    # Inputs
    file<- reactive({
      shiny::validate(shiny::need(!is.null(input$file1),
                                  "Please select a file"))
      
      res= list(path=input$file1$datapath,name=input$file1$name)
      print(res)
      return(res)
    })
    
    
    
    if (is.null(file()$path)){
      return(NULL)
    }
    
    
    
    # import data -------------------------------------------------------------
    
    don <- reactive({
      
      donF=NULL
      if(!is.null(file()$path)){
        
        for (f in 1:length(file()$name)){
          
          fileName=file()$name[f]
          # print(fileName)
          TimeStart=ymd_hms(donT[donT$File==str_remove(fileName,'.CSV'),'start'])
          
          don_raw=data.table::fread(input =  file()$path[f]) %>% 
            mutate(time=TimeStart+Timestamp,
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
          
          
          donF_sub=don %>%
            tidyr::gather(key = 'info',value = 'value',contains('info')) %>% 
            mutate(value=str_remove(value,'_A11')) %>% 
            tidyr::separate(col = value,into = c('sensor','value'),sep = '_') %>% 
            mutate(value=as.numeric(value),
                   DATA=fileName)
          
          
          donF_sub=merge(donF_sub,tableSlot) %>% 
            mutate(Date=str_sub(time,1,10),
                   hms=hms(str_sub(time,12,19)),
                   value=ifelse(sensor=='T3 S1',value*coefFR,value),
                   value=ifelse(sensor=='T2 S1',value*coefPAR,value))    
          
          donF=rbind(donF,donF_sub)
        }
        
        # print(donF)
        return(donF)
      }
      
    })
    
    
    # date selection ----------------------------------------------------------
    
    
    # if (is.null(input$Time)){
    
    
    
    output$Dates<- renderUI({
      
      if (is.null(don())){
        return(NULL)
      }
      print(don())
      
      dateRangeInput('Time', 'Select a date range:',
                     start =min(ymd(don()$Date)),
                     end = max(ymd(don()$Date)),
                     max =max(ymd(don()$Date)))
      
    })
    # }
    
    # graphics ----------------------------------------------------------------
    
    output$graph <- renderPlotly({
      
      if (is.null(don())){
        return(NULL)
      }
      
      if(input$variable=='Temperature'){
        # ylab=expression(Temperature~(degree~C))
        ylab='Temperature (°C)'}
      if(input$variable=='Battery'){
        ylab='Battery (V)'}
      if(input$variable=='PAR'){
        ylab='PAR (micro mol m-2 s-1)'}
      if(input$variable=='FR'){
        ylab='Far Red (micro mol m-2 s-1)'}
      
      ###select Dates
      # isolate(
      minDate <- input$Time[1]
      maxDate <- input$Time[2]
      # )
      
      graph=don() %>%
        filter(variable %in% input$variable) %>%
        filter(Date>=minDate & Date<=maxDate)%>%
        ggplot(aes(x=time,y=value,group=DATA))+
        geom_line()+
        facet_wrap(~variable,scale='free_y')+
        scale_x_datetime()+
        labs(y=ylab)
      
      return(graph)
    })
    
  })
}
# Run app -------------------------------
shinyApp(ui = ui, server = server)