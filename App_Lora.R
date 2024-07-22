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
                                  # "Temperature" = "Temperature", 
                                  'Battery'='Battery')),
                 
                 
                 selectInput("Loggers", "Select logger(s):",
                             list('All'='All',
                                  'C1'='C1',
                                  'C2'='C2',
                                  'C3'='C3')),
                 
                 selectInput("Sensors", "Select sensor(s):",
                             list('All'='All',
                                  'T1'='T1',
                                  'T2'='T2',
                                  'T3'='T3',
                                  'T4'='T4',
                                  'T5'='T5'
                                  ))
                 
 
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



server<-function(input, output) {
  output$contents <- reactive({
    
    # Inputs
    file<- reactive({
      shiny::validate(shiny::need(!is.null(input$file1),
                                  "Please select a file"))
      
      res= list(path=input$file1$datapath,name=input$file1$name)
      # print(res)
      return(res)
    })
    
    
    
    if (is.null(file()$path)){
      return(NULL)
    }
    
    
    
    # import data -------------------------------------------------------------
    
    don <- reactive({
      
      allDat=NULL
      if(!is.null(file()$path)){
        
        for (f in 1:length(file()$name)){
          
          fileName=file()$name[f]
          
          print(paste('open',fileName))
          
          don_raw=data.table::fread(input =  file()$path[f]) %>% 
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
                   DATA=fileName,
                   Logger=str_sub(COM,1,2),
                   sensor=str_remove(sensor,' S1'),
                   sensor=str_remove(sensor,' S4'))
          
  
          
          donF=merge(donF,tableSlot)
          donF=merge(donF,tableCoeff)
          allDat=rbind(allDat,donF) %>% 
            mutate(value=tension*Coeff)
          
        }
        
        print(head(allDat))
        
        return(allDat)
      }
      
    })
    
    
    # date selection ----------------------------------------------------------
    
    
    # if (is.null(input$Time)){
    
    
    
    output$Dates<- renderUI({
      
      if (is.null(don())){
        return(NULL)
      }
      # print(don())
      
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
      
      # if(input$variable=='Temperature'){
      #   # ylab=expression(Temperature~(degree~C))
      #   ylab='Temperature (°C)'}
      if(input$variable=='Battery'){
        ylab='Battery (V)'}
      if(input$variable=='PAR'){
        ylab='PAR (micro mol m-2 s-1)'}
      if(input$variable=='FR'){
        ylab='Far Red (micro mol m-2 s-1)'}
      # 
      

      Sensors=input$Sensors
      print(paste('Sensors',Sensors))  
      if(Sensors=='All'){
        Sensors=c('T0','T1','T2','T3','T4','T5')
      }
      

      Loggers=input$Loggers
      print(paste('Loggers',Loggers))  
      if(Loggers=='All'){
        Loggers=c('C1','C2','C3')
      }

        
      ###select Dates
      
      # isolate(
      minDate <- input$Time[1]
      maxDate <- input$Time[2]
      # )
      
      graph=don() %>%
        filter(variable %in% input$variable) %>%
        filter(sensor %in% Sensors) %>%
        filter(Logger %in% Loggers) %>%
        filter(Date>=minDate & Date<=maxDate)%>%
        ggplot(aes(x=time,y=value,col=sensor,group=paste(sensor,Logger,DATA)))+
        geom_line()+
        facet_grid(~Logger,scale='free_y')+
        scale_x_datetime()+
        labs(y=ylab)
      
      return(graph)
    })
    
  })
}
# Run app -------------------------------
shinyApp(ui = ui, server = server)