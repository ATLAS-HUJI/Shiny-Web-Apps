library(shiny)
library(shinythemes)
library(RMySQL)
library(DT)
library(plotly)
library(reshape)
library(leaflet)
require(RCurl)
library(stringr)
library(data.table)
library(RMySQL)

#Sys.setlocale(category = "LC_ALL", locale = "UK")

# Define UI for app that draws a histogram ----
ui <- fluidPage(theme=shinytheme("cosmo"),
  
  # App title ----
  titlePanel("ATLAS-Monitor"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      uiOutput("dynSlider"),
      textInput("db_ip","Host IP", value = "52.18.81.255"),
      textInput("db_usr","User", value = ""),
      passwordInput("db_pwd", "Password", value = ""),
      actionButton("con_db", "Connect")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Plots ----
      tabsetPanel(
        tabPanel("Beacons",br(),plotlyOutput("BCLPlot"),hr(),plotlyOutput("BCDPlot")),
        tabPanel("Basestations",br(),plotlyOutput("BSPlot"),hr(),tags$b("Base Station Summaries"),br(),br(),DT::dataTableOutput("BSSum")),
        tabPanel("Tags", br(),plotlyOutput("TAGLPlot"),hr(),plotlyOutput("TAGDPlot")),
        tabPanel("Tag Summaries",tags$b("Tag Summaries"),br(),br(),textOutput("activetags"),br(),DT::dataTableOutput("TAGSum"))
      )
      
    )
  )
)
# Define server logic  ----
server <- function(input, output, session) {
  
  td <- Sys.Date() #today
  ndays <- 14 #number of days
  
  observeEvent(input$con_db, {
    #urls
    track_url <- "https://webapps:Ingo2018@svn.cs.tau.ac.il/stoledo/projects/atlas/atlas-configuration/hula/track.txt"
    tag_url <- "https://webapps:Ingo2018@svn.cs.tau.ac.il/stoledo/projects/atlas/atlas-configuration/hula/tags.txt"
    vhs_url <- "https://webapps:Ingo2018@svn.cs.tau.ac.il/stoledo/projects/atlas/atlas-configuration/hula/vh-schedules.txt"
    vh_url <- "https://webapps:Ingo2018@svn.cs.tau.ac.il/stoledo/projects/atlas/atlas-configuration/hula/vildehaye.txt"
    
    #read data from track.txt on svn to determine active tags
    con<-textConnection(getURL(track_url))
    track<-read.table(con,  header = FALSE, sep = ",", quote = "#", strip.white=T, skip=1)
    close(con)
    active <- track$V2 
    rm(track)
    
    #read data from tags.txt on svn
    con<-textConnection(getURL(tag_url))
    res<-NULL
    while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) 
    {
      if (substr(str_trim(oneLine),1,3) =="Tag")
      {
        res<-c(res,oneLine)
      } 
    } 
    close(con)
    tags<-read.table(textConnection(res),fill=T, sep=",")
    v1<-data.table(Tag=tags$V4,Rate=3600/tags$V6)
    rm(tags)
    
    #read data from vh-schedules.txt on svn
    con<-textConnection(getURL(vhs_url))
    vhs<-read.table(con,  header = FALSE, sep = ",", quote = "#", skip=1, fill=T)
    close(con)
    vhs<-vhs[complete.cases(vhs),]
    v2<-data.table(Tag=vhs$V8,Rate=3600/vhs$V11)
    rm(vhs)
    
    #read data from vildehaye.txt on svn
    con<-textConnection(getURL(vh_url))
    vh<-read.table(con,  header = FALSE, sep = ",", quote = "#", skip=1, fill=T)
    vh <- vh[vh$V1=="VHTag",2:3]
    vh <- vh[vh$V3!="",]
    vh$V4 <- NA
    vh$V4[grep("_120",vh$V3)] <- 120
    vh$V4[grep("_75",vh$V3)] <- 75
    v2c <- data.table(Tag=as.numeric(as.character(vh$V2)),Channel=vh$V4)
    rm(vh)
    
    v2<-merge (v2,v2c,by='Tag',all.x=T)
    v2$Tag[which(is.na(v2$Channel))]-972001000000
    
    #get date for last 14 days
    et <- as.numeric(as.POSIXct(Sys.Date(), tz="UTC", origin="1970-01-01"))*1000
    st <- et -ndays*24*60*60*1000
    
    tv <- seq(from=st,to=et,by=60*60*1000)
    dv <- as.POSIXct(seq(from=st,to=et,by=60*60*1000)/1000, tz="UTC", origin="1970-01-01")
    nv <- which(as.numeric(substr(as.character(dv),12,13)) > 18 | as.numeric(substr(as.character(dv),12,13)) <= 6)
    sv <- rep(0,length(dv)) 
    sv[nv]<-1
    dc_dt <- data.table(DATE=dv,DN=sv)
    rm(tv,dv,nv,sv)
    
    con <- dbConnect(MySQL(),
                     user = input$db_usr,
                     password = input$db_pwd,
                     host = input$db_ip,
                     dbname = 'atlas')
    
    #get tag summaries
    sql <- paste("SELECT * FROM hourly_tag_summaries WHERE HOUR >", st, sep="")
    rs <- dbSendQuery(con, sql)
    loc_dt = as.data.table(fetch(rs, n=-1))
    
    #get base station summaries
    sql <- paste("SELECT * FROM hourly_tag_bs_summaries WHERE HOUR >", st, sep="")
    rs <- dbSendQuery(con, sql)
    bs_dt = as.data.table(fetch(rs, n=-1))
    
    dbDisconnect(con)
    #Beacon Localization Plot
    
    #beacon numbers
    b1 <- 1001000001 #beacon 1
    b2 <- 1001000003 # beacon 3
    b3 <- 972001000005 # beacon 5 
    
    output$BCLPlot <- renderPlotly({
    
      #beacon localizations
      b1x <- which(loc_dt$TAG==b1)
      b2x <- which(loc_dt$TAG==b2)
      b3x <- which(loc_dt$TAG==b3)
      bcx <- c(b1x,b2x,b3x)
      bc_dt <- loc_dt[bcx,]
      bc_dt$RATE <- 3600
      bc_dt$PRC <- bc_dt$LOCALIZATIONS / bc_dt$RATE
      bc_dt$DATE <- as.POSIXct(bc_dt$HOUR/1000, tz="UTC", origin="1970-01-01")
      bc_dt<-bc_dt[order(bc_dt$HOUR),]
      p<-plot_ly() %>%
        add_ribbons(data=dc_dt, x~DATE, ymin=0, ymax=~DN, line = list(color = 'rgba(80,80,80,0)'), fillcolor = "rgba(80,80,80,0.1)", name = "Night") %>%
        add_lines(data=bc_dt,x=~DATE, y=~PRC, type = 'scatter', mode='lines', split=~TAG) %>%
        layout(title = 'Beacon Localization Performance', xaxis = list(title="Date", range = c(min(dc_dt$DATE),et)),yaxis = list(title=" Localization Rate", range=c(0,1),tickformat = "%"))
      
      p$elementId <- NULL
      p
    })
    output$BCDPlot <- renderPlotly({
      
      #beacon detections
      b1x <- which(bs_dt$TAG==b1)
      b2x <- which(bs_dt$TAG==b2)
      b3x <- which(bs_dt$TAG==b3)
      bcx <- c(b1x,b2x,b3x)
      bc_dt <- bs_dt[bcx,]
      bc_dt$RATE <- 3600
      bc_dt$PRC <- bc_dt$DETECTIONS / bc_dt$RATE
      bc_adt<-aggregate(PRC~HOUR+TAG,bc_dt,FUN=mean)
      bc_adt$DATE <- as.POSIXct(bc_adt$HOUR/1000, tz="UTC", origin="1970-01-01")
      bc_adt<-bc_adt[order(bc_adt$HOUR),]
      p<-plot_ly() %>%
        add_ribbons(data=dc_dt, x~DATE, ymin=0, ymax=~DN, line = list(color = 'rgba(80,80,80,0)'), fillcolor = "rgba(80,80,80,0.1)", name = "Night") %>%
        add_lines(data=bc_adt,x=~DATE, y=~PRC, type = 'scatter', mode='lines', split=~TAG) %>%
        layout(title = 'Beacon Detection Performance', xaxis = list(title="Date", range = c(min(dc_dt$DATE),et)),yaxis = list(title=" Detection Rate", range=c(0,1),tickformat = "%"))
      
      p$elementId <- NULL
      p
      })
    output$BSPlot <- renderPlotly({
      #beacon detections
      b1x <- which(bs_dt$TAG==b1)
      b2x <- which(bs_dt$TAG==b2)
      b3x <- which(bs_dt$TAG==b3)
      bcx <- c(b1x,b2x,b3x)
      bc_dt <- bs_dt[bcx,]
      bc_dt$RATE <- 3600
      bc_dt$PRC <- bc_dt$DETECTIONS / bc_dt$RATE
      bs_adt<-aggregate(PRC~HOUR+BS,bc_dt,FUN=mean)
      bs_adt<-bs_adt[order(bs_adt$HOUR),]
      bs_adt$DATE <- as.POSIXct(bs_adt$HOUR/1000, tz="UTC", origin="1970-01-01")
      p<-plot_ly() %>%
        add_ribbons(data=dc_dt, x~DATE, ymin=0, ymax=~DN, line = list(color = 'rgba(80,80,80,0)'), fillcolor = "rgba(80,80,80,0.1)", name = "Night") %>%
        add_lines(data=bs_adt,x=~DATE, y=~PRC, type = 'scatter', mode='lines', split=~BS) %>%
        layout(title = 'Base Station Performance', xaxis = list(title="Date", range = c(min(dc_dt$DATE),et)),yaxis = list(title=" Detection Rate", range=c(0,1),tickformat = "%"))
      p$elementId <- NULL
      p
    })
    
    #base station summary
    output$BSSum <- DT::renderDataTable({ 

      bs_dt <- bs_dt[which(bs_dt$TAG > 972001000000),]
      v2_hr <- v2[which(!duplicated(v2$Tag)),]
      bs_dt <- merge (bs_dt,v2_hr, by.x ='TAG', by.y ='Tag', all.x=T)
      bs_dt$PRC <- bs_dt$DETECTIONS/bs_dt$Rate
      edx<-which(bs_dt$PRC>1)
      if (length(edx)>0)
      {
        bs_dt$PRC[edx]<-bs_dt$PRC[edx]/2
      }
      bs_dt$CNT <- 1
      bs_adt <- aggregate(PRC~BS+HOUR,bs_dt,FUN=mean)
      bs_adt <- aggregate(PRC~BS,bs_adt,FUN=mean)
      bs_add <- aggregate(CNT~BS+HOUR,bs_dt,FUN=sum)
      bs_add <- aggregate(CNT~BS,bs_add,FUN=mean)
      bs_adt <- merge(bs_adt,bs_add,by='BS')
      bs_adt$PRC <- round(bs_adt$PRC,digits=2)
      bs_adt$CNT <- round(bs_adt$CNT)
      colnames(bs_adt)<-c('Base Station','Avg. Detection Rate','Avg. No. of Tags')
      datatable(bs_adt,rownames=FALSE,options = list(pageLength = 20))
      
      })
    
    output$TAGLPlot <- renderPlotly({
      v2_dt <- loc_dt[which(loc_dt$TAG > 972001000000),]
      v2_dt$sTAG <- v2_dt$TAG-972001000000
      v2_hr <- v2[which(!duplicated(v2$Tag)),]
      v2_dt <- merge (v2_dt,v2_hr, by.x ='TAG', by.y ='Tag', all.x=T)
      v2_dt$Performance <- v2_dt$LOCALIZATIONS/v2_dt$Rate
      v2_dt$COUNT<-1
      edx<-which(v2_dt$Performance>1)
      if (length(edx)>0)
      {
        v2_dt$Performance[edx]<-v2_dt$Performance[edx]/2
      }
      v2_adc <- aggregate(COUNT~HOUR,v2_dt,FUN=sum)
      v2_adt <- aggregate(Performance~HOUR+Rate,v2_dt,FUN=mean)
      v2_adt$DATE <- as.POSIXct(v2_adt$HOUR/1000, tz="UTC", origin="1970-01-01")
      v2_adc$DATE <- as.POSIXct(v2_adc$HOUR/1000, tz="UTC", origin="1970-01-01")
      v2_adt<-v2_adt[order(v2_adt$HOUR),]
      v2_adt$Rate <- 3600/v2_adt$Rate
      p<-plot_ly() %>%
        add_ribbons(data=dc_dt, x~DATE, ymin=0, ymax=~DN, line = list(color = 'rgba(80,80,80,0)'), fillcolor = "rgba(80,80,80,0.1)", name = "Night") %>%
        add_lines(data=v2_adt,x=~DATE, y=~Performance, type = 'scatter', mode='lines', split=~Rate) %>%
        add_lines(data=v2_adc,x=~DATE, y=~COUNT, type='scatter', mode='lines', line = list(color = 'rgba(0,0,0,1)'),name ="Tags", yaxis = "y2") %>%
        layout(title = 'Tag Localization Performanceby Rate', 
               xaxis = list(title="Date", range = c(min(dc_dt$DATE),et)),
               yaxis = list(title=" Localization Rate", range=c(0,1),tickformat = "%"),
               yaxis2 = list(title="Est. No. Tags", overlaying = "y",side = "right", range = c(0,120)))
      p$elementId <- NULL
      p
    })
    
    output$TAGDPlot <- renderPlotly({
      #worst tags
      v2_dt <- bs_dt[which(bs_dt$TAG > 972001000000),]
      v2_dt$sTAG <- v2_dt$TAG-972001000000
      v2_hr <- v2[which(!duplicated(v2$Tag)),]
      v2_dt <- merge (v2_dt,v2_hr, by.x ='TAG', by.y ='Tag', all.x=T)
      v2_dt$Performance <- v2_dt$DETECTIONS/v2_dt$Rate
      v2_dt$COUNT<-1
      edx<-which(v2_dt$Performance>1)
      if (length(edx)>0)
      {
        v2_dt$Performance[edx]<-v2_dt$Performance[edx]/2
      }
      v2_adc <- aggregate(COUNT~HOUR+TAG,v2_dt,FUN=sum)
      v2_adc$COUNT<-1
      v2_adc <- aggregate(COUNT~HOUR,v2_adc,FUN=sum)
      v2_adt <- aggregate(Performance~HOUR+TAG+Rate,v2_dt,FUN=mean)
      v2_adt<- aggregate(Performance~HOUR+Rate,v2_adt,FUN=mean)
      v2_adt$DATE <- as.POSIXct(v2_adt$HOUR/1000, tz="UTC", origin="1970-01-01")
      v2_adc$DATE <- as.POSIXct(v2_adc$HOUR/1000, tz="UTC", origin="1970-01-01")
      v2_adt<-v2_adt[order(v2_adt$HOUR),]
      v2_adt$Rate <- 3600/v2_adt$Rate
      
      p<-plot_ly() %>%
        add_ribbons(data=dc_dt, x~DATE, ymin=0, ymax=~DN, line = list(color = 'rgba(80,80,80,0)'), fillcolor = "rgba(80,80,80,0.1)", name = "Night") %>%
        add_lines(data=v2_adt,x=~DATE, y=~Performance, type = 'scatter', mode='lines', split=~Rate) %>%
        add_lines(data=v2_adc,x=~DATE, y=~COUNT, type='scatter', mode='lines', line = list(color = 'rgba(0,0,0,1)'),name ="Tags", yaxis = "y2") %>%
        layout(title = 'Tag Detection Performance by Rate', 
               xaxis = list(title="Date", range = c(min(dc_dt$DATE),et)),
               yaxis = list(title=" Detection Rate", range=c(0,1),tickformat = "%"),
               yaxis2 = list(title="Est. No. Tags", overlaying = "y",side = "right", range = c(0,120)))
      p$elementId <- NULL
      p
    })
    
    #tag summaries
    output$TAGSum <- DT::renderDataTable({ 
      
      bs_dt <- bs_dt[which(bs_dt$TAG > 972001000000),]
      v2_hr <- v2[which(!duplicated(v2$Tag)),]
      bs_dt <- merge (bs_dt,v2_hr, by.x ='TAG', by.y ='Tag', all.x=T)
      bs_dt$PRC <- bs_dt$DETECTIONS/bs_dt$Rate
      edx<-which(bs_dt$PRC>1)
      if (length(edx)>0)
      {
        bs_dt$PRC[edx]<-bs_dt$PRC[edx]/2
      }
      bs_dt$CNT <- 1
      bs_adt <- aggregate(PRC~TAG+HOUR,bs_dt,FUN=mean)
      bs_adt <- aggregate(PRC~TAG,bs_adt,FUN=mean)
      bs_add <- aggregate(CNT~TAG+HOUR,bs_dt,FUN=sum)
      bs_add <- aggregate(CNT~TAG,bs_add,FUN=mean)
      bs_adt <- merge(bs_adt,bs_add,by='TAG')
      bs_adt$PRC <- round(bs_adt$PRC,digits=2)
      bs_adt$CNT <- round(bs_adt$CNT)
      
      #sort data worst to best for visibility
      bs_adt <- bs_adt[order(bs_adt$CNT,bs_adt$PRC),]
      #show only active tags
      adx <- which(bs_adt$TAG %in% active)
      bs_adt<-bs_adt[adx,]
      output$activetags <- renderText({
        return(paste(nrow(bs_adt),"Active Tags"))
      })
      
      colnames(bs_adt)<-c('Tag','Avg. Detection Performance','Avg. No. of Basestations')
      datatable(bs_adt,rownames=FALSE,options = list(pageLength = 20)) %>%
      formatStyle('Avg. No. of Basestations', target = 'row', fontWeight = styleEqual(0, "bold")) %>%
      formatStyle('Avg. Detection Performance', target = 'row', color = styleInterval(c(0,0.5), c('red','orange','green')))
      
    })
    
  })
}
shinyApp(ui = ui, server = server)
