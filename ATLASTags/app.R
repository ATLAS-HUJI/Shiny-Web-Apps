library(shiny)
library(shinythemes)
library(RMySQL)
library(DT)
library(plotly)
library(reshape)
library(leaflet)
#Sys.setlocale(category = "LC_ALL", locale = "UK")

# Define UI for app that draws a histogram ----
ui <- fluidPage(theme=shinytheme("cosmo"),
  
  # App title ----
  titlePanel("ATLAS-Deployment-Plan"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      uiOutput("dynSlider")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Plots ----
      tabsetPanel(
        tabPanel("Plot", plotlyOutput("tagPlot"),plotlyOutput("ratePlot")),
        tabPanel("Table", DT::dataTableOutput("deployTab")),
        tabPanel("Map", leafletOutput("bo2017map")),
        tabPanel("Login",      textOutput("datetoday"),
                 textOutput("activetags"),
                 textOutput("stat_tag"),
                 textOutput("stat_loc"),
                 textInput("db_usr","User", value = ""),
                 passwordInput("db_pwd", "Password", value = ""),
                 actionButton("con_db", "Connect"))
        
      )
      
    )
  )
)
# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  #basic code
  load("dply_df.RData")
  dply_df <- na.omit(dply_df)
  res = NULL
  td <- Sys.Date()
  
  if (!exists("input$days"))
  {
    min_date <- min(as.Date(dply_df$start_date,format="%d.%m.%Y"))
    max_date <- max(as.Date(dply_df$end_date,format="%d.%m.%Y"))
  }
  else
  {
    min_date <- input$days[1]
    max_date <- input$days[2]
  }
  
  #attempt at dynamic slider # RELOADS every time PROBLEM!!!!
  output$dynSlider <- renderUI({
    
    sliderInput("days",
                label = "Selected Days:",
                min = as.Date(min_date),
                max = as.Date(max_date),
                value = c(as.Date(min_date),as.Date(max_date))
    )
  })
  
  tab_res = NULL
  for (i in 1:nrow(dply_df))
  {
    sd <- as.Date(dply_df$start_date[i],format="%d.%m.%Y")
    ed <- as.Date(dply_df$end_date[i],format="%d.%m.%Y")
    tmp_date <- as.Date(sd:ed, origin="1970-01-01")
    Researcher <- rep(dply_df$researcher[i],length(tmp_date))
    Species <- rep(dply_df$species[i],length(tmp_date))
    if (dply_df$active[i] == "D") {
      Day <- rep(dply_df$ntags[i],length(tmp_date))
      Night <- rep(0,length(tmp_date))
    } else if (dply_df$active[i] == "N"){
      Day <- rep(0,length(tmp_date))
      Night <- rep(dply_df$ntags[i],length(tmp_date))
    } else {
      Day <- rep(dply_df$ntags[i],length(tmp_date))
      Night <- rep(dply_df$ntags[i],length(tmp_date))
    }
    tmp_tab <- data.frame(tmp_date,Researcher,Species,Day,Night)
    tab_res <- rbind(tab_res,tmp_tab)
  }
  tab_sum <- aggregate(cbind(Day, Night) ~ tmp_date, data=tab_res, FUN = sum)
  tab_sum$Researcher <- "Summary"
  tab_sum$Species <- ""
  tab_cmb <- rbind(tab_res, tab_sum)
  tab_cmb <- tab_cmb[with(tab_cmb, order(tmp_date, rev(Species))),]
  date <- as.Date(min_date:max_date, origin="1970-01-01")
  Day <- rep(0,length(date))
  Night <- rep(0,length(date))
  D.Rate <- rep(0,length(date))
  N.Rate <- rep(0,length(date))
  for (i in 1:nrow(dply_df))
  {
    sd <- as.Date(dply_df$start_date[i],format="%d.%m.%Y")
    ed <- as.Date(dply_df$end_date[i],format="%d.%m.%Y")
    tmp_vec <- as.Date(sd:ed, origin="1970-01-01")
    idx <- which(date %in% tmp_vec)
    if (dply_df$active[i] == "D") {
      Day[idx]<- Day[idx] + dply_df$ntags[i]
      D.Rate[idx]<- D.Rate[idx] + (dply_df$ntags[i]*dply_df$rate[i])
    } else  if(dply_df$active[i] == "N"){
      Night[idx]<- Night[idx] + dply_df$ntags[i]
      N.Rate[idx]<- N.Rate[idx] + (dply_df$ntags[i]*dply_df$rate[i])
    } else {
      Day[idx]<- Day[idx] + dply_df$ntags[i]
      D.Rate[idx]<- D.Rate[idx] + (dply_df$ntags[i]*dply_df$rate[i])
      Night[idx]<- Night[idx] + dply_df$ntags[i]
      N.Rate[idx]<- N.Rate[idx] + (dply_df$ntags[i]*dply_df$rate[i])
      }
  }
  plt_res <- data.frame(date,Day,Night,D.Rate,N.Rate)
  
  observeEvent(input$con_db, {
    con <- dbConnect(MySQL(),
                     user = input$db_usr,
                     password = input$db_pwd,
                     host = '52.48.90.213',
                     dbname = 'atlas')
    
    #determine last 24 hours
    h24e <- as.numeric(as.POSIXct(td, tz="UTC", origin="1970-01-01"))*1000
    h24s <- h24e -24*60*60*1000
    sql <- paste("SELECT * FROM hourly_tag_summaries WHERE HOUR >", h24s, " AND HOUR < ", h24e, sep="")
    rs <- dbSendQuery(con, sql)
    cd = fetch(rs, n=-1)
    output$activetags <- renderText({
      return(paste("Active Tags",length(unique(cd$TAG))))
    })
    #all data
    sql <- "SELECT * FROM hourly_tag_summaries"
    rs <- dbSendQuery(con, sql)
    ad = fetch(rs, n=-1)
    ntags <- length(unique(ad$TAG))
    nloc <- sum(ad$LOCALIZATIONS)
    output$stat_tag <- renderText({
      return(paste("Total No. of Tags:",ntags))
    })
    output$stat_loc <- renderText({
      return(paste("Total No. of Localizations",nloc))
    })
    dbDisconnect(con)
  })
  
  observeEvent(input$days,{
    output$tagPlot <- renderPlotly({
    
    sp <- which(plt_res$date == input$days[1])
    ep <- which(plt_res$date == input$days[2])
    
    odf <- data.frame(plt_res[sp:ep,])
    p<-plot_ly(odf,x=~date) %>%
      add_ribbons(ymin = 0, ymax = 60, line = list(color = 'rgba(130,229,153,1)'), fillcolor = "rgba(130,229,153,0.25)", name = "good") %>%
      add_ribbons(ymin = 60, ymax = 90, line = list(color = 'rgba(255,110,0,1)'), fillcolor = "rgba(255,110,0,0.25)", name = "monitor") %>%
      add_ribbons(ymin = 90, ymax = 125, line = list(color = 'rgba(255,0,0,1)'), fillcolor = "rgba(255,0,0,0.25)", name = "critical") %>%
      add_trace(y = ~Day+Night, name="Tags", type = 'scatter', mode = 'lines', line = list(color = 'rgba(0,0,0,1)', width = 3)) %>%
      layout(title = "Predicted Effect on Searching Performance",
             xaxis = list(title = "Date"),
             yaxis = list (title = "No. of Tags"))
    p$elementId <- NULL
    p
    })
    output$ratePlot <- renderPlotly({
      
      sp <- which(plt_res$date == input$days[1])
      ep <- which(plt_res$date == input$days[2])
      
      odf <- data.frame(plt_res[sp:ep,])
      p<-plot_ly(odf,x=~date) %>%
        add_ribbons(ymin = 0, ymax = 60, line = list(color = 'rgba(130,229,153,1)'), fillcolor = "rgba(130,229,153,0.25)", name = "good") %>%
        add_ribbons(ymin = 60, ymax = 90, line = list(color = 'rgba(255,110,0,1)'), fillcolor = "rgba(255,110,0,0.25)", name = "monitor") %>%
        add_ribbons(ymin = 90, ymax = 125, line = list(color = 'rgba(255,0,0,1)'), fillcolor = "rgba(255,0,0,0.25)", name = "critical") %>%
        add_trace(y = ~D.Rate+(Night*0.125), name="Day", type = 'scatter', mode = 'lines', line = list(color = 'rgba(237,154,30,1)', width = 3)) %>%
        add_trace(y = ~N.Rate+(Day*0.125), name="Night", type = 'scatter', mode = 'lines', line = list(color = 'rgba(28,16,135,1)', width = 3))%>%
        layout(title = "Predicted Effect on Tracking Performance",
               xaxis = list(title = "Date"),
               yaxis = list (title = "No. of Tags * Rate"))
      p$elementId <- NULL
      p
      })

  })
  ##table output
  observeEvent(input$days,{
    
    out_tab <- subset(tab_cmb, tmp_date >= input$days[1] & tmp_date <= input$days[2])
    out_tab$tmp_date <- as.character(out_tab$tmp_date)
    names(out_tab)[names(out_tab)=="tmp_date"] <- "Date"
    output$deployTab <- DT::renderDataTable({ datatable(out_tab,rownames=FALSE) %>%
        formatStyle('Researcher', target = 'row', fontWeight = styleEqual("Summary", "bold")) %>%
        formatStyle('Day', color = styleInterval(c(60,89), c('black','orange','red'))) %>% 
        formatStyle('Night', color = styleInterval(c(60,89), c('black','orange','red'))) 
    })
  })
  
  output$datetoday <- renderText({
    td <- Sys.Date()
    return(paste("Today:",as.character(as.Date(td))))
  })
  #map output
  output$bo2017map <- renderLeaflet({
    boll<-get(load("bo2017map.RData"))
    ball<-get(load("bat2015-2017map.RData"))
    bsll<-get(load("bs.RData"))
    lv <- unique(10)
    lv <- lv[order(lv)]
    pal <- colorBin(palette = "RdYlGn", domain = boll$val, bins=10, reverse = TRUE)
    
    min_lat = min(bsll$lat)
    min_lon = min(bsll$lon)
    max_lat = max(bsll$lat)
    max_lon = max(bsll$lon)
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron, group="Positron") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      fitBounds(min_lon, min_lat, max_lon, max_lat) %>% 
      addCircleMarkers(data=boll, radius = 3, weight = 1, color = pal(boll$val), stroke = FALSE, fillOpacity = 0.75, group="Barn Owls 2017") %>% 
      addCircleMarkers(data=ball, radius = 3, weight = 1, color = pal(ball$val), stroke = FALSE, fillOpacity = 0.75, group="Bats 2015-2017") %>%
      addCircleMarkers(data=bsll, radius = 3, weight = 3, color = "black", stroke = TRUE, fillOpacity = 0, group="Base Stations") %>% 
      addLegend("bottomright", pal = pal, values = ll$val, title = "Loss (%)", opacity = 1) %>%
      addScaleBar(position ="bottomleft", options = scaleBarOptions()) %>%
      # Layers control
      addLayersControl(baseGroups = c("Positron", "Toner", "Toner Lite"),
        overlayGroups = c("Barn Owls 2017","Bats 2015-2017","Base Stations"),
        options = layersControlOptions(collapsed = FALSE))%>% hideGroup("Bats 2015-2017")
      
  })

  
}
shinyApp(ui = ui, server = server)
