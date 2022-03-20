#survey_app <- function(){

if(FALSE){
  library(shiny)

  # incorporate GPS sensor
  # https://github.com/AugustT/shiny_geolocation/blob/master/accuracy_and_dynamic/server.r

  # figure out DT data fixing
  # further develop & package

  # Image processing: stabilize and make into package

}
##############################################################################
##############################################################################
# Defaults

tab_size <- 200
tab_width <- 200
keypad_size <- 300
keypad_padding <- 30
scan_target <- .5
grabit <- 'store'
gps_interval = 10

##############################################################################
##############################################################################
# Functions

get_filename <- function(dt=NULL){
  if(is.null(dt)){ dt <- Sys.Date() } ; dt
  new_filename <- paste0('data/',dt,'.csv') ; new_filename
  return(new_filename)
}

log_line <- function(event, event_data = ''){
  new_filename <- get_filename() ; new_filename
  if(!dir.exists('data')){dir.create('data')}
  new_line <- paste0(Sys.time(),',',event,',',paste(event_data, collapse=','),'\n') ; new_line
  cat(new_line, file=new_filename, append=TRUE)
  df <- read.csv(new_filename, header=FALSE)
  print(df)
  return(df)
}

load_all_days <- function(){
  lf <- list.files('data/') ; lf
  dfs <- data.frame()
  if(length(lf)>0){
    lf <- paste0('data/',lf) ; lf
    i=5
    for(i in 1:length(lf)){
      dfi <- read.csv(lf[i],stringsAsFactors=FALSE,header=FALSE, row.names=NULL) ; head(data)
      dfs <- plyr::rbind.fill(dfs,dfi)
    }
  }
  dfs
  return(dfs)
}

onStart <- function(){
  #log_line('B')

  onStop(function() {
    log_line('E')
  })
}

##############################################################################
##############################################################################
# UI

ui <- fluidPage(
  br(),
  fluidRow(column(12,

                  ##############################################################################
                  ##############################################################################
                  # Keypad


                  sidebarPanel(
                    #br(),
                    fluidRow(column(1), column(11, span(textOutput('keypad'), style=paste0('font-size:',keypad_size,'%')))),
                    #br(),
                    fluidRow(column(12,
                                    actionButton('b1','1', style=paste0('padding:',keypad_padding,'px; font-size:',keypad_size,'%'), width= '30%'),
                                    actionButton('b2','2', style=paste0('padding:',keypad_padding,'px; font-size:',keypad_size,'%'), width= '30%'),
                                    actionButton('b3','3', style=paste0('padding:',keypad_padding,'px; font-size:',keypad_size,'%'), width= '30%')
                    )),
                    fluidRow(column(12,
                                    actionButton('b4','4', style=paste0('padding:',keypad_padding,'px; font-size:',keypad_size,'%'), width= '30%'),
                                    actionButton('b5','5', style=paste0('padding:',keypad_padding,'px; font-size:',keypad_size,'%'), width= '30%'),
                                    actionButton('b6','6', style=paste0('padding:',keypad_padding,'px; font-size:',keypad_size,'%'), width= '30%')
                    )),
                    fluidRow(column(12,
                                    actionButton('b7','7', style=paste0('padding:',keypad_padding,'px; font-size:',keypad_size,'%'), width= '30%'),
                                    actionButton('b8','8', style=paste0('padding:',keypad_padding,'px; font-size:',keypad_size,'%'), width= '30%'),
                                    actionButton('b9','9', style=paste0('padding:',keypad_padding,'px; font-size:',keypad_size,'%'), width= '30%')
                    )),
                    fluidRow(column(12,
                                    actionButton('b0','0', style=paste0('padding:',keypad_padding,'px; font-size:',keypad_size,'%'), width= '30%'),
                                    actionButton('bdec','.', style=paste0('padding:',keypad_padding,'px; font-size:',keypad_size,'%'), width= '30%')
                    )),
                    br(),
                    fluidRow(column(12,
                                    actionButton('bclr','clear', style=paste0('padding:',round(keypad_padding*.7),'px; font-size:',round(keypad_size*.7),'%'), width= '95%')
                    )),
                    br(),
                    width = 3
                  ),

                  ##############################################################################
                  ##############################################################################

                  mainPanel(

                    tags$style(HTML(paste0("
    .tabbable > .nav > li > a {font-size:",tab_size,"%; width: ",tab_width,"px;}
  "))),

                    tabsetPanel(

                      ##############################################################################
                      ##############################################################################
                      tabPanel('Effort',
                               br(),
                               fluidRow(column(6, 'observers etc'),
                                        column(6,
                                               uiOutput('scan'),
                                               br(),
                                               textOutput('scan_duration')
                                        ))
                      ),

                      ##############################################################################
                      ##############################################################################
                      tabPanel('Conditions',
                               br(),
                               fluidRow(column(12, 'hello'))
                      ),

                      ##############################################################################
                      ##############################################################################
                      tabPanel('Sightings',
                               br(),
                               fluidRow(column(3,
                                               h4('Bearing: '),
                                               uiOutput('bearing'),
                                               br(), br(), h4('In line with ...'),
                                               'select menu'),
                                        column(3,
                                               h4('Reticle'),
                                               uiOutput('reticle'),
                                               br(), br(), h4('according to'),
                                               'select menu',
                                               br(), br(), h4('Est. distance (km)'),
                                               'button'),
                                        column(3,
                                               h4('Max. group size'),
                                               'button',
                                               br(), br(), h4('Min group size'),
                                               'button',
                                               br(), br(), h4('Best estimate'),
                                               'button',
                                               br(), br(), h4('Detection cue'),
                                               'select menu'),
                                        column(3,
                                               h4('Sighting of ...'),
                                               'menu',
                                               br(), br(), h4('Species / type:'),
                                               'menu')
                               ),
                               br()
                      ),

                      ##############################################################################
                      ##############################################################################
                      tabPanel('Comment',
                               br(),
                               fluidRow(column(12, 'hello'))
                      ),

                      ##############################################################################
                      ##############################################################################
                      tabPanel('Review',
                               tabsetPanel(
                                 tabPanel(h3('Data'),
                                          br(),
                                          DTOutput('data'),
                                          br(),
                                          fluidRow(column(3,uiOutput("editclearscan")),
                                                   column(3,uiOutput("remove_row")),
                                                   column(3,uiOutput("copypaste")),
                                                   column(3,uiOutput("undo")))
                                 ),

                                 tabPanel(h3('Map'),
                                          br(),
                                          'hello')
                               )
                      )
                    ),

                    width= 9
                  )

  )),

  ##############################################################################
  ##############################################################################
  # GPS

  br(),
  fluidRow(
    tags$script(paste0('
              $(document).ready(function () {

                function getLocation(callback){
                var options = {
                  enableHighAccuracy: true,
                  timeout: 5000,
                  maximumAge: 0
                };

                navigator.geolocation.getCurrentPosition(onSuccess, onError);

                function onError (err) {
                  Shiny.onInputChange("geolocation", false);
                }

                function onSuccess (position) {
                  setTimeout(function () {
                    var coords = position.coords;
                    var timestamp = new Date();

                    console.log(coords.latitude + ", " + coords.longitude, "," + coords.accuracy);
                    Shiny.onInputChange("geolocation", true);
                    Shiny.onInputChange("lat", coords.latitude);
                    Shiny.onInputChange("long", coords.longitude);
                    Shiny.onInputChange("accuracy", coords.accuracy);
                    Shiny.onInputChange("time", timestamp)

                    console.log(timestamp);

                    if (callback) {
                      callback();
                    }
                  }, 1100)
                }
              }

              var TIMEOUT = ',1000,'; //SPECIFY
              var started = false;
              function getLocationRepeat(){
                //first time only - no delay needed
                if (!started) {
                  started = true;
                  getLocation(getLocationRepeat);
                  return;
                }

                setTimeout(function () {
                  getLocation(getLocationRepeat);
                }, TIMEOUT);

              };

              getLocationRepeat();

            });
            ')), # end of tag
    column(3, h4('Latitude'), verbatimTextOutput("lat")),
    column(3, h4('Longitude'), verbatimTextOutput("long")),
    column(3, h4('Speed (kmh)'), verbatimTextOutput("speed")),
    column(3, h4('Time'), verbatimTextOutput("time")))

)

##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################

server <- function(input, output, session) {

  rv <- reactiveValues()
  rv$df <- log_line('B')
  rv$df_backup <- NULL
  rv$keypad <- '0'
  gps_timer <- reactiveTimer(1000*gps_interval)

  rv$scan <- 0
  rv$scan_start <- Sys.time()
  rv$scan_duration <- NULL
  scan_timer <- reactiveTimer(1000)

  rv$bearing <- NULL
  rv$inline <- NULL
  rv$reticle <- NULL
  rv$reticle_how <- NULL
  rv$est_dist <- NULL
  rv$grp_max <- NULL
  rv$grp_min <- NULL
  rv$grp_best <- NULL
  rv$cue <- NULL
  rv$spp_cat <- NULL
  rv$spp <- NULL

  # behaviors & acoustics
  # species_specific fields

  rv$row <- NULL
  rv$row_data <- NULL

  ##############################################################################
  ##############################################################################
  # Keypad

  output$keypad <- renderText(rv$keypad)
  shiny::observeEvent(input$b1, { shiny::isolate({ rv$keypad <- ifelse(rv$keypad == '0', '1', paste0(rv$keypad, '1')) }) })
  shiny::observeEvent(input$b2, { shiny::isolate({ rv$keypad <- ifelse(rv$keypad == '0', '2', paste0(rv$keypad, '2')) }) })
  shiny::observeEvent(input$b3, { shiny::isolate({ rv$keypad <- ifelse(rv$keypad == '0', '3', paste0(rv$keypad, '3')) }) })
  shiny::observeEvent(input$b4, { shiny::isolate({ rv$keypad <- ifelse(rv$keypad == '0', '4', paste0(rv$keypad, '4')) }) })
  shiny::observeEvent(input$b5, { shiny::isolate({ rv$keypad <- ifelse(rv$keypad == '0', '5', paste0(rv$keypad, '5')) }) })
  shiny::observeEvent(input$b6, { shiny::isolate({ rv$keypad <- ifelse(rv$keypad == '0', '6', paste0(rv$keypad, '6')) }) })
  shiny::observeEvent(input$b7, { shiny::isolate({ rv$keypad <- ifelse(rv$keypad == '0', '7', paste0(rv$keypad, '7')) }) })
  shiny::observeEvent(input$b8, { shiny::isolate({ rv$keypad <- ifelse(rv$keypad == '0', '8', paste0(rv$keypad, '8')) }) })
  shiny::observeEvent(input$b9, { shiny::isolate({ rv$keypad <- ifelse(rv$keypad == '0', '9', paste0(rv$keypad, '9')) }) })
  shiny::observeEvent(input$b0, { shiny::isolate({ rv$keypad <- ifelse(rv$keypad == '0', '0', paste0(rv$keypad, '0')) }) })
  shiny::observeEvent(input$bdec, { shiny::isolate({ rv$keypad <- ifelse(rv$keypad == '0', '0.', paste0(rv$keypad, '.')) }) })
  shiny::observeEvent(input$bclr, { shiny::isolate({ rv$keypad <- '0' }) })

  ##############################################################################
  ##############################################################################
  # GPS

  output$lat <- renderPrint({ input$lat })
  output$long <- renderPrint({ input$long })
  output$geolocation <- renderPrint({ input$geolocation })
  output$accuracy <- renderPrint({ input$accuracy })
  output$time <- renderPrint({ input$time })
  output$speed <- renderPrint({ input$speed })

  # Log GPS position
  observeEvent(gps_timer(), {
    isolate({
      gps_test1 <- c(exists('input$lat'), exists('input$long'), exists('input$time'))
      #print(gps_test1)
      if(all(gps_test1)){
        gps_test2 <- c(!is.null(input$lat), !is.null(input$long), !is.null(input$time))
        #print(gps_test2)
        if(all(gps_test2)){
          gps_test3 <- c(!is.na(as.numeric((input$lat))), !is.na(as.numeric((input$long))), !is.na(as.numeric((input$time))))
          # print(gps_test3)
          if(all(gps_test3)){
            gps_data <- c(input$lat, input$long, input$time)
            rv$df <- log_line('*', gps_data)
          }
        }
      }
    })
  })

  ##############################################################################
  ##############################################################################
  # Effort

  # Button to begin/end scan
  output$scan <- renderUI({
    if(rv$scan == 1){
      actionButton("scan_off",label="End Scan", style='padding:50px; font-size:400%', width="90%")
    }else{
      actionButton("scan_on",label="Start Scan",style='padding:50px; font-size:400%', width="90%")
    }
  })

  # When scan begins
  observeEvent(input$scan_on,{
    rv$scan <- 1
    rv$scan_start <- Sys.time()
    scan_data <- c('obs', 'inside', rv$scan)
    rv$df <- log_line('EFF', scan_data)
  })

  # When scan ends
  observeEvent(input$scan_off,{
    rv$scan <- 0
    scan_timer <- NULL
    scan_data <- c('obs', 'inside', rv$scan)
    rv$df <- log_line('EFF', scan_data)
  })

  # Begin scan timer
  observe({
    if(rv$scan == 1){
      scan_timer()
      secs <- difftime(Sys.time(), rv$scan_start, units='secs')
      rv$scan_duration <- secs
    }
  })

  # Print scan duration
  output$scan_duration <- renderText({
    if(rv$scan){
      secs <- rv$scan_duration
      mins <- secs / 60
      mins_show <- floor(mins)
      secs_show <- round( (mins - mins_show) * 60)
      paste0('Scan duration: ', mins_show,' minutes ',secs_show, ' seconds')
    }else{
      ''
    }
  })

  # Announce end of scan
  observe({
    if(!is.null(rv$scan_duration)){
      if(!is.null(scan_target)){
        if(round(as.numeric(rv$scan_duration)) == round((scan_target*60))){
          beepr::beep(3)
        }
      }
    }
  })

  ##############################################################################
  ##############################################################################
  # Conditions


  ##############################################################################
  ##############################################################################
  # Sighting

  # Bearing
  output$bearing <- renderUI({
    button_label <- ifelse(is.null(rv$bearing), grabit, rv$bearing)
    actionButton("bearing", label=button_label, style='padding:20px; font-size:150%', width="90%")
  })
  observeEvent(input$bearing,{ isolate({
    rv$bearing <- rv$keypad
    rv$keypad <- '0'
  }) })

  # Reticle
  output$reticle <- renderUI({
    button_label <- ifelse(is.null(rv$reticle), grabit, rv$reticle)
    actionButton("reticle", label=button_label, style='padding:20px; font-size:150%', width="90%")
  })
  observeEvent(input$reticle,{ isolate({
    rv$reticle <- rv$keypad
    rv$keypad <- '0'
  }) })

  ##############################################################################
  ##############################################################################
  # Comment


  ##############################################################################
  ##############################################################################
  # Review - Data

  output$data = DT::renderDT( rv$df, options = list(lengthChange = FALSE), editable=TRUE)

  # Select row
  observe({ rv$row <- input$data_rows_selected ; print(rv$row) })

  # Remove row  =======================================

  output$remove_row <- renderUI({
    if(length(rv$row) > 0){ actionButton("remove_row",h4("Remove row(s)"),width="95%") }
  })
  observeEvent(input$remove_row,{
    isolate({
      rv$df_backup <- rv$df
      keeps <- which(! 1:nrow(rv$df) %in% rv$row)
      new_df <- rv$df[keeps,]
      new_filename <- get_filename()
      readr::write_csv(x=new_df, file=new_filename, quote='none', col_names=FALSE, append=FALSE)
      rv$df <- new_df
    })
  })

  # Copy / paste =======================================

  output$copypaste <- renderUI({
    if(length(rv$row)>0){
      if(is.null(rv$row_data)){
        actionButton("copy",h4("Copy row"),width="95%")
      }else{
        actionButton("paste",h4("Paste below current row"),width="95%")
      }
    }
  })

  observeEvent(input$copy,{ isolate({ rv$row_data <- rv$df[rv$row,] }) })

  observeEvent(input$paste,{ isolate({
    rv$df_backup <- rv$df
    new_row <- (rv$row[length(rv$row)]+1)
    if(new_row >= nrow(rv$df)){ new_row <- NULL}
    new_df <- DataCombine::InsertRow(rv$df, NewRow = rv$row_data, RowNum = new_row)
    new_filename <- get_filename()
    readr::write_csv(x=new_df, file=new_filename, quote='none', col_names=FALSE, append=FALSE)
    rv$df <- new_df
    rv$row_data <- NULL
  }) })

  # Edit a row ========================

  proxy5 = dataTableProxy('data')
  observeEvent(input$data_cell_edit, {
    info = input$data_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    dti <- rv$df
    rv$df_backup <- rv$df
    new_df <- rv$df
    new_df[i, j] <- isolate(DT::coerceValue(v, new_df[i, j]))
    readr::write_csv(x=new_df, file=new_filename, quote='none', col_names=FALSE, append=FALSE)
    rv$df <- new_df
  })

  # Undo edit  ========================================

  output$undo <- renderUI({
    if(!is.null(rv$df_backup)){
      if(!identical(rv$df, rv$df_backup)){ actionButton("undo",h4("Undo last edit"),width="95%") }
    }
  })
  observeEvent(input$undo,{
    isolate({
      new_df <- rv$df_backup
      readr::write_csv(x=new_df, file=new_filename, quote='none', col_names=FALSE, append=FALSE)
      rv$df <- new_df
    })
  })

  ##############################################################################
  ##############################################################################
  # Review - Map

}

##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################

shinyApp(ui,
         server,
         onStart)


#}
