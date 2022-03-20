#survey_app <- function(){

if(FALSE){
  library(shiny)
}
##############################################################################
##############################################################################
# Defaults

tab_size <- 200
tab_width <- 200
keypad_size <- 300
keypad_padding <- 40
scan_target <- .5
grabit <- 'store'

##############################################################################
##############################################################################
# Functions

get_filename <- function(dt=NULL){
  if(is.null(dt)){ dt <- Sys.Date() } ; dt
  new_filename <- paste0('data/',dt,'.csv') ; new_filename
  return(new_filename)
}

log_line <- function(event, event_data){
  new_filename <- get_filename() ; new_filename
  if(!dir.exists('data')){dir.create('data')}
  new_line <- paste0(Sys.time(),',',event,',',paste(event_data, collapse=','),'\n') ; new_line
  cat(new_line, file=new_filename, append=TRUE)
}


##############################################################################
##############################################################################
# UI

ui <- pageWithSidebar(
  br(),

  ##############################################################################
  ##############################################################################
  # Keypad

  sidebarPanel(
    fluidRow(column(1), column(11, span(textOutput('keypad'), style=paste0('font-size:',keypad_size,'%')))),
    br(),
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
                          'hello'),
                 tabPanel(h3('Map'),
                          br(),
                          'hello')
               )
      )
    ),

    width= 9
  )
)

##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################

server <- function(input, output, session) {

  rv <- reactiveValues()
  rv$keypad <- '0'

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
    log_line('EFF', scan_data)
  })

  # When scan ends
  observeEvent(input$scan_off,{
    rv$scan <- 0
    scan_timer <- NULL
    scan_data <- c('obs', 'inside', rv$scan)
    log_line('EFF', scan_data)
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


  ##############################################################################
  ##############################################################################
  # Review - Map

}

##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################

shinyApp(ui, server)


#}
