library(shiny)

shinyUI(fluidPage(
  
  navbarPage(
    'Chaku Chaku Line Analytics',
    tabPanel('DNOX',
      
      fluidRow(
        column(2,
               numericInput('target',"Target CT", value = 35, step = 0.1)
               ),
        column(2,
               br(),
               div(style = 'margin:5px',
                   actionButton('confirm_target', 'OK')
               )

               )
      ),      
             
             
      tabsetPanel(
        tabPanel(
          'Trend',
          HTML(paste0('<h4><center><b>','Performance Trend','</b></center></h4>')),
          
          selectInput('trend_y_scale', 'Y scale option', choices = c('Free scale','Fixed scale'), selected = 'Free scale'),
          
          plotOutput('Trend',height = 800)
        ),
        tabPanel(
          'Shift Overview',
          
          HTML(paste0('<h4><center><b>','Shift Performance Analysis','</b></center></h4>')),
          
          fluidRow(
            column(2,
                   uiOutput('DATE')
                   ),
            column(2,
                   uiOutput('SHIFT')
                   )
            # ,
            # column(2,
            #        br(),
            #        div(style = 'margin:5px',
            #         actionButton('run_deepdive', 'Run')
            #        )
            #        )
          ),
          hr(),
          
          plotOutput('L_Paired_Summary_Overview'),
          hr(),
          HTML('<button data-toggle="collapse" data-target="#filght_chart">Show/hide flight chart</button>'),
          
          div(id = 'filght_chart', class= 'collapse',
            HTML(paste0('<h4><center>','Piece flow flight chart','</center></h4>')),
            fluidRow(
              column(1,''),
              column(10, uiOutput('TIME_INTERVAL')),
              column(1,'')
            ),
            plotOutput('flight_chart',height = 500)
          ),
          
          hr(),
          
          HTML('<button data-toggle="collapse" data-target="#time_loss_analysis">Show/hide time loss diagnosis</button>'),
          
          div(id = 'time_loss_analysis', class = 'collapse',
            HTML(paste0('<h4><center>','Time Loss Analysis','</center></h4>')),
            fluidRow(
              column(2,numericInput('max_bottle_num','Highlight top positive time losses', value = 5, step = 1, min =1, max =10)),
              column(2,plotOutput('Legend', height = 60, width= 250))
              # ,
              # column(6,
              #        div(style = 'font-size:x-small',
              #            helpText('Notes:'),
              #            helpText('All time losses in units [seconds/piece]'),
              #            helpText('x axis(Piece view, station to station [s]): Time from end of previous station to end of current station for the current piece'),
              #            helpText('y axis(Station view, piece to piece [s]): Time from end of previous piece to end of current piece for the current station'),
              #            helpText('Total Piece loss = Total time loss / (Total time loss + Target CT)')
              #            
              #        )
              # )
            ),
  
            plotOutput('paired_2', height = 900)
          ),
          hr(),
          HTML('<button data-toggle="collapse" data-target="#calc_table_2">Show calculations</button>'),
          div(id = 'calc_table_2', class = 'collapse',
              HTML(paste0('<h4><center>','Scoring Calculation Table','</center></h4>')),
              div(style = 'font-size:xx-small',DT::dataTableOutput('score_Table_2'))
          ),
          br(),br(),br()
          
        )
      )
    )
  ),
  absolutePanel(
    top = 40,right=10,fixed = TRUE,
    checkboxInput('adjust_delta','Use adjusted data',value = 1)
  )
))
