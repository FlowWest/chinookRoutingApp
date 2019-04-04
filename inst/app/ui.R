fluidPage(
  theme = shinythemes::shinytheme("paper"),
  inverse = TRUE,
  tags$h3("Chinook Routing"),
  includeCSS("styles.css"),
  sidebarLayout(
    sidebarPanel(
      width = 3, 
      tabsetPanel(
        type = "pills",
        tabPanel("Home", 
                 tagList(
                   tags$br(),
                   tags$h4("Introduction"), 
                   tags$p("Use the Chinook Routing tool to visualize survival 
                          of Chinook at different points of interest in the delta.
                          Start by selecting the number of fish existing at each of 
                          Freeport and Vernalis, you can then modify different parameters
                          affecting survival through routes in the delta. Parameters
                          are described below:"), 
                   tags$br(), 
                   tags$h5("Additional Options"), 
                   tags$p("Apart from paremters options, there are additional features for 
                          the visualizations available.")
                 )
        ),
        tabPanel("Dash",
                 tags$hr(),
                 # tags$p("Clear up all points selected from the map"),
                 # actionButton("clear_selected_routing_points", "clear selection", class="btn-primary"),
                 # tags$hr(),
                 tags$div(
                   tags$div(style="display:inline-block",
                            numericInput("fish_above_freeport", "No. of fish above Freeport", 10000, 
                                         width = "160px")
                   ),
                   tags$div(style="display:inline-block",
                            numericInput("fish_above_vernalis", "No. of fish above Vernalis", 10000, 
                                         width = "160px")
                   )
                 ),
                 radioButtons("dcc_open", "Delta Cross Channel Gate", 
                              choices = c("open"=1, "closed"=0), inline=TRUE),
                 radioButtons("hor_barr", "Head of Old River", 
                              choices = c("barrier in place"=1, "no barrier"=0), inline=TRUE),
                 radioButtons("bio_fence", "Bioacoustic fence at Sutter/Steamboar", 
                              choices = c("fence in place"=1, "no fence"=0), inline = TRUE),
                 numericInput("q_free", "Freeport average daily discharge (cms, 150 < x < 2400)", 
                              value = 150, min = 150, max = 2400),
                 
                 # these two numeric inputs need addtional context
                 tags$div(
                   tags$div(style="display:inline-block", 
                            numericInput("q_vern", "Vernalis average daily discharge (cms, 13.6 < x < 807)", 
                                         value = 14, min = 13.6, max = 807)), 
                   tags$div(style="display:inline-block", 
                            actionButton("help_with_q_vern", label = NULL, 
                                         icon = icon("question"), 
                                         class="btn-xs btn-primary routing-help-button"))
                 ),
                 tags$div(
                   tags$div(style="display:inline-block", 
                            numericInput("q_stck", "Stockton average daily discharge (cms)", 
                                         value = 0, min = -8.9, max = 325.6)), 
                   tags$div(style="display:inline-block", 
                            actionButton("help_with_q_stck", label = NULL, 
                                         icon = icon("question"), 
                                         class="btn-xs btn-primary routing-help-button"))
                 ),
                 numericInput("temp_vern", "Vernalis average daily temperature (C°, 8.2 < x < 21.8)", 
                              value = 10, min = 8.2, max = 21.8),
                 numericInput("temp_pp", "SJR at Prisoner's Point average daily temperature (C°, 7.8 < x < 20.8)", 
                              value = 10, min = 7.8, max = 20.8),
                 numericInput("cvp_exp", "CVP average daily exports (cms)", 
                              value = 10, min = 6.1, max = 120),
                 numericInput("swp_exp", "SWP average daily exports (cms)", 
                              value = 10, min = 5.2, max = 235.7),
                 numericInput("fl", "Fork length (mm)", value = 10) 
                 # tags$hr(), 
                 # tags$h5("Additional Options"),
                 # selectInput("options_highlight_bar", 
                 #             "Highlight location on plot", 
                 #             choices = c("None", chinook_routing_points$location))
                 )
      )
    ), 
    mainPanel(
      width = 9,
      leafletOutput("chinook_routing_map"),
      tabsetPanel(
        type = "pills",
        tabPanel("Plot",plotlyOutput("chinook_routing_plot", height = "450px")),
        tabPanel("Data", DT::dataTableOutput("chinook_routing_table"))
      )
    )
  )
)
