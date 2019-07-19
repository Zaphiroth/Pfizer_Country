
##-- App part
ui <- dashboardPage(
  dashboardHeader(title = "Pfizer County Hospitals Potential"),
  dashboardSidebar(
    tags$head(includeCSS('./www/fix_siderbar.css')),
    collapsed = FALSE,
    fluidRow(column(12, fileInput('summary', 'Please Upload the Raw Data'))),
    fluidRow(tags$div(
      tags$div(column(1, actionButton("goButton", "Go!")),
               style = "display:inline-block;margin-down: 1px;vertical-align:middle"),
      
      tags$style(".skin-blue .sidebar a { color: #444; }"),
      
      tags$div(column(#offset = 1, 
                      2,
                      downloadButton(outputId = "downloadData", 
                                     label = "Download")),
               style = "display:inline-block;margin-down: 1px;vertical-align:middle"))),

    # conditionalPanel(condition="input.tabselected==2", 
    #                  selectInput(
    #                    "top2",
    #                    "Top Brand",
    #                    c(
    #                      "zero" = 0,
    #                      "three" = 3,
    #                      "five" = 5,
    #                      "ten" = 10,
    #                      "All" = 100000
    #                    ),
    #                    selected = 10
    #                  )),
    # 
    # conditionalPanel(condition="input.tabselected==3", 
    #                  selectInput(
    #                    "top3",
    #                    "Top Brand",
    #                    c(
    #                      "zero" = 0,
    #                      "three" = 3,
    #                      "five" = 5,
    #                      "ten" = 10,
    #                      "All" = 100000
    #                    ),
    #                    selected = 10
    #                  )),
    
    selectInput("year", "Year", "", multiple = TRUE),
    selectInput("mkt", "Market", "", multiple = TRUE),
    selectInput("province", "Province", "", multiple = TRUE)
  ),
  
  dashboardBody(
    useShinyjs(),
    br(),
    tabsetPanel(
      tabPanel(strong("High Level Summary Statistics"), value=1,
               br(),
               fluidRow(
                 box(title = "Cities and Hospitals Statistics",
                     width = 12,
                     solidHeader = TRUE,
                     status = "primary",
                     
                     # fluidRow(column(3, uiOutput("sub_cat_ui"))),
                     # fluidRow(
                       column(width = 4,
                              # offset = 1,
                              div(DT::dataTableOutput("summary_table"), 
                                  style = "font-size:90%")),
                       column(width = 3,
                              offset = 1,
                              div(DT::dataTableOutput("summary_table1"),
                                  style = "font-size:90%"),
                              br(),
                              div(plotlyOutput("summary_bar1", height = "180px"), 
                                  style = "font-size:90%")
                              ),
                       
                       column(width = 2,
                              offset = 1,
                              div(DT::dataTableOutput("summary_table2"),
                                  style = "font-size:90%"),
                              br(),
                              div(plotlyOutput("summary_bar2", height = "180px"), 
                                  style = "font-size:90%")
                              )
                       # style = "height:200px"
                       # )
                     )
                 ),
               
               fluidRow(
                 box(
                   title = "Current Potential by City", 
                   status = "primary",
                   solidHeader = TRUE,
                   collapsible = FALSE,
                   width = 12,
                   
                   br(),
                   h5("1, Current Potential Contribution by City(CU/Mn)"),
                   br(),
                   div(DT::dataTableOutput("current_potential_by_city"),
                       style = "font-size:90%; overflow-x:scroll;"),
                   
                   br(), br(),
                   h5("2, City Rank of Current Potential Contribution(CU/Mn)"),
                   br(),
                   div(plotlyOutput("city_rank_current_potential", height = "200px"),
                       style = "font-size:90%"),
                   
                   br(), br(),
                   h5("3, Channel Distribution of Current Potential Contribution by City(CU/Mn)"),
                   br(),
                   div(plotlyOutput("channel_distribution_current_potential_by_city", height = "200px"),
                       style = "font-size:90%")
                   )
                 ),
                   
               fluidRow(
                 box(
                   title = "Channel Distribuion of Hospital Counts by City", 
                   status = "primary",
                   solidHeader = TRUE,
                   collapsible = FALSE,
                   width = 12,
                   
                   br(),
                   # h3("Current Potential Contribution by City(CU/Mn)"),
                   # br(),
                   div(DT::dataTableOutput("channel_dist_hospital_cnt_by_city"),
                       style = "font-size:90%; overflow-x:scroll;")
                 )
               ),
               
               fluidRow(
                 box(
                   title = "Total Potential and Share by City", 
                   status = "primary",
                   solidHeader = TRUE,
                   collapsible = FALSE,
                   width = 12,
                   
                   br(),
                   h5("1, Total Current Potential and Share by City(CU/Mn)"),
                   br(),
                   div(DT::dataTableOutput("total_current_potential_share_by_city"),
                       style = "font-size:90%; overflow-x:scroll;"),
                   
                   br(), br(),
                   h5("2, Total Current Potential VS. Pfizer(CU/Mn)"),
                   br(),
                   div(plotlyOutput("total_current_potential_share", height = "200px"),
                       style = "font-size:90%")
                 )
               ),
               
               fluidRow(
                 box(
                   title = "City Hospitals Potential and Share by City", 
                   status = "primary",
                   solidHeader = TRUE,
                   collapsible = FALSE,
                   width = 12,
                   
                   br(),
                   h5("1, City Hospitals Current Potential and Share by City(CU/Mn)"),
                   br(),
                   div(DT::dataTableOutput("city_hospitals_current_potential_share_by_city"),
                       style = "font-size:90%; overflow-x:scroll;"),
                   
                   br(), br(),
                   h5("2, City Hospitals Current Potential VS. Pfizer(CU/Mn)"),
                   br(),
                   div(plotlyOutput("city_hospitals_current_potential_share", height = "200px"),
                       style = "font-size:90%")
                 )
               ),
               
               fluidRow(
                 box(
                   title = "County Hospitals Current Potential and Share by City", 
                   status = "primary",
                   solidHeader = TRUE,
                   collapsible = FALSE,
                   width = 12,
                   
                   br(),
                   h5("1, County Hospitals Current Potential and Share by City(CU/Mn)"),
                   br(),
                   div(DT::dataTableOutput("County_hospitals_current_potential_share_by_city"),
                       style = "font-size:90%; overflow-x:scroll;"),
                   
                   br(), br(),
                   h5("2, County Hospitals Current Potential VS. Pfizer(CU/Mn)"),
                   br(),
                   div(plotlyOutput("county_hospitals_current_potential_share", height = "200px"),
                       style = "font-size:90%")
                 )
               ),
               
               fluidRow(
                 box(
                   title = "Current Potential Growth by City", 
                   status = "primary",
                   solidHeader = TRUE,
                   collapsible = FALSE,
                   width = 12,
                   
                   br(),
                   h5("1, Current Potential Growth Between City Hospitals and County Hospitals by City(CU/Mn)"),
                   br(),
                   div(DT::dataTableOutput("growth_current_potential_by_city"),
                       style = "font-size:90%; overflow-x:scroll;"),
                   
                   br(), br(),
                   h5("2, Current Potential Growth Between City Hospitals and County Hospitals by City(CU/Mn)"),
                   br(),
                   div(plotlyOutput("growth_current_potential_by_city_chart", height = "200px"),
                       style = "font-size:90%")
                 )
               )
      ),
      
      tabPanel(strong("Corporation"), value=2,
               
               br(),
               fluidRow(
                 box(title = "China Market Summary Value(RMB)",
                     width = 12,
                     solidHeader = TRUE,
                     status = "primary",
                     # Dynamic valueBoxes to show the KPIs
                     fluidRow(column(3, uiOutput("sub_cat_c_ui"))),
                     fluidRow(
                       column(width = 6,
                              offset = 1,
                              div(DT::dataTableOutput("summary_table_c"), style = "font-size:90%")),
                       column(width = 3,
                              offset = 1,
                              div(DT::dataTableOutput("summary_table1_c"), style = "font-size:90%"))

                     )
                 )
               ),
               
               fluidRow(
                 box(
                   title = "Market Trend Performance", status = "primary", solidHeader = TRUE,
                   collapsible = FALSE,
                   width = 12,
                   fluidRow(
                     br(),
                     column(3, 
                            materialSwitch(inputId = "bp_p_c", 
                                           label = "Corporation Performance",
                                           status = "primary", 
                                           right = TRUE,
                                           value = TRUE)
                     ),
                     column(3, 
                            materialSwitch(inputId = "rpp_p_c", 
                                           label = "Region&Province Performance",
                                           status = "primary", 
                                           right = TRUE,
                                           value = FALSE)
                     )
                   ),
                   
                   fluidRow(
                     br(),
                     column(3, uiOutput("sub_top_c_ui")),
                     column(3, uiOutput("sub_measure_c_ui")),
                     column(3, uiOutput("sub_index_c_ui")),
                     column(3, uiOutput("sub_region_c_ui"))
                   ),
                   
                   fluidRow(column(12,
                                   checkboxInput("label_c",
                                                 "Show Data Labels",
                                                 value = FALSE,
                                                 width = NULL))),
                 
                   fluidRow(tags$div(
                     
                     tags$div(
                       column(9, downloadButton(outputId = "downloadData_c",
                                                label = "Download Plot Data"))
                     ),
                     
                     tags$div(
                       column(3, 
                              downloadButton(outputId = "downloadData_c1", 
                                             label = "Download Plot Data")),
                       
                       style = "display:inline-block;margin-down: 1px;vertical-align:middle"))),
                   br(),
                   fluidRow(column(6, plotlyOutput("chart_c", width = "80%", height = "550px")),
                            column(6, plotlyOutput("bar_chart_c", width = "80%", height = "550px")))
                   # fluidRow(
                   #   br(),
                   #   tags$div(
                   #     column(9,
                   #            downloadButton("downloadPlot_c", "Download Plot"))),
                   #   tags$div(
                   #     column(3,
                   #            downloadButton("downloadPlot_c1", "Download Plot")),
                   #     style = "display:inline-block;margin-down: 1px;vertical-align:middle"
                   #   )
                   #   
                   # )
                   
                   
                   
                 )
               ),
               
               fluidRow( 
                 br(),
                 box(
                   title = "2, CHPA Data", status = "primary", solidHeader = TRUE,
                   collapsible = FALSE,
                   width = 12,
                   br(),
                   fluidRow(column(12, div(DT::dataTableOutput("contents_c"), style = "font-size:80%")))
                 ))
               
             
      ),
      tabPanel(strong("Molecule"), value=3,
               fluidRow(
                 br(),
                 box(title = "China Market Summary Value(RMB)",
                     width = 12, 
                     solidHeader = TRUE,
                     status = "primary",
                     # Dynamic valueBoxes to show the KPIs
                     fluidRow(
                       column(width = 6,
                              offset = 1,
                              div(DT::dataTableOutput("summary_table_m"), style = "font-size:90%")),
                       column(width = 3,
                              offset = 1,
                              div(DT::dataTableOutput("summary_table1_m"), style = "font-size:90%"))
                       
                     )
                 )
               ),
               
               fluidRow(
                 box(
                   title = "Market Trend Performance", status = "primary", solidHeader = TRUE,
                   collapsible = FALSE,
                   width = 12,
                   # fluidRow(
                   #   br(),
                   #   column(3, 
                   #          materialSwitch(inputId = "bp_p_m", 
                   #                         label = "Molecule Performance",
                   #                         status = "primary", 
                   #                         right = TRUE,
                   #                         value = TRUE)
                   #   ),
                   #   column(3, 
                   #          materialSwitch(inputId = "rpp_p_m", 
                   #                         label = "Region&Province Performance",
                   #                         status = "primary", 
                   #                         right = TRUE,
                   #                         value = FALSE)
                   #   )
                   # ),
                   
                   # fluidRow(
                   #   br(),
                   #   # column(3, selectInput("sub_top_m", "Top Molecule", choices = NULL,
                   #   #                       multiple = TRUE)),
                   #   # column(3, selectInput("sub_measure_m", "Measure", choices = NULL)),
                   #   # column(3, selectInput("sub_index_m", "Index", choices = NULL)),
                   #   # column(3, 
                   #   #        selectInput("sub_region_m", "Region/Province", choices = NULL)
                   #   #        # uiOutput("ui_sub_region_m")
                   #   #        )
                   #   
                   #   column(3, uiOutput("sub_top_m_ui")),
                   #   column(3, uiOutput("sub_measure_m_ui")),
                   #   column(3, uiOutput("sub_index_m_ui")),
                   #   column(3, uiOutput("sub_region_m_ui"))
                   #   
                   # ),
                   
                   fluidRow(column(12,
                                   checkboxInput("label_m",
                                                 "Show Data Labels",
                                                 value = FALSE,
                                                 width = NULL))),
                   fluidRow(tags$div(
                     
                     tags$div(
                       column(9, downloadButton(outputId = "downloadData_m",
                                                label = "Download Plot Data"))
                     ),
                     
                     tags$div(
                       column(3, 
                              downloadButton(outputId = "downloadData_m1", 
                                             label = "Download Plot Data")),
                       
                       style = "display:inline-block;margin-down: 1px;vertical-align:middle"))),
                   br(),
                   
                   # fluidRow(
                   #   column(offset = 9, 
                   #          3, 
                   #          downloadButton(outputId = "downloadData_m", 
                   #                         label = "Download Plot Data"))
                   # ),
                   fluidRow(column(6, plotlyOutput("chart_m", width = "80%", height = "550px")),
                            column(6, plotlyOutput("bar_chart_m", width = "80%", height = "550px"))),
                   
                   fluidRow(column(6, textOutput("caption")),
                            column(6, textOutput("caption1")))
                   
                   # fluidRow(
                   #   br(),
                   #   tags$div(
                   #     column(9,
                   #            downloadButton("downloadPlot_m", "Download Plot"))),
                   #   tags$div(
                   #     column(3,
                   #            downloadButton("downloadPlot_m1", "Download Plot")),
                   #     style = "display:inline-block;margin-down: 1px;vertical-align:middle"
                   #   )
                   #   
                   # )
                 )
               ),
               
               fluidRow( 
                 br(),
                 box(
                   title = "CHPA Data", status = "primary", solidHeader = TRUE,
                   collapsible = FALSE,
                   width = 12,
                   br(),
                   fluidRow(column(12, div(DT::dataTableOutput("contents_m"), style = "font-size:80%")))
                 ))
               
               
  
      ),
      id = "tabselected"
    )
  )
    
)
