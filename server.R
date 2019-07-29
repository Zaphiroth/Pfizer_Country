options(shiny.maxRequestSize = 1000 * 1024 ^ 2)

load("./data/data.RData")
load("./data/raw_data_final.RData")
load("./data/raw_data_forecast.RData")
######################## load function graph1 ##################################

source("./functions/quadrant_table.R")

######################## load function graph1 ##################################

# source("./functions/99_graph1_corp.R")

######################## load function graph1m #################################

# source("./functions/99_graph1_corp_m.R")

######################## shiny server core code#################################


server <- function(input, output, session) {
  # summary <- reactive({
  #   if (is.null(input$summary))
  #     return(NULL)
  #   inFile.summary <- input$summary
  # 
  #   data <-
  #     read_csv(inFile.summary$datapath, locale = locale(encoding = "GB18030")) %>%
  #     data.frame(stringsAsFactors = FALSE)
  #   
  #   colnames(data) <- tolower(colnames(data))
  #   
  #   data
  #   
  # })
  
  if_chc <- reactive({
    if (input$chc == "yes") {
      if_chc <- c("City", "County", "CHC")
    } else if (input$chc == "no") {
      if_chc <- c("City", "County")
    } else {
      if_chc <- NULL
      stop("if_chc Error.")
    }
    
    if_chc
  })
  
  observeEvent(raw_data_final, {
    updateSelectInput(session,
                      "year",
                      choices = sort(unique(raw_data_final$year)),
                      selected = sort(unique(raw_data_final$year))[1])
  })
  
  
  observeEvent(raw_data_final, {
    updateSelectInput(session,
                      "mkt",
                      choices = sort(unique(raw_data_final$market)),
                      selected = sort(unique(raw_data_final$market))[1])
  })
  

  observeEvent(raw_data_final, {
    updateSelectInput(session, 
                      "province",
                      choices = sort(unique(raw_data_final$province)),
                      selected = sort(unique(raw_data_final$province))[1])
  })
  
  ##-- for summary tables and plots
  summary_data <- 
    eventReactive(input$goButton, {
      all_data = expand.grid(省份 = input$province,
                               城市级别 = c("1线城市", "2线城市", "3线城市", 
                                        "4线城市", "5线城市", "Total"),
                               stringsAsFactors = FALSE) %>%
        arrange(省份, 城市级别)
      
      # summary table
      data <- raw_data_final %>%
        filter(year %in% input$year, 
               market %in% input$mkt,
               province %in% input$province) %>%
        select(省份 = province, 城市级别 = `city.tier`, 地级市数量 = `地级市by.tier`, 
                 市辖县数量 = `县by.tier`, 县级市数量 = `县级市by.tier`) %>%
        distinct() %>%
        ungroup() 
      
      data1 <- data %>%
        group_by(省份, 城市级别 = "Total") %>%
        summarise(地级市数量 = sum(地级市数量, na.rm = TRUE),
                       市辖县数量 = sum(市辖县数量, na.rm = TRUE),
                       县级市数量 = sum(县级市数量, na.rm = TRUE))
      
      data2 <- bind_rows(data, data1) %>%
        arrange(省份, 城市级别)
      
      all_data_m <- all_data %>%
        left_join(data2) %>%
        group_by(城市级别) %>%
        summarise(地级市数量 = sum(地级市数量, na.rm = TRUE),
                       市辖县数量 = sum(市辖县数量, na.rm = TRUE),
                       县级市数量 = sum(县级市数量, na.rm = TRUE))
        
    
      # summary table1
      data3 <- raw_data_final %>%
        filter(year %in% input$year, 
               market %in% input$mkt,
               province %in% input$province) %>%
        select(省份 = province, 三级医院 = `三级.by.province`, 
                 二级医院 = `二级.by.province`, 
                 一级及以下 = `一级及其他.by.province`) %>%
        distinct() %>%
        ungroup() %>%
        summarise(三级医院 = sum(三级医院, na.rm = TRUE),
                      二级医院 = sum(二级医院, na.rm = TRUE),
                      一级及以下 = sum(一级及以下, na.rm = TRUE))
      
      # summary table2
      data4 <- raw_data_final %>% 
        filter(year %in% input$year, 
               market %in% input$mkt,
               province %in% input$province) %>% 
        select(`省份` = province,
               `县医院#` = county.hp.by.province,
               `城市医院#` = city.hp.by.province) %>% 
        distinct() %>% 
        ungroup() %>% 
        summarise(`县医院#` = sum(`县医院#`, na.rm = TRUE),
                  `城市医院#` = sum(`城市医院#`, na.rm = TRUE))
      
      
      list(summary_table = all_data_m,
           summary_table1 = data3,
           summary_table2 = data4)
    })
  
  output$summary_table <- renderDT({
    if (is.null(summary_data())) return(NULL)
    input$goButton
    isolate({
      DT::datatable(
        summary_data()$summary_table,
        rownames = FALSE,
        # extensions = c('FixedColumns', 'Buttons'),
        #filter = 'bottom',
        ##### this sentence need to be changed when new variables added
        options = list(
          # dom = '<"bottom">Bfrtpl',
          # buttons = I('colvis'),
          columnDefs = list(list(
            className = 'dt-center', targets = c(0, 1, 2, 3)
          )),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#008F91', 'color': '#fff'});",
            "}"
          ),
          paging = FALSE,
          scrollX = FALSE,
          searching = FALSE,
          ordering = FALSE,
          pageLength = 5,
          lengthChange = FALSE,
          bInfo = FALSE
        )
      ) %>% 
        formatStyle(
          "城市级别",
          target = "row",
          color = styleEqual("Total", "#008F91"),
          fontWeight = styleEqual("Total", "bold")
        )
    })
  })
  output$summary_table1 <- renderDT({
    if (is.null(summary_data())) return(NULL)
    input$goButton
    isolate({
      DT::datatable(
        summary_data()$summary_table1,
        rownames = FALSE,
        # extensions = c('FixedColumns', 'Buttons'),
        #filter = 'bottom',
        ##### this sentence need to be changed when new variables added
        options = list(
          # dom = '<"bottom">Bfrtpl',
          # buttons = I('colvis'),
          columnDefs = list(list(
            className = 'dt-center', targets = c(0, 1, 2)
          )),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#008F91', 'color': '#fff'});",
            "}"
          ),
          paging = FALSE,
          scrollX = FALSE,
          searching = FALSE,
          ordering = FALSE,
          pageLength = 5,
          lengthChange = FALSE,
          bInfo = FALSE
        )
      )
    })
  })
  
  output$summary_table2 <- renderDT({
    if (is.null(summary_data())) return(NULL)
    input$goButton
    isolate({
      DT::datatable(
        summary_data()$summary_table2,
        rownames = FALSE,
        # extensions = c('FixedColumns', 'Buttons'),
        #filter = 'bottom',
        ##### this sentence need to be changed when new variables added
        options = list(
          # dom = '<"bottom">Bfrtpl',
          # buttons = I('colvis'),
          columnDefs = list(
            list(
              className = 'dt-center',
              targets = c(0, 1)
            )
          ),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#008F91', 'color': '#fff'});",
            "}"
          ),
          paging = FALSE,
          scrollX = FALSE,
          searching = FALSE,
          ordering = FALSE,
          pageLength = 5,
          lengthChange = FALSE,
          bInfo = FALSE
        )
      )
    })
  })
  
  output$summary_bar1 <- renderPlotly({
    if (is.null(summary_data())) return(NULL)
    input$goButton
    isolate({
      plot_data <- summary_data()$summary_table1 %>% 
        melt()
      
      plot_ly(hoverinfo = "x+y") %>% 
        add_bars(x = plot_data$variable,
                 y = plot_data$value,
                 type = "bar",
                 text = plot_data$value,
                 textposition = "outside",
                 color = I("#93CDDD")) %>% 
        layout(
          showlegend = FALSE,
          xaxis = list(
            zeroline = FALSE,
            showline = FALSE,
            showgrid = FALSE,
            title = "",
            mirror = "ticks"
          ),
          yaxis = list(
            zeroline = TRUE,
            showline = FALSE,
            showgrid = FALSE,
            showticklabels = FALSE,
            title = "",
            mirror = "ticks",
            range = c(0, max(plot_data$value) * 1.2)
          )
        )
    })
  })
  
  output$summary_bar2 <- renderPlotly({
    if (is.null(summary_data())) return(NULL)
    input$goButton
    isolate({
      plot_data <- summary_data()$summary_table2 %>% 
        melt()
      
      plot_ly(hoverinfo = "x+y") %>% 
        add_bars(x = plot_data$variable,
                 y = plot_data$value,
                 type = "bar",
                 text = plot_data$value,
                 textposition = "outside",
                 color = I("#93CDDD")) %>% 
        layout(
          showlegend = FALSE,
          xaxis = list(
            zeroline = FALSE,
            showline = FALSE,
            showgrid = FALSE,
            title = "",
            mirror = "ticks"
          ),
          yaxis = list(
            zeroline = TRUE,
            showline = FALSE,
            showgrid = FALSE,
            showticklabels = FALSE,
            title = "",
            mirror = "ticks",
            range = c(0, max(plot_data$value) * 1.2)
          )
        )
    })
  })
  
  ##-- for current potential contribution
  contribution_data <- eventReactive(input$goButton, {
    if (is.null(input$year) | is.null(input$mkt) | is.null(input$province) | is.null(if_chc())) return(NULL)
    
    data <- raw_data_final %>% 
      filter(year %in% input$year, 
             market %in% input$mkt,
             province %in% input$province,
             channel %in% if_chc()) %>% 
      select(city, channel, value)
    
    mapping <- data.frame(city = rep(unique(data$city), each = 3),
                          channel = rep(c("City", "County", "CHC"), times = length(unique(data$city))),
                          stringsAsFactors = FALSE)
    
    data1 <- data %>% 
      right_join(mapping, by = c("city", "channel")) %>% 
      mutate(value = ifelse(is.na(value), 0, value)) %>% 
      dcast(city~channel,value.var = "value") %>% 
      mutate(`Total` = `City` + `County` + `CHC`) %>% 
      mutate(`City` = round(`City`/1000000, 2),
             `County` = round(`County`/1000000, 2),
             `CHC` = round(`CHC` / 1000000, 2),
             `Total` = round(`Total`/1000000, 2)) %>% 
      select("city", "City", "County", "CHC", "Total") %>% 
      arrange(-`Total`)
    
    ordering <- data1$city
    
    data2 <- data1 %>% 
      melt(id.vars = "city") %>% 
      dcast(variable~city, value.var = "value") %>% 
      mutate(variable = as.character(variable),
             variable = ifelse(variable == "City",
                               "城市医院",
                               ifelse(variable == "County",
                                      "县",
                                      ifelse(variable == "CHC",
                                             "社区",
                                             variable)))) %>% 
      select("城市" = "variable", ordering)
    data2[is.na(data2)] <- 0
    
    list(data = data2,
         ordering = ordering)
  })
  
  output$current_potential_by_city <- renderDT({
    if (is.null(contribution_data())) return(NULL)
    input$goButton
    isolate({
      DT::datatable(
        contribution_data()$data,
        rownames = FALSE,
        # extensions = c('FixedColumns', 'Buttons'),
        #filter = 'bottom',
        ##### this sentence need to be changed when new variables added
        options = list(
          # dom = '<"bottom">Bfrtpl',
          # buttons = I('colvis'),
          columnDefs = list(list(
            className = 'dt-center', targets = '_all'
          )),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#008F91', 'color': '#fff'});",
            "}"
          ),
          paging = FALSE,
          scrollX = FALSE,
          searching = FALSE,
          ordering = FALSE,
          pageLength = 5,
          lengthChange = FALSE,
          bInfo = FALSE
        )
      ) %>% 
        formatStyle(
          "城市",
          target = "row",
          color = styleEqual("Total", "#008F91"),
          fontWeight = styleEqual("Total", "bold")
        )
    })
  })

  output$city_rank_current_potential <- renderPlotly({
    if (is.null(contribution_data())) return(NULL)
    input$goButton
    isolate({
      plot_data <- contribution_data()$data %>%
        filter(`城市` == "Total") %>%
        melt()

      plot_ly(hoverinfo = "x+y") %>%
        add_bars(x = plot_data$variable,
                 y = plot_data$value,
                 type = "bar",
                 text = plot_data$value,
                 textposition = "outside",
                 name = "Total",
                 color = I("#4BACC6")) %>%
        layout(
          showlegend = TRUE,
          xaxis = list(
            zeroline = FALSE,
            showline = FALSE,
            showgrid = FALSE,
            title = "",
            mirror = "ticks"
          ),
          yaxis = list(
            zeroline = TRUE,
            showline = FALSE,
            showgrid = FALSE,
            showticklabels = FALSE,
            title = "",
            mirror = "ticks",
            range = c(0, max(plot_data$value) * 1.2)
          )
        )
    })
  })

  output$channel_distribution_current_potential_by_city <- renderPlotly({
    if (is.null(contribution_data())) return(NULL)
    input$goButton
    isolate({
      plot_data <- contribution_data()$data %>%
        melt() %>%
        dcast(variable~`城市`, value.var = "value") %>%
        mutate(y1 = round(`城市医院` / `Total`, 2),
               y2 = round(`县` / `Total`, 2))
      
      if ("CHC" %in% if_chc()) {
        plot_data <- plot_data %>% 
          mutate(y3 = round(`社区` / `Total`, 2))
      }

      p <- plot_ly(hoverinfo = "name+x+y") %>%
        add_bars(x = plot_data$variable,
                 y = plot_data$y1,
                 type = "bar",
                 name = "城市医院",
                 color = I("#9BBB59")) %>%
        add_bars(x = plot_data$variable,
                 y = plot_data$y2,
                 type = "bar",
                 name = "县",
                 color = I("#4BACC6"))
      
      if ("CHC" %in% if_chc()) {
        p <- p %>% 
          add_bars(x = plot_data$variable,
                   y = plot_data$y3,
                   type = "bar",
                   name = "社区",
                   color = I("#967846"))
      }
      
      p <- p %>%
        layout(
          showlegend = TRUE,
          barmode = "stack",
          xaxis = list(
            zeroline = FALSE,
            showline = FALSE,
            showgrid = FALSE,
            title = "",
            mirror = "ticks"
          ),
          yaxis =  list(
            zeroline = TRUE,
            showline = FALSE,
            showgrid = FALSE,
            showticklabels = FALSE,
            title = "",
            mirror = "ticks"
          )
        )
    })
  })
  
  ##-- for hospital counts
  hospital_data <- eventReactive(contribution_data(), {
    if (is.null(input$mkt) | is.null(input$province)) return(NULL)
    
    ordering <- contribution_data()$ordering
    
    data <- raw_data_forecast %>% 
      filter(market %in% input$mkt,
             province %in% input$province) %>% 
      select(city, channel, terminal.) %>% 
      distinct() %>% 
      dcast(city~channel, value.var = "terminal.")
    
    if (!("City" %in% names(data))) {
      data <- mutate(data, `City` = 0)
    }
    if (!("County" %in% names(data))) {
      data <- mutate(data, `County` = 0)
    }
    
    data1 <- data %>% 
      mutate(`Total` = `City` + `County`) %>% 
      melt(id.vars = "city") %>% 
      dcast(variable~city, value.var = "value") %>% 
      mutate(variable = as.character(variable),
             variable = ifelse(variable == "City",
                               "城市医院",
                               ifelse(variable == "County",
                                      "县",
                                      variable))) %>% 
      select("城市" = "variable", ordering)
    data1[is.na(data1)] <- 0
    
    data1
  })
  
  output$channel_dist_hospital_cnt_by_city <- renderDT({
    if (is.null(hospital_data())) return(NULL)
    input$goButton
    isolate({
      DT::datatable(
        hospital_data(),
        rownames = FALSE,
        # extensions = c('FixedColumns', 'Buttons'),
        #filter = 'bottom',
        ##### this sentence need to be changed when new variables added
        options = list(
          # dom = '<"bottom">Bfrtpl',
          # buttons = I('colvis'),
          columnDefs = list(list(
            className = 'dt-center', targets = '_all'
          )),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#008F91', 'color': '#fff'});",
            "}"
          ),
          paging = FALSE,
          scrollX = FALSE,
          searching = FALSE,
          ordering = FALSE,
          pageLength = 5,
          lengthChange = FALSE,
          bInfo = FALSE
        )
      ) %>% 
        formatStyle(
          "城市",
          target = "row",
          color = styleEqual("Total", "#008F91"),
          fontWeight = styleEqual("Total", "bold")
        )
    })
  })
  
  ##-- for total potential and share
  share_data <- eventReactive(input$goButton, {
    if (is.null(input$year) | is.null(input$mkt) | is.null(input$province) | is.null(if_chc())) return(NULL)
    
    data <- raw_data_final %>% 
      filter(year %in% input$year, 
             market %in% input$mkt,
             province %in% input$province,
             channel %in% if_chc()) %>% 
      select(city, value, internal.sales) %>% 
      group_by(city) %>% 
      summarise(value = sum(value, na.rm = TRUE),
                internal.sales = sum(internal.sales, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(share = round(internal.sales / value, 3),
             value = round(value/1000000, 2),
             internal.sales = round(internal.sales/1000000, 2)) %>% 
      melt(id.vars = "city") %>% 
      mutate(variable = as.character(variable),
             variable = ifelse(variable == "value",
                               "Total potential",
                               ifelse(variable == "internal.sales",
                                      "TTH",
                                      ifelse(variable == "share",
                                             "Share%",
                                             variable)))) %>% 
      dcast(city~variable, value.var = "value")
    data[is.na(data)] <- 0
    
    data
  })
  
  output$total_current_potential_share_by_city <- renderDT({
    if (is.null(share_data()) | is.null(contribution_data())) return(NULL)
    input$goButton
    isolate({
      ordering <- contribution_data()$ordering
      
      table_data <- share_data() %>% 
        mutate(`Share%` = paste0(`Share%` * 100, "%")) %>% 
        select("city", "Total potential", "TTH", "Share%") %>% 
        melt(id.vars = "city") %>% 
        dcast(variable~city, value.var = "value") %>% 
        select("城市" = "variable", ordering)
      
      DT::datatable(
        table_data,
        rownames = FALSE,
        # extensions = c('FixedColumns', 'Buttons'),
        #filter = 'bottom',
        ##### this sentence need to be changed when new variables added
        options = list(
          # dom = '<"bottom">Bfrtpl',
          # buttons = I('colvis'),
          columnDefs = list(list(
            className = 'dt-center', targets = '_all'
          )),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#008F91', 'color': '#fff'});",
            "}"
          ),
          paging = FALSE,
          scrollX = FALSE,
          searching = FALSE,
          ordering = FALSE,
          pageLength = 5,
          lengthChange = FALSE,
          bInfo = FALSE
        )
      ) %>% 
        formatStyle(
          "城市",
          target = "row",
          color = styleEqual("Share%", "#008F91"),
          fontWeight = styleEqual("Share%", "bold")
        )
    })
  })
  
  output$total_current_potential_share <- renderPlotly({
    if (is.null(share_data()) | is.null(contribution_data())) return(NULL)
    input$goButton
    isolate({
      ordering = contribution_data()$ordering
      
      plot_data <- share_data() %>% 
        mutate(city = factor(city, levels = ordering)) %>% 
        arrange(city)
      
      plot_ly(hoverinfo = "name+x+y") %>% 
        add_bars(x = plot_data$city,
                 y = plot_data$`Total potential`,
                 text = plot_data$`Total potential`,
                 textposition = "outside",
                 type = "bar",
                 name = "Total potential",
                 color = I("#9BBB59")) %>% 
        add_bars(x = plot_data$city,
                 y = plot_data$`TTH`,
                 text = plot_data$`TTH`,
                 textposition = "outside",
                 type = "bar",
                 name = "TTH",
                 color = I("#4BACC6")) %>% 
        add_trace(x = plot_data$city,
                  y = plot_data$`Share%`,
                  yaxis = "y2",
                  type = "scatter",
                  mode = "lines+markers",
                  name = "Share%",
                  color = I("#E46C0A")) %>% 
        add_annotations(x = plot_data$city,
                        y = plot_data$`Share%`,
                        text = paste0(plot_data$`Share%` * 100, "%"),
                        xref = "x",
                        yref = "y2",
                        xanchor = "center",
                        yanchor = "bottom",
                        showarrow = FALSE,
                        font = list(color = "#E46C0A")) %>% 
        layout(
          showlegend = TRUE,
          xaxis = list(
            zeroline = FALSE,
            showline = FALSE,
            showgrid = FALSE,
            title = "",
            mirror = "ticks"
          ),
          yaxis =  list(
            side = "left",
            zeroline = TRUE,
            showline = FALSE,
            showgrid = FALSE,
            showticklabels = FALSE,
            title = "",
            mirror = "ticks",
            range = c(0, max(plot_data$`Total potential`, 0.01) * 1.2)
          ),
          yaxis2 = list(
            overlaying = "y",
            side = "right",
            zeroline = TRUE,
            showline = FALSE,
            showgrid = FALSE,
            showticklabels = FALSE,
            title = "",
            mirror = "ticks",
            range = c(0, max(plot_data$`Share%`, 0.001) * 1.2)
          )
        )
    })
  })
  
  ##-- for city potential and share
  city_data <- eventReactive(contribution_data(), {
    if (is.null(input$year) | is.null(input$mkt) | is.null(input$province)) return(NULL)
    
    data <- raw_data_final %>% 
      filter(year %in% input$year, 
             market %in% input$mkt,
             province %in% input$province,
             channel %in% c("City")) %>% 
      select(city, value, internal.sales)
    
    if (length(data$city) == 0) {
      data <- bind_rows(data,
                        data.frame(city = contribution_data()$ordering,
                                   value = 0,
                                   internal.sales = 0,
                                   stringsAsFactors = FALSE))
    }
    
    data1 <- data %>% 
      group_by(city) %>% 
      summarise(value = sum(value, na.rm = TRUE),
                internal.sales = sum(internal.sales, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(share = round(internal.sales / value, 3),
             value = round(value/1000000, 2),
             internal.sales = round(internal.sales/1000000, 2)) %>% 
      melt(id.vars = "city") %>% 
      mutate(variable = as.character(variable),
             variable = ifelse(variable == "value",
                               "City hospitals potential",
                               ifelse(variable == "internal.sales",
                                      "TTH",
                                      ifelse(variable == "share",
                                             "Share%",
                                             variable)))) %>% 
      dcast(city~variable, value.var = "value")
    
    data1[is.na(data1)] <- 0
    
    data1
  })
  
  output$city_hospitals_current_potential_share_by_city <- renderDT({
    if (is.null(city_data()) | is.null(contribution_data())) return(NULL)
    input$goButton
    isolate({
      ordering <- contribution_data()$ordering
      
      table_data <- city_data() %>% 
        mutate(`Share%` = paste0(`Share%` * 100, "%")) %>% 
        select("city", "City hospitals potential", "TTH", "Share%") %>% 
        melt(id.vars = "city") %>% 
        dcast(variable~city, value.var = "value") %>% 
        select("城市" = "variable", ordering)
      
      DT::datatable(
        table_data,
        rownames = FALSE,
        # extensions = c('FixedColumns', 'Buttons'),
        #filter = 'bottom',
        ##### this sentence need to be changed when new variables added
        options = list(
          # dom = '<"bottom">Bfrtpl',
          # buttons = I('colvis'),
          columnDefs = list(list(
            className = 'dt-center', targets = '_all'
          )),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#008F91', 'color': '#fff'});",
            "}"
          ),
          paging = FALSE,
          scrollX = FALSE,
          searching = FALSE,
          ordering = FALSE,
          pageLength = 5,
          lengthChange = FALSE,
          bInfo = FALSE
        )
      ) %>% 
        formatStyle(
          "城市",
          target = "row",
          color = styleEqual("Share%", "#008F91"),
          fontWeight = styleEqual("Share%", "bold")
        )
    })
  })
  
  output$city_hospitals_current_potential_share <- renderPlotly({
    if (is.null(city_data()) | is.null(contribution_data())) return(NULL)
    input$goButton
    isolate({
      ordering <- contribution_data()$ordering
      
      plot_data <- city_data() %>% 
        mutate(city = factor(city, levels = ordering)) %>% 
        arrange(city)
      
      plot_ly(hoverinfo = "name+x+y") %>% 
        add_bars(x = plot_data$city,
                 y = plot_data$`City hospitals potential`,
                 text = plot_data$`City hospitals potential`,
                 textposition = "outside",
                 type = "bar",
                 name = "City hospitals potential",
                 color = I("#9BBB59")) %>% 
        add_bars(x = plot_data$city,
                 y = plot_data$`TTH`,
                 text = plot_data$`TTH`,
                 textposition = "outside",
                 type = "bar",
                 name = "TTH",
                 color = I("#4BACC6")) %>% 
        add_trace(x = plot_data$city,
                  y = plot_data$`Share%`,
                  yaxis = "y2",
                  type = "scatter",
                  mode = "lines+markers",
                  name = "Share%",
                  color = I("#E46C0A")) %>% 
        add_annotations(x = plot_data$city,
                        y = plot_data$`Share%`,
                        text = paste0(plot_data$`Share%` * 100, "%"),
                        xref = "x",
                        yref = "y2",
                        xanchor = "center",
                        yanchor = "bottom",
                        showarrow = FALSE,
                        font = list(color = "#E46C0A")) %>% 
        layout(
          showlegend = TRUE,
          xaxis = list(
            zeroline = FALSE,
            showline = FALSE,
            showgrid = FALSE,
            title = "",
            mirror = "ticks"
          ),
          yaxis =  list(
            side = "left",
            zeroline = TRUE,
            showline = FALSE,
            showgrid = FALSE,
            showticklabels = FALSE,
            title = "",
            mirror = "ticks",
            range = c(0, max(plot_data$`City hospitals potential`, 0.01) * 1.2)
          ),
          yaxis2 = list(
            overlaying = "y",
            side = "right",
            zeroline = FALSE,
            showline = FALSE,
            showgrid = FALSE,
            showticklabels = FALSE,
            title = "",
            mirror = "ticks",
            range = c(0, max(plot_data$`Share%`, 0.001) * 1.2)
          )
        )
    })
  })
  
  ##-- for county potential and share
  county_data <- eventReactive(contribution_data(), {
    if (is.null(input$year) | is.null(input$mkt) | is.null(input$province)) return(NULL)
    
    data <- raw_data_final %>% 
      filter(year %in% input$year, 
             market %in% input$mkt,
             province %in% input$province,
             channel %in% c("County")) %>% 
      select(city, value, internal.sales)
    
    if (length(data$city) != length(contribution_data()$ordering)) {
      data <- bind_rows(data,
                        data.frame(city = setdiff(contribution_data()$ordering, data$city),
                                   value = 0,
                                   internal.sales = 0,
                                   stringsAsFactors = FALSE))
    }
    
    data1 <- data %>% 
      group_by(city) %>% 
      summarise(value = sum(value, na.rm = TRUE),
                internal.sales = sum(internal.sales, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(share = round(internal.sales / value, 3),
             value = round(value/1000000, 2),
             internal.sales = round(internal.sales/1000000, 2)) %>% 
      melt(id.vars = "city") %>% 
      mutate(variable = as.character(variable),
             variable = ifelse(variable == "value",
                               "County hospitals potential",
                               ifelse(variable == "internal.sales",
                                      "TTH",
                                      ifelse(variable == "share",
                                             "Share%",
                                             variable)))) %>% 
      dcast(city~variable, value.var = "value")
    data1[is.na(data1)] <- 0
    
    data1
  })
  
  output$County_hospitals_current_potential_share_by_city <- renderDT({
    if (is.null(county_data()) | is.null(contribution_data())) return(NULL)
    input$goButton
    isolate({
      ordering <- contribution_data()$ordering
      
      table_data <- county_data() %>% 
        mutate(`Share%` = paste0(`Share%` * 100, "%")) %>% 
        select("city", "County hospitals potential", "TTH", "Share%") %>% 
        melt(id.vars = "city") %>% 
        dcast(variable~city, value.var = "value") %>% 
        select("城市" = "variable", ordering)
      
      DT::datatable(
        table_data,
        rownames = FALSE,
        # extensions = c('FixedColumns', 'Buttons'),
        #filter = 'bottom',
        ##### this sentence need to be changed when new variables added
        options = list(
          # dom = '<"bottom">Bfrtpl',
          # buttons = I('colvis'),
          columnDefs = list(list(
            className = 'dt-center', targets = '_all'
          )),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#008F91', 'color': '#fff'});",
            "}"
          ),
          paging = FALSE,
          scrollX = FALSE,
          searching = FALSE,
          ordering = FALSE,
          pageLength = 5,
          lengthChange = FALSE,
          bInfo = FALSE
        )
      ) %>% 
        formatStyle(
          "城市",
          target = "row",
          color = styleEqual("Share%", "#008F91"),
          fontWeight = styleEqual("Share%", "bold")
        )
    })
  })
  
  output$county_hospitals_current_potential_share <- renderPlotly({
    if (is.null(county_data()) | is.null(contribution_data())) return(NULL)
    input$goButton
    isolate({
      ordering <- contribution_data()$ordering
      
      plot_data <- county_data() %>% 
        mutate(city = factor(city, levels = ordering)) %>% 
        arrange(city)
      
      plot_ly(hoverinfo = "name+x+y") %>% 
        add_bars(x = plot_data$city,
                 y = plot_data$`County hospitals potential`,
                 text = plot_data$`County hospitals potential`,
                 textposition = "outside",
                 type = "bar",
                 name = "County hospitals potential",
                 color = I("#9BBB59")) %>% 
        add_bars(x = plot_data$city,
                 y = plot_data$`TTH`,
                 text = plot_data$`TTH`,
                 textposition = "outside",
                 type = "bar",
                 name = "TTH",
                 color = I("#4BACC6")) %>% 
        add_trace(x = plot_data$city,
                  y = plot_data$`Share%`,
                  yaxis = "y2",
                  type = "scatter",
                  mode = "lines+markers",
                  name = "Share%",
                  color = I("#E46C0A")) %>% 
        add_annotations(x = plot_data$city,
                        y = plot_data$`Share%`,
                        text = paste0(plot_data$`Share%` * 100, "%"),
                        xref = "x",
                        yref = "y2",
                        xanchor = "center",
                        yanchor = "bottom",
                        showarrow = FALSE,
                        font = list(color = "#E46C0A")) %>% 
        layout(
          showlegend = TRUE,
          xaxis = list(
            zeroline = FALSE,
            showline = FALSE,
            showgrid = FALSE,
            title = "",
            mirror = "ticks"
          ),
          yaxis =  list(
            side = "left",
            zeroline = TRUE,
            showline = FALSE,
            showgrid = FALSE,
            showticklabels = FALSE,
            title = "",
            mirror = "ticks",
            range = c(0, max(plot_data$`County hospitals potential`, 0.01) * 1.2)
          ),
          yaxis2 = list(
            overlaying = "y",
            side = "right",
            zeroline = FALSE,
            showline = FALSE,
            showgrid = FALSE,
            showticklabels = FALSE,
            title = "",
            mirror = "ticks",
            range = c(0, max(plot_data$`Share%`, 0.001) * 1.2)
          )
        )
    })
  })
  
  ##-- for potential growth
  growth_data <- eventReactive(contribution_data(), {
    if (input$year == 2016) return(NULL)
    if (is.null(input$year) | is.null(input$mkt) | is.null(input$province) | is.null(if_chc())) return(NULL)
    
    data <- raw_data_final %>% 
      filter(year %in% c(as.numeric(input$year)-1, as.numeric(input$year)), 
             market %in% input$mkt,
             province %in% input$province,
             channel %in% if_chc()) %>% 
      select(city, channel, year, value) %>% 
      dcast(city+year~channel, value.var = "value")
    
    if (!("City" %in% names(data))) {
      data <- mutate(data, `City` = 0)
    }
    if (!("County" %in% names(data))) {
      data <- mutate(data, `County` = 0)
    }
    if ("CHC" %in% if_chc() & !("CHC" %in% names(data))) {
      data <- mutate(data, `CHC` = 0)
    }
    
    data[is.na(data)] <- 0
    
    data1 <- data %>% 
      mutate(year = ifelse(year == input$year,
                           "r",
                           "p")) %>% 
      melt() %>% 
      dcast(city~variable+year, value.var = "value") %>% 
      mutate(city_growth = round(`City_r` / `City_p` - 1, 3),
             county_growth = round(`County_r` / `County_p` - 1, 3))
    
    if ("CHC" %in% if_chc()) {
      data1 <- data1 %>% 
        mutate(chc_growth = round(`CHC_r` / `CHC_p` - 1, 3),
               total_growth = round((`City_r` + `County_r` + `CHC_r`) / (`City_p` + `County_p` + `CHC_p`) - 1, 3)) %>% 
        select(city, city_growth, county_growth, chc_growth, total_growth)
    } else {
      data1 <- data1 %>% 
        mutate(total_growth = round((`City_r` + `County_r`) / (`City_p` + `County_p`) - 1, 3)) %>% 
        select(city, city_growth, county_growth, total_growth)
    }
    
    data1[is.na(data1)] <- 0
    
    data1
  })
  
  output$growth_current_potential_by_city <- renderDT({
    if (is.null(growth_data()) | is.null(contribution_data())) return(NULL)
    input$goButton
    isolate({
      ordering <- contribution_data()$ordering
      
      table_data <- growth_data() %>% 
        mutate_if(is.numeric, function(x) {paste0(x * 100, "%")}) %>% 
        melt(id.vars = "city") %>% 
        dcast(variable~city, value.var = "value") %>% 
        mutate(variable = ifelse(variable == "city_growth",
                                 "城市医院",
                                 ifelse(variable == "county_growth",
                                        "县",
                                        ifelse(variable == "chc_growth",
                                               "社区",
                                               ifelse(variable == "total_growth",
                                                      "Total",
                                                      variable))))) %>% 
        select("城市" = "variable", ordering)
      
      DT::datatable(
        table_data,
        rownames = FALSE,
        # extensions = c('FixedColumns', 'Buttons'),
        #filter = 'bottom',
        ##### this sentence need to be changed when new variables added
        options = list(
          # dom = '<"bottom">Bfrtpl',
          # buttons = I('colvis'),
          columnDefs = list(list(
            className = 'dt-center', targets = '_all'
          )),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#008F91', 'color': '#fff'});",
            "}"
          ),
          paging = FALSE,
          scrollX = FALSE,
          searching = FALSE,
          ordering = FALSE,
          pageLength = 5,
          lengthChange = FALSE,
          bInfo = FALSE
        )
      ) %>% 
        formatStyle(
          "城市",
          target = "row",
          color = styleEqual("Total", "#008F91"),
          fontWeight = styleEqual("Total", "bold")
        )
    })
  })
  
  output$growth_current_potential_by_city_chart <- renderPlotly({
    if (is.null(growth_data()) | is.null(contribution_data())) return(NULL)
    input$goButton
    isolate({
      ordering <- contribution_data()$ordering
      
      plot_data <- growth_data() %>% 
        mutate(city = factor(city, levels = ordering)) %>% 
        arrange(city)
      
      plot_ly(hoverinfo = "name+x+y") %>% 
        add_bars(x = plot_data$city,
                 y = plot_data$city_growth * 100,
                 text = paste0(plot_data$city_growth * 100, "%"),
                 textposition = "outside",
                 type = "bar",
                 name = "城市医院",
                 color = I("#9BBB59")) %>% 
        add_bars(x = plot_data$city,
                 y = plot_data$county_growth * 100,
                 text = paste0(plot_data$county_growth * 100, "%"),
                 textposition = "outside",
                 type = "bar",
                 name = "县",
                 color = I("#4BACC6")) %>% 
        layout(
          showlegend = TRUE,
          xaxis = list(
            zeroline = FALSE,
            showline = FALSE,
            showgrid = FALSE,
            title = "",
            mirror = "ticks"
          ),
          yaxis =  list(
            zeroline = TRUE,
            showline = FALSE,
            showgrid = FALSE,
            ticksuffix = "%",
            title = "",
            mirror = "ticks",
            range = c(0, max(plot_data$city_growth, plot_data$county_growth) * 120)
          )
        )
    })
  })
  
  ##-- detail
  detail_data <- eventReactive(input$refresh, {
    if (is.null(input$channel) | is.null(input$mkt) | is.null(input$province) | 
        is.null(input$chc) | is.null(input$potential_div) | is.null(input$share_div))
      return(NULL)
    
    data <- raw_data_forecast %>% 
      mutate(channel = tolower(channel)) %>% 
      select("province", "city", "channel", "market",
             "terminal" = "terminal.",
             "potential_2020" = "x2020.city",
             "potential_chc_2020" = "x2020.city.chc",
             "chc_2020" = "x2020.chc",
             "potential_2018" = "x2018.city",
             "potential_chc_2018" = "x2018.city.chc",
             "chc_2018" = "x2018.chc",
             "molecule_2018" = "molecule_sales.2018",
             "internal" = "tth.2018") %>% 
      mutate(chc_2020 = gsub(",", "", chc_2020),
             chc_2020 = ifelse(chc_2020 == "-",
                               0,
                               chc_2020),
             chc_2020 = as.numeric(chc_2020),
             chc_2018 = gsub(",", "", chc_2018),
             chc_2018 = ifelse(chc_2018 == "-",
                               0,
                               chc_2018),
             chc_2018 = as.numeric(chc_2018),
             internal = gsub(",", "", internal),
             internal = ifelse(internal == "-",
                               0,
                               internal),
             internal = as.numeric(internal)) %>% 
      mutate(share_pot = internal / potential_2018,
             share_pot_chc = internal / potential_chc_2018,
             share_mol = internal / molecule_2018) %>% 
      filter(channel %in% input$channel,
             market %in% input$mkt,
             province %in% input$province) %>% 
      select(city, terminal, potential_2018, potential_2020, potential_chc_2018, potential_chc_2020, 
             chc_2020, chc_2018, molecule_2018, internal, share_mol) %>% 
      mutate(potential_con_2018 = potential_2018 / sum(potential_2018, na.rm = TRUE),
             potential_chc_con_2018 = potential_chc_2018 / sum(potential_chc_2018, na.rm = TRUE),
             potential_con_2020 = potential_2020 / sum(potential_2020, na.rm = TRUE),
             potential_chc_con_2020 = potential_chc_2020 / sum(potential_chc_2020, na.rm = TRUE),
             internal_con = internal / sum(internal, na.rm = TRUE))
    
    if (input$chc == "no") {
      data1 <- data %>% 
        arrange(-potential_con_2020) %>% 
        mutate(potential_con_cum = cumsum(potential_con_2020))
      
    } else if (input$chc == "yes") {
      data1 <- data %>% 
        arrange(-potential_chc_con_2020) %>% 
        mutate(potential_con_cum = cumsum(potential_chc_con_2020))
      
    } else {
      stop("Detail CHC Error.")
    }
    
    data2 <- data1 %>% 
      mutate(segment = ifelse(potential_con_cum > as.numeric(input$potential_div)/100 & share_mol >= input$share_div/100,
                              1,
                              ifelse(potential_con_cum <= as.numeric(input$potential_div)/100 & share_mol >= input$share_div/100,
                                     2,
                                     ifelse(potential_con_cum > as.numeric(input$potential_div)/100 & share_mol < input$share_div/100,
                                            3,
                                            ifelse(potential_con_cum <= as.numeric(input$potential_div)/100 & share_mol < input$share_div/100,
                                                   4,
                                                   0)))))
    
    data2
  })
  
  output$opportunistic <- renderDT({
    if (is.null(detail_data())) return(NULL)
    input$refresh
    isolate({
      table_data <- quadrant(detail_data(), seg = 1, chc = input$chc)
      
      DT::datatable(
        table_data,
        rownames = FALSE,
        colnames = NULL,
        options = list(
          columnDefs = list(
            list(className = "dt-left",
                 targets = 0),
            list(className = "dt-center",
                 targets = 1)
          ),
          # initComplete = JS(
          #   "function(settings, json) {",
          #   "$(this.api().table().header()).css({'background-color': '#008F91', 'color': '#fff'});",
          #   "}"
          # ),
          paging = FALSE,
          scrollX = FALSE,
          searching = FALSE,
          ordering = FALSE,
          pageLength = 6,
          lengthChange = FALSE,
          bInfo = FALSE
        )
      ) %>% 
        formatStyle(
          c("key"),
          fontWeight = "bold"
        ) %>% 
        formatStyle(
          c("key", "value"),
          backgroundColor = "#DAEEF3"
        )
    })
  })
  
  output$defend <- renderDT({
    if (is.null(detail_data())) return(NULL)
    input$refresh
    isolate({
      table_data <- quadrant(detail_data(), seg = 2, chc = input$chc)
      
      DT::datatable(
        table_data,
        rownames = FALSE,
        colnames = NULL,
        options = list(
          columnDefs = list(
            list(className = "dt-left",
                 targets = 0),
            list(className = "dt-center",
                 targets = 1)
          ),
          # initComplete = JS(
          #   "function(settings, json) {",
          #   "$(this.api().table().header()).css({'background-color': '#008F91', 'color': '#fff'});",
          #   "}"
          # ),
          paging = FALSE,
          scrollX = FALSE,
          searching = FALSE,
          ordering = FALSE,
          pageLength = 6,
          lengthChange = FALSE,
          bInfo = FALSE
        )
      ) %>% 
        formatStyle(
          c("key"),
          fontWeight = "bold"
        ) %>% 
        formatStyle(
          c("key", "value"),
          backgroundColor = "#EBF1DE"
        )
    })
  })
  
  output$broad <- renderDT({
    if (is.null(detail_data())) return(NULL)
    input$refresh
    isolate({
      table_data <- quadrant(detail_data(), seg = 3, chc = input$chc)
      
      DT::datatable(
        table_data,
        rownames = FALSE,
        colnames = NULL,
        options = list(
          columnDefs = list(
            list(className = "dt-left",
                 targets = 0),
            list(className = "dt-center",
                 targets = 1)
          ),
          # initComplete = JS(
          #   "function(settings, json) {",
          #   "$(this.api().table().header()).css({'background-color': '#008F91', 'color': '#fff'});",
          #   "}"
          # ),
          paging = FALSE,
          scrollX = FALSE,
          searching = FALSE,
          ordering = FALSE,
          pageLength = 6,
          lengthChange = FALSE,
          bInfo = FALSE
        )
      ) %>% 
        formatStyle(
          c("key"),
          fontWeight = "bold"
        ) %>% 
        formatStyle(
          c("key", "value"),
          backgroundColor = "#EBF1DE"
        )
    })
  })
  
  output$top <- renderDT({
    if (is.null(detail_data())) return(NULL)
    input$refresh
    isolate({
      table_data <- quadrant(detail_data(), seg = 4, chc = input$chc)
      
      DT::datatable(
        table_data,
        rownames = FALSE,
        colnames = NULL,
        options = list(
          columnDefs = list(
            list(className = "dt-left",
                 targets = 0),
            list(className = "dt-center",
                 targets = 1)
          ),
          # initComplete = JS(
          #   "function(settings, json) {",
          #   "$(this.api().table().header()).css({'background-color': '#008F91', 'color': '#fff'});",
          #   "}"
          # ),
          paging = FALSE,
          scrollX = FALSE,
          searching = FALSE,
          ordering = FALSE,
          pageLength = 6,
          lengthChange = FALSE,
          bInfo = FALSE
        )
      ) %>% 
        formatStyle(
          c("key"),
          fontWeight = "bold"
        ) %>% 
        formatStyle(
          c("key", "value"),
          backgroundColor = "#DAEEF3"
        )
    })
  })
  
  output$scatter <- renderPlotly({
    if (is.null(detail_data()) | is.null(input$potential_div) | is.null(input$share_div)) return(NULL)
    input$refresh
    isolate({
      plot_data <- detail_data()
      
      p <- plot_ly(hoverinfo = "name+x+y")
      
      for (i in unique(plot_data$city)) {
        p <- p %>% 
          add_trace(x = plot_data$potential_con_cum[which(plot_data$city == i)],
                    y = plot_data$share_mol[which(plot_data$city == i)],
                    type = "scatter",
                    mode = "markers",
                    name = i,
                    color = I("#2C5D98"))
      }
      
      p <- p %>% 
        add_segments(x = 0,
                     xend = 1,
                     y = input$share_div/100,
                     yend = input$share_div/100,
                     color = I("#FF0000")) %>% 
        add_segments(x = input$potential_div/100,
                     xend = input$potential_div/100,
                     y = 0,
                     yend = 1,
                     color = I("#FF0000")) %>% 
        layout(
          showlegend = FALSE,
          xaxis = list(
            range = c(1, 0),
            zeroline = FALSE,
            showline = TRUE,
            showgrid = TRUE,
            title = "Cumulative of 2020 Market Potential",
            mirror = "ticks"
          ),
          yaxis = list(
            side = "right",
            range = c(0, 1),
            zeroline = FALSE,
            showline = TRUE,
            showgrid = TRUE,
            title = "Share(TTH/Molecule)",
            mirror = "ticks"
          )
        )
      
      p
    })
  })
  
  output$detail <- renderDT({
    if (is.null(detail_data())) return(NULL)
    input$refresh
    isolate({
      table_data <- detail_data() %>% 
        mutate(segment = ifelse(segment == 1,
                                "Opportunistic",
                                ifelse(segment == 2,
                                       "Defend",
                                       ifelse(segment == 3,
                                              "Broad Coverage",
                                              ifelse(segment == 4,
                                                     "Top Priority",
                                                     0)))))
      
      if (input$chc == "no") {
        total_data <- data.table(`City` = "Total",
                                 `Type` = NA,
                                 `Terminal#` = sum(table_data$terminal, na.rm = TRUE),
                                 `Potential-2020` = sum(table_data$potential_2020, na.rm = TRUE),
                                 `CHC Potential-2020` = sum(table_data$chc_2020, na.rm = TRUE),
                                 `2020 Potential Con%` = 1,
                                 `Potential-2018` = sum(table_data$potential_2018, na.rm = TRUE),
                                 `CHC Potential-2018` = sum(table_data$chc_2018, na.rm = TRUE),
                                 `2018 Potential Con%` = 1,
                                 `2018 Molecule Sales` = sum(table_data$molecule_2018, na.rm = TRUE),
                                 `Share%(TTH/Molecule)` = sum(table_data$internal, na.rm = TRUE) / sum(table_data$molecule_2018, na.rm = TRUE),
                                 `Internal` = sum(table_data$internal, na.rm = TRUE),
                                 stringsAsFactors = FALSE)
        
        table_data1 <- table_data %>% 
          select("City" = "city",
                 "Type" = "segment",
                 "Terminal#" = "terminal",
                 "Potential-2020" = "potential_2020",
                 "CHC Potential-2020" = "chc_2020",
                 "2020 Potential Con%" = "potential_con_2020",
                 "Potential-2018" = "potential_2018",
                 "CHC Potential-2018" = "chc_2018",
                 "2018 Potential Con%" = "potential_con_2018",
                 "2018 Molecule Sales" = "molecule_2018",
                 "Share%(TTH/Molecule)" = "share_mol",
                 "Internal" = "internal")
        
      } else if (input$chc == "yes") {
        total_data <- data.table(`City` = "Total",
                                 `Type` = NA,
                                 `Terminal#` = sum(table_data$terminal, na.rm = TRUE),
                                 `Potential-2020` = sum(table_data$potential_chc_2020, na.rm = TRUE),
                                 `CHC Potential-2020` = sum(table_data$chc_2020, na.rm = TRUE),
                                 `2020 Potential Con%` = 1,
                                 `Potential-2018` = sum(table_data$potential_chc_2018, na.rm = TRUE),
                                 `CHC Potential-2018` = sum(table_data$chc_2018, na.rm = TRUE),
                                 `2018 Potential Con%` = 1,
                                 `2018 Molecule Sales` = sum(table_data$molecule_2018, na.rm = TRUE),
                                 `Share%(TTH/Molecule)` = sum(table_data$internal, na.rm = TRUE) / sum(table_data$molecule_2018, na.rm = TRUE),
                                 `Internal` = sum(table_data$internal, na.rm = TRUE),
                                 stringsAsFactors = FALSE)
        
        table_data1 <- table_data %>% 
          select("City" = "city",
                 "Type" = "segment",
                 "Terminal#" = "terminal",
                 "Potential-2020" = "potential_chc_2020",
                 "CHC Potential-2020" = "chc_2020",
                 "2020 Potential Con%" = "potential_chc_con_2020",
                 "Potential-2018" = "potential_chc_2018",
                 "CHC Potential-2018" = "chc_2018",
                 "2018 Potential Con%" = "potential_chc_con_2018",
                 "2018 Molecule Sales" = "molecule_2018",
                 "Share%(TTH/Molecule)" = "share_mol",
                 "Internal" = "internal")
        
      } else {
        stop("Detail Table CHC Error.")
      }
      
      table_data2 <- bind_rows(total_data, table_data1)
      
      DT::datatable(
        table_data2,
        rownames = FALSE,
        options = list(
          columnDefs = list(
            list(className = "dt-center",
                 targets = "_all")
          ),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#008F91', 'color': '#fff'});",
            "}"
          ),
          paging = FALSE,
          scrollX = FALSE,
          searching = FALSE,
          ordering = FALSE,
          pageLength = 5,
          lengthChange = FALSE,
          bInfo = FALSE
        )
      ) %>% 
        formatStyle(
          "City",
          target = "row",
          fontWeight = styleEqual("Total", "bold")
        ) %>% 
        formatPercentage(
          c("2020 Potential Con%", "2018 Potential Con%", "Share%(TTH/Molecule)"),
          digits = 2
        )
    })
  })
  
  
  
  
}
