options(shiny.maxRequestSize = 1000 * 1024 ^ 2)

load("./data/data.RData")
######################## load function graph1 ##################################

source("./functions/99_graph1_corp.R")

######################## load function graph1m #################################

source("./functions/99_graph1_corp_m.R")

######################## shiny server core code#################################


server <- function(input, output, session) {
  summary <- reactive({
    if (is.null(input$summary))
      return(NULL)
    inFile.summary <- input$summary
  
    data <- 
      read_csv(inFile.summary$datapath, locale = locale(encoding = "GB18030")) %>% 
      data.frame(stringsAsFactors = FALSE)
    
    colnames(data) <- tolower(colnames(data))
    
    data
    
  })
  
  
  observeEvent(input$summary, {
    updateSelectInput(session,
                      "year",
                      choices = sort(unique(summary()$year)),
                      selected = sort(unique(summary()$year))[1])
  })
  
  
  observeEvent(input$summary, {
    updateSelectInput(session,
                      "mkt",
                      choices = sort(unique(summary()$market)),
                      selected = sort(unique(summary()$market))[1])
  })
  

  observeEvent(input$summary, {
    updateSelectInput(session, 
                      "province",
                      choices = sort(unique(summary()$province)),
                      selected = sort(unique(summary()$province))[1])
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
      data <- summary() %>%
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
      data3 <- summary() %>%
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
      data4 <- summary() %>% 
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
    data <- summary() %>% 
      filter(year %in% input$year, 
             market %in% input$mkt,
             province %in% input$province,
             channel %in% c("City", "County")) %>% 
      select(city, channel, value)
    
    mapping <- data.frame(city = rep(unique(data$city), each = 2),
                          channel = rep(c("City", "County"), times = length(unique(data$city))))
    
    data1 <- data %>% 
      right_join(mapping, by = c("city", "channel")) %>% 
      mutate(value = ifelse(is.na(value), 0, value)) %>% 
      dcast(city~channel,value.var = "value") %>% 
      mutate(`Total` = `City` + `County`) %>% 
      mutate(`City` = round(`City`/1000000, 2),
             `County` = round(`County`/1000000, 2),
             `Total` = round(`Total`/1000000, 2)) %>% 
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
                                      variable))) %>% 
      select("城市" = "variable", ordering)
    data2[is.na(data2)] <- 0
    
    return(list(data = data2,
                ordering = ordering))
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

      plot_ly(hoverinfo = "x+y") %>%
        add_bars(x = plot_data$variable,
                 y = plot_data$y1,
                 type = "bar",
                 name = "城市医院",
                 color = I("#9BBB59")) %>%
        add_bars(x = plot_data$variable,
                 y = plot_data$y2,
                 type = "bar",
                 name = "县",
                 color = I("#4BACC6")) %>%
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
    ordering <- contribution_data()$ordering
    
    data <- summary() %>% 
      filter(year %in% input$year, 
             market %in% input$mkt,
             province %in% input$province,
             channel %in% c("City", "County")) %>% 
      select(city, city.hp.by.province, county.hp.by.province) %>% 
      distinct() %>% 
      rename("city_count" = "city.hp.by.province",
             "county_count" = "county.hp.by.province") %>% 
      mutate(`Total` = `city_count` + `county_count`) %>% 
      melt(id.vars = "city") %>% 
      dcast(variable~city, value.var = "value") %>% 
      mutate(variable = as.character(variable),
             variable = ifelse(variable == "city_count",
                               "城市医院",
                               ifelse(variable == "county_count",
                                      "县",
                                      variable))) %>% 
      select("城市" = "variable", ordering)
    data[is.na(data)] <- 0
    
    return(data)
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
    data <- summary() %>% 
      filter(year %in% input$year, 
             market %in% input$mkt,
             province %in% input$province,
             channel %in% c("City", "County")) %>% 
      select(city, value, internal.sales) %>% 
      group_by(city) %>% 
      summarise(value = sum(value, na.rm = TRUE),
                internal.sales = sum(internal.sales, na.rm = TRUE)) %>% 
      ungroup() %>% 
      mutate(share = round(internal.sales / value, 3),
             value = round(value/1000000, 2),
             internal.sales = round(internal.sales/1000000, 2)) %>% 
      melt(id.vars = "city") %>% 
      mutate(variable = ifelse(variable == "value",
                               "Total potential",
                               ifelse(variable == "internal.sales",
                                      "TTH",
                                      ifelse(variable == "share",
                                             "Share%",
                                             variable))))
    data[is.na(data)] <- 0
    
    return(data)
  })
  
  output$total_current_potential_share_by_city <- renderDT({
    if (is.null(share_data()) | is.null(contribution_data())) return(NULL)
    input$goButton
    isolate({
      ordering <- contribution_data()$ordering
      
      table_data <- share_data() %>% 
        mutate(value = ifelse(variable == "Share%",
                              paste0(value*100, "%"),
                              value)) %>% 
        dcast(variable~city, value.var = "value") %>% 
        .[c(which(.$variable == "Total potential"), which(.$variable == "TTH"), which(.$variable == "Share%")), ] %>% 
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
      
      plot_ly(hoverinfo = "x+y") %>% 
        add_bars(x = plot_data$city[which(plot_data$variable == "Total potential")],
                 y = plot_data$value[which(plot_data$variable == "Total potential")],
                 text = plot_data$value[which(plot_data$variable == "Total potential")],
                 textposition = "outside",
                 type = "bar",
                 name = "Total potential",
                 color = I("#9BBB59")) %>% 
        add_bars(x = plot_data$city[which(plot_data$variable == "TTH")],
                 y = plot_data$value[which(plot_data$variable == "TTH")],
                 text = plot_data$value[which(plot_data$variable == "TTH")],
                 textposition = "outside",
                 type = "bar",
                 name = "TTH",
                 color = I("#4BACC6")) %>% 
        add_trace(x = plot_data$city[which(plot_data$variable == "Share%")],
                  y = plot_data$value[which(plot_data$variable == "Share%")],
                  yaxis = "y2",
                  type = "scatter",
                  mode = "lines+markers",
                  name = "Share%",
                  color = I("#E46C0A")) %>% 
        add_annotations(x = plot_data$city[which(plot_data$variable == "Share%")],
                        y = plot_data$value[which(plot_data$variable == "Share%")],
                        text = paste0(plot_data$value[which(plot_data$variable == "Share%")]*100, "%"),
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
            range = c(0, max(plot_data$value[which(plot_data$variable == "Total potential")], 0.01) * 1.2)
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
            range = c(0, max(plot_data$value[which(plot_data$variable == "Share%")], 0.001) * 1.2)
          )
        )
    })
  })
  
  ##-- for city potential and share
  city_data <- eventReactive(contribution_data(), {
    data <- summary() %>% 
      filter(year %in% input$year, 
             market %in% input$mkt,
             province %in% input$province,
             channel %in% c("City")) %>% 
      select(city, value, internal.sales)
    
    if (length(data$city) == 0) {
      data <- bind_rows(data,
                        data.frame(city = contribution_data()$ordering,
                                   value = 0,
                                   internal.sales = 0))
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
      mutate(variable = ifelse(variable == "value",
                               "Total potential",
                               ifelse(variable == "internal.sales",
                                      "TTH",
                                      ifelse(variable == "share",
                                             "Share%",
                                             variable))))
    data1[is.na(data1)] <- 0
    
    return(data1)
  })
  
  output$city_hospitals_current_potential_share_by_city <- renderDT({
    if (is.null(city_data()) | is.null(contribution_data())) return(NULL)
    input$goButton
    isolate({
      ordering <- contribution_data()$ordering
      
      table_data <- city_data() %>% 
        mutate(value = ifelse(variable == "Share%",
                              paste0(value*100, "%"),
                              value)) %>% 
        dcast(variable~city, value.var = "value") %>% 
        .[c(which(.$variable == "Total potential"), which(.$variable == "TTH"), which(.$variable == "Share%")), ] %>% 
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
      ordering = contribution_data()$ordering
      plot_data <- city_data() %>% 
        mutate(city = factor(city, levels = ordering)) %>% 
        arrange(city)
      
      plot_ly(hoverinfo = "x+y") %>% 
        add_bars(x = plot_data$city[which(plot_data$variable == "Total potential")],
                 y = plot_data$value[which(plot_data$variable == "Total potential")],
                 text = plot_data$value[which(plot_data$variable == "Total potential")],
                 textposition = "outside",
                 type = "bar",
                 name = "Total potential",
                 color = I("#9BBB59")) %>% 
        add_bars(x = plot_data$city[which(plot_data$variable == "TTH")],
                 y = plot_data$value[which(plot_data$variable == "TTH")],
                 text = plot_data$value[which(plot_data$variable == "TTH")],
                 textposition = "outside",
                 type = "bar",
                 name = "TTH",
                 color = I("#4BACC6")) %>% 
        add_trace(x = plot_data$city[which(plot_data$variable == "Share%")],
                  y = plot_data$value[which(plot_data$variable == "Share%")],
                  yaxis = "y2",
                  type = "scatter",
                  mode = "lines+markers",
                  name = "Share%",
                  color = I("#E46C0A")) %>% 
        add_annotations(x = plot_data$city[which(plot_data$variable == "Share%")],
                        y = plot_data$value[which(plot_data$variable == "Share%")],
                        text = paste0(plot_data$value[which(plot_data$variable == "Share%")]*100, "%"),
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
            range = c(0, max(plot_data$value[which(plot_data$variable == "Total potential")], 0.01) * 1.2)
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
            range = c(0, max(plot_data$value[which(plot_data$variable == "Share%")], 0.001) * 1.2)
          )
        )
    })
  })
  
  ##-- for county potential and share
  county_data <- eventReactive(contribution_data(), {
    data <- summary() %>% 
      filter(year %in% input$year, 
             market %in% input$mkt,
             province %in% input$province,
             channel %in% c("County")) %>% 
      select(city, value, internal.sales)
    
    if (length(data$city) != length(contribution_data()$ordering)) {
      data <- bind_rows(data,
                        data.frame(city = setdiff(contribution_data()$ordering, data$city),
                                   value = 0,
                                   internal.sales = 0))
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
      mutate(variable = ifelse(variable == "value",
                               "Total potential",
                               ifelse(variable == "internal.sales",
                                      "TTH",
                                      ifelse(variable == "share",
                                             "Share%",
                                             variable))))
    data1[is.na(data1)] <- 0
    
    return(data1)
  })
  
  output$County_hospitals_current_potential_share_by_city <- renderDT({
    if (is.null(county_data()) | is.null(contribution_data())) return(NULL)
    input$goButton
    isolate({
      ordering <- contribution_data()$ordering
      
      table_data <- county_data() %>% 
        mutate(value = ifelse(variable == "Share%",
                              paste0(value*100, "%"),
                              value)) %>% 
        dcast(variable~city, value.var = "value") %>% 
        .[c(which(.$variable == "Total potential"), which(.$variable == "TTH"), which(.$variable == "Share%")), ] %>% 
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
      ordering = contribution_data()$ordering
      plot_data <- county_data() %>% 
        mutate(city = factor(city, levels = ordering)) %>% 
        arrange(city)
      
      plot_ly(hoverinfo = "x+y") %>% 
        add_bars(x = plot_data$city[which(plot_data$variable == "Total potential")],
                 y = plot_data$value[which(plot_data$variable == "Total potential")],
                 text = plot_data$value[which(plot_data$variable == "Total potential")],
                 textposition = "outside",
                 type = "bar",
                 name = "Total potential",
                 color = I("#9BBB59")) %>% 
        add_bars(x = plot_data$city[which(plot_data$variable == "TTH")],
                 y = plot_data$value[which(plot_data$variable == "TTH")],
                 text = plot_data$value[which(plot_data$variable == "TTH")],
                 textposition = "outside",
                 type = "bar",
                 name = "TTH",
                 color = I("#4BACC6")) %>% 
        add_trace(x = plot_data$city[which(plot_data$variable == "Share%")],
                  y = plot_data$value[which(plot_data$variable == "Share%")],
                  yaxis = "y2",
                  type = "scatter",
                  mode = "lines+markers",
                  name = "Share%",
                  color = I("#E46C0A")) %>% 
        add_annotations(x = plot_data$city[which(plot_data$variable == "Share%")],
                        y = plot_data$value[which(plot_data$variable == "Share%")],
                        text = paste0(plot_data$value[which(plot_data$variable == "Share%")]*100, "%"),
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
            range = c(0, max(plot_data$value[which(plot_data$variable == "Total potential")], 0.01) * 1.2)
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
            range = c(0, max(plot_data$value[which(plot_data$variable == "Share%")], 0.001) * 1.2)
          )
        )
    })
  })
  
  ##-- for potential growth
  growth_data <- eventReactive(contribution_data(), {
    if (input$year == 2016) return(NULL)
    data <- summary() %>% 
      filter(year %in% c(as.numeric(input$year)-1, input$year), 
             market %in% input$mkt,
             province %in% input$province,
             channel %in% c("City", "County")) %>% 
      select(city, channel, year, value)
    
    if (!("City" %in% data$channel)) {
      data <- bind_rows(data,
                        data.frame(city = contribution_data()$ordering,
                                   channel = "City",
                                   year = c(as.numeric(input$year)-1, as.numeric(input$year)),
                                   value = 0))
    } else if (!("County" %in% data$channel)) {
      data <- bind_rows(data,
                        data.frame(city = contribution_data()$ordering,
                                   channel = "County",
                                   year = c(as.numeric(input$year)-1, as.numeric(input$year)),
                                   value = 0))
    }
    
    data1 <- data %>% 
      mutate(year = ifelse(year == input$year,
                           "r",
                           "p")) %>% 
      dcast(city~channel+year, value.var = "value") %>% 
      mutate(city_growth = round(`City_r` / `City_p` - 1, 3),
             county_growth = round(`County_r` / `County_p` - 1, 3),
             total_growth = round((`City_r` + `County_r`) / (`City_p` + `County_p`) - 1, 3)) %>% 
      select(city, city_growth, county_growth, total_growth)
    data1[is.na(data1)] <- 0
    
    return(data1)
  })
  
  output$growth_current_potential_by_city <- renderDT({
    if (is.null(growth_data()) | is.null(contribution_data())) return(NULL)
    input$goButton
    isolate({
      ordering <- contribution_data()$ordering
      
      table_data <- growth_data() %>% 
        mutate(city_growth = paste0(city_growth*100, "%"),
               county_growth = paste0(county_growth*100, "%"),
               total_growth = paste0(total_growth*100, "%")) %>% 
        melt(id.vars = "city") %>% 
        dcast(variable~city, value.var = "value") %>% 
        mutate(variable = ifelse(variable == "city_growth",
                                 "城市医院",
                                 ifelse(variable == "county_growth",
                                        "县",
                                        ifelse(variable == "total_growth",
                                               "Total",
                                               variable)))) %>% 
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
        mutate(city = factor(city, levels = ordering))
      
      plot_ly(hoverinfo = "x+y") %>% 
        add_bars(x = plot_data$city,
                 y = plot_data$city_growth*100,
                 text = paste0(plot_data$city_growth*100, "%"),
                 textposition = "outside",
                 type = "bar",
                 name = "城市医院",
                 color = I("#9BBB59")) %>% 
        add_bars(x = plot_data$city,
                 y = plot_data$county_growth*100,
                 text = paste0(plot_data$county_growth*100, "%"),
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
  
}
