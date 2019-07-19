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
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
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
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
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
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
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
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
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
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
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
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
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
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
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
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
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
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
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
  
  
  
  
  
  
  
  # tablefor3 <- reactive ({
  #   result1 <- result1()
  #   
  #   if (!"China"  %in% c(input$region, input$province)) {
  #     result1 <- result1[which(result1$AUDIT.DESC != "China"), ]
  #   }
  #   
  #   if (input$TA == "Others") {
  #     result2 <- result1[which(result1$PRODUCT.DESC %in% toplist()),]
  #     result2_c <-
  #       result1[which(result1$CORPORATE.DESC %in% toplist_c()),]
  #     result2_m <-
  #       result1[which(result1$COMPS.DESC %in% toplist_m()),]
  #     
  #     result2$AUDIT.DESC <-
  #       factor(
  #         result2$AUDIT.DESC,
  #         levels = c(
  #           "China",
  #           "BI&Lilly",
  #           "BI",
  #           "Lilly",
  #           "Beijing Hospital Audit",
  #           "East Region Audit",
  #           "East1 Region Audit",
  #           "East2 Region Audit",
  #           "Anhui Provincial Audit",
  #           "Fujian Provincial Audit",
  #           "Jiangsu Provincial Audit",
  #           "Shanghai Hospital Audit",
  #           "Zhejiang Provincial Audit",
  #           "South Region Audit",
  #           "Guangdong Provincial Audit",
  #           "Hainan Provincial Audit",
  #           "Hubei Provincial Audit",
  #           "Hunan Provincial Audit",
  #           "Jiangxi Provincial Audit",
  #           "Guangxi Provincial Audit",
  #           "West Region Audit",
  #           "Ningxia Provincial Audit",
  #           "Sichuan Provincial Audit",
  #           "Xinjiang Provincial Audit",
  #           "Chongqing Hospital Audit",
  #           "Yunnan Provincial Audit",
  #           "Gansu Provincial Audit",
  #           "Guizhou Provincial Audit",
  #           "Qinghai Provincial Audit",
  #           "Shaanxi Provincial Audit",
  #           "North Region Audit",
  #           "Hebei Provincial Audit",
  #           "Henan Provincial Audit",
  #           "Heilongjiang Provincial Audit",
  #           "Jilin Provincial Audit",
  #           "Liaoning Provincial Audit",
  #           "Neimenggu Provincial Audit",
  #           "Shandong Provincial Audit",
  #           "Tianjin Hospital Audit",
  #           "Shanxi Provincial Audit"
  #         )
  #       )
  #     
  #     result2_c$AUDIT.DESC <-
  #       factor(
  #         result2_c$AUDIT.DESC,
  #         levels = c(
  #           "China",
  #           "BI&Lilly",
  #           "BI",
  #           "Lilly",
  #           "Beijing Hospital Audit",
  #           "East Region Audit",
  #           "East1 Region Audit",
  #           "East2 Region Audit",
  #           "Anhui Provincial Audit",
  #           "Fujian Provincial Audit",
  #           "Jiangsu Provincial Audit",
  #           "Shanghai Hospital Audit",
  #           "Zhejiang Provincial Audit",
  #           "South Region Audit",
  #           "Guangdong Provincial Audit",
  #           "Hainan Provincial Audit",
  #           "Hubei Provincial Audit",
  #           "Hunan Provincial Audit",
  #           "Jiangxi Provincial Audit",
  #           "Guangxi Provincial Audit",
  #           "West Region Audit",
  #           "Ningxia Provincial Audit",
  #           "Sichuan Provincial Audit",
  #           "Xinjiang Provincial Audit",
  #           "Chongqing Hospital Audit",
  #           "Yunnan Provincial Audit",
  #           "Gansu Provincial Audit",
  #           "Guizhou Provincial Audit",
  #           "Qinghai Provincial Audit",
  #           "Shaanxi Provincial Audit",
  #           "North Region Audit",
  #           "Hebei Provincial Audit",
  #           "Henan Provincial Audit",
  #           "Heilongjiang Provincial Audit",
  #           "Jilin Provincial Audit",
  #           "Liaoning Provincial Audit",
  #           "Neimenggu Provincial Audit",
  #           "Shandong Provincial Audit",
  #           "Tianjin Hospital Audit",
  #           "Shanxi Provincial Audit"
  #         )
  #       )
  #     
  #     result2_m$AUDIT.DESC <-
  #       factor(
  #         result2_m$AUDIT.DESC,
  #         levels = c(
  #           "China",
  #           "BI&Lilly",
  #           "BI",
  #           "Lilly",
  #           "Beijing Hospital Audit",
  #           "East Region Audit",
  #           "East1 Region Audit",
  #           "East2 Region Audit",
  #           "Anhui Provincial Audit",
  #           "Fujian Provincial Audit",
  #           "Jiangsu Provincial Audit",
  #           "Shanghai Hospital Audit",
  #           "Zhejiang Provincial Audit",
  #           "South Region Audit",
  #           "Guangdong Provincial Audit",
  #           "Hainan Provincial Audit",
  #           "Hubei Provincial Audit",
  #           "Hunan Provincial Audit",
  #           "Jiangxi Provincial Audit",
  #           "Guangxi Provincial Audit",
  #           "West Region Audit",
  #           "Ningxia Provincial Audit",
  #           "Sichuan Provincial Audit",
  #           "Xinjiang Provincial Audit",
  #           "Chongqing Hospital Audit",
  #           "Yunnan Provincial Audit",
  #           "Gansu Provincial Audit",
  #           "Guizhou Provincial Audit",
  #           "Qinghai Provincial Audit",
  #           "Shaanxi Provincial Audit",
  #           "North Region Audit",
  #           "Hebei Provincial Audit",
  #           "Henan Provincial Audit",
  #           "Heilongjiang Provincial Audit",
  #           "Jilin Provincial Audit",
  #           "Liaoning Provincial Audit",
  #           "Neimenggu Provincial Audit",
  #           "Shandong Provincial Audit",
  #           "Tianjin Hospital Audit",
  #           "Shanxi Provincial Audit"
  #         )
  #       )
  #     
  #     
  #     
  #     result2$PRODUCT.DESC <- factor(result2$PRODUCT.DESC ,
  #                                    levels =  c("Overall",
  #                                                setdiff(unique(toplist(
  #                                                )),
  #                                                "Overall")))
  #     result2_c$CORPORATE.DESC <- factor(result2_c$CORPORATE.DESC ,
  #                                        levels = c("Overall",
  #                                                   setdiff(unique(toplist_c(
  #                                                   )),
  #                                                   "Overall")))
  #     result2_c$PRODUCT.DESC <- factor(result2_c$PRODUCT.DESC ,
  #                                      levels = c("Overall",
  #                                                 setdiff(
  #                                                   unique(result2_c$PRODUCT.DESC),
  #                                                   "Overall"
  #                                                 )))
  #     result2_m$COMPS.DESC <- factor(result2_m$COMPS.DESC ,
  #                                    levels = c("Overall",
  #                                               setdiff(unique(toplist_m(
  #                                               )),
  #                                               "Overall")))
  #     result2_m$PRODUCT.DESC <- factor(result2_m$PRODUCT.DESC ,
  #                                      levels = c("Overall",
  #                                                 setdiff(
  #                                                   unique(result2_m$PRODUCT.DESC),
  #                                                   "Overall"
  #                                                 )))
  #     
  #     result3 <-
  #       result2[order(result2$AUDIT.DESC, result2$PRODUCT.DESC),]
  #     result3_c <- result2_c[order(result2_c$AUDIT.DESC,
  #                                  result2_c$CORPORATE.DESC,
  #                                  result2_c$PRODUCT.DESC),]
  #     result3_m <-
  #       result2_m[order(result2_m$AUDIT.DESC,
  #                       result2_m$COMPS.DESC,
  #                       result2_m$PRODUCT.DESC),]
  #     
  #     
  #     list(result3 = result3,
  #          result3_c = result3_c,
  #          result3_m = result3_m)
  #     
  #   } else if (input$TA == "Onco") {
  #     result2 <- result1[which(result1$PRODUCT.DESC %in% toplist()),]
  #     result2_c <-
  #       result1[which(result1$CORPORATE.DESC %in% toplist_c()),]
  #     result2_m <-
  #       result1[which(result1$COMPS.DESC %in% toplist_m()),]
  #     
  #     result2$AUDIT.DESC <-
  #       factor(
  #         result2$AUDIT.DESC,
  #         levels = c(
  #           "China",
  #           "BI&Lilly",
  #           "BI",
  #           "Lilly",
  #           "East Region Audit",
  #           "East1 Region Audit",
  #           "Shanghai Hospital Audit",
  #           "Shandong Provincial Audit",
  #           "Henan Provincial Audit",
  #           "Jiangsu Provincial Audit",
  #           "Anhui Provincial Audit",
  #           "East2 Region Audit",
  #           "Fujian Provincial Audit",
  #           "Jiangxi Provincial Audit",
  #           "Zhejiang Provincial Audit",
  #           "South Region Audit",
  #           "Guangdong Provincial Audit",
  #           "Hainan Provincial Audit",
  #           "Hubei Provincial Audit",
  #           "Hunan Provincial Audit",
  #           "Guangxi Provincial Audit",
  #           "West Region Audit",
  #           "Ningxia Provincial Audit",
  #           "Sichuan Provincial Audit",
  #           "Xinjiang Provincial Audit",
  #           "Chongqing Hospital Audit",
  #           "Yunnan Provincial Audit",
  #           "Gansu Provincial Audit",
  #           "Guizhou Provincial Audit",
  #           "Qinghai Provincial Audit",
  #           "Shaanxi Provincial Audit",
  #           "North Region Audit",
  #           "Beijing Hospital Audit",
  #           "Hebei Provincial Audit",
  #           "Heilongjiang Provincial Audit",
  #           "Jilin Provincial Audit",
  #           "Liaoning Provincial Audit",
  #           "Neimenggu Provincial Audit",
  #           "Tianjin Hospital Audit",
  #           "Shanxi Provincial Audit"
  #         )
  #       )
  #     result2_c$AUDIT.DESC <-
  #       factor(
  #         result2_c$AUDIT.DESC,
  #         levels = c(
  #           "China",
  #           "BI&Lilly",
  #           "BI",
  #           "Lilly",
  #           "East Region Audit",
  #           "East1 Region Audit",
  #           "Shanghai Hospital Audit",
  #           "Shandong Provincial Audit",
  #           "Henan Provincial Audit",
  #           "Jiangsu Provincial Audit",
  #           "Anhui Provincial Audit",
  #           "East2 Region Audit",
  #           "Fujian Provincial Audit",
  #           "Jiangxi Provincial Audit",
  #           "Zhejiang Provincial Audit",
  #           "South Region Audit",
  #           "Guangdong Provincial Audit",
  #           "Hainan Provincial Audit",
  #           "Hubei Provincial Audit",
  #           "Hunan Provincial Audit",
  #           "Guangxi Provincial Audit",
  #           "West Region Audit",
  #           "Ningxia Provincial Audit",
  #           "Sichuan Provincial Audit",
  #           "Xinjiang Provincial Audit",
  #           "Chongqing Hospital Audit",
  #           "Yunnan Provincial Audit",
  #           "Gansu Provincial Audit",
  #           "Guizhou Provincial Audit",
  #           "Qinghai Provincial Audit",
  #           "Shaanxi Provincial Audit",
  #           "North Region Audit",
  #           "Beijing Hospital Audit",
  #           "Hebei Provincial Audit",
  #           "Heilongjiang Provincial Audit",
  #           "Jilin Provincial Audit",
  #           "Liaoning Provincial Audit",
  #           "Neimenggu Provincial Audit",
  #           "Tianjin Hospital Audit",
  #           "Shanxi Provincial Audit"
  #         )
  #       )
  #     result2_m$AUDIT.DESC <-
  #       factor(
  #         result2_m$AUDIT.DESC,
  #         levels = c(
  #           "China",
  #           "BI&Lilly",
  #           "BI",
  #           "Lilly",
  #           "East Region Audit",
  #           "East1 Region Audit",
  #           "Shanghai Hospital Audit",
  #           "Shandong Provincial Audit",
  #           "Henan Provincial Audit",
  #           "Jiangsu Provincial Audit",
  #           "Anhui Provincial Audit",
  #           "East2 Region Audit",
  #           "Fujian Provincial Audit",
  #           "Jiangxi Provincial Audit",
  #           "Zhejiang Provincial Audit",
  #           "South Region Audit",
  #           "Guangdong Provincial Audit",
  #           "Hainan Provincial Audit",
  #           "Hubei Provincial Audit",
  #           "Hunan Provincial Audit",
  #           "Guangxi Provincial Audit",
  #           "West Region Audit",
  #           "Ningxia Provincial Audit",
  #           "Sichuan Provincial Audit",
  #           "Xinjiang Provincial Audit",
  #           "Chongqing Hospital Audit",
  #           "Yunnan Provincial Audit",
  #           "Gansu Provincial Audit",
  #           "Guizhou Provincial Audit",
  #           "Qinghai Provincial Audit",
  #           "Shaanxi Provincial Audit",
  #           "North Region Audit",
  #           "Beijing Hospital Audit",
  #           "Hebei Provincial Audit",
  #           "Heilongjiang Provincial Audit",
  #           "Jilin Provincial Audit",
  #           "Liaoning Provincial Audit",
  #           "Neimenggu Provincial Audit",
  #           "Tianjin Hospital Audit",
  #           "Shanxi Provincial Audit"
  #         )
  #       )
  #     
  #     result2$PRODUCT.DESC <- factor(result2$PRODUCT.DESC ,
  #                                    levels =  c("Overall",
  #                                                setdiff(unique(toplist(
  #                                                )),
  #                                                "Overall")))
  #     result2_c$CORPORATE.DESC <- factor(result2_c$CORPORATE.DESC ,
  #                                        levels = c("Overall",
  #                                                   setdiff(unique(toplist_c(
  #                                                   )),
  #                                                   "Overall")))
  #     result2_c$PRODUCT.DESC <- factor(result2_c$PRODUCT.DESC ,
  #                                      levels = c("Overall",
  #                                                 setdiff(
  #                                                   unique(result2_c$PRODUCT.DESC),
  #                                                   "Overall"
  #                                                 )))
  #     result2_m$COMPS.DESC <- factor(result2_m$COMPS.DESC ,
  #                                    levels = c("Overall",
  #                                               setdiff(unique(toplist_m(
  #                                               )),
  #                                               "Overall")))
  #     result2_m$PRODUCT.DESC <- factor(result2_m$PRODUCT.DESC ,
  #                                      levels = c("Overall",
  #                                                 setdiff(
  #                                                   unique(result2_m$PRODUCT.DESC),
  #                                                   "Overall"
  #                                                 )))
  #     
  #     result3 <-
  #       result2[order(result2$AUDIT.DESC, result2$PRODUCT.DESC),]
  #     result3_c <-
  #       result2_c[order(result2_c$AUDIT.DESC,
  #                       result2_c$CORPORATE.DESC,
  #                       result2_c$PRODUCT.DESC),]
  #     result3_m <-
  #       result2_m[order(result2_m$AUDIT.DESC,
  #                       result2_m$COMPS.DESC,
  #                       result2_m$PRODUCT.DESC),]
  #     
  #     list(result3 = result3,
  #          result3_c = result3_c,
  #          result3_m = result3_m)
  #     
  #   } else{
  #     result2 <- result1[which(result1$PRODUCT.DESC %in% toplist()),]
  #     result2_c <-
  #       result1[which(result1$CORPORATE.DESC %in% toplist_c()),]
  #     result2_m <-
  #       result1[which(result1$COMPS.DESC %in% toplist_m()),]
  #     
  #     result2$AUDIT.DESC <-
  #       factor(
  #         result2$AUDIT.DESC,
  #         levels = c(
  #           "China",
  #           "East Region Audit",
  #           "East1 Region Audit",
  #           "East2 Region Audit",
  #           "South Region Audit",
  #           "West Region Audit",
  #           "North Region Audit",
  #           "BI&Lilly",
  #           "Shanghai Hospital Audit",
  #           "Beijing Hospital Audit"  ,
  #           "BI",
  #           "Anhui Provincial Audit",
  #           "Hebei Provincial Audit",
  #           "Hubei Provincial Audit",
  #           "Hunan Provincial Audit",
  #           "Guangdong Provincial Audit",
  #           "Hainan Provincial Audit",
  #           "Ningxia Provincial Audit",
  #           "Tianjin Hospital Audit",
  #           "Neimenggu Provincial Audit",
  #           "Jiangsu Provincial Audit",
  #           "Guangxi Provincial Audit",
  #           "Shanxi Provincial Audit",
  #           "Qinghai Provincial Audit",
  #           "Lilly",
  #           "Shandong Provincial Audit",
  #           "Henan Provincial Audit",
  #           "Zhejiang Provincial Audit",
  #           "Fujian Provincial Audit",
  #           "Jiangxi Provincial Audit",
  #           "Liaoning Provincial Audit",
  #           "Heilongjiang Provincial Audit",
  #           "Jilin Provincial Audit",
  #           "Sichuan Provincial Audit",
  #           "Xinjiang Provincial Audit",
  #           "Chongqing Hospital Audit",
  #           "Yunnan Provincial Audit",
  #           "Shaanxi Provincial Audit",
  #           "Xizang Provincial Audit",
  #           "Gansu Provincial Audit",
  #           "Guizhou Provincial Audit"
  #         )
  #       )
  #     result2_c$AUDIT.DESC <-
  #       factor(
  #         result2_c$AUDIT.DESC,
  #         levels = c(
  #           "China",
  #           "East Region Audit",
  #           "East1 Region Audit",
  #           "East2 Region Audit",
  #           "South Region Audit",
  #           "West Region Audit",
  #           "North Region Audit",
  #           "BI&Lilly",
  #           "Shanghai Hospital Audit",
  #           "Beijing Hospital Audit"  ,
  #           "BI",
  #           "Anhui Provincial Audit",
  #           "Hebei Provincial Audit",
  #           "Hubei Provincial Audit",
  #           "Hunan Provincial Audit",
  #           "Guangdong Provincial Audit",
  #           "Hainan Provincial Audit",
  #           "Ningxia Provincial Audit",
  #           "Tianjin Hospital Audit",
  #           "Neimenggu Provincial Audit",
  #           "Jiangsu Provincial Audit",
  #           "Guangxi Provincial Audit",
  #           "Shanxi Provincial Audit",
  #           "Qinghai Provincial Audit",
  #           "Lilly",
  #           "Shandong Provincial Audit",
  #           "Henan Provincial Audit",
  #           "Zhejiang Provincial Audit",
  #           "Fujian Provincial Audit",
  #           "Jiangxi Provincial Audit",
  #           "Liaoning Provincial Audit",
  #           "Heilongjiang Provincial Audit",
  #           "Jilin Provincial Audit",
  #           "Sichuan Provincial Audit",
  #           "Xinjiang Provincial Audit",
  #           "Chongqing Hospital Audit",
  #           "Yunnan Provincial Audit",
  #           "Shaanxi Provincial Audit",
  #           "Xizang Provincial Audit",
  #           "Gansu Provincial Audit",
  #           "Guizhou Provincial Audit"
  #         )
  #       )
  #     result2_m$AUDIT.DESC <-
  #       factor(
  #         result2_m$AUDIT.DESC,
  #         levels = c(
  #           "China",
  #           "East Region Audit",
  #           "East1 Region Audit",
  #           "East2 Region Audit",
  #           "South Region Audit",
  #           "West Region Audit",
  #           "North Region Audit",
  #           "BI&Lilly",
  #           "Shanghai Hospital Audit",
  #           "Beijing Hospital Audit"  ,
  #           "BI",
  #           "Anhui Provincial Audit",
  #           "Hebei Provincial Audit",
  #           "Hubei Provincial Audit",
  #           "Hunan Provincial Audit",
  #           "Guangdong Provincial Audit",
  #           "Hainan Provincial Audit",
  #           "Ningxia Provincial Audit",
  #           "Tianjin Hospital Audit",
  #           "Neimenggu Provincial Audit",
  #           "Jiangsu Provincial Audit",
  #           "Guangxi Provincial Audit",
  #           "Shanxi Provincial Audit",
  #           "Qinghai Provincial Audit",
  #           "Lilly",
  #           "Shandong Provincial Audit",
  #           "Henan Provincial Audit",
  #           "Zhejiang Provincial Audit",
  #           "Fujian Provincial Audit",
  #           "Jiangxi Provincial Audit",
  #           "Liaoning Provincial Audit",
  #           "Heilongjiang Provincial Audit",
  #           "Jilin Provincial Audit",
  #           "Sichuan Provincial Audit",
  #           "Xinjiang Provincial Audit",
  #           "Chongqing Hospital Audit",
  #           "Yunnan Provincial Audit",
  #           "Shaanxi Provincial Audit",
  #           "Xizang Provincial Audit",
  #           "Gansu Provincial Audit",
  #           "Guizhou Provincial Audit"
  #         )
  #       )
  #     
  #     result2$PRODUCT.DESC <- factor(result2$PRODUCT.DESC ,
  #                                    levels =  c("Overall",
  #                                                setdiff(unique(toplist(
  #                                                )),
  #                                                "Overall")))
  #     result2_c$CORPORATE.DESC <- factor(result2_c$CORPORATE.DESC ,
  #                                        levels = c("Overall",
  #                                                   setdiff(unique(toplist_c(
  #                                                   )),
  #                                                   "Overall")))
  #     result2_c$PRODUCT.DESC <- factor(result2_c$PRODUCT.DESC ,
  #                                      levels = c("Overall",
  #                                                 setdiff(
  #                                                   unique(result2_c$PRODUCT.DESC),
  #                                                   "Overall"
  #                                                 )))
  #     result2_m$COMPS.DESC <- factor(result2_m$COMPS.DESC ,
  #                                    levels = c("Overall",
  #                                               setdiff(unique(toplist_m(
  #                                               )),
  #                                               "Overall")))
  #     result2_m$PRODUCT.DESC <- factor(result2_m$PRODUCT.DESC ,
  #                                      levels = c("Overall",
  #                                                 setdiff(
  #                                                   unique(result2_m$PRODUCT.DESC),
  #                                                   "Overall"
  #                                                 )))
  #     
  #     result3 <-
  #       result2[order(result2$AUDIT.DESC, result2$PRODUCT.DESC),]
  #     result3_c <-
  #       result2_c[order(result2_c$AUDIT.DESC,
  #                       result2_c$CORPORATE.DESC,
  #                       result2_c$PRODUCT.DESC),]
  #     result3_m <-
  #       result2_m[order(result2_m$AUDIT.DESC,
  #                       result2_m$COMPS.DESC,
  #                       result2_m$PRODUCT.DESC),]
  #     
  #     
  #     list(result3 = result3,
  #          result3_c = result3_c,
  #          result3_m = result3_m)
  #     
  #   }
  #   
  # })
  # ot <- reactive({
  #   if (input$goButton == 0)
  #     return(NULL)
  #   input$goButton
  #   isolate({
  #     outputtable <- tablefor3()
  #     outputtable
  #   })
  # })
  # 
  # pagenumber <- reactive({
  #   if (input$top == "3") {
  #     pageno = 12
  #   } else if (input$top == "5") {
  #     pageno = 12
  #   } else if (input$top == "10") {
  #     pageno = 11
  #   } else{
  #     pageno = 15
  #   }
  #   pageno
  # })
  # 
  # ##-- summary kpi product
  # output$summary_table <-
  #   DT::renderDataTable({
  #     # input$goButton
  #     if (input$goButton == 0) {
  #       return(NULL)
  #     }
  #     isolate({
  #       dat <-
  #         DT::datatable(
  #           result2()$summary_table ,
  #           rownames = FALSE,
  #           # extensions = c('FixedColumns', 'Buttons'),
  #           #filter = 'bottom',
  #           ##### this sentence need to be changed when new variables added
  #           options = list(
  #             # dom = '<"bottom">Bfrtpl',
  #             # buttons = I('colvis'),
  #             columnDefs = list(list(
  #               className = 'dt-center', targets = c(0, 1, 2, 3, 4, 5)
  #             )),
  #             initComplete = JS(
  #               "function(settings, json) {",
  #               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  #               "}"
  #             ),
  #             paging = FALSE,
  #             scrollX = FALSE,
  #             searching = FALSE,
  #             ordering = FALSE,
  #             pageLength = 5,
  #             lengthChange = FALSE,
  #             bInfo = FALSE
  #             
  #           )) %>%
  #         formatPercentage(c("MKT GR vs. LY", "BI GR vs. LY", "MS", "MS+/- vs. LY"), 2) %>%
  #         formatStyle("Period",
  #                     color = 'white',
  #                     backgroundColor = "rgb(153,204,51)",
  #                     fontWeight = 'bold')
  #       
  #       return(dat)
  #     })
  #     
  #   })
  # 
  # output$summary_table1 <-
  #   DT::renderDataTable({
  #     # input$goButton
  #     if (input$goButton == 0) {
  #       return(NULL)
  #     }
  #     
  #     isolate({
  #       dat <-
  #         DT::datatable(
  #           result2()$summary_table1 ,
  #           rownames = FALSE,
  #           # extensions = c('FixedColumns', 'Buttons'),
  #           #filter = 'bottom',
  #           
  #           ##### this sentence need to be changed when new variables added
  #           options = list(
  #             # columnDefs = list(list(
  #             #   visible = TRUE,
  #             #   targets = c(0, 1)
  #             # )),
  #             # columnDefs = list(list(
  #             #   width = '20px', targets = "_all"
  #             # )),
  #             
  #             # dom = 'Bfrtpl',
  #             # dom = '<"bottom">Bfrtpl',
  #             # buttons = I('colvis'),
  #             columnDefs = list(list(
  #               className = 'dt-center', targets = c(0, 1)
  #             )),
  #             initComplete = JS(
  #               "function(settings, json) {",
  #               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  #               "}"
  #             )
  #             ,
  #             paging = FALSE,
  #             scrollX = FALSE,
  #             searching = FALSE,
  #             ordering = FALSE,
  #             pageLength = 5,
  #             lengthChange = FALSE,
  #             bInfo = FALSE
  #           )) %>%
  #         formatStyle("Metrics",
  #                     color = 'white',
  #                     backgroundColor = 'orange',
  #                     fontWeight = 'bold')
  #       
  #       return(dat)
  #     })
  #     
  #   })
  # 
  # ##-- summary kpi corporation
  # output$summary_table_c <-
  #   DT::renderDataTable({
  #     # input$goButton
  #     if (input$goButton == 0) {
  #       return(NULL)
  #     }
  #     
  #     isolate({
  #       dat <-
  #         DT::datatable(
  #           result2()$summary_table ,
  #           rownames = FALSE,
  #           # extensions = c('FixedColumns', 'Buttons'),
  #           #filter = 'bottom',
  #           ##### this sentence need to be changed when new variables added
  #           options = list(
  #             # dom = '<"bottom">Bfrtpl',
  #             # buttons = I('colvis'),
  #             columnDefs = list(list(
  #               className = 'dt-center', targets = c(0, 1, 2, 3, 4, 5)
  #             )),
  #             initComplete = JS(
  #               "function(settings, json) {",
  #               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  #               "}"
  #             ),
  #             paging = FALSE,
  #             scrollX = FALSE,
  #             searching = FALSE,
  #             ordering = FALSE,
  #             pageLength = 5,
  #             lengthChange = FALSE,
  #             bInfo = FALSE
  #             
  #           )) %>%
  #         formatPercentage(c("MKT GR vs. LY", "BI GR vs. LY", "MS", "MS+/- vs. LY"), 2) %>%
  #         formatStyle("Period",
  #                     color = 'white',
  #                     backgroundColor = "rgb(153,204,51)",
  #                     fontWeight = 'bold')
  #       
  #       return(dat)
  #     })
  #     
  #   })
  # 
  # output$summary_table1_c <-
  #   DT::renderDataTable({
  #     # input$goButton
  #     if (input$goButton == 0) {
  #       return(NULL)
  #     }
  #     
  #     isolate({
  #       dat <-
  #         DT::datatable(
  #           result2()$summary_table1 ,
  #           rownames = FALSE,
  #           # extensions = c('FixedColumns', 'Buttons'),
  #           #filter = 'bottom',
  #           
  #           ##### this sentence need to be changed when new variables added
  #           options = list(
  #             # columnDefs = list(list(
  #             #   visible = TRUE,
  #             #   targets = c(0, 1)
  #             # )),
  #             # columnDefs = list(list(
  #             #   width = '20px', targets = "_all"
  #             # )),
  #             
  #             # dom = 'Bfrtpl',
  #             # dom = '<"bottom">Bfrtpl',
  #             # buttons = I('colvis'),
  #             columnDefs = list(list(
  #               className = 'dt-center', targets = c(0, 1)
  #             )),
  #             initComplete = JS(
  #               "function(settings, json) {",
  #               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  #               "}"
  #             )
  #             ,
  #             paging = FALSE,
  #             scrollX = FALSE,
  #             searching = FALSE,
  #             ordering = FALSE,
  #             pageLength = 5,
  #             lengthChange = FALSE,
  #             bInfo = FALSE
  #           )) %>%
  #         formatStyle("Metrics",
  #                     color = 'white',
  #                     backgroundColor = 'orange',
  #                     fontWeight = 'bold')
  #       
  #       return(dat)
  #     })
  #     
  #   })
  # 
  # ##-- summary kpi molecule
  # output$summary_table_m <-
  #   DT::renderDataTable({
  #     # input$goButton
  #     if (input$goButton == 0) {
  #       return(NULL)
  #     }
  #     
  #     isolate({
  #       dat <-
  #         DT::datatable(
  #           result2()$summary_table ,
  #           rownames = FALSE,
  #           # extensions = c('FixedColumns', 'Buttons'),
  #           #filter = 'bottom',
  #           ##### this sentence need to be changed when new variables added
  #           options = list(
  #             # dom = '<"bottom">Bfrtpl',
  #             # buttons = I('colvis'),
  #             columnDefs = list(list(
  #               className = 'dt-center', targets = c(0, 1, 2, 3, 4, 5)
  #             )),
  #             initComplete = JS(
  #               "function(settings, json) {",
  #               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  #               "}"
  #             ),
  #             paging = FALSE,
  #             scrollX = FALSE,
  #             searching = FALSE,
  #             ordering = FALSE,
  #             pageLength = 5,
  #             lengthChange = FALSE,
  #             bInfo = FALSE
  #             
  #           )) %>%
  #         formatPercentage(c("MKT GR vs. LY", "BI GR vs. LY", "MS", "MS+/- vs. LY"), 2) %>%
  #         formatStyle("Period",
  #                     color = 'white',
  #                     backgroundColor = "rgb(153,204,51)",
  #                     fontWeight = 'bold')
  #       
  #       return(dat)
  #     })
  #     
  #   })
  # 
  # output$summary_table1_m <-
  #   DT::renderDataTable({
  #     # input$goButton
  #     if (input$goButton == 0) {
  #       return(NULL)
  #     }
  #     
  #     isolate({
  #       dat <-
  #         DT::datatable(
  #           result2()$summary_table1 ,
  #           rownames = FALSE,
  #           # extensions = c('FixedColumns', 'Buttons'),
  #           #filter = 'bottom',
  #           
  #           ##### this sentence need to be changed when new variables added
  #           options = list(
  #             # columnDefs = list(list(
  #             #   visible = TRUE,
  #             #   targets = c(0, 1)
  #             # )),
  #             # columnDefs = list(list(
  #             #   width = '20px', targets = "_all"
  #             # )),
  #             
  #             # dom = 'Bfrtpl',
  #             # dom = '<"bottom">Bfrtpl',
  #             # buttons = I('colvis'),
  #             columnDefs = list(list(
  #               className = 'dt-center', targets = c(0, 1)
  #             )),
  #             initComplete = JS(
  #               "function(settings, json) {",
  #               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  #               "}"
  #             )
  #             ,
  #             paging = FALSE,
  #             scrollX = FALSE,
  #             searching = FALSE,
  #             ordering = FALSE,
  #             pageLength = 5,
  #             lengthChange = FALSE,
  #             bInfo = FALSE
  #           )) %>%
  #         formatStyle("Metrics",
  #                     color = 'white',
  #                     backgroundColor = 'orange',
  #                     fontWeight = 'bold')
  #       
  #       return(dat)
  #     })
  #     
  #   })
  # 
  # output$contents <- renderDataTable({
  #   input$goButton
  #   isolate({
  #     if (is.null(ot()))
  #       return(NULL)
  #     
  #     ot <- ot()$result3
  #     ot <-
  #       filter(
  #         ot,
  #         (CORPORATE.DESC != "Overall" & COMPS.DESC != "Overall") |
  #           (CORPORATE.DESC == "Overall" &
  #              COMPS.DESC == "Overall")
  #       )
  #     
  #     
  #     ot$AUDIT.DESC <- gsub(" .*$", "", ot$AUDIT.DESC)
  #     
  #     ot$PRODUCT.DESC <- as.character(ot$PRODUCT.DESC)
  #     ot$CORPORATE.DESC <- as.character(ot$CORPORATE.DESC)
  #     
  #     ot$PRODUCT.DESC[which(ot$CORPORATE.DESC == "Overall")] <-
  #       ot$AUDIT.DESC[which(ot$CORPORATE.DESC == "Overall")]
  #     KK <- which(ot$PRODUCT.DESC == "Overall")
  #     
  #     ot$PRODUCT.DESC[KK] <- ot$CORPORATE.DESC[KK]
  #     ot$CORPORATE.DESC[KK] <- "Company"
  #     
  #     names(ot)[names(ot) == 'AUDIT.DESC'] <- 'AUDIT'
  #     names(ot)[names(ot) == 'CORPORATE.DESC'] <- 'CORPORATE'
  #     names(ot)[names(ot) == 'PRODUCT.DESC'] <- 'PRODUCT'
  #     
  #     ot$PRODUCT <- as.factor(ot$PRODUCT)
  #     ot$CORPORATE <- as.factor(ot$CORPORATE)
  #     ot$AUDIT <- as.factor(ot$AUDIT)
  #     
  #     ot <- as.data.frame(ot)
  #     otnum <- which(grepl("Index", names(ot)))
  #     #number here need to be updated for new update
  #     # ot[which(ot$Index=="ABS"),(otnum+1):length(ot)]<-
  #     #   format(ot[which(ot$Index=="ABS"), (otnum+1):length(ot)], nsmall = 2, big.mark = ",", scientific = FALSE)
  #     ot[, (otnum + 1):length(ot)] <-
  #       format(
  #         ot[, (otnum + 1):length(ot)],
  #         nsmall = 2,
  #         big.mark = ",",
  #         scientific = FALSE
  #       )
  #     
  #     
  #     ot$Measure[which(ot$Measure == "RENMINBI")] <- "RMB"
  #     
  #     if (input$period == "mat" |
  #         input$period == "ytd" |
  #         input$period == "yrl" |
  #         input$period == "qtr" &
  #         as.numeric(input$window) <= 3 |
  #         (input$period == "rqtr" &
  #          input$window == "1") | (input$period == "mth" & input$window == "1")) {
  #       dat <-
  #         DT::datatable(
  #           ot ,
  #           rownames = FALSE,
  #           extensions = c('FixedColumns', 'Buttons'),
  #           #filter = 'bottom',
  #           
  #           ##### this sentence need to be changed when new variables added
  #           options = list(
  #             columnDefs = list(list(
  #               visible = FALSE,
  #               targets = c(0, 1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13)
  #             )),
  #             columnDefs = list(list(
  #               width = '20px', targets = "_all"
  #             )),
  #             
  #             # dom = 'Bfrtpl',
  #             dom = '<"bottom">Bfrtpl',
  #             buttons = I('colvis'),
  #             initComplete = JS(
  #               "function(settings, json) {",
  #               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  #               "}"
  #             ),
  #             
  #             paging = TRUE,
  #             scrollX = TRUE,
  #             
  #             lengthMenu = c(
  #               pagenumber(),
  #               2 * pagenumber(),
  #               3 * pagenumber(),
  #               4 * pagenumber(),
  #               5 * pagenumber()
  #             ),
  #             pageLength = pagenumber(),
  #             fixedColumns = list(leftColumns = otnum, rightColumns = 0)
  #           )
  #         ) %>%
  #         formatStyle(
  #           "CORPORATE",
  #           target = "row",
  #           fontWeight = styleEqual(c("Company", "Overall"), c('bold', 'bold')),
  #           backgroundColor = styleEqual(c("Company", "Overall"), c('deepskyblue', "grey")),
  #           color = styleEqual(c("Company", "Overall"), c('white', 'white'))
  #         )
  #     } else{
  #       dat <- DT::datatable(
  #         ot,
  #         rownames = FALSE,
  #         #filter = 'bottom',
  #         extensions = c('FixedColumns', 'Buttons'),
  #         
  #         options = list(
  #           columnDefs = list(list(
  #             visible = FALSE,
  #             targets = c(0, 1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13)
  #           )),
  #           columnDefs = list(list(
  #             width = '20px', targets = "_all"
  #           )),
  #           autoWidth = TRUE,
  #           dom = '<"bottom">Bfrtpl',
  #           buttons = I('colvis'),
  #           initComplete = JS(
  #             "function(settings, json) {",
  #             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  #             "}"
  #           ),
  #           
  #           scrollX = TRUE,
  #           paging = TRUE,
  #           lengthMenu = c(
  #             pagenumber(),
  #             2 * pagenumber(),
  #             3 * pagenumber(),
  #             4 * pagenumber(),
  #             5 * pagenumber()
  #           ),
  #           pageLength = pagenumber(),
  #           
  #           fixedColumns = list(leftColumns = otnum, rightColumns = 0)
  #         )
  #       ) %>%
  #         formatStyle(
  #           "CORPORATE",
  #           target = "row",
  #           fontWeight = styleEqual(c("Company", "Overall"), c('bold', 'bold')),
  #           backgroundColor = styleEqual(c("Company", "Overall"), c('deepskyblue', "grey")),
  #           color = styleEqual(c("Company", "Overall"), c('white', 'white'))
  #         )
  #       #  formatCurrency("CORPORATE",target = "row", currency = "", interval = 3, mark = ",")
  #       #    formatCurrency( "Index",target = "row",  currency = styleEqual(c("ABS"), c('')),
  #       #                 interval = styleEqual(c("ABS"), c(3)),  mark = styleEqual(c("ABS"), c( ",")))
  #       
  #     }
  #     
  #     return(dat)
  #     
  #   })
  #   
  #   
  # })
  # 
  # output$contents_c <- renderDataTable({
  #   input$goButton
  #   
  #   isolate({
  #     if (is.null(ot()))
  #       return(NULL)
  #     
  #     ot <- ot()$result3_c
  #     ot <- filter(
  #       ot,
  #       (CORPORATE.DESC != "Overall") |
  #         (CORPORATE.DESC == "Overall" &
  #            COMPS.DESC == "Overall")
  #     )
  #     
  #     if ("ALL" %in% input$province) {
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region,  province())),]
  #     } else {
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, input$province)),]
  #     }
  #     
  #     top_brand <- graph1m(
  #       summary,
  #       cate = input$category,
  #       subcate = input$subcategory,
  #       value = "RENMINBI",
  #       period = input$period,
  #       kpi = "abs",
  #       window = 1,
  #       level = "corporation",
  #       top = as.integer(input$top2)
  #     ) %>%
  #       select(AUDIT.DESC, PRODUCT.DESC, CORPORATE.DESC)
  #     
  #     levels_contains <- list(AUDIT.DESC = levels(ot$AUDIT.DESC),
  #                             PRODUCT.DESC = levels(ot$PRODUCT.DESC),
  #                             CORPORATE.DESC = levels(ot$CORPORATE.DESC))
  #     
  #     ot <- ot %>% inner_join(top_brand)
  #     
  #     summary1 <- summary()[which(summary()$AUDIT.DESC == "China"),]
  #     ggg <-  rbind.fill(
  #       graph1(
  #         summary1,
  #         cate = input$category,
  #         subcate = input$subcategory,
  #         value = "RENMINBI",
  #         period = input$period,
  #         kpi = "abs",
  #         window = 1,
  #         level = "corporation"
  #       )
  #     )
  #     ggg <- distinct(ggg) %>%
  #       filter((CORPORATE.DESC != "Overall" &
  #                 COMPS.DESC != "Overall") |
  #                (CORPORATE.DESC == "Overall" &
  #                   COMPS.DESC == "Overall")
  #       ) 
  #     tmp <- ggg[order(-ggg[, length(ggg)]),]$PRODUCT.DESC
  #     
  #     
  #     
  #     ot$AUDIT.DESC <- factor(ot$AUDIT.DESC, 
  #                             levels = levels_contains$AUDIT.DESC)
  #     ot$CORPORATE.DESC <- factor(ot$CORPORATE.DESC, 
  #                                 levels = levels_contains$CORPORATE.DESC)
  #     ot$PRODUCT.DESC <- factor(ot$PRODUCT.DESC, levels = tmp)
  #     
  #     ot <-
  #       ot[order(ot$AUDIT.DESC,
  #                ot$CORPORATE.DESC,
  #                ot$PRODUCT.DESC),]
  #     
  #     ot$AUDIT.DESC <- gsub(" .*$", "", ot$AUDIT.DESC)
  #     
  #     ot$PRODUCT.DESC <- as.character(ot$PRODUCT.DESC)
  #     ot$CORPORATE.DESC <- as.character(ot$CORPORATE.DESC)
  #     
  #     ot$PRODUCT.DESC[which(ot$CORPORATE.DESC == "Overall")] <-
  #       ot$AUDIT.DESC[which(ot$CORPORATE.DESC == "Overall")]
  #     KK <- which(ot$PRODUCT.DESC == "Overall")
  #     
  #     ot$PRODUCT.DESC[KK] <- ot$CORPORATE.DESC[KK]
  #     ot$CORPORATE.DESC[KK] <- "Company"
  #     
  #     names(ot)[names(ot) == 'AUDIT.DESC'] <- 'AUDIT'
  #     names(ot)[names(ot) == 'CORPORATE.DESC'] <- 'CORPORATE'
  #     names(ot)[names(ot) == 'PRODUCT.DESC'] <- 'PRODUCT'
  #     
  #     ot$PRODUCT <- as.factor(ot$PRODUCT)
  #     ot$CORPORATE <- as.factor(ot$CORPORATE)
  #     ot$AUDIT <- as.factor(ot$AUDIT)
  #     
  #     ot <- as.data.frame(ot)
  #     otnum <- which(grepl("Index", names(ot)))
  #     #number here need to be updated for new update
  #     # ot[which(ot$Index=="ABS"),(otnum+1):length(ot)]<-
  #     #   format(ot[which(ot$Index=="ABS"), (otnum+1):length(ot)], nsmall = 2,  big.mark = ",", scientific = FALSE)
  #     ot[, (otnum + 1):length(ot)] <-
  #       format(
  #         ot[, (otnum + 1):length(ot)],
  #         nsmall = 2,
  #         big.mark = ",",
  #         scientific = FALSE
  #       )
  #     ot$Measure[which(ot$Measure == "RENMINBI")] <- "RMB"
  #     if (input$period == "mat" |
  #         input$period == "ytd" |
  #         input$period == "yrl" |
  #         input$period == "qtr" &
  #         as.numeric(input$window) <= 3 |
  #         (input$period == "rqtr" &
  #          input$window == "1") | (input$period == "mth" & input$window == "1")) {
  #       dat <-
  #         DT::datatable(
  #           ot ,
  #           rownames = FALSE,
  #           extensions = c('FixedColumns', 'Buttons'),
  #           #filter = 'bottom',
  #           ##### this sentence need to be changed when new variables added
  #           options = list(
  #             columnDefs = list(list(
  #               visible = FALSE,
  #               targets = c(0, 1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
  #             )),
  #             columnDefs = list(list(
  #               width = '20px', targets = "_all"
  #             )),
  #             dom = 'Bfrtpl',
  #             buttons = I('colvis'),
  #             initComplete = JS(
  #               "function(settings, json) {",
  #               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  #               "}"
  #             ),
  #             paging = TRUE,
  #             scrollX = TRUE,
  #             lengthMenu = c(
  #               pagenumber(),
  #               2 * pagenumber(),
  #               3 * pagenumber(),
  #               4 * pagenumber(),
  #               5 * pagenumber()
  #             ),
  #             pageLength = pagenumber(),
  #             fixedColumns = list(leftColumns = otnum, rightColumns = 0)
  #           )
  #         ) %>%
  #         formatStyle(
  #           "CORPORATE",
  #           target = "row",
  #           fontWeight = styleEqual(c("Company", "Overall"), c('bold', 'bold')),
  #           backgroundColor = styleEqual(c("Company", "Overall"), c('deepskyblue', "grey")),
  #           color = styleEqual(c("Company", "Overall"), c('white', 'white'))
  #         )
  #     } else{
  #       dat <- DT::datatable(
  #         ot,
  #         rownames = FALSE,
  #         #filter = 'bottom',
  #         extensions = c('FixedColumns', 'Buttons'),
  #         options = list(
  #           columnDefs = list(list(
  #             visible = FALSE,
  #             targets = c(0, 1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
  #           )),
  #           columnDefs = list(list(
  #             width = '20px', targets = "_all"
  #           )),
  #           autoWidth = TRUE,
  #           dom = '<"bottom">Bfrtpl',
  #           buttons = I('colvis'),
  #           initComplete = JS(
  #             "function(settings, json) {",
  #             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  #             "}"
  #           ),
  #           scrollX = TRUE,
  #           paging = TRUE,
  #           lengthMenu = c(
  #             pagenumber(),
  #             2 * pagenumber(),
  #             3 * pagenumber(),
  #             4 * pagenumber(),
  #             5 * pagenumber()
  #           ),
  #           pageLength = pagenumber(),
  #           fixedColumns = list(leftColumns = otnum, rightColumns = 0)
  #         )
  #       ) %>%
  #         formatStyle(
  #           "CORPORATE",
  #           target = "row",
  #           fontWeight = styleEqual(c("Company", "Overall"), c('bold', 'bold')),
  #           backgroundColor = styleEqual(c("Company", "Overall"), c('deepskyblue', "grey")),
  #           color = styleEqual(c("Company", "Overall"), c('white', 'white'))
  #         )
  #       #  formatCurrency("CORPORATE",target = "row", currency = "", interval = 3, mark = ",")
  #       #    formatCurrency( "Index",target = "row",  currency = styleEqual(c("ABS"), c('')),
  #       #                 interval = styleEqual(c("ABS"), c(3)),  mark = styleEqual(c("ABS"), c( ",")))
  #       
  #     }
  #     return(dat)
  #   })
  # })
  # 
  # output$contents_m <- renderDataTable({
  #   input$goButton
  #   isolate({
  #     if (is.null(ot()))
  #       return(NULL)
  #     ot <- ot()$result3_m
  #     ot <- filter(
  #       ot,
  #       (COMPS.DESC != "Overall") |
  #         (CORPORATE.DESC == "Overall" &
  #            COMPS.DESC == "Overall")
  #     )
  #     
  #     if ("ALL" %in% input$province) {
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region,  province())),]
  #     } else {
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, input$province)),]
  #     }
  #     top_brand <- graph1m(
  #       summary,
  #       cate = input$category,
  #       subcate = input$subcategory,
  #       value = "RENMINBI",
  #       period = input$period,
  #       kpi = "abs",
  #       window = 1,
  #       level = "molecule",
  #       top = as.integer(input$top3)
  #     ) %>%
  #       select(AUDIT.DESC, PRODUCT.DESC, COMPS.DESC)
  #     
  #     levels_contains <- list(AUDIT.DESC = levels(ot$AUDIT.DESC),
  #                             PRODUCT.DESC = levels(ot$PRODUCT.DESC),
  #                             COMPS.DESC = levels(ot$COMPS.DESC))
  #     
  #     ot <- ot %>% inner_join(top_brand)
  #    
  #     
  #     summary1 <- summary()[which(summary()$AUDIT.DESC == "China"),]
  #     ggg <-  rbind.fill(
  #       graph1(
  #         summary1,
  #         cate = input$category,
  #         subcate = input$subcategory,
  #         value = "RENMINBI",
  #         period = input$period,
  #         kpi = "abs",
  #         window = 1,
  #         level = "corporation"
  #       )
  #     )
  #     ggg <- distinct(ggg) %>%
  #       filter((CORPORATE.DESC != "Overall" &
  #                 COMPS.DESC != "Overall") |
  #                (CORPORATE.DESC == "Overall" &
  #                   COMPS.DESC == "Overall")
  #       ) 
  #     tmp <- ggg[order(-ggg[, length(ggg)]),]$PRODUCT.DESC
  #     
  #     
  #     ot$AUDIT.DESC <- factor(ot$AUDIT.DESC, 
  #                             levels = levels_contains$AUDIT.DESC)
  #     ot$COMPS.DESC <- factor(ot$COMPS.DESC, 
  #                                 levels = levels_contains$COMPS.DESC)
  #     
  #     ot$PRODUCT.DESC <- factor(ot$PRODUCT.DESC, levels = tmp)
  #     
  #     ot <-
  #       ot[order(ot$AUDIT.DESC,
  #                       ot$COMPS.DESC,
  #                       ot$PRODUCT.DESC),]
  #     
  #     
  #     ot$AUDIT.DESC <- gsub(" .*$", "", ot$AUDIT.DESC)
  #     ot$PRODUCT.DESC <- as.character(ot$PRODUCT.DESC)
  #     ot$COMPS.DESC <- as.character(ot$COMPS.DESC)
  #     ot$PRODUCT.DESC[which(ot$COMPS.DESC == "Overall")] <-
  #       ot$AUDIT.DESC[which(ot$COMPS.DESC == "Overall")]
  #     KK <- which(ot$PRODUCT.DESC == "Overall")
  #     ot$PRODUCT.DESC[KK] <- ot$COMPS.DESC[KK]
  #     ot$COMPS.DESC[KK] <- "Molecule"
  #     names(ot)[names(ot) == 'AUDIT.DESC'] <- 'AUDIT'
  #     names(ot)[names(ot) == 'CORPORATE.DESC'] <- 'CORPORATE'
  #     names(ot)[names(ot) == 'PRODUCT.DESC'] <- 'PRODUCT'
  #     ot$PRODUCT <- as.factor(ot$PRODUCT)
  #     ot$CORPORATE <- as.factor(ot$CORPORATE)
  #     ot$AUDIT <- as.factor(ot$AUDIT)
  #     ot <- as.data.frame(ot)
  #     otnum <- which(grepl("Index", names(ot)))
  #     #number here need to be updated for new update
  #     # ot[which(ot$Index=="ABS"),(otnum+1):length(ot)]<-
  #     #   format(ot[which(ot$Index=="ABS"), (otnum+1):length(ot)], nsmall = 2, big.mark = ",", scientific = FALSE)
  #     ot[, (otnum + 1):length(ot)] <-
  #       format(
  #         ot[, (otnum + 1):length(ot)],
  #         nsmall = 2,
  #         big.mark = ",",
  #         scientific = FALSE
  #       )
  #     ot$Measure[which(ot$Measure == "RENMINBI")] <- "RMB"
  #     if (input$period == "mat" |
  #         input$period == "ytd" |
  #         input$period == "yrl" |
  #         input$period == "qtr" &
  #         as.numeric(input$window) <= 3 |
  #         (input$period == "rqtr" &
  #          input$window == "1") | (input$period == "mth" & input$window == "1")) {
  #       dat <-
  #         DT::datatable(
  #           ot ,
  #           rownames = FALSE,
  #           extensions = c('FixedColumns', 'Buttons'),
  #           #filter = 'bottom',
  #           
  #           ##### this sentence need to be changed when new variables added
  #           options = list(
  #             columnDefs = list(list(
  #               visible = FALSE,
  #               targets = c(0, 1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
  #             )),
  #             columnDefs = list(list(
  #               width = '20px', targets = "_all"
  #             )),
  #             dom = 'Bfrtpl',
  #             buttons = I('colvis'),
  #             initComplete = JS(
  #               "function(settings, json) {",
  #               "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  #               "}"
  #             ),
  #             scrollX = TRUE,
  #             paging = TRUE,
  #             fixedColumns = list(leftColumns = otnum, rightColumns = 0),
  #             lengthMenu = c(
  #               pagenumber(),
  #               2 * pagenumber(),
  #               3 * pagenumber(),
  #               4 * pagenumber(),
  #               5 * pagenumber()
  #             ),
  #             pageLength = pagenumber(),
  #             fixedColumns = list(leftColumns = otnum, rightColumns = 0)
  #           )
  #         ) %>%
  #         formatStyle(
  #           "COMPS.DESC",
  #           target = "row",
  #           fontWeight = styleEqual(c("Molecule", "Overall"), c('bold', 'bold')),
  #           backgroundColor = styleEqual(c("Molecule", "Overall"), c('deepskyblue', "grey")),
  #           color = styleEqual(c("Molecule", "Overall"), c('white', 'white'))
  #         )
  #     } else{
  #       dat <- DT::datatable(
  #         ot,
  #         rownames = FALSE,
  #         #filter = 'bottom',
  #         extensions = c('FixedColumns', 'Buttons'),
  #         options = list(
  #           columnDefs = list(list(
  #             visible = FALSE,
  #             targets = c(0, 1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
  #           )),
  #           columnDefs = list(list(
  #             width = '20px', targets = "_all"
  #           )),
  #           autoWidth = TRUE,
  #           dom = '<"bottom">Bfrtpl',
  #           buttons = I('colvis'),
  #           initComplete = JS(
  #             "function(settings, json) {",
  #             "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
  #             "}"
  #           ),
  #           scrollX = TRUE,
  #           paging = TRUE,
  #           lengthMenu = c(
  #             pagenumber(),
  #             2 * pagenumber(),
  #             3 * pagenumber(),
  #             4 * pagenumber(),
  #             5 * pagenumber()
  #           ),
  #           pageLength = pagenumber(),
  #           fixedColumns = list(leftColumns = otnum, rightColumns = 0)
  #         )
  #       ) %>%
  #         formatStyle(
  #           "COMPS.DESC",
  #           target = "row",
  #           fontWeight = styleEqual(c("Molecule", "Overall"), c('bold', 'bold')),
  #           backgroundColor = styleEqual(c("Molecule", "Overall"), c('deepskyblue', "grey")),
  #           color = styleEqual(c("Molecule", "Overall"), c('white', 'white'))
  #         )
  #       #  formatCurrency("CORPORATE",target = "row", currency = "", interval = 3, mark = ",")
  #       #    formatCurrency( "Index",target = "row",  currency = styleEqual(c("ABS"), c('')),
  #       #                 interval = styleEqual(c("ABS"), c(3)),  mark = styleEqual(c("ABS"), c( ",")))
  #     }
  #     return(dat)
  #   })
  # })
  # 
  # 
  # ################################### download data############################
  # 
  # writeDown <- function(data1, data2, data3) {
  #   wb <- createWorkbook()
  #   ## 1
  #   addWorksheet(wb, "Product")
  #   writeDataTable(
  #     wb,
  #     sheet = "Product",
  #     x = data1,
  #     withFilter = F,
  #     startRow = 1,
  #     rowNames = F,
  #     colNames = T
  #   )
  #   ## 2
  #   addWorksheet(wb, "Corporation")
  #   writeDataTable(
  #     wb,
  #     sheet = "Corporation",
  #     x = data2,
  #     withFilter = F,
  #     startRow = 1,
  #     rowNames = F,
  #     colNames = T
  #   )
  #   ## 3
  #   addWorksheet(wb, "Molecule")
  #   writeDataTable(
  #     wb,
  #     sheet = "Molecule",
  #     x = data3,
  #     withFilter = F,
  #     startRow = 1,
  #     rowNames = F,
  #     colNames = T
  #   )
  #   return(wb)
  # }
  # 
  # otdown <- reactive({
  #   if (input$goButton == 0)
  #     return(NULL)
  #   input$goButton
  #   isolate({
  #     outputtable <- tablefor3()$result3
  #     outputtable <-
  #       filter(
  #         outputtable,
  #         (CORPORATE.DESC != "Overall" & COMPS.DESC != "Overall") |
  #           (CORPORATE.DESC == "Overall" &
  #              COMPS.DESC == "Overall")
  #       )
  #     names(outputtable) <- paste0(names(outputtable), "\t")
  #     outputtable
  #   })
  # })
  # 
  # otdown_c <- reactive({
  #   if (input$goButton == 0)
  #     return(NULL)
  #   input$goButton
  #   isolate({
  #     outputtable <- tablefor3()$result3_c
  #     outputtable <-
  #       filter(
  #         outputtable,
  #         (CORPORATE.DESC != "Overall") |
  #           (CORPORATE.DESC == "Overall" &
  #              COMPS.DESC == "Overall")
  #       )
  #     if ("ALL" %in% input$province) {
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region,  province())),]
  #     } else {
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, input$province)),]
  #     }
  #     top_brand <- graph1m(
  #       summary,
  #       cate = input$category,
  #       subcate = input$subcategory,
  #       value = "RENMINBI",
  #       period = input$period,
  #       kpi = "abs",
  #       window = 1,
  #       level = "corporation",
  #       top = as.integer(input$top2)
  #     ) %>%
  #       select(AUDIT.DESC, PRODUCT.DESC, CORPORATE.DESC)
  #     outputtable <- outputtable %>% inner_join(top_brand)
  #     names(outputtable) <- paste0(names(outputtable), "\t")
  #     outputtable
  #   })
  # })
  # 
  # otdown_m <- reactive({
  #   if (input$goButton == 0)
  #     return(NULL)
  #   input$goButton
  #   isolate({
  #     outputtable <- tablefor3()$result3_m
  #     outputtable <- filter(
  #       outputtable,
  #       (COMPS.DESC != "Overall") |
  #         (CORPORATE.DESC == "Overall" &
  #            COMPS.DESC == "Overall")
  #     )
  #     if ("ALL" %in% input$province) {
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region,  province())),]
  #     } else {
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, input$province)),]
  #     }
  #     top_brand <- graph1m(
  #       summary,
  #       cate = input$category,
  #       subcate = input$subcategory,
  #       value = "RENMINBI",
  #       period = input$period,
  #       kpi = "abs",
  #       window = 1,
  #       level = "molecule",
  #       top = as.integer(input$top3)
  #     ) %>%
  #       select(AUDIT.DESC, PRODUCT.DESC, COMPS.DESC)
  #     outputtable <- outputtable %>% inner_join(top_brand)
  #     names(outputtable) <- paste0(names(outputtable), "\t")
  #     outputtable
  #   })
  # })
  # 
  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     paste("CHPA_Data_",
  #           input$period,
  #           "_",
  #           input$window,
  #           "year",
  #           '.xlsx',
  #           sep = '')
  #   },
  #   content = function(file) {
  #     saveWorkbook(writeDown(otdown(), otdown_c(), otdown_m()),
  #                  file,
  #                  overwrite = TRUE)
  #   }
  # )
  # 
  # 
  # ################################ for plot ####################################
  # value_m <- c("UNIT", "DOT", "RMB")
  # names(value_m) <- c("UNIT", "DOT", "Value In RMB")
  # value_choice = list("UNIT" = "UNIT",
  #                     "DOT" = "DOT",
  #                     "Value In RMB" = "RMB")
  # 
  # kpi_m <- c("abs", "ms", "mc", "gr", "ev", "pi")
  # names(kpi_m) <- c("Sales", "MS%", "MS+/- %", "GR%", "EV", "PI")
  # kpi_choice = list(
  #   "Sales" = "abs",
  #   "MS%" = "ms",
  #   "MS+/- %" = "mc",
  #   "GR%" = "gr",
  #   "EV" = "ev",
  #   "PI" = "pi"
  # )
  # observeEvent(input$goButton, priority = 1, {
  #   toggleState(id = "bp_p", condition = as.integer(input$top) > 0)
  #   toggleState(id = "rpp_p", condition = as.integer(input$top) > 0)
  #   
  #   toggleState(id = "sub_top", condition = as.integer(input$top) > 0)
  #   toggleState(id = "sub_top_c", condition = as.integer(input$top) > 0)
  #   toggleState(id = "sub_top_m", condition = as.integer(input$top) > 0)
  #   
  #   toggleState(id = "sub_region", condition = as.integer(input$top) > 0)
  #   toggleState(id = "sub_region_c", condition = as.integer(input$top) > 0)
  #   toggleState(id = "sub_region_m", condition = as.integer(input$top) > 0)
  #   
  #   
  #   if ("ALL" %in% input$province) {
  #     tmp_region <- setdiff(province(), "ALL")
  #   } else {
  #     tmp_region <- input$province
  #   }
  #   
  #   brand_choice <- unique(toplist())
  #   
  #   if ("RENMINBI" %in% input$value) {
  #     tmp_value <- c("RMB", input$value[input$value != "RENMINBI"])
  #   } else {
  #     tmp_value <- input$value
  #   }
  #   
  #   corporation_choice <- unique(toplist_c())
  #   molecule_choice <- unique(toplist_m())
  #   
  #   ##-- for brand
  #   observe({
  #     if (input$bp_p) {
  #       updateMaterialSwitch(session, inputId = "rpp_p",
  #                            value = FALSE)
  #     } else {
  #       updateMaterialSwitch(session, inputId = "rpp_p",
  #                            value = TRUE)
  #     }
  #   })
  #   
  #   observe({
  #     if (input$rpp_p) {
  #       updateMaterialSwitch(session, inputId = "bp_p",
  #                            value = FALSE)
  #     } else {
  #       updateMaterialSwitch(session, inputId = "bp_p",
  #                            value = TRUE)
  #     }
  #   })
  #   
  #   observe({
  #     if (input$bp_p) {
  #       output$sub_top_ui <- renderUI(selectInput(
  #         "sub_top",
  #         "Top Brand",
  #         choices = brand_choice[-1],
  #         selected =  brand_choice[-1][1:ifelse(length(brand_choice[-1]) < 5, length(brand_choice[-1]), 5)],
  #         multiple = TRUE
  #       ))
  #     } else {
  #       output$sub_top_ui <- renderUI(selectInput(
  #         "sub_top",
  #         "Top Brand",
  #         choices = brand_choice[-1],
  #         selected = brand_choice[2],
  #         multiple = FALSE
  #       ))
  #       
  #     }
  #     # })
  #     
  #     output$sub_measure_ui <- renderUI(
  #       selectInput(
  #         "sub_measure",
  #         "Measure",
  #         choices = value_choice[names(value_m[value_m %in% tmp_value])],
  #         selected = tmp_value[1],
  #         multiple = FALSE
  #       )
  #     )
  #     
  #     output$sub_index_ui <- renderUI(
  #       selectInput(
  #         "sub_index",
  #         "Index",
  #         choices = kpi_choice[names(kpi_m[kpi_m %in% input$kpi])],
  #         selected = input$kpi[1],
  #         multiple = FALSE
  #       )
  #     )
  #     
  #   })
  #   
  #   
  #   observe({
  #     if (input$rpp_p) {
  #       output$sub_region_ui <- renderUI(selectInput(
  #         "sub_region",
  #         "Region/Province",
  #         choices = c(input$region, tmp_region),
  #         selected = c(input$region, tmp_region)[1],
  #         multiple = TRUE
  #       ))
  #     } else {
  #       output$sub_region_ui <- renderUI(selectInput(
  #         "sub_region",
  #         "Region/Province",
  #         choices = c(input$region, tmp_region),
  #         selected = c(input$region, tmp_region)[1],
  #         multiple = FALSE
  #       ))
  #     }
  #   })
  #   
  #   ##-- for corporation
  #   observe({
  #     if (input$bp_p_c) {
  #       updateMaterialSwitch(session, inputId = "rpp_p_c",
  #                            value = FALSE)
  #     } else {
  #       updateMaterialSwitch(session, inputId = "rpp_p_c",
  #                            value = TRUE)
  #     }
  #   })
  #   
  #   observe({
  #     if (input$rpp_p_c) {
  #       updateMaterialSwitch(session, inputId = "bp_p_c",
  #                            value = FALSE)
  #     } else {
  #       updateMaterialSwitch(session, inputId = "bp_p_c",
  #                            value = TRUE)
  #     }
  #   })
  #   
  #   observe({
  #     # toggleState(id = "bp_p_c", condition = as.integer(input$rpp_p_c) == 0)
  #     # toggleState(id = "rpp_p_c", condition = as.integer(input$bp_p_c) == 0)
  #     if (input$bp_p_c) {
  #       output$sub_top_c_ui <- renderUI(
  #         selectInput(
  #           "sub_top_c",
  #           "Top Corporation",
  #           choices  = corporation_choice[-1],
  #           selected =  corporation_choice[-1][1:ifelse(length(corporation_choice[-1]) < 5, length(corporation_choice[-1]), 5)],
  #           multiple = TRUE
  #         )
  #       )
  #     } else {
  #       output$sub_top_c_ui <- renderUI(
  #         selectInput(
  #           "sub_top_c",
  #           "Top Corporation",
  #           choices = corporation_choice[-1],
  #           selected = corporation_choice[2],
  #           multiple = FALSE
  #         )
  #       )
  #     }
  #     
  #     output$sub_measure_c_ui <- renderUI(
  #       selectInput(
  #         "sub_measure_c",
  #         "Measure",
  #         choices = value_choice[names(value_m[value_m %in% tmp_value])],
  #         selected = tmp_value[1],
  #         multiple = FALSE
  #       )
  #     )
  #     
  #     output$sub_index_c_ui <- renderUI(
  #       selectInput(
  #         "sub_index_c",
  #         "Index",
  #         choices = kpi_choice[names(kpi_m[kpi_m %in% input$kpi])],
  #         selected = input$kpi[1],
  #         multiple = FALSE
  #       )
  #     )
  #     
  #     if (input$rpp_p_c) {
  #       output$sub_region_c_ui <- renderUI(selectInput(
  #         "sub_region_c",
  #         "Region/Province",
  #         choices = c(input$region, tmp_region),
  #         selected = c(input$region, tmp_region)[1],
  #         multiple = TRUE
  #       ))
  #     } else {
  #       output$sub_region_c_ui <- renderUI(
  #         selectInput(
  #           "sub_region_c",
  #           "Region/Province",
  #           choices = c(input$region, tmp_region),
  #           selected = c(input$region, tmp_region)[1],
  #           multiple = FALSE
  #         )
  #       )
  #     }
  #   })
  #   
  #   ##-- for molecule
  #   # observe({
  #   #   if (input$bp_p_m) {
  #   #     updateMaterialSwitch(session, inputId = "rpp_p_m",
  #   #                          value = FALSE)
  #   #   } else {
  #   #     updateMaterialSwitch(session, inputId = "rpp_p_m",
  #   #                          value = TRUE)
  #   #   }
  #   #   
  #   #   
  #   # })
  #   # 
  #   # observe({
  #   #   if (input$rpp_p_m) {
  #   #     updateMaterialSwitch(session, inputId = "bp_p_m",
  #   #                          value = FALSE)
  #   #   } else {
  #   #     updateMaterialSwitch(session, inputId = "bp_p_m",
  #   #                          value = TRUE)
  #   #   }
  #   # })
  #   # 
  #   # observe({
  #   #   # toggleState(id = "bp_p_m", condition = as.integer(input$rpp_p_m) == 0)
  #   #   # toggleState(id = "rpp_p_m", condition = as.integer(input$bp_p_m) == 0)
  #   #   if (input$bp_p_m) {
  #   #     output$sub_top_m_ui <- renderUI(
  #   #       selectInput(
  #   #         "sub_top_m",
  #   #         "Top Molecule",
  #   #         choices = molecule_choice[-1],
  #   #         multiple = TRUE
  #   #       )
  #   #     )
  #   #   } else {
  #   #     output$sub_top_m_ui <- renderUI(
  #   #       selectInput(
  #   #         "sub_top_m",
  #   #         "Top Molecule",
  #   #         choices = molecule_choice[-1],
  #   #         multiple = FALSE
  #   #       )
  #   #     )
  #   #   }
  #   #   
  #   #   output$sub_measure_m_ui <- renderUI(
  #   #     selectInput(
  #   #       "sub_measure_m",
  #   #       "Measure",
  #   #       choices = value_choice[names(value_m[value_m %in% tmp_value])],
  #   #       selected = tmp_value[1],
  #   #       multiple = FALSE
  #   #     )
  #   #   )
  #   #   
  #   #   output$sub_index_m_ui <- renderUI(
  #   #     selectInput(
  #   #       "sub_index_m",
  #   #       "Index",
  #   #       choices = kpi_choice[names(kpi_m[kpi_m %in% input$kpi])],
  #   #       selected = input$kpi[1],
  #   #       multiple = FALSE
  #   #     )
  #   #   )
  #   #   
  #   #   if (input$rpp_p_m) {
  #   #     output$sub_region_m_ui <- renderUI(selectInput(
  #   #       "sub_region_m",
  #   #       "Region/Province",
  #   #       choices = c(input$region, tmp_region),
  #   #       multiple = TRUE
  #   #     ))
  #   #   } else {
  #   #     output$sub_region_m_ui <- renderUI(
  #   #       selectInput(
  #   #         "sub_region_m",
  #   #         "Region/Province",
  #   #         choices = c(input$region, tmp_region),
  #   #         multiple = FALSE
  #   #       )
  #   #     )
  #   #   }
  #   #   
  #   #   
  #   # })
  # })
  # 
  # product_plot <- reactive({
  #   if (input$goButton == 0) {
  #     return(NULL)
  #   }
  #   # input$goButton
  #   # isolate({
  #   if (is.null(tablefor3()))
  #     return(NULL)
  #   
  #   ot <- tablefor3()$result3
  #   ot <-
  #     filter(
  #       ot,
  #       (CORPORATE.DESC != "Overall" & COMPS.DESC != "Overall") |
  #         (CORPORATE.DESC == "Overall" &
  #            COMPS.DESC == "Overall")
  #     )
  #   ot$AUDIT.DESC <- gsub(" .*$", "", ot$AUDIT.DESC)
  #   
  #   ot$PRODUCT.DESC <- as.character(ot$PRODUCT.DESC)
  #   ot$CORPORATE.DESC <- as.character(ot$CORPORATE.DESC)
  #   
  #   ot$PRODUCT.DESC[which(ot$CORPORATE.DESC == "Overall")] <-
  #     ot$AUDIT.DESC[which(ot$CORPORATE.DESC == "Overall")]
  #   KK <- which(ot$PRODUCT.DESC == "Overall")
  #   
  #   ot$PRODUCT.DESC[KK] <- ot$CORPORATE.DESC[KK]
  #   ot$CORPORATE.DESC[KK] <- "Company"
  #   
  #   names(ot)[names(ot) == 'AUDIT.DESC'] <- 'AUDIT'
  #   names(ot)[names(ot) == 'CORPORATE.DESC'] <- 'CORPORATE'
  #   names(ot)[names(ot) == 'PRODUCT.DESC'] <- 'PRODUCT'
  #   
  #   ot$PRODUCT <- as.factor(ot$PRODUCT)
  #   ot$CORPORATE <- as.factor(ot$CORPORATE)
  #   ot$AUDIT <- as.factor(ot$AUDIT)
  #   
  #   ot <- as.data.frame(ot)
  #   otnum <- which(grepl("Index", names(ot)))
  #   #number here need to be updated for new update
  #   ot[which(ot$Index == "ABS"), (otnum + 1):length(ot)] <-
  #     format(ot[which(ot$Index == "ABS"), (otnum + 1):length(ot)],
  #            big.mark = ",", scientific = FALSE)
  #   ot$Measure[which(ot$Measure == "RENMINBI")] <- "RMB"
  #   
  #   
  #   if ("ms" %in% input$sub_index) {
  #     tmp_sub_index <- "MS%"
  #   } else if ("gr" %in% input$sub_index) {
  #     tmp_sub_index <- "GR%"
  #   } else if ("mc" %in% input$sub_index) {
  #     tmp_sub_index <- "MS+/- %"
  #   } else {
  #     tmp_sub_index <- input$sub_index
  #   }
  #   
  #   if (as.integer(input$top) > 0) {
  #     plot_data <- ot %>%
  #       filter(
  #         Measure %in% input$sub_measure,
  #         Index %in% toupper(tmp_sub_index),
  #         AUDIT %in% gsub(" .*$", "", input$sub_region),
  #         # COMPS.DESC %in% c("Molecule", "Overall"),
  #         # (as.character(PRODUCT) == as.character(AUDIT) |
  #         as.character(PRODUCT) %in% input$sub_top
  #         # )
  #       ) %>%
  #       # toplist()[1:(1 + as.integer(input$sub_top))])) %>%
  #       gather(
  #         Date,
  #         Value,
  #         -AUDIT,
  #         -Category,
  #         -Sub.category,
  #         -CORPORATE,
  #         -PRODUCT,-COMPS.DESC,
  #         -pro.code,
  #         -MANUF.TYPE.DESC,
  #         -TC.III.SHORT.DESC,-TC.III.DESC,
  #         -Category_CN,
  #         -COMPS.DESC,
  #         -Brand_CN,
  #         -MANU_CN,-Measure,
  #         -Index
  #       ) %>%
  #       mutate(
  #         Date = as.Date(ymd(paste(
  #           Date, "01", "."
  #         ))),
  #         Value = as.numeric(gsub(",", "", Value)),
  #         Date1 = substr(as.character(Date), 1, 7)
  #       )
  #   } else {
  #     plot_data <- ot %>%
  #       filter(
  #         Measure %in% input$sub_measure,
  #         Index %in% toupper(tmp_sub_index),
  #         # AUDIT %in% gsub(" .*$", "", input$sub_region),
  #         # COMPS.DESC %in% c("Molecule", "Overall"),
  #         # (
  #         as.character(PRODUCT) == as.character(AUDIT)
  #         # |
  #         # as.character(PRODUCT) %in% input$sub_top
  #         # )
  #       ) %>%
  #       # toplist()[1:(1 + as.integer(input$sub_top))])) %>%
  #       gather(
  #         Date,
  #         Value,
  #         -AUDIT,
  #         -Category,
  #         -Sub.category,
  #         -CORPORATE,
  #         -PRODUCT,-COMPS.DESC,
  #         -pro.code,
  #         -MANUF.TYPE.DESC,
  #         -TC.III.SHORT.DESC,-TC.III.DESC,
  #         -Category_CN,
  #         -COMPS.DESC,
  #         -Brand_CN,
  #         -MANU_CN,-Measure,
  #         -Index
  #       ) %>%
  #       mutate(
  #         Date = as.Date(ymd(paste(
  #           Date, "01", "."
  #         ))),
  #         Value = as.numeric(gsub(",", "", Value)),
  #         Date1 = substr(as.character(Date), 1, 7)
  #       )
  #   }
  #   
  #   tickval <- unlist(distinct(plot_data, Date1))
  #   
  #   if ((as.integer(input$top) > 0 && input$bp_p) ||
  #       as.numeric(input$top) == 0) {
  #     if (input$label_p) {
  #       if (input$sub_index %in% c("abs")) {
  #         p <- plot_ly(hoverinfo = "x+y+name")
  #         for (i in as.character(unique(plot_data$PRODUCT))) {
  #           if (grepl("B.I$", i)) {
  #             p <- add_trace(
  #               p,
  #               x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
  #               y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"] / 1000000,
  #               type = "scatter",
  #               mode = "lines+markers",
  #               color = list("blue"),
  #               colors = "Set1",
  #               name = i
  #             ) %>%
  #               add_text(
  #                 x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
  #                 y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"] / 1000000,
  #                 text = round(plot_data[as.character(plot_data$PRODUCT) == i, "Value"]  / 1000000, 2),
  #                 textfont = list(size = 10),
  #                 textposition = "top",
  #                 hoverinfo = "none",
  #                 showlegend = FALSE
  #               )
  #           } else {
  #             p <- add_trace(
  #               p,
  #               x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
  #               y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"] / 1000000,
  #               type = "scatter",
  #               mode = "lines+markers",
  #               colors = "Set1",
  #               name = i
  #             ) %>%
  #               add_text(
  #                 x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
  #                 y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"]  / 1000000,
  #                 text = round(plot_data[as.character(plot_data$PRODUCT) == i, "Value"]  / 1000000, 2),
  #                 textfont = list(size = 10),
  #                 textposition = "top",
  #                 hoverinfo = "none",
  #                 showlegend = FALSE
  #               )
  #           }
  #           
  #         }
  #         
  #         p %>%
  #           config(
  #             displaylogo = FALSE,
  #             collaborate = FALSE,
  #             modeBarButtonsToRemove = list(
  #               'sendDataToCloud',
  #               'autoScale2d',
  #               'zoom2d',
  #               'pan2d',
  #               'select2d',
  #               'lasso2d',
  #               'toggleSpikelines'
  #             )
  #           ) %>%
  #           layout(
  #             title='Players Trend Performance',
  #             xaxis = list(
  #               zeroline = FALSE,
  #               title = "",
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks",
  #               tickvals = tickval,
  #               tickformat = "%y%m",
  #               type = "date"
  #             ),
  #             yaxis = list(
  #               zeroline = FALSE,
  #               title = paste(toupper(input$sub_measure), 
  #                             "(", 
  #                             ifelse(input$sub_index == "abs",
  #                                    "Million", toupper(input$sub_index)),
  #                             ")",
  #                             sep = ""), 
  #               # "Measure + (Million)",
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks"
  #             ),
  #             legend = list(x = 0.5,
  #                           xanchor = "center",
  #                           # y = 100,
  #                           orientation = "h")
  #           )
  #       } else {
  #         p <- plot_ly(hoverinfo = "x+y+name")
  #         for (i in as.character(unique(plot_data$PRODUCT))) {
  #           if (grepl("B.I$", i)) {
  #             p <- add_trace(
  #               p,
  #               x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
  #               y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"],
  #               type = "scatter",
  #               mode = "lines+markers",
  #               color = list("blue"),
  #               colors = "Set1",
  #               name = i
  #             ) %>%
  #               add_text(
  #                 x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
  #                 y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"],
  #                 text = round(plot_data[as.character(plot_data$PRODUCT) == i, "Value"], 2),
  #                 textfont = list(size = 10),
  #                 textposition = "top",
  #                 hoverinfo = "none",
  #                 showlegend = FALSE
  #               )
  #           } else {
  #             p <- add_trace(
  #               p,
  #               x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
  #               y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"],
  #               type = "scatter",
  #               mode = "lines+markers",
  #               colors = "Set1",
  #               name = i
  #             ) %>%
  #               add_text(
  #                 x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
  #                 y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"],
  #                 text = round(plot_data[as.character(plot_data$PRODUCT) == i, "Value"], 2),
  #                 textfont = list(size = 10),
  #                 textposition = "top",
  #                 hoverinfo = "none",
  #                 showlegend = FALSE
  #               )
  #           }
  #           
  #         }
  #         
  #         p %>%
  #           config(
  #             displaylogo = FALSE,
  #             collaborate = FALSE,
  #             modeBarButtonsToRemove = list(
  #               'sendDataToCloud',
  #               'autoScale2d',
  #               'zoom2d',
  #               'pan2d',
  #               'select2d',
  #               'lasso2d',
  #               'toggleSpikelines'
  #             )
  #           ) %>%
  #           layout(
  #             title='Players Trend Performance',
  #             xaxis = list(
  #               zeroline = FALSE,
  #               title = "",
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks",
  #               tickvals = tickval,
  #               tickformat = "%y%m",
  #               type = "date"
  #             ),
  #             yaxis = list(
  #               zeroline = FALSE,
  #               title = paste(toupper(input$sub_measure), 
  #                             "(", 
  #                             ifelse(input$sub_index == "abs",
  #                                    "Million", toupper(input$sub_index)),
  #                             ")",
  #                             sep = ""), #"<b>Value</b>",
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks"
  #             ),
  #             legend = list(x = 0.5,
  #                           xanchor = "center",
  #                           # y = 100,
  #                           orientation = "h")
  #           )
  #       }
  #       
  #     } else {
  #       if (c("abs") %in% input$sub_index) {
  #         p <- plot_ly(hoverinfo = "x+y+name")
  #         for (i in as.character(unique(plot_data$PRODUCT))) {
  #           if (grepl("B.I$", i)) {
  #             p <- add_trace(
  #               p,
  #               x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
  #               y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"] / 1000000,
  #               type = "scatter",
  #               mode = "lines+markers",
  #               color = list("blue"),
  #               colors = "Set1",
  #               name = i
  #             )
  #             # %>%
  #             #   add_text(x = plot_data[as.character(plot_data$PRODUCT) == i, "date"],
  #             #            y = plot_data[as.character(plot_data$PRODUCT) == i, "value"] / 1000000,
  #             #            text = round(plot_data[as.character(plot_data$PRODUCT) == i, "value"]  / 1000000, 0),
  #             #            textfont = list(size = 10),
  #             #            textposition = "top",
  #             #            showlegend = FALSE)
  #           } else {
  #             p <- add_trace(
  #               p,
  #               x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
  #               y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"] / 1000000,
  #               type = "scatter",
  #               mode = "lines+markers",
  #               colors = "Set1",
  #               name = i
  #             )
  #             # %>%
  #             #   add_text(x = plot_data[as.character(plot_data$PRODUCT) == i, "date"],
  #             #            y = plot_data[as.character(plot_data$PRODUCT) == i, "value"]  / 1000000,
  #             #            text = round(plot_data[as.character(plot_data$PRODUCT) == i, "value"]  / 1000000, 0),
  #             #            textfont = list(size = 10),
  #             #            textposition = "top",
  #             #            showlegend = FALSE)
  #           }
  #           
  #         }
  #         
  #         p %>%
  #           config(
  #             displaylogo = FALSE,
  #             collaborate = FALSE,
  #             modeBarButtonsToRemove = list(
  #               'sendDataToCloud',
  #               'autoScale2d',
  #               'zoom2d',
  #               'pan2d',
  #               'select2d',
  #               'lasso2d',
  #               'toggleSpikelines'
  #             )
  #           ) %>%
  #           layout(
  #             title='Players Trend Performance',
  #             xaxis = list(
  #               zeroline = FALSE,
  #               title = "",
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks",
  #               tickvals = tickval,
  #               tickformat = "%y%m",
  #               type = "date"
  #             ),
  #             yaxis = list(
  #               zeroline = FALSE,
  #               title = paste(toupper(input$sub_measure), 
  #                             "(", 
  #                             ifelse(input$sub_index == "abs",
  #                                    "Million", toupper(input$sub_index)),
  #                             ")",
  #                             sep = ""),
  #               # "Measure + (Million)",
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks"
  #             ),
  #             legend = list(x = 0.5,
  #                           xanchor = "center",
  #                           # y = 100,
  #                           orientation = "h")
  #           )
  #         
  #       } else {
  #         p <- plot_ly(hoverinfo = "x+y+name")
  #         for (i in as.character(unique(plot_data$PRODUCT))) {
  #           if (grepl("B.I$", i)) {
  #             p <- add_trace(
  #               p,
  #               x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
  #               y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"],
  #               type = "scatter",
  #               mode = "lines+markers",
  #               color = list("blue"),
  #               colors = "Set1",
  #               name = i
  #             )
  #           } else {
  #             p <- add_trace(
  #               p,
  #               x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
  #               y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"],
  #               type = "scatter",
  #               mode = "lines+markers",
  #               colors = "Set1",
  #               name = i
  #             )
  #           }
  #           
  #         }
  #         
  #         p %>%
  #           config(
  #             displaylogo = FALSE,
  #             collaborate = FALSE,
  #             modeBarButtonsToRemove = list(
  #               'sendDataToCloud',
  #               'autoScale2d',
  #               'zoom2d',
  #               'pan2d',
  #               'select2d',
  #               'lasso2d',
  #               'toggleSpikelines'
  #             )
  #           ) %>%
  #           layout(
  #             title='Players Trend Performance',
  #             xaxis = list(
  #               zeroline = FALSE,
  #               title = "",
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks",
  #               tickvals = tickval,
  #               tickformat = "%y%m",
  #               type = "date"
  #             ),
  #             yaxis = list(
  #               zeroline = FALSE,
  #               title = paste(toupper(input$sub_measure), 
  #                             "(", 
  #                             ifelse(input$sub_index == "abs",
  #                                    "Million", toupper(input$sub_index)),
  #                             ")",
  #                             sep = ""), #"Measurement",
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks"
  #             ),
  #             legend = list(x = 0.5,
  #                           xanchor = "center",
  #                           # y = 100,
  #                           orientation = "h")
  #           )
  #       }
  #     }
  #   } else {
  #     if (input$label_p) {
  #       if (c("abs") %in% input$sub_index) {
  #         p <- plot_ly(hoverinfo = "x+y+name")
  #         for (i in as.character(unique(plot_data$AUDIT))) {
  #           if (grepl("B.I$", i)) {
  #             p <- add_trace(
  #               p,
  #               x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
  #               y = plot_data[as.character(plot_data$AUDIT) == i, "Value"] / 1000000,
  #               type = "scatter",
  #               mode = "lines+markers",
  #               color = list("blue"),
  #               colors = "Set1",
  #               name = i
  #             ) %>%
  #               add_text(
  #                 x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
  #                 y = plot_data[as.character(plot_data$AUDIT) == i, "Value"] / 1000000,
  #                 text = round(plot_data[as.character(plot_data$AUDIT) == i, "Value"]  / 1000000, 2),
  #                 textfont = list(size = 10),
  #                 textposition = "top",
  #                 hoverinfo = "none",
  #                 showlegend = FALSE
  #               )
  #           } else {
  #             p <- add_trace(
  #               p,
  #               x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
  #               y = plot_data[as.character(plot_data$AUDIT) == i, "Value"] / 1000000,
  #               type = "scatter",
  #               mode = "lines+markers",
  #               colors = "Set1",
  #               name = i
  #             ) %>%
  #               add_text(
  #                 x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
  #                 y = plot_data[as.character(plot_data$AUDIT) == i, "Value"]  / 1000000,
  #                 text = round(plot_data[as.character(plot_data$AUDIT) == i, "Value"]  / 1000000, 2),
  #                 textfont = list(size = 10),
  #                 textposition = "top",
  #                 hoverinfo = "none",
  #                 showlegend = FALSE
  #               )
  #           }
  #           
  #         }
  #         
  #         p %>%
  #           config(
  #             displaylogo = FALSE,
  #             collaborate = FALSE,
  #             modeBarButtonsToRemove = list(
  #               'sendDataToCloud',
  #               'autoScale2d',
  #               'zoom2d',
  #               'pan2d',
  #               'select2d',
  #               'lasso2d',
  #               'toggleSpikelines'
  #             )
  #           ) %>%
  #           layout(
  #             title='Players Trend Performance',
  #             xaxis = list(
  #               zeroline = FALSE,
  #               title = "",
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks",
  #               tickvals = tickval,
  #               tickformat = "%y%m",
  #               type = "date"
  #             ),
  #             yaxis = list(
  #               zeroline = FALSE,
  #               title = paste(toupper(input$sub_measure), 
  #                             "(", 
  #                             ifelse(input$sub_index == "abs",
  #                                    "Million", toupper(input$sub_index)),
  #                             ")",
  #                             sep = ""),
  #               # "Measure + (Million)",
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks"
  #             ),
  #             legend = list(x = 0.5,
  #                           xanchor = "center",
  #                           # y = 100,
  #                           orientation = "h")
  #           )
  #       } else {
  #         p <- plot_ly(hoverinfo = "x+y+name")
  #         for (i in as.character(unique(plot_data$AUDIT))) {
  #           if (grepl("B.I$", i)) {
  #             p <- add_trace(
  #               p,
  #               x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
  #               y = plot_data[as.character(plot_data$AUDIT) == i, "Value"],
  #               type = "scatter",
  #               mode = "lines+markers",
  #               color = list("blue"),
  #               colors = "Set1",
  #               name = i
  #             ) %>%
  #               add_text(
  #                 x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
  #                 y = plot_data[as.character(plot_data$AUDIT) == i, "Value"],
  #                 text = round(plot_data[as.character(plot_data$AUDIT) == i, "Value"], 2),
  #                 textfont = list(size = 10),
  #                 textposition = "top",
  #                 hoverinfo = "none",
  #                 showlegend = FALSE
  #               )
  #           } else {
  #             p <- add_trace(
  #               p,
  #               x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
  #               y = plot_data[as.character(plot_data$AUDIT) == i, "Value"],
  #               type = "scatter",
  #               mode = "lines+markers",
  #               colors = "Set1",
  #               name = i
  #             ) %>%
  #               add_text(
  #                 x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
  #                 y = plot_data[as.character(plot_data$AUDIT) == i, "Value"],
  #                 text = round(plot_data[as.character(plot_data$AUDIT) == i, "Value"], 2),
  #                 textfont = list(size = 10),
  #                 textposition = "top",
  #                 hoverinfo = "none",
  #                 showlegend = FALSE
  #               )
  #           }
  #           
  #         }
  #         
  #         p %>%
  #           config(
  #             displaylogo = FALSE,
  #             collaborate = FALSE,
  #             modeBarButtonsToRemove = list(
  #               'sendDataToCloud',
  #               'autoScale2d',
  #               'zoom2d',
  #               'pan2d',
  #               'select2d',
  #               'lasso2d',
  #               'toggleSpikelines'
  #             )
  #           ) %>%
  #           layout(
  #             title='Players Trend Performance',
  #             xaxis = list(
  #               zeroline = FALSE,
  #               title = "",
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks",
  #               tickvals = tickval,
  #               tickformat = "%y%m",
  #               type = "date"
  #             ),
  #             
  #             yaxis = list(
  #               zeroline = FALSE,
  #               title = paste(toupper(input$sub_measure), 
  #                             "(", 
  #                             ifelse(input$sub_index == "abs",
  #                                    "Million", toupper(input$sub_index)),
  #                             ")",
  #                             sep = ""),#"Measurement",
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks"
  #             ),
  #             legend = list(x = 0.5,
  #                           xanchor = "center",
  #                           # y = 100,
  #                           orientation = "h")
  #           )
  #       }
  #       
  #     } else {
  #       if (c("abs") %in% input$sub_index) {
  #         p <- plot_ly(hoverinfo = "x+y+name")
  #         for (i in as.character(unique(plot_data$AUDIT))) {
  #           if (grepl("B.I$", i)) {
  #             p <- add_trace(
  #               p,
  #               x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
  #               y = plot_data[as.character(plot_data$AUDIT) == i, "Value"] / 1000000,
  #               type = "scatter",
  #               mode = "lines+markers",
  #               color = list("blue"),
  #               colors = "Set1",
  #               name = i
  #             )
  #             # %>%
  #             #   add_text(x = plot_data[as.character(plot_data$AUDIT) == i, "date"],
  #             #            y = plot_data[as.character(plot_data$AUDIT) == i, "value"] / 1000000,
  #             #            text = round(plot_data[as.character(plot_data$AUDIT) == i, "value"]  / 1000000, 0),
  #             #            textfont = list(size = 10),
  #             #            textposition = "top",
  #             #            showlegend = FALSE)
  #           } else {
  #             p <- add_trace(
  #               p,
  #               x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
  #               y = plot_data[as.character(plot_data$AUDIT) == i, "Value"] / 1000000,
  #               type = "scatter",
  #               mode = "lines+markers",
  #               colors = "Set1",
  #               name = i
  #             )
  #             # %>%
  #             #   add_text(x = plot_data[as.character(plot_data$AUDIT) == i, "date"],
  #             #            y = plot_data[as.character(plot_data$AUDIT) == i, "value"]  / 1000000,
  #             #            text = round(plot_data[as.character(plot_data$AUDIT) == i, "value"]  / 1000000, 0),
  #             #            textfont = list(size = 10),
  #             #            textposition = "top",
  #             #            showlegend = FALSE)
  #           }
  #           
  #         }
  #         
  #         p %>%
  #           config(
  #             displaylogo = FALSE,
  #             collaborate = FALSE,
  #             modeBarButtonsToRemove = list(
  #               'sendDataToCloud',
  #               'autoScale2d',
  #               'zoom2d',
  #               'pan2d',
  #               'select2d',
  #               'lasso2d',
  #               'toggleSpikelines'
  #             )
  #           ) %>%
  #           layout(
  #             title='Players Trend Performance',
  #             xaxis = list(
  #               zeroline = FALSE,
  #               title = "",
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks",
  #               tickvals = tickval,
  #               tickformat = "%y%m",
  #               type = "date"
  #             ),
  #             yaxis = list(
  #               zeroline = FALSE,
  #               title = paste(toupper(input$sub_measure), 
  #                             "(", 
  #                             ifelse(input$sub_index == "abs",
  #                                    "Million", toupper(input$sub_index)),
  #                             ")",
  #                             sep = ""),
  #               # "Measure + (Million)",
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks"
  #             ),
  #             legend = list(x = 0.5,
  #                           xanchor = "center",
  #                           # y = 100,
  #                           orientation = "h")
  #           )
  #         
  #       } else {
  #         p <- plot_ly(hoverinfo = "x+y+name")
  #         for (i in as.character(unique(plot_data$AUDIT))) {
  #           if (grepl("B.I$", i)) {
  #             p <- add_trace(
  #               p,
  #               x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
  #               y = plot_data[as.character(plot_data$AUDIT) == i, "Value"],
  #               type = "scatter",
  #               mode = "lines+markers",
  #               color = list("blue"),
  #               colors = "Set1",
  #               name = i
  #             )
  #           } else {
  #             p <- add_trace(
  #               p,
  #               x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
  #               y = plot_data[as.character(plot_data$AUDIT) == i, "Value"],
  #               type = "scatter",
  #               mode = "lines+markers",
  #               colors = "Set1",
  #               name = i
  #             )
  #           }
  #           
  #         }
  #         
  #         p %>%
  #           config(
  #             displaylogo = FALSE,
  #             collaborate = FALSE,
  #             modeBarButtonsToRemove = list(
  #               'sendDataToCloud',
  #               'autoScale2d',
  #               'zoom2d',
  #               'pan2d',
  #               'select2d',
  #               'lasso2d',
  #               'toggleSpikelines'
  #             )
  #           ) %>%
  #           layout(
  #             title='Players Trend Performance',
  #             xaxis = list(
  #               zeroline = FALSE,
  #               title = "",
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks",
  #               tickvals = tickval,
  #               tickformat = "%y%m",
  #               type = "date"
  #             ),
  #             yaxis = list(
  #               zeroline = FALSE,
  #               title = paste(toupper(input$sub_measure), 
  #                             "(", 
  #                             ifelse(input$sub_index == "abs",
  #                                    "Million", toupper(input$sub_index)),
  #                             ")",
  #                             sep = ""), #"Measurement",
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks"
  #             ),
  #             legend = list(x = 0.5,
  #                           xanchor = "center",
  #                           # y = 100,
  #                           orientation = "h")
  #           )
  #       }
  #     }
  #   }
  #   
  #   
  #   
  #   
  # })
  # 
  # output$chart <- 
  #   renderPlotly(if (is.null(product_plot())) {
  #     plotly_empty()} else {
  #       product_plot()})
  # 
  # 
  # product_bar_plot <- reactive({
  #   if (input$goButton == 0) {
  #     return(NULL)
  #   }
  #   
  #   if ("RENMINBI"  %in% input$value) {
  #     if ("ALL" %in% input$province) {
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region,  province())),]
  #     } else {
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, input$province)),]
  #     }
  #     
  #     if ("Out hospital" ==  input$category && length(input$category) == 1) { # input$category == "Out hospital"
  #       summary <- summary%>%
  #         mutate(CORPORATE.DESC =
  #                  ifelse(PRODUCT.DESC %in% c("ATROVENT B.I", "COMBIVENT B.I"),
  #                         "999", CORPORATE.DESC))
  #     } 
  #     
  #     # summary <- summary %>%
  #     #   mutate(CORPORATE.DESC =
  #     #            ifelse(PRODUCT.DESC %in% c("ATROVENT B.I", "COMBIVENT B.I"),
  #     #                   "999", CORPORATE.DESC))
  # 
  #   
  #     rmb <- rbind.fill(
  #       graph1(
  #         summary,
  #         cate = input$category,
  #         subcate = input$subcategory,
  #         value = "RENMINBI",
  #         period = input$period,
  #         kpi = c("abs", "gr"),
  #         window = as.numeric(input$window),
  #         level = "corporation"
  #       ),
  #       graph1(
  #         summary,
  #         cate = input$category,
  #         subcate = input$subcategory,
  #         value = "RENMINBI",
  #         period = input$period,
  #         kpi = c("abs", "gr"),
  #         window = as.numeric(input$window),
  #         level = "molecule"
  #       )
  #     )
  #     rmb <- distinct(rmb)
  #     
  #   } else{
  #     rmb <- NULL
  #   }
  #   
  #   
  #   if ("UNIT" %in% input$value) {
  #     if ("ALL" %in% input$province) {
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region,  province())),]
  #     } else{
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, input$province)),]
  #     }
  #     summary <- summary %>%
  #       mutate(CORPORATE.DESC =
  #                ifelse(PRODUCT.DESC %in% c("ATROVENT B.I", "COMBIVENT B.I"),
  #                       "999", CORPORATE.DESC))
  #     
  #     unit <- rbind.fill(
  #       graph1(
  #         summary,
  #         cate = input$category,
  #         subcate = input$subcategory,
  #         value = "UNIT",
  #         period = input$period,
  #         kpi = c("abs", "gr"),
  #         window = as.numeric(input$window),
  #         level = "corporation"
  #       ),
  #       graph1(
  #         summary,
  #         cate = input$category,
  #         subcate = input$subcategory,
  #         value = "UNIT",
  #         period = input$period,
  #         kpi = c("abs", "gr"),
  #         window = as.numeric(input$window),
  #         level = "molecule"
  #       )
  #     )
  #     unit <- distinct(unit)
  #     
  #   } else{
  #     unit <- NULL
  #   }
  #   
  #   if ("DOT"  %in% input$value) {
  #     if ("ALL" %in% input$province) {
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region,  province())),]
  #     } else{
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, input$province)),]
  #     }
  #     summary <- summary %>%
  #       mutate(CORPORATE.DESC =
  #                ifelse(PRODUCT.DESC %in% c("ATROVENT B.I", "COMBIVENT B.I"),
  #                       "999", CORPORATE.DESC))
  #     
  #     dot <- rbind.fill(
  #       graph1(
  #         summary,
  #         cate = input$category,
  #         subcate = input$subcategory,
  #         value = "DOT",
  #         period = input$period,
  #         kpi = c("abs", "gr"),
  #         window = as.numeric(input$window),
  #         level = "corporation"
  #       ),
  #       graph1(
  #         summary,
  #         cate = input$category,
  #         subcate = input$subcategory,
  #         value = "DOT",
  #         period = input$period,
  #         kpi = c("abs", "gr"),
  #         window = as.numeric(input$window),
  #         level = "molecule"
  #       )
  #     )
  #     dot <- distinct(dot)
  #   } else{
  #     dot <- NULL
  #   }
  #   
  #   result1 <- rbind(rmb, unit, dot)
  #   
  #   
  #   
  #   result1$Measure[which(result1$Measure == "RENMINBI")] <- "RMB"
  #   
  #   if (is.null(input$sub_measure) || is.null(input$sub_region)) {
  #     return(NULL)
  #   } else {
  #     test <- result1 %>%
  #       filter(Measure %in% input$sub_measure) %>%
  #       gather(key = "date", value = "value", -c(AUDIT.DESC:Index)) %>%
  #       spread(Index, value) %>%
  #       filter(PRODUCT.DESC == "Overall", COMPS.DESC == "Overall",
  #              (CORPORATE.DESC == "B.INGELHEIM" |
  #                 CORPORATE.DESC == "Overall")) %>% 
  #       select(AUDIT.DESC, CORPORATE.DESC, Measure, date, ABS, GR = `GR%`) %>%
  #       # select(AUDIT.DESC, CORPORATE.DESC, Measure, date) %>%
  #       # # rename(GR = `GR%`) %>%
  #       # mutate(ABS = 1, GR = 1) %>%
  #       filter(AUDIT.DESC %in% input$sub_region)
  #   }
  #   
  #   
  #   
  #   # test <- outputtable %>% filter(AUDIT.DESC %in% input$sub_region) %>%
  #   #   select(AUDIT.DESC, CORPORATE.DESC, Measure, date, ABS, GR = `GR%`)
  #   # test <- test[, c(1, 4, 15, 16, 17, 18)]
  #   # colnames(test)[5] <- "ABS"
  #   # colnames(test)[6] <- "GR"
  #   # colnames(test)[which(colnames(test) == "GR%")] <- "GR"
  #   test1 <- setDT(test)
  #   test1 <- data.table::dcast(test1,
  #                              AUDIT.DESC +  Measure + date ~ CORPORATE.DESC,
  #                              value.var = c("ABS", "GR"))
  #   test1 <- setDF(test1) %>%
  #     mutate(date = substr(date, 3, 6))
  #   # tickval <- unlist(distinct(test1, Date))
  #   
  #   if (!("GR_B.INGELHEIM" %in% colnames(test1))) {
  #     test1 <- test1 %>% mutate(GR_B.INGELHEIM = NA)
  #   }
  #   
  #   if (all(is.na(test1$GR_B.INGELHEIM))) {
  #     if (input$label_p) {
  #       p0_overall <-
  #         plot_ly(test1) %>%
  #         add_trace(x = ~date,
  #                   y = ~ABS_Overall / 1000000,
  #                   type = "bar",
  #                   marker = list(color = '#D2D2D2'),
  #                   name = "TTL MKT Val",
  #                   width = 0.5) %>%
  #         add_trace(x = ~date,
  #                   y = ~GR_Overall,
  #                   type = "scatter",
  #                   mode = "lines+markers",
  #                   # color = ~factor(1),
  #                   line = list(color = 'red'),
  #                   # colors = "red",
  #                   yaxis = "y2",
  #                   name = "TTL_MKT_GR(%)") %>%
  #         layout(
  #           # legend = list(x = 0.5,
  #           #               xanchor = "center",
  #           #               y = 100,
  #           #               orientation = "h"),
  #           # autosize = T
  #           # ,
  #           margin =  list(
  #             # l = 50,
  #             r = 50
  #             # ,
  #             # b = 50,
  #             # t = 50,
  #             # pad = 4
  #           )
  #         ) %>%
  #         add_annotations(x = ~date,
  #                         y = ~ABS_Overall / 1000000 + 0.05 * ABS_Overall / 1000000,
  #                         text = ~format(round(ABS_Overall / 1000000, 2), big.mark = ","),
  #                         yref = "y",
  #                         xanchor = 'center',
  #                         yanchor = 'center',
  #                         showarrow = FALSE) %>%
  #         add_annotations(x = ~date[-1],
  #                         y = ~GR_Overall[-1] - 0.5,
  #                         # y = ~c(2, 2, 18),
  #                         text = ~paste(round(GR_Overall[-1], 2), "%", ""),
  #                         yref = "y2",
  #                         xanchor = 'center',
  #                         yanchor = 'center',
  #                         showarrow = FALSE) %>%
  #         add_annotations(x = ~date[-1],
  #                         y = ~GR_B.INGELHEIM[-1] + 0.5,
  #                         # y = ~c(2, 2, 18),
  #                         text = ~paste(round(GR_B.INGELHEIM[-1], 2), "%", ""),
  #                         yref = "y2",
  #                         xanchor = 'center',
  #                         yanchor = 'center',
  #                         showarrow = FALSE) %>%
  #         config(
  #           displaylogo = FALSE,
  #           collaborate = FALSE,
  #           modeBarButtonsToRemove = list(
  #             'sendDataToCloud',
  #             'autoScale2d',
  #             'zoom2d',
  #             'pan2d',
  #             'select2d',
  #             'lasso2d',
  #             'toggleSpikelines'
  #           )
  #         ) %>%
  #         layout(
  #           title='Market & BI GR% Performance',
  #           xaxis = list(
  #             zeroline = FALSE,
  #             title = "",
  #             showline = TRUE,
  #             showgrid = FALSE,
  #             mirror = "ticks"
  #             # ,
  #             # tickvals = tickval,
  #             # tickformat = "%y%m",
  #             # type = "date"
  #             
  #           ),
  #           yaxis = list(
  #             zeroline = FALSE,
  #             title = #paste(input$sub_measure, " + ",
  #               #     input$sub_index, "(Million)",
  #               #    sep = ""),
  #               "Measure + (Million)",
  #             showline = TRUE,
  #             showgrid = FALSE,
  #             mirror = "ticks",
  #             showticklabels = TRUE
  #           ),
  #           yaxis2 = list(title = "",
  #                         overlaying = "y",
  #                         ticksuffix = "%",
  #                         side = "right",
  #                         fill = "tozeroy",
  #                         zeroline = FALSE,
  #                         showgrid = FALSE,
  #                         showticklabels = TRUE),
  #           legend = list(x = 0.5,
  #                         xanchor = "center",
  #                         # y = 100,
  #                         orientation = "h")
  #         )
  #     } else {
  #       p0_overall <-
  #         plot_ly(test1) %>%
  #         add_trace(x = ~date,
  #                   y = ~ABS_Overall / 1000000,
  #                   type = "bar",
  #                   marker = list(color = '#D2D2D2'),
  #                   name = "TTL MKT Val",
  #                   width = 0.5) %>%
  #         add_trace(x = ~date,
  #                   y = ~GR_Overall,
  #                   type = "scatter",
  #                   mode = "lines+markers",
  #                   # color = ~factor(1),
  #                   line = list(color = 'red'),
  #                   # colors = "red",
  #                   yaxis = "y2",
  #                   name = "TTL_MKT_GR(%)") %>%
  #         layout(
  #           # legend = list(x = 0.5,
  #           #               xanchor = "center",
  #           #               y = 100,
  #           #               orientation = "h"),
  #           autosize = T
  #           ,
  #           margin =  list(
  #             # l = 50,
  #             r = 50
  #             # ,
  #             # b = 50,
  #             # t = 50,
  #             # pad = 4
  #           )
  #         ) %>%
  #         config(
  #           displaylogo = FALSE,
  #           collaborate = FALSE,
  #           modeBarButtonsToRemove = list(
  #             'sendDataToCloud',
  #             'autoScale2d',
  #             'zoom2d',
  #             'pan2d',
  #             'select2d',
  #             'lasso2d',
  #             'toggleSpikelines'
  #           )
  #         )  %>%
  #         layout(
  #           title='Market & BI GR% Performance',
  #           xaxis = list(
  #             zeroline = FALSE,
  #             title = "",
  #             showline = TRUE,
  #             showgrid = FALSE,
  #             mirror = "ticks"
  #             # ,
  #             # tickvals = tickval,
  #             # tickformat = "%y%m",
  #             # type = "date"
  #             
  #           ),
  #           yaxis = list(
  #             zeroline = FALSE,
  #             title = #paste(input$sub_measure, " + ",
  #               #      input$sub_index, "(Million)",
  #               #     sep = ""),
  #               "RMB(Million)",
  #             showline = TRUE,
  #             showgrid = FALSE,
  #             mirror = "ticks",
  #             showticklabels = TRUE
  #           ),
  #           yaxis2 = list(title = "",
  #                         overlaying = "y",
  #                         ticksuffix = "%",
  #                         side = "right",
  #                         fill = "tozeroy",
  #                         zeroline = FALSE,
  #                         showgrid = FALSE,
  #                         showticklabels = TRUE),
  #           legend = list(x = 0.5,
  #                         xanchor = "center",
  #                         # y = -1,
  #                         orientation = "h")
  #         )
  #       
  #     }
  #   } else {
  #     if (input$label_p) {
  #       p0_overall <-
  #         plot_ly(test1) %>%
  #         add_trace(x = ~date,
  #                   y = ~ABS_Overall / 1000000,
  #                   type = "bar",
  #                   marker = list(color = '#D2D2D2'),
  #                   name = "TTL MKT Val",
  #                   width = 0.5) %>%
  #         add_trace(x = ~date,
  #                   y = ~GR_Overall,
  #                   type = "scatter",
  #                   mode = "lines+markers",
  #                   # color = ~factor(1),
  #                   line = list(color = 'red'),
  #                   # colors = "red",
  #                   yaxis = "y2",
  #                   name = "TTL_MKT_GR(%)") %>%
  #         add_trace(x = ~date,
  #                   y = ~GR_B.INGELHEIM,
  #                   type = "scatter",
  #                   mode = "lines+markers",
  #                   # color = ~factor(2),
  #                   line = list(color = 'blue'),
  #                   # colors = "blue",
  #                   yaxis = "y2",
  #                   name = "TTL_BI_GR(%)") %>%
  #         layout(
  #           # legend = list(x = 0.5,
  #           #               xanchor = "center",
  #           #               y = 100,
  #           #               orientation = "h"),
  #           # autosize = T
  #           # ,
  #           margin =  list(
  #             # l = 50,
  #             r = 50
  #             # ,
  #             # b = 50,
  #             # t = 50,
  #             # pad = 4
  #           )
  #         ) %>%
  #         add_annotations(x = ~date,
  #                         y = ~ABS_Overall / 1000000 + 0.05 * ABS_Overall / 1000000,
  #                         text = ~format(round(ABS_Overall / 1000000, 2), big.mark = ","),
  #                         yref = "y",
  #                         xanchor = 'center',
  #                         yanchor = 'center',
  #                         showarrow = FALSE) %>%
  #         add_annotations(x = ~date[-1],
  #                         y = ~GR_Overall[-1] - 0.5,
  #                         # y = ~c(2, 2, 18),
  #                         text = ~paste(round(GR_Overall[-1], 2), "%", ""),
  #                         yref = "y2",
  #                         xanchor = 'center',
  #                         yanchor = 'center',
  #                         showarrow = FALSE) %>%
  #         add_annotations(x = ~date[-1],
  #                         y = ~GR_B.INGELHEIM[-1] + 0.5,
  #                         # y = ~c(2, 2, 18),
  #                         text = ~paste(round(GR_B.INGELHEIM[-1], 2), "%", ""),
  #                         yref = "y2",
  #                         xanchor = 'center',
  #                         yanchor = 'center',
  #                         showarrow = FALSE) %>%
  #         config(
  #           displaylogo = FALSE,
  #           collaborate = FALSE,
  #           modeBarButtonsToRemove = list(
  #             'sendDataToCloud',
  #             'autoScale2d',
  #             'zoom2d',
  #             'pan2d',
  #             'select2d',
  #             'lasso2d',
  #             'toggleSpikelines'
  #           )
  #         ) %>%
  #         layout(
  #           title='Market & BI GR% Performance',
  #           xaxis = list(
  #             zeroline = FALSE,
  #             title = "",
  #             showline = TRUE,
  #             showgrid = FALSE,
  #             mirror = "ticks"
  #             # ,
  #             # tickvals = tickval,
  #             # tickformat = "%y%m",
  #             # type = "date"
  #             
  #           ),
  #           yaxis = list(
  #             zeroline = FALSE,
  #             title = #paste(input$sub_measure, " + ",
  #               #     input$sub_index, "(Million)",
  #               #     sep = ""),
  #               "RMB(Million)",
  #             showline = TRUE,
  #             showgrid = FALSE,
  #             mirror = "ticks",
  #             showticklabels = TRUE
  #           ),
  #           yaxis2 = list(title = "",
  #                         overlaying = "y",
  #                         ticksuffix = "%",
  #                         side = "right",
  #                         fill = "tozeroy",
  #                         zeroline = FALSE,
  #                         showgrid = FALSE,
  #                         showticklabels = TRUE),
  #           legend = list(x = 0.5,
  #                         xanchor = "center",
  #                         # y = 100,
  #                         orientation = "h")
  #         )
  #     } else {
  #       p0_overall <-
  #         plot_ly(test1) %>%
  #         add_trace(x = ~date,
  #                   y = ~ABS_Overall / 1000000,
  #                   type = "bar",
  #                   marker = list(color = '#D2D2D2'),
  #                   name = "TTL MKT Val",
  #                   width = 0.5) %>%
  #         add_trace(x = ~date,
  #                   y = ~GR_Overall,
  #                   type = "scatter",
  #                   mode = "lines+markers",
  #                   # color = ~factor(1),
  #                   line = list(color = 'red'),
  #                   # colors = "red",
  #                   yaxis = "y2",
  #                   name = "TTL_MKT_GR(%)") %>%
  #         add_trace(x = ~date,
  #                   y = ~GR_B.INGELHEIM,
  #                   type = "scatter",
  #                   mode = "lines+markers",
  #                   # color = ~factor(2),
  #                   line = list(color = 'blue'),
  #                   # colors = "blue",
  #                   yaxis = "y2",
  #                   name = "TTL_BI_GR(%)") %>%
  #         layout(
  #           # legend = list(x = 0.5,
  #           #               xanchor = "center",
  #           #               y = 100,
  #           #               orientation = "h"),
  #           autosize = T
  #           ,
  #           margin =  list(
  #             # l = 50,
  #             r = 50
  #             # ,
  #             # b = 50,
  #             # t = 50,
  #             # pad = 4
  #           )
  #         ) %>%
  #         config(
  #           displaylogo = FALSE,
  #           collaborate = FALSE,
  #           modeBarButtonsToRemove = list(
  #             'sendDataToCloud',
  #             'autoScale2d',
  #             'zoom2d',
  #             'pan2d',
  #             'select2d',
  #             'lasso2d',
  #             'toggleSpikelines'
  #           )
  #         )  %>%
  #         layout(
  #           title='Market & BI GR% Performance',
  #           xaxis = list(
  #             zeroline = FALSE,
  #             title = "",
  #             showline = TRUE,
  #             showgrid = FALSE,
  #             mirror = "ticks"
  #             # ,
  #             # tickvals = tickval,
  #             # tickformat = "%y%m",
  #             # type = "date"
  #             
  #           ),
  #           yaxis = list(
  #             zeroline = FALSE,
  #             title = #paste(input$sub_measure, " + ",
  #               #      input$sub_index, "(Million)",
  #               #      sep = ""),
  #               "RMB(Million)",
  #             showline = TRUE,
  #             showgrid = FALSE,
  #             mirror = "ticks",
  #             showticklabels = TRUE
  #           ),
  #           yaxis2 = list(title = "",
  #                         overlaying = "y",
  #                         ticksuffix = "%",
  #                         side = "right",
  #                         fill = "tozeroy",
  #                         zeroline = FALSE,
  #                         showgrid = FALSE,
  #                         showticklabels = TRUE),
  #           legend = list(x = 0.5,
  #                         xanchor = "center",
  #                         # y = -1,
  #                         orientation = "h")
  #         )
  #       
  #     }
  #   }
  #   return(p0_overall)
  # })
  # 
  # output$bar_chart <- 
  #   renderPlotly(if (is.null(product_bar_plot()) | input$rpp_p) {
  #     plotly_empty() } else {
  #       product_bar_plot()})
  # 
  # 
  # output$downloadPlot_p <- downloadHandler(
  #   filename = function() {
  #     paste("Product_Trend_plot_",
  #           input$period,
  #           "_",
  #           input$window,
  #           "year",
  #           '.png',
  #           sep = '')
  #   },
  #   content = function(file) {
  #     export(product_plot(), file = file)
  #   }
  # )
  # 
  # output$downloadPlot_p1 <- downloadHandler(
  #   filename = function() {
  #     paste("Product_bar_plot_",
  #           input$period,
  #           "_",
  #           input$window,
  #           "year",
  #           '.png',
  #           sep = '')
  #   },
  #   content = function(file) {
  #     export(product_bar_plot(), file = file)
  #   }
  # )
  # 
  # corporation_plot <- reactive({
  #   if (input$goButton == 0) {
  #     return(NULL)
  #   }
  #   # isolate({
  #   if (is.null(ot()))
  #     return(NULL)
  #   
  #   ot <- ot()$result3_c
  #   ot <- filter(
  #     ot,
  #     (CORPORATE.DESC != "Overall") |
  #       (CORPORATE.DESC == "Overall" &
  #          COMPS.DESC == "Overall")
  #   )
  #   
  #   if ("ALL" %in% input$province) {
  #     summary <-
  #       summary()[which(summary()$AUDIT.DESC %in% c("China", input$region,  province())),]
  #   } else {
  #     summary <-
  #       summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, input$province)),]
  #   }
  #   
  #   
  #   top_brand <- graph1m(
  #     summary,
  #     cate = input$category,
  #     subcate = input$subcategory,
  #     value = "RENMINBI",
  #     period = input$period,
  #     kpi = "abs",
  #     window = 1,
  #     level = "corporation",
  #     top = as.integer(input$top2)
  #   ) %>%
  #     select(AUDIT.DESC, PRODUCT.DESC, CORPORATE.DESC)
  #   
  #   ot <- ot %>% inner_join(top_brand)
  #   
  #   ot$AUDIT.DESC <- gsub(" .*$", "", ot$AUDIT.DESC)
  #   
  #   
  #   ot$PRODUCT.DESC <- as.character(ot$PRODUCT.DESC)
  #   ot$CORPORATE.DESC <- as.character(ot$CORPORATE.DESC)
  #   
  #   ot$PRODUCT.DESC[which(ot$CORPORATE.DESC == "Overall")] <-
  #     ot$AUDIT.DESC[which(ot$CORPORATE.DESC == "Overall")]
  #   KK <- which(ot$PRODUCT.DESC == "Overall")
  #   
  #   ot$PRODUCT.DESC[KK] <- ot$CORPORATE.DESC[KK]
  #   ot$CORPORATE.DESC[KK] <- "Company"
  #   
  #   names(ot)[names(ot) == 'AUDIT.DESC'] <- 'AUDIT'
  #   names(ot)[names(ot) == 'CORPORATE.DESC'] <- 'CORPORATE'
  #   names(ot)[names(ot) == 'PRODUCT.DESC'] <- 'PRODUCT'
  #   
  #   ot$PRODUCT <- as.factor(ot$PRODUCT)
  #   ot$CORPORATE <- as.factor(ot$CORPORATE)
  #   ot$AUDIT <- as.factor(ot$AUDIT)
  #   
  #   ot <- as.data.frame(ot)
  #   otnum <- which(grepl("Index", names(ot)))
  #   #number here need to be updated for new update
  #   ot[which(ot$Index == "ABS"), (otnum + 1):length(ot)] <-
  #     format(ot[which(ot$Index == "ABS"), (otnum + 1):length(ot)],
  #            big.mark = ",", scientific = FALSE)
  #   ot$Measure[which(ot$Measure == "RENMINBI")] <- "RMB"
  #   
  #   if ("ms" %in% input$sub_index_c) {
  #     tmp_sub_index <- "MS%"
  #   } else if ("gr" %in% input$sub_index_c) {
  #     tmp_sub_index <- "GR%"
  #   } else if ("mc" %in% input$sub_index_c) {
  #     tmp_sub_index <- "MS+/- %"
  #   } else {
  #     tmp_sub_index <- input$sub_index_c
  #   }
  #   
  #   if (as.integer(input$top) > 0) {
  #     plot_data <- ot %>%
  #       filter(
  #         Measure %in% input$sub_measure_c,
  #         Index %in%  toupper(tmp_sub_index),
  #         AUDIT %in% gsub(" .*$", "", input$sub_region_c),
  #         # as.character(CORPORATE) %in% c("Company", "Overall"),
  #         # (as.character(PRODUCT) == as.character(AUDIT) |
  #         as.character(PRODUCT) %in% input$sub_top_c
  #         # )
  #       ) %>%
  #       # toplist_c()[1:(1 + as.integer(input$sub_top_c))])) %>%
  #       gather(
  #         Date,
  #         Value,
  #         -AUDIT,
  #         -Category,
  #         -Sub.category,
  #         -CORPORATE,
  #         -PRODUCT,-COMPS.DESC,
  #         -pro.code,
  #         -MANUF.TYPE.DESC,
  #         -TC.III.SHORT.DESC,-TC.III.DESC,
  #         -Category_CN,
  #         -COMPS.DESC,
  #         -Brand_CN,
  #         -MANU_CN,-Measure,
  #         -Index
  #       ) %>%
  #       mutate(
  #         Date = as.Date(ymd(paste(
  #           Date, "01", "."
  #         ))),
  #         Value = as.numeric(gsub(",", "", Value)),
  #         Date1 = substr(as.character(Date), 1, 7)
  #       )
  #   } else {
  #     plot_data <- ot %>%
  #       filter(
  #         Measure %in% input$sub_measure_c,
  #         Index %in%  toupper(tmp_sub_index),
  #         # AUDIT %in% gsub(" .*$", "", input$sub_region_c),
  #         # as.character(CORPORATE) %in% c("Company", "Overall"),
  #         # (
  #         as.character(PRODUCT) == as.character(AUDIT)
  #         # |
  #         # as.character(PRODUCT) %in% input$sub_top_c
  #         # )
  #       ) %>%
  #       # toplist_c()[1:(1 + as.integer(input$sub_top_c))])) %>%
  #       gather(
  #         Date,
  #         Value,
  #         -AUDIT,
  #         -Category,
  #         -Sub.category,
  #         -CORPORATE,
  #         -PRODUCT,-COMPS.DESC,
  #         -pro.code,
  #         -MANUF.TYPE.DESC,
  #         -TC.III.SHORT.DESC,-TC.III.DESC,
  #         -Category_CN,
  #         -COMPS.DESC,
  #         -Brand_CN,
  #         -MANU_CN,-Measure,
  #         -Index
  #       ) %>%
  #       mutate(
  #         Date = as.Date(ymd(paste(
  #           Date, "01", "."
  #         ))),
  #         Value = as.numeric(gsub(",", "", Value)),
  #         Date1 = substr(as.character(Date), 1, 7)
  #       )
  #   }
  #   
  #   tickval <- unlist(distinct(plot_data, Date1))
  #   
  #   
  #   if ((as.integer(input$top) > 0 && input$bp_p_c) ||
  #       as.numeric(input$top) == 0) {
  #     if (input$label_c) {
  #       if (c("abs") %in% input$sub_index_c) {
  #         p <- plot_ly(hoverinfo = "x+y+name")
  #         for (i in as.character(unique(plot_data$PRODUCT))) {
  #           p <- add_trace(
  #             p,
  #             x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
  #             y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"] / 1000000,
  #             type = "scatter",
  #             mode = "lines+markers",
  #             colors = "Set1",
  #             name = i
  #           ) %>%
  #             add_text(
  #               x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
  #               y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"]  / 1000000,
  #               text = round(plot_data[as.character(plot_data$PRODUCT) == i, "Value"]  / 1000000, 2),
  #               textfont = list(size = 10),
  #               textposition = "top",
  #               hoverinfo = "none",
  #               showlegend = FALSE
  #             )
  #         }
  #         
  #         p %>%
  #           config(
  #             displaylogo = FALSE,
  #             collaborate = FALSE,
  #             modeBarButtonsToRemove = list(
  #               'sendDataToCloud',
  #               'autoScale2d',
  #               'zoom2d',
  #               'pan2d',
  #               'select2d',
  #               'lasso2d',
  #               'toggleSpikelines'
  #             )
  #           ) %>%
  #           layout(
  #             title='Players Trend Performance',
  #             xaxis = list(
  #               zeroline = FALSE,
  #               title = "",
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks",
  #               tickvals = tickval,
  #               tickformat = "%y%m",
  #               type = "date"
  #             ),
  #             yaxis = list(
  #               zeroline = FALSE,
  #               title = "Measure + (Million)",
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks"
  #             ),
  #             legend = list(x = 0.5,
  #                           xanchor = "center",
  #                           # y = 100,
  #                           orientation = "h")
  #           )
  #       } else {
  #         p <- plot_ly(hoverinfo = "x+y+name")
  #         for (i in as.character(unique(plot_data$PRODUCT))) {
  #           p <- add_trace(
  #             p,
  #             x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
  #             y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"],
  #             type = "scatter",
  #             mode = "lines+markers",
  #             colors = "Set1",
  #             name = i
  #           ) %>%
  #             add_text(
  #               x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
  #               y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"],
  #               text = round(plot_data[as.character(plot_data$PRODUCT) == i, "Value"], 2),
  #               textfont = list(size = 10),
  #               textposition = "top",
  #               hoverinfo = "none",
  #               showlegend = FALSE
  #             )
  #         }
  #         
  #         p %>%
  #           config(
  #             displaylogo = FALSE,
  #             collaborate = FALSE,
  #             modeBarButtonsToRemove = list(
  #               'sendDataToCloud',
  #               'autoScale2d',
  #               'zoom2d',
  #               'pan2d',
  #               'select2d',
  #               'lasso2d',
  #               'toggleSpikelines'
  #             )
  #           ) %>%
  #           layout(
  #             title='Players Trend Performance',
  #             xaxis = list(
  #               zeroline = FALSE,
  #               title = "",
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks",
  #               tickvals = tickval,
  #               tickformat = "%y%m",
  #               type = "date"
  #             ),
  #             yaxis = list(
  #               zeroline = FALSE,
  #               title = paste(toupper(input$sub_measure_c), 
  #                             "(", 
  #                             ifelse(input$sub_index_c == "abs",
  #                                    "Million", toupper(input$sub_index_c)),
  #                             ")",
  #                             sep = ""),#"<b>Value</b>",
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks"
  #             ),
  #             legend = list(x = 0.5,
  #                           xanchor = "center",
  #                           # y = 100,
  #                           orientation = "h")
  #           )
  #       }
  #       
  #     } else {
  #       if (c("abs") %in% input$sub_index_c) {
  #         p <- plot_ly(hoverinfo = "x+y+name")
  #         for (i in as.character(unique(plot_data$PRODUCT))) {
  #           p <- add_trace(
  #             p,
  #             x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
  #             y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"] / 1000000,
  #             type = "scatter",
  #             mode = "lines+markers",
  #             colors = "Set1",
  #             name = i
  #           )
  #           # %>%
  #           #   add_text(x = plot_data[as.character(plot_data$PRODUCT) == i, "date"],
  #           #            y = plot_data[as.character(plot_data$PRODUCT) == i, "value"]  / 1000000,
  #           #            text = round(plot_data[as.character(plot_data$PRODUCT) == i, "value"]  / 1000000, 0),
  #           #            textfont = list(size = 10),
  #           #            textposition = "top",
  #           #            showlegend = FALSE)
  #           
  #         }
  #         
  #         p %>%
  #           config(
  #             displaylogo = FALSE,
  #             collaborate = FALSE,
  #             modeBarButtonsToRemove = list(
  #               'sendDataToCloud',
  #               'autoScale2d',
  #               'zoom2d',
  #               'pan2d',
  #               'select2d',
  #               'lasso2d',
  #               'toggleSpikelines'
  #             )
  #           ) %>%
  #           layout(
  #             title='Players Trend Performance',
  #             xaxis = list(
  #               zeroline = FALSE,
  #               title = "",
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks",
  #               tickvals = tickval,
  #               tickformat = "%y%m",
  #               type = "date"
  #             ),
  #             yaxis = list(
  #               zeroline = FALSE,
  #               title = paste(toupper(input$sub_measure_c), 
  #                             "(", 
  #                             ifelse(input$sub_index_c == "abs",
  #                                    "Million", toupper(input$sub_index_c)),
  #                             ")",
  #                             sep = ""),
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks"
  #             ),
  #             legend = list(x = 0.5,
  #                           xanchor = "center",
  #                           # y = 100,
  #                           orientation = "h")
  #           )
  #         
  #       } else {
  #         p <- plot_ly(hoverinfo = "x+y+name")
  #         for (i in as.character(unique(plot_data$PRODUCT))) {
  #           p <- add_trace(
  #             p,
  #             x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
  #             y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"],
  #             type = "scatter",
  #             mode = "lines+markers",
  #             colors = "Set1",
  #             name = i
  #           )
  #         }
  #         
  #         p %>%
  #           config(
  #             displaylogo = FALSE,
  #             collaborate = FALSE,
  #             modeBarButtonsToRemove = list(
  #               'sendDataToCloud',
  #               'autoScale2d',
  #               'zoom2d',
  #               'pan2d',
  #               'select2d',
  #               'lasso2d',
  #               'toggleSpikelines'
  #             )
  #           ) %>%
  #           layout(
  #             title='Players Trend Performance',
  #             xaxis = list(
  #               zeroline = FALSE,
  #               title = "",
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks",
  #               tickvals = tickval,
  #               tickformat = "%y%m",
  #               type = "date"
  #             ),
  #             yaxis = list(
  #               zeroline = FALSE,
  #               title =paste(toupper(input$sub_measure_c), 
  #                            "(", 
  #                            ifelse(input$sub_index_c == "abs",
  #                                   "Million", toupper(input$sub_index_c)),
  #                            ")",
  #                            sep = ""),#"Measurement",
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks"
  #             ),
  #             legend = list(x = 0.5,
  #                           xanchor = "center",
  #                           # y = 100,
  #                           orientation = "h")
  #           )
  #       }
  #     }
  #   } else {
  #     if (input$label_c) {
  #       if (c("abs") %in% input$sub_index_c) {
  #         p <- plot_ly(hoverinfo = "x+y+name")
  #         for (i in as.character(unique(plot_data$AUDIT))) {
  #           
  #           p <- add_trace(
  #             p,
  #             x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
  #             y = plot_data[as.character(plot_data$AUDIT) == i, "Value"] / 1000000,
  #             type = "scatter",
  #             mode = "lines+markers",
  #             colors = "Set1",
  #             name = i
  #           ) %>%
  #             add_text(
  #               x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
  #               y = plot_data[as.character(plot_data$AUDIT) == i, "Value"]  / 1000000,
  #               text = round(plot_data[as.character(plot_data$AUDIT) == i, "Value"]  / 1000000, 2),
  #               textfont = list(size = 10),
  #               textposition = "top",
  #               hoverinfo = "none",
  #               showlegend = FALSE
  #             )
  #           
  #           
  #         }
  #         
  #         p %>%
  #           config(
  #             displaylogo = FALSE,
  #             collaborate = FALSE,
  #             modeBarButtonsToRemove = list(
  #               'sendDataToCloud',
  #               'autoScale2d',
  #               'zoom2d',
  #               'pan2d',
  #               'select2d',
  #               'lasso2d',
  #               'toggleSpikelines'
  #             )
  #           ) %>%
  #           layout(
  #             title='Players Trend Performance',
  #             xaxis = list(
  #               zeroline = FALSE,
  #               title = "",
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks",
  #               tickvals = tickval,
  #               tickformat = "%y%m",
  #               type = "date"
  #             ),
  #             yaxis = list(
  #               zeroline = FALSE,
  #               title = paste(toupper(input$sub_measure_c), 
  #                             "(", 
  #                             ifelse(input$sub_index_c == "abs",
  #                                    "Million", toupper(input$sub_index_c)),
  #                             ")",
  #                             sep = ""),
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks"
  #             ),
  #             legend = list(x = 0.5,
  #                           xanchor = "center",
  #                           # y = 100,
  #                           orientation = "h")
  #           )
  #       } else {
  #         p <- plot_ly(hoverinfo = "x+y+name")
  #         for (i in as.character(unique(plot_data$AUDIT))) {
  #           p <- add_trace(
  #             p,
  #             x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
  #             y = plot_data[as.character(plot_data$AUDIT) == i, "Value"],
  #             type = "scatter",
  #             mode = "lines+markers",
  #             colors = "Set1",
  #             name = i
  #           ) %>%
  #             add_text(
  #               x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
  #               y = plot_data[as.character(plot_data$AUDIT) == i, "Value"],
  #               text = round(plot_data[as.character(plot_data$AUDIT) == i, "Value"], 2),
  #               textfont = list(size = 10),
  #               textposition = "top",
  #               hoverinfo = "none",
  #               showlegend = FALSE
  #             )
  #         }
  #         
  #         p %>%
  #           config(
  #             displaylogo = FALSE,
  #             collaborate = FALSE,
  #             modeBarButtonsToRemove = list(
  #               'sendDataToCloud',
  #               'autoScale2d',
  #               'zoom2d',
  #               'pan2d',
  #               'select2d',
  #               'lasso2d',
  #               'toggleSpikelines'
  #             )
  #           ) %>%
  #           layout(
  #             title='Players Trend Performance',
  #             xaxis = list(
  #               zeroline = FALSE,
  #               title = "",
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks",
  #               tickvals = tickval,
  #               tickformat = "%y%m",
  #               type = "date"
  #             ),
  #             
  #             yaxis = list(
  #               zeroline = FALSE,
  #               title = paste(toupper(input$sub_measure_c), 
  #                             "(", 
  #                             ifelse(input$sub_index_c == "abs",
  #                                    "Million", toupper(input$sub_index_c)),
  #                             ")",
  #                             sep = ""),#"Measurement",
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks"
  #             ),
  #             legend = list(x = 0.5,
  #                           xanchor = "center",
  #                           # y = 100,
  #                           orientation = "h")
  #           )
  #       }
  #       
  #     } else {
  #       if (c("abs") %in% input$sub_index_c) {
  #         p <- plot_ly(hoverinfo = "x+y+name")
  #         for (i in as.character(unique(plot_data$AUDIT))) {
  #           p <- add_trace(
  #             p,
  #             x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
  #             y = plot_data[as.character(plot_data$AUDIT) == i, "Value"] / 1000000,
  #             type = "scatter",
  #             mode = "lines+markers",
  #             colors = "Set1",
  #             name = i
  #           )
  #         }
  #         
  #         p %>%
  #           config(
  #             displaylogo = FALSE,
  #             collaborate = FALSE,
  #             modeBarButtonsToRemove = list(
  #               'sendDataToCloud',
  #               'autoScale2d',
  #               'zoom2d',
  #               'pan2d',
  #               'select2d',
  #               'lasso2d',
  #               'toggleSpikelines'
  #             )
  #           ) %>%
  #           layout(
  #             title='Players Trend Performance',
  #             xaxis = list(
  #               zeroline = FALSE,
  #               title = "",
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks",
  #               tickvals = tickval,
  #               tickformat = "%y%m",
  #               type = "date"
  #             ),
  #             yaxis = list(
  #               zeroline = FALSE,
  #               title = paste(toupper(input$sub_measure_c), 
  #                             "(", 
  #                             ifelse(input$sub_index_c == "abs",
  #                                    "Million", toupper(input$sub_index_c)),
  #                             ")",
  #                             sep = ""),
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks"
  #             ),
  #             legend = list(x = 0.5,
  #                           xanchor = "center",
  #                           # y = 100,
  #                           orientation = "h")
  #           )
  #         
  #       } else {
  #         p <- plot_ly(hoverinfo = "x+y+name")
  #         for (i in as.character(unique(plot_data$AUDIT))) {
  #           
  #           p <- add_trace(
  #             p,
  #             x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
  #             y = plot_data[as.character(plot_data$AUDIT) == i, "Value"],
  #             type = "scatter",
  #             mode = "lines+markers",
  #             colors = "Set1",
  #             name = i
  #           )
  #           
  #           
  #         }
  #         
  #         p %>%
  #           config(
  #             displaylogo = FALSE,
  #             collaborate = FALSE,
  #             modeBarButtonsToRemove = list(
  #               'sendDataToCloud',
  #               'autoScale2d',
  #               'zoom2d',
  #               'pan2d',
  #               'select2d',
  #               'lasso2d',
  #               'toggleSpikelines'
  #             )
  #           ) %>%
  #           layout(
  #             title='Players Trend Performance',
  #             xaxis = list(
  #               zeroline = FALSE,
  #               title = "",
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks",
  #               tickvals = tickval,
  #               tickformat = "%y%m",
  #               type = "date"
  #             ),
  #             yaxis = list(
  #               zeroline = FALSE,
  #               title = paste(toupper(input$sub_measure_c), 
  #                             "(", 
  #                             ifelse(input$sub_index_c == "abs",
  #                                    "Million", toupper(input$sub_index_c)),
  #                             ")",
  #                             sep = ""),
  #               showline = TRUE,
  #               showgrid = FALSE,
  #               mirror = "ticks"
  #             ),
  #             legend = list(x = 0.5,
  #                           xanchor = "center",
  #                           # y = 100,
  #                           orientation = "h")
  #           )
  #       }
  #     }
  #   }
  #   
  #   
  #   
  # })
  # corporation_bar_plot <- reactive({
  #   if (input$goButton == 0) {
  #     return(NULL)
  #   }
  #   
  #   if ("RENMINBI"  %in% input$value) {
  #     if ("ALL" %in% input$province) {
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region,  province())),]
  #     } else {
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, input$province)),]
  #     }
  #     
  #     if ("Out hospital" ==  input$category && length(input$category) == 1) { # input$category == "Out hospital"
  #       summary <- summary%>%
  #         mutate(CORPORATE.DESC =
  #                  ifelse(PRODUCT.DESC %in% c("ATROVENT B.I", "COMBIVENT B.I"),
  #                         "999", CORPORATE.DESC))
  #     } 
  #     
  #     # summary <- summary %>%
  #     #   mutate(CORPORATE.DESC =
  #     #            ifelse(PRODUCT.DESC %in% c("ATROVENT B.I", "COMBIVENT B.I"),
  #     #                   "999", CORPORATE.DESC))
  #     
  #     rmb <- rbind.fill(
  #       graph1(
  #         summary,
  #         cate = input$category,
  #         subcate = input$subcategory,
  #         value = "RENMINBI",
  #         period = input$period,
  #         kpi = c("abs", "gr"),
  #         window = as.numeric(input$window),
  #         level = "corporation"
  #       ),
  #       graph1(
  #         summary,
  #         cate = input$category,
  #         subcate = input$subcategory,
  #         value = "RENMINBI",
  #         period = input$period,
  #         kpi = c("abs", "gr"),
  #         window = as.numeric(input$window),
  #         level = "molecule"
  #       )
  #     )
  #     rmb <- distinct(rmb)
  #     
  #   } else{
  #     rmb <- NULL
  #   }
  #   
  #   
  #   if ("UNIT" %in% input$value) {
  #     if ("ALL" %in% input$province) {
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region,  province())),]
  #     } else{
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, input$province)),]
  #     }
  #     summary <- summary %>%
  #       mutate(CORPORATE.DESC =
  #                ifelse(PRODUCT.DESC %in% c("ATROVENT B.I", "COMBIVENT B.I"),
  #                       "999", CORPORATE.DESC))
  #     
  #     unit <- rbind.fill(
  #       graph1(
  #         summary,
  #         cate = input$category,
  #         subcate = input$subcategory,
  #         value = "UNIT",
  #         period = input$period,
  #         kpi = c("abs", "gr"),
  #         window = as.numeric(input$window),
  #         level = "corporation"
  #       ),
  #       graph1(
  #         summary,
  #         cate = input$category,
  #         subcate = input$subcategory,
  #         value = "UNIT",
  #         period = input$period,
  #         kpi = c("abs", "gr"),
  #         window = as.numeric(input$window),
  #         level = "molecule"
  #       )
  #     )
  #     unit <- distinct(unit)
  #     
  #   } else{
  #     unit <- NULL
  #   }
  #   
  #   if ("DOT"  %in% input$value) {
  #     if ("ALL" %in% input$province) {
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region,  province())),]
  #     } else{
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, input$province)),]
  #     }
  #     summary <- summary %>%
  #       mutate(CORPORATE.DESC =
  #                ifelse(PRODUCT.DESC %in% c("ATROVENT B.I", "COMBIVENT B.I"),
  #                       "999", CORPORATE.DESC))
  #     
  #     dot <- rbind.fill(
  #       graph1(
  #         summary,
  #         cate = input$category,
  #         subcate = input$subcategory,
  #         value = "DOT",
  #         period = input$period,
  #         kpi = c("abs", "gr"),
  #         window = as.numeric(input$window),
  #         level = "corporation"
  #       ),
  #       graph1(
  #         summary,
  #         cate = input$category,
  #         subcate = input$subcategory,
  #         value = "DOT",
  #         period = input$period,
  #         kpi = c("abs", "gr"),
  #         window = as.numeric(input$window),
  #         level = "molecule"
  #       )
  #     )
  #     dot <- distinct(dot)
  #   } else{
  #     dot <- NULL
  #   }
  #   
  #   result1 <- rbind(rmb, unit, dot)
  #   
  #   
  #   
  #   result1$Measure[which(result1$Measure == "RENMINBI")] <- "RMB"
  #   
  #   if (is.null(input$sub_measure_c) || is.null(input$sub_region_c)) {
  #     return(NULL)
  #   } else {
  #     test <- result1 %>%
  #       filter(Measure %in% input$sub_measure_c) %>%
  #       gather(key = "date", value = "value", -c(AUDIT.DESC:Index)) %>%
  #       spread(Index, value) %>%
  #       filter(PRODUCT.DESC == "Overall", COMPS.DESC == "Overall",
  #              (CORPORATE.DESC == "B.INGELHEIM" |
  #                 CORPORATE.DESC == "Overall")) %>% 
  #       select(AUDIT.DESC, CORPORATE.DESC, Measure, date, ABS, GR = `GR%`) %>%
  #       # select(AUDIT.DESC, CORPORATE.DESC, Measure, date) %>%
  #       # # rename(GR = `GR%`) %>%
  #       # mutate(ABS = 1, GR = 1) %>%
  #       filter(AUDIT.DESC %in% input$sub_region_c)
  #   }
  #   
  #   
  #   
  #   # test <- outputtable %>% filter(AUDIT.DESC %in% input$sub_region) %>%
  #   #   select(AUDIT.DESC, CORPORATE.DESC, Measure, date, ABS, GR = `GR%`)
  #   # test <- test[, c(1, 4, 15, 16, 17, 18)]
  #   # colnames(test)[5] <- "ABS"
  #   # colnames(test)[6] <- "GR"
  #   # colnames(test)[which(colnames(test) == "GR%")] <- "GR"
  #   test1 <- setDT(test)
  #   test1 <- data.table::dcast(test1,
  #                              AUDIT.DESC +  Measure + date ~ CORPORATE.DESC,
  #                              value.var = c("ABS", "GR"))
  #   test1 <- setDF(test1) %>%
  #     mutate(date = substr(date, 3, 6))
  #   # tickval <- unlist(distinct(test1, Date))
  #   
  #   if (!("GR_B.INGELHEIM" %in% colnames(test1))) {
  #     test1 <- test1 %>% mutate(GR_B.INGELHEIM = NA)
  #   }
  #   
  #   if (all(is.na(test1$GR_B.INGELHEIM))) {
  #     if (input$label_c) {
  #       p0_overall <-
  #         plot_ly(test1) %>%
  #         add_trace(x = ~date,
  #                   y = ~ABS_Overall / 1000000,
  #                   type = "bar",
  #                   marker = list(color = '#D2D2D2'),
  #                   name = "TTL MKT Val",
  #                   width = 0.5) %>%
  #         add_trace(x = ~date,
  #                   y = ~GR_Overall,
  #                   type = "scatter",
  #                   mode = "lines+markers",
  #                   # color = ~factor(1),
  #                   line = list(color = 'red'),
  #                   # colors = "red",
  #                   yaxis = "y2",
  #                   name = "TTL_MKT_GR(%)") %>%
  #         layout(
  #           # legend = list(x = 0.5,
  #           #               xanchor = "center",
  #           #               y = 100,
  #           #               orientation = "h"),
  #           # autosize = T
  #           # ,
  #           margin =  list(
  #             # l = 50,
  #             r = 50
  #             # ,
  #             # b = 50,
  #             # t = 50,
  #             # pad = 4
  #           )
  #         ) %>%
  #         add_annotations(x = ~date,
  #                         y = ~ABS_Overall / 1000000 + 0.05 * ABS_Overall / 1000000,
  #                         text = ~format(round(ABS_Overall / 1000000, 2), big.mark = ","),
  #                         yref = "y",
  #                         xanchor = 'center',
  #                         yanchor = 'center',
  #                         showarrow = FALSE) %>%
  #         add_annotations(x = ~date[-1],
  #                         y = ~GR_Overall[-1] - 0.5,
  #                         # y = ~c(2, 2, 18),
  #                         text = ~paste(round(GR_Overall[-1], 2), "%", ""),
  #                         yref = "y2",
  #                         xanchor = 'center',
  #                         yanchor = 'center',
  #                         showarrow = FALSE) %>%
  #         add_annotations(x = ~date[-1],
  #                         y = ~GR_B.INGELHEIM[-1] + 0.5,
  #                         # y = ~c(2, 2, 18),
  #                         text = ~paste(round(GR_B.INGELHEIM[-1], 2), "%", ""),
  #                         yref = "y2",
  #                         xanchor = 'center',
  #                         yanchor = 'center',
  #                         showarrow = FALSE) %>%
  #         config(
  #           displaylogo = FALSE,
  #           collaborate = FALSE,
  #           modeBarButtonsToRemove = list(
  #             'sendDataToCloud',
  #             'autoScale2d',
  #             'zoom2d',
  #             'pan2d',
  #             'select2d',
  #             'lasso2d',
  #             'toggleSpikelines'
  #           )
  #         ) %>%
  #         layout(
  #           title='Market & BI GR% Performance',
  #           xaxis = list(
  #             zeroline = FALSE,
  #             title = "",
  #             showline = TRUE,
  #             showgrid = FALSE,
  #             mirror = "ticks"
  #             # ,
  #             # tickvals = tickval,
  #             # tickformat = "%y%m",
  #             # type = "date"
  #             
  #           ),
  #           yaxis = list(
  #             zeroline = FALSE,
  #             title = "RMB(Million)",
  #             showline = TRUE,
  #             showgrid = FALSE,
  #             mirror = "ticks",
  #             showticklabels = TRUE
  #           ),
  #           yaxis2 = list(title = "",
  #                         overlaying = "y",
  #                         ticksuffix = "%",
  #                         side = "right",
  #                         fill = "tozeroy",
  #                         zeroline = FALSE,
  #                         showgrid = FALSE,
  #                         showticklabels = TRUE),
  #           legend = list(x = 0.5,
  #                         xanchor = "center",
  #                         # y = 100,
  #                         orientation = "h")
  #         )
  #     } else {
  #       p0_overall <-
  #         plot_ly(test1) %>%
  #         add_trace(x = ~date,
  #                   y = ~ABS_Overall / 1000000,
  #                   type = "bar",
  #                   marker = list(color = '#D2D2D2'),
  #                   name = "TTL MKT Val",
  #                   width = 0.5) %>%
  #         add_trace(x = ~date,
  #                   y = ~GR_Overall,
  #                   type = "scatter",
  #                   mode = "lines+markers",
  #                   # color = ~factor(1),
  #                   line = list(color = 'red'),
  #                   # colors = "red",
  #                   yaxis = "y2",
  #                   name = "TTL_MKT_GR(%)") %>%
  #         layout(
  #           # legend = list(x = 0.5,
  #           #               xanchor = "center",
  #           #               y = 100,
  #           #               orientation = "h"),
  #           autosize = T
  #           ,
  #           margin =  list(
  #             # l = 50,
  #             r = 50
  #             # ,
  #             # b = 50,
  #             # t = 50,
  #             # pad = 4
  #           )
  #         ) %>%
  #         config(
  #           displaylogo = FALSE,
  #           collaborate = FALSE,
  #           modeBarButtonsToRemove = list(
  #             'sendDataToCloud',
  #             'autoScale2d',
  #             'zoom2d',
  #             'pan2d',
  #             'select2d',
  #             'lasso2d',
  #             'toggleSpikelines'
  #           )
  #         )  %>%
  #         layout(
  #           title='Market & BI GR% Performance',
  #           xaxis = list(
  #             zeroline = FALSE,
  #             title = "",
  #             showline = TRUE,
  #             showgrid = FALSE,
  #             mirror = "ticks"
  #             # ,
  #             # tickvals = tickval,
  #             # tickformat = "%y%m",
  #             # type = "date"
  #             
  #           ),
  #           yaxis = list(
  #             zeroline = FALSE,
  #             title = "RMB(Million)",
  #             showline = TRUE,
  #             showgrid = FALSE,
  #             mirror = "ticks",
  #             showticklabels = TRUE
  #           ),
  #           yaxis2 = list(title = "",
  #                         overlaying = "y",
  #                         ticksuffix = "%",
  #                         side = "right",
  #                         fill = "tozeroy",
  #                         zeroline = FALSE,
  #                         showgrid = FALSE,
  #                         showticklabels = TRUE),
  #           legend = list(x = 0.5,
  #                         xanchor = "center",
  #                         # y = -1,
  #                         orientation = "h")
  #         )
  #       
  #     }
  #     
  #   } else {
  #     if (input$label_c) {
  #       p0_overall <-
  #         plot_ly(test1) %>%
  #         add_trace(x = ~date,
  #                   y = ~ABS_Overall / 1000000,
  #                   type = "bar",
  #                   marker = list(color = '#D2D2D2'),
  #                   name = "TTL MKT Val",
  #                   width = 0.5) %>%
  #         add_trace(x = ~date,
  #                   y = ~GR_Overall,
  #                   type = "scatter",
  #                   mode = "lines+markers",
  #                   # color = ~factor(1),
  #                   line = list(color = 'red'),
  #                   # colors = "red",
  #                   yaxis = "y2",
  #                   name = "TTL_MKT_GR(%)") %>%
  #         add_trace(x = ~date,
  #                   y = ~GR_B.INGELHEIM,
  #                   type = "scatter",
  #                   mode = "lines+markers",
  #                   # color = ~factor(2),
  #                   line = list(color = 'blue'),
  #                   # colors = "blue",
  #                   yaxis = "y2",
  #                   name = "TTL_BI_GR(%)") %>%
  #         layout(
  #           # legend = list(x = 0.5,
  #           #               xanchor = "center",
  #           #               y = 100,
  #           #               orientation = "h"),
  #           # autosize = T
  #           # ,
  #           margin =  list(
  #             # l = 50,
  #             r = 50
  #             # ,
  #             # b = 50,
  #             # t = 50,
  #             # pad = 4
  #           )
  #         ) %>%
  #         add_annotations(x = ~date,
  #                         y = ~ABS_Overall / 1000000 + 0.05 * ABS_Overall / 1000000,
  #                         text = ~format(round(ABS_Overall / 1000000, 2), big.mark = ","),
  #                         yref = "y",
  #                         xanchor = 'center',
  #                         yanchor = 'center',
  #                         showarrow = FALSE) %>%
  #         add_annotations(x = ~date[-1],
  #                         y = ~GR_Overall[-1] - 0.5,
  #                         # y = ~c(2, 2, 18),
  #                         text = ~paste(round(GR_Overall[-1], 2), "%", ""),
  #                         yref = "y2",
  #                         xanchor = 'center',
  #                         yanchor = 'center',
  #                         showarrow = FALSE) %>%
  #         add_annotations(x = ~date[-1],
  #                         y = ~GR_B.INGELHEIM[-1] + 0.5,
  #                         # y = ~c(2, 2, 18),
  #                         text = ~paste(round(GR_B.INGELHEIM[-1], 2), "%", ""),
  #                         yref = "y2",
  #                         xanchor = 'center',
  #                         yanchor = 'center',
  #                         showarrow = FALSE) %>%
  #         config(
  #           displaylogo = FALSE,
  #           collaborate = FALSE,
  #           modeBarButtonsToRemove = list(
  #             'sendDataToCloud',
  #             'autoScale2d',
  #             'zoom2d',
  #             'pan2d',
  #             'select2d',
  #             'lasso2d',
  #             'toggleSpikelines'
  #           )
  #         ) %>%
  #         layout(
  #           title='Market & BI GR% Performance',
  #           xaxis = list(
  #             zeroline = FALSE,
  #             title = "",
  #             showline = TRUE,
  #             showgrid = FALSE,
  #             mirror = "ticks"
  #             # ,
  #             # tickvals = tickval,
  #             # tickformat = "%y%m",
  #             # type = "date"
  #             
  #           ),
  #           yaxis = list(
  #             zeroline = FALSE,
  #             title = "RMB(Million)",
  #             showline = TRUE,
  #             showgrid = FALSE,
  #             mirror = "ticks",
  #             showticklabels = TRUE
  #           ),
  #           yaxis2 = list(title = "",
  #                         overlaying = "y",
  #                         ticksuffix = "%",
  #                         side = "right",
  #                         fill = "tozeroy",
  #                         zeroline = FALSE,
  #                         showgrid = FALSE,
  #                         showticklabels = TRUE),
  #           legend = list(x = 0.5,
  #                         xanchor = "center",
  #                         # y = 100,
  #                         orientation = "h")
  #         )
  #     } else {
  #       p0_overall <-
  #         plot_ly(test1) %>%
  #         add_trace(x = ~date,
  #                   y = ~ABS_Overall / 1000000,
  #                   type = "bar",
  #                   marker = list(color = '#D2D2D2'),
  #                   name = "TTL MKT Val",
  #                   width = 0.5) %>%
  #         add_trace(x = ~date,
  #                   y = ~GR_Overall,
  #                   type = "scatter",
  #                   mode = "lines+markers",
  #                   # color = ~factor(1),
  #                   line = list(color = 'red'),
  #                   # colors = "red",
  #                   yaxis = "y2",
  #                   name = "TTL_MKT_GR(%)") %>%
  #         add_trace(x = ~date,
  #                   y = ~GR_B.INGELHEIM,
  #                   type = "scatter",
  #                   mode = "lines+markers",
  #                   # color = ~factor(2),
  #                   line = list(color = 'blue'),
  #                   # colors = "blue",
  #                   yaxis = "y2",
  #                   name = "TTL_BI_GR(%)") %>%
  #         layout(
  #           # legend = list(x = 0.5,
  #           #               xanchor = "center",
  #           #               y = 100,
  #           #               orientation = "h"),
  #           autosize = T
  #           ,
  #           margin =  list(
  #             # l = 50,
  #             r = 50
  #             # ,
  #             # b = 50,
  #             # t = 50,
  #             # pad = 4
  #           )
  #         ) %>%
  #         config(
  #           displaylogo = FALSE,
  #           collaborate = FALSE,
  #           modeBarButtonsToRemove = list(
  #             'sendDataToCloud',
  #             'autoScale2d',
  #             'zoom2d',
  #             'pan2d',
  #             'select2d',
  #             'lasso2d',
  #             'toggleSpikelines'
  #           )
  #         )  %>%
  #         layout(
  #           title='Market & BI GR% Performance',
  #           xaxis = list(
  #             zeroline = FALSE,
  #             title = "",
  #             showline = TRUE,
  #             showgrid = FALSE,
  #             mirror = "ticks"
  #             # ,
  #             # tickvals = tickval,
  #             # tickformat = "%y%m",
  #             # type = "date"
  #             
  #           ),
  #           yaxis = list(
  #             zeroline = FALSE,
  #             title = "RMB(Million)",
  #             showline = TRUE,
  #             showgrid = FALSE,
  #             mirror = "ticks",
  #             showticklabels = TRUE
  #           ),
  #           yaxis2 = list(title = "",
  #                         overlaying = "y",
  #                         ticksuffix = "%",
  #                         side = "right",
  #                         fill = "tozeroy",
  #                         zeroline = FALSE,
  #                         showgrid = FALSE,
  #                         showticklabels = TRUE),
  #           legend = list(x = 0.5,
  #                         xanchor = "center",
  #                         # y = -1,
  #                         orientation = "h")
  #         )
  #       
  #     }
  #   }
  #   return(p0_overall)
  # })
  # 
  # 
  # # output$chart_c <- renderPlotly(corporation_plot())
  # output$chart_c <- 
  #   renderPlotly(if (is.null(corporation_plot())) {
  #     plotly_empty()} else {
  #       corporation_plot()})
  # 
  # output$bar_chart_c <- 
  #   renderPlotly(if (is.null(corporation_bar_plot()) | input$rpp_p_c) {
  #     plotly_empty() } else {
  #       corporation_bar_plot()})
  # 
  # 
  # output$downloadPlot_c <- downloadHandler(
  #   filename = function() {
  #     paste(
  #       "Corporation_Visulization_",
  #       input$period,
  #       "_",
  #       input$window,
  #       "year",
  #       '.png',
  #       sep = ''
  #     )
  #   },
  #   content = function(file) {
  #     export(corporation_plot(), file = file)
  #     # plotly_IMAGE(molecule_plot(), out_file = file)
  #     # file.copy('tempPlot.png',file)
  #   }
  # )
  # 
  # output$downloadPlot_c1 <- downloadHandler(
  #   filename = function() {
  #     paste(
  #       "Corporation_Visulization_",
  #       input$period,
  #       "_",
  #       input$window,
  #       "year",
  #       '.png',
  #       sep = ''
  #     )
  #   },
  #   content = function(file) {
  #     export(corporation_bar_plot(), file = file)
  #     # plotly_IMAGE(molecule_plot(), out_file = file)
  #     # file.copy('tempPlot.png',file)
  #   }
  # )
  # 
  # 
  # molecule_plot_data <- NULL
  # 
  # molecule_plot <- reactive({
  #   if (input$goButton == 0) {
  #     return(NULL)
  #   }
  #   
  #   if (is.null(summary()))
  #     return(NULL)
  #   
  #   chk <- summary() %>%
  #     filter(AUDIT.DESC == "China") %>%
  #     select(AUDIT.DESC:MANU_CN, contains("mat_RENMINBI")) %>%
  #     filter(Category %in% input$category,
  #            Sub.category %in% input$subcategory)
  #   
  #   chk1 <- chk %>%
  #     select(1:(ncol(chk) - 5), (ncol(chk) - 1):ncol(chk))
  #   
  #   colnames(chk1)[(ncol(chk1) - 1):ncol(chk1)] <- c("Y1", "Y2")
  #   
  #   chk2 <- chk1 %>%
  #     mutate(Y1 = as.numeric(Y1),
  #            Y2 = as.numeric(Y2)) %>%
  #     group_by(Sub.category, COMPS.DESC) %>%
  #     summarise(Y1 = sum(Y1, na.rm = TRUE),
  #               Y2 = sum(Y2, na.rm = TRUE)) %>%
  #     ungroup() %>%
  #     mutate(level = 3)
  #   
  #   chk3 <- chk1 %>%
  #     mutate(Y1 = as.numeric(Y1),
  #            Y2 = as.numeric(Y2)) %>%
  #     group_by(Sub.category) %>%
  #     summarise(Y1 = sum(Y1, na.rm = TRUE),
  #               Y2 = sum(Y2, na.rm = TRUE)) %>%
  #     ungroup() %>%
  #     mutate(level = 2)
  #   
  #   chk4 <- chk1 %>%
  #     mutate(Y1 = as.numeric(Y1),
  #            Y2 = as.numeric(Y2)) %>%
  #     group_by(Sub.category = "Total Category") %>%
  #     summarise(Y1 = sum(Y1, na.rm = TRUE),
  #               Y2 = sum(Y2, na.rm = TRUE)) %>%
  #     ungroup() %>%
  #     mutate(level = 1)
  #   
  #   if (length(unique(chk$Sub.category)) == 1 && unique(chk$Sub.category) == "Others") {
  #     chk5 <- bind_rows(chk4, chk2) %>%
  #       mutate(COMPS.DESC = ifelse(is.na(COMPS.DESC), Sub.category, COMPS.DESC),
  #              GR = Y2 / Y1 - 1
  #              ,
  #              Sub.category = factor(Sub.category,
  #                                    levels = c(setdiff(unique(Sub.category),
  #                                                       "Total Category"), "Total Category"))
  #       ) %>%
  #       arrange(Sub.category, desc(level)) %>%
  #       mutate(COMPS.DESC = factor(COMPS.DESC, levels = unique(COMPS.DESC)),
  #              level1 = ifelse(level == 1, "Total Category", 
  #                              ifelse(level == 2, "Sub-category",
  #                                     "Molecule")),
  #              GR = ifelse(is.na(GR), 0, GR))
  #     
  #     tmp <- bind_rows(chk4, chk2) %>%
  #       arrange(desc(Y2)) %>%
  #       mutate(rank = row_number(),
  #              Sub.category = as.character(Sub.category),
  #              COMPS.DESC = ifelse(is.na(COMPS.DESC), Sub.category, COMPS.DESC))
  #     
  #     chk5 <- chk5 %>%
  #       left_join(tmp[, c("Sub.category", "COMPS.DESC", "rank")],
  #                 by = c("Sub.category", "COMPS.DESC")) %>%
  #       arrange(desc(rank), desc(level)) %>%
  #       mutate(COMPS.DESC = factor(COMPS.DESC, levels = unique(COMPS.DESC)))
  #     
  #   } else {
  #     chk5 <- bind_rows(chk4, chk3, chk2) %>%
  #       mutate(COMPS.DESC = ifelse(is.na(COMPS.DESC), Sub.category, COMPS.DESC),
  #              GR = Y2 / Y1 - 1,
  #              Sub.category = factor(Sub.category,
  #                                    levels = c(setdiff(unique(Sub.category),
  #                                                       "Total Category"), "Total Category"))
  #       ) %>%
  #       arrange(Sub.category, desc(level)) %>%
  #       mutate(COMPS.DESC = factor(COMPS.DESC, levels = unique(COMPS.DESC)),
  #              level1 = ifelse(level == 1, "Total Category", 
  #                              ifelse(level == 2, "Sub-category",
  #                                     "Molecule")),
  #              GR = ifelse(is.na(GR), 0, GR))
  #     
  #     tmp <- bind_rows(chk4, chk3, chk2) %>% 
  #       filter(level == 2 | level == 1) %>%
  #       arrange(desc(Y2)) %>%
  #       mutate(rank = row_number(),
  #              Sub.category = as.character(Sub.category))
  #     
  #     
  #     chk5 <- chk5 %>%
  #       left_join(tmp[, c("Sub.category", "rank")],
  #                 by = "Sub.category") %>%
  #       arrange(desc(rank), desc(level), Y2) %>%
  #       mutate(COMPS.DESC = factor(COMPS.DESC, levels = unique(COMPS.DESC)))
  #   }
  #   
  #   molecule_plot_data <<- chk5
  #   
  #   pal <- c("#4F81BD",#"#0000ff",
  #            "#FFC000",#"#ffff00",
  #            "#C5D9F1"#"#a3c2c2"
  #   )
  #   pal <- setNames(pal, c("Total Category", "Sub-category", "Molecule"))
  #   
  #   if (input$label_m) {
  #     plot_ly(chk5,
  #             x = ~round(GR * 100, 2), 
  #             y = ~COMPS.DESC, 
  #             color = ~level1,
  #             colors = pal,
  #             type = 'bar',
  #             orientation = 'h') %>%
  #       add_annotations(xref = "x",
  #                       yref = "y",
  #                       x = ~round(GR * 100, 2) + 5,
  #                       y = ~COMPS.DESC,
  #                       # y = ~c(2, 2, 18),
  #                       text = ~gsub(" ", "", paste(round(GR * 100, 2), "%", "")),
  #                       # yref = "y2",
  #                       xanchor = 'center',
  #                       yanchor = 'center',
  #                       showarrow = FALSE) %>%
  #       config(
  #         displaylogo = FALSE,
  #         collaborate = FALSE,
  #         modeBarButtonsToRemove = list(
  #           'sendDataToCloud',
  #           'autoScale2d',
  #           'zoom2d',
  #           'pan2d',
  #           'select2d',
  #           'lasso2d',
  #           'toggleSpikelines'
  #         )
  #       ) %>%
  #       layout(
  #         title='China: China: Sub-category and Molecule MAT Value GR',
  #         autosize = T,
  #         margin =  list(
  #           l = 250
  #           # ,
  #           # r = 100,
  #           # b = 50,
  #           # t = 50,
  #           # pad = 4
  #         ),
  #         xaxis = list(
  #           zeroline = TRUE,
  #           title = "",
  #           ticksuffix = "%",
  #           showline = TRUE,
  #           showgrid = TRUE,
  #           mirror = "ticks"
  #           # ,
  #           # tickvals = tickval,
  #           # tickformat = "%y%m",
  #           # type = "date"
  #         ),
  #         yaxis = list(
  #           zeroline = FALSE,
  #           title = "",
  #           showline = FALSE,
  #           showgrid = FALSE,
  #           mirror = "ticks"
  #         ),
  #         legend = list(x = 0.5,
  #                       xanchor = "center",
  #                       # y = 100,
  #                       orientation = "h"))
  #   } else {
  #     plot_ly(chk5,
  #             x = ~round(GR * 100, 2), 
  #             y = ~COMPS.DESC, 
  #             color = ~level1,
  #             colors = pal,
  #             type = 'bar',
  #             orientation = 'h') %>%
  #       config(
  #         displaylogo = FALSE,
  #         collaborate = FALSE,
  #         modeBarButtonsToRemove = list(
  #           'sendDataToCloud',
  #           'autoScale2d',
  #           'zoom2d',
  #           'pan2d',
  #           'select2d',
  #           'lasso2d',
  #           'toggleSpikelines'
  #         )
  #       ) %>%
  #       layout(
  #         title='China: Sub-category and Molecule MAT Value GR',
  #         autosize = T,
  #         margin =  list(
  #           l = 250
  #           # ,
  #           # r = 100,
  #           # b = 50,
  #           # t = 50,
  #           # pad = 4
  #         ),
  #         xaxis = list(
  #           zeroline = TRUE,
  #           title = "",
  #           ticksuffix = "%",
  #           showline = TRUE,
  #           showgrid = TRUE,
  #           mirror = "ticks"
  #           # ,
  #           # tickvals = tickval,
  #           # tickformat = "%y%m",
  #           # type = "date"
  #         ),
  #         yaxis = list(
  #           zeroline = FALSE,
  #           title = "",
  #           showline = FALSE,
  #           showgrid = FALSE,
  #           mirror = "ticks"
  #         ),
  #         legend = list(x = 0.5,
  #                       xanchor = "center",
  #                       # y = 100,
  #                       orientation = "h"))
  #   }
  #   
  #   
  # })
  # 
  # bar_molecule_plot_data <- NULL
  # 
  # bar_molecule_plot <- reactive({
  #   if (input$goButton == 0) {
  #     return(NULL)
  #   }
  #   
  #   if (is.null(summary()))
  #     return(NULL)
  #   
  #   chk <- summary() %>%
  #     filter(AUDIT.DESC == "China") %>%
  #     select(AUDIT.DESC:MANU_CN, contains("mat_RENMINBI")) %>%
  #     filter(Category %in% input$category,
  #            Sub.category %in% input$subcategory)
  #   
  #   chk1 <- chk %>%
  #     select(1:(ncol(chk) - 5), (ncol(chk) - 1):ncol(chk))
  #   
  #   colnames(chk1)[(ncol(chk1) - 1):ncol(chk1)] <- c("Y1", "Y2")
  #   
  #   chk5 <- chk1 %>% 
  #     mutate(Y1 = as.numeric(Y1),
  #            Y2 = as.numeric(Y2)) %>%
  #     mutate(Type = ifelse(MANUF.TYPE.DESC %in% c("IMPORT", "JOINT-VENTURE"),
  #                          "MNC", "Local")) %>%
  #     group_by(COMPS.DESC = "Total Category",
  #              Type) %>%
  #     summarise(Y2 = sum(Y2, na.rm = TRUE)) %>%
  #     ungroup() %>%
  #     spread(key = Type, value = Y2) 
  #   
  #   if (!("Local" %in% colnames(chk5))) {
  #     chk5_m <- bind_cols(chk5, Local = rep(0, nrow(chk5)))
  #   } else if (!("MNC" %in% colnames(chk5))) {
  #     chk5_m <- bind_cols(chk5, MNC = rep(0, nrow(chk5)))
  #   } else {
  #     chk5_m <- chk5
  #   }
  #   
  #   chk5_m <- chk5_m %>%
  #     mutate(Local = ifelse(is.na(Local), 0, Local),
  #            MNC = ifelse(is.na(MNC), 0, MNC),
  #            Local_sh = round(Local / (Local + MNC) * 100, 2),
  #            MNC_sh = round(MNC / (Local + MNC) * 100, 2),
  #            Local_sh = ifelse(is.na(Local_sh), 0, Local_sh),
  #            MNC_sh = ifelse(is.na(MNC_sh), 0, MNC_sh))
  #   
  #   chk6 <- chk1 %>% 
  #     mutate(Y1 = as.numeric(Y1),
  #            Y2 = as.numeric(Y2)) %>%
  #     mutate(Type = ifelse(MANUF.TYPE.DESC %in% c("IMPORT", "JOINT-VENTURE"),
  #                          "MNC", "Local")) %>%
  #     group_by(COMPS.DESC,
  #              Type) %>%
  #     summarise(Y2 = sum(Y2, na.rm = TRUE)) %>%
  #     ungroup() %>%
  #     spread(key = Type, value = Y2)
  #   
  #   if (!("Local" %in% colnames(chk5))) {
  #     chk6_m <- bind_cols(chk6, Local = rep(0, nrow(chk6)))
  #   } else if (!("MNC" %in% colnames(chk5))) {
  #     chk6_m <- bind_cols(chk6, MNC = rep(0, nrow(chk6)))
  #   } else {
  #     chk6_m <- chk6
  #   }
  #   
  #   chk6_m <- chk6_m%>%
  #     mutate(Local = ifelse(is.na(Local), 0, Local),
  #            MNC = ifelse(is.na(MNC), 0, MNC),
  #            Local_sh = round(Local / (Local + MNC) * 100, 2),
  #            MNC_sh = round(MNC / (Local + MNC) * 100, 2),
  #            Local_sh = ifelse(is.na(Local_sh), 0, Local_sh),
  #            MNC_sh = ifelse(is.na(MNC_sh), 0, MNC_sh))
  #   
  #   chk7 <- bind_rows(chk5_m, chk6_m) %>%
  #     mutate(total_sales = MNC + Local) %>%
  #     arrange(total_sales) %>%
  #     select(-c(Local, MNC, total_sales)) %>%
  #     # gather(key = type, value = share, -COMPS.DESC) %>%
  #     mutate(COMPS.DESC = factor(COMPS.DESC, 
  #                                levels = COMPS.DESC))
  #   
  #   bar_molecule_plot_data <<- chk7
  #   
  #   
  #   if (input$label_m) {
  #     plot_ly(chk7, 
  #             x = ~MNC_sh, 
  #             y = ~COMPS.DESC, 
  #             type = 'bar', 
  #             orientation = 'h',
  #             marker = list(color = "rgb(79, 129,	189)"#'rgba(38, 24, 74, 0.8)',
  #                           # line = list(color = 'rgb(248, 248, 249)', width = 1)
  #             ),
  #             name = "MNC") %>%
  #       add_trace(x = ~Local_sh, 
  #                 marker = list(color = "rgb(255,0,0)"#'rgba(71, 58, 131, 0.8)'
  #                 ),
  #                 name = "Local") %>%
  #       layout(
  #         xaxis = list(
  #           zeroline = TRUE,
  #           title = "",
  #           ticksuffix = "%",
  #           showline = TRUE,
  #           showgrid = TRUE,
  #           mirror = "ticks"
  #           # ,
  #           # tickvals = tickval,
  #           # tickformat = "%y%m",
  #           # type = "date"
  #         ),
  #         yaxis = list(
  #           zeroline = FALSE,
  #           title = "",
  #           showline = FALSE,
  #           showgrid = FALSE,
  #           mirror = "ticks"
  #         ),
  #         legend = list(x = 0.5,
  #                       # y = 0,
  #                       xanchor = "center",
  #                       orientation = "h"),
  #         barmode = 'stack',
  #         title = "China: Sub-category and Molecule MAT Value GR",
  #         # paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
  #         autosize = T,
  #         margin =  list(
  #           l = 250
  #           # ,
  #           # r = 25,
  #           # b = 100,
  #           # t = 100,
  #           # pad = 4
  #         ),
  #         showlegend = TRUE) %>%
  #       # labeling the percentages of each bar (x_axis)
  #       add_annotations(xref = 'x', 
  #                       yref = 'y',
  #                       x = ~MNC_sh / 2, 
  #                       y = ~COMPS.DESC,
  #                       text = ~ifelse(MNC_sh == 0, "",paste(round(MNC_sh, 2), '%')),
  #                       font = list(family = 'Arial', size = 12,
  #                                   color = 'rgb(248, 248, 255)'),
  #                       showarrow = FALSE) %>%
  #       add_annotations(xref = 'x', 
  #                       yref = 'y',
  #                       x = ~MNC_sh + Local_sh / 2, 
  #                       y = ~COMPS.DESC,
  #                       text = ~ifelse(Local_sh == 0, "", paste(round(Local_sh, 2), '%')),
  #                       font = list(family = 'Arial', size = 12,
  #                                   color = 'rgb(248, 248, 255)'),
  #                       showarrow = FALSE) %>%
  #       config(
  #         displaylogo = FALSE,
  #         collaborate = FALSE,
  #         modeBarButtonsToRemove = list(
  #           'sendDataToCloud',
  #           'autoScale2d',
  #           'zoom2d',
  #           'pan2d',
  #           'select2d',
  #           'lasso2d',
  #           'toggleSpikelines'
  #         ))
  #   } else {
  #     plot_ly(chk7, 
  #             x = ~MNC_sh, 
  #             y = ~COMPS.DESC, 
  #             type = 'bar', 
  #             orientation = 'h',
  #             marker = list(color = "rgb(79, 129,	189)"
  #                           # line = list(color = 'rgb(248, 248, 249)', width = 1)
  #             ),
  #             name = "MNC") %>%
  #       add_trace(x = ~Local_sh, 
  #                 marker = list(color = "rgb(255,0,0)"),
  #                 name = "Local") %>%
  #       layout(
  #         xaxis = list(
  #           zeroline = TRUE,
  #           title = "",
  #           ticksuffix = "%",
  #           showline = TRUE,
  #           showgrid = TRUE,
  #           mirror = "ticks"
  #           # ,
  #           # tickvals = tickval,
  #           # tickformat = "%y%m",
  #           # type = "date"
  #         ),
  #         yaxis = list(
  #           zeroline = FALSE,
  #           title = "",
  #           showline = FALSE,
  #           showgrid = FALSE,
  #           mirror = "ticks"
  #         ),
  #         legend = list(x = 0.5,
  #                       # y = 0,
  #                       xanchor = "center",
  #                       orientation = "h"),
  #         barmode = 'stack',
  #         title = "China: Sub-category and Molecule MAT Value GR",
  #         # paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
  #         autosize = T,
  #         margin =  list(
  #           l = 250
  #           # ,
  #           # r = 25,
  #           # b = 100,
  #           # t = 100,
  #           # pad = 4
  #         ),
  #         showlegend = TRUE)  %>%
  #       config(
  #         displaylogo = FALSE,
  #         collaborate = FALSE,
  #         modeBarButtonsToRemove = list(
  #           'sendDataToCloud',
  #           'autoScale2d',
  #           'zoom2d',
  #           'pan2d',
  #           'select2d',
  #           'lasso2d',
  #           'toggleSpikelines'
  #         ))
  #   }
  # })
  # 
  # output$chart_m <- 
  #   renderPlotly(if (is.null(molecule_plot())) {
  #     plotly_empty()} else {
  #       molecule_plot()})
  # 
  # output$bar_chart_m <- 
  #   renderPlotly(if (is.null(bar_molecule_plot())) {
  #     plotly_empty() } else {
  #       bar_molecule_plot()})
  # 
  # output$caption <- 
  #   renderText("Note: Sub-category in descending order + Sub-category's 
  #              Molecules in descending order by MAT Value")
  # output$caption1 <- renderText("Note: Molecules MAT Value in 
  #                               descending order by MAT Value")
  # 
  # output$downloadPlot_m <- downloadHandler(
  #   filename = function() {
  #     paste("Molecule_Visulization", '.png', sep = '')
  #   },
  #   content = function(file) {
  #     export(molecule_plot(), file = file)
  #     # plotly_IMAGE(molecule_plot(), out_file = file)
  #     # file.copy('tempPlot.png',file)
  #   }
  # )
  # 
  # output$downloadPlot_m1 <- downloadHandler(
  #   filename = function() {
  #     paste("Bar_Molecule_Visulization", '.png', sep = '')
  #   },
  #   content = function(file) {
  #     export(bar_molecule_plot(), file = file)
  #     # plotly_IMAGE(molecule_plot(), out_file = file)
  #     # file.copy('tempPlot.png',file)
  #   }
  # )
  # 
  # 
  # # output$downloadPlot_m <- downloadHandler(
  # #   filename = function() { paste("Molecule_Visulization", '.png', sep='') },
  # #   content = function(file) {
  # #     # export(molecule_plot(), file = file)
  # #     plotly_IMAGE(molecule_plot(), out_file = file)
  # #   }
  # # )
  # 
  # 
  # ############################# download plot data #############################
  # 
  # plot_data_p <- reactive({
  #   if (is.null(ot()))
  #     return(NULL)
  #   
  #   ot <- ot()$result3
  #   ot <-
  #     filter(
  #       ot,
  #       (CORPORATE.DESC != "Overall" & COMPS.DESC != "Overall") |
  #         (CORPORATE.DESC == "Overall" &
  #            COMPS.DESC == "Overall")
  #     )
  #   ot$AUDIT.DESC <- gsub(" .*$", "", ot$AUDIT.DESC)
  #   
  #   ot$PRODUCT.DESC <- as.character(ot$PRODUCT.DESC)
  #   ot$CORPORATE.DESC <- as.character(ot$CORPORATE.DESC)
  #   
  #   ot$PRODUCT.DESC[which(ot$CORPORATE.DESC == "Overall")] <-
  #     ot$AUDIT.DESC[which(ot$CORPORATE.DESC == "Overall")]
  #   KK <- which(ot$PRODUCT.DESC == "Overall")
  #   
  #   ot$PRODUCT.DESC[KK] <- ot$CORPORATE.DESC[KK]
  #   ot$CORPORATE.DESC[KK] <- "Company"
  #   
  #   names(ot)[names(ot) == 'AUDIT.DESC'] <- 'AUDIT'
  #   names(ot)[names(ot) == 'CORPORATE.DESC'] <- 'CORPORATE'
  #   names(ot)[names(ot) == 'PRODUCT.DESC'] <- 'PRODUCT'
  #   
  #   ot$PRODUCT <- as.factor(ot$PRODUCT)
  #   ot$CORPORATE <- as.factor(ot$CORPORATE)
  #   ot$AUDIT <- as.factor(ot$AUDIT)
  #   
  #   ot <- as.data.frame(ot)
  #   otnum <- which(grepl("Index", names(ot)))
  #   #number here need to be updated for new update
  #   ot[which(ot$Index == "ABS"), (otnum + 1):length(ot)] <-
  #     format(ot[which(ot$Index == "ABS"), (otnum + 1):length(ot)],
  #            big.mark = ",", scientific = FALSE)
  #   ot$Measure[which(ot$Measure == "RENMINBI")] <- "RMB"
  #   
  #   if (input$sub_index == "ms") {
  #     tmp_sub_index <- "MS%"
  #   } else if (input$sub_index == "gr") {
  #     tmp_sub_index <- "GR%"
  #   } else if (input$sub_index == "mc") {
  #     tmp_sub_index <- "MS+/- %"
  #   } else {
  #     tmp_sub_index <- input$sub_index
  #   }
  #   
  #   if (as.integer(input$top) > 0) {
  #     plot_data <- ot %>%
  #       filter(
  #         Measure == input$sub_measure,
  #         Index == toupper(tmp_sub_index),
  #         AUDIT %in% gsub(" .*$", "", input$sub_region),
  #         as.character(PRODUCT) %in% input$sub_top,
  #         # COMPS.DESC %in% c("Molecule", "Overall"),
  #         (
  #           as.character(PRODUCT) == as.character(AUDIT) |
  #             as.character(PRODUCT) %in% input$sub_top
  #         )
  #       ) %>%
  #       mutate(PRODUCT = factor(PRODUCT, levels = input$sub_top)) %>%
  #       # toplist()[1:(1 + as.integer(input$sub_top))])) %>%
  #       gather(
  #         Date,
  #         Value,
  #         -AUDIT,
  #         -Category,
  #         -Sub.category,
  #         -CORPORATE,
  #         -PRODUCT,-COMPS.DESC,
  #         -pro.code,
  #         -MANUF.TYPE.DESC,
  #         -TC.III.SHORT.DESC,-TC.III.DESC,
  #         -Category_CN,
  #         -COMPS.DESC,
  #         -Brand_CN,
  #         -MANU_CN,-Measure,
  #         -Index
  #       ) %>%
  #       mutate(Date = as.Date(ymd(paste(
  #         Date, "01", "."
  #       ))),
  #       Value = as.numeric(gsub(",", "", Value))) %>%
  #       spread(Date, Value) %>%
  #       arrange(PRODUCT)
  #   } else {
  #     plot_data <- ot %>%
  #       filter(
  #         Measure == input$sub_measure,
  #         Index == toupper(tmp_sub_index),
  #         # AUDIT == gsub(" .*$", "", input$sub_region),
  #         # COMPS.DESC %in% c("Molecule", "Overall"),
  #         (
  #           as.character(PRODUCT) == as.character(AUDIT) |
  #             as.character(PRODUCT) %in% input$sub_top
  #         )
  #       ) %>%
  #       mutate(PRODUCT = factor(PRODUCT, levels = input$sub_top)) %>%
  #       # toplist()[1:(1 + as.integer(input$sub_top))])) %>%
  #       gather(
  #         Date,
  #         Value,
  #         -AUDIT,
  #         -Category,
  #         -Sub.category,
  #         -CORPORATE,
  #         -PRODUCT,-COMPS.DESC,
  #         -pro.code,
  #         -MANUF.TYPE.DESC,
  #         -TC.III.SHORT.DESC,-TC.III.DESC,
  #         -Category_CN,
  #         -COMPS.DESC,
  #         -Brand_CN,
  #         -MANU_CN,-Measure,
  #         -Index
  #       ) %>%
  #       mutate(Date = as.Date(ymd(paste(
  #         Date, "01", "."
  #       ))),
  #       Value = as.numeric(gsub(",", "", Value))) %>%
  #       spread(Date, Value) %>%
  #       arrange(PRODUCT)
  #   }
  #   plot_data
  # })
  # plot_data_p1 <- reactive({
  #   if (input$goButton == 0) {
  #     return(NULL)
  #   }
  #   
  #   
  #   if ("RENMINBI"  %in% input$value) {
  #     if ("ALL" %in% input$province) {
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region,  province())),]
  #     } else {
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, input$province)),]
  #     }
  #     
  #     if ("Out hospital" ==  input$category && length(input$category) == 1) { # input$category == "Out hospital"
  #       summary <- summary %>%
  #         mutate(CORPORATE.DESC =
  #                  ifelse(PRODUCT.DESC %in% c("ATROVENT B.I", "COMBIVENT B.I"),
  #                         "999", CORPORATE.DESC))
  #     } 
  #     
  #     
  #     rmb <- rbind.fill(
  #       graph1(
  #         summary,
  #         cate = input$category,
  #         subcate = input$subcategory,
  #         value = "RENMINBI",
  #         period = input$period,
  #         kpi = c("abs", "gr"),
  #         window = as.numeric(input$window),
  #         level = "corporation"
  #       ),
  #       graph1(
  #         summary,
  #         cate = input$category,
  #         subcate = input$subcategory,
  #         value = "RENMINBI",
  #         period = input$period,
  #         kpi = c("abs", "gr"),
  #         window = as.numeric(input$window),
  #         level = "molecule"
  #       )
  #     )
  #     rmb <- distinct(rmb)
  #     
  #   } else{
  #     rmb <- NULL
  #   }
  #   
  #   
  #   if ("UNIT" %in% input$value) {
  #     if ("ALL" %in% input$province) {
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region,  province())),]
  #     } else{
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, input$province)),]
  #     }
  #     
  #     unit <- rbind.fill(
  #       graph1(
  #         summary,
  #         cate = input$category,
  #         subcate = input$subcategory,
  #         value = "UNIT",
  #         period = input$period,
  #         kpi = c("abs", "gr"),
  #         window = as.numeric(input$window),
  #         level = "corporation"
  #       ),
  #       graph1(
  #         summary,
  #         cate = input$category,
  #         subcate = input$subcategory,
  #         value = "UNIT",
  #         period = input$period,
  #         kpi = c("abs", "gr"),
  #         window = as.numeric(input$window),
  #         level = "molecule"
  #       )
  #     )
  #     unit <- distinct(unit)
  #     
  #   } else{
  #     unit <- NULL
  #   }
  #   
  #   if ("DOT"  %in% input$value) {
  #     if ("ALL" %in% input$province) {
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region,  province())),]
  #     } else{
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, input$province)),]
  #     }
  #     
  #     dot <- rbind.fill(
  #       graph1(
  #         summary,
  #         cate = input$category,
  #         subcate = input$subcategory,
  #         value = "DOT",
  #         period = input$period,
  #         kpi = c("abs", "gr"),
  #         window = as.numeric(input$window),
  #         level = "corporation"
  #       ),
  #       graph1(
  #         summary,
  #         cate = input$category,
  #         subcate = input$subcategory,
  #         value = "DOT",
  #         period = input$period,
  #         kpi = c("abs", "gr"),
  #         window = as.numeric(input$window),
  #         level = "molecule"
  #       )
  #     )
  #     dot <- distinct(dot)
  #   } else{
  #     dot <- NULL
  #   }
  #   
  #   result1 <- rbind(rmb, unit, dot)
  #   
  #   
  #   
  #   result1$Measure[which(result1$Measure == "RENMINBI")] <- "RMB"
  #   
  #   if (is.null(input$sub_measure) || is.null(input$sub_region)) {
  #     return(NULL)
  #   } else {
  #     test <- result1 %>%
  #       filter(Measure %in% input$sub_measure) %>%
  #       gather(key = "date", value = "value", -c(AUDIT.DESC:Index)) %>%
  #       spread(Index, value) %>%
  #       filter(PRODUCT.DESC == "Overall", COMPS.DESC == "Overall",
  #              (CORPORATE.DESC == "B.INGELHEIM" |
  #                 CORPORATE.DESC == "Overall")) %>% 
  #       select(AUDIT.DESC, CORPORATE.DESC, Measure, date, ABS, GR = `GR%`) %>%
  #       # select(AUDIT.DESC, CORPORATE.DESC, Measure, date) %>%
  #       # # rename(GR = `GR%`) %>%
  #       # mutate(ABS = 1, GR = 1) %>%
  #       filter(AUDIT.DESC %in% input$sub_region)
  #   }
  #   
  #   
  #   
  #   # test <- outputtable %>% filter(AUDIT.DESC %in% input$sub_region) %>%
  #   #   select(AUDIT.DESC, CORPORATE.DESC, Measure, date, ABS, GR = `GR%`)
  #   # test <- test[, c(1, 4, 15, 16, 17, 18)]
  #   # colnames(test)[5] <- "ABS"
  #   # colnames(test)[6] <- "GR"
  #   # colnames(test)[which(colnames(test) == "GR%")] <- "GR"
  #   test1 <- setDT(test)
  #   test1 <- data.table::dcast(test1,
  #                              AUDIT.DESC +  Measure + date ~ CORPORATE.DESC,
  #                              value.var = c("ABS", "GR"))
  #   test1 <- setDF(test1) %>%
  #     mutate(Date = paste(substr(date, 1, 4), ".", 
  #                         substr(date, 5, 6), sep = ""))
  #   
  #   test1
  # })
  # 
  # plot_data_c <- reactive({
  #   if (is.null(ot()))
  #     return(NULL)
  #   
  #   ot <- ot()$result3_c
  #   ot <- filter(
  #     ot,
  #     (CORPORATE.DESC != "Overall") |
  #       (CORPORATE.DESC == "Overall" &
  #          COMPS.DESC == "Overall")
  #   )
  #   
  #   if ("ALL" %in% input$province) {
  #     summary <- summary()[which(summary()$AUDIT.DESC %in%
  #                                  c("China", input$region,  province())),]
  #   } else {
  #     summary <- summary()[which(summary()$AUDIT.DESC %in%
  #                                  c("China", input$region, input$province)),]
  #   }
  #   
  #   
  #   top_brand <- graph1m(
  #     summary,
  #     cate = input$category,
  #     subcate = input$subcategory,
  #     value = "RENMINBI",
  #     period = input$period,
  #     kpi = "abs",
  #     window = 1,
  #     level = "corporation",
  #     top = as.integer(input$top2)
  #   ) %>%
  #     select(AUDIT.DESC, PRODUCT.DESC, CORPORATE.DESC)
  #   
  #   ot <- ot %>% inner_join(top_brand)
  #   
  #   ot$AUDIT.DESC <- gsub(" .*$", "", ot$AUDIT.DESC)
  #   
  #   
  #   ot$PRODUCT.DESC <- as.character(ot$PRODUCT.DESC)
  #   ot$CORPORATE.DESC <- as.character(ot$CORPORATE.DESC)
  #   
  #   ot$PRODUCT.DESC[which(ot$CORPORATE.DESC == "Overall")] <-
  #     ot$AUDIT.DESC[which(ot$CORPORATE.DESC == "Overall")]
  #   KK <- which(ot$PRODUCT.DESC == "Overall")
  #   
  #   ot$PRODUCT.DESC[KK] <- ot$CORPORATE.DESC[KK]
  #   ot$CORPORATE.DESC[KK] <- "Company"
  #   
  #   names(ot)[names(ot) == 'AUDIT.DESC'] <- 'AUDIT'
  #   names(ot)[names(ot) == 'CORPORATE.DESC'] <- 'CORPORATE'
  #   names(ot)[names(ot) == 'PRODUCT.DESC'] <- 'PRODUCT'
  #   
  #   ot$PRODUCT <- as.factor(ot$PRODUCT)
  #   ot$CORPORATE <- as.factor(ot$CORPORATE)
  #   ot$AUDIT <- as.factor(ot$AUDIT)
  #   
  #   ot <- as.data.frame(ot)
  #   otnum <- which(grepl("Index", names(ot)))
  #   #number here need to be updated for new update
  #   ot[which(ot$Index == "ABS"), (otnum + 1):length(ot)] <-
  #     format(ot[which(ot$Index == "ABS"), (otnum + 1):length(ot)],
  #            big.mark = ",", scientific = FALSE)
  #   ot$Measure[which(ot$Measure == "RENMINBI")] <- "RMB"
  #   
  #   if (input$sub_index_c == "ms") {
  #     tmp_sub_index <- "MS%"
  #   } else if (input$sub_index_c == "gr") {
  #     tmp_sub_index <- "GR%"
  #   } else if (input$sub_index_c == "mc") {
  #     tmp_sub_index <- "MS+/- %"
  #   } else {
  #     tmp_sub_index <- input$sub_index_c
  #   }
  #   
  #   if (as.integer(input$top) > 0) {
  #     plot_data <- ot %>%
  #       filter(
  #         Measure == input$sub_measure_c,
  #         Index == toupper(tmp_sub_index),
  #         AUDIT %in% gsub(" .*$", "", input$sub_region_c),
  #         as.character(CORPORATE) %in% c("Company", "Overall"),
  #         as.character(PRODUCT) %in% input$sub_top_c,
  #         (
  #           as.character(PRODUCT) == as.character(AUDIT) |
  #             as.character(PRODUCT) %in% input$sub_top_c
  #         )
  #       ) %>%
  #       mutate(PRODUCT = factor(PRODUCT, levels = input$sub_top_c)) %>%
  #       # toplist_c()[1:(1 + as.integer(input$sub_top_c))])) %>%
  #       gather(
  #         Date,
  #         Value,
  #         -AUDIT,
  #         -Category,
  #         -Sub.category,
  #         -CORPORATE,
  #         -PRODUCT,-COMPS.DESC,
  #         -pro.code,
  #         -MANUF.TYPE.DESC,
  #         -TC.III.SHORT.DESC,-TC.III.DESC,
  #         -Category_CN,
  #         -COMPS.DESC,
  #         -Brand_CN,
  #         -MANU_CN,-Measure,
  #         -Index
  #       ) %>%
  #       mutate(Date = as.Date(ymd(paste(
  #         Date, "01", "."
  #       ))),
  #       Value = as.numeric(gsub(",", "", Value))) %>%
  #       spread(Date, Value) %>%
  #       arrange(PRODUCT)
  #   } else {
  #     plot_data <- ot %>%
  #       filter(
  #         Measure == input$sub_measure_c,
  #         Index == toupper(tmp_sub_index),
  #         # AUDIT == gsub(" .*$", "", input$sub_region_c),
  #         as.character(CORPORATE) %in% c("Company", "Overall"),
  #         (
  #           as.character(PRODUCT) == as.character(AUDIT) |
  #             as.character(PRODUCT) %in% input$sub_top_c
  #         )
  #       ) %>%
  #       mutate(PRODUCT = factor(PRODUCT, levels = input$sub_top_c)) %>%
  #       # toplist_c()[1:(1 + as.integer(input$sub_top_c))])) %>%
  #       gather(
  #         Date,
  #         Value,
  #         -AUDIT,
  #         -Category,
  #         -Sub.category,
  #         -CORPORATE,
  #         -PRODUCT,-COMPS.DESC,
  #         -pro.code,
  #         -MANUF.TYPE.DESC,
  #         -TC.III.SHORT.DESC,-TC.III.DESC,
  #         -Category_CN,
  #         -COMPS.DESC,
  #         -Brand_CN,
  #         -MANU_CN,-Measure,
  #         -Index
  #       ) %>%
  #       mutate(Date = as.Date(ymd(paste(
  #         Date, "01", "."
  #       ))),
  #       Value = as.numeric(gsub(",", "", Value))) %>%
  #       spread(Date, Value) %>%
  #       arrange(PRODUCT)
  #   }
  #   
  #   plot_data
  # })
  # plot_data_c1 <-  reactive({
  #   if (input$goButton == 0) {
  #     return(NULL)
  #   }
  #   
  #   if ("RENMINBI"  %in% input$value) {
  #     if ("ALL" %in% input$province) {
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region,  province())),]
  #     } else {
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, input$province)),]
  #     }
  #     
  #     if ("Out hospital" ==  input$category && length(input$category) == 1) { # input$category == "Out hospital"
  #       summary <- summary%>%
  #         mutate(CORPORATE.DESC =
  #                  ifelse(PRODUCT.DESC %in% c("ATROVENT B.I", "COMBIVENT B.I"),
  #                         "999", CORPORATE.DESC))
  #     } 
  #     
  #     rmb <- rbind.fill(
  #       graph1(
  #         summary,
  #         cate = input$category,
  #         subcate = input$subcategory,
  #         value = "RENMINBI",
  #         period = input$period,
  #         kpi = c("abs", "gr"),
  #         window = as.numeric(input$window),
  #         level = "corporation"
  #       ),
  #       graph1(
  #         summary,
  #         cate = input$category,
  #         subcate = input$subcategory,
  #         value = "RENMINBI",
  #         period = input$period,
  #         kpi = c("abs", "gr"),
  #         window = as.numeric(input$window),
  #         level = "molecule"
  #       )
  #     )
  #     rmb <- distinct(rmb)
  #     
  #   } else{
  #     rmb <- NULL
  #   }
  #   
  #   
  #   if ("UNIT" %in% input$value) {
  #     if ("ALL" %in% input$province) {
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region,  province())),]
  #     } else{
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, input$province)),]
  #     }
  #     summary <- summary %>%
  #       mutate(CORPORATE.DESC =
  #                ifelse(PRODUCT.DESC %in% c("ATROVENT B.I", "COMBIVENT B.I"),
  #                       "999", CORPORATE.DESC))
  #     
  #     unit <- rbind.fill(
  #       graph1(
  #         summary,
  #         cate = input$category,
  #         subcate = input$subcategory,
  #         value = "UNIT",
  #         period = input$period,
  #         kpi = c("abs", "gr"),
  #         window = as.numeric(input$window),
  #         level = "corporation"
  #       ),
  #       graph1(
  #         summary,
  #         cate = input$category,
  #         subcate = input$subcategory,
  #         value = "UNIT",
  #         period = input$period,
  #         kpi = c("abs", "gr"),
  #         window = as.numeric(input$window),
  #         level = "molecule"
  #       )
  #     )
  #     unit <- distinct(unit)
  #     
  #   } else{
  #     unit <- NULL
  #   }
  #   
  #   if ("DOT"  %in% input$value) {
  #     if ("ALL" %in% input$province) {
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region,  province())),]
  #     } else{
  #       summary <-
  #         summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, input$province)),]
  #     }
  #     summary <- summary %>%
  #       mutate(CORPORATE.DESC =
  #                ifelse(PRODUCT.DESC %in% c("ATROVENT B.I", "COMBIVENT B.I"),
  #                       "999", CORPORATE.DESC))
  #     
  #     dot <- rbind.fill(
  #       graph1(
  #         summary,
  #         cate = input$category,
  #         subcate = input$subcategory,
  #         value = "DOT",
  #         period = input$period,
  #         kpi = c("abs", "gr"),
  #         window = as.numeric(input$window),
  #         level = "corporation"
  #       ),
  #       graph1(
  #         summary,
  #         cate = input$category,
  #         subcate = input$subcategory,
  #         value = "DOT",
  #         period = input$period,
  #         kpi = c("abs", "gr"),
  #         window = as.numeric(input$window),
  #         level = "molecule"
  #       )
  #     )
  #     dot <- distinct(dot)
  #   } else{
  #     dot <- NULL
  #   }
  #   
  #   result1 <- rbind(rmb, unit, dot)
  #   
  #   
  #   
  #   result1$Measure[which(result1$Measure == "RENMINBI")] <- "RMB"
  #   
  #   if (is.null(input$sub_measure_c) || is.null(input$sub_region_c)) {
  #     return(NULL)
  #   } else {
  #     test <- result1 %>%
  #       filter(Measure %in% input$sub_measure_c) %>%
  #       gather(key = "date", value = "value", -c(AUDIT.DESC:Index)) %>%
  #       spread(Index, value) %>%
  #       filter(PRODUCT.DESC == "Overall", COMPS.DESC == "Overall",
  #              (CORPORATE.DESC == "B.INGELHEIM" |
  #                 CORPORATE.DESC == "Overall")) %>% 
  #       select(AUDIT.DESC, CORPORATE.DESC, Measure, date, ABS, GR = `GR%`) %>%
  #       # select(AUDIT.DESC, CORPORATE.DESC, Measure, date) %>%
  #       # # rename(GR = `GR%`) %>%
  #       # mutate(ABS = 1, GR = 1) %>%
  #       filter(AUDIT.DESC %in% input$sub_region_c)
  #   }
  #   
  #   
  #   
  #   # test <- outputtable %>% filter(AUDIT.DESC %in% input$sub_region) %>%
  #   #   select(AUDIT.DESC, CORPORATE.DESC, Measure, date, ABS, GR = `GR%`)
  #   # test <- test[, c(1, 4, 15, 16, 17, 18)]
  #   # colnames(test)[5] <- "ABS"
  #   # colnames(test)[6] <- "GR"
  #   # colnames(test)[which(colnames(test) == "GR%")] <- "GR"
  #   # test1 <- setDT(test)
  #   # test1 <- data.table::dcast(test1,
  #   #                            AUDIT.DESC +  Measure + date ~ CORPORATE.DESC,
  #   #                            value.var = c("ABS", "GR"))
  #   # test1 <- setDF(test1) %>%
  #   #   mutate(date = substr(date, 3, 6))
  #   
  #   test1 <- setDT(test)
  #   test1 <- data.table::dcast(test1,
  #                              AUDIT.DESC +  Measure + date ~ CORPORATE.DESC,
  #                              value.var = c("ABS", "GR"))
  #   test1 <- setDF(test1) %>%
  #     mutate(Date = paste(substr(date, 1, 4), ".", 
  #                         substr(date, 5, 6), sep = ""))
  #   
  #   test1
  # 
  # })
  # 
  # plot_data_m <- reactive({
  #   if (input$goButton == 0) {
  #     return(NULL)
  #   }
  #   
  #   if (is.null(summary()))
  #     return(NULL)
  #   
  #   chk <- summary() %>%
  #     filter(AUDIT.DESC == "China") %>%
  #     select(AUDIT.DESC:MANU_CN, contains("mat_RENMINBI")) %>%
  #     filter(Category %in% input$category,
  #            Sub.category %in% input$subcategory)
  #   
  #   chk1 <- chk %>%
  #     select(1:(ncol(chk) - 5), (ncol(chk) - 1):ncol(chk))
  #   
  #   colnames(chk1)[(ncol(chk1) - 1):ncol(chk1)] <- c("Y1", "Y2")
  #   
  #   chk2 <- chk1 %>%
  #     group_by(Sub.category, COMPS.DESC) %>%
  #     summarise(Y1 = sum(Y1, na.rm = TRUE),
  #               Y2 = sum(Y2, na.rm = TRUE)) %>%
  #     ungroup() %>%
  #     mutate(level = 3)
  #   
  #   chk3 <- chk1 %>%
  #     group_by(Sub.category) %>%
  #     summarise(Y1 = sum(Y1, na.rm = TRUE),
  #               Y2 = sum(Y2, na.rm = TRUE)) %>%
  #     ungroup() %>%
  #     mutate(level = 2)
  #   
  #   chk4 <- chk1 %>%
  #     group_by(Sub.category = "Total Category") %>%
  #     summarise(Y1 = sum(Y1, na.rm = TRUE),
  #               Y2 = sum(Y2, na.rm = TRUE)) %>%
  #     ungroup() %>%
  #     mutate(level = 1)
  #   
  #   if (unique(chk$Sub.category) == "Others") {
  #     chk5 <- bind_rows(chk4, chk2) %>%
  #       mutate(COMPS.DESC = ifelse(is.na(COMPS.DESC), Sub.category, COMPS.DESC),
  #              GR = Y2 / Y1 - 1,
  #              Sub.category = factor(Sub.category, 
  #                                    levels = c(setdiff(unique(Sub.category), 
  #                                                       "Total Category"), "Total Category"))) %>%
  #       arrange(Sub.category, desc(level)) %>%
  #       mutate(COMPS.DESC = factor(COMPS.DESC, levels = COMPS.DESC),
  #              level1 = ifelse(level == 1, "Total Category", 
  #                              ifelse(level == 2, "Sub-category",
  #                                     "Molecule")),
  #              GR = ifelse(is.na(GR), 0, GR))
  #   } else {
  #     chk5 <- bind_rows(chk4, chk3, chk2) %>%
  #       mutate(COMPS.DESC = ifelse(is.na(COMPS.DESC), Sub.category, COMPS.DESC),
  #              GR = Y2 / Y1 - 1,
  #              Sub.category = factor(Sub.category, 
  #                                    levels = c(setdiff(unique(Sub.category), 
  #                                                       "Total Category"), "Total Category"))) %>%
  #       arrange(Sub.category, desc(level)) %>%
  #       mutate(COMPS.DESC = factor(COMPS.DESC, levels = COMPS.DESC),
  #              level1 = ifelse(level == 1, "Total Category", 
  #                              ifelse(level == 2, "Sub-category",
  #                                     "Molecule")),
  #              GR = ifelse(is.na(GR), 0, GR))
  #   }
  #   chk5
  #   })
  # 
  # plot_data_m1 <- reactive({
  #   if (input$goButton == 0) {
  #     return(NULL)
  #   }
  #   if (is.null(summary()))
  #     return(NULL)
  #   chk <- summary() %>%
  #     filter(AUDIT.DESC == "China") %>%
  #     select(AUDIT.DESC:MANU_CN, contains("mat_RENMINBI")) %>%
  #     filter(Category %in% input$category,
  #            Sub.category %in% input$subcategory)
  #   
  #   chk1 <- chk %>%
  #     select(1:(ncol(chk) - 5), (ncol(chk) - 1):ncol(chk))
  #   
  #   colnames(chk1)[(ncol(chk1) - 1):ncol(chk1)] <- c("Y1", "Y2")
  #   
  #   chk5 <- chk1 %>% 
  #     mutate(Type = ifelse(MANUF.TYPE.DESC %in% c("IMPORT", "JOINT-VENTURE"),
  #                          "MNC", "Local")) %>%
  #     group_by(COMPS.DESC = "Total Category",
  #              Type) %>%
  #     summarise(Y2 = sum(Y2, na.rm = TRUE)) %>%
  #     ungroup() %>%
  #     spread(key = Type, value = Y2) %>%
  #     mutate(Local = ifelse(is.na(Local), 0, Local),
  #            MNC = ifelse(is.na(MNC), 0, MNC),
  #            Local_sh = round(Local / (Local + MNC) * 100, 2),
  #            MNC_sh = round(MNC / (Local + MNC) * 100, 2),
  #            Local = ifelse(is.na(Local_sh), 0, Local_sh),
  #            MNC = ifelse(is.na(MNC_sh), 0, MNC_sh))
  #   
  #   chk6 <- chk1 %>% 
  #     mutate(Type = ifelse(MANUF.TYPE.DESC %in% c("IMPORT", "JOINT-VENTURE"),
  #                          "MNC", "Local")) %>%
  #     group_by(COMPS.DESC,
  #              Type) %>%
  #     summarise(Y2 = sum(Y2, na.rm = TRUE)) %>%
  #     ungroup() %>%
  #     spread(key = Type, value = Y2) %>%
  #     mutate(Local = ifelse(is.na(Local), 0, Local),
  #            MNC = ifelse(is.na(MNC), 0, MNC),
  #            Local_sh = round(Local / (Local + MNC) * 100, 2),
  #            MNC_sh = round(MNC / (Local + MNC) * 100, 2),
  #            Local = ifelse(is.na(Local_sh), 0, Local_sh),
  #            MNC = ifelse(is.na(MNC_sh), 0, MNC_sh))
  #   
  #   chk7 <- bind_rows(chk5, chk6) %>%
  #     # select(-c(Local_sh, MNC_sh)) %>%
  #     # gather(key = type, value = share, -COMPS.DESC) %>%
  #     mutate(COMPS.DESC = factor(COMPS.DESC, 
  #                                levels = c(setdiff(unique(COMPS.DESC), 
  #                                                   "Total Category"),
  #                                           "Total Category")))
  #   
  #   chk7
  #   
  # })
  # 
  # output$downloadData_p <- downloadHandler(
  #   filename = function() {
  #     paste("trend_plot_data_by_product_",
  #           input$period,
  #           "_",
  #           input$window,
  #           "year",
  #           '.xlsx',
  #           sep = '')
  #   },
  #   content = function(file) {
  #     write.xlsx(plot_data_p(), file, overwrite = TRUE)
  #   }
  # )
  # 
  # output$downloadData_p1 <- downloadHandler(
  #   filename = function() {
  #     paste("bar_plot_data_by_product_",
  #           input$period,
  #           "_",
  #           input$window,
  #           "year",
  #           '.xlsx',
  #           sep = '')
  #   },
  #   content = function(file) {
  #     write.xlsx(plot_data_p1(), file, overwrite = TRUE)
  #   }
  # )
  # 
  # output$downloadData_c <- downloadHandler(
  #   filename = function() {
  #     paste(
  #       "plot_data_by_corporation_",
  #       input$period,
  #       "_",
  #       input$window,
  #       "year",
  #       '.xlsx',
  #       sep = ''
  #     )
  #   },
  #   content = function(file) {
  #     write.xlsx(plot_data_c(), file, overwrite = TRUE)
  #   }
  # )
  # 
  # output$downloadData_c1 <- downloadHandler(
  #   filename = function() {
  #     paste(
  #       "bar_plot_data_by_corporation_",
  #       input$period,
  #       "_",
  #       input$window,
  #       "year",
  #       '.xlsx',
  #       sep = ''
  #     )
  #   },
  #   content = function(file) {
  #     write.xlsx(plot_data_c1(), file, overwrite = TRUE)
  #   }
  # )
  # 
  # output$downloadData_m <- downloadHandler(
  #   filename = function() {
  #     paste("bar_plot_data_by_sub_cat_molecule_",
  #           '.xlsx',
  #           sep = '')
  #   },
  #   content = function(file) {
  #     write.xlsx(#plot_data_m(), 
  #       molecule_plot_data,
  #       file, 
  #       overwrite = TRUE
  #     )
  #   }
  # )
  # 
  # output$downloadData_m1 <- downloadHandler(
  #   filename = function() {
  #     paste("ms_plot_data_by_sub_cat_molecule_",
  #           '.xlsx',
  #           sep = '')
  #   },
  #   content = function(file) {
  #     write.xlsx(#plot_data_m1(), 
  #       bar_molecule_plot_data,
  #       file, 
  #       overwrite = TRUE)
  #   }
  # )
  
  
}
