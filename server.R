source('share_load.R')

server <- 
  function(input, output, session) {
    
    # Reactive expression to create the color palette based on the input data
    pal <- reactive({
      color_palette <- colorRampPalette(c("#FFEDA0", "#FEB24C", "#FC4E2A", "#E31A1C", "#BD0026", "#800026"))
      colorNumeric(palette = color_palette(6), domain = merged_data_sf$accidents_per_km)
    })
    
    # Render the Leaflet map
    output$map <- renderLeaflet({
      leaflet(merged_data_sf, options = leafletOptions(minZoom = 11, zoomControl = FALSE)) %>%
        addProviderTiles(providers$Esri.WorldImagery) %>%
        setView(lng = 69.25411937864908, lat = 41.31687112633275, zoom = 15) %>%
        addPolygons(
          fillColor = ~pal()(accidents_per_km),  # Use the reactive palette
          color = "#FFFFFF",
          weight = 1,
          smoothFactor = 0.5,
          opacity = 0.8,
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            weight = 2,
            color = "#666666",
            fillOpacity = 0.9,
            bringToFront = TRUE
          ),
          popup = ~paste("<b>Ko'cha nomi:</b>", name_latin, "<br/>", 
                         "<b>Har 1 kmga hodisalar soni:</b>", round(accidents_per_km, 0)),
          labelOptions = labelOptions(
            style = list("background" = "rgba(255, 255, 255, 0.8)", "font-weight" = "normal", "padding" = "5px", "border" = "1px solid #cccccc", "border-radius" = "4px"),
            textsize = "15px",
            direction = "auto",
            html = TRUE)
        ) %>%
        
        addLegend(
          pal = pal(),  # Use the reactive palette
          values = ~accidents_per_km,
          opacity = 1,
          title = "Har 1 kmga hodisalar soni",
          position = "bottomright"
        ) 
      
      
    })
    
    data <- dataset_clean %>% 
      count(accident_type_name_uz)
    # Assuming 'data' is available in the server environment or passed as a reactive expression
    
    #plot1
    
    output$plot1 <- renderPlotly({
        # Your plot_ly code
        plot <- plot_ly(data, 
                        labels = ~accident_type_name_uz, 
                        values = ~n, 
                        type = 'pie', 
                        hole = 0.4, 
                        hoverinfo = "label+percent") %>%
          layout(
            legend = list(x = 1.05, 
                          y = 0.5, 
                          orientation = 'v'),
            plot_bgcolor = "transparent",
            paper_bgcolor = "transparent"
          ) %>%
          config(displaylogo = FALSE, 
                 modeBarButtons = list(list("toImage"),
                                       list("zoomIn2d"),
                                       list("zoomOut2d"),
                                       list("resetScale2d"))
          )
      })
    
    data1 <- dataset_clean %>%  
      count(road_condition_name_uz)
    data1 <- na.omit(data1)
    
    #plot2
    
    
      output$plot2 <- renderPlotly({
        data1 <- data1 %>%  # Assuming 'data1' is already defined, else you need to read or compute it here
          count(road_condition_name_uz)
        
        plot <- plot_ly(data1, 
                        labels = ~road_condition_name_uz, 
                        values = ~n, 
                        type = 'pie', 
                        hole = 0.4, 
                        hoverinfo = "label+percent") %>%
          layout(
            legend = list(x = 1.05, 
                          y = 0.5, 
                          orientation = 'v'),
            plot_bgcolor = "transparent",
            paper_bgcolor = "transparent"
          ) %>%
          config(displaylogo = FALSE, 
                 modeBarButtons = list(list("toImage"),
                                       list("zoomIn2d"),
                                       list("zoomOut2d"),
                                       list("resetScale2d"))
          )
      })
    
    
    data2 <- dataset_clean %>%  
      count(weather_condition_name_uz)
    glimpse(data2)
    data1 <- na.omit(data2)
    
    #plot3
    
      output$plot3 <- renderPlotly({
        # If 'data1' needs to be processed or is a reactive value, do it here
        plot_ly(data1, 
                labels = ~weather_condition_name_uz, 
                values = ~n, 
                type = 'pie', 
                hole = 0.4, 
                hoverinfo = "label+percent") %>%
          layout(
            legend = list(x = 1.05, 
                          y = 0.5, 
                          orientation = 'v'),
            plot_bgcolor = "transparent",
            paper_bgcolor = "transparent"
          ) %>%
          config(displaylogo = FALSE, 
                 modeBarButtons = list(list("toImage"),
                                       list("zoomIn2d"),
                                       list("zoomOut2d"),
                                       list("resetScale2d"))
          )
      })
    
      data_age_s <- data_c %>%
        group_by(age_segments) %>%
        count()
      
      #plot4
      
      output$plot4 <- renderPlotly({
          # If 'data_age_s' is reactive, ensure you call it as a function e.g., data_age_s()
          plot_ly(data_age_s, 
                  labels = ~age_segments, 
                  values = ~n, 
                  type = 'pie', 
                  hole = 0.4, 
                  hoverinfo = "label+percent") %>%
            layout(
              legend = list(x = 1.05, 
                            y = 0.5, 
                            orientation = 'v'),
              plot_bgcolor = "transparent",
              paper_bgcolor = "transparent"
            ) %>%
            config(displaylogo = FALSE, 
                   modeBarButtons = list(list("toImage"),
                                         list("zoomIn2d"),
                                         list("zoomOut2d"),
                                         list("resetScale2d"))
            )
        })
      
      data_violation <- dataset_clean %>%
        group_by(len_violation_sets) %>%
        count()
      
      #plot5
      
      
        output$plot5 <- renderPlotly({
          # If 'data_violation' is a reactive data source, it should be called as data_violation()
          plot_ly(data_violation, 
                  labels = ~len_violation_sets, 
                  values = ~n, 
                  type = 'pie', 
                  hole = 0.4, 
                  hoverinfo = "label+percent") %>%
            layout(
              legend = list(x = 1.05, 
                            y = 0.5, 
                            orientation = 'v'),
              plot_bgcolor = "transparent",
              paper_bgcolor = "transparent"
            ) %>%
            config(displaylogo = FALSE, 
                   modeBarButtons = list(list("toImage"),
                                         list("zoomIn2d"),
                                         list("zoomOut2d"),
                                         list("resetScale2d"))
            )
        })
      
      
      data_violation1 <- dataset_clean %>%
        filter(len_violation_sets == 0) %>%
        group_by(accident_type_name_uz) %>%
        count()
      
      #plot6
      
        output$plot6 <- renderPlotly({
          # If 'data_violation1' is reactive, you should call it as a function e.g., data_violation1()
          plot_ly(data_violation1, 
                  labels = ~accident_type_name_uz, 
                  values = ~n, 
                  type = 'pie', 
                  hole = 0.4, 
                  hoverinfo = "label+percent") %>%
            layout(
              legend = list(x = 1.05, 
                            y = 0.5, 
                            orientation = 'v'),
              plot_bgcolor = "transparent",
              paper_bgcolor = "transparent"
            ) %>%
            config(displaylogo = FALSE, 
                   modeBarButtons = list(list("toImage"),
                                         list("zoomIn2d"),
                                         list("zoomOut2d"),
                                         list("resetScale2d"))
            )
        })
      
        databar <- dataset_clean %>%
          group_by(day_hour) %>%
          count()
        
        color_scale <- colorRamp("#E51A4B") 
        
        # values to a 0-1 scale
        normalized_values <- (databar$n - min(databar$n)) / (max(databar$n) - min(databar$n))
        
        # scale the color to the normalized values
        bar_colors <- apply(matrix(normalized_values), 1, function(v) rgb(color_scale(v), maxColorValue = 255))
        
        #plot7
        
          output$plot7 <- renderPlotly({
            plot_ly(databar, x = ~day_hour, y = ~n, type = 'bar', marker = list(color = bar_colors)) %>%
              layout(
                xaxis = list(
                  title = "Vaqt",
                  showticklabels = TRUE,
                  tickangle = 360, 
                  tickvals = ~day_hour, 
                  ticktext = ~day_hour  
                ),
                yaxis = list(title = "Sodir etilgan YTHlari soni"),
                plot_bgcolor = "transparent",
                paper_bgcolor = "transparent"
              ) %>%
              config(displaylogo = FALSE, 
                     modeBarButtons = list(list("toImage"),
                                           list("zoomIn2d"),
                                           list("zoomOut2d"),
                                           list("resetScale2d"))
              )
          })
        
          #plot8
          
            output$plot8 <- renderPlotly({
              plot_ly(databar, 
                      labels = ~day_hour, 
                      values = ~n, 
                      type = 'pie', 
                      hole = 0.4, 
                      hoverinfo = "label+percent") %>%
                layout(
                  legend = list(x = 1.05, 
                                y = 0.5, 
                                orientation = 'v'),
                  plot_bgcolor = "transparent",
                  paper_bgcolor = "transparent"
                ) %>%
                config(displaylogo = FALSE, 
                       modeBarButtons = list(list("toImage"),
                                             list("zoomIn2d"),
                                             list("zoomOut2d"),
                                             list("resetScale2d"))
                )
            })
            
            databar <- dataset_clean %>%
              group_by(day_name, day_of_week) %>%
              count()
            
            # 2. barchart
            color_scale <- colorRamp("#E51A4B") 
            
            # values to a 0-1 scale
            normalized_values <- (databar$n - min(databar$n)) / (max(databar$n) - min(databar$n))
            
            # scale the color to the normalized values
            bar_colors <- apply(matrix(normalized_values), 1, function(v) rgb(color_scale(v), maxColorValue = 255))
            
            
            databar <- databar %>%
              arrange(day_of_week)
            
        #plot9
            
                output$plot9 <- renderPlotly({
                plot_ly(databar, x = ~day_name, y = ~n, type = 'bar', marker = list(color = bar_colors)) %>%
                  layout(
                    xaxis = list(
                      title = "",
                      showticklabels = TRUE,
                      tickangle = 315, 
                      tickvals = ~day_name, 
                      ticktext = ~day_name  
                    ),
                    yaxis = list(title = "Sodir etilgan YTHlari soni"),
                    plot_bgcolor = "transparent",
                    paper_bgcolor = "transparent"
                  ) %>%
                  config(displaylogo = FALSE, 
                         modeBarButtons = list(list("toImage"),
                                               list("zoomIn2d"),
                                               list("zoomOut2d"),
                                               list("resetScale2d"))
                  )
              })
            
          
            #plot10
                  output$plot10 <- renderPlotly({
                    # If 'databar' is reactive, you should call it as a function e.g., databar()
                    plot_ly(databar, 
                            labels = ~day_name, 
                            values = ~n, 
                            type = 'pie', 
                            hole = 0.4, 
                            hoverinfo = "label+percent") %>%
                      layout(
                        legend = list(x = 1.05, 
                                      y = 0.5, 
                                      orientation = 'v'),
                        plot_bgcolor = "transparent",
                        paper_bgcolor = "transparent"
                      ) %>%
                      config(displaylogo = FALSE, 
                             modeBarButtons = list(list("toImage"),
                                                   list("zoomIn2d"),
                                                   list("zoomOut2d"),
                                                   list("resetScale2d"))
                      )
                  })
                
                
  }