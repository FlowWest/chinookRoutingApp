function(input, output, session) {


  # there is a linear correlation between the two
  observeEvent(input$q_vern, {
    updateNumericInput(session, "q_stck",
                       max = (-4.155134 + 0.429387 * input$q_vern)*1.3,
                       min = (-4.155134 + 0.429387 * input$q_vern)*0.7)
  })

  observeEvent(input$temp_vern, {
    updateNumericInput(session, "temp_pp",
                       max = (0.91483 + 0.88525 * input$temp_vern)*1.2,
                       min = (0.91483 + 0.88525 * input$temp_vern)*0.8)
  })

  observeEvent(input$clear_selected_routing_points, {
    chinook_routing_locations_selected$data <- NULL

    leafletProxy("chinook_routing_map") %>%
      clearGroup("selected_points")
  })

  cmap <- colorFactor("Dark2", domain = chinook_regions$Id)

  # NOTE: these need to be refactored...
  # chinook_routing <- reactiveVal(NULL)
  chinook_routing_locations_selected <- reactiveValues(data=NULL)


  chinook_routing <- reactive({
    DeltaS(
      as.numeric(input$dcc_open),
      as.numeric(input$hor_barr),
      as.numeric(input$bio_fence),
      input$q_free,
      input$q_vern,
      input$q_stck,
      input$temp_vern,
      input$temp_pp,
      input$cvp_exp,
      input$swp_exp,
      input$fish_above_freeport,
      input$fish_above_vernalis,
      input$fl) %>%
    tibble(
      loc_id = names(.),
      value = .
    ) %>%
      left_join(chinook_routing_points, by=c("loc_id"="location_id")) %>%
      mutate(proportion_at_location = value/sum(value),
             percent_at_location = proportion_at_location * 100) %>%
      select(
        location,
        location_id = loc_id,
        value = value,
        lat,
        lng,
        proportion_at_location,
        percent_at_location
      ) %>%
      mutate(
        location = factor(location),
        location = fct_reorder(location, value, .desc = TRUE)
      )
  })



  output$chinook_routing_map <- renderLeaflet({
    pal <- colorNumeric("YlGnBu", domain = chinook_routing()$value)

    leaflet(chinook_routing()) %>%
      addProviderTiles(provider = providers$Esri.WorldTopoMap, group="Topo") %>%
      addProviderTiles(provider = providers$Esri.WorldImagery, group = "Imagery",
                       options = leafletOptions(opacity=0.6)) %>%
      # addPolygons(
      #   weight = 2,
      #   fillOpacity = .5,
      #   fillColor = "#8c8c8c",
      #   color="#8c8c8c",
      #   popup=~paste0("<b>", Id, "</b>"),
      #   group = "Chinook Regions") %>%
      # addCircleMarkers(data=chinook_dropoff_locations,
      #                  fillColor = "#ffad33", color="#ffad33",
      #                  fillOpacity = .6, label=~location) %>%
      addCircleMarkers(
                       label=~paste(location),
                       weight=1, fillOpacity = .8,
                       layerId = ~location_id,
                       group = "Routing Points",
                       fillColor = ~pal(value),
                       popup = ~paste0(
                         "<b> Total Survival: ", value, "</b><br>",
                         "<b> Proportion: ", round(proportion_at_location, 3), "</b><br>",
                         "<b> Percent: ", round(percent_at_location, 2), "%</b><br>"
                       )
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = ~ value,
        group = "Routing Points"
      ) %>%
      addLayersControl(
        baseGroups = c("Topo", "Imagery")
        # ,
        # overlayGroups = c("Chinook Regions")
      )
  })

  plot_range <- reactive(
    c(0, max(input$fish_above_freeport, input$fish_above_vernalis))
  )

  output$chinook_routing_plot <- renderPlotly({

    chinook_routing() %>%
      plot_ly(x=~location, y=~value, type='bar',
              marker = list(color = "#424242"),
              text = ~paste0("<b>Proportion:</b> ", round(proportion_at_location, 3), "<br>",
                             "<b>Total:</b> ", value),
              hoverinfo = "text") %>%
      layout(xaxis = list(title=""),
             yaxis=list(title="Survival",
                        range = plot_range())) #TODO: grab the largest of the two inputs in this section

  })

  output$chinook_routing_table <- DT::renderDataTable({
    chinook_routing() %>%
      transmute(`Location`=location,
             `Abundance` = value,
             `Proportion Survival` = round(proportion_at_location, 3),
             `Percent Survival` = round(percent_at_location, 2)) %>%
      DT::datatable(options = list(dom = 't'))
  })

  # Help modals --------------------------------------------------------
  observeEvent(input$help_with_q_vern, {
    showModal(modalDialog(
      title = "Flow at Vernalis",
      tagList(
        tags$p("Flow at Vernallis is correlated with flow at Stockton.
              The application will change the allowed values based on
              a quantified version of this correlation.")),
      easyClose = TRUE
    ))
  })

  # observeEvent({
  #   as.numeric(input$dcc_open)
  #   as.numeric(input$hor_barr)
  #   as.numeric(input$bio_fence)
  #   input$q_free
  #   input$q_vern
  #   input$q_stck
  #   input$temp_vern
  #   input$temp_pp
  #   input$cvp_exp
  #   input$swp_exp
  #   input$fish_above_freeport
  #   input$fish_above_vernalis
  #   input$fl
  # }, {
  #   pal <- colorNumeric("YlGnBu", domain = chinook_routing()$value)
  #
  #   leafletProxy("chinook_routing_map", data=chinook_routing()) %>%
  #     clearGroup("Routing Points") %>%
  #     clearControls() %>%
  #     clearPopups() %>%
  #     clearMarkers() %>%
  #     addCircleMarkers(
  #       label=~paste(location_id),
  #       weight=1, fillOpacity = .8,
  #       layerId = ~location_id,
  #       group = "Routing Points",
  #       fillColor = ~pal(value)
  #     ) %>%
  #     addLegend(
  #       "bottomright",
  #       pal = pal,
  #       values = ~ value,
  #       group = "Routing Points"
  #     )
  # }, ignoreInit = TRUE)

}
