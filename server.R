shinyServer(function(input, output) {
  
  v <- reactiveValues(sel_id = '', msg = '') # set default to GLOBAL = 0
  
  get_network = reactive({
    
    # get network based on selection
    net = list()
    net$nodes = nodes %>%
      filter(
        group %in% c('Index','goal','subgoal', input$nav_dims)) # 'layer for status','layer for pressures','layer for resilience'
    #net$nodes = nodes
    net$edges = edges %>%
      filter(
        from %in% nodes$id,
        to %in% nodes$id)
    #net$nodes$hidden = ifelse(net$nodes$group %in% c('Index','goal','subgoal', input$nav_dims), F, T)

    # # append if selection
    # if ( (!is.null(input$network_selected) && input$network_selected != '' && input$network_selected != v$sel_id)){
    #   v$sel_id = sel_id = input$network_selected
    # } else if ( (!is.null(input$network_selected) && input$network_selected == '') & (v$sel_id > '')){
    #   sel_id = v$sel_id
    # } else {
    #   sel_id = ''
    # }
    # 
    # if (sel_id > ''){
    #   sel_edges = edges %>%
    #     filter(from == sel_id)
    #   sel_nodes = nodes %>%
    #     filter(nodes$id %in% sel_edges$to)
    #   net$nodes = bind_rows(
    #     net$nodes,
    #     sel_nodes)
    #   net$edges = bind_rows(
    #     net$edges,
    #     sel_edges)
    # }
    
    return(net)
  })
  
  # elements tab ----
  
  output$network <- renderVisNetwork({
    
    # use reactive
    #net = get_network()
    
    # skip reactive get_network()
    net = list()
    net$nodes = nodes %>%
      filter(
        group %in% c('Index','goal','subgoal')) # , input$nav_dims # 'layer for status','layer for pressures','layer for resilience'
    #net$nodes = nodes
    net$edges = edges %>%
      filter(
        from %in% nodes$id,
        to %in% nodes$id)

    # plot network
    visNetwork(net$nodes, net$edges) %>% # , width = '100%'
      visPhysics(stabilization=F) %>%
      #visEdges(smooth=F) %>%
      #visIgraphLayout() %>% # TODO for performance, options
      visOptions(highlightNearest=T, nodesIdSelection=T) %>% #, selectedBy = "group") %>%
      #visOptions(nodesIdSelection=T) %>% #, selectedBy = "group") %>%
      #visOptions(highlightNearest=T) %>% #, selectedBy = "group") %>%
      #visOptions(highlightNearest = list(enabled=T, degree=list(from=2, to=2), hover=T, algorithm='hierarchical')) %>%
      #visEdges(arrows = "from") %>%
      # TODO levels in nodes: nodes <- data.frame(id = 1:4, level = c(2, 1, 1, 1))
      visHierarchicalLayout(
        direction = "LR", 
        #parentCentralization = F, edgeMinimization = F, blockShifting = F,
        sortMethod = 'directed', # parentCentralization = F, # blockShifting = T, edgeMinimization = T,
        levelSeparation = 400, nodeSpacing = 200) %>% #
      visGroups(groupname = 'goal'                , color = '#97FFFF', shape = 'circle') %>%   # darkslategray1 - darkslategray3 (#79CDCD)
      visGroups(groupname = 'subgoal'             , color = '#8DEEEE', shape = 'circle') %>%   # http://research.stowers-institute.org/efg/R/Color/Chart/ColorChart.pdf
      visGroups(groupname = 'layer for status'    , color = '#79CDCD', shape = 'box') %>%      # http://research.stowers-institute.org/efg/R/Color/Chart/ColorChart.pdf
      visGroups(groupname = 'layer for pressures' , color = '#FFB6C1', shape = 'box') %>%      # lightpink
      visGroups(groupname = 'layer for resilience', color = '#98FB98', shape = 'box') %>%      # palegreen
      visInteraction(hover=T) %>%
      #visEvents(
      #  hoverNode  = "function(nodes) {Shiny.onInputChange('network_hover', nodes);;}",
      #  selectNode = "function(nodes) {Shiny.onInputChange('network_sel', nodes);;}")
      # visLegend() # %>%
      visInteraction(
        navigationButtons = T,
        keyboard = TRUE, tooltipDelay = 0) #%>%
      # visExport()
    
  })
  
  observeEvent(input$network_sel, {
    v$msg <- paste(v$msg, "network_sel: ", input$network_selected, br())
  })
  
  observeEvent(input$network_selected, {
    v$msg <- paste(v$msg, "network_selected: ", input$network_selected, br())

    #id_goals = nodes %>% filter(group %in% c('goal','subgoal')) %>% .$id
    #req(input$network_selected %in% id_goals)

      #input = list(); input$network_selected = 'LIV'

      sel_edges = edges %>%
        filter(from == input$network_selected)
      sel_nodes = nodes %>%
        filter(nodes$id %in% sel_edges$to)

      visNetworkProxy("network") %>%
        visUpdateNodes(sel_nodes) %>%
        visUpdateEdges(sel_edges) #%>%
        #visSetSelection(input$network_selected)
        #visFocus(id = input$Focus, scale = 4)
   #}
  })
    
  observeEvent(input$network_hover, {
    #v$msg <- paste(v$msg, "network_hover: ", input$network_hover, br())
  })
  
  #observeEvent(input$network_selectedBy, {
  #  v$msg <- paste(v$msg, "network_selectedBy: ", input$network_selectedBy, br())
  #})
  
  # sunburstr ----
  
  output$sunburst <- renderSunburst({
    #invalidateLater(1000, session)
    
    sequences <- sequences[sample(nrow(sequences),1000),]
    
    add_shiny(sunburst(
      sequences, 
      explanation = "function(d){return d.name}"))
  })
  
  selection <- reactive({
    input$sunburst_mouseover
  })
  
  output$selection <- renderText(selection())
  
  # message ----
  output$message <- renderUI({ HTML(v$msg) })
})
