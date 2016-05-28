shinyServer(function(input, output) {
  
  v <- reactiveValues(hi_id = 0, msg = '') # set default to GLOBAL = 0
  
  # elements tab ----
  
  output$network <- renderVisNetwork({

    # plot network
    visNetwork(nodes, edges) %>% # , width = '100%'
      visOptions(highlightNearest=T, nodesIdSelection=T) %>% #, selectedBy = "group") %>%
      visEdges(arrows = "from") %>%
      visHierarchicalLayout(
        direction = "LR", 
        sortMethod = 'directed', # parentCentralization = F, # blockShifting = T, edgeMinimization = T,
        levelSeparation = 500) %>% #
      visGroups(groupname = 'goal'                , color = '#97FFFF', shape = 'circle') %>%   # darkslategray1 - darkslategray3 (#79CDCD)
      visGroups(groupname = 'subgoal'             , color = '#8DEEEE', shape = 'circle') %>%   # http://research.stowers-institute.org/efg/R/Color/Chart/ColorChart.pdf
      visGroups(groupname = 'layer for status'    , color = '#79CDCD', shape = 'box') %>%      # http://research.stowers-institute.org/efg/R/Color/Chart/ColorChart.pdf
      visGroups(groupname = 'layer for pressures' , color = '#FFB6C1', shape = 'box') %>%      # lightpink
      visGroups(groupname = 'layer for resilience', color = '#98FB98', shape = 'box') %>%      # palegreen
      visLegend() %>%
      visInteraction(
        navigationButtons = T, 
        keyboard = TRUE, tooltipDelay = 0) %>%
      visExport()
    
  })
  
  observeEvent(input$network_selected, {
    #v$msg <- paste("network_selected", input$network_selected)
  })
  observeEvent(input$network_selectedBy, {
    #v$msg <- paste("network_selectedBy", input$network_selectedBy)
  })
  
  output$message <- renderText(v$msg)
})
