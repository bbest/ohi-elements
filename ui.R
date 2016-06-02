dashboardPage(
  
  dashboardHeader(title="Ocean Health Index"),
  
  dashboardSidebar(
    
    sidebarMenu(
      id = 'sidebarmenu',
      
      sidebarSearchForm(textId = "searchText", buttonId = "searchButton", label = "Search..."),
      
      menuItem("Explore Data", tabName='explore',icon=icon("globe",lib='font-awesome')),
  
      menuItem("Navigate Components", tabName='navigate',icon=icon("sitemap",lib='font-awesome'), selected=T),
    
      conditionalPanel(
        "input.sidebarmenu === 'navigate'",
        
        # paste(unique(nodes$group), collapse="','"): c('Index','goal','subgoal','layer for status','layer for pressures','layer for resilience')
        
        checkboxGroupInput(
          'nav_dims', 'Dimensions', 
          c('layer for status','layer for pressures','layer for resilience'), selected = NULL)))),
      
  dashboardBody(
    tabItems(
      
      tabItem(
        tabName='explore',
        h2("Explore Data")),
      
      tabItem(
        tabName='navigate',
        h2("Navigate Components"),
        visNetworkOutput("network", height = '600px'),
        #verbatimTextOutput('message'),
        uiOutput("message")
      ))))