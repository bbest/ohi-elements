dashboardPage(
  
  dashboardHeader(title="Ocean Health Index"),
  
  dashboardSidebar(
    
    sidebarMenu(
      id = 'sidebarmenu',
      
      menuItem("Explore Data", tabName='explore',icon=icon("globe",lib='font-awesome')),
  
      menuItem("Navigate Components", tabName='navigate',icon=icon("sitemap",lib='font-awesome'), selected=T))),
      
  dashboardBody(
    tabItems(
      
      tabItem(
        tabName='explore',
        h2("Explore Data")),
      
      tabItem(
        tabName='navigate',
        h2("Navigate Components"),
        visNetworkOutput("network", height = '800px'),
        textOutput("message", container = h3))
      
      )))