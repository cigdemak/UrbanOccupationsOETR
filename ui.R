source("ui_occupational_analysis.R")
source("ui_population_analysis.R")
# source("ui_agricultural_analysis.R")
source("ui_about.R")
shinyUI(
  dashboardPage(
    header = dashboardHeader(title = "UrbanOccupationsOETR", titleWidth = 200),
    sidebar = dashboardSidebar(width = 200, 
                               sidebarMenu(id = "tab",
                                           menuItem(
                                             text = "Occupational Structure",
                                             tabName = "occupational_analysis",
                                             icon = icon("briefcase")
                                           ),
                                           menuItem(
                                             text = "Population Geography",
                                             tabName = "population_analysis",
                                             icon = icon("venus-mars")
                                           ), 
                                           # menuItem(
                                           #   text = "Agricultural Production",
                                           #   tabName = "agricultural_analysis",
                                           #   icon = icon("seedling")
                                           # ),
                                           menuItem(
                                             text = "About",
                                             tabName = "about",
                                             icon = icon("info-circle")
                                           )
                               )),
    body = dashboardBody(
      tabItems(ui_occupational_analysis, 
               ui_population_analysis,
               # ui_agricultural_analysis,
               ui_about
               
      ),
      tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 14px;
      }
    '))),
      skin = "black")
  )
)