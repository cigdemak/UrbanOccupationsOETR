
ui_about <-
  tabItem(tabName = "about",
          fluidRow(column(
            width = 12,
            box(
              title = "Industrialisation and Urban Growth from the mid-nineteenth century Ottoman Empire to Contemporary Turkey in a Comparative Perspective, 1850-2000",
              width = NULL,
              collapsible = FALSE,
              solidHeader = TRUE,
              status = "success",
              'This website presents social and economic census data of the Ottoman Empire and the Republic of Turkey for the project',
              tags$a(href = "https://urbanoccupations.ku.edu.tr/", "Urban Occupations OETR."),
              'Code for the site is open source, and can be found on',
              tags$a(href = "https://github.com/cigdemak/UrbanOccupationsOETR", "the this Github repository."),
              HTML("<br><br>"),
              'This project has received funding from the European Research Council (ERC) under the European Unions Horizon 2020 research and innovation programme (grant agreement No. [679097])'
            ),
            box(
              title = "Koc University",
              width = NULL,
              collapsiple = FALSE,
              solidHeader = TRUE,
              status = "danger",
              infoBox(
                title ="CIGDEM AK",
                value = "PhD",
                href = "mailto:cigdemak@ku.edu.tr",
                subtitle = "Design & Implementation",
                icon = icon("user"),
                width = 6,
                color = "aqua",
                fill = TRUE
              ),
              infoBox(
                title = "MUSTAFA ERDEM KABADAYI",
                value = "PhD",
                href = "mailto:mkabadayi@ku.edu.tr",
                subtitle = "Academic Support & Supervision",
                icon = icon("user"),
                width = 6,
                color = "aqua",
                fill = TRUE
              )
             
            )
          )))
         
         
