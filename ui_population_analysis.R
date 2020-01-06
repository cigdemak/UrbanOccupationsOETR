ui_population_analysis <- 
  tabItem(tabName = "population_analysis",
          fluidRow(column(
            width = 3,
            box(title = "Analysis Parameters",
                width = NULL,
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "danger",
                radioButtons(inputId = "population_analysis_level",
                             label = "Analysis level:",
                             choiceNames = list("District", "Subdistrict", "Point"),
                             choiceValues = list("dist", "subdist", "point"), selected = "dist",
                             inline = TRUE
                ),
                checkboxGroupInput(
                  inputId = "population_selected_countries",
                  label = "Choose country:",
                  choiceNames = "",
                  choiceValues = "",
                  inline = TRUE
                ),
                textInput(
                  inputId = "population_result_name",
                  label = "Result Name:",
                  placeholder = "Give a name to result ..."
                ),
                actionButton(inputId = "population_add_to_list", label = "Add to Result List"),
                selectizeInput(
                  inputId = "population_result_list",
                  label = "Result List:",
                  choices = NULL,
                  selected = NULL,
                  options = list(maxItems = 1)
                )
            ),
            conditionalPanel(
              condition = "input.population_selected_countries.includes('OE') == true",
              box(
                title = "The Ottoman Empire parameters",
                width = NULL,
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "danger",
                selectInput(
                  inputId = "population_census_year_OE", 
                  label = "The Ottoman Empire Census Year:", 
                  choices = "",
                  selected = "", 
                  width = "100%"
                ),
                sliderInput(inputId = 'population_display_range_OE',
                            label = "",
                            min = 0,
                            max = 0,
                            value = c(0, 1000)
                ),
                checkboxInput(
                  inputId = "population_location_filter_OE",
                  label = "Filter by Location",
                  value = FALSE
                ),
                conditionalPanel(
                  condition = "input.tab == 'population_analysis' && input.population_location_filter_OE == true",
                  uiOutput('population_city_choices_OE')
                ),
                checkboxInput(
                  inputId = "population_er_filter_OE",
                  label = "Filter by Ethnicity/Religion",
                  value = FALSE
                ),
                conditionalPanel(
                  condition = "input.tab == 'population_analysis' && input.population_er_filter_OE == true",
                  pickerInput(inputId = "population_selected_er_OE",
                              label = "Selected Ethnicity/Religion:",
                              choices = "",
                              selected = "",
                              multiple = TRUE,
                              options = list(plugins = list("remove_button"), `actions-box` = TRUE, `live-search` = TRUE, `multiple-separator` = '|')
                  )
                ),
                radioButtons(inputId = "population_gh_filter_OE",
                             label = "",
                             choiceNames = list("Filter by Gender", "Filter by Household"),
                             choiceValues = list("population_gender_filter", "population_househould_filter"),
                             selected = "",
                             inline = FALSE
                ),
                conditionalPanel(
                  condition = "input.tab == 'population_analysis' && input.population_gh_filter_OE == 'population_gender_filter'",
                  selectizeInput(
                    inputId = "population_selected_gender_OE",
                    label = "Selected Variable:",
                    choices = "",
                    selected = "",
                    multiple = FALSE,
                    options = list(plugins = list("remove_button"))
                  )
                ),
                conditionalPanel(
                  condition = "input.tab == 'population_analysis' && input.population_gh_filter_OE == 'population_househould_filter'",
                  uiOutput('population_household_choices_OE')
                )
              )
            ),
            conditionalPanel(
              condition = "input.population_selected_countries.includes('BUL') == true",
              box(
                title = "Bulgaria parameters",
                width = NULL,
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "danger",
                selectInput(
                  inputId = "population_census_year_BUL", 
                  label = "Bulgaria Census Year:", 
                  choices = "",
                  selected = "",
                  width = "100%"
                ),
                sliderInput(inputId = 'population_display_range_BUL',
                            label = "",
                            min = 0,
                            max = 0,
                            value = c(0, 1000)
                ),
                checkboxInput(
                  inputId = "population_location_filter_BUL",
                  label = "Filter by Location",
                  value = FALSE
                ),
                conditionalPanel(
                  condition = "input.tab == 'population_analysis' && input.population_location_filter_BUL == true",
                  uiOutput('population_city_choices_BUL')
                ),
                checkboxInput(
                  inputId = "population_er_filter_BUL",
                  label = "Filter by Ethnicity/Religion",
                  value = FALSE
                ),
                conditionalPanel(
                  condition = "input.tab == 'population_analysis' && input.population_er_filter_BUL == true",
                  pickerInput(inputId = "population_selected_er_BUL",
                              label = "Selected Ethnicity/Religion:",
                              choices = "",
                              selected = "",
                              multiple = TRUE,
                              options = list(plugins = list("remove_button"), `actions-box` = TRUE, `live-search` = TRUE, `multiple-separator` = '|')
                  )
                ),
                radioButtons(inputId = "population_gh_filter_BUL",
                             label = "",
                             choiceNames = list("Filter by Gender", "Filter by Household"),
                             choiceValues = list("population_gender_filter", "population_househould_filter"), 
                             selected = "",
                             inline = FALSE
                ),
                conditionalPanel(
                  condition = "input.tab == 'population_analysis' && input.population_gh_filter_BUL == 'population_gender_filter'",
                  selectizeInput(
                    inputId = "population_selected_gender_BUL",
                    label = "Selected Variable:",
                    choices = "",
                    selected = "",
                    multiple = FALSE,
                    options = list(plugins = list("remove_button"))
                  )
                  
                ),
                conditionalPanel(
                  condition = "input.tab == 'population_analysis' && input.population_gh_filter_BUL == 'population_household_filter'",
                  uiOutput('population_household_choices_BUL')
                )
              )
            ),
            conditionalPanel(
              condition = "input.tab == 'population_analysis' && input.population_selected_countries.includes('TUR')",
              box(
                title = "Turkey parameters",
                width = NULL,
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "danger",
                selectInput(
                  inputId = "population_census_year_TUR", 
                  label = "Turkey Census Year:", 
                  choices = "",
                  selected = "",
                  width = "100%"
                ),
                sliderInput(inputId = 'population_display_range_TUR',
                            label = "",
                            min = 0,
                            max = 0,
                            value = c(0, 1000)
                ),
                checkboxInput(
                  inputId = "population_location_filter_TUR",
                  label = "Filter by Location",
                  value = FALSE
                ),
                conditionalPanel(
                  condition = "input.tab == 'population_analysis' && input.population_location_filter_TUR == true",
                  uiOutput('population_city_choices_TUR')
                ),
                checkboxInput(
                  inputId = "population_er_filter_TUR",
                  label = "Filter by Ethnicity/Religion",
                  value = FALSE
                ),
                conditionalPanel(
                  condition = "input.tab == 'population_analysis' && input.population_er_filter_TUR == true",
                  pickerInput(inputId = "population_selected_er_TUR",
                              label = "Selected Ethnicity/Religion:",
                              choices = "",
                              selected = "",
                              multiple = TRUE,
                              options = list(plugins = list("remove_button"), `actions-box` = TRUE, `live-search` = TRUE, `multiple-separator` = '|')
                  )
                ),
                radioButtons(inputId = "population_gh_filter_TUR",
                             label = "",
                             choiceNames = list("Filter by Gender", "Filter by Household"),
                             choiceValues = list("population_gender_filter", "population_househould_filter"), 
                             selected = "", 
                             inline = FALSE
                ),
                conditionalPanel(
                  condition = "input.tab == 'population_analysis' && input.population_gh_filter_TUR == 'population_gender_filter'",
                  selectizeInput(
                    inputId = "population_selected_gender_TUR",
                    label = "Selected Variable:",
                    choices = "",
                    selected = "",
                    multiple = FALSE,
                    options = list(plugins = list("remove_button"))
                  )
                ),
                conditionalPanel(
                  condition = "input.tab == 'population_analysis' && input.population_gh_filter_TUR == 'population_househould_filter'",
                  uiOutput('population_household_choices_TUR')
                )
              )
            ),
            conditionalPanel(condition = "input.tab == 'population_analysis' && population_analysis_level == 'point'",
                             box(
                               title = "Map Settings",
                               width = NULL,
                               collapsible = TRUE,
                               solidHeader = FALSE,
                               status = "danger", 
                               collapsed = TRUE,
                               colourpicker::colourInput(
                                 inputId = "population_map_color_mode",
                                 label = "Map Color:",
                                 palette = "limited",
                                 showColour = "background",
                                 allowedCols = names(map_colors),
                                 value = "#807DBA"
                               )
                             ),
                             # box(
                             #   title = "Plot Settings",
                             #   width = NULL,
                             #   collapsible = TRUE,
                             #   solidHeader = FALSE,
                             #   status = "danger", 
                             #   collapsed = TRUE,
                             #   selectInput(
                             #     inputId = "population_plot_type",
                             #     label = "Plot Type:",
                             #     choices = c("Bar Plot" = "bar", "Line Graph" = "line"),
                             #     selected = "bar"
                             #   ),
                             #   colourpicker::colourInput(
                             #     inputId = "population_plot_color_mode",
                             #     label = "Plot Color:",
                             #     palette = "limited",
                             #     showColour = "background",
                             #     allowedCols = names(map_colors),
                             #     value = "#4292C6"
                             #   )
                             # )
            )
          ),
          column(
            width = 9,
            conditionalPanel(
              condition = "input.tab == 'population_analysis' && input.population_result_list != null && input.population_result_list.length >= 1",
              box(
                title = "Result Map",
                width = NULL,
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "success",
                # downloadButton(outputId = "population_download_map", label = "Download map"), textOutput("selected_var"),
                conditionalPanel(
                  condition = "input.tab == 'population_analysis' && !input.population_analysis_level.includes('point')",
                  
                  selectInput(inputId = "population_plot_measure", label = "", 
                              choices = c("Real count number of selected population in area in country", 
                                          "Percentage of selected population in area in country",
                                          "Percentage of selected population in area"), selected = "Percentage of the area in country",
                              width = "100%")
                ),
                uiOutput(outputId = "population_leaflet_map")
              )#,
              # conditionalPanel(condition = "input.tab == 'population_analysis' && input.population_analysis_level != 'point'",
              #                  box(
              #                    title = "Result Plot",
              #                    width = NULL,
              #                    collapsible = TRUE,
              #                    solidHeader = TRUE,
              #                    status = "success",
              #                    HTML("<br>"),
              #                    htmlOutput(outputId = "population_result_plot")
              #                  ),
              #                  box(
              #                    title = "Result Table",
              #                    width = NULL,
              #                    collapsible = TRUE,
              #                    solidHeader = TRUE,
              #                    status = "success",
              #                    HTML("<br>"),
              #                    DT::DTOutput(outputId = "population_result_table")
              #                  )
              # )
            )
          )
          )
  )
