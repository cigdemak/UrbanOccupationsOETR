ui_agricultural_analysis <- 
  tabItem(tabName = "agricultural_analysis",
          fluidRow(column(
            width = 3,
            box(
              title = "Analysis Parameters",
              width = NULL,
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "danger",
              checkboxGroupInput(
                inputId = "agricultural_selected_countries",
                label = "Choose country:",
                choiceNames = list("Bulgaria", "Turkey"),
                choiceValues = list("BUL", "TUR"), selected = "BUL",
                inline = TRUE
              ),
              radioButtons(inputId = "agricultural_analysis_level",
                           label = "Analysis level:",
                           choiceNames = list("District", "Subdistrict"),
                           choiceValues = list("dist", "subdist"), selected = "dist",
                           inline = TRUE
              ),
              
              textInput(
                inputId = "agricultural_result_name",
                label = "Result Name:",
                placeholder = "Give a name to result ..."
              ),
              actionButton(inputId = "agricultural_add_to_list", label = "Add to Result List"),
              selectizeInput(
                inputId = "agricultural_result_list",
                label = "Result List:",
                choices = NULL,
                selected = NULL,
                options = list(maxItems = 1)
              )
            ),
            conditionalPanel(
              condition = "input.tab == 'agricultural_analysis' && input.agricultural_selected_countries.includes('BUL') == true",
              box(
                title = "Bulgaria parameters",
                width = NULL,
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "danger",
                selectInput(
                  inputId = "agricultural_census_year_BUL", 
                  label = "Bulgaria Census Year:", 
                  choices = c(1897),
                  selected = 1897, 
                  width = "100%"
                ),
                shinyTree(outputId = "agricultural_tree_BUL", theme = "proton", themeIcons = FALSE,
                          themeDots = FALSE, checkbox = TRUE, search = TRUE),
                HTML("<br>"),
                
                uiOutput('agricultural_display_range_BUL'),
                checkboxInput(
                  inputId = "agricultural_location_filter_BUL",
                  label = "Filter by Location",
                  value = FALSE
                ),
                conditionalPanel(
                  condition = "input.agricultural_location_filter_BUL == true",
                  uiOutput('agricultural_city_choices_BUL')
                ),
                checkboxInput(
                  inputId = "agricultural_gender_filter_BUL",
                  label = "Filter by Gender",
                  value = FALSE
                ),
                conditionalPanel(
                  condition = "input.tab == 'agricultural_analysis' && input.agricultural_gender_filter_BUL == true",
                  selectizeInput(
                    inputId = "agricultural_selected_gender_BUL",
                    label = "Selected Gender:",
                    choices = gender_names,
                    selected = default_gender_names,
                    multiple = FALSE,
                    options = list(plugins = list("remove_button"))
                  )
                )
              )
            ),
            conditionalPanel(
              condition = "input.tab == 'agricultural_analysis' && input.agricultural_selected_countries.includes('TUR')",
              box(
                title = "Turkey parameters",
                width = NULL,
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "danger",
                selectInput(
                  inputId = "agricultural_census_year_TUR", 
                  label = "Turkey Census Year:", 
                  choices = c(1927),
                  selected = 1927, 
                  width = "100%"
                ),
                shinyTree(outputId = "agricultural_tree_TUR", theme = "proton", themeIcons = FALSE,
                          themeDots = FALSE, checkbox = TRUE, search = TRUE),
                HTML("<br>"),
                uiOutput('agricultural_display_range_TUR'),
                checkboxInput(
                  inputId = "agricultural_location_filter_TUR",
                  label = "Filter by Location",
                  value = FALSE
                ),
                conditionalPanel(
                  condition = "input.tab == 'agricultural_analysis' && input.agricultural_location_filter_TUR == true",
                  uiOutput('agricultural_city_choices_TUR')
                ),
                checkboxInput(
                  inputId = "agricultural_gender_filter_TUR",
                  label = "Filter by Gender",
                  value = FALSE
                ),
                conditionalPanel(
                  condition = "input.tab == 'agricultural_analysis' && input.agricultural_gender_filter_TUR == true",
                  selectizeInput(
                    inputId = "agricultural_selected_gender_TUR",
                    label = "Selected Gender:",
                    choices = gender_names,
                    selected = default_gender_names,
                    multiple = FALSE,
                    options = list(plugins = list("remove_button"))
                  )
                )
              )
            ),
            box(
              title = "Map Settings",
              width = NULL,
              collapsible = TRUE,
              solidHeader = FALSE,
              status = "danger",
              colourpicker::colourInput(
                inputId = "agricultural_map_color_mode",
                label = "Map Color:",
                palette = "limited",
                showColour = "background",
                allowedCols = names(map_colors),
                value = "#807DBA"
              )
            ),
            box(
              title = "Plot Settings",
              width = NULL,
              collapsible = TRUE,
              solidHeader = FALSE,
              status = "danger",
              selectInput(
                inputId = "agricultural_plot_type",
                label = "Plot Type:",
                choices = c("Bar Plot" = "bar", "Line Graph" = "line"),
                selected = "bar"
              ),
              colourpicker::colourInput(
                inputId = "agricultural_plot_color_mode",
                label = "Plot Color:",
                palette = "limited",
                showColour = "background",
                allowedCols = names(map_colors),
                value = "#4292C6"
              )
            )
          ),
          column(
            width = 9,
            conditionalPanel(
              condition = "input.tab == 'agricultural_analysis' && input.agricultural_result_list != null && input.agricultural_result_list.length >= 1",
              box(
                title = "Result Map",
                width = NULL,
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "success",
                selectInput(inputId = "agricultural_plot_measure", label = "", 
                            choices = c("Real count number of selected occupations in area in country", "Percentage of selected occupations in area in country", "Percentage of selected occupations in area"), selected = "Percentage of selected occupations in area in country",
                            width = "100%"),
                uiOutput(outputId = "agricultural_leaflet_map")
              ),
              box(
                title = "Result Plot",
                width = NULL,
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "success",
                htmlOutput(outputId = "agricultural_result_plot")
              ),
              box(
                title = "Result Table",
                width = NULL,
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "success",
                HTML("<br>"),
                dataTableOutput(outputId = "agricultural_result_table")
              )
            )
          )
          )
  )