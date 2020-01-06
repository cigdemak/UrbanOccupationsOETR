ui_occupational_analysis <- 
  tabItem(tabName = "occupational_analysis",
          fluidRow(column(
            width = 3,
            box(
              title = "Analysis Parameters",
              width = NULL,
              collapsible = TRUE,
              solidHeader = TRUE,
              status = "danger",
              radioButtons(inputId = "occupation_analysis_level",
                           label = "Analysis level:",
                           choiceNames = list("District"),
                           choiceValues = list("dist"), selected = "dist",
                           inline = TRUE
              ),
              checkboxGroupInput(
                inputId = "occupation_selected_countries",
                label = "Choose country:",
                choiceNames = "",
                choiceValues = "",
                inline = TRUE
              ),
              textInput(
                inputId = "occupation_result_name",
                label = "Result Name:",
                placeholder = "Give a name to result ..."
              ),
              actionButton(inputId = "occupation_add_to_list", label = "Add to Result List"),
              selectizeInput(
                inputId = "occupation_result_list",
                label = "Result List:",
                choices = NULL,
                selected = NULL,
                options = list(maxItems = 1)
              )
            ),
            conditionalPanel(
              condition = "input.tab == 'occupational_analysis' && input.occupation_selected_countries.includes('BUL') == true",
              box(
                title = "Bulgaria parameters",
                width = NULL,
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "danger",
                selectInput(
                  inputId = "occupation_census_year_BUL", 
                  label = "Bulgaria Census Year:", 
                  choices = "",
                  selected = "", 
                  width = "100%"
                ),
                shinyTree("occupation_tree_BUL", theme = "proton", themeIcons = FALSE,
                          themeDots = FALSE, checkbox = TRUE, search = TRUE),
                HTML("<br>"),
                uiOutput('occupation_display_range_BUL'),
                checkboxInput(
                  inputId = "occupation_location_filter_BUL",
                  label = "Filter by Location",
                  value = FALSE
                ),
                conditionalPanel(
                  condition = "input.occupation_location_filter_BUL == true",
                  uiOutput('occupation_city_choices_BUL')
                ),
                checkboxInput(
                  inputId = "occupation_gender_filter_BUL",
                  label = "Filter by Gender",
                  value = FALSE
                ),
                conditionalPanel(
                  condition = "input.tab == 'occupational_analysis' && input.occupation_gender_filter_BUL == true",
                  selectizeInput(
                    inputId = "occupation_selected_gender_BUL",
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
              condition = "input.tab == 'occupational_analysis' && input.occupation_selected_countries.includes('TUR')",
              box(
                title = "Turkey parameters",
                width = NULL,
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "danger",
                selectInput(
                  inputId = "occupation_census_year_TUR", 
                  label = "Turkey Census Year:", 
                  choices = "",
                  selected = "", 
                  width = "100%"
                ),
                shinyTree("occupation_tree_TUR", theme = "proton", themeIcons = FALSE,
                          themeDots = FALSE, checkbox = TRUE, search = TRUE),
                HTML("<br>"),
                uiOutput('occupation_display_range_TUR'),
                checkboxInput(
                  inputId = "occupation_location_filter_TUR",
                  label = "Filter by Location",
                  value = FALSE
                ),
                conditionalPanel(
                  condition = "input.tab == 'occupational_analysis' && input.occupation_location_filter_TUR == true",
                  uiOutput('occupation_city_choices_TUR')
                ),
                checkboxInput(
                  inputId = "occupation_gender_filter_TUR",
                  label = "Filter by Gender",
                  value = FALSE
                ),
                conditionalPanel(
                  condition = "input.tab == 'occupational_analysis' && input.occupation_gender_filter_TUR == true",
                  selectizeInput(
                    inputId = "occupation_selected_gender_TUR",
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
                inputId = "occupation_map_color_mode",
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
            #   selectInput(
            #     inputId = "occupation_plot_type",
            #     label = "Plot Type:",
            #     choices = c("Bar Plot" = "bar", "Line Graph" = "line"),
            #     selected = "bar"
            #   ),
            #   colourpicker::colourInput(
            #     inputId = "occupation_plot_color_mode",
            #     label = "Plot Color:",
            #     palette = "limited",
            #     showColour = "background",
            #     allowedCols = names(map_colors),
            #     value = "#4292C6"
            #   )
            # )
          ),
          column(
            width = 9,
            conditionalPanel(
              condition = "input.tab == 'occupational_analysis' && input.occupation_result_list != null && input.occupation_result_list.length >= 1",
              box(
                title = "Result Map",
                width = NULL,
                collapsible = TRUE,
                solidHeader = TRUE,
                status = "success",
                selectInput(inputId = "occupation_plot_measure", label = "", 
                            choices = c("Real count number of selected occupations in area in country", "Percentage of selected occupations in area in country", "Percentage of selected occupations in area"), selected = "Percentage of selected occupations in area in country",
                            width = "100%"),
                uiOutput(outputId = "occupation_leaflet_map")
              ),
              # box(
              #   title = "Result Plot",
              #   width = NULL,
              #   collapsible = TRUE,
              #   solidHeader = TRUE,
              #   status = "success",
              #   htmlOutput(outputId = "occupation_result_plot")
              # ),
              # box(
              #   title = "Result Table",
              #   width = NULL,
              #   collapsible = TRUE,
              #   solidHeader = TRUE,
              #   status = "success",
              #   HTML("<br>"),
              #   DT::DTOutput(outputId = "occupation_result_table")
              # )
            )
          )
          )
  )
