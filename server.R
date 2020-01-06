shinyServer(function(input, output, session) {
  
  # occupation
  session$userData$occupation_result_table <- list()
  session$userData$occupation_selected_countries <- list()
  session$userData$occupation_analysis_level <- list()
  
  session$userData$occupation_census_year_BUL <- list()
  session$userData$occupation_selected_occupation_groups_BUL <- list()
  session$userData$occupation_selected_gender_BUL <- list()
  
  session$userData$occupation_census_year_TUR <- list()
  session$userData$occupation_selected_occupation_groups_TUR <- list()
  session$userData$occupation_selected_gender_TUR <- list()
  
  # population
  session$userData$population_result_table <- list()
  session$userData$population_selected_countries <- list()
  session$userData$population_analysis_level <- list()
  
  session$userData$population_census_year_OE <- list()
  session$userData$population_selected_er_OE <- list()
  
  session$userData$population_census_year_BUL <- list()
  session$userData$population_selected_er_BUL <- list()
  
  session$userData$population_census_year_TUR <- list()
  session$userData$population_selected_er_TUR <- list()
  
  # agriculture
  session$userData$agricultural_result_table <- list()
  session$userData$agricultural_selected_countries <- list()
  session$userData$agricultural_analysis_level <- list()
  
  session$userData$agricultural_census_year_BUL <- list()
  session$userData$agricultural_selected_agriculture_groups_BUL <- list()
  session$userData$agricultural_selected_gender_BUL <- list()
  
  session$userData$agricultural_census_year_TUR <- list()
  session$userData$agricultural_selected_agriculture_groups_TUR <- list()
  session$userData$agricultural_selected_gender_TUR <- list()
  
  # occupation
  output$occupation_tree_BUL <- renderTree({
    pst_list_bul <- all_occupational_data[[c(input$occupation_analysis_level, "BUL", input$occupation_census_year_BUL)]]
    
    list(
      "Primary Sector" = structure(primary_sector[which(primary_sector %in% unique(pst_list_bul$`PST CONC`))], stselected = TRUE),
      "Secondary Sector" = secondary_sector[which(secondary_sector %in% unique(pst_list_bul$`PST CONC`))],
      "Tertiary Sector" = tertiary_sector[which(tertiary_sector %in% unique(pst_list_bul$`PST CONC`))],
      "Sectorally Unspecific Occupations" = unspecified[which(unspecified %in% unique(pst_list_bul$`PST CONC`))],
      "Without Occupation or Unstated" = unstated[which(unstated %in% unique(pst_list_bul$`PST CONC`))]
    )
  })
  output$occupation_tree_TUR <- renderTree({
    pst_list_tur <- all_occupational_data[[c(input$occupation_analysis_level, "TUR", input$occupation_census_year_TUR)]]
    
    list(
      "Primary Sector" = structure(primary_sector[which(primary_sector %in% unique(pst_list_tur$`PST CONC`))], stselected = TRUE),
      "Secondary Sector" = secondary_sector[which(primary_sector %in% unique(pst_list_tur$`PST CONC`))],
      "Tertiary Sector" = tertiary_sector[which(primary_sector %in% unique(pst_list_tur$`PST CONC`))],
      "Sectorally Unspecific Occupations" = unspecified[which(unspecified %in% unique(pst_list_tur$`PST CONC`))],
      "Without Occupation or Unstated" = unstated[which(unstated %in% unique(pst_list_tur$`PST CONC`))]
    )
  })
  
  # agriculture
  output$agricultural_tree_BUL <- renderTree({
    agri_list <- as.matrix(all_agricultural_data[[c(input$agricultural_analysis_level, "BUL", input$agricultural_census_year_BUL)]])
    grains <- unique(agri_list[which(agri_list[,"AGRI_Group_Code"] == "Grains"), "AGRI_Code"])
    legumes <- unique(agri_list[which(agri_list[,"AGRI_Group_Code"] == "Legumes"), "AGRI_Code"])
    crops <- unique(agri_list[which(agri_list[,"AGRI_Group_Code"] == "Industrial Crops"), "AGRI_Code"])
    list(
      "Grains" = structure(setNames(as.list(grains), grains), stselected = TRUE),
      "Legumes" = setNames(as.list(legumes), legumes),
      "Industrial Crops" = setNames(as.list(crops), crops)
    )
  })
  output$agricultural_tree_TUR <- renderTree({
    agri_list <- as.matrix(all_agricultural_data[[c(input$agricultural_analysis_level, "TUR", input$agricultural_census_year_TUR)]])
    grains <- unique(agri_list[which(agri_list[,"AGRI_Group_Code"] == "Grains"), "AGRI_Code"])
    legumes <- unique(agri_list[which(agri_list[,"AGRI_Group_Code"] == "Legumes"), "AGRI_Code"])
    crops <- unique(agri_list[which(agri_list[,"AGRI_Group_Code"] == "Industrial Crops"), "AGRI_Code"])
    list(
      "Grains" = structure(setNames(as.list(grains), grains), stselected = TRUE),
      "Legumes" = setNames(as.list(legumes), legumes),
      "Industrial Crops" = setNames(as.list(crops), crops)
    )
  })
  
  # occupation
  occupation_update_results <- function(occupation_result_name,
                                        occupation_result_table,
                                        occupation_selected_countries,
                                        occupation_analysis_level,
                                        occupation_census_year_BUL,
                                        occupation_selected_occupation_groups_BUL,
                                        occupation_selected_cities_BUL,
                                        occupation_selected_gender_BUL,
                                        occupation_census_year_TUR,
                                        occupation_selected_occupation_groups_TUR,
                                        occupation_selected_cities_TUR,
                                        occupation_selected_gender_TUR){
    result_name <- trimws(occupation_result_name)
    if(occupation_result_name != "") {
      session$userData$occupation_result_table[[occupation_result_name]] <- occupation_result_table
      session$userData$occupation_selected_countries[[occupation_result_name]] <- occupation_selected_countries
      session$userData$occupation_analysis_level[[occupation_result_name]] <- occupation_analysis_level 
      
      session$userData$occupation_census_year_BUL[[occupation_result_name]] <- occupation_census_year_BUL
      session$userData$occupation_selected_occupation_groups_BUL[[occupation_result_name]] <- unlist(get_selected(input$occupation_tree_BUL))
      session$userData$occupation_selected_cities_BUL[[occupation_result_name]] <- input$occupation_selected_cities_BUL
      session$userData$occupation_location_filter_BUL[[occupation_result_name]] <- input$occupation_location_filter_BUL
      session$userData$occupation_selected_gender_BUL[[occupation_result_name]] <- input$occupation_selected_gender_BUL
      
      session$userData$occupation_census_year_TUR[[occupation_result_name]] <- occupation_census_year_TUR
      session$userData$occupation_selected_occupation_groups_TUR[[occupation_result_name]] <- unlist(get_selected(input$occupation_tree_TUR))
      session$userData$occupation_selected_cities_TUR[[occupation_result_name]] <- input$occupation_selected_cities_TUR
      session$userData$occupation_location_filter_TUR[[occupation_result_name]] <- input$occupation_location_filter_TUR
      session$userData$occupation_selected_gender_TUR[[occupation_result_name]] <- input$occupation_selected_gender_TUR
      
      updateSelectizeInput(session = session, inputId = "occupation_result_list", selected = occupation_result_name,
                           choices = sort(names(session$userData$occupation_result_table)))
      updateTextInput(session = session, inputId = "occupation_result_name", value = "")
    }
  }
  
  # population
  population_update_results <- function(population_result_name,
                                        population_result_table,
                                        population_selected_countries,
                                        population_analysis_level,
                                        population_census_year_OE,
                                        population_selected_cities_OE,
                                        population_selected_er_OE,
                                        population_location_filter_OE,
                                        population_er_filter_OE,
                                        #population_display_range_OE,
                                        population_census_year_BUL,
                                        population_selected_cities_BUL,
                                        population_selected_er_BUL,
                                        population_location_filter_BUL,
                                        population_er_filter_BUL,
                                        # population_display_range_BUL,
                                        population_census_year_TUR,
                                        population_selected_cities_TUR,
                                        population_selected_er_TUR,
                                        population_location_filter_TUR,
                                        population_er_filter_TUR
                                        # population_display_range_TUR
  ){
    population_result_name <- trimws(population_result_name)
    if(population_result_name != "") {
      session$userData$population_result_table[[population_result_name]] <- population_result_table
      session$userData$population_selected_countries[[population_result_name]] <- population_selected_countries
      session$userData$population_analysis_level[[population_result_name]] <- population_analysis_level 
      
      session$userData$population_census_year_OE[[population_result_name]] <- population_census_year_OE
      session$userData$population_selected_cities_OE[[population_result_name]] <- population_selected_cities_OE
      session$userData$population_selected_er_OE[[population_result_name]] <- population_selected_er_OE
      session$userData$population_location_filter_OE[[population_result_name]] <- population_location_filter_OE
      session$userData$population_er_filter_OE[[population_result_name]] <- population_er_filter_OE
      #session$userData$population_display_range_OE[[population_result_name]] <- population_display_range_OE
      
      session$userData$population_census_year_BUL[[population_result_name]] <- population_census_year_BUL
      session$userData$population_selected_cities_BUL[[population_result_name]] <- population_selected_cities_BUL
      session$userData$population_selected_er_BUL[[population_result_name]] <- population_selected_er_BUL
      session$userData$population_location_filter_BUL[[population_result_name]] <- population_location_filter_BUL
      session$userData$population_er_filter_BUL[[population_result_name]] <- population_er_filter_BUL
      # session$userData$population_display_range_BUL[[population_result_name]] <- population_display_range_BUL
      
      session$userData$population_census_year_TUR[[population_result_name]] <- population_census_year_TUR
      session$userData$population_selected_cities_TUR[[population_result_name]] <- population_selected_cities_TUR
      session$userData$population_selected_er_TUR[[population_result_name]] <- population_selected_er_TUR
      session$userData$population_location_filter_TUR[[population_result_name]] <- population_location_filter_TUR
      session$userData$population_er_filter_TUR[[population_result_name]] <- population_er_filter_TUR
      # session$userData$population_display_range_TUR[[population_result_name]] <- population_display_range_TUR
      
      
      updateSelectizeInput(session = session, inputId = "population_result_list", selected = population_result_name,
                           choices = sort(names(session$userData$population_result_table)))
      updateTextInput(session = session, inputId = "population_result_name", value = "")
    }
  }
  
  # agriculture
  agricultural_update_results <- function(agricultural_result_name,
                                          agricultural_result_table,
                                          agricultural_selected_countries,
                                          agricultural_analysis_level,
                                          agricultural_census_year_BUL,
                                          agricultural_selected_agriculture_groups_BUL,
                                          agricultural_selected_cities_BUL,
                                          agricultural_selected_gender_BUL,
                                          agricultural_census_year_TUR,
                                          agricultural_selected_agriculture_groups_TUR,
                                          agricultural_selected_cities_TUR,
                                          agricultural_selected_gender_TUR){
    
    agricultural_result_name <- trimws(agricultural_result_name)
    if(agricultural_result_name != "") {
      session$userData$agricultural_result_table[[agricultural_result_name]] <- agricultural_result_table
      session$userData$agricultural_selected_countries[[agricultural_result_name]] <- agricultural_selected_countries
      session$userData$agricultural_analysis_level[[agricultural_result_name]] <- agricultural_analysis_level 
      
      session$userData$agricultural_census_year_BUL[[agricultural_result_name]] <- agricultural_census_year_BUL
      session$userData$agricultural_selected_agrigulture_groups_BUL[[agricultural_result_name]] <- unlist(get_selected(input$agricultural_tree_BUL))
      session$userData$agricultural_selected_cities_BUL[[agricultural_result_name]] <- input$agricultural_selected_cities_BUL
      session$userData$agricultural_location_filter_BUL[[agricultural_result_name]] <- input$agricultural_location_filter_BUL
      session$userData$agricultural_selected_gender_BUL[[agricultural_result_name]] <- input$agricultural_selected_gender_BUL
      
      session$userData$agricultural_census_year_TUR[[agricultural_result_name]] <- agricultural_census_year_TUR
      session$userData$agricultural_selected_agriculture_groups_TUR[[agricultural_result_name]] <- unlist(get_selected(input$agricultural_tree_TUR))
      session$userData$agricultural_selected_cities_TUR[[agricultural_result_name]] <- input$agricultural_selected_cities_TUR
      session$userData$agricultural_location_filter_TUR[[agricultural_result_name]] <- input$agricultural_location_filter_TUR
      session$userData$agricultural_selected_gender_TUR[[agricultural_result_name]] <- input$agricultural_selected_gender_TUR
      
      updateSelectizeInput(session = session, inputId = "agricultural_result_list", selected = agricultural_result_name,
                           choices = sort(names(session$userData$agricultural_result_table)))
      updateTextInput(session = session, inputId = "agricultural_result_name", value = "")
    }
  }
  
  # occupation
  occupation_clear_results <- function() {
    session$userData$occupation_result_table <- list()
    session$userData$occupation_selected_countries <- list()
    
    session$userData$occupation_census_year_BUL <- list()
    session$userData$occupation_selected_occupation_groups_BUL <- list()
    session$userData$occupation_selected_gender_BUL <- list()
    
    session$userData$occupation_census_year_TUR <- list()
    session$userData$occupation_selected_occupation_groups_TUR <- list()
    session$userData$occupation_selected_gender_TUR <- list()
    
    updateSelectizeInput(session = session, inputId = "occupation_result_list", selected = NULL, choices = character(0))
  }
  
  # population
  population_clear_results <- function() {
    session$userData$population_result_table <- list()
    session$userData$population_selected_countries <- list()
    
    session$userData$population_census_year_OE <- list()
    session$userData$population_selected_er_OE <- list()
    session$userData$population_selected_gender_OE <- list()
    session$userData$population_selected_household_OE <- list()
    
    
    session$userData$population_census_year_BUL <- list()
    session$userData$population_selected_er_BUL <- list()
    session$userData$population_selected_gender_BUL <- list()
    session$userData$population_selected_household_BUL <- list()
    
    session$userData$population_census_year_TUR <- list()
    session$userData$population_selected_er_TUR <- list()
    session$userData$population_selected_gender_TUR <- list()
    session$userData$population_selected_household_TUR <- list()
    
    updateSelectizeInput(session = session, inputId = "population_result_list", selected = NULL, choices = character(0))
  }
  
  # agriculture
  agricultural_clear_results <- function() {
    session$userData$agricultural_result_table <- list()
    session$userData$agricultural_selected_countries <- list()
    
    session$userData$agricultural_census_year_BUL <- list()
    session$userData$agricultural_selected_agriculturre_group_BUL <- list()
    session$userData$agricultural_selected_gender_BUL <- list()
    
    session$userData$agricultural_census_year_TUR <- list()
    session$userData$agricultural_selected_agriculture_group_TUR <- list()
    session$userData$agricultural_selected_gender_TUR <- list()
    
    updateSelectizeInput(session = session, inputId = "agricultural_result_list", selected = NULL, choices = character(0))
  }
  
  # occupation
  observe({
    x <- input$occupation_analysis_level
    updateCheckboxGroupInput(session, "occupation_selected_countries",
                             label = paste("Choose country:"),
                             choiceNames = as.character(country_codes[(names(all_occupational_data[[which(names(all_occupational_data) == x)]]))]),
                             choiceValues = as.list(names(all_occupational_data[[which(names(all_occupational_data) == x)]])), selected = as.list(names(all_occupational_data[[which(names(all_occupational_data) == x)]]))[1]
    )
  })
  
  observe({
    occupation_selected_countries <- input$occupation_selected_countries
    occupation_analysis_level <- input$occupation_analysis_level
    
    for(country in occupation_selected_countries){
      years <- names(all_occupational_data[[c(occupation_analysis_level, country)]])
      inputid <- paste0("occupation_census_year_",country)
      updateSelectInput(session,
                        inputId = inputid, 
                        label = "Census Year:", 
                        choices = years,
                        selected = years[1]
      )}
  })
  
  output$occupation_city_choices_BUL <- renderUI({
    if(sum(input$occupation_selected_countries == "BUL") == 1){
      pickerInput(
        inputId = "occupation_selected_cities_BUL",
        label = "Selected Cities:",
        choices = all_index[[input$occupation_analysis_level]][[c("BUL")]][[input$occupation_census_year_BUL]]$NAME,
        selected = all_index[[input$occupation_analysis_level]][[c("BUL")]][[input$occupation_census_year_BUL]]$NAME,
        multiple = TRUE,
        options = list(plugins = list("remove_button"), `actions-box` = TRUE, `live-search` = TRUE, `multiple-separator` = '|')
      )
    }
  })
  
  output$occupation_city_choices_TUR <- renderUI({
    if(sum(input$occupation_selected_countries == "TUR") == 1){
      pickerInput(
        inputId = "occupation_selected_cities_TUR",
        label = "Selected Cities:",
        choices = all_index[[input$occupation_analysis_level]][[c("TUR")]][[input$occupation_census_year_TUR]]$NAME,
        selected = all_index[[input$occupation_analysis_level]][[c("TUR")]][[input$occupation_census_year_TUR]]$NAME,
        multiple = TRUE,
        options = list(plugins = list("remove_button"), `actions-box` = TRUE, `live-search` = TRUE, `multiple-separator` = '|')
      )
    }
  })
  
  # population
  observe({
    x <- input$population_analysis_level
    updateCheckboxGroupInput(session, "population_selected_countries",
                             label = paste("Choose country:"),
                             choiceNames = as.character(country_codes[(names(all_population_data[[which(names(all_population_data) == x)]]))]),
                             choiceValues = as.list(names(all_population_data[[which(names(all_population_data) == x)]])), selected = as.list(names(all_population_data[[which(names(all_population_data) == x)]]))[1]
    )
  })
  
  observe({
    population_selected_countries <- input$population_selected_countries
    population_analysis_level <- input$population_analysis_level
    
    for(country in population_selected_countries){
      years <- names(all_population_data[[c(population_analysis_level, country)]])
      inputid <- paste0("population_census_year_",country)
      updateSelectInput(session,
                        inputId = inputid, 
                        label = "Census Year:", 
                        choices = years,
                        selected = years[1]
      )}
  })
  
  output$population_city_choices_OE <- renderUI({
    if(sum(input$population_selected_countries == "OE") == 1){
      pickerInput(
        inputId = "population_selected_cities_OE",
        label = "Selected Cities:",
        choices = all_index[[input$population_analysis_level]][["OE"]][[input$population_census_year_OE]][["NAME"]],
        selected = all_index[[c(input$population_analysis_level, "OE", input$population_census_year_OE)]]$NAME,
        multiple = TRUE,
        options = list(plugins = list("remove_button"), `actions-box` = TRUE, `live-search` = TRUE, `multiple-separator` = '|')
      )
    }
  })
  
  output$population_city_choices_BUL <- renderUI({
    if(sum(input$population_selected_countries == "BUL") == 1){
      pickerInput(
        inputId = "population_selected_cities_BUL",
        label = "Selected Cities:",
        choices = all_index[[c(input$population_analysis_level, "BUL", input$population_census_year_BUL)]]$NAME,
        selected = all_index[[c(input$population_analysis_level, "BUL", input$population_census_year_BUL)]]$NAME,
        multiple = TRUE,
        options = list(plugins = list("remove_button"), `actions-box` = TRUE, `live-search` = TRUE, `multiple-separator` = '|')
      )
    }
  })
  output$population_city_choices_TUR <- renderUI({
    if(sum(input$population_selected_countries == "TUR") == 1){
      pickerInput(
        inputId = "population_selected_cities_TUR",
        label = "Selected Cities:",
        choices = all_index[[c(input$population_analysis_level, "TUR", input$population_census_year_TUR)]]$NAME,
        selected = all_index[[c(input$population_analysis_level,"TUR", input$population_census_year_TUR)]]$NAME,
        multiple = TRUE,
        options = list(plugins = list("remove_button"), `actions-box` = TRUE, `live-search` = TRUE, `multiple-separator` = '|')
      )
    }
  })
  
  observe({
    population_selected_countries <- input$population_selected_countries
    population_analysis_level <- input$population_analysis_level
    population_census_year_OE <- input$population_census_year_OE
    population_census_year_BUL <- input$population_census_year_BUL
    population_census_year_TUR <- input$population_census_year_TUR
    
    for(country in population_selected_countries){
      req(all_population_data[[c(as.character(input$population_analysis_level),country)]])
      population_census_year <- get(paste0("population_census_year_",country))
      er_choices <- levels(all_population_data[[c(as.character(input$population_analysis_level),country,population_census_year)]]$ER_Code)
      if(sum(er_choices == "Population") == 1){
        er_choices <- c(" " = "Population")
      }
      updatePickerInput(session,
                        inputId = paste0("population_selected_er_",country),
                        label = "Selected Ethnicity/Religion:",
                        choices = er_choices,
                        selected = er_choices
      )
      all <- colnames(all_population_data[[c(population_analysis_level,country, population_census_year)]])
      updateSelectizeInput(session,
                           inputId = paste0("population_selected_gender_",country),
                           label = "Selected Variable:",
                           choices = intersect(all, c("Female", "Male", "Both")),
                           selected = "Both"
      )
    }
  })
  
  output$population_household_choices_OE <- renderUI({
    if(sum(input$population_selected_countries == "OE") == 1){
      all <- colnames(all_population_data[[c(input$population_analysis_level,"OE", input$population_census_year_OE)]])
      selectizeInput(
        inputId = "population_selected_household_OE",
        label = "Selected Variable:",
        choices = intersect(all, c("Household Count", "Household Size")),
        selected = default_household,
        multiple = FALSE,
        options = list(plugins = list("remove_button"))
      )
    }
  })
  
  output$population_household_choices_BUL <- renderUI({
    if(sum(input$population_selected_countries == "BUL") == 1){
      all <- colnames(all_population_data[[c(input$population_analysis_level,"BUL", input$population_census_year_BUL)]])
      selectizeInput(
        inputId = "population_selected_household_BUL",
        label = "Selected Variable:",
        choices = intersect(all, c("Household Count", "Household Size")),
        selected = c(),
        multiple = FALSE,
        options = list(plugins = list("remove_button"))
      )
    }
  })
  
  output$population_household_choices_TUR <- renderUI({
    if(sum(input$population_selected_countries == "TUR") == 1){
      all <- colnames(all_population_data[[c(input$population_analysis_level,"TUR", input$population_census_year_TUR)]])
      selectizeInput(
        inputId = "population_selected_household_TUR",
        label = "Selected Variable:",
        choices = intersect(all, c("Household Count", "Household Size")),
        selected = c(),
        multiple = FALSE,
        options = list(plugins = list("remove_button"))
      )
    }
  })
  
  # agricultural
  output$agricultural_city_choices_BUL <- renderUI({
    if(sum(input$agricultural_selected_countries == "BUL") == 1){
      pickerInput(
        inputId = "agricultural_selected_cities_BUL",
        label = "Selected Cities:",
        choices = all_index[[c(input$agricultural_analysis_level, "BUL", input$agricultural_census_year_BUL)]]$NAME,
        selected = all_index[[c(input$agricultural_analysis_level, "BUL", input$agricultural_census_year_BUL)]]$NAME,
        multiple = TRUE,
        options = list(plugins = list("remove_button"), `actions-box` = TRUE, `live-search` = TRUE, `multiple-separator` = '|')
      )
    }
  })
  
  output$agricultural_city_choices_TUR <- renderUI({
    if(sum(input$agricultural_selected_countries == "TUR") == 1){
      pickerInput(
        inputId = "agricultural_selected_cities_TUR",
        label = "Selected Cities:",
        choices = all_index[[c(input$agricultural_analysis_level, "TUR", input$agricultural_census_year_TUR)]]$NAME,
        selected = all_index[[c(input$agricultural_analysis_level,"TUR", input$agricultural_census_year_TUR)]]$NAME,
        multiple = TRUE,
        options = list(plugins = list("remove_button"), `actions-box` = TRUE, `live-search` = TRUE, `multiple-separator` = '|')
      )
    }
  })
  
  # occupation
  occupation_process_update_list <- function() {
    occupation_colorvar <- input$occupation_plot_measure
    occupation_selected_countries = as.character(input$occupation_selected_countries)
    occupation_analysis_level = input$occupation_analysis_level
    
    occupation_census_year_BUL = as.character(input$occupation_census_year_BUL)
    occupation_selected_cities_BUL = input$occupation_selected_cities_BUL
    occupation_location_filter_BUL = input$occupation_location_filter_BUL
    occupation_gender_filter_BUL = input$occupation_gender_filter_BUL
    occupation_selected_gender_BUL = input$occupation_selected_gender_BUL
    occupation_selected_occupation_groups_BUL <- unlist(get_selected(input$occupation_tree_BUL))
    
    occupation_census_year_TUR = as.character(input$occupation_census_year_TUR)
    occupation_selected_cities_TUR = input$occupation_selected_cities_TUR
    occupation_location_filter_TUR = input$occupation_location_filter_TUR
    occupation_gender_filter_TUR = input$occupation_gender_filter_TUR
    occupation_selected_gender_TUR =input$occupation_selected_gender_TUR
    occupation_selected_occupation_groups_TUR <- unlist(get_selected(input$occupation_tree_TUR))
    
    occupation_result_table <- list()
    for(country in occupation_selected_countries){
      country_year_index <- c(occupation_analysis_level, country, get(paste0("occupation_census_year_", country)))
      selected_location <- get(paste0("occupation_selected_cities_", country))
      location_filter <- get(paste0("occupation_location_filter_", country))
      gender_filter <- get(paste0("occupation_gender_filter_", country))
      selected_gender <- get(paste0("occupation_selected_gender_", country))
      selected_occupation_groups <- get(paste0("occupation_selected_occupation_groups_", country))
      
      city_index <- all_index[[country_year_index]]
      city_maps <- all_maps[[country_year_index]]
      
      econ_data <- as.data.frame(all_occupational_data[[country_year_index]])
      city_names <- city_index$NAME
      default_city_names <- city_names
      
      city_name_list = as.character(default_city_names)#[sort(as.numeric(as.character(unique(city_maps@data$MAP_ID))))])
      
      if (location_filter == TRUE ) {
        selected_cities_names <- selected_location
        selected_cities_names <- city_name_list[city_name_list %in% selected_cities_names]
      } else {
        selected_cities_names <- city_name_list
      }
      if (gender_filter == TRUE) {
        selected_gender <- selected_gender 
      } else {
        selected_gender <- default_gender_names
      }
      selected_cities = as.numeric(unlist(city_index[as.character(unlist(city_index$NAME)) %in% selected_cities_names, "LOCATION_ID"]))

      # filter the data
      result <- subset(econ_data, LOCATION_INDEX %in% selected_cities & `PST CONC` %in% occupation_filter[selected_occupation_groups],
                       select = c(selected_gender, "LOCATION_INDEX"), drop = FALSE)
      general_result <- subset(econ_data, `PST CONC` %in% working_pop_filter, select = c(selected_gender, "LOCATION_INDEX"), drop = FALSE)
      
      if(nrow(result) == 0){
        result_aggr <- matrix(NA, nrow = length(selected_cities), ncol = 3)
        data <- data.frame(matrix(NA, nrow = length(city_name_list), ncol = (ncol(result_aggr)-1)), row.names = city_name_list)
        data[sort(selected_cities),] <- result_aggr[,1:2]
        colnames(data) <- c("LOCATION_INDEX", selected_gender)
        result_f <- cbind((1:length(city_name_list)), (data[,selected_gender]), data[,selected_gender], data[,selected_gender])
        colnames(result_f) <- c("LOCATION_INDEX", "Raw_data", "PercentageOfAreaInCountry", "PercentageOfArea")
      }else{
        result_aggr <- aggregate(x = result, by = list(result$LOCATION_INDEX), FUN = "sum", na.rm = TRUE, drop =  FALSE)
        general_result_aggr <- aggregate(x = general_result, by = list(general_result$LOCATION_INDEX), FUN = "sum", na.rm = TRUE, drop =  FALSE)
        data <- data.frame(matrix(NA, nrow = length(city_name_list), ncol = 2))
        
        data[which(result_aggr[,1] %in% sort(selected_cities)),] <- result_aggr[,1:2]
        colnames(data) <- c("LOCATION_INDEX", selected_gender)
        general_data <- data.frame(matrix(NA, nrow = length(sort(selected_cities)), ncol = 2))
        general_data[which(result_aggr[,1] %in% sort(selected_cities)),] <- general_result_aggr[,1:2]
        colnames(general_data) <- c("LOCATION_INDEX", selected_gender)
        result_f <- cbind((1:nrow(data)),
                          data[,selected_gender],
                          round((100 * (data[,selected_gender]/sum(data[,selected_gender], na.rm = TRUE))), digits = 2),
                          round((100 * (data[,selected_gender]/as.numeric(general_data[,selected_gender]))), digits = 2))
        colnames(result_f) <- c("LOCATION_INDEX", "Raw_data", "PercentageOfAreaInCountry", "PercentageOfArea")
      }
   
      result <- data.frame(row.names = city_name_list)#[city_index$LOCATION_ID == (selected_cities)])
      result[, "City"] <-  city_name_list#[(selected_cities)]
      result[, "Real count number of selected occupations in area in country"] <- result_f[,"Raw_data"]
      result[, "Percentage of selected occupations in area in country"] <- result_f[, "PercentageOfAreaInCountry"]
      result[, "Percentage of selected occupations in area"] <- result_f[, "PercentageOfArea"]
      occupation_result_table[[country]] <- result
    }
    
    # occupation
    occupation_update_results(input$occupation_result_name, 
                              occupation_result_table,
                              occupation_selected_countries,
                              occupation_analysis_level,
                              occupation_census_year_BUL, 
                              occupation_selected_occupation_groups_BUL,
                              occupation_selected_cities_BUL, 
                              occupation_selected_gender_BUL,
                              occupation_census_year_TUR,
                              occupation_selected_occupation_groups_TUR, 
                              occupation_selected_cities_TUR, 
                              occupation_selected_gender_TUR)
    occupation_process_analysis()
  }
  
  # population
  population_process_update_list <- function() {
    population_colorvar <- input$population_plot_measure
    population_selected_countries = as.character(input$population_selected_countries)
    population_analysis_level = input$population_analysis_level
    
    population_census_year_OE = as.character(input$population_census_year_OE)
    population_selected_cities_OE = input$population_selected_cities_OE
    population_location_filter_OE = input$population_location_filter_OE
    population_selected_er_OE = input$population_selected_er_OE
    population_gh_filter_OE = input$population_gh_filter_OE
    # population_household_filter_OE = input$population_household_filter_OE
    population_er_filter_OE = input$population_er_filter_OE
    population_selected_er_OE = input$population_selected_er_OE
    population_selected_gender_OE = input$population_selected_gender_OE
    population_selected_household_OE = input$population_selected_household_OE
    # population_display_range_OE = input$population_display_range_OE
    
    population_census_year_BUL = as.character(input$population_census_year_BUL)
    population_selected_cities_BUL = input$population_selected_cities_BUL
    population_location_filter_BUL = input$population_location_filter_BUL
    population_selected_er_BUL = input$population_selected_er_BUL
    population_gh_filter_BUL = input$population_gh_filter_BUL
    # population_household_filter_BUL = input$population_household_filter_BUL
    population_er_filter_BUL = input$population_er_filter_BUL
    population_selected_er_BUL = input$population_selected_er_BUL
    population_selected_gender_BUL = input$population_selected_gender_BUL
    population_selected_household_BUL = input$population_selected_household_BUL
    # population_display_range_BUL = input$population_display_range_BUL
    
    population_census_year_TUR = as.character(input$population_census_year_TUR)
    population_selected_cities_TUR = input$population_selected_cities_TUR
    population_location_filter_TUR = input$population_location_filter_TUR
    population_selected_er_TUR = input$population_selected_er_TUR
    population_gh_filter_TUR = input$population_gh_filter_TUR
    # population_household_filter_TUR = input$population_household_filter_TUR
    population_er_filter_TUR = input$population_er_filter_TUR
    population_selected_er_TUR = input$population_selected_er_TUR
    population_selected_gender_TUR = input$population_selected_gender_TUR
    population_selected_household_TUR = input$population_selected_household_TUR
    # population_display_range_TUR = input$population_display_range_TUR
    
    population_result_table <- list()
    for(country in population_selected_countries){
      country_year_index <- c(population_analysis_level, country, get(paste0("population_census_year_", country)))
      census_year <- get(paste0("population_census_year_", country))
      selected_location <- get(paste0("population_selected_cities_", country))
      location_filter <- get(paste0("population_location_filter_", country))
      gh_filter <- get(paste0("population_gh_filter_", country))
      #household_filter <- get(paste0("population_household_filter_", country))
      selected_er_codes <- get(paste0("population_selected_er_", country))
      selected_gender <- get(paste0("population_selected_gender_", country))
      selected_household <- get(paste0("population_selected_household_", country))
      er_filter <- get(paste0("population_er_filter_", country))
      
      city_index <- all_index[[country_year_index]]
      city_maps <- all_maps[[country_year_index]]
      
      population_data <- as.data.frame(all_population_data[[country_year_index]])
      city_names <- city_index$NAME
      default_city_names <- city_names
      
      city_name_list = as.character(default_city_names)
      
      if (location_filter == TRUE ) {
        selected_cities_names <- selected_location
        selected_cities_names <- city_name_list[city_name_list %in% selected_cities_names]
      } else {
        selected_cities_names <- city_name_list
      }
      
      if (is.null(gh_filter)) {
        selected_gender <- selected_gender
        selected_household <- default_household
      }else if(gh_filter == "population_gender_filter") {
        selected_gender <- selected_gender
        selected_household <- default_household
      } else {
        selected_gender <- selected_household
      }
      
      if (er_filter == TRUE) {
        selected_er_codes <- selected_er_codes
      } else {
        selected_er_codes <- levels(all_population_data[[population_analysis_level]][[country]][[census_year]]$ER_Code)
      }
      selected_cities = as.numeric(unlist(city_index[as.character(unlist(city_index$NAME)) %in% selected_cities_names, "LOCATION_ID"]))
      
      if(input$population_analysis_level == "point"){
        # filter the data
        filtered_data <- subset(population_data, LOCATION_INDEX %in% selected_cities & ER_Code %in% selected_er_codes,
                                select = c("LOCATION_INDEX", "ER_Code", selected_gender,"N_coord", "E_coord"), drop = FALSE)
        er_list <- selected_er_codes
        
        result <- data.frame(matrix(NA, nrow = length(city_name_list), ncol = (length(er_list) + 3)), row.names = city_name_list)
        colnames(result) <- c("City", "N_coord", "E_coord", (er_list))
        
        for(city in selected_cities){
          for(er_code in er_list){
            result[city, er_code] <- filtered_data[which(filtered_data$LOCATION_INDEX == city & filtered_data$ER_Code == er_code), selected_gender]
          }
          result[city, "City"] <-  city_names[filtered_data[which(filtered_data$LOCATION_INDEX == city)[1], "LOCATION_INDEX"]]
          result[city, "N_coord"] <-  filtered_data[which(filtered_data$LOCATION_INDEX == city)[1], "N_coord"]
          result[city, "E_coord"] <- filtered_data[which(filtered_data$LOCATION_INDEX == city)[1], "E_coord"]
        }
        result[, "Real count number of selected population in area in country"] <- apply(as.matrix(result[, er_list, drop = FALSE]), 1, sum)
        result[, "Percentage of selected population in area in country"] <- apply(as.matrix(result[, er_list, drop = FALSE]), 1, sum)
        result[, "Percentage of selected population in area"] <- apply(as.matrix(result[, er_list, drop = FALSE]), 1, sum)
        population_result_table[[country]] <- result
      }else{
        # filter the data
        filtered_data <- subset(population_data, LOCATION_INDEX %in% selected_cities & ER_Code %in% selected_er_codes,
                                select = c(selected_gender, "LOCATION_INDEX"), drop = FALSE)
        
        general_result <- subset(population_data,  select = c(selected_gender, "LOCATION_INDEX"), drop = FALSE)
        
        if(nrow(filtered_data) == 0){
          result_aggr <- matrix(NA, nrow = length(selected_cities), ncol = 3)
          data <- data.frame(matrix(NA, nrow = length(city_name_list), ncol = (ncol(result_aggr)-1)), row.names = city_name_list)
          data[sort(selected_cities),] <- result_aggr[,1:2]
          colnames(data) <- c("LOCATION_INDEX", selected_gender)
          result_f <- as.matrix(cbind((1:length(city_name_list)), (data[,selected_gender]), data[,selected_gender], data[,selected_gender]))
          colnames(result_f) <- c("LOCATION_INDEX", "Raw_data", "PercentageOfAreaInCountry", "PercentageOfArea")
        }else{
          result_aggr <- aggregate(x = filtered_data, by = list(filtered_data$LOCATION_INDEX), FUN = "sum", na.rm = TRUE, drop =  FALSE)
          general_result_aggr <- aggregate(x = general_result, by = list(general_result$LOCATION_INDEX), FUN = "sum", na.rm = TRUE, drop =  FALSE)
          data <- data.frame(matrix(NA, nrow = length(city_name_list), ncol = 2))
          
          data[which(result_aggr[,1] %in% sort(selected_cities)),] <- result_aggr[,1:2]
          colnames(data) <- c("LOCATION_INDEX", selected_gender)
          general_data <- data.frame(matrix(NA, nrow = length(sort(selected_cities)), ncol = 2))
          general_data[which(result_aggr[,1] %in% sort(selected_cities)),] <- general_result_aggr[,1:2]
          colnames(general_data) <- c("LOCATION_INDEX", selected_gender)
          result_f <- cbind((1:nrow(data)),
                            data[,selected_gender],
                            round((100 * (data[,selected_gender]/sum(data[,selected_gender], na.rm = TRUE))), digits = 2),
                            round((100 * (data[,selected_gender]/as.numeric(general_data[,selected_gender]))), digits = 2))
          colnames(result_f) <- c("LOCATION_INDEX", "Raw_data", "PercentageOfAreaInCountry", "PercentageOfArea")
          
        }
        
        result <- data.frame(row.names = city_name_list)
        result[, "City"] <-  city_name_list
        result[, "Real count number of selected population in area in country"] <- result_f[,"Raw_data"]
        result[, "Percentage of selected population in area in country"] <- result_f[, "PercentageOfAreaInCountry"]
        result[, "Percentage of selected population in area"] <- result_f[, "PercentageOfArea"]
        population_result_table[[country]] <- result
        
      }
    }
    population_update_results(input$population_result_name, 
                              population_result_table,
                              population_selected_countries,
                              population_analysis_level,
                              population_census_year_OE, 
                              population_selected_cities_OE, 
                              population_selected_er_OE,
                              population_location_filter_OE,
                              population_er_filter_OE,
                              population_census_year_BUL, 
                              population_selected_cities_BUL, 
                              population_selected_er_BUL,
                              population_location_filter_BUL,
                              population_er_filter_BUL,
                              population_census_year_TUR,
                              population_selected_cities_TUR, 
                              population_selected_er_TUR,
                              population_location_filter_TUR,
                              population_er_filter_TUR)
    population_process_analysis()
  }
  
  # occupation
  occupation_process_clear_list <- function() {
    occupation_clear_results()
  }
  
  # population
  population_process_clear_list <- function() {
    population_clear_results()
  }
  
  # agriculture
  agricultural_process_clear_list <- function() {
    agricultural_clear_results()
  }
  
  # occupation
  occupation_process_analysis <- function() {
    if(input$occupation_result_list != "") {
      occupation_result_name <- input$occupation_result_list
      
      occupation_result_table <- session$userData$occupation_result_table[[occupation_result_name]]
      occupation_selected_countries <- session$userData$occupation_selected_countries[[occupation_result_name]]
      occupation_analysis_level <- session$userData$occupation_analysis_level[[occupation_result_name]]
      
      occupation_census_year_BUL <- session$userData$occupation_census_year_BUL[[occupation_result_name]]
      occupation_selected_occupation_groups_BUL <- unlist(get_selected(input$occupation_tree_BUL))
      occupation_selected_cities_BUL = session$userData$occupation_selected_cities_BUL[[occupation_result_name]]
      occupation_location_filter_BUL = session$userData$occupation_location_filter_BUL[[occupation_result_name]]
      occupation_selected_gender_BUL <- session$userData$occupation_selected_gender_BUL[[occupation_result_name]]
      
      occupation_census_year_TUR <- session$userData$occupation_census_year_TUR[[occupation_result_name]]
      occupation_selected_occupation_groups_TUR <- unlist(get_selected(input$occupation_tree_TUR))
      occupation_selected_cities_TUR = session$userData$occupation_selected_cities_TUR[[occupation_result_name]]
      occupation_location_filter_TUR = session$userData$occupation_location_filter_TUR[[occupation_result_name]]
      occupation_selected_gender_TUR <- session$userData$occupation_selected_gender_TUR[[occupation_result_name]]
      
      updateSelectInput(session, inputId = "occupation_plot_measure", choices = colnames(occupation_result_table)[-1], selected = colnames(occupation_result_table)[3])        
      
      output$occupation_leaflet_map <- renderUI({
        xlim_min <- c()
        xlim_max <- c()
        ylim_min <- c()
        ylim_max <- c()
        
        for(country in occupation_selected_countries){
          for(country in occupation_selected_countries){
            country_year_index <- c(occupation_analysis_level, country, get(paste0("occupation_census_year_", country)))
          }
          city_maps <- all_maps[[country_year_index]]
          
          xlim_min <- c(xlim_min, bbox(city_maps)[1, "min"])
          xlim_max <- c(xlim_max, bbox(city_maps)[1, "max"])
          ylim_min <- c(ylim_min, bbox(city_maps)[2, "min"])
          ylim_max <- c(ylim_max, bbox(city_maps)[2, "max"])
        }
        xlim_min <- min(xlim_min)
        xlim_max <- max(xlim_max)
        ylim_min <- min(ylim_min)
        ylim_max <- max(ylim_max)
        
        leafletOutput("occupation_result_map",
                      width = "100%",#max(700, 250 + 250 * (xlim_max - xlim_min) / (ylim_max - ylim_min)),
                      height = 500)
      })
      
      output$occupation_result_map <- renderLeaflet({
        map <- leaflet() %>% addTiles()
        colorvar <- input$occupation_plot_measure
        map_color <- input$occupation_map_color_mode
        
        global_map_data <- c()
        for(country in occupation_selected_countries){
          global_map_data <- rbind(global_map_data, occupation_result_table[[country]])
        }
        for(country in occupation_selected_countries){
          if(country == "BUL"){
            country_year_index <- c(country, occupation_census_year_BUL)
            census_year <- occupation_census_year_BUL
            selected_cities <- occupation_selected_cities_BUL
            location_filter <- occupation_location_filter_BUL
            display_range <- input$occupation_display_range_BUL
          }else{
            country_year_index <- c(country, occupation_census_year_TUR)
            census_year <- occupation_census_year_TUR
            selected_cities <- occupation_selected_cities_TUR
            location_filter <- occupation_location_filter_TUR
            display_range <- input$occupation_display_range_TUR
          }
          city_maps <- all_maps[[c(occupation_analysis_level, country_year_index)]]
          city_index <- all_index[[c(occupation_analysis_level, country_year_index)]]
          default_city_names <- all_index[[c(occupation_analysis_level, country_year_index)]]$NAME
          city_name_list <- as.character(default_city_names)
          
          if (location_filter == TRUE ) {
            selected_cities_names <- selected_cities
            selected_cities_names <- city_name_list[city_name_list %in% selected_cities_names]
          } else {
            selected_cities_names <- city_name_list
          }
          
          selected_cities <- as.numeric(unlist(city_index[as.character(unlist(city_index$NAME)) %in% selected_cities_names, 2]))
          
          city_values <- occupation_result_table[[country]]
          
          rownames(city_values) <- city_values[,"City"]
          
          min_value <- min(as.numeric(global_map_data[,colorvar]), na.rm = TRUE)
          max_value <- max(as.numeric(global_map_data[,colorvar]), na.rm = TRUE)
          step_value <- (max_value - min_value) / 100
          
          normalizer <- max(1, 1000^floor(log(max_value, 1000)))
          
          full_map_data <- data.frame(matrix(NA, nrow = length(city_name_list),
                                             ncol = ncol(city_values)), row.names = city_name_list)
          
          colnames(full_map_data) <- colnames(city_values)
          full_map_data[, "City"] <- city_name_list
          
          full_map_data[sort(unique(as.numeric(as.character(city_maps@data$MAP_ID)))) %in% sort(selected_cities),] <-  city_values
          
          # filter by display range
          if(colorvar == "Real count number of selected occupations in area in country"){
            full_map_data[!(full_map_data[, colorvar] >= display_range[1] * normalizer & full_map_data[, colorvar] <= display_range[2] * normalizer), colorvar] <- NA
          } else {
            full_map_data[is.na(full_map_data)] <- FALSE
            full_map_data[!(full_map_data[, colorvar] >= display_range[1] & full_map_data[, colorvar] <= display_range[2]), colorvar] <- NA
          }
          
          city_maps@data <- full_map_data[match(as.numeric(as.character(city_maps@data$MAP_ID)), city_index$LOCATION_ID),]
          
          map_data <- city_maps@data[,colorvar]
          
          color_index <- floor((map_data - min_value) / step_value) + 1
          color_index[color_index < 0] <- NA
          city_colors <- map_colors[[map_color]][color_index]
          city_colors[is.na(city_colors)] <- "gray"
          city_maps <- spTransform(city_maps, CRS("+proj=longlat +datum=WGS84 +no_defs"))
          
          if(colorvar == "Real count number of selected occupations in area in country"){
            map <- addPolygons(map = map, data = city_maps, color = "#000", weight = 1.0, opacity = 0.75, 
                               fillColor = city_colors, fillOpacity = 0.5,
                               popup = sprintf("<strong>%s</strong><br>%s: <strong>%.d</strong>",
                                               rownames(city_maps@data),
                                               colorvar,
                                               city_maps@data[,colorvar]))
          }else{
            map <- addPolygons(map = map, data = city_maps, color = "#000", weight = 1.0, opacity = 0.75, 
                               fillColor = city_colors, fillOpacity = 0.5,
                               popup = sprintf("<strong>%s</strong><br>%s: <strong>%.2f</strong>",
                                               rownames(city_maps@data), 
                                               colorvar, 
                                               city_maps@data[,colorvar]))
          }
        }
        map
        
        normalizer <- max(1, 1000^floor(log(max_value, 1000)))
        unit <- ifelse(normalizer == 1, "", ifelse(normalizer == 1000, "(thousands)", "(millions)"))
        
        map <- leaflet::addLegend(map = map, position = "bottomleft", pal = colorNumeric(palette = map_colors[[map_color]], domain = c(min_value, max_value) / normalizer),
                                  values = global_map_data[,colorvar] / normalizer, title = unit, labFormat = labelFormat(), opacity = 1, bins = 5)
        map <- leaflet::addLegend(map = map, position = "topright", pal = colorNumeric(palette = map_colors[[map_color]], domain = c(min_value, max_value) / normalizer),
                                  values = global_map_data[,colorvar] / normalizer, title = unit, labFormat = labelFormat(), opacity = 1, bins = 5)
        # map <- setMaxBounds(map = map, lng1 = xlim_min, lng2 = xlim_max, lat1 = ylim_min, lat2 = ylim_max)
      })
      
      output$occupation_display_range_BUL <- renderUI({
        global_map_data <- occupation_result_table[["BUL"]]
        
        colorvar <- input$occupation_plot_measure
        min_value <- min(global_map_data[,colorvar], na.rm = TRUE)
        max_value <- max(global_map_data[,colorvar], na.rm = TRUE)
        step_value <- (max_value - min_value) / 5
        
        normalizer <- max(1, 1000^floor(log(max_value, 1000)))
        unit <- ifelse(normalizer == 1, "", ifelse(normalizer == 1000, "(thousands)", "(millions)"))
        min_range <- min(0,floor(min_value/normalizer), na.rm = TRUE)
        max_range <- max(0,ceiling(max_value/normalizer), na.rm = TRUE)
        sliderInput(inputId = 'occupation_display_range_BUL', 
                    label = 'Display Range:', 
                    min = min_range,
                    max = max_range,
                    value = c(min_range, max_range)#, step = floor(step_value)
        )
      })
      
      output$occupation_display_range_TUR <- renderUI({
        global_map_data <- occupation_result_table[["TUR"]]
        
        colorvar <- input$occupation_plot_measure
        min_value <- min(global_map_data[,colorvar], na.rm = TRUE)
        max_value <- max(global_map_data[,colorvar], na.rm = TRUE)
        step_value <- (max_value - min_value) / 5
        
        normalizer <- max(1, 1000^floor(log(max_value, 1000)))
        unit <- ifelse(normalizer == 1, "", ifelse(normalizer == 1000, "(thousands)", "(millions)"))
        min_range <- min(0,floor(min_value/normalizer), na.rm = TRUE)
        max_range <- max(0,ceiling(max_value/normalizer), na.rm = TRUE)
        sliderInput(inputId = 'occupation_display_range_TUR', 
                    label = 'Display Range:', 
                    min = min_range,
                    max = max_range,
                    value = c(min_range, max_range)#, step = floor(step_value)
        )
      })
      
      output$occupation_result_plot <- renderGvis({
        global_map_data <- c()
        for(country in occupation_selected_countries){
          global_map_data <- rbind(global_map_data, occupation_result_table[[country]])
        }
        plot_var <- input$occupation_plot_measure
        
        period_values <- data.frame(row.names = rownames(global_map_data))
        period_values[,"Cities"] <- rownames(global_map_data)
        period_values[,plot_var] <- (global_map_data[,plot_var])
        
        plot_type <- input$occupation_plot_type
        plot_color <- input$occupation_plot_color_mode
        ylim_min <- 0
        ylim_max <- max(period_values[,plot_var], na.rm = TRUE)
        # normalizer <- max(1, 1000^floor(log(ylim_max, 1000)))
        normalizer <- 1
        period_values[,plot_var] <- period_values[,plot_var] / normalizer
        ylim_min <- ylim_min / normalizer
        ylim_max <- ylim_max / normalizer
        plot_var_long <- ifelse(normalizer == 1e+3, sprintf("%s (thousands)", plot_var), ifelse(normalizer == 1e+6, sprintf("%s (millions)", plot_var), plot_var))
        colnames(period_values)[which(colnames(period_values) == plot_var)] <- plot_var_long
        if (plot_type == "bar") {
          gc <- gvisColumnChart(data = period_values, xvar = "Cities", yvar = plot_var_long, chartid = "gvis",
                                options = list(title = "", width = "100%", #700, 
                                               height = 500, legend = "none",
                                               colors = sprintf("['%s']", plot_color),
                                               vAxis = sprintf("{'title': '%s', 'viewWindowMode': 'explicit', 'viewWindow': {'max': %f, 'min': %f}}", plot_var_long, ylim_min, ylim_max),
                                               hAxis = "{'slantedText': true, 'slantedTextAngle': 90}",
                                               chartArea = "{'width': '88%', 'left': '10%', 'height': '80%', 'bottom': '18%'}"))
        }
        if (plot_type == "line") {
          gc <- gvisLineChart(data = period_values, xvar = "Cities", yvar = plot_var_long, chartid = "gvis",
                              options = list(title = "", width = "100%", #700,
                                             height = 500, legend = "none",
                                             colors = sprintf("['%s']", plot_color),
                                             vAxis = sprintf("{'title': '%s', 'viewWindowMode': 'explicit', 'viewWindow': {'max': %f, 'min': %f}}", plot_var_long, ylim_min, ylim_max),
                                             hAxis = "{'slantedText': true, 'slantedTextAngle': 90}",
                                             chartArea = "{'width': '88%', 'left': '10%', 'height': '80%', 'bottom': '18%'}"))
        }
        gc
      })
      output$occupation_result_table <- DT::renderDataTable({
        global_map_data <- c()
        for(country in occupation_selected_countries){
          global_map_data <- rbind(global_map_data, occupation_result_table[[country]])
        }
        # plot_var <- input$plot_measure
        period_values <- data.frame(row.names = rownames(global_map_data))
        period_values[,"Cities"] <- rownames(global_map_data)
        period_values <- (global_map_data)
        return(period_values)
      }, options = list(searching = FALSE, paging = FALSE, ordering = TRUE, info = FALSE))
    }
  }
  
  # population
  population_process_analysis <- function() {
    if(input$population_result_list != "") {
      population_result_name <- input$population_result_list
      population_colorvar <- input$population_plot_measure
      population_result_table <- session$userData$population_result_table[[population_result_name]]
      population_selected_countries <- session$userData$population_selected_countries[[population_result_name]]
      population_analysis_level <-  session$userData$population_analysis_level[[population_result_name]]
      
      population_census_year_OE <- session$userData$population_census_year_OE[[population_result_name]]
      population_selected_cities_OE = session$userData$population_selected_cities_OE[[population_result_name]]
      population_location_filter_OE = session$userData$population_location_filter_OE[[population_result_name]]
      population_er_filter_OE = session$userData$population_er_filter_OE[[population_result_name]]
      population_selected_er_OE <- session$userData$population_selected_er_OE[[population_result_name]]
      population_display_range_OE <- input$population_display_range_OE
      population_gh_filter_OE = session$userData$population_gh_filter_OE[[population_result_name]]
      # population_household_filter_OE = session$userData$population_household_filter_OE[[population_result_name]]
      population_selected_gender_OE = session$userData$population_selected_gender_OE[[population_result_name]]
      population_selected_household_OE = session$userData$population_selected_household_OE[[population_result_name]]
      
      population_census_year_BUL <- session$userData$population_census_year_BUL[[population_result_name]]
      population_selected_cities_BUL = session$userData$population_selected_cities_BUL[[population_result_name]]
      population_location_filter_BUL = session$userData$population_location_filter_BUL[[population_result_name]]
      population_er_filter_BUL = session$userData$population_er_filter_BUL[[population_result_name]]
      population_selected_er_BUL <- session$userData$population_selected_er_BUL[[population_result_name]]
      population_display_range_BUL <- input$population_display_range_BUL
      population_gh_filter_BUL = session$userData$population_gh_filter_BUL[[population_result_name]]
      # population_household_filter_BUL = session$userData$population_household_filter_BUL[[population_result_name]]
      population_selected_gender_BUL = session$userData$population_selected_gender_BUL[[population_result_name]]
      population_selected_household_BUL = session$userData$population_selected_household_BUL[[population_result_name]]
      
      population_census_year_TUR <- session$userData$population_census_year_TUR[[population_result_name]]
      population_selected_cities_TUR = session$userData$population_selected_cities_TUR[[population_result_name]]
      population_location_filter_TUR = session$userData$population_location_filter_TUR[[population_result_name]]
      population_er_filter_TUR = session$userData$population_er_filter_TUR[[population_result_name]]
      population_selected_er_TUR <- session$userData$population_selected_er_TUR[[population_result_name]]
      population_display_range_TUR <-input$population_display_range_TUR
      population_gh_filter_TUR = session$userData$population_gh_filter_TUR[[population_result_name]]
      #  population_household_filter_TUR = session$userData$population_gender_filter_TUR[[population_result_name]]
      population_selected_gender_TUR = session$userData$population_selected_household_TUR[[population_result_name]]
      population_selected_household_TUR = session$userData$population_selected_household_TUR[[population_result_name]]
      
      updateSelectInput(session, inputId = "population_plot_measure", choices = c("Real count number of selected population in area in country", 
                                                                                  "Percentage of selected population in area in country",
                                                                                  "Percentage of selected population in area"),
                        selected = "Real count number of selected population in area in country") 
      
      output$population_leaflet_map <- renderUI({
        xlim_min <- c()
        xlim_max <- c()
        ylim_min <- c()
        ylim_max <- c()
        if(population_analysis_level == "point"){
          leafletOutput("population_result_map",
                        width = "100%", #max(700, 250 + 250 * (xlim_max - xlim_min) / (ylim_max - ylim_min)),
                        height = 500)
        }else{
          for(country in population_selected_countries){
            for(country in population_selected_countries){
              country_year_index <- c(population_analysis_level, country, get(paste0("population_census_year_", country)))
            }
            city_maps <- all_maps[[country_year_index]]
            
            xlim_min <- c(xlim_min, bbox(city_maps)[1, "min"])
            xlim_max <- c(xlim_max, bbox(city_maps)[1, "max"])
            ylim_min <- c(ylim_min, bbox(city_maps)[2, "min"])
            ylim_max <- c(ylim_max, bbox(city_maps)[2, "max"])
          }
          xlim_min <- min(xlim_min)
          xlim_max <- max(xlim_max)
          ylim_min <- min(ylim_min)
          ylim_max <- max(ylim_max)
          
          leafletOutput("population_result_map",
                        width = "100%", #max(700, 250 + 250 * (xlim_max - xlim_min) / (ylim_max - ylim_min)),
                        height = 500)}
      })
      
      observe({
        population_selected_countries <- input$population_selected_countries
        population_colorvar <- input$population_plot_measure
        for(country in population_selected_countries){
          
          req(population_result_table[[country]])
          global_map_data <- population_result_table[[country]]
          min_value <- min(global_map_data[,population_colorvar], na.rm = TRUE)
          max_value <- max(global_map_data[,population_colorvar], na.rm = TRUE)
          
          normalizer <- max(1, 1000^floor(log(max_value, 1000)))
          unit <- ifelse(normalizer == 1, "", ifelse(normalizer == 1000, "(thousands)", "(millions)"))
          min_range <- min(0,floor(min_value/normalizer), na.rm = TRUE)
          max_range <- max(0,ceiling(max_value/normalizer), na.rm = TRUE)
          updateSliderInput(session,
                            inputId = paste0("population_display_range_", country),
                            label = sprintf('Display Range in %s:',unit),
                            min = min_range,
                            max = max_range,
                            value = c(min_range, max_range)
          )
        }
      })
      
      output$population_result_map <- renderLeaflet({
        population_colorvar <- input$population_plot_measure
        population_display_range_OE <- input$population_display_range_OE
        population_display_range_BUL <- input$population_display_range_BUL
        population_display_range_TUR <- input$population_display_range_TUR
        
        if(population_analysis_level == "point"){
          
          map <- leaflet() %>% addTiles()
          for(country in population_selected_countries){
            
            census_year <- get(paste0("population_census_year_", country))
            country_year_index <- c(country, census_year)
            selected_location <- get(paste0("population_selected_cities_", country))
            location_filter <- get(paste0("population_location_filter_", country))
            gh_filter <- get(paste0("population_gh_filter_", country))
            # household_filter <- get(paste0("population_household_filter_", country))
            selected_gender <- get(paste0("population_selected_gender_", country))
            selected_household <- get(paste0("population_selected_household_", country))
            selected_er <- get(paste0("population_selected_er_", country))
            population_er_filter <- get(paste0("population_er_filter_", country))
            display_range <- get(paste0("population_display_range_", country))
            
            if(population_er_filter == TRUE){
              selected_er_codes <- selected_er
            }else{
              selected_er_codes <- levels(all_population_data[[population_analysis_level]][[country]][[census_year]]$ER_Code)
            }
            city_values <- population_result_table[[country]]
            city_values[, "sum"] <- apply(as.matrix(city_values[, selected_er_codes, drop = FALSE]), 1, sum)
            normalizer <- max(1, 1000^floor(log(max(city_values[, "sum"], na.rm = TRUE), 1000)))
            unit <- ifelse(normalizer == 1, "", ifelse(normalizer == 1000, "(thousands)", "(millions)"))
            city_values[is.na(city_values)] <- FALSE
            
            # filter by display_range
            if(sum(display_range != "") == 2){
              city_values[!(city_values[, "sum"] >= display_range[1] * normalizer & city_values[, "sum"] <= display_range[2] * normalizer), "sum"] <- 0
              
            }
            percentages <- round(100 * (city_values[,selected_er_codes] / city_values[,"sum"]), 2)
            
            if(length(selected_er_codes) == 1){
              percentages1 <- matrix(percentages, nrow = nrow(city_values), ncol = length(selected_er_codes), dimnames = list(rownames(city_values), selected_er_codes))
              percentages1[is.nan(percentages1)] <- 0
              percentages <- as.data.frame(matrix(paste("%",as.matrix(percentages1)), nrow = nrow(percentages1), ncol = length(selected_er_codes)), dimnames = list(rownames(city_values), selected_er_codes))
              colnames(percentages) <- selected_er_codes
              rownames(percentages) <- rownames(city_values)
            }else{
              percentages <- as.data.frame(matrix(paste("%",as.matrix(percentages)), nrow = nrow(percentages), ncol = length(selected_er_codes)))
              colnames(percentages) <- selected_er_codes
            } 
            if(sum(selected_er_codes == "Population") == 1){
              city_values <- as.data.frame(city_values)
              map <- addCircleMarkers(map = map,
                                      lng = city_values$E_coord,
                                      lat = city_values$N_coord,
                                      popup = sprintf("<strong>%s</strong><br>%s: <strong>%.d</strong>", city_values[, "City"], selected_er_codes, city_values[,selected_er_codes]),
                                      radius = 3 * (as.numeric(city_values[,selected_er_codes])/(max(as.numeric(city_values[,selected_er_codes])))),
                                      color = as.array(unlist(er_colors[selected_er_codes][1])), fillOpacity = 0.5, stroke = FALSE
              )
            }else{
              map <- leaflet() %>% addTiles() %>%
                addMinicharts(
                  lng = city_values$E_coord, lat = city_values$N_coord,
                  type = "pie",
                  chartdata = city_values[,selected_er_codes, drop = FALSE],
                  fillColor = as.array(unlist(er_colors[selected_er_codes][1])),
                  colorPalette = as.array(unlist(er_colors[selected_er_codes])),
                  width = 50 * sqrt(city_values$sum) / sqrt(max(city_values$sum)),
                  transitionTime = 0,
                  layerId = paste0(rownames(city_values), " Total ", selected_household, city_values[,"sum"]),
                  popup = popupArgs(labels = selected_er_codes, showValues = TRUE, supValues = as.data.frame(percentages[, selected_er_codes]), supLabels = colnames(percentages))
                  #popup = popupArgs(labels = sprintf("<strong>%s</strong><br>%s: <strong>%.f</strong><br>%.f", paste(selected_er_codes), selected_household, paste(percentages[,selected_er_codes])))
                )
            }
          }
        }else{
          map <-leaflet() %>% addTiles()
          colorvar <- population_colorvar
          map_color <- input$population_map_color_mode
          global_map_data <- c()
          for(country in population_selected_countries){
            global_map_data <- as.matrix(rbind(global_map_data, as.matrix(population_result_table[[country]])))
          }
          for(country in population_selected_countries){
            
            census_year <- get(paste0("population_census_year_", country))
            country_year_index <- c(country, census_year)
            selected_cities <- get(paste0("population_selected_cities_", country))
            location_filter <- get(paste0("population_location_filter_", country))
            gh_filter <- get(paste0("population_gh_filter_", country))
            #household_filter <- get(paste0("population_household_filter_", country))
            selected_gender <- get(paste0("population_selected_gender_", country))
            selected_household <- get(paste0("population_selected_household_", country))
            selected_er <- get(paste0("population_selected_er_", country))
            er_filter <- get(paste0("population_er_filter_", country))
            display_range <- get(paste0("population_display_range_", country))
            analysis_level <- population_analysis_level
            
            city_index <- all_index[[c(analysis_level, country_year_index)]]
            city_maps <- all_maps[[c(analysis_level, country_year_index)]]
            
            population_data <- all_population_data[[c(analysis_level, country_year_index)]]
            
            default_city_names <- city_index$NAME
            city_name_list <- default_city_names
            
            if (location_filter == TRUE ) {
              selected_cities_names <- selected_cities
              selected_cities_names <- city_name_list[city_name_list %in% selected_cities_names]
            } else {
              selected_cities_names <- city_name_list
            }
            selected_cities <- as.numeric(unlist(city_index[as.character(unlist(city_index$NAME)) %in% selected_cities_names, "LOCATION_ID"]))
            
            city_values <- population_result_table[[country]]
            rownames(city_values) <- city_values[,"City"]
            
            min_value <- min(as.numeric(global_map_data[,colorvar]), na.rm = TRUE)
            max_value <- max(as.numeric(global_map_data[,colorvar]), na.rm = TRUE)
            step_value <- (max_value - min_value) / 100
            normalizer <- max(1, 1000^floor(log(max_value, 1000)))
            
            full_map_data <- data.frame(matrix(NA, nrow = length(city_index$NAME),
                                               ncol = ncol(city_values)), row.names = city_index$NAME)
            full_map_data[sort(unique(as.numeric(as.character(city_maps@data$MAP_ID)))) %in% sort((selected_cities)),] <-  city_values#[sort(selected_cities),]
            
            full_map_data[, "City"] <- city_name_list
            colnames(full_map_data) <- colnames(city_values)
            full_map_data[is.na(full_map_data)] <- FALSE
            
            
            if(sum(display_range != "") == 2){
              #filter by display range
              if(colorvar == "Real count number of selected population in area in country"){
                full_map_data[!(full_map_data[, colorvar] >= display_range[1] * normalizer & full_map_data[, colorvar] <= display_range[2] * normalizer), colorvar] <- NA
              } else {
                #full_map_data[is.na(full_map_data)] <- FALSE
                full_map_data[!(full_map_data[, colorvar] >= display_range[1] & full_map_data[, colorvar] <= display_range[2]), colorvar] <- NA
              }
              
            }
            city_maps@data <- full_map_data[match(as.numeric(as.character(city_maps@data$MAP_ID)), city_index$LOCATION_ID),]
            
            map_data <- city_maps@data[,colorvar]
            color_index <- floor((map_data - min_value) / step_value) + 1
            color_index[color_index < 0] <- NA
            city_colors <- map_colors[[map_color]][color_index]
    
            city_colors[is.na(city_colors)] <- "gray"
            
            city_maps <- spTransform(city_maps, CRS("+proj=longlat +datum=WGS84 +no_defs"))
            
            if(colorvar == "Real count number of selected population in area in country"){
              map <- addPolygons(map = map, data = city_maps, color = "#000", weight = 1.0, opacity = 0.75, 
                                 fillColor = city_colors, fillOpacity = 0.5,
                                 popup = sprintf("<strong>%s</strong><br>%s: <strong>%.d</strong>",
                                                 rownames(city_maps@data),
                                                 colorvar,
                                                 city_maps@data[,colorvar]))
            }else{
              map <- addPolygons(map = map, data = city_maps, color = "#000", weight = 1.0, opacity = 0.75, 
                                 fillColor = city_colors, fillOpacity = 0.5,
                                 popup = sprintf("<strong>%s</strong><br>%s: <strong>%.2f</strong>",
                                                 rownames(city_maps@data), 
                                                 colorvar, 
                                                 city_maps@data[,colorvar]))}
          }
          normalizer <- max(1, 1000^floor(log(max_value, 1000)))
          unit <- ifelse(normalizer == 1, "", ifelse(normalizer == 1000, "(thousands)", "(millions)"))
          
          map <- leaflet::addLegend(map = map,
                                    position = "bottomleft",
                                    pal = colorNumeric(palette = map_colors[[map_color]], domain = c(min_value, max_value) / normalizer),
                                    values = as.numeric(global_map_data[,colorvar]) / normalizer, 
                                    title = unit, 
                                    labFormat = labelFormat(),
                                    opacity = 1,
                                    bins = 5)
          
          map <- leaflet::addLegend(map = map,
                                    position = "topright",
                                    pal = colorNumeric(palette = map_colors[[map_color]], domain = c(min_value, max_value) / normalizer),
                                    values = as.numeric(global_map_data[,colorvar]) / normalizer,
                                    title = unit, 
                                    labFormat = labelFormat(), 
                                    opacity = 1,
                                    bins = 5)
        }
        map
      })
      
      if(population_analysis_level == "point"){
        output$population_result_plot <- c()
      }else{
        output$population_result_plot <- renderGvis({
          plot_type <- input$population_plot_type
          plot_color <- input$population_plot_color_mode
          
          global_map_data <- c()
          for(country in population_selected_countries){
            global_map_data <- rbind(global_map_data, population_result_table[[country]])
          }
          
          plot_var <- input$population_plot_measure
          period_values <- data.frame(row.names = rownames(global_map_data))
          period_values[,"Cities"] <- rownames(global_map_data)
          period_values[,plot_var] <- (global_map_data[,plot_var])
          
          plot_type <- input$population_plot_type
          plot_color <- input$population_plot_color_mode
          ylim_min <- 0
          ylim_max <- max(period_values[,plot_var], na.rm = TRUE)
          #normalizer <- max(1, 1000^floor(log(ylim_max, 1000)))
          normalizer <- 1
          period_values[,plot_var] <- period_values[,plot_var] / normalizer
          ylim_min <- ylim_min / normalizer
          ylim_max <- ylim_max / normalizer
          plot_var_long <- ifelse(normalizer == 1e+3, sprintf("%s (thousands)", plot_var), ifelse(normalizer == 1e+6, sprintf("%s (millions)", plot_var), plot_var))
          colnames(period_values)[which(colnames(period_values) == plot_var)] <- plot_var_long
          if (plot_type == "bar") {
            gc <- gvisColumnChart(data = period_values, xvar = "", yvar = plot_var_long, chartid = "gvis",
                                  options = list(title = "", width = "100%", #700, 
                                                 height = 500, legend = "none",
                                                 colors = sprintf("['%s']", plot_color),
                                                 vAxis = sprintf("{'title': '%s', 'viewWindowMode': 'explicit', 'viewWindow': {'max': %f, 'min': %f}}", plot_var_long, ylim_min, ylim_max),
                                                 hAxis = "{'slantedText': true, 'slantedTextAngle': 90}",
                                                 chartArea = "{'width': '88%', 'left': '10%', 'height': '80%', 'bottom': '18%'}"))
          }
          if (plot_type == "line") {
            gc <- gvisLineChart(data = period_values, xvar = "Cities", yvar = plot_var_long, chartid = "gvis",
                                options = list(title = "", width = "100%", #700,
                                               height = 500, legend = "none",
                                               colors = sprintf("['%s']", plot_color),
                                               vAxis = sprintf("{'title': '%s', 'viewWindowMode': 'explicit', 'viewWindow': {'max': %f, 'min': %f}}", plot_var_long, ylim_min, ylim_max),
                                               hAxis = "{'slantedText': true, 'slantedTextAngle': 90}",
                                               chartArea = "{'width': '88%', 'left': '10%', 'height': '80%', 'bottom': '18%'}"))
          }
          gc
        })
      }
      output$population_result_table <- DT::renderDataTable({
        global_map_data <- c()
        if(!(population_analysis_level == "point")){
          for(country in population_selected_countries){
            global_map_data <- rbind(global_map_data, population_result_table[[country]])
          }
        }
        #plot_var <- input$population_plot_measure
        period_values <- data.frame(row.names = rownames(global_map_data))
        period_values[,"Cities"] <- rownames(global_map_data)
        period_values <- (global_map_data)
        return(period_values)
      }, options = list(searching = FALSE, paging = FALSE, ordering = TRUE, info = FALSE)
      )
    }
  }  
  
  blocked_processing <- function(process) {
    showModal(modalDialog(
      wellPanel(
        icon("pulse fa-spinner fa-5x", lib = "font-awesome"),
        HTML("<br>"),
        h2("Processing ..."), style = "color:#428bca; text-align:center;"),
      easyClose = FALSE, 
      footer = NULL,
      title = NULL
    ))
    process()
    removeModal()
  }
  observeEvent(input$occupation_add_to_list, blocked_processing(occupation_process_update_list))
  observeEvent(input$population_add_to_list, blocked_processing(population_process_update_list))
  observeEvent(input$occupation_clear_list, occupation_clear_list())
  observeEvent(input$population_clear_list, population_clear_list())
  
  observeEvent(input$occupation_result_list, occupation_process_analysis())
  observeEvent(input$population_result_list, population_process_analysis())
})
