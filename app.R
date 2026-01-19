library(shiny)
library(bslib)
library(bsicons)

source("Simplex.R")
source("ProjectData.R")

# UI
ui <- page(
  title = "City Pollution Reduction Solver",
  theme = bs_theme(version = 5, bootswatch = "darkly"),
  
  # CSS for slider
  tags$head(
    tags$style(HTML("
      #company-checkbox-wrapper {
        max-height: 45vh; 
        overflow-y: auto;
        border: 1px solid #444;
        padding: 10px;
        border-radius: 5px;
        background-color: #2b2b2b;
      }
    "))
  ),
  
  layout_sidebar(
    sidebar = sidebar(
      title = "Optimization Controls",
      p("Select the mitigation projects to include in the optimization."),
      
      # Batch selection controls
      layout_columns(
        col_widths = c(6, 6),
        actionButton("select_all", "Select All", class = "btn-sm btn-outline-light w-100"),
        actionButton("reset_selection", "Reset", class = "btn-sm btn-outline-light w-100")
      ),
      tags$hr(),
      
      h5("Available Projects"),
      div(
        id = "company-checkbox-wrapper",
        uiOutput("company_checkboxes")
      ),
      tags$hr(),
      
      # Calculate button
      actionButton(
        "calculate_btn", 
        "Calculate Optimal Solution",
        icon = icon("cogs"),
        class = "btn-primary btn-lg w-100"
      )
    ),
    
    # Results Area
    navset_card_tab(
      id = "results_tabs",
      
      nav_panel(
        "Optimal Solution",
        layout_columns(
          col_widths = c(6, 6),
          uiOutput("cost_box"),
          uiOutput("status_box")
        ),
        card(
          card_header("Solution Details"),
          card_body(
            uiOutput("solution_feedback"), 
            tableOutput("Optimized_solution"),
            tags$hr(),
            h5("Selected Projects for Calculation"),
            uiOutput("selected_output"),
            uiOutput("Initial_tableau_label"),
            uiOutput("Initial_tableau"),
          )
        )
      ),
      
      nav_panel(
        "Solver Iterations",
        card(
          card_header("Simplex Tableau Iterations"),
          card_body(
            p("Each tab below shows the state of the Simplex tableau at each step of the algorithm."),
            uiOutput("Iteration_solution")
          )
        )
      ),
      
      nav_panel(
        "All Data",
        card(
          card_header("All Mitigation Project Options"),
          tableOutput("All_Projects")
        ),
        card(
          card_header("Pollutant Targets"),
          tableOutput("All_Targets")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive values to hold state across user interactions
  selected_companies_rv <- reactiveVal(character(0))
  optimized_cost_rv <- reactiveVal(NULL)
  status_rv <- reactiveVal(NULL) # Tracks: Optimal, Infeasible, or Error
  selected_names_rv <- reactiveVal(character(0))
  
  # Matrix data holders
  results_table_rv <- reactiveVal(NULL)
  iteration_table_rv <-reactiveVal(NULL)
  initial_tableau_rv <- reactiveVal(NULL)
  iteration_basicsol_rv <- reactiveVal(NULL)
  
  # For selection
  observeEvent(input$select_all, { selected_companies_rv(project_full_names) })
  observeEvent(input$reset_selection, { selected_companies_rv(character(0)) })
  
  # Sync checkbox clicks with reactive state
  observeEvent(input$companies_group, {
    if (!isTRUE(input$select_all) && !isTRUE(input$reset_selection)) {
      selected_companies_rv(input$companies_group)
    }
  }, ignoreInit = TRUE)
  
  output$company_checkboxes <- renderUI({
    checkboxGroupInput("companies_group", label = NULL, choices = project_full_names, selected = selected_companies_rv())
  })
  
  # Calculate Button
  observeEvent(input$calculate_btn, {
    current_selection <- selected_companies_rv()
    selected_names_rv(current_selection)
    
    # Creates the Transposed Matrix
    data_prep <- MinimizationTranspose(namesToMatrix(current_selection), pollutant_targets)
    
    # Uses the Simplex by inputting the Transposed Matrix
    solution <- SimplexMethod(data_prep)
    
    # Update everything
    initial_tableau_rv(solution$InitialTableau)
    iteration_table_rv(solution$TableauIteration)
    iteration_basicsol_rv(solution$BasicSolIteration)
    status_rv(solution$Status)
    
    # Checks if Optimal
    # Prints results and optimized cost only when optimal
    if(solution$Status == "Optimal"){
      solution_matrix <- solution$forPrintSol
      if (is.matrix(solution_matrix) || is.data.frame(solution_matrix)) {
        colnames(solution_matrix) <- c("Mitigation Project", "Number of Project Units", "Cost($)")
      }
      results_table_rv(solution_matrix)
      optimized_cost_rv(format(round(solution$Z, 2), big.mark = ",", nsmall = 2))
      
      showNotification("Calculation Successful", type = "message")
    } else {
      # Handle Infeasible/Error states
      results_table_rv(NULL)
      optimized_cost_rv(NULL)
      showNotification(paste("Calculation stopped:", solution$Status), type = "warning", duration = 8)
    }
  })
  
  # Iteration observer
  observe({
    req(iteration_table_rv())
    lapply(names(iteration_table_rv()), function(name) {
      
      # Render Tableau Matrix
      output[[paste0(name, "_tableau")]] <- renderTable({
        tbl <- iteration_table_rv()[[name]]
        tbl <- as.data.frame(tbl)
        # Rename generic "V" variables to "S" 
        if (!is.null(colnames(tbl))) {
          defaultCols <- grep("^V[0-9]+$", colnames(tbl))
          if (length(defaultCols) > 0) colnames(tbl)[defaultCols] <- sub("^V", "S", colnames(tbl)[defaultCols])
        } else {
          colnames(tbl) <- paste0("S", seq_len(ncol(tbl)))
        }
        tbl
      }, striped = TRUE, bordered = TRUE, digits = 4)
      
      # Render Basic Solution
      output[[paste0(name, "_basicsol")]] <- renderTable({
        req(iteration_basicsol_rv())
        tbl <- iteration_basicsol_rv()[[name]]
        tbl <- as.data.frame(tbl)
        # Rename generic "V" variables to "S" 
        if (ncol(tbl) > 0) {
          if(is.null(colnames(tbl))) colnames(tbl) <- paste0("S", seq_len(ncol(tbl)))
          defaultCols <- grep("^V[0-9]+$", colnames(tbl))
          if (length(defaultCols) > 0) colnames(tbl)[defaultCols] <- sub("^V", "S", colnames(tbl)[defaultCols])
        }
        tbl 
      }, striped = TRUE, bordered = TRUE, digits = 4)
    })
  })
  
  # Shows red alert if Infeasible
  output$solution_feedback <- renderUI({
    stat <- status_rv()
    if(is.null(stat) || stat == "Optimal") return(NULL)
    
    div(class = "alert alert-danger",
        h4(bs_icon("exclamation-triangle-fill"), " No Optimal Solution Found"),
        p(paste("The solver stopped because the problem appears to be:", stat)),
        p("Check the 'Solver Iterations' tab to see where the algorithm stopped.")
    )
  })
  
  output$selected_output <- renderUI({
    names <- selected_names_rv()
    if (length(names) == 0) {
      div(class = "alert alert-secondary", "No projects selected.")
    } else {
      tags$ul(lapply(names, tags$li))
    }
  })
  
  # Shows Cost or Failure status
  output$cost_box <- renderUI({
    cost <- optimized_cost_rv()
    stat <- status_rv()
    
    if (is.null(stat)) {
      value_box("Optimized Cost", "N/A", showcase = bs_icon("hourglass"), theme = "bg-secondary")
    } else if (stat == "Infeasible" || stat == "Error") {
      value_box("Optimized Cost", "Failed", showcase = bs_icon("x-circle"), theme = "bg-danger", p("See iterations"))
    } else {
      value_box("Optimized Cost", paste0("$", cost), showcase = bs_icon("cash-coin"), theme = "bg-primary")
    }
  })
  
  # Shows status
  output$status_box <- renderUI({
    stat <- status_rv()
    if (is.null(stat)) {
      value_box("Status", "Pending", showcase = bs_icon("hourglass"), theme = "bg-secondary")
    } else if (stat == "Infeasible" || stat == "Error") {
      value_box("Status", stat, showcase = bs_icon("exclamation-triangle"), theme = "bg-danger")
    } else {
      value_box("Status", "Solved", showcase = bs_icon("check-circle"), theme = "bg-success")
    }
  })
  
  # Prints Initial Tableau label 
  output$Initial_tableau_label <- renderUI({ req(initial_tableau_rv()); HTML('<h3><strong>Initial Tableau:</strong></h3>') })
  
  # Shows the initial Tableau
  output$Initial_tableau <- renderUI({
    req(initial_tableau_rv())
    renderTable({
      tbl <- as.data.frame(initial_tableau_rv())
      if (!is.null(colnames(tbl))) {
        defaultCols <- grep("^V[0-9]+$", colnames(tbl))
        if (length(defaultCols) > 0) colnames(tbl)[defaultCols] <- sub("^V", "S", colnames(tbl)[defaultCols])
      } else {
        colnames(tbl) <- paste0("S", seq_len(ncol(tbl)))
      }
      tbl
    }, striped = TRUE, bordered = TRUE, digits = 4)
  })
  
  # Shows the optimized solution
  output$Optimized_solution <- renderTable({
    req(results_table_rv())
    data.frame(results_table_rv(), check.names = FALSE)
  }, striped = TRUE, bordered = TRUE, digits = 4)
  
  # Creates dynamic tabs for each iteration
  output$Iteration_solution <- renderUI({
    req(iteration_table_rv())
    tabs <- lapply(names(iteration_table_rv()), function(name) {
      tabPanel(
        title = name,
        HTML('<h3><strong>Tableau:</strong></h3>'),
        tableOutput(outputId = paste0(name, "_tableau")),
        HTML('<h3><strong>Basic Solution:</strong></h3>'),
        tableOutput(outputId = paste0(name, "_basicsol"))
      )
    })
    do.call(tabsetPanel, tabs)
  })
  
  # Reference Data Tables
  output$All_Projects <- renderTable({ projects_matrix }, striped = TRUE, bordered = TRUE,rownames = TRUE)
  output$All_Targets <- renderTable({ pollutant_targets }, striped = TRUE, bordered = TRUE,rownames = TRUE)
}

shinyApp(ui = ui, server = server)