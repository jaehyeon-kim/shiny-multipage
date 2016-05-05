is_logged <- FALSE
auth_username <- "admin"
auth_password <- "admin"

render_page <- function(..., f, title = "multipage demo", theme = shinytheme("cerulean")) {
  page <- f(...)
  renderUI({
    div(class = "outer", fluidPage(page, title = title, theme = theme))
  })
}

server <- function(input, output, session) {
  ## authentication
  user_info <- reactiveValues(is_logged = is_logged)
  
  tryCatch({
    observeEvent(input$login_login, {
      username <- isolate(input$login_username)
      password <- isolate(input$login_password)
      is_same_username <- auth_username == username
      is_same_password <- auth_password == password
      if (is_same_username & is_same_password) {
        user_info$is_logged <- TRUE
        user_info$username <- input$login_username
      }
    })
  }, error = function(err) 0)
  
  ## add profile, application buttons when logged in
  observe({
    if(user_info$is_logged) {
      shinyjs::disable("login_username")
      shinyjs::disable("login_password")
      output$login_more <- renderUI({
        list(
          hr(),
          actionButton("login_profile", "Profile", icon = icon("arrow-left"), width = "100px"),
          actionButton("login_application", "Application", icon = icon("arrow-right"), width = "110px")
        )
      })
    }
  })
  
  ## render default login page
  output$page <- render_page(f = ui_login)
  
  ## render a different page from login page - register, profile, application
  tryCatch({
    observeEvent(input$login_register, {
      output$page <- render_page(f = ui_register)
    })
  }, error = function(err) 0)
  
  tryCatch({
    observeEvent(input$login_profile, {
      output$page <- render_page(f = ui_profile)
    })
  }, error = function(err) 0)
  
  tryCatch({
    observeEvent(input$login_application, {
      output$page <- render_page(username = isolate(user_info$username), f = ui_application)
    })
  }, error = function(err) 0)
  
  ## render a different page from register page - login
  tryCatch({
    observeEvent(input$register_login, {
      output$page <- render_page(f = ui_login)
    })
  }, error = function(err) 0)
  
  ## render a different page from profile - logout, application
  tryCatch({
    observeEvent(input$profile_logout, {
      output$page <- render_page(f = ui_logout)
    })
  }, error = function(err) 0)
  
  tryCatch({
    observeEvent(input$profile_application, {
      output$page <- render_page(username = isolate(user_info$username), f = ui_application)
    })
  }, error = function(err) 0)
  
  ## render a different page from application - logout, profile
  tryCatch({
    observeEvent(input$application_logout, {
      output$page <- render_page(f = ui_logout)
    })
  }, error = function(err) 0)
  
  tryCatch({
    observeEvent(input$application_profile, {
      output$page <- render_page(f = ui_profile)
    })
  }, error = function(err) 0)
  
  ## render plot
  tryCatch({
    output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
  })
}
