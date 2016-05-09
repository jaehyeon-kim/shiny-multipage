init_users()

app_name <- "multipage demo"
is_logged <- FALSE
auth_username <- "admin"
auth_password <- "admin"
app_key <- "application-key"

render_page <- function(..., f, title = app_name, theme = shinytheme("cerulean")) {
  page <- f(...)
  renderUI({
    fluidPage(page, title = title, theme = theme)
  })
}

server <- function(input, output, session) {
  ######################################################
  ################### Authentication ###################
  ######################################################
  user_info <- reactiveValues(is_logged = is_logged)
  
  tryCatch({
    observeEvent(input$login_login, {
      username <- isolate(input$login_username)
      password <- isolate(input$login_password)
      
      is_valid_credentials <- check_login_credentials(username = username, password = password, app_name = app_name)
      if(is_valid_credentials) {
        user_info$is_logged <- TRUE
        user_info$username <- username
      }
      
      log_session(username = username, is_in = 1, app_name = app_name)
      
      1
    })
  }, error = function(err) 0)
  
  ###############################################################
  ################### Render Pages/UI Element ###################
  ###############################################################
  ## add profile, application buttons when logged in
  observe({
    if(user_info$is_logged) {
      shinyjs::disable("login_username")
      shinyjs::disable("login_password")
      output$login_more <- renderUI({
        list(
          hr(),
          actionButton("login_profile", "Profile", icon = icon("edit"), width = "100px"),
          actionButton("login_application", "Application", icon = icon("bar-chart"), width = "110px")
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
      
      output$register_success <- renderText("")
      output$register_fail <- renderText("")
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
      output$page <- render_page(message = "You are logged out now!", f = ui_logout)
      log_session(username = user_info$username, is_in = 0, app_name = app_name)
      stopApp(returnValue = 1)
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
      output$page <- render_page(message = "You are logged out now!", f = ui_logout)
      log_session(username = user_info$username, is_in = 0, app_name = app_name)
      stopApp(returnValue = 1)
    })
  }, error = function(err) 0)
  
  tryCatch({
    observeEvent(input$application_profile, {
      output$page <- render_page(f = ui_profile)
    })
  }, error = function(err) 0)
  
  ########################################################
  ###################### Page Logic ######################
  ########################################################
  ## register messages
  observe({
    if(!is.null(input$register_register)) {
      username <- input$register_username
      password <- input$register_password
      password_re <- input$register_password_re
      app_key <- input$register_app_key
      if(username != "") output$register_username_msg <- renderText("")
      if(password != "") output$register_password_msg <- renderText("")
      if(password_re != "") output$register_password_re_msg <- renderText("")
      if(app_key != "") output$register_app_key_msg <- renderText("")
    }
  })
  
  ## register action
  observeEvent(input$register_register, {
    username <- input$register_username
    password <- input$register_password
    password_re <- input$register_password_re
    app_key <- input$register_app_key
    
    if(username == "") output$register_username_msg <- renderText("Please enter user name")
    if(password == "") output$register_password_msg <- renderText("Please enter password")
    if(password_re == "") output$register_password_re_msg <- renderText("Please enter password again")
    if(app_key == "") output$register_app_key_msg <- renderText("Please enter application key")
    
    withProgress(message = "Validation", detail = "Credentials are validated...", value = 0, {
      if(!any(username == "", password == "", password_re == "", app_key == "")) {
        is_registerable <- check_registration_info(username = username, password = NULL, app_key = app_key, app_name = app_name, conn = NULL, to_disconnect = TRUE, verbose = FALSE)
        if(password != password_re) {
          output$register_fail <- renderText("Enter same password")
        } else if(!is_registerable$app_key_pass) {
          output$register_fail <- renderText("App key doesn't match")
        } else if(!is_registerable$user_not_found) {
          output$register_fail <- renderText("Same user name exists")
        } else {
          is_registered <- register_user(username = username, password = password, password_re = password_re, app_key = app_key, app_name = app_name, conn = NULL, to_disconnect = TRUE, verbose = FALSE)
          if(is_registered) {
            output$register_fail <- renderText("")
            output$register_success <- renderText("Registration succeeded")
            shinyjs::disable("register_username")
            shinyjs::disable("register_password")
            shinyjs::disable("register_password_re")
            shinyjs::disable("register_app_key")
            
            Sys.sleep(1)
            output$page <- render_page(f = ui_login)
          } else {
            output$register_success <- renderText("")
            output$register_fail <- renderText("Registration failed, try again or contact admin")
          }
        }
      }
    })
  })
  
#   tryCatch({
#     observeEvent(input$register_register, {
#       username <- input$register_username
#       password <- input$register_password
#       password_re <- input$register_password_re
#       app_key <- input$register_app_key
#       
#       if(username == "") output$register_username_msg <- renderText("Please enter user name")
#       if(password == "") output$register_password_msg <- renderText("Please enter password")
#       if(password_re == "") output$register_password_re_msg <- renderText("Please enter password again")
#       if(app_key == "") output$register_app_key_msg <- renderText("Please enter application key")
#       
#       if(!any(username == "", password == "", password_re == "", app_key == "")) {
#         is_registerable <- check_registration_info(username = username, password = NULL, app_key = app_key, app_name = app_name, conn = NULL, to_disconnect = TRUE, verbose = FALSE)
#         if(password != password_re) {
#           output$register_fail <- renderText("Enter same password")
#         } else if(!is_registerable$app_key_pass) {
#           output$register_fail <- renderText("App key doesn't match")
#         } else if(!is_registerable$user_not_found) {
#           output$register_fail <- renderText("Same user name exists")
#         } else {
#           is_registered <- register_user(username = username, password = password, password_re = password_re, app_key = app_key, app_name = app_name, conn = NULL, to_disconnect = TRUE, verbose = FALSE)
#           if(is_registered) {
#             output$register_fail <- renderText("")
#             output$register_success <- renderText("Registration succeeded")
#           } else {
#             output$register_success <- renderText("")
#             output$register_fail <- renderText("Registration failed, try again or contact admin")
#           }
#         }
#       }
#     })
#   })
  
  ########################################################
  ################### Application Code ###################
  ########################################################
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
