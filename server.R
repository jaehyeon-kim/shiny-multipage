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
  user_info <- reactiveValues(is_logged = is_logged)
  
  observe({ 
    if (!user_info$is_logged) {
      if (!is.null(input$login_login)) {
        # observeEvent equivalent
        if(input$login_login > 0) {
          username <- isolate(input$login_username)
          password <- isolate(input$login_password)
          is_same_username <- auth_username == username
          is_same_password <- auth_password == password
          if (is_same_username & is_same_password) {
            user_info$is_logged <- TRUE
            user_info$username <- input$login_username
          }
        }
      }
    }
  })
  
  ## render default login page
  observe({
    if(!user_info$is_logged) {
      output$page <- render_page(f = ui_login)
    }
  })
  
  ## render a different page from login page
  observe({
    if(!is.null(input$login_login) && input$login_login > 0) {
      if(user_info$is_logged) {
        output$page <- render_page(username = isolate(user_info$username), f = ui_main)
      } else {
        ### you're not logged in
      }
    }
    
#     if(!is.null(input$login_login) && input$login_profile > 0) {
#       output$page <- render_page(f = ui_profile)
#     }
    
    if(!is.null(input$login_login) && input$login_register > 0) {
      if(!user_info$is_logged) {
        output$page <- render_page(f = ui_register)
      } else {
        ### you've already registered
      }
    }
  })
  
  ## render a different page from register page
  observe({
    if(!is.null(input$register_login) && input$register_login > 0) {
      output$page <- render_page(f = ui_login)
    }
  })
  
  ## render a different page from main page
  observe({
    if(!is.null(input$main_login)) {
      if(input$main_login > 0) {
        output$page <- render_page(f = ui_login)
      }
    }
  })
}
