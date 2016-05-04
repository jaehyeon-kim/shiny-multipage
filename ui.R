ui_login <- function(...) {
  args <- list(...)
  fluidRow(
    tags$head(tags$style(HTML(".container-fluid {margin: 25px;}"))),
    column(3, offset = 4,
           wellPanel(
             h4("LOGIN"),
             textInput("login_username", "User name"),
             passwordInput("login_password", "Password"),
             br(),
             actionButton("login_login", "Log in", icon = icon("sign-in"), width = "100px"),
             actionButton("login_register", "Register", icon = icon("user-plus"), width = "100px"),
             actionButton("login_profile", "Profile", icon = icon("arrow-right"), width = "100px")
           )
    )
  )
}

ui_profile <- function(...) {
  args <- list(...)
  fluidRow(
    tags$head(tags$style(HTML(".container-fluid {margin: 25px;}"))),
    column(3, offset = 4,
           wellPanel(
             h4("PROFILE"),
             textInput("profile_username", "User name"),
             br(),
             actionButton("login_login", "Log in", icon = icon("sign-in"), width = "100px"),
             hr(),
             actionButton("profile_password", "Change password", icon = icon("sign-in"), width = "100px")
           )
    )
  )
}

ui_register <- function(...) {
  args <- list(...)
  fluidRow(
    tags$head(tags$style(HTML(".container-fluid {margin: 25px;}"))),
    column(3, offset = 4,
           wellPanel(
             h4("REGISTER"),
             textInput("register_username", "User name"),
             passwordInput("register_password", "Password"),
             passwordInput("register_password_re", "Re-enter password"),
             textInput("register_key", "Application key"),
             br(),
             actionButton("register_login", "Log in", icon = icon("arrow-left"), width = "100px")
           )
    )
  )
}

ui_main <- function(...) {
  args <- list(...)
  fluidRow(
    div(id = "greeting", h1(paste0("Hello ", args$username, "!"))),
    actionButton("main_login", "Log in", icon = icon("arrow-left"), width = "100px")
  )
}

ui <- (htmlOutput("page"))


