ui_login <- function(...) {
  args <- list(...)
  fluidRow(
    useShinyjs(),
    tags$head(tags$style(HTML(".container-fluid {margin: 25px;}"))),
    tags$script('$(document).keyup(function(event) { if (event.keyCode == 13) { $("#login_login").click(); }});'),
    column(3, offset = 4,
           wellPanel(
             h4("LOGIN"),
             textInput("login_username", "User name"),
             passwordInput("login_password", "Password"),
             actionButton("login_login", "Log in", icon = icon("sign-in"), width = "100px"),
             actionButton("login_register", "Register", icon = icon("user-plus"), width = "100px"),
             uiOutput("login_more")
           )
    )
  )
}

ui_logout <- function(...) {
  args <- list(...)
  fluidRow(
    useShinyjs(),
    tags$head(tags$style(HTML(".container-fluid {margin: 25px;} #logout_message {text-align: center;}"))),
    column(3, offset = 4,
           wellPanel(
             div(id = "logout_message",
                 h4("You are logged out now!")
                 )
           )
    )
  )
}

ui_register <- function(...) {
  args <- list(...)
  fluidRow(
    useShinyjs(),
    tags$head(tags$style(HTML(".container-fluid {margin: 25px;} #register_link {float:right;}"))),
    tags$script('$(document).keyup(function(event) { if (event.keyCode == 13) { $("#register_register").click(); }});'),
    column(3, offset = 4,
           wellPanel(
             div(id = "register_link",
                 actionButton("register_login", "Log in", icon = icon("arrow-left"), width = "100px")
             ),
             br(),
             br(),
             h4("REGISTER"),
             textInput("register_username", "User name"),
             passwordInput("register_password", "Password"),
             passwordInput("register_password_re", "Re-enter password"),
             textInput("register_key", "Application key"),
             actionButton("register_register", "Register", icon = icon("user-plus"), width = "100px")
           )
    )
  )
}

ui_profile <- function(...) {
  args <- list(...)
  fluidRow(
    useShinyjs(),
    tags$head(tags$style(HTML(".container-fluid {margin: 25px;} #profile_link {float:right;}"))),
    tags$script('$(document).keyup(function(event) { if (event.keyCode == 13) { $("#profile_password_ch").click(); }});'),
    column(3, offset = 4,
           wellPanel(
             div(id = "profile_link",
                 actionButton("profile_logout", "Log out", icon = icon("sign-out"), width = "100px"),
                 actionButton("profile_application", "Application", icon = icon("arrow-right"), width = "110px")
             ),
             br(),
             br(),
             h4("PROFILE"),
             textInput("profile_username", "User name"),
             passwordInput("profile_password", "Password"),
             passwordInput("profile_password_re", "Re-enter password"),
             actionButton("profile_password_ch", "Change password", icon = icon("exclamation"), width = "150px")
           )
    )
  )
}

ui_application <- function(...) {
  args <- list(...)
  fluidRow(
    useShinyjs(),
    tags$head(tags$style(HTML(".container-fluid {margin: 25px;} #application_link {float:right;} #application_user {text-align: right;}"))),
    column(10, offset = 1,
           wellPanel(
             div(id = "application_user",
                 p(paste0("Logged in as '", args$username, "'"))
                 ),
             div(id = "application_link",
                 actionButton("application_logout", "Log out", icon = icon("sign-out"), width = "100px"),
                 actionButton("application_profile", "Profile", icon = icon("arrow-right"), width = "100px")
             ),
             titlePanel("Old Faithful Geyser Data"),
             sidebarLayout(
               sidebarPanel(sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30)),
               mainPanel(plotOutput("distPlot"))
               )
             )
           )
    )
}

ui <- (htmlOutput("page"))


