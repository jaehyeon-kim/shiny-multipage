ui_login <- function(...) {
  args <- list(...)
  fluidRow(
    useShinyjs(),
    #css hex color - http://www.w3schools.com/cssref/css_colors.asp
    tags$head(tags$style(HTML(".container-fluid {margin: 25px;} #login_link {float:right;} #login_login {color: #006600;} #login_application {color: #0000ff;}"))),
    tags$head(tags$style(HTML(".input_msg {color: #FF9999;} .input_success {color: #339900;} .input_fail {color: #CC0033;} "))),
    tags$script('$(document).keyup(function(event) { if (event.keyCode == 13) { if($("#login_application").length) { $("#login_application").click(); } else { $("#login_login").click();}}});'),
    tags$script('$("#login_username").focus();'),
    column(3, offset = 4,
           wellPanel(
             div(id = "login_link",
                 actionButton("login_leave", "Leave", icon = icon("close"), width = "100px")
             ),
             br(),
             br(),
             h4("LOGIN"),
             textInput("login_username", "User name"),
             div(class = "input_msg", textOutput("login_username_msg")),
             passwordInput("login_password", "Password"),
             div(class = "input_msg", textOutput("login_password_msg")),
             actionButton("login_login", "Log in", icon = icon("sign-in"), width = "100px"),
             actionButton("login_register", "Register", icon = icon("user-plus"), width = "100px"),
             br(),
             div(class = "input_fail", textOutput("login_fail")),
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
                 h4(args$message)
                 )
           )
    )
  )
}

ui_register <- function(...) {
  args <- list(...)
  fluidRow(
    useShinyjs(),
    tags$head(tags$style(HTML(".container-fluid {margin: 25px;} #register_link {float:right;} #register_register {color: #FF6633;}"))),
    tags$head(tags$style(HTML(".input_msg {color: #FF9999;} .input_success {color: #339900;} .input_fail {color: #CC0033;} "))),
    tags$script('$(document).keyup(function(event) { if (event.keyCode == 13) { $("#register_register").click(); }});'),
    tags$script('$("#register_username").focus();'),
    column(3, offset = 4,
           wellPanel(
             div(id = "register_link",
                 actionButton("register_login", "Log in", icon = icon("sign-in"), width = "100px"),
                 actionButton("register_leave", "Leave", icon = icon("close"), width = "100px")
             ),
             br(),
             br(),
             h4("REGISTER"),
             textInput("register_username", "User name"),
             div(class = "input_msg", textOutput("register_username_msg")),
             passwordInput("register_password", "Password"),
             div(class = "input_msg", textOutput("register_password_msg")),
             passwordInput("register_password_re", "Re-enter password"),
             div(class = "input_msg", textOutput("register_password_re_msg")),
             textInput("register_app_key", "Application key"),
             div(class = "input_msg", textOutput("register_app_key_msg")),
             actionButton("register_register", "Register", icon = icon("user-plus"), width = "100px"),
             br(),
             div(class = "input_success", textOutput("register_success")),
             div(class = "input_fail", textOutput("register_fail"))
           )
    )
  )
}

ui_profile <- function(...) {
  args <- list(...)
  fluidRow(
    useShinyjs(),
    tags$head(tags$style(HTML(".container-fluid {margin: 25px;} #profile_link {float:right;} #profile_password_ch {color: #CC0033;}"))),
    tags$head(tags$style(HTML(".input_msg {color: #FF9999;} .input_success {color: #339900;} .input_fail {color: #CC0033;} "))),
    tags$script('$(document).keyup(function(event) { if (event.keyCode == 13) { $("#profile_password_ch").click(); }});'),
    tags$script('$("#profile_password").focus();'),
    column(3, offset = 4,
           wellPanel(
             div(id = "profile_link",
                 actionButton("profile_logout", "Log out", icon = icon("sign-out"), width = "100px"),
                 actionButton("profile_application", "App", icon = icon("bar-chart"), width = "100px")
             ),
             br(),
             br(),
             h4("PROFILE"),
             textInput("profile_username", "User name"),
             div(class = "input_msg", textOutput("profile_username_msg")),
             passwordInput("profile_password", "Password"),
             div(class = "input_msg", textOutput("profile_password_msg")),
             passwordInput("profile_password_new", "New password"),
             div(class = "input_msg", textOutput("profile_password_new_msg")),
             passwordInput("profile_password_new_re", "Re-enter new password"),
             div(class = "input_msg", textOutput("profile_password_new_re_msg")),
             actionButton("profile_password_ch", "Change password", icon = icon("exclamation"), width = "150px"),
             br(),
             div(class = "input_success", textOutput("profile_success")),
             div(class = "input_fail", textOutput("profile_fail"))
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
                 actionButton("application_profile", "Profile", icon = icon("edit"), width = "100px")
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


