library(sqldf)
library(bcrypt)
source("utils/db.R")
source("utils/logic.R")

## initialize
init_users <- function(verbose = FALSE) {
  if("conn" %in% ls()) dbDisconnect(conn)
  
  db_name <- "db.sqlite"
  if(file.exists(db_name)) unlink(db_name, force = TRUE)
  
  qry_create_user_info <- "
  CREATE TABLE user_info (name TEXT, password TEXT, app_name TEXT, added_ts DATETIME)
  "
  qry_create_user_log <- "
  CREATE TABLE user_log (name TEXT, app_name TEXT, is_in INTEGER, added_ts DATETIME)
  "
  
  users <- data.frame(name = c("admin", "john.doe", "jane.doe"),
                      password = unlist(lapply(c("admin", "john.doe", "jane.doe"), hashpw)),
                      app_name = c("all", rep("multipage demo", 2)),
                      added_ts = rep(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), 3),
                      stringsAsFactors = FALSE)
  
  conn <- connect_db(db_name = db_name, verbose = verbose)
  is_user_info_created <- send_query(conn = conn, query = qry_create_user_info, verbose = verbose)
  is_user_log_created <- send_query(conn = conn, query = qry_create_user_log, verbose = verbose)
  
  if(is_user_log_created$is_success) {
    is_users_inserted <- lapply(1:nrow(users), function(x) {
      qry_insert_user <- set_insert_qry(tbl = "user_info", rec = as.list(users[x,]))
      send_query(conn = conn, query = qry_insert_user, verbose = verbose)
    })
  }
  
  users_rs <- get_query(conn = conn, query = "SELECT * FROM user_info", verbose = verbose)
  if(verbose) print(users_rs)
  
  dbDisconnect(conn)
  
  1
}

# initialize
# init_users()

# check user info
# user_info <- get_query(conn = NULL, db_name = "db.sqlite", query = "SELECT * FROM user_info", to_disconnect = TRUE)
# user_log <- get_query(conn = NULL, db_name = "db.sqlite", query = "SELECT * FROM user_log", to_disconnect = TRUE)

# check login credentials
# check_login_credentials(username = "admin", password = "admin", app_name = "any value")
# check_login_credentials(username = "admin", password = "admin_11", app_name = "any value")
# 
# check_login_credentials(username = "john.doe", password = "john.doe", app_name = "multipage demo")
# check_login_credentials(username = "john.doe", password = "john.doe", app_name = "any value")

