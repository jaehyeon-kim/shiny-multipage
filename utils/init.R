# library(sqldf)
# library(bcrypt)
# source("utils/db.R")
# source("utils/logic.R")

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
  
  qry_create_app_key <- "
  CREATE TABLE app_key (name TEXT, app_name TEXT, added_ts DATETIME)
  "
  
  app_name <- "multipage demo"
  added_ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  users <- data.frame(name = c("admin", "john.doe", "jane.doe"),
                      password = unlist(lapply(c("admin", "john.doe", "jane.doe"), hashpw)),
                      app_name = c("all", rep(app_name, 2)),
                      added_ts = rep(added_ts, 3),
                      stringsAsFactors = FALSE)
  
  conn <- connect_db(db_name = db_name, verbose = verbose)
  if(verbose) message("INFO CREATE TABLES")
  is_user_info_created <- send_query(conn = conn, query = qry_create_user_info, verbose = verbose)
  is_user_log_created <- send_query(conn = conn, query = qry_create_user_log, verbose = verbose)
  is_app_key_created <- send_query(conn = conn, query = qry_create_app_key, verbose = verbose)
  
  if(verbose) message("INFO ADD USERS")
  if(is_user_log_created$is_success) {
    is_users_inserted <- lapply(1:nrow(users), function(x) {
      qry_insert_user <- set_insert_qry(tbl = "user_info", rec = as.list(users[x,]))
      send_query(conn = conn, query = qry_insert_user, verbose = verbose)
    })
  }
  
  if(verbose) message("INFO ADD APP KEY")
  if(is_app_key_created$is_success) {
    qry_insert_app_key <- set_insert_qry(tbl = "app_key", rec = list(name = hashpw("application-key"), app_name = app_name, added_ts = added_ts))
    send_query(conn = conn, query = qry_insert_app_key, verbose = verbose)
  }
  
  user_info <- get_query(conn = conn, query = "SELECT * FROM user_info", verbose = verbose)
  app_key <- get_query(conn = conn, query = "SELECT * FROM app_key", verbose = verbose)
  if(verbose) {
    message("INFO SELECT * FROM user_info")
    print(user_info)$rs
    message("INFO SELECT * FROM app_key")
    print(app_key)$rs
  }
  
  dbDisconnect(conn)
  
  1
}

# initialize
# init_users(TRUE)

# check db records
# user_info <- get_query(conn = NULL, db_name = "db.sqlite", query = "SELECT * FROM user_info", to_disconnect = TRUE)
# user_log <- get_query(conn = NULL, db_name = "db.sqlite", query = "SELECT * FROM user_log", to_disconnect = TRUE)
# app_key <- get_query(conn = NULL, db_name = "db.sqlite", query = "SELECT * FROM app_key", to_disconnect = TRUE)

# check login credentials
# check_login_credentials(username = "admin", password = "admin", app_name = "any value")
# check_login_credentials(username = "admin", password = "admin_11", app_name = "any value")
# 
# check_login_credentials(username = "john.doe", password = "john.doe", app_name = "multipage demo")
# check_login_credentials(username = "john.doe", password = "john.doe", app_name = "any value")

# registration
# check_registration_info(username = "john.doe", password = NULL, app_key = "application-key", app_name = "multipage demo", conn = NULL, to_disconnect = TRUE, verbose = TRUE)
# register_user(username = "john.doe", password = "abc", password_re = "bcd", app_key = "application-key", app_name = "multipage demo", conn = NULL, to_disconnect = TRUE, verbose = T)
# register_user(username = "john.d", password = "abc", password_re = "bcd", app_key = "application-key", app_name = "multipage demo", conn = NULL, to_disconnect = TRUE, verbose = T)
# register_user(username = "john.d", password = "abc", password_re = "abc", app_key = "application-key", app_name = "multipage demo", conn = NULL, to_disconnect = TRUE, verbose = T)

# check change password
# pw <- get_query(conn = NULL, db_name = "db.sqlite", query = "SELECT password FROM user_info WHERE name = 'john.doe'", to_disconnect = TRUE)$rs$password
# pw
# checkpw("john.doe", pw)
# change_password(username = "john.doe", password_new = "john", password_new_re = "john", app_name = "multipage demo", conn = NULL, to_disconnect = TRUE, verbose = TRUE)
# pw <- get_query(conn = NULL, db_name = "db.sqlite", query = "SELECT password FROM user_info WHERE name = 'john.doe'", to_disconnect = TRUE)$rs$password
# pw
# checkpw("john", pw)




