## application related functions - check credentials, logging...
check_login_credentials <- function(username, password, app_name, verbose = FALSE) {
  qry_select_db_password <- if(username == "admin") {
    set_select_qry(tbl = "user_info", what = list("password"), filter_list = list(name = username))
  } else {
    set_select_qry(tbl = "user_info", what = list("password"), filter_list = list(name = username, app_name = app_name))
  }
  db_password <- get_query(db_name = "db.sqlite", query = qry_select_db_password, to_disconnect = TRUE, verbose = verbose)$rs$password
  if(length(db_password) > 0) {
    checkpw(password, db_password)
  } else {
    FALSE
  }
}

log_session <- function(username, is_in, app_name, added_ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), verbose = FALSE) {
  qry_insert_log <- set_insert_qry(tbl = "user_log", rec = list(name = username, app_name = app_name, is_in = is_in, added_ts = added_ts))
  send_query(conn = NULL, db_name = "db.sqlite", query = qry_insert_log, verbose = verbose)
}
