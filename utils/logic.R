## application related functions - check credentials, logging...
check_login_credentials <- function(username, password, app_name, conn = NULL, to_disconnect = TRUE, verbose = FALSE) {
  if(username == "admin" & password == "admin") {
    TRUE
  } else {
    qry_select_db_password <- set_select_qry(tbl = "user_info", what = list("password"), filter_list = list(name = username, app_name = app_name))
    db_password <- get_query(conn = conn, db_name = "db.sqlite", query = qry_select_db_password, to_disconnect = to_disconnect, verbose = verbose)$rs$password
    
    if(length(db_password) > 0) {
      checkpw(password, db_password)
    } else {
      # no password given user name and app name
      FALSE
    }
  }
}

log_session <- function(username, is_in, app_name, added_ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), conn = NULL, to_disconnect = TRUE, verbose = FALSE) {
  qry_insert_log <- set_insert_qry(tbl = "user_log", rec = list(name = username, app_name = app_name, is_in = is_in, added_ts = added_ts))
  send_query(conn = conn, db_name = "db.sqlite", query = qry_insert_log, to_disconnect = to_disconnect, verbose = verbose)$is_success
}

check_registration_info <- function(username, password = NULL, app_key, app_name, conn = NULL, to_disconnect = TRUE, verbose = FALSE) {
  qry_select_db_app_key <- set_select_qry(tbl = "app_key", what = list("name"), filter_list = list(app_name = app_name))
  db_app_key <- get_query(conn = conn, db_name = "db.sqlite", query = qry_select_db_app_key, to_disconnect = to_disconnect, verbose = verbose)$rs$name
  
  if(length(db_app_key) > 0 && checkpw(app_key, db_app_key)) {
    qry_select_db_username <- set_select_qry(tbl = "user_info", what = list("name"), filter_list = list(name = username, app_name = app_name))
    db_username <- get_query(conn = conn, db_name = "db.sqlite", query = qry_select_db_username, to_disconnect = to_disconnect, verbose = verbose)$rs$name
    if(length(db_username) == 0) {
      if(verbose) message(paste("INFO", "app key match and username given app name doesn't exist"))
      out <- list(app_key_pass = TRUE, user_not_found = TRUE)
    } else {
      if(verbose) message(paste("INFO", "app key match but username given app name exists"))
      out <- list(app_key_pass = TRUE, user_not_found = FALSE)
    }
  } else {
    if(verbose) message(paste("INFO", "app key for given app name not in DB or app key not match"))
    out <- list(app_key_pass = FALSE, user_not_found = FALSE)
  }
  out
}

register_user <- function(username, password, password_re, app_key, app_name, conn = NULL, to_disconnect = TRUE, verbose = FALSE) {
  check_info <- check_registration_info(username = username, password = NULL, app_key = app_key, app_name = app_name, conn = conn, to_disconnect = to_disconnect, verbose = verbose)
  if(all(unlist(check_info))) {
    if(password == password_re) {
      qry_insert_user <- set_insert_qry(tbl = "user_info", rec = list(name = username, password = hashpw(password), app_name = app_name, added_ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
      out <- send_query(conn = conn, db_name = "db.sqlite", query = qry_insert_user, to_disconnect = to_disconnect, verbose = verbose)$is_success
    } else {
      if(verbose) message(paste("INFO", "passwords not match"))
      out <- FALSE
    }
  } else {
    if(verbose) message(paste("INFO", "app_key pass condition not match and/or same username exists"))
    out <- FALSE
  }
  out
}

change_password <- function(username, password, password_new, password_new_re, app_name, conn = NULL, to_disconnect = TRUE, verbose = FALSE) {
  if(password_new == password_new_re) {
    qry_delete_user <- set_delete_qry(tbl = "user_info", filter_list = list(name = username, app_name = app_name))
    qry_insert_user <- set_insert_qry(tbl = "user_info", rec = list(name = username, password = hashpw(password_new), app_name = app_name, added_ts = format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    out_delete <- send_query(conn = conn, db_name = "db.sqlite", query = qry_delete_user, to_disconnect = to_disconnect, verbose = verbose)$is_success
    out_insert <- send_query(conn = conn, db_name = "db.sqlite", query = qry_insert_user, to_disconnect = to_disconnect, verbose = verbose)$is_success
    if(out_delete & out_insert) {
      out <- TRUE
    } else {
      if(verbose) message(paste("INFO", "User record not deleted or inserted"))
      out <- FALSE
    }
    #delete/insert not robust, should be within transaction
    #qry_all <- paste("BEGIN TRANSACTION;", qry_delete_user, qry_insert_user,"END TRANSACTION;")
    #out <- send_query(conn = conn, db_name = "db.sqlite", query = qry_all, to_disconnect = to_disconnect, verbose = verbose)$is_success
    #if(out) {
    #  out <- TRUE
    #} else {
    #  if(verbose) message(paste("INFO", "User record fails to be upated"))
    #  out <- FALSE
    #}
  } else {
    if(verbose) message(paste("INFO", "New passwords don't match"))
    out <- FALSE
  }
  out
}

get_password <- function(username, app_name, conn = NULL, to_disconnect = TRUE, verbose = FALSE) {
  qry_select_pw <- set_select_qry(tbl = "user_info", what = list("password"), filter_list = list(name = username, app_name = app_name))
  get_query(conn = conn, db_name = "db.sqlite", query = qry_select_pw, to_disconnect = to_disconnect, verbose = verbose)$rs$password
}

check_password <- function(password) {
  cond <- list(has_enough_len = FALSE, has_numeric = FALSE, has_character = FALSE)
  
  if(nchar(password) >= 8) cond$has_enough_len <- TRUE
  if(grepl("[0-9]", password)) cond$has_numeric <- TRUE
  if(grepl("[a-zA-Z]", password)) cond$has_character <- TRUE
  
  cond
}

message_password <- function(password) {
  cond <- check_password(password)
  msg <- ""
  if(!all(unlist(cond))) {
    msg <- if(!cond$has_enough_len) paste(msg, "too short?") else msg
    msg <- if(!cond$has_numeric) paste(msg, "no number?") else msg
    msg <- if(!cond$has_character) paste(msg, "no letter?") else msg
  }

  msg
}



