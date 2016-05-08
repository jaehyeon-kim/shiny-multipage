## db related functions - connect, send, get record set
connect_db <- function(db_name, verbose = TRUE) {
  conn <- dbConnect(SQLite(), db_name)
  
  conn <- if(dbGetException(conn)$errorNum == 0) conn else NULL
  
  if(verbose) message(paste("INFO connection made to", db_name))
  
  conn
}

send_query <- function(conn = NULL, db_name = NULL, query, to_disconnect = FALSE, verbose = TRUE) {
  conn <- if(is.null(conn)) {
    stopifnot(!is.null(db_name))
    connect_db(db_name = db_name, verbose = verbose)
  } else {
    conn
  }
  
  stopifnot(!is.null(conn))
  if(verbose) message(paste("INFO send query\t", query))
  
  out <- tryCatch({
    dbSendQuery(conn, query)
    
    error_num <- dbGetException(conn)$errorNum
    is_success <- if(error_num == 0) TRUE else FALSE
    message <- if(is_success) NULL else paste("error num -", error_num, "error message -", dbGetException(conn)$errorMsg)
    list(is_success = is_success, message = message)
  }, error = function(err) {
    list(is_success = FALSE, message = err)
  })
  
  if(verbose) message(paste("INFO send query result", out$is_success))
  if(to_disconnect) dbDisconnect(conn)
  
  out
}

get_query <- function(conn = NULL, db_name = NULL, query, to_disconnect = FALSE, verbose = TRUE) {
  conn <- if(is.null(conn)) {
    stopifnot(!is.null(db_name))
    connect_db(db_name = db_name, verbose = verbose)
  } else {
    conn
  }
  
  stopifnot(!is.null(conn))
  if(verbose) message(paste("INFO get query\t", query))
  
  out <- tryCatch({
    rs <- dbGetQuery(conn, query)
    
    error_num <- dbGetException(conn)$errorNum
    is_success <- if(error_num == 0) TRUE else FALSE
    message <- if(is_success) NULL else paste("error num -", error_num, "error message -", dbGetException(conn)$errorMsg)
    list(is_success = is_success, message = message, rs = rs)
  }, error = function(err) {
    list(is_success = FALSE, message = err, rs = NULL)
  })
  
  if(verbose) message(paste("INFO get query result", out$is_success))
  if(to_disconnect) dbDisconnect(conn)
  
  out
}

## query generating functions
set_select_qry <- function(schema = NULL, tbl, what = list("*"), filter_list = list()) {
  if(what[[1]] == '*') {
    what <- '*'
  } else {
    what <- gsub("'", '', paste(what, collapse = ", "))
  }
  
  filter_chunk <- if(length(filter_list) > 0) paste("WHERE", concat_rec(filter_list)) else ""
  
  paste('SELECT', what, 'FROM', paste(c(schema, tbl), collapse = "."), filter_chunk,';')  
}

set_insert_qry <- function(schema = NULL, tbl, rec) {
  paste('INSERT INTO', paste(c(schema, tbl), collapse = "."), '(', concat_rec(rec, outer_collapse = ',', target = 'name'), ')',
        'VALUES', '(', concat_rec(rec, outer_collapse = ',', target = 'value'), ');')
}

set_insert_qry_apply <- function(f, recs, trans = FALSE, ...) {
  #rec <- list(a = 1, b = 2)
  #recs <- rbind(rec1 = rec, rec2 = rec)
  #qry_insert(tbl = "user_info", rec = rec)
  #qry_insert_apply(qry_insert, recs = recs, trans = TRUE, tbl = "user_info")
  if(nrow(recs) > 0) {
    lst <- lapply(1:nrow(recs), function(x) {
      rec <- lapply(as.list(recs[x,]), as.character)
      f(..., rec = rec)
    })
    qry <- paste(unlist(lst), collapse = ' ')    
  } else {
    qry <- ''
  }
  
  if(trans) paste('BEGIN TRANSACTION;', qry, 'END TRANSACTION;') else qry
}

concat_rec <- function(rec, inner_collapse = "=", outer_collapse = "AND", target = "both") {
  #rec <- list(a = 1, b = 2)
  #concat_rec(rec, inner_collapse = "=", outer_collapse = "AND", target = "both")
  #concat_rec(rec, outer_collapse = ",", target = "name")
  cover_char <- function(value, what = ",") {
    paste0(what, value, what)
  }
  
  rec_expr <- lapply(names(rec), function(name) {
    value <- cover_char(rec[[name]], "'")
    if(target == "both") {
      expr <- paste(c(name, value), collapse = cover_char(inner_collapse, " "))
    } else if(target == "name") {
      expr <- name
    } else {
      expr <- value
    }
  })
  
  outer_collapse <- if(is.null(outer_collapse)) "," else outer_collapse
  paste(rec_expr, collapse = cover_char(outer_collapse, " "))
}
