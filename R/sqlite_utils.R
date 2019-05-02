### DEFINE USEFUL FUNCTIONS FOR SQLite
### dbwrite, dbquery, dbclear

### a more or less robust & fast data-base connection (?)
### In fact, really robust is impossible!
### 2 sessions of RSQLite on the same computer
### are not well isolated (not thread safe)
### so e.g. dbGetException doesn't give the correct messages???
### dbWriteTable and dbReadTable should be avoided!!!

### little convenience functions:

# Note - these fuctions are not exported. #AD: WHY NOT?

#' @param dbfile Name of an SQLite file. If it does not exist, it is created.
#' @param lock If TRUE, file locking is used.
#'   Set to FALSE on network file systems like NFS and lustre.
#' @value A data base connection
#' @export
dbopen <- function(dbfile, lock) {
  if (packageVersion("DBI") < "0.5") stop("Unfortunately, HARP will only
function correctly with package DBI version 0.5 or higher.")
  if (missing(lock)) {
    if (exists(".SQLiteLocking")) lock <- .SQLiteLocking
    else lock <- TRUE
  }
  if (lock)  DBI::dbConnect(RSQLite::SQLite(), dbname=dbfile)
  else {
    DBI::dbConnect(RSQLite::SQLite(), dbname=dbfile, vfs="unix-none")  # for Lustre, NFS
  }
### WARNING: this turns off file locking completely!
}

dbclear <- function(db) {
# in recent RSQLite, some strangeness may happen!
# dbListResults is deprecated, so we can't do anything here!

#  if (packageVersion('RSQLite')<'1.0.0') {for(i in dbListResults(db)) dbClearResult(i)}
#  else for (i in dbListResults(db)) {if (dbIsValid(i)) dbClearResult(i)}
  cat("warning: you shouldn't call dbclear anymore.")
}

dbclose <- function(db) {
#  bclear(db)
  invisible(DBI::dbDisconnect(db))
}

### add a new column to an existing table
db.add.columns <- function(db, table, colnames, quiet=FALSE){
  for (col in colnames){
    if (!quiet) cat("ADDING NEW COLUMN",col,"TO TABLE",table,"\n")
    sql_add <- paste("ALTER TABLE",table,"ADD",col,"REAL DEFAULT NULL")
    dbquery(db, sql_add)
  }
}

#######################################################################
### Robust dbwrite: write a full data.frame to an existing table    ###
###                 wait for file locks to avoid data corruption    ###
###                 safe for multiple processes accessing same file ###
#######################################################################

dbwrite <- function(conn, table, mydata, rounding=NULL, maxtry=20, sleep=5, show_query = FALSE){
  tnames <- DBI::dbListFields(conn, table)
  if (length(setdiff(tolower(names(mydata)), tolower(tnames))) > 0) {
    cat("ERROR: The new data contains fields that do not exist in the data base table!\n",
        "Consider re-creating SQLite file.\n")
    stop("Can not write data.")
  }

  sql_write <- paste0("REPLACE into ",table," (",paste(names(mydata),collapse=","),") ",
                " values ",
                "(:", paste(names(mydata),collapse=",:"),")")

  if (!is.null(rounding)) {
  # notice that we include the "," in the substitution string to avoid partial fit
    for (f in intersect(names(rounding), names(mydata))) {
      sub(paste0(":",f,",") ,sprintf("round(:%s, %i),",f, rounding[[f]]), sql_write)
    }
  }

  if (packageVersion('DBI') < '0.3.0') {
    dbBegin <- get("dbBeginTransaction", envir = asNamespace("DBI"))
  } else if (packageVersion("RSQLite") < "1.0.0") {
    stop("RSQLite version is inconsistent with DBI. Consider upgrading.")
  }
  DBI::dbBegin(conn)  ### this is OK: doesn't require a lock

  prepOK <- FALSE
  count <- 1
  if (show_query) message("sending query: ", sql_write)
  while (!prepOK & count<=maxtry){
    tryOK1 <- tryCatch(DBI::dbSendQuery(conn, sql_write, params=mydata),
                      error=function(e) {print(e);return(e)}) ### this needs RESERVED lock
    if (inherits(tryOK1,"error")) {
      print(paste("FAILURE dbSendQuery",count,"/",maxtry))
      print(tryOK1$message)

      Sys.sleep(sleep)
      count <- count + 1
      next
    }
    else {
      prepOK <- TRUE
    }
  }
  if (!prepOK) {
    DBI::dbRollback(conn)
    stop("FATAL DBFAILURE: Unable to acquire lock.")
  }

### second stage (commit) will fail if another process
### is accessing the db (even just for reading)
  commitOK <- FALSE
  count <- 1
  while (!commitOK & count<=maxtry){
   DBI::dbClearResult(tryOK1)
    tryOK2 <- tryCatch(DBI::dbCommit(conn),
                      error=function(e) {print(e);return(e)})
    ### commit needs an EXCLUSIVE lock
    if (inherits(tryOK2,"error")) {
      print(paste("FAILURE commit",count,"/",maxtry))
      print(tryOK2$message)

      Sys.sleep(sleep)
      count <- count + 1
      next
    }
    else {
      commitOK <- TRUE
    }
  }
  if (!commitOK) {
    DBI::dbRollback(conn)
    DBI::dbClearResult(tryOK1)
    stop("FATAL DBFAILURE: Unable to commit.")
  } else {
    return(TRUE)
  }
}

######################################################
### Robust dbread: submit SQL to an existing table ###
######################################################
# dbGetResult combines send and fetch : hard to know where it went wrong... avoid
# it seems that the SendQuery doesn't require a lock, and doesn't impose any either.
# but once you "fetch" part of the result, database has a SHARED lock, so writing is not possible.
# if fetch is locked (other process is writing), dbGetExceptions doesn't give the error!
# so we use a different method!
dbquery <- function(conn, sql, maxtry=20, sleep=5){
  sendOK <- FALSE
  count <- 1
  while (!sendOK & count<=maxtry){
    result <- tryCatch(DBI::dbSendQuery(conn, sql),
                       error=function(e) {print(e); return(e)})
    if (inherits(result,"error")) {
      print(paste("FAILURE dbSendQuery",count,"/",maxtry))
      print(result$message)

      Sys.sleep(sleep)
      count <- count + 1
      next
    }
    else {
      sendOK <- TRUE
    }
  }
  if (!sendOK) stop("FATAL DBFAILURE: Unable to query database.")

#-- if the sql statement doesn't return data (not a select), there is nothing more to do:
# Note $completed has changed to $has.completed in newer versions of DBI
  lCompleted <- DBI::dbGetInfo(result)$has.completed
  if (is.null(lCompleted)) lCompleted <- DBI::dbGetInfo(result)$completed
  if (is.null(lCompleted)) {
     cat("Problem with dbGetInfo result\n")
     print(DBI::dbGetInfo(result))
     stop("ABORTING!")
  }
  if (lCompleted) {
    # do I have to clear the result even if it is completed? YES!
    DBI::dbClearResult(result)
    return(TRUE)
  }

# not yet completed: so we are expecting a return result
  fetchOK <- FALSE
  count <- 1
   while (!fetchOK & count<=maxtry){
    if (packageVersion('DBI')<'0.3.0') dbFetch <- get("fetch", envir = asNamespace("DBI"))
    data <- tryCatch(DBI::dbFetch(result,n=-1),
                     error=function(e) {print(e);return(e)})
    if (inherits(data,"error")) {
      print(paste("FAILURE dbFetch",count,"/",maxtry))
      print(data$message)

      Sys.sleep(sleep)
      count <- count + 1
      next
    }
    else {
      fetchOK <- TRUE
    }
  }
  # do I have to clear the result after a fetch? YES!
  DBI::dbClearResult(result)
  if (!fetchOK) stop("FATAL DBFAILURE: Unable to fetch from database.")
  return(data)
}

#############################
#' Create a new table in an SQLite data base
#'
#' @param db A database connection
#' @param name A name for the table. If it already exists, nothing happens.
#' @param a data.frame. Only column names and type are used.
#' @param primary Primary keys
#' @export
create_table <- function(db, name, data, primary=NULL, show_query = FALSE) {
  if (DBI::dbExistsTable(db, name)) {
## TODO: check fields are the same!!!
    return(NULL)
  }
  types <- vapply(seq_len(dim(data)[2]), function(x) switch(class(data[[x]]),
                                             "integer"="INTEGER",
                                             "numeric"="REAL",
                                             "character"="CHARACTER"), FUN.VAL="a")
  if ( is.null(primary) ) {
    sql_create <- sprintf("CREATE TABLE %s ( %s )",
                           name,
                           paste(names(data), types, collapse=","))
  } else {
    sql_create <- sprintf("CREATE TABLE %s ( %s , PRIMARY KEY(%s))",
                           name,
                           paste(names(data), types, collapse=","),
                           paste(primary, collapse=","))
  }
  if (show_query) message("Creation query: ", sql_create)
  dbquery(db, sql_create)
  invisible(sql_create)
}

cleanup_table <- function(db, tabname, where.list, show_query = FALSE) {
  # character values must be wrapped with single quotes in the SQL command!
  where.list <- lapply(where.list,
                       function(x) if (is.character(x)) paste0("'",x,"'")
                                   else x)
  wlist <- vapply(names(where.list), FUN.VAL="a",
                  FUN=function(cc) sprintf("%s=%s",cc,where.list[[cc]]))
  sql_cleanup <- sprintf("DELETE FROM %s WHERE %s",
                         tabname, paste(wlist, collapse=" AND "))
  if (show_query) message("Cleanup query: ", sql_cleanup)
  dbquery(db, sql_cleanup)
  invisible(sql_cleanup)
}


