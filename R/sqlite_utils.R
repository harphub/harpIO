### DEFINE USEFUL FUNCTIONS FOR SQLite
### dbwrite, dbquery, dbclear

### a more or less robust & fast data-base connection (?)
### In fact, really robust is impossible!
### 2 sessions of RSQLite on the same computer
### are not well isolated (not thread safe)
### so e.g. dbGetException doesn't give the correct messages???
### dbWriteTable and dbReadTable should be avoided!!!

### little convenience functions:

# Note - these fuctions are not exported. Do they need to be? Also need some DBI namespacing.

dbopen <- function(dbfile, lock) {
  require(DBI)
  if (packageVersion("DBI") < "0.5") stop("Unfortunately, HARP will only
function correctly with package DBI version 0.5 or higher.")
  if (missing(lock)) {
    if (exists(".SQLiteLocking")) lock <- .SQLiteLocking
    else lock <- TRUE
  }
  if (lock)  dbConnect(RSQLite::SQLite(), dbname=dbfile)
  else {
    dbConnect(RSQLite::SQLite(), dbname=dbfile, vfs="unix-none")  # for Lustre, NFS
  }
### WARNING: this turns off file locking completely!
}

dbclear <- function(db) {
# in recent RSQLite, some strangeness may happen!
# dbListResults is deprecated, so we can't do anything here!

#  if (packageVersion('RSQLite')<'1.0.0') {for(i in dbListResults(db)) dbClearResult(i)}
#  else for (i in dbListResults(db)) {if (dbIsValid(i)) dbClearResult(i)}
  cat("warning: you shoudln't call dbclear anymore.")
}

dbclose <- function(db) {
#  bclear(db)
  invisible(dbDisconnect(db))
}

### add a new column to an existing table
db.add.columns <- function(db, table, colnames, quiet=FALSE){
  for (col in colnames){
    if (!quiet) cat("ADDING NEW COLUMN",col,"TO TABLE",table,"\n")
    SQLadd <- paste("ALTER TABLE",table,"ADD",col,"REAL DEFAULT NULL")
    dbquery(db, SQLadd)
  }
}

####################################################################
### Robust dbwrite: write a full data.frame to an existing table ###
####################################################################

dbwrite <- function(conn, table, mydata, rounding=NULL, maxtry=20, sleep=5){
  tnames <- dbListFields(conn, table)
  if (length(setdiff(tolower(names(mydata)), tolower(tnames))) > 0) {
    cat("ERROR: The new data contains fields that do not exist in the data base table!\n",
        "Consider re-creating SQLite file.\n")
    stop("Can not write data.")
  }

  SQL <- paste0("REPLACE into ",table," (",paste(names(mydata),collapse=","),") ",
                " values ",
                "(:", paste(names(mydata),collapse=",:"),")")

  if (!is.null(rounding)) {
  # notice that we include the "," in the substitution string to avoid partial fit
    for (f in intersect(names(rounding), names(mydata))) {
      sub(paste0(":",f,",") ,sprintf("round(:%s, %i),",f, rounding[[f]]), SQL)
    }
  }

  if (packageVersion('DBI')<'0.3.0') {
    dbBegin <- dbBeginTransaction
  } else if (packageVersion("RSQLite") < "1.0.0") {
    stop("RSQLite version is inconsistent with DBI. Consider upgrading.")
  }
  dbBegin(conn)  ### this is OK: doesn't require a lock

  prepOK <- FALSE
  count <- 1
  while (!prepOK & count<=maxtry){
    tryOK1 <- tryCatch(dbSendQuery(conn, SQL, params=mydata),
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
    dbRollback(conn)
    stop("FATAL DBFAILURE: Unable to acquire lock.")
  }

### second stage (commit) will fail if another process is accessing the db (even just for reading)
  commitOK <- FALSE
  count <- 1
  while (!commitOK & count<=maxtry){
   dbClearResult(tryOK1)
    tryOK2 <- tryCatch(dbCommit(conn),
                      error=function(e) {print(e);return(e)})  ### commit needs an EXCLUSIVE lock
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
    dbRollback(conn)
    dbClearResult(tryOK1)
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
    result <- tryCatch(dbSendQuery(conn, sql),
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
  lCompleted <- dbGetInfo(result)$has.completed
  if (is.null(lCompleted)) lCompleted <- dbGetInfo(result)$completed
  if (is.null(lCompleted)) {
     cat("Problem with dbGetInfo result\n")
     print(dbGetInfo(result))
     stop("ABORTING!")
  }
  if (lCompleted) {
    # do I have to clear the result even if it is completed? YES!
    dbClearResult(result)
    return(TRUE)
  }

# not yet completed: so we are expecting a return result
  fetchOK <- FALSE
  count <- 1
   while (!fetchOK & count<=maxtry){
    if (packageVersion('DBI')<'0.3.0') dbFetch <- fetch
    data <- tryCatch(dbFetch(result,n=-1),
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
  dbClearResult(result)
  if (!fetchOK) stop("FATAL DBFAILURE: Unable to fetch from database.")
  return(data)
}

#############################

create_table <- function(db, tab) {
  if (dbExistsTable(db, tab$name)) {
## TODO: check fields are the same!!!

    return(NULL)
  }
  if ( is.null(tab$primary) || is.na(tab$primary) || tab$primary=="") {
    sql.create <- sprintf("CREATE TABLE %s ( %s )",
                           tab$name,
                           paste(tab$fields, tab$types,collapse=","))
  } else {
    sql.create <- sprintf("CREATE TABLE %s ( %s , PRIMARY KEY(%s))",
                           tab$name,
                           paste(tab$fields, tab$types,collapse=","),
                           paste(tab$primary,collapse=","))
  }
  dbquery(db, sql.create)
  invisible(sql.create)
}

cleanup_table <- function(db, tabname, where.list) {
  # character values must be wrapped with single quotes in the SQL command!
  where.list <- lapply(where.list, function(x) if (is.character(x)) paste0("'",x,"'") else x)
  wlist <- vapply(names(where.list), FUN.VAL="a",
                  FUN=function(cc) sprintf("%s=%s",cc,where.list[[cc]]))
  sql.cleanup <- sprintf("DELETE FROM %s WHERE %s", tabname, paste(wlist, collapse=" AND "))
  dbquery(db, sql.cleanup)
}


