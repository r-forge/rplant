# Copyright (c) 2012, University of Tennessee
# rPlant directly interacts with iplant's command-line API for the 
# Discovery Environment (DE)

# -- AUTHENTICATION FUNCTIONS -- #

utils::globalVariables(c("rplant.foundation.env"))
utils::globalVariables(c("rplant.agave.env"))
utils::globalVariables(c("rplant.env"))

#################
#################
### Validate ####
#################
#################

################# Maybe add time AND number of uses to the Validate function #################
Validate <- function(user, pwd, consumer_key=NULL, consumer_secret=NULL, api="foundation", print.curl=FALSE) {
  
  api <- match.arg(api, c("agave", "foundation"))

  if (api == "foundation"){

    web_BASE <- "https://foundation.iplantcollaborative.org/"
    curl.call <- getCurlHandle(userpwd=paste(user, pwd, sep=":"), httpauth=1L, ssl.verifypeer=FALSE)
    res <- fromJSON(getURL(paste(web_BASE,"auth-v1/",sep=""), curl=curl.call)) 

    if (res$status == "success"){
      assign("rplant.foundation.env", new.env(hash = TRUE), envir = .GlobalEnv)
      assign("rplant.env", new.env(hash = TRUE), envir = .GlobalEnv)
      assign("api", "f", envir=rplant.env)
      assign("user", user, envir=rplant.env)
      assign("webio", paste(web_BASE, "io-v1/", sep=""), envir=rplant.foundation.env)
      assign("webapps", paste(web_BASE, "apps-v1/", sep=""), envir=rplant.foundation.env)
      assign("webauth", paste(web_BASE, "auth-v1/", sep=""), envir=rplant.foundation.env)
      assign("webprofiles", paste(web_BASE, "profile-v1/profile/", sep=""), envir=rplant.foundation.env)
      assign("user", user, envir=rplant.foundation.env)
      assign("pwd", pwd, envir=rplant.foundation.env) 
      assign("curl.call", getCurlHandle(userpwd=paste(get("user", envir=rplant.foundation.env), get("pwd", envir=rplant.foundation.env), sep=":"), httpauth=1L, ssl.verifypeer=FALSE), envir=rplant.foundation.env)
      curl.string <- paste("curl -sku '", rplant.foundation.env$user, "' ", rplant.foundation.env$webauth, sep="")

    } else {
      return(res$message)
    }
  } else {

    web_BASE <- "https://agave.iplantc.org/"
    content <- c()
    content[1] <- "grant_type=client_credentials"
    content[2] <- "scope=PRODUCTION"
    content[3] <- paste("username=", user, sep="")
    content[4] <- paste("password=", pwd, sep="")
    string <- paste(content, collapse = "&")

    val <- charToRaw(string)

    web <- paste(web_BASE, "token", sep="")

    curl.string <- paste("curl -sku '", consumer_key, ":", consumer_secret,"' -X POST -d '", string, "' ", web, sep="")

    curl.call <- getCurlHandle(userpwd=paste(consumer_key, consumer_secret, sep=":"), httpauth=1L, ssl.verifypeer=FALSE)
    expire <- as.POSIXlt(format(Sys.time(),"%Y-%m-%d %k:%M:%OS"))
    expire$hour=expire$hour+4
    tryCatch(res <- fromJSON(getURLContent(web, curl=curl.call, infilesize=length(val), readfunction=val, upload=TRUE, customrequest="POST")), error=function(x){return(res <- data.frame(status=paste(x)))})

    if (length(res) == 4){
      assign("rplant.agave.env", new.env(hash = TRUE), envir = .GlobalEnv)
      assign("rplant.env", new.env(hash = TRUE), envir = .GlobalEnv)
      assign("api", "a", envir=rplant.env)
      assign("user", user, envir=rplant.env)
      assign("consumer_key", consumer_key, envir=rplant.agave.env)
      assign("consumer_secret", consumer_secret, envir=rplant.agave.env)
      assign("webio", paste(web_BASE, "files/2.0/", sep=""), envir=rplant.agave.env)
      assign("webapps", paste(web_BASE, "apps/2.0/", sep=""), envir=rplant.agave.env)
      assign("webjobs", paste(web_BASE, "jobs/2.0/", sep=""), envir=rplant.agave.env)
      assign("webauth", paste(web_BASE, "token", sep=""), envir=rplant.agave.env)
      assign("webprofiles", paste(web_BASE, "profiles/2.0/", sep=""), envir=rplant.agave.env)
      assign("webtransforms", paste(web_BASE, "transforms/2.0/", sep=""), envir=rplant.agave.env)
      assign("user", user, envir=rplant.agave.env)
      assign("pwd", pwd, envir=rplant.agave.env) 
      assign("expire", expire, envir=rplant.agave.env) 
      assign("access_token", res$access_token, envir=rplant.agave.env)
      assign("refresh_token", res$refresh_token, envir=rplant.agave.env)
      assign("curl.call", getCurlHandle(httpheader=c(paste("Authorization: Bearer ", get("access_token", envir=rplant.agave.env), sep="")), httpauth=1L, ssl.verifypeer=FALSE), envir=rplant.agave.env)
    } else {
      sub <- substring(res$status,1,5)
      if (sub == "Error"){
        return("Error: Shared username is not valid")
      } else if (sub == "error"){
        return(res$message)
      } else {
        return(paste(res$status))
      }
    }
  }

  if (print.curl){
    print(curl.string)
  }
}

#################
#################
## RenewToken ###
#################
#################

RenewToken <- function(print.curl=FALSE) {

  content <- c()
  content[1] <- "grant_type=refresh_token"
  content[2] <- "scope=PRODUCTION"
  content[3] <- paste("refresh_token=", rplant.agave.env$refresh_token, sep="")
  string <- paste(content, collapse = "&")
  web <- rplant.agave.env$webauth
  val <- charToRaw(string)

  curl.call <- getCurlHandle(userpwd=paste(consumer_key, consumer_secret, sep=":"), httpauth=1L, ssl.verifypeer=FALSE)

  tryCatch(res <- fromJSON(getURLContent(web, curl=curl.call, infilesize=length(val), readfunction=val, upload=TRUE, customrequest="POST")), error=function(x){return(res <- data.frame(status=paste(x)))})

  if (length(res) == 4){
    assign("access_token", res$access_token, envir=rplant.agave.env)
    assign("refresh_token", res$refresh_token, envir=rplant.agave.env) 
  } else {
    sub <- substring(res$status,1,5)
    if (sub == "Error"){
      return("Error: Shared username is not valid")
    } else if (sub == "error"){
      return(res$message)
    } else {
      return(paste(res$status))
    }
  }

  if (print.curl){
    curl.string <- paste("curl -sku ", rplant.agave.env$consumer_key, ":", rplant.agave.env$consumer_secret," -X POST -d '", string, "' ", web, sep="")
    print(curl.string)
  }
}

#################
#################
##### Misc. #####
#################
#################

Renew <- function(ret=FALSE){
  if (rplant.env$api == "a") {
    assign("curl.call", getCurlHandle(httpheader=c(paste("Authorization: Bearer ", get("access_token", envir=rplant.agave.env), sep="")), httpauth=1L, ssl.verifypeer=FALSE), envir=rplant.agave.env)
  } else {
    assign("curl.call", getCurlHandle(userpwd=paste(get("user", envir=rplant.foundation.env), get("pwd", envir=rplant.foundation.env), sep=":"), httpauth=1L, ssl.verifypeer=FALSE), envir=rplant.foundation.env)
  }
  if (ret == TRUE){
    if (rplant.env$api == "a") {
      return(paste("Authorization: Bearer ", get("access_token", envir=rplant.agave.env), sep=""))
    } else {
      return(paste(get("user", envir=rplant.foundation.env), get("pwd", envir=rplant.foundation.env), sep=":"))
    }
  }
}

Time <- function(){
  if (rplant.env$api != "f"){
    compare <- as.POSIXlt(format(Sys.time(),"%Y-%m-%d %k:%M:%OS"))
    if (compare > rplant.agave.env$expire){
      expire <- as.POSIXlt(format(Sys.time(),"%Y-%m-%d %k:%M:%OS"))
      expire$hour=expire$hour+4
      assign("expire", expire, envir=rplant.agave.env)
      RenewToken()
    }
  }
}

TestApp <- function(APP){

  Renew()
  if (rplant.env$api == "f"){
    web <- paste(rplant.foundation.env$webapps, "apps/name/", sep="")
    curl.call <- rplant.foundation.env$curl.call
    first_string <- "res$result[[len]]"
  } else {
    web <- rplant.agave.env$webapps
    curl.call <- rplant.agave.env$curl.call
    first_string <- "res$result"
  }

  if (substring(APP,nchar(APP)-1,nchar(APP)-1) == "u"){
    priv.APP <- substring(APP,1,nchar(APP)-2)
  } else if (substring(APP,nchar(APP)-2,nchar(APP)-2) == "u"){
    priv.APP <- substring(APP,1,nchar(APP)-3)
  } else {
    priv.APP <- APP
  }

  res <- fromJSON(getForm(paste(web, priv.APP, sep=""), .checkparams=FALSE, curl=curl.call))
  len <- length(res$result)

  if (length(res) == 0){
    return(list(NULL))
  } else {
    shortd <- eval(parse(text=paste(first_string, "$shortDescription", sep="")))
    shortn <- nchar(shortd)
    longd <- eval(parse(text=paste(first_string, "$lomgDescription", sep="")))
    if (is.null(longd)) {longn = 0} else {longn <- nchar(longd)}
    if (longn >= shortn) {
      description <- longd
    } else {
      description <- shortd
    }
    return(c(eval(parse(text=paste(first_string, "$id", sep=""))), description))
  }
}

Error <- function(ERR){
  sub1 <- substring(ERR,8,8)
  if (sub1 == "B"){
    return("Error: Bad Request")
  } else if (sub1 == "U"){
    return("Error: Invalid username/password combination")
  } else if ((sub1 == "F") || (sub1 == "N")){
    return("Error: file (or directory, or job id) does not exist")
  } else {
    return(ERR)
  }
}

Wait <- function(job.id, minWaitsec, maxWaitsec, print=FALSE){
  currentStatus= ''
  currentWait = minWaitsec
  while (( currentStatus != 'FAILED' ) && (currentStatus != 'ARCHIVING_FINISHED')) {
    # cache the status from previous inquiry
    oldStatus = currentStatus
    currentStatus = CheckJobStatus( job.id )

    if (currentStatus == oldStatus) {# status hasn't changed from last time we asked so
      currentWait = currentWait * 1.10 # wait 10% longer to poll in the future

      if (currentWait > maxWaitsec) {
        currentWait = maxWaitsec # but don't wait too long
      }
    } else {
      currentWait = minWaitsec # status changed so reset wait counter to min value
    }
    
#   if (print == TRUE) {
#     print(paste("Wait Time:", currentWait, "Status:", currentStatus))
#   }
    
    Sys.sleep(currentWait) # sit idle for proscribed time. If you are using an event-based programming model, you could just schedule the next check currentWait sec in the future 
  }

  if (print == TRUE) {
    print(paste("Job number: '", job.id, "' has status: ", currentStatus, sep=""))
  }
}

# -- END -- #




# -- FILE FUNCTIONS -- #

#################
#################
## UploadFile ###
#################
#################

UploadFile <- function(local.file.name, local.file.path="", filetype=NULL,
                       print.curl=FALSE, suppress.Warnings=FALSE) {

  if (local.file.path == ""){
    file.path = paste(getwd(), local.file.name, sep="/")
  } else {
    file.path = paste(local.file.path, local.file.name, sep="/")
  }

  Time()
  Renew()
  if (rplant.env$api == "f") {
    web <- paste(rplant.foundation.env$webio, "io/", rplant.foundation.env$user, sep="")
    web_check <- paste(rplant.foundation.env$webio, "io/list/", rplant.foundation.env$user, "/", local.file.name, sep="")
    first_string <- paste("curl -sku '", rplant.foundation.env$user, "'", sep="")
    curl.call <- rplant.foundation.env$curl.call
    options <- list(userpwd=paste(rplant.foundation.env$user, rplant.foundation.env$pwd, sep=":"), ssl.verifypeer=FALSE, httpauth=AUTH_BASIC, useragent="R", followlocation=TRUE)
  } else {
    web <- paste(rplant.agave.env$webio, "media/", rplant.agave.env$user, sep="")
    web_check <- paste(rplant.agave.env$webio, "listings/", rplant.agave.env$user, "/", local.file.name, sep="")
    first_string <- paste("curl -sk -H '", Renew(TRUE), "'", sep="")
    curl.call <- rplant.agave.env$curl.call
    options <- list(httpheader=c(paste("Authorization: Bearer ", rplant.agave.env$access_token, sep="")), ssl.verifypeer=FALSE, httpauth=AUTH_BASIC, useragent="R", followlocation=TRUE)
  }

  if (suppress.Warnings == FALSE){
    
    file.exist <- fromJSON(getURL(web_check, curl=curl.call)) 
    if (length(file.exist$result) != 0){
      return(paste("Error: file `", local.file.name, "' already exists in iPlant directory", sep=""))
    }
  }

  if (!is.null(filetype)){
    tryCatch(res <- fromJSON(postForm(web, style="httppost", fileToUpload=fileUpload(file.path), fileType=filetype, .opts=options)), error=function(x){return(res <- data.frame(status=paste(x)))})
    curl.string <- paste(first_string," -F 'fileToUpload=@", file.path, "' -F 'fileType=", filetype, "' ", web, sep="")
  } else {
    tryCatch(res <- fromJSON(postForm(web, style="httppost", fileToUpload=fileUpload(file.path), .opts=options)), error=function(x){return(res <- data.frame(status=paste(x)))})
    curl.string <- paste(first_string," -F 'fileToUpload=@", file.path, "' ", web, sep="")
  }

  if (print.curl==TRUE){
    print(curl.string)
  }

  if (res$status != "success") {
    sub <- substring(res$status,1,5)
    if (sub == "Error"){
      return(Error(paste(res$status)))
    } else if (sub == "error"){
      return(res$message)
    } else {
      return(paste(res$status))
    }
  }
}

#################
#################
### ShareFile ###
#################
#################

ShareFile <- function(file.name, file.path="", shared.username, read=TRUE, execute=TRUE, write=TRUE, print.curl=FALSE, suppress.Warnings=FALSE) {

  Time()
  Renew()
  content <- c()
  if (rplant.env$api == "f") {
    web <- paste(rplant.foundation.env$webio, "io/share/", rplant.foundation.env$user, "/", sep="")
    web_check <- paste(rplant.foundation.env$webio, "io/list/", rplant.foundation.env$user, "/", sep="")
    first_string <- paste("curl -sku '", rplant.foundation.env$user, "'", sep="")
    curl.call <- rplant.foundation.env$curl.call

    if ((read == TRUE) && (execute == TRUE) && (write == TRUE)) {
      content[1] <- "can_execute=true"
      content[2] <- "can_read=true"
      content[3] <- "can_write=true"
      content[4] <- paste("username=", shared.username, sep="")
    } else if ((read == TRUE) && (execute == TRUE) && (write == FALSE)) {
      content[1] <- "can_execute=true"
      content[2] <- "can_read=true"
      content[3] <- paste("username=", shared.username, sep="")
    } else if ((read == TRUE) && (execute == FALSE) && (write == TRUE)) {
      content[1] <- "can_read=true"
      content[2] <- "can_write=true"
      content[3] <- paste("username=", shared.username, sep="")
    } else if ((read == TRUE) && (execute == FALSE) && (write == FALSE)) {
      content[1] <- "can_read=true"
      content[2] <- paste("username=", shared.username, sep="")
    } else if ((read == FALSE) && (execute == TRUE) && (write == TRUE)) {
      content[1] <- "can_execute=true"
      content[2] <- "can_write=true"
      content[3] <- paste("username=", shared.username, sep="")
    } else if ((read == FALSE) && (execute == TRUE) && (write == FALSE)) {
      content[1] <- "can_execute=true"
      content[2] <- paste("username=", shared.username, sep="")
    } else if ((read == FALSE) && (execute == FALSE) && (write == TRUE)) {
      content[1] <- "can_write=true"
      content[2] <- paste("username=", shared.username, sep="")
    } else {
      return("Error: Must select some permissions")
    }
  } else {
    web <- paste(rplant.agave.env$webio,"pems/",rplant.agave.env$user,"/", sep="")
    web_check <- paste(rplant.agave.env$webio, "listings/", rplant.agave.env$user, "/", sep="")
    first_string <- paste("curl -sk -H '", Renew(TRUE), "'", sep="")
    curl.call <- rplant.agave.env$curl.call

    if ((read == TRUE) && (execute == TRUE) && (write == TRUE)) {
      content[1] <- "permission=all"
      content[2] <- paste("username=", shared.username, sep="")
    } else if ((read == TRUE) && (execute == TRUE) && (write == FALSE)) {
      content[1] <- "permission=read_execute"
      content[2] <- paste("username=", shared.username, sep="")
    } else if ((read == TRUE) && (execute == FALSE) && (write == TRUE)) {
      content[1] <- "permission=read_write"
      content[2] <- paste("username=", shared.username, sep="")
    } else if ((read == TRUE) && (execute == FALSE) && (write == FALSE)) {
      content[1] <- "permission=read"
      content[2] <- paste("username=", shared.username, sep="")
    } else if ((read == FALSE) && (execute == TRUE) && (write == TRUE)) {
      content[1] <- "permission=write_execute"
      content[2] <- paste("username=", shared.username, sep="")
    } else if ((read == FALSE) && (execute == TRUE) && (write == FALSE)) {
      content[1] <- "permission=execute"
      content[2] <- paste("username=", shared.username, sep="")
    } else if ((read == FALSE) && (execute == FALSE) && (write == TRUE)) {
      content[1] <- "permission=write"
      content[2] <- paste("username=", shared.username, sep="")
    } else {
      return("Error: Must select some permissions")
    }
  }

  if (suppress.Warnings == FALSE){
    dir.exist <- fromJSON(getURL(paste(web_check, file.path, sep=""), curl=curl.call)) 
    if (length(dir.exist$result) != 0){
      if (file.path==""){
        file.exist <- fromJSON(getURL(paste(web_check, file.name, sep=""), curl=curl.call))
      } else {
        file.exist <- fromJSON(getURL(paste(web_check, file.path, "/", file.name, sep=""), curl=curl.call))
      }
    } else {
      if (dir.exist$status == "error"){
        if (dir.exist$message == "File does not exist"){
          return("Error: `file.path' not proper directory")
        } else {
          return("Error: improper username/password combination")
        }
      } else {
        return("Error: `file.path' not proper directory")
      }
    }

    if (length(file.exist$result) == 0){
      return(paste("Error: Invalid `file.name', no `", file.name, "' in the directory `", file.path, "'", sep=""))
    }
  }

  if (file.path == "") {
    web <- paste(web,file.name, sep="")
  } else {
    web <- paste(web, file.path, "/",file.name, sep="")
  }

  if (print.curl){
    curl.string <- paste(first_string," -X POST -d '", paste(content, collapse = "&"), "' ", web, sep="")
    print(curl.string)
  }

  val <- charToRaw(paste(content, collapse = "&"))

  tryCatch(res <- fromJSON(getURLContent(web, curl=curl.call,  infilesize=length(val), readfunction=val, upload=TRUE, customrequest="POST")), error=function(x){return(res <- data.frame(status=paste(x)))})

  if (res$status != "success") {
    sub <- substring(res$status,1,5)
    if (sub == "Error"){
      return("Error: Shared username is not valid")
    } else if (sub == "error"){
      return(res$message)
    } else {
      return(paste(res$status))
    }
  }
}

#################
#################
## RenameFile ###
#################
#################

RenameFile <- function(file.name, new.file.name, file.path="", print.curl=FALSE, suppress.Warnings=FALSE) {

  Time()
  Renew()
  content <- c()
  if (rplant.env$api == "f") {
    web <- paste(rplant.foundation.env$webio, "io/", rplant.foundation.env$user, "/", sep="")
    web_check <- paste(rplant.foundation.env$webio, "io/list/", rplant.foundation.env$user, "/", sep="")
    first_string <- paste("curl -sku '", rplant.foundation.env$user, "'", sep="")
    curl.call <- rplant.foundation.env$curl.call
    content[1] <- "action=rename"
    content[2] <- paste("newName=", new.file.name, sep="")
  } else {
    web <- paste(rplant.agave.env$webio,"media/",rplant.agave.env$user,"/", sep="")
    web_check <- paste(rplant.agave.env$webio, "listings/", rplant.agave.env$user, "/", sep="")
    first_string <- paste("curl -sk -H '", Renew(TRUE), "'", sep="")
    curl.call <- rplant.agave.env$curl.call
    if (file.path == ""){
      content[1] <- paste("path=", rplant.agave.env$user, "/", new.file.name, sep="")
    } else {
      content[1] <- paste("path=", rplant.agave.env$user, "/", file.path, "/", new.file.name, sep="")
    }
    content[2] <- "action=move"
  }

  if (suppress.Warnings == FALSE){
    dir.exist <- fromJSON(getURL(paste(web_check, file.path, sep=""), curl=curl.call)) 
    if (length(dir.exist$result) != 0){
      if (file.path==""){
        file.exist <- fromJSON(getURL(paste(web_check, file.name, sep=""), curl=curl.call))
        new.file.exist <- fromJSON(getURL(paste(web_check, new.file.name, sep=""), curl=curl.call))    
      } else {
        file.exist <- fromJSON(getURL(paste(web_check, file.path, "/", file.name, sep=""), curl=curl.call))
        new.file.exist <- fromJSON(getURL(paste(web_check, file.path, "/", new.file.name, sep=""), curl=curl.call)) 
      }
    } else {
      if (dir.exist$status == "error"){
        if (dir.exist$message == "File does not exist"){
          return("Error: `file.path' not proper directory")
        } else {
          return("Error: improper username/password combination")
        }
      } else {
        return("Error: `file.path' not proper directory")
      }
    }

    if (length(file.exist$result) == 0){
      return(paste("Error: Invalid `file.name', no `", file.name, "' in the directory `", file.path, "'", sep=""))
    }

    if (length(new.file.exist$result) != 0){
      return(paste("Error: Invalid `new.file.name', already file name `", new.file.name, "' in the directory", sep=""))
    }
  }

  if (file.path == ""){
    web <- paste(web, file.name, sep="")
  } else {
    web <- paste(web, file.path, "/", file.name, sep="")
  }

  if (print.curl){
    curl.string <- paste(first_string, " -X PUT -d '", paste(content, collapse = "&"), "' ", web, sep="")
    print(curl.string)
  }

  val <- charToRaw(paste(content, collapse = "&"))

  tryCatch(res <- fromJSON(httpPUT(web, content=val, curl=curl.call)), error=function(x){return(res <- data.frame(status=paste(x)))})

}

#################
#################
### MoveFile ####
#################
#################

MoveFile <- function(file.name, file.path="", end.path="", print.curl=FALSE, suppress.Warnings=FALSE) {

  Time()
  Renew()
  if (rplant.env$api == "f") {
    web <- paste(rplant.foundation.env$webio, "io/", rplant.foundation.env$user, "/", sep="")
    web_check <- paste(rplant.foundation.env$webio, "io/list/", rplant.foundation.env$user, "/", sep="")
    first_string <- paste("curl -sku '", rplant.foundation.env$user, "'", sep="")
    curl.call <- rplant.foundation.env$curl.call
    path <- "newPath="
  } else {
    web <- paste(rplant.agave.env$webio,"media/",rplant.agave.env$user,"/", sep="")
    web_check <- paste(rplant.agave.env$webio, "listings/", rplant.agave.env$user, "/", sep="")
    first_string <- paste("curl -sk -H '", Renew(TRUE), "'", sep="")
    curl.call <- rplant.agave.env$curl.call
    path <- "path="
  }

  if (suppress.Warnings == FALSE){
    dir.exist <- fromJSON(getURL(paste(web_check, file.path, sep=""), curl=curl.call)) 
    if (length(dir.exist$result) != 0){
      if (file.path==""){
        file.exist <- fromJSON(getURL(paste(web_check, file.name, sep=""), curl=curl.call)) 
      } else {
        file.exist <- fromJSON(getURL(paste(web_check, file.path, "/", file.name, sep=""), curl=curl.call))
      }
    } else {
      if (dir.exist$status == "error"){
        if (dir.exist$message == "File does not exist"){
          return("Error: `file.path' not proper directory")
        } else {
          return("Error: improper username/password combination")
        }
      } else {
        return("Error: `file.path' not proper directory")
      }
    }

    if (length(file.exist$result) == 0){
      return(paste("Error: Invalid `file.name', no `", file.name, "' in the directory `", file.path, "'", sep=""))
    }

    dir2.exist <- fromJSON(getURL(paste(web_check, end.path, sep=""), curl=curl.call)) 

    if (length(dir2.exist$result) != 0){
      if (end.path==""){
        file2.exist <- fromJSON(getURL(paste(web_check, file.name, sep=""), curl=curl.call)) 
      } else {
        file2.exist <- fromJSON(getURL(paste(web_check, end.path, "/", file.name, sep=""), curl=curl.call))
      }
    } else {
      return("Error: `end.path' not proper directory")
    }

    if (length(file2.exist$result) != 0){
      return(paste("Error: Cannot move file because `", file.name,"' is already in the directory `", end.path, "'", sep=""))
    }
  }

  content <- c()
  if (end.path == ""){
    content[1] <- paste(path, rplant.env$user, "/", file.name, sep="")
  } else {
    content[1] <- paste(path, rplant.env$user, "/", end.path, "/", file.name, sep="")
  }
  content[2] <- "action=move"

  if (file.path == ""){
    web <- paste(web, file.name, sep="")
  } else {
    web <- paste(web, file.path, "/", file.name, sep="")
  }

  if (print.curl){
    curl.string <- paste(first_string, " -X PUT -d '", paste(content, collapse = "&"), "' ", web, sep="")
    print(curl.string)
  }

  val <- charToRaw(paste(content, collapse = "&"))

  tryCatch(res <- fromJSON(httpPUT(web, content=val, curl=curl.call)), error=function(x){return(res <- data.frame(status=paste(x)))})
}

#################
#################
## DeleteFile ###
#################
#################

DeleteFile <- function(file.name, file.path="", print.curl=FALSE, suppress.Warnings=FALSE) {

  Time()
  Renew()
  if (rplant.env$api == "f") {
    web <- paste(rplant.foundation.env$webio, "io/", rplant.foundation.env$user, "/", sep="")
    web_check <- paste(rplant.foundation.env$webio, "io/list/", rplant.foundation.env$user, "/", sep="")
    first_string <- paste("curl -sku '", rplant.foundation.env$user, "'", sep="")
    curl.call <- rplant.foundation.env$curl.call
  } else {
    web <- paste(rplant.agave.env$webio,"media/",rplant.agave.env$user,"/", sep="")
    web_check <- paste(rplant.agave.env$webio, "listings/", rplant.agave.env$user, "/", sep="")
    first_string <- paste("curl -sk -H '", Renew(TRUE), "'", sep="")
    curl.call <- rplant.agave.env$curl.call
  }

  if (suppress.Warnings == FALSE){
    dir.exist <- fromJSON(getURL(paste(web_check, file.path, sep=""), curl=curl.call)) 
    if (length(dir.exist$result) != 0){
      if (file.path==""){
        file.exist <- fromJSON(getURL(paste(web_check, file.name, sep=""), curl=curl.call)) 
      } else {
        file.exist <- fromJSON(getURL(paste(web_check, file.path, "/", file.name, sep=""), curl=curl.call))
      }
    } else {
      if (dir.exist$status == "error"){
        if (dir.exist$message == "File does not exist"){
          return("Error: `file.path' not proper directory")
        } else {
          return("Error: improper username/password combination")
        }
      } else {
        return("Error: `file.path' not proper directory")
      }
    }

    if (length(file.exist$result) == 0){
      return(paste("Error: Invalid `file.name', no `", file.name, "' in the directory `", file.path, "'", sep=""))
    }
  }

  if (file.path == "") {
    web <- paste(web, file.name, sep="")
  } else {
    web <- paste(web, file.path, "/", file.name, sep="")
  }

  if (print.curl) {
    curl.string <- paste(first_string, " -X DELETE ", web, sep="")
    print(curl.string)
  }

  tryCatch(res <- fromJSON(httpDELETE(web, curl = curl.call)), error=function(x){return(res <- data.frame(status=paste(x)))})
}

#################
#################
## SupportFile ##
#################
#################

SupportFile <- function(print.curl=FALSE) {  

  Time()
  Renew()
  if (rplant.env$api == "f") {
    web <- paste(rplant.foundation.env$webio, "data/transforms/", sep="")
    first_string <- paste("curl -sku '", rplant.foundation.env$user, "'", sep="")
    curl.call <- rplant.foundation.env$curl.call
  } else {
    web <- rplant.agave.env$webtransforms
    first_string <- paste("curl -sk -H '", Renew(TRUE), "'", sep="")
    curl.call <- rplant.agave.env$curl.call
  }

  tryCatch(res <- fromJSON(getForm(web, .checkparams=FALSE, curl=curl.call)), 
           error=function(x){return(res <- data.frame(status=paste(x)))})

  if (print.curl) {
    curl.string <- paste(first_string, "-X GET", web)
    print(curl.string)
  }

  if (res$status != "success") {
    sub <- substring(res$status,1,5)
    if (sub == "Error"){
      return(Error(paste(res$status)))
    } else if (sub == "error"){
      return(res$message)
    } else {
      return(paste(res$status))
    }
  } else {
    file.types <- c()
    for(i in 1:length(res$result)) {
      file.types <- c(file.types, res$result[[i]]$name)
    }
    return(file.types)
  }
}

# -- END -- #




# -- DIRECTORY FUNCTIONS -- #

#################
#################
#### ListDir ####
#################
#################

ListDir <- function(dir.name, dir.path="", print.curl=FALSE, shared.username=NULL, suppress.Warnings=FALSE) {

  Time()
  Renew()
  if (rplant.env$api == "f") {
    web <- paste(rplant.foundation.env$webio, "io/list/", sep="")
    first_string <- paste("curl -sku '", rplant.foundation.env$user, "'", sep="")
    curl.call <- rplant.foundation.env$curl.call
  } else {
    web <- paste(rplant.agave.env$webio, "listings/", sep="")
    first_string <- paste("curl -sk -H '", Renew(TRUE), "'", sep="")
    curl.call <- rplant.agave.env$curl.call
  }

  if (is.null(shared.username)){
    web <- paste(web, rplant.env$user, "/", sep="")
  } else {
    web <- paste(web, shared.username, "/", sep="")
  }

  if (dir.path == ""){
    web <- paste(web, dir.name, sep="")
  } else {
    web <- paste(web, dir.path, "/", dir.name, sep="")
  }

  if (suppress.Warnings == FALSE){
    dir.exist <- fromJSON(getURL(web, curl=curl.call))
    if (length(dir.exist$result) == 0){
      if (dir.exist$status == "error"){
        if (dir.exist$message == "File does not exist"){
          return(paste("Error: `dir.path': `", dir.path,"' not proper directory", sep=""))
        } else {
          return("Error: improper username/password combination")
        }
      } else {
        return(paste("Error: `dir.path': `", dir.path,"' not proper directory", sep=""))
      }
    }
  }

  if (print.curl){
    curl.string <- paste(first_string, " ", web, sep="")
    print(curl.string)
  }

  tryCatch(tmp <- fromJSON(getURL(web, curl=curl.call)), 
           error=function(x){return(tmp <- data.frame(status=paste(x)))})

  res <- matrix(, length(tmp$result), 2)
  colnames(res) <- c("name", "type")
  for (i in 1:length(tmp$result)) {
    res[i, 1] <- tmp$result[[i]]$name
    res[i, 2] <- tmp$result[[i]]$type
  }
  return(res)
}

#################
#################
### ShareDir ####
#################
#################

ShareDir <- function(dir.name, dir.path="", shared.username, read=TRUE, execute=TRUE, write=TRUE, print.curl=FALSE, suppress.Warnings=FALSE) {

  Time()
  Renew()
  content <- c()
  if (rplant.env$api == "f") {
    web <- paste(rplant.foundation.env$webio, "io/share/", rplant.foundation.env$user, "/", sep="")
    web_check <- paste(rplant.foundation.env$webio, "io/list/", rplant.foundation.env$user, "/", sep="")
    first_string <- paste("curl -sku '", rplant.foundation.env$user, "'", sep="")
    curl.call <- rplant.foundation.env$curl.call

    if ((read == TRUE) && (execute == TRUE) && (write == TRUE)) {
      content[1] <- "can_execute=true"
      content[2] <- "can_read=true"
      content[3] <- "can_write=true"
      content[4] <- "recursive=true"
      content[5] <- paste("username=", shared.username, sep="")
    } else if ((read == TRUE) && (execute == TRUE) && (write == FALSE)) {
      content[1] <- "can_execute=true"
      content[2] <- "can_read=true"
      content[3] <- "recursive=true"
      content[4] <- paste("username=", shared.username, sep="")
    } else if ((read == TRUE) && (execute == FALSE) && (write == TRUE)) {
      content[1] <- "can_read=true"
      content[2] <- "can_write=true"
      content[3] <- "recursive=true"
      content[4] <- paste("username=", shared.username, sep="")
    } else if ((read == TRUE) && (execute == FALSE) && (write == FALSE)) {
      content[1] <- "can_read=true"
      content[2] <- "recursive=true"
      content[3] <- paste("username=", shared.username, sep="")
    } else if ((read == FALSE) && (execute == TRUE) && (write == TRUE)) {
      content[1] <- "can_execute=true"
      content[2] <- "can_write=true"
      content[3] <- "recursive=true"
      content[4] <- paste("username=", shared.username, sep="")
    } else if ((read == FALSE) && (execute == TRUE) && (write == FALSE)) {
      content[1] <- "can_execute=true"
      content[2] <- "recursive=true"
      content[3] <- paste("username=", shared.username, sep="")
    } else if ((read == FALSE) && (execute == FALSE) && (write == TRUE)) {
      content[1] <- "can_write=true"
      content[2] <- "recursive=true"
      content[3] <- paste("username=", shared.username, sep="")
    } else {
      return("Error: Must select some permissions")
    }
  } else {
    web <- paste(rplant.agave.env$webio,"pems/",rplant.agave.env$user,"/", sep="")
    web_check <- paste(rplant.agave.env$webio, "listings/", rplant.agave.env$user, "/", sep="")
    first_string <- paste("curl -sk -H '", Renew(TRUE), "'", sep="")
    curl.call <- rplant.agave.env$curl.call

    if ((read == TRUE) && (execute == TRUE) && (write == TRUE)) {
      content[1] <- "permission=all"
      content[2] <- paste("username=", shared.username, sep="")
    } else if ((read == TRUE) && (execute == TRUE) && (write == FALSE)) {
      content[1] <- "permission=read_execute"
      content[2] <- paste("username=", shared.username, sep="")
    } else if ((read == TRUE) && (execute == FALSE) && (write == TRUE)) {
      content[1] <- "permission=read_write"
      content[2] <- paste("username=", shared.username, sep="")
    } else if ((read == TRUE) && (execute == FALSE) && (write == FALSE)) {
      content[1] <- "permission=read"
      content[2] <- paste("username=", shared.username, sep="")
    } else if ((read == FALSE) && (execute == TRUE) && (write == TRUE)) {
      content[1] <- "permission=write_execute"
      content[2] <- paste("username=", shared.username, sep="")
    } else if ((read == FALSE) && (execute == TRUE) && (write == FALSE)) {
      content[1] <- "permission=execute"
      content[2] <- paste("username=", shared.username, sep="")
    } else if ((read == FALSE) && (execute == FALSE) && (write == TRUE)) {
      content[1] <- "permission=write"
      content[2] <- paste("username=", shared.username, sep="")
    } else {
      return("Error: Must select some permissions")
    }
    content[3] <- "recursive=true"
  }

  if (suppress.Warnings == FALSE){
    if (dir.path == ""){
      dir.exist <- fromJSON(getURL(paste(web_check, dir.name, sep=""), curl=curl.call))
    } else {
      dir.exist <- fromJSON(getURL(paste(web_check, dir.path, "/", dir.name, sep=""), curl=curl.call))
    }

    if (length(dir.exist$result) == 0){
      if (dir.exist$status == "error"){
        if (dir.exist$message == "File does not exist"){
          return("Error: `dir.path' not proper directory")
        } else {
          return("Error: improper username/password combination")
        }
      } else {
        return("Error: `dir.path' not proper directory")
      }
    }
  }

  if (dir.path == "") {
    web <- paste(web, dir.name, sep="")
  } else {
    web <- paste(web, dir.path, "/",dir.name, sep="")
  }

  if (print.curl){
    curl.string <- paste(first_string," -X POST -d '", paste(content, collapse = "&"), "' ", web, sep="")
    print(curl.string)
  }

  val <- charToRaw(paste(content, collapse = "&"))

  tryCatch(res <- fromJSON(getURLContent(web, curl=curl.call,  infilesize=length(val), readfunction=val, upload=TRUE, customrequest="POST")), error=function(x){return(res <- data.frame(status=paste(x)))})

  if (res$status != "success") {
    sub <- substring(res$status,1,5)
    if (sub == "Error"){
      return("Error: Shared username is not valid")
    } else if (sub == "error"){
      return(res$message)
    } else {
      return(paste(res$status))
    }
  }
}

#################
#################
### RenameDir ###
#################
#################

RenameDir <- function(dir.name, new.dir.name, dir.path="", print.curl=FALSE, suppress.Warnings=FALSE) {

  Time()
  Renew()
  content <- c()
  if (rplant.env$api == "f") {
    web <- paste(rplant.foundation.env$webio, "io/", rplant.foundation.env$user, "/", sep="")
    web_check <- paste(rplant.foundation.env$webio, "io/list/", rplant.foundation.env$user, "/", sep="")
    web_check <- paste(rplant.foundation.env$webio, "io/list/", rplant.foundation.env$user, "/", sep="")
    first_string <- paste("curl -sku '", rplant.foundation.env$user, "'", sep="")
    curl.call <- rplant.foundation.env$curl.call
    content[1] <- "action=rename"
    content[2] <- paste("newName=", new.dir.name, sep="")
  } else {
    web <- paste(rplant.agave.env$webio,"media/",rplant.agave.env$user,"/", sep="")
    web_check <- paste(rplant.agave.env$webio, "listings/", rplant.agave.env$user, "/", sep="")
    first_string <- paste("curl -sk -H '", Renew(TRUE), "'", sep="")
    curl.call <- rplant.agave.env$curl.call
    if (dir.path == ""){
      content[1] <- paste("path=", rplant.agave.env$user, "/", new.dir.name, sep="")
    } else {
      content[1] <- paste("path=", rplant.agave.env$user, "/", dir.path, "/", new.dir.name, sep="")
    }
    content[2] <- "action=move"
  }

  if (suppress.Warnings == FALSE){
    dir.exist <- fromJSON(getURL(paste(web_check, dir.path, sep=""), curl=curl.call)) 
    if (length(dir.exist$result) != 0){
      if (dir.path==""){
        file.exist <- fromJSON(getURL(paste(web_check, dir.name, sep=""), curl=curl.call))
        new.file.exist <- fromJSON(getURL(paste(web_check, new.dir.name, sep=""), curl=curl.call))    
      } else {
        file.exist <- fromJSON(getURL(paste(web_check, dir.path, "/", dir.name, sep=""), curl=curl.call))
        new.file.exist <- fromJSON(getURL(paste(web_check, dir.path, "/", new.dir.name, sep=""), curl=curl.call)) 
      }
    } else {
      if (dir.exist$status == "error"){
        if (dir.exist$message == "Directory does not exist"){
          return("Error: `dir.path' not proper directory")
        } else {
          return("Error: improper username/password combination")
        }
      } else {
        return("Error: `dir.path' not proper directory")
      }
    }

    if (length(file.exist$result) == 0){
      return(paste("Error: Invalid `dir.name', no `", dir.name, "' in the directory `", dir.path, "'", sep=""))
    }

    if (length(new.file.exist$result) != 0){
      return(paste("Error: Invalid `new.dir.name', already directory named `", new.dir.name, "' in the directory", sep=""))
    }
  }

  if (dir.path == ""){
    web <- paste(web, dir.name, sep="")
  } else {
    web <- paste(web, dir.path, "/", dir.name, sep="")
  }

  if (print.curl){
    curl.string <- paste(first_string, " -X PUT -d '", paste(content, collapse = "&"), "' ", web, sep="")
    print(curl.string)
  }

  val <- charToRaw(paste(content, collapse = "&"))

  tryCatch(res <- fromJSON(httpPUT(web, content=val, curl=curl.call)), error=function(x){return(res <- data.frame(status=paste(x)))})
}

#################
#################
#### MoveDir ####
#################
#################

MoveDir <- function(dir.name, dir.path="", end.path="", print.curl=FALSE, suppress.Warnings=FALSE) {

  Time()
  Renew()
  if (rplant.env$api == "f") {
    web <- paste(rplant.foundation.env$webio, "io/", rplant.foundation.env$user, "/", sep="")
    web_check <- paste(rplant.foundation.env$webio, "io/list/", rplant.foundation.env$user, "/", sep="")
    first_string <- paste("curl -sku '", rplant.foundation.env$user, "'", sep="")
    curl.call <- rplant.foundation.env$curl.call
    path <- "newPath="
  } else {
    web <- paste(rplant.agave.env$webio,"media/",rplant.agave.env$user,"/", sep="")
    web_check <- paste(rplant.agave.env$webio, "listings/", rplant.agave.env$user, "/", sep="")
    first_string <- paste("curl -sk -H '", Renew(TRUE), "'", sep="")
    curl.call <- rplant.agave.env$curl.call
    path <- "path="
  }

  if (suppress.Warnings == FALSE){
    dir.exist <- fromJSON(getURL(paste(web_check, dir.path, sep=""), curl=curl.call)) 
    if (length(dir.exist$result) != 0){
      if (dir.path==""){
        file.exist <- fromJSON(getURL(paste(web_check, dir.name, sep=""), curl=curl.call)) 
      } else {
        file.exist <- fromJSON(getURL(paste(web_check, dir.path, "/", dir.name, sep=""), curl=curl.call))
      }
    } else {
      if (dir.exist$status == "error"){
        if (dir.exist$message == "File does not exist"){
          return("Error: `dir.path' not proper directory")
        } else {
          return("Error: improper username/password combination")
        }
      } else {
        return("Error: `dir.path' not proper directory")
      }
    }

    if (length(file.exist$result) == 0){
      return(paste("Error: Invalid `dir.name', no `", dir.name, "' in the directory `", dir.path, "'", sep=""))
    }

    dir2.exist <- fromJSON(getURL(paste(web_check, end.path, sep=""), curl=curl.call)) 

    if (length(dir2.exist$result) != 0){
      if (end.path==""){
        file2.exist <- fromJSON(getURL(paste(web_check, dir.name, sep=""), curl=curl.call)) 
      } else {
        file2.exist <- fromJSON(getURL(paste(web_check, end.path, "/", dir.name, sep=""), curl=curl.call))
      }
    } else {
      return("Error: `end.path' not proper directory")
    }

    if (length(file2.exist$result) != 0){
      return(paste("Error: Cannot move file because `", dir.name,"' is already in the directory `", end.path, "'", sep=""))
    }
  }

  content <- c()
  if (end.path == ""){
    content[1] <- paste(path, rplant.env$user, "/", dir.name, sep="")
  } else {
    content[1] <- paste(path, rplant.env$user, "/", end.path, "/", dir.name, sep="")
  }
  content[2] <- "action=move"

  if (dir.path == ""){
    web <- paste(web, dir.name, sep="")
  } else {
    web <- paste(web, dir.path, "/", dir.name, sep="")
  }

  if (print.curl){
    curl.string <- paste(first_string, " -X PUT -d '", paste(content, collapse = "&"), "' ", web, sep="")
    print(curl.string)
  }

  val <- charToRaw(paste(content, collapse = "&"))

  tryCatch(res <- fromJSON(httpPUT(web, content=val, curl=curl.call)), error=function(x){return(res <- data.frame(status=paste(x)))})
}

#################
#################
#### MakeDir ####
#################
#################

MakeDir <- function(dir.name, dir.path="", print.curl=FALSE, suppress.Warnings=FALSE) {

  Time()
  Renew()
  content <- c()
  if (rplant.env$api == "f") {
    web_check <- paste(rplant.foundation.env$webio, "io/list/", rplant.foundation.env$user, "/", sep="")
    first_string <- paste("curl -sku '", rplant.foundation.env$user, "'", sep="")
    curl.call <- rplant.foundation.env$curl.call
    content[1] <- paste("dirName=", dir.name, sep="")
    if (dir.path==""){
      web <- paste(rplant.foundation.env$webio, "io/", rplant.foundation.env$user, "/", sep="")
    } else {
      web <- paste(rplant.foundation.env$webio, "io/", rplant.foundation.env$user, "/", dir.path, sep="")
    }
  } else {
    web <- paste(rplant.agave.env$webio,"media/",rplant.agave.env$user,"/", sep="")
    web_check <- paste(rplant.agave.env$webio, "listings/", rplant.agave.env$user, "/", sep="")
    first_string <- paste("curl -sk -H '", Renew(TRUE), "'", sep="")
    curl.call <- rplant.agave.env$curl.call
    if (dir.path==""){
      content[1] <- paste("path=", dir.name, sep="") 
    } else {
      content[1] <- paste("path=", dir.path, "/", dir.name, sep="") 
    }
  }

  if (suppress.Warnings == FALSE){
    dir.exist <- fromJSON(getURL(paste(web_check, dir.path, sep=""), curl=curl.call)) 
    if (length(dir.exist$result) != 0){
      if (dir.path==""){
        dir2.exist <- fromJSON(getURL(paste(web_check, dir.name, sep=""), curl=curl.call)) 
      } else {
        dir2.exist <- fromJSON(getURL(paste(web_check, dir.path, "/", dir.name, sep=""), curl=curl.call))
      }
    } else {
      if (dir.exist$status == "error"){
        if (dir.exist$message == "File does not exist"){
          return(paste("Error: `dir.path': `", dir.path,"' not proper directory", sep=""))
        } else {
          return("Error: improper username/password combination")
        }
      } else {
        return(paste("Error: `dir.path': `", dir.path,"' not proper directory", sep=""))
      }
    }

    if (length(dir2.exist$result) != 0){
      return(paste("Error: Cannot make directory because `", dir.name,"' already exists in the directory `", dir.path, "'", sep=""))
    }
  }

  content[2] <- "action=mkdir"

  if (print.curl) {
    curl.string <- paste(first_string, " -d '", paste(content, collapse = "&"), "' ", web, sep="")
    print(curl.string)
  }

  val <- charToRaw(paste(content, collapse = "&"))

  tryCatch(res <- fromJSON(httpPUT(paste(web, dir.path, sep=""), content=val, curl=curl.call)), error=function(x){return(res <- data.frame(status=paste(x)))})
}

#################
#################
### DeleteDir ###
#################
#################

DeleteDir <- function(dir.name, dir.path="", print.curl=FALSE, suppress.Warnings=FALSE) {

  Time()
  Renew()
  if (rplant.env$api == "f") {
    web <- paste(rplant.foundation.env$webio, "io/", rplant.foundation.env$user, "/", sep="")
    web_check <- paste(rplant.foundation.env$webio, "io/list/", rplant.foundation.env$user, "/", sep="")
    first_string <- paste("curl -sku '", rplant.foundation.env$user, "'", sep="")
    curl.call <- rplant.foundation.env$curl.call
  } else {
    web <- paste(rplant.agave.env$webio,"media/",rplant.agave.env$user,"/", sep="")
    web_check <- paste(rplant.agave.env$webio, "listings/", rplant.agave.env$user, "/", sep="")
    first_string <- paste("curl -sk -H '", Renew(TRUE), "'", sep="")
    curl.call <- rplant.agave.env$curl.call
  }

  if (suppress.Warnings == FALSE){
    dir.exist <- fromJSON(getURL(paste(web_check, dir.path, sep=""), curl=curl.call)) 
    if (length(dir.exist$result) != 0){

      if (dir.path==""){
        dir2.exist <- fromJSON(getURL(paste(web_check, dir.name, sep=""), curl=curl.call)) 
      } else {
        dir2.exist <- fromJSON(getURL(paste(web_check, dir.path, "/", dir.name, sep=""), curl=curl.call))
      }
    } else {
      if (dir.exist$status == "error"){
        if (dir.exist$message == "File does not exist"){
          return(paste("Error: `dir.path': `", dir.path,"' not proper directory", sep=""))
        } else {
          return("Error: improper username/password combination")
        }
      } else {
        return(paste("Error: `dir.path': `", dir.path,"' not proper directory", sep=""))
      }
    }

    if (length(dir2.exist$result) == 0){
      return(paste("Error: `", dir.name,"' does not exist in `", dir.path, "'", sep=""))
    }
  }
  if (dir.path == "") {
    web <- paste(web, dir.name, sep="")
  } else {
    web <- paste(web, dir.path, "/", dir.name, sep="")
  }

  if (print.curl) {
    curl.string <- paste(first_string, " -X DELETE ", web, sep="")
    print(curl.string)
  }

  tryCatch(res <- fromJSON(httpDELETE(web, curl = curl.call)), error=function(x){return(res <- data.frame(status=paste(x)))})
}

# -- END -- #




# -- APPLICATION FUNCTIONS -- #

#################
#################
### ListApps ####
#################
#################

ListApps<- function (description=FALSE, print.curl=FALSE) 
{

  Time()
  Renew()
  if (rplant.env$api == "f") {
    web <- paste(rplant.foundation.env$webapps, "apps/list/", sep="")
    first_string <- paste("curl -sku '", rplant.foundation.env$user, "'", sep="")
    curl.call <- rplant.foundation.env$curl.call
  } else {
    web <- rplant.agave.env$webapps
    first_string <- paste("curl -sk -H '", Renew(TRUE), "'", sep="")
    curl.call <- rplant.agave.env$curl.call
  }

  tryCatch(tmp <- fromJSON(getForm(web, .checkparams=FALSE, curl=curl.call)), 
           error=function(x){return(tmp <- data.frame(status=paste(x)))})

  if (print.curl) {
    curl.string <- paste(first_string, " ", web, sep="")
    print(curl.string)
  }

  if (tmp$status != "success") {
    sub <- substring(tmp$status,1,5)
    if (sub == "Error"){
      Error(paste(tmp$status))
    } else if (sub == "error"){
      return(tmp$message)
    } else {
      return(paste(tmp$status))
    }
  } else {
    Apps <- list()
    for (j in 1:length(tmp$result)){
      ans <- TestApp(tmp$result[[j]]$id)
      if ((j != 1) & (!is.null(ans[[1]]))){
        for (k in 1:length(Apps)){
          if (ans[[1]] == Apps[[k]][1]){
            ans <- list(NULL, NULL)
            break
          }
        }
      }
      if (!is.null(ans[[1]])){
        Apps <- append(Apps,list(c(ans)))
      }
    }
    if (description == TRUE){
      res <- matrix(, length(Apps))
      colnames(res) <- "Application"
      for (i in 1:length(Apps)) res[i, 1] <- paste(Apps[[i]], collapse=" - ")
    } else {
      res <- matrix(, length(Apps))
      colnames(res) <- "Application"
      for (i in 1:length(Apps)) res[i, 1] <- Apps[[i]][1]
    }
    return(sort(res))
  }
}

#################
#################
## GetAppInfo ###
#################
#################

GetAppInfo <- function(application, return.json=FALSE, print.curl=FALSE) {

  Time()
  Renew()
  if (rplant.env$api == "f"){
    web <- paste(rplant.foundation.env$webapps, "apps/name/", sep="")
    curl.call <- rplant.foundation.env$curl.call
    first_string <- paste("curl -sku '", rplant.foundation.env$user, "'", sep="")
    tmp_string <- "tmp$result[[len]]"
  } else {
    web <- rplant.agave.env$webapps
    curl.call <- rplant.agave.env$curl.call
    first_string <- paste("curl -sk -H '", Renew(TRUE), "'", sep="")
    tmp_string <- "tmp$result"
  }

 if (substring(application,nchar(application)-1,nchar(application)-1) == "u"){
    priv.APP <- substring(application,1,nchar(application)-2)
    version <- as.numeric(substring(application,nchar(application),nchar(application)))
    text <- "Public App"
  } else if (substring(application,nchar(application)-2,nchar(application)-2) == "u"){
    priv.APP <- substring(application,1,nchar(application)-3)
    version <- as.numeric(paste(substring(application,nchar(application)-1,nchar(application)-1),substring(application,nchar(application),nchar(application)),sep=""))
    text <- "Public App"
  } else {
    priv.APP <- application
    text <- "Private App"
  }

  tryCatch(tmp <- fromJSON(getForm(paste(web, priv.APP, sep=""), .checkparams=FALSE, curl=curl.call)), error=function(x){return(tmp <- data.frame(status=paste(x)))})

  if (tmp$status != "success") {
    sub <- substring(tmp$status,1,5)
    if (sub == "Error"){
      return(Error(paste(tmp$status)))
    } else if (sub == "error"){
      return(tmp$message)
    } else {
      return(paste(tmp$status))
    }
  } else {
    len <- length(tmp$result)

    if (eval(parse(text=paste(tmp_string, "$public", sep=""))) == FALSE) {
      text <- "Private App"
    }  else if (length(tmp) == 0) {
      return("No information on application")
    }

    if (text == "Public App"){
      APP <- tmp$result[[len]]$id
      if (substring(APP,nchar(APP)-1,nchar(APP)-1) == "u"){
        priv.APP <- substring(APP,1,nchar(APP)-2)
        version.APP <- as.numeric(substring(APP,nchar(APP),nchar(APP)))
      } else if (substring(APP,nchar(APP)-2,nchar(APP)-2) == "u"){
        priv.APP <- substring(APP,1,nchar(APP)-3)
        version.APP <- as.numeric(paste(substring(APP,nchar(APP)-1,nchar(APP)-1),substring(APP,nchar(APP),nchar(APP)),sep=""))
      }
      if (version.APP > version){
        v.text <- paste("Deprecated, the newest version is:", APP)
      } else {
        v.text <- "Newest Version"
      }
    }

    if (print.curl) {
      curl.string <- paste(first_string, " ", web, priv.APP, sep="")
      print(curl.string)
    }
  
    if (return.json) {
      return(tmp)
    } else {
      app.info<-c()

      for (input in sequence(length(eval(parse(text=paste(tmp_string, "$inputs", sep="")))))) {
        app.info <- rbind(app.info, c("input", eval(parse(text=paste(tmp_string, "$inputs[[input]]$id", sep=""))), eval(parse(text=paste(tmp_string, "$inputs[[input]]$semantics$fileTypes[1]", sep=""))), eval(parse(text=paste(tmp_string, "$inputs[[input]]$details$description", sep="")))))
      }

      for (output in sequence(length(eval(parse(text=paste(tmp_string, "$outputs", sep="")))))) {
        app.info <- rbind(app.info, c("output", eval(parse(text=paste(tmp_string, "$outputs[[output]]$id", sep=""))), eval(parse(text=paste(tmp_string, "$outputs[[output]]$semantics$fileTypes[1]", sep=""))), eval(parse(text=paste(tmp_string, "$outputs[[output]]$details$description", sep=""))))) 
      }

      for (parameter in sequence(length(eval(parse(text=paste(tmp_string, "$parameters", sep="")))))) {
        app.info <- rbind(app.info, c("output", eval(parse(text=paste(tmp_string, "$parameters[[parameter]]$id", sep=""))), eval(parse(text=paste(tmp_string, "$parameters[[parameter]]$value$type", sep=""))), eval(parse(text=paste(tmp_string, "$parameters[[parameter]]$details$label", sep=""))))) 
      }

      colnames(app.info)<-c("kind", "id", "fileType/value", "details")
      if (text == "Private App"){
        return(list(eval(parse(text=paste(tmp_string, "$longDescription", sep=""))), Application=c(application, text), app.info))
      } else {
        return(list(Description=eval(parse(text=paste(tmp_string, "$longDescription", sep=""))), Application=c(application, text, v.text), Information=app.info))
      }
    }
  }
}
# -- END -- #
 

# -- JOB FUNCTIONS -- #

#################
#################
### SubmitJob ###
#################
#################

SubmitJob <- function(application, file.path="", file.list=NULL, input.list, 
                      args.list=NULL, job.name, nprocs=1, private.APP=FALSE, 
                      suppress.Warnings=FALSE, shared.username=NULL,
                      print.curl=FALSE, email=TRUE) {

  ### Job Name is automatically time stamped
  job.name <- paste(unlist(strsplit(paste(job.name, "_", format(Sys.time(), 
                    "%Y-%m-%d_%k-%M-%OS3"), sep=""), " ")), collapse="")

  Time()
  Renew()
  content <- c()

  if (rplant.env$api == "f") {
    web <- paste(rplant.foundation.env$webapps, "job", sep="")
    web_check <- paste(rplant.foundation.env$webio, "io/list/", sep="")
    web_email <- paste(rplant.foundation.env$webprofiles, "search/username/", rplant.env$user, sep="")
    web_test <- paste(rplant.foundation.env$webapps, "apps/name/", sep="")
    tmp_string <- "tmp$result[[len]]"
    eml_string <- "callbackUrl="
    first_string <- paste("curl -sku '", rplant.foundation.env$user, "'", sep="")
    curl.call <- rplant.foundation.env$curl.call
    content[1] <- paste("jobName=", job.name, sep="")
    content[2] <- paste("softwareName=", application, sep="")
    content[3] <- paste("processorCount=", nprocs, sep="")
    content[4] <- "requestedTime=24:00:00"
  } else {
    web <- rplant.agave.env$webjobs
    web_check <- paste(rplant.agave.env$webio, "listings/", sep="")
    web_email <- paste(rplant.agave.env$webprofiles, "search/username/", rplant.env$user, sep="")
    web_test <- rplant.agave.env$webapps
    tmp_string <- "tmp$result"
    eml_string <- "callbackURL="
    first_string <- paste("curl -sk -H '", Renew(TRUE), "'", sep="")
    curl.call <- rplant.agave.env$curl.call
    content[1] <- paste("name=", job.name, sep="")
    content[2] <- paste("appId=", application, sep="")
    content[3] <- paste("nodeCount=", nprocs, sep="")
    content[4] <- "maxRunTime=24:00:00"
  }

  if (is.null(shared.username)){
    web_check <- paste(web_check, rplant.env$user, "/", sep="");
  } else {
    web_check <- paste(web_check, shared.username, "/", sep="")
  }

  if (suppress.Warnings == FALSE){
    dir.exist <- fromJSON(getURL(paste(web_check, file.path, sep=""), curl=curl.call)) 
    if (length(dir.exist$result) != 0){
      file.exist <- list()
      for (i in 1:length(file.list)){
        if (file.path==""){
          file.exist[[i]] <- fromJSON(getURL(paste(web_check, file.list[[i]], sep=""), curl=curl.call)) 
        } else {
          file.exist[[i]] <- fromJSON(getURL(paste(web_check, file.path, "/", file.list[[i]], sep=""), curl=curl.call)) 
        }
      }
    } else {
      if (dir.exist$status == "error"){
        if (dir.exist$message == "File does not exist"){
          return("Error: `file.path' not proper directory")
        } else {
          return("Error: improper username/password combination")
        }
      } else {
        return("Error: `file.path' not proper directory")
      }
    }

    for (i in 1:length(file.list)){
      if (length(file.exist[[i]]$result) == 0){
        return("Error: At least one of the input files in `file.list' does not exist")
      }
    }

    if (substring(application,nchar(application)-1,nchar(application)-1) == "u"){
      priv.APP <- substring(application,1,nchar(application)-2)
    } else if (substring(application,nchar(application)-2,nchar(application)-2) == "u"){
      priv.APP <- substring(application,1,nchar(application)-3)
    } else {
      if (private.APP==FALSE){
        return("Private application, not valid for SubmitJob.  If it is your own private application use private.APP=TRUE")
      } else {
        priv.APP <- application
      }
    }

    tmp <- fromJSON(getForm(paste(web_test, priv.APP, sep=""), .checkparams=FALSE, curl=curl.call))

    len <- length(tmp$result)
    if (private.APP==FALSE) {
      if (eval(parse(text=paste(tmp_string, "$public", sep=""))) == FALSE) {
        return("Not a valid application: it's private, must be public.  Check ListApps function")
      }
    }
    if (length(tmp) == 0) {
      return("No information on application: not valid")
    } else if (eval(parse(text=paste(tmp_string, "$id", sep=""))) != application){
      return(paste("Application deprecated, should be:",tmp$result[[len]]$id))
    }

    app.info<-c()
    for (input in sequence(length(eval(parse(text=paste(tmp_string, "$inputs", sep="")))))) {
      app.info <- rbind(app.info, c("input", eval(parse(text=paste(tmp_string, "$inputs[[input]]$id", sep="")))))
    }

    test.input <- rep(0, length(input.list))
    for (j in 1:length(input.list)){
      for (i in 1:length(app.info[,2])){
        if (input.list[[j]] == app.info[,2][i]){
          test.input[j] <- 1
          break;
        }
      }
    }

    for (i in 1:length(test.input)){
      if (test.input[i] == 0 ){
        return("At least one of the inputs in `input.list' is incorrect, check GetAppInfo function for proper inputs")
      }
    }
  }

  if (suppress.Warnings==TRUE){
    if (substring(application,nchar(application)-1,nchar(application)-1) == "u"){
      priv.APP <- substring(application,1,nchar(application)-2)
    } else if (substring(application,nchar(application)-2,nchar(application)-2) == "u"){
      priv.APP <- substring(application,1,nchar(application)-3)
    } else {
      priv.APP <- application
    }

    tmp <- fromJSON(getForm(paste(web_test, priv.APP, sep=""), .checkparams=FALSE, curl=curl.call))

  }

  len <- length(tmp$result)
  set <- eval(parse(text=paste(tmp_string, "$parallelism", sep="")))

# for (i in 1:length(tmp$result)){
#   set <- tryCatch(tmp$result[[i]]$parallelism, error=function(x){return(NA)})
#   if (!is.na(set)){
#     break
#   }
# }

  if (set == "PARALLEL"){
    if (nprocs < 2){
      nprocs = 12
    } else if (nprocs > 512){
      nprocs = 512
    }
  } else {
    if (nprocs != 1){
      nprocs = 1
    }
  }

  if(!is.null(args.list)){
    m <- length(args.list)
  } else {
    m <- NULL
  }
  if(!is.null(file.list)){
    n <- length(file.list)
    n1 <- length(file.list)
  } else {
    n <- NULL
    n1 <- 0
  }

  # Automatically make analyses directory; will not overwrite if already present
  MakeDir("analyses", suppress.Warnings=TRUE)

  content[5] <- "archive=1"
  content[6] <- paste("archivePath=/", rplant.env$user, "/analyses/", job.name, sep=""); x <- 6;

  if (email==TRUE){
    tryCatch(res <- fromJSON(getURLContent(web_email, curl=curl.call)), error=function(x){return(res <- data.frame(status=paste(x)))})
    content[7] <- paste(eml_string, res$result[[1]]$email, sep=""); x <- 7;
  }

  if (!is.null(n)){
    for (i in c(1:n)){
      if (file.path=="") {
        if (is.null(shared.username)){
          content[x+i] <- paste(input.list[[i]],"=/", rplant.env$user, 
                                "/", file.list[[i]], sep="")
        } else {
          content[x+i] <- paste(input.list[[i]],"=/", shared.username, 
                                "/", file.list[[i]], sep="")
        }
      } else {
        if (is.null(shared.username)){
          content[x+i] <- paste(input.list[[i]],"=/", rplant.env$user, "/",
                                file.path, "/", file.list[[i]], sep="")
        } else {
          content[x+i] <- paste(input.list[[i]],"=/", shared.username, "/",
                                file.path, "/", file.list[[i]], sep="")
        }
      }
    }
  }

  if (!is.null(m)){
    for (i in c(1:m)){
      content[x+n1+i] <- paste(args.list[[i]][1],"=", 
                               args.list[[i]][2], sep="")
    }
  }

  if (print.curl) {
    curl.string <- paste(first_string," -X POST -d '", paste(content, collapse = "&"), "' ", web, sep="")
    print(curl.string)
  }

  val <- charToRaw(paste(content, collapse = "&"))

  tryCatch(res <- fromJSON(getURLContent(web, curl=curl.call,  infilesize=length(val), readfunction=val, upload=TRUE, customrequest="POST")), error=function(x){return(res <- data.frame(status=paste(x)))})

  if (res$status != "success") {
    sub <- substring(res$status,1,5)
    if (sub == "Error"){
      return(Error(paste(res$status)))
    } else if (sub == "error"){
      return(res$message)
    } else {
      return(paste(res$status))
    }
  } else {
    cat(paste("Job submitted. You can check your job using CheckJobStatus(", 
        res$result$id, ")", sep=""), "\n")
    return(list(res$result$id, job.name))
  }
}

####################
####################
## CheckJobStatus ##
####################
####################

CheckJobStatus <- function(job.id, details=FALSE, print.curl=FALSE) {

  Time()
  Renew()
  if (rplant.env$api == "f") {
    web <- paste(rplant.foundation.env$webapps, "job/", job.id, sep="")
    first_string <- paste("curl -sku '", rplant.foundation.env$user, "'", sep="")
    curl.call <- rplant.foundation.env$curl.call
  } else {
    web <- paste(rplant.agave.env$webjobs, job.id, "/", sep="")
    first_string <- paste("curl -sk -H '", Renew(TRUE), "'", sep="")
    curl.call <- rplant.agave.env$curl.call
  }

  if (!(((rplant.env$api == "f") && (details == TRUE)) || (details == FALSE))){
     web <- paste(web, "history", sep="")
  }

  tryCatch(res <- fromJSON(getForm(web, .checkparams=FALSE, curl=curl.call)),  error=function(x){return(res <- data.frame(status=paste(x)))})

  if (print.curl) {
    curl.string <- paste(first_string, web)
    print(curl.string)
  }

  if ((res$status != "success") || (length(res$result) == 0)) {
    sub <- substring(res$status,1,5)
    if (sub == "Error"){
      sub1 <- substring(res$status,8,8)
      if (sub1 == "U"){
        return("Error: Invalid username/password combination")
      }
    }
    return(paste("Error: job: `",job.id,"' does not exist", sep=""))
  } else {

    if (!(((rplant.env$api == "f") && (details == TRUE)) || (details == FALSE))){
      len <- length(res$result)
      strt_str <- strsplit(res$result[[1]]$created,"T")
      strt_time <- as.POSIXct(strsplit(strt_str[[1]][2],"-")[[1]][1],format="%H:%M:%S")
      end_str <- strsplit(res$result[[len]]$created,"T")
      end_time <- as.POSIXct(strsplit(end_str[[1]][2],"-")[[1]][1],format="%H:%M:%S")
      return(paste(res$result[[len]]$description, ", Time:", round(end_time - strt_time,2), " mins", sep=""))
    } else { 
      return(res$result$status)
    }
  }
}

####################
####################
##### KillJob ######
####################
####################

KillJob <- function(job.id, print.curl=FALSE) {

  Time()
  Renew()
  if (rplant.env$api == "f") {
    web <- paste(rplant.foundation.env$webapps, "job/", job.id, sep="")
    first_string <- paste("curl -sku '", rplant.foundation.env$user, "'", sep="")
    curl.call <- rplant.foundation.env$curl.call
  } else {
    web <- paste(rplant.agave.env$webjobs, job.id, "/", sep="")
    first_string <- paste("curl -sk -H '", Renew(TRUE), "'", sep="")
    curl.call <- rplant.agave.env$curl.call
  }

  content <- c()
  content[1] <- "action=stop"

  val <- charToRaw(paste(content, collapse = "&"))

  tryCatch(res <- fromJSON(getURLContent(web, curl=curl.call,  infilesize=length(val), readfunction=val, upload=TRUE, customrequest="POST")), error=function(x){return(res <- data.frame(status=paste(x)))})

  if (print.curl) {
    curl.string <- paste(first_string, " -X POST -d '", paste(content, collapse = "&"), "' ",  web, sep="")
    print(curl.string)
  }

  if (res$status != "success") {
    sub <- substring(res$status,1,5)
    if (sub == "Error"){
      sub1 <- substring(res$status,8,8)
      if (sub1 == "U"){
        return("Error: Invalid username/password combination")
      }
    }
    return(paste("Error: job: `",job.id,"' does not exist", sep=""))
  }
}

####################
####################
#### DeleteALL #####
####################
####################

DeleteALL <- function() {

  Time()
  Renew()
  if (rplant.env$api == "f") {
    web <- paste(rplant.foundation.env$webapps, "jobs/list", sep="")
    web_check <- paste(rplant.foundation.env$webio, "io/list/", rplant.foundation.env$user, "/", sep="")
    web_del <- paste(rplant.foundation.env$webio, "io/", rplant.foundation.env$user, "/", sep="")
    first_string <- paste("curl -sku '", rplant.foundation.env$user, "'", sep="")
    curl.call <- rplant.foundation.env$curl.call
  } else {
    web <- rplant.agave.env$webjobs
    web_check <- paste(rplant.agave.env$webio, "listings/", rplant.agave.env$user, "/", sep="")
    web_del <- paste(rplant.agave.env$webio,"media/",rplant.agave.env$user,"/", sep="")
    first_string <- paste("curl -sk -H '", Renew(TRUE), "'", sep="")
    curl.call <- rplant.agave.env$curl.call
  }

  tryCatch(res <- fromJSON(getForm(web, .checkparams=FALSE, curl=curl.call)), error=function(x){return(res <- data.frame(status=paste(x)))})

  if (res$status != "success") {
    sub <- substring(res$status,1,5)
    if (sub == "Error"){
      sub1 <- substring(res$status,8,8)
      if (sub1 == "U"){
        return("Error: Invalid username/password combination")
      }
    }
    return(res$status)
  } else if (length(res$result) == 0) {
    return("No jobs in job history")
  } else {
    for (i in 2:length(res$result)){
print(i)
      if ((res$result[[i]]$status == "STOPPED") || (res$result[[i]]$status == "FINISHED") || (res$result[[i]]$status == "ARCHIVING_FINISHED") || (res$result[[i]]$status == "FAILED")){

        if (rplant.env$api =="f"){web <- paste(rplant.foundation.env$webapps, "job/", res$result[[i]]$id, sep="")} else {paste(web, res$result[[i]]$id, sep="/")}
print(web)
        tryCatch(JS <- fromJSON(getForm(web,
                 .checkparams=FALSE, curl=curl.call)), 
                 error=function(x){return(JS <- data.frame(status=paste(x)))})
        if (length(JS$result$archivePath) !=0){ 
          dir.name <- unlist(strsplit(JS$result$archivePath, "/"))[length(unlist(strsplit(JS$result$archivePath, "/")))]

          dir.path <- substr(JS$result$archivePath, nchar(rplant.env$user) + 3, nchar(JS$result$archivePath)-nchar(dir.name)-1)

          dir.exist <- fromJSON(getURL(paste(web_check, dir.path, "/", dir.name, sep=""), curl=curl.call))

          if (length(dir.exist$result) != 0){
            tmp <- fromJSON(httpDELETE(paste(web_del, dir.path, "/", dir.name, sep=""), curl=curl.call))
          }
        }
        tmp <- fromJSON(httpDELETE(web, curl = curl.call))
      }
    }
  }
}

####################
####################
#### DeleteOne #####
####################
####################

DeleteOne <- function(job.id, print.curl=FALSE) {

  Time()
  Renew()
  if (rplant.env$api == "f") {
    web <- paste(rplant.foundation.env$webapps, "job/", job.id, sep="")
    web_check <- paste(rplant.foundation.env$webio, "io/list/", rplant.foundation.env$user, "/", sep="")
    web_del <- paste(rplant.foundation.env$webio, "io/", rplant.foundation.env$user, "/", sep="")
    first_string <- paste("curl -sku '", rplant.foundation.env$user, "'", sep="")
    curl.call <- rplant.foundation.env$curl.call
  } else {
    web <- paste(rplant.agave.env$webjobs, job.id, sep="")
    web_check <- paste(rplant.agave.env$webio, "listings/", rplant.agave.env$user, "/", sep="")
    web_del <- paste(rplant.agave.env$webio,"media/",rplant.agave.env$user,"/", sep="")
    first_string <- paste("curl -sk -H '", Renew(TRUE), "'", sep="")
    curl.call <- rplant.agave.env$curl.call
  }

  tryCatch(JS <- fromJSON(getForm(web, .checkparams=FALSE, curl=curl.call)), 
           error=function(x){return(JS <- data.frame(status=paste(x)))})

  if (JS$status != "success"){
    sub <- substring(JS$status,1,5)
    if (sub == "Error"){
      sub1 <- substring(JS$status,8,8)
      if (sub1 == "U"){
        return("Error: Invalid username/password combination")
      }
    }
    return(paste("Error: job: `",job.id,"' does not exist", sep=""))
  }

  if ((JS$result$status == "FINISHED") || (JS$result$status == "STOPPED") || (JS$result$status == "ARCHIVING_FINISHED") || (JS$result$status == "FAILED")){
    dir.name <- unlist(strsplit(JS$result$archivePath, "/"))[length(unlist(strsplit(JS$result$archivePath, "/")))]

    dir.path <- substr(JS$result$archivePath, nchar(rplant.env$user) + 3, nchar(JS$result$archivePath)-nchar(dir.name)-1)

    dir.exist <- fromJSON(getURL(paste(web_check, dir.path, "/", dir.name, sep=""), curl=curl.call))

    if (length(dir.exist$result) != 0){
        tmp <- fromJSON(httpDELETE(paste(web_del, dir.path, "/", dir.name, sep=""), curl=curl.call))
      }
      tmp <- fromJSON(httpDELETE(web, curl = curl.call))
  
  if (print.curl) {
    curl.string <- paste(first_string, "-X DELETE", web, sep="")
    print(curl.string)
  }

  } else {
    return(paste("Error: Could not delete, job status:", JS$result$status))
  }
}

####################
####################
#### DeleteJob #####
####################
####################

DeleteJob <- function(job.id, print.curl=FALSE, ALL=FALSE) {
  if (ALL==TRUE){
    DeleteALL()
    if (print.curl) {
      print("No curl statement to print")
    }
  } else {
    DeleteOne(job.id, print.curl)
  }
}

####################
####################
### RetrieveOne ####
####################
####################

RetrieveOne <- function(file, archive.path, file.path, print.curl) {  

  Time()
  Renew()
  if (rplant.env$api == "f") {
    web <- paste(rplant.foundation.env$webio, "io", archive.path, "/", file, sep="")
    first_string <- paste("curl -sku '", rplant.foundation.env$user, "'", sep="")
    curl.call <- rplant.foundation.env$curl.call
  } else {
    web <- paste(rplant.agave.env$webio,"media", archive.path, "/", file, sep="")
    first_string <- paste("curl -sk -H '", Renew(TRUE), "'", sep="")
    curl.call <- rplant.agave.env$curl.call
  }

  curlPerform(url=web, curl=curl.call, writedata = CFILE(file.path(file.path,file), mode="wrb")@ref)
  gc()

  if (print.curl) {
    curl.string <- paste(first_string, "-X GET", web)
    print(curl.string)
  }
}

####################
####################
### RetrieveJob ####
####################
####################

RetrieveJob <- function(job.id, file.vec, print.curl=FALSE, verbose=FALSE) {  

  Time()
  Renew()
  if (rplant.env$api == "f") {
    web <- paste(rplant.foundation.env$webapps, "job/", job.id, sep="")
    first_string <- paste("curl -sku '", rplant.foundation.env$user, "'", sep="")
    curl.call <- rplant.foundation.env$curl.call
  } else {
    web <- paste(rplant.agave.env$webjobs, job.id, sep="")
    first_string <- paste("curl -sk -H '", Renew(TRUE), "'", sep="")
    curl.call <- rplant.agave.env$curl.call
  }

  tryCatch(JS <- fromJSON(getForm(web, .checkparams=FALSE, curl=curl.call)), 
           error=function(x){return(JS <- data.frame(status=paste(x)))})

  if ((JS$status != "success") || (length(JS$result) == 0)) {
    sub <- substring(JS$status,1,5)
    if (sub == "Error"){
      sub1 <- sub1 <- substring(JS$status,8,8)
      if (sub1 == "U"){
        return("Error: Invalid username/password combination")
      }
    }
    return(paste("Error: job: `",job.id,"' does not exist", sep=""))
  } else {

    if ((JS$res$status == "ARCHIVING_FINISHED") || (JS$res$status == "FINISHED")) {

      dir.path <- file.path(getwd(), JS$result[[2]])

      if (.Platform$OS.type=="windows") {
        invisible(shell(paste("mkdir ", JS$result[[2]], sep="")))
      } else {
        dir.create(dir.path)
      }

      fileList <- ListJobOutput(job.id, print.total=FALSE)
      for (file in 1:length(file.vec)) {
        # if file exists in output then download
        if (file.vec[file] %in% fileList) {

          RetrieveOne(file.vec[file], JS$result$archivePath, dir.path, print.curl)

          if (verbose==TRUE) {
            print(paste("Downloaded", file.vec[file], "to", getwd(), "directory"))
          }
        } else {
          return(paste("`",file.vec[file], "' is not found within `", job.id,"'", sep=""))
        }
      }
    } else {
      warning("Job is ", JS$res$status)
    }
  }
}

####################
####################
## ListJobOutput ###
####################
####################

ListJobOutput <- function(job.id, print.curl=FALSE, print.total=TRUE) {

  Time()
  Renew()
  if (rplant.env$api == "f") {
    web_check <- paste(rplant.foundation.env$webapps, "job/", job.id, sep="")
    web <- paste(rplant.foundation.env$webio, "io/list", sep="")
    first_string <- paste("curl -sku '", rplant.foundation.env$user, "'", sep="")
    curl.call <- rplant.foundation.env$curl.call
  } else {
    web_check <- paste(rplant.agave.env$webjobs, job.id, sep="")
    web <- paste(rplant.agave.env$webio, "listings", sep="")
    first_string <- paste("curl -sk -H '", Renew(TRUE), "'", sep="")
    curl.call <- rplant.agave.env$curl.call
  }
 
  tryCatch(JS <- fromJSON(getForm(web_check, .checkparams=FALSE, curl=curl.call)), 
           error=function(x){return(JS <- data.frame(status=paste(x)))})

  if ((JS$status != "success") || (length(JS$result) == 0)) {
    sub <- substring(JS$status,1,5)
    if (sub == "Error"){
      sub1 <- sub1 <- substring(JS$status,8,8)
      if (sub1 == "U"){
        return("Error: Invalid username/password combination")
      }
    }
    return(paste("Error: job: `",job.id,"' does not exist", sep=""))
  } else {
    file.vec <- c()
    if ((JS$res$status == "FINISHED") || (JS$res$status == "ARCHIVING_FINISHED")) {

      web <- paste(web, JS$re$archivePath, sep="")
      res <- fromJSON(getURLContent(web, curl=curl.call))

      if (print.curl) {
        curl.string <- paste(first_string, "-X GET", web)
        print(curl.string)
      }

      len <- length(res$result)
      if (len == 0){
        return(paste("There are ", len, " output files for job '", job.id,"'", sep=""))
      }

      if (print.total == TRUE) {
        print(paste("There are ", len-1, " output files for job '", job.id,"'", sep=""))
      }
      for (i in 2:length(res$result)) {
        file.vec <- append(file.vec, res$result[[i]]$name)
      }
      return(file.vec)
    } else {
      warning("Job is ", JS$res$status)
    }
  } 
}

####################
####################
## GetJobHistory ###
####################
####################

GetJobHistory <- function(return.json=FALSE, print.curl=FALSE) {

  Time()
  Renew()
  if (rplant.env$api == "f") {
    web <- paste(rplant.foundation.env$webapps, "jobs/list", sep="")
    first_string <- paste("curl -sku '", rplant.foundation.env$user, "'", sep="")
    curl.call <- rplant.foundation.env$curl.call
    tmp_string <- "res$result[[i]]$software"
  } else {
    web <- rplant.agave.env$webjobs
    curl.call <- rplant.agave.env$curl.call
    first_string <- paste("curl -sk -H '", Renew(TRUE), "'", sep="")
    tmp_string <- "res$result[[i]]$appId"
  }

  jobList <- c()

  tryCatch(res <- fromJSON(getForm(web, .checkparams=FALSE, curl=curl.call)), 
           error=function(x){return(res <- data.frame(status=paste(x)))})

  if (print.curl) {
    curl.string <- paste(first_string, "-X GET", web)
    print(curl.string)
  }

  if (res$status != "success") {
    return("Error: Invalid username/password combination")
  } else if (length(res$result) == 0){
    return("No jobs in history")
  } else {
    if (return.json) 
      return(res)
    if (length(res$result) != 0) {
      for (i in 1: length(res$result)) {
        job <- c(res$result[[i]]$id, res$result[[i]]$name, 
                 eval(parse(text=tmp_string)), res$result[[i]]$status) 
        jobList <- rbind(jobList, job)
        colnames(jobList) <- c("job.id", "job.name", "application", "status")
      } 
      return(jobList)
    } else {
      return("No jobs in job history")
    }
  }
}

# -- END -- #
