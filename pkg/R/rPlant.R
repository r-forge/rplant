# Copyright (c) 2012, University of Tennessee
# rPlant directly interacts with iplant's command-line API for the 
# Discovery Environment (DE)

# Maybe add a CopyFile and a CopyDir function?

# -- AUTHENTICATION FUNCTIONS -- #

utils::globalVariables(c("rplant.env"))

#################
#################
### Validate ####
#################
#################

################# Maybe add time AND number of uses to the Validate function #################
Create_Keys <- function(user, pwd) {
  web <- "https://agave.iplantc.org/clients/v2"
  curl.call <- getCurlHandle(userpwd=paste(user,pwd, sep=":"), httpauth=1L, ssl.verifypeer=FALSE)
  res <- tryCatch(fromJSON(postForm(web, clientName = "rPlant", tier = "Unlimited", description = "", callbackUrl = "", style = "POST", curl = curl.call)), error=function(err){return(paste(err))})
  Error(res)
  return(list(res$result$consumerKey, res$result$consumerSecret))
}

Validate <- function(user, pwd, api="agave", print.curl=FALSE) {
  
  api <- match.arg(api, c("agave", "foundation"))

  if (api == "foundation"){

    web_BASE <- "https://foundation.iplantcollaborative.org/"
    web <- paste(web_BASE, "auth-v1/", sep="")
    curl.string <- paste("curl -sku '", user, "' ", web, sep="")
    curl.call <- getCurlHandle(userpwd=paste(user, pwd, sep=":"), httpauth=1L, ssl.verifypeer=FALSE)
    tryCatch(res <- fromJSON(getURL(web, curl=curl.call)), error=function(x){return(res <- data.frame(status=paste(x)))}) 

    if (res$status == "success"){
      assign("rplant.env", new.env(hash = TRUE), envir = .GlobalEnv)
      assign("api", "f", envir=rplant.env)
      assign("webio", paste(web_BASE, "io-v1/io/", user, sep=""), envir=rplant.env)
      assign("webio1", paste(web_BASE, "io-v1/io", sep=""), envir=rplant.env)
      assign("webcheck", paste(web_BASE, "io-v1/io/list/", user, sep=""), envir=rplant.env)
      assign("weblist", paste(web_BASE, "io-v1/io/list", sep=""), envir=rplant.env)
      assign("webshare", paste(web_BASE, "io-v1/io/share/", user, sep=""), envir=rplant.env)
      assign("webtransform", paste(web_BASE, "io-v1/data/transforms/", user, sep=""), envir=rplant.env)
      assign("webappslist", paste(web_BASE, "apps-v1/apps/list", sep=""), envir=rplant.env)
      assign("webappsname", paste(web_BASE, "apps-v1/apps/name", sep=""), envir=rplant.env)
      assign("webjob", paste(web_BASE, "apps-v1/job", sep=""), envir=rplant.env)
      assign("webjoblist", paste(web_BASE, "apps-v1/jobs/list", sep=""), envir=rplant.env)
      assign("webprofiles", paste(web_BASE, "profile-v1/profile/search/username/", user, sep=""), envir=rplant.env)
      assign("first", paste("curl -sku '", user, "'", sep=""), envir=rplant.env)
      assign("user", user, envir=rplant.env)
      assign("pwd", pwd, envir=rplant.env) 
      assign("curl.call", getCurlHandle(userpwd=paste(get("user", envir=rplant.env), get("pwd", envir=rplant.env), sep=":"), httpauth=1L, ssl.verifypeer=FALSE), envir=rplant.env)
    } else {
      return(res$message)
    }
  } else {
    keys <- list()
    keys <- Create_Keys(user,pwd)
    web_BASE <- "https://agave.iplantc.org/"
    content <- c()
    content[1] <- "grant_type=client_credentials"
    content[2] <- "scope=PRODUCTION"
    content[3] <- paste("username=", user, sep="")
    content[4] <- paste("password=", pwd, sep="")
    string <- paste(content, collapse = "&")

    val <- charToRaw(string)

    web <- paste(web_BASE, "token", sep="")

    curl.string <- paste("curl -sku '", keys[[1]], ":", keys[[2]],"' -X POST -d '", string, "' ", web, sep="")

    curl.call <- getCurlHandle(userpwd=paste(keys[[1]], keys[[2]], sep=":"), httpauth=1L, ssl.verifypeer=FALSE)
    expire <- as.POSIXlt(format(Sys.time(),"%Y-%m-%d %k:%M:%OS"))
    expire$hour=expire$hour+2
    tryCatch(res <- fromJSON(getURLContent(web, curl=curl.call, infilesize=length(val), readfunction=val, upload=TRUE, customrequest="POST")), error=function(x){return(res <- data.frame(status=paste(x)))})

    if (length(res) == 4){
      assign("rplant.env", new.env(hash = TRUE), envir = .GlobalEnv)
      assign("api", "a", envir=rplant.env)
      assign("consumer_key", keys[[1]], envir=rplant.env)
      assign("consumer_secret", keys[[2]], envir=rplant.env)
      assign("webio", paste(web_BASE, "files/v2/media/", user, sep=""), envir=rplant.env)
      assign("webio1", paste(web_BASE, "files/v2/media/", sep=""), envir=rplant.env)
      assign("webcheck", paste(web_BASE, "files/v2/listings/", user, sep=""), envir=rplant.env)
      assign("weblist", paste(web_BASE, "files/v2/listings", sep=""), envir=rplant.env)
      assign("webshare", paste(web_BASE, "files/v2/pems/", user, sep=""), envir=rplant.env)
      assign("webtransforms", paste(web_BASE, "transforms/v2/", sep=""), envir=rplant.env)
      assign("webappslist", paste(web_BASE, "apps/v2", sep=""), envir=rplant.env)
      assign("webappsname", paste(web_BASE, "apps/v2", sep=""), envir=rplant.env)
      assign("webjob", paste(web_BASE, "jobs/v2", sep=""), envir=rplant.env)
      assign("webjoblist", paste(web_BASE, "jobs/v2", sep=""), envir=rplant.env)
      assign("webprofiles", paste(web_BASE, "profiles/v2/search/username/", user, sep=""), envir=rplant.env)
      assign("webauth", paste(web_BASE, "token", sep=""), envir=rplant.env)
      assign("first", paste("curl -sk -H 'Authorization: Bearer ", res$access_token, "'", sep=""), envir=rplant.env)
      assign("user", user, envir=rplant.env)
      assign("pwd", pwd, envir=rplant.env) 
      assign("expire", expire, envir=rplant.env) 
      assign("access_token", res$access_token, envir=rplant.env)
      assign("refresh_token", res$refresh_token, envir=rplant.env)
      assign("curl.call", getCurlHandle(httpheader=c(paste("Authorization: Bearer ", get("access_token", envir=rplant.env), sep="")), httpauth=1L, ssl.verifypeer=FALSE), envir=rplant.env)
    } else {
      sub <- substring(res$status,1,5)
      if (length(sub) == 0){
        return(stop("Please Retry", call. = FALSE))
      } else if (sub == "error"){
        return(stop(res$message, call. = FALSE))
      } else {
        return(stop(res$status, call. = FALSE))
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

  if (rplant.env$api == "a"){
    content <- c()
    content[1] <- "grant_type=refresh_token"
    content[2] <- "scope=PRODUCTION"
    content[3] <- paste("refresh_token=", rplant.env$refresh_token, sep="")
    string <- paste(content, collapse = "&")
    val <- charToRaw(string)

    curl.call <- getCurlHandle(userpwd=paste(get("consumer_key", envir=rplant.env), get("consumer_secret", envir=rplant.env), sep=":"), httpauth=1L, ssl.verifypeer=FALSE)

    res <- tryCatch(fromJSON(getURLContent(rplant.env$webauth, curl=curl.call, infilesize=length(val), readfunction=val, upload=TRUE, customrequest="POST")), error = function(err) {return(paste(err))})
    Error(res)

    if (length(res) == 4){
      assign("access_token", res$access_token, envir=rplant.env)
      assign("refresh_token", res$refresh_token, envir=rplant.env) 
      assign("first", paste("curl -sk -H 'Authorization: Bearer ", res$access_token, "'", sep=""), envir=rplant.env)
      assign("curl.call", getCurlHandle(httpheader=c(paste("Authorization: Bearer ", get("access_token", envir=rplant.env), sep="")), httpauth=1L, ssl.verifypeer=FALSE), envir=rplant.env)
    }

    if (print.curl){
      curl.string <- paste("curl -sku ", rplant.env$consumer_key, ":", rplant.env$consumer_secret," -X POST -d '", string, "' ", rplant.env$webauth, sep="")
      print(curl.string)
    }
  }
}

#################
#################
##### Misc. #####
#################
#################

Renew <- function(ret=FALSE){
  if (rplant.env$api == "a") {
    assign("curl.call", getCurlHandle(httpheader=c(paste("Authorization: Bearer ", get("access_token", envir=rplant.env), sep="")), httpauth=1L, ssl.verifypeer=FALSE), envir=rplant.env)
  } else {
    assign("curl.call", getCurlHandle(userpwd=paste(get("user", envir=rplant.env), get("pwd", envir=rplant.env), sep=":"), httpauth=1L, ssl.verifypeer=FALSE), envir=rplant.env)
  }
}

Time <- function(){
  if (rplant.env$api != "f"){
    compare <- as.POSIXlt(format(Sys.time(),"%Y-%m-%d %k:%M:%OS"))
    if (compare > rplant.env$expire){
      expire <- as.POSIXlt(format(Sys.time(),"%Y-%m-%d %k:%M:%OS"))
      expire$hour=expire$hour+2
      assign("expire", expire, envir=rplant.env)
      RenewToken()
    }
  }
}

TestApp <- function(APP){

  if (rplant.env$api == "f"){
    first_string <- "res$result[[len]]"
      if (substring(APP,nchar(APP)-1,nchar(APP)-1) == "u"){
      priv.APP <- substring(APP,1,nchar(APP)-2)
    } else if (substring(APP,nchar(APP)-2,nchar(APP)-2) == "u"){
      priv.APP <- substring(APP,1,nchar(APP)-3)
    } else {
      priv.APP <- APP
    }
  } else {
    first_string <- "res$result"
    priv.APP <- APP
  }

  Renew()
  res <- fromJSON(getForm(paste(rplant.env$webappsname, priv.APP, sep="/"), .checkparams=FALSE, curl=rplant.env$curl.call))
  len <- length(res$result)

  if (length(res) == 0){
    return(list(NULL))
  } else {
    shortd <- eval(parse(text=paste(first_string, "$shortDescription", sep="")))
    shortn <- nchar(shortd)
    longd <- eval(parse(text=paste(first_string, "$longDescription", sep="")))
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
  if (length(ERR) == 1){
    sub1 <- substring(ERR,8,8)
    if (sub1 == "B"){
      return(stop("Bad Request", call. = FALSE))
    } else if (sub1 == "U"){
      return(stop("Invalid username/password combination", call. = FALSE))
    } else if ((sub1 == "F") || (sub1 == "N")){
      return(stop("file or directory or job id does not exist", call. = FALSE))
    } else {
      len <- nchar(ERR)
      return(stop(substring(ERR,8,len-3), call. = FALSE))
    }
  } else {
    for (i in 1:length(ERR)){
      if (names(ERR)[i] == "status"){
        if (ERR$status == "error"){
          return(stop(ERR$message, call. = FALSE))
        }
        break;
      }
    }
  }
}

appINFO <- function(application, dep=FALSE, input=FALSE){
  Time()
  Renew()

  if (rplant.env$api == "f"){
    tmp_string <- "tmp$result[[len]]"
    tmp_str <- "$public"
    if (substring(application,nchar(application)-1,nchar(application)-1) == "u"){
      priv.APP <- substring(application,1,nchar(application)-2)
    } else if (substring(application,nchar(application)-2,nchar(application)-2) == "u"){
      priv.APP <- substring(application,1,nchar(application)-3)
    } else {
      priv.APP <- application
    }
  } else {
    tmp_string <- "tmp$result"
    tmp_str <- "$isPublic"
    priv.APP <- application
  }
  
  if (substring(application,nchar(application)-1,nchar(application)-1) == "u"){
    version <- as.numeric(substring(application,nchar(application),nchar(application)))
    text <- "Public App"
  } else if (substring(application,nchar(application)-2,nchar(application)-2) == "u"){
    version <- as.numeric(paste(substring(application,nchar(application)-1,nchar(application)-1),substring(application,nchar(application),nchar(application)),sep=""))
    text <- "Public App"
  } else {
    text <- "Private App"
  }

  tmp <- tryCatch(fromJSON(getForm(paste(rplant.env$webappsname, priv.APP, sep="/"), .checkparams=FALSE, curl=rplant.env$curl.call)), error = function(err) {return(paste(err))})
  Error(tmp)

  len <- length(tmp$result)
  if (eval(parse(text=paste(tmp_string, tmp_str, sep=""))) == FALSE) {
    text <- "Private App"
  } else if (length(tmp) == 0) {
    return(stop("No information on application: not valid", call. = FALSE))
  }

  if (text == "Public App"){
    APP <- eval(parse(text=paste(tmp_string, "$id", sep="")))
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
    if (dep){
      if (substring(v.text, 1, 1) == "D"){
        return(stop(paste("Application deprecated, should be:", APP), call. = FALSE))
      }
    }
  }

  set <- eval(parse(text=paste(tmp_string, "$parallelism", sep="")))

  if (!input){
    if (text == "Private App"){
      return(list("Private App", priv.APP, tmp, set))
    } else {
      return(list("Public App", priv.APP, v.text, APP, tmp, set))
    }
  } else {
    app.info<-c()
    for (input in sequence(length(eval(parse(text=paste(tmp_string, "$inputs", sep="")))))) {
      app.info <- rbind(app.info, eval(parse(text=paste(tmp_string, "$inputs[[input]]$id", sep=""))))
    }
    if (text == "Private App"){
      return(list("Private App", priv.APP, tmp, app.info, set))
    } else {
      return(list("Public App", priv.APP, v.text, APP, tmp, app.info, set))
    }
  }
}

Check <- function(name, path="", suppress.Warnings=FALSE, shared.username=NULL, check=FALSE, dir=FALSE){
  if (suppress.Warnings == FALSE){
    Time()
    Renew()
    if (!dir){
      if (is.null(shared.username)){
      ### What about shared.username=NULL?
        dir.exist <- fromJSON(getURL(paste(rplant.env$webcheck, path, sep="/"), curl=rplant.env$curl.call)) 
        if (length(dir.exist$result) != 0){
          if (path==""){
            file.exist <- fromJSON(getURL(paste(rplant.env$webcheck, name, sep="/"), curl=rplant.env$curl.call))
          } else {
            file.exist <- fromJSON(getURL(paste(rplant.env$webcheck, path, name, sep="/"), curl=rplant.env$curl.call))
          }
        } else {
          if (dir.exist$status == "error"){
            if ((dir.exist$message == "File does not exist") || (dir.exist$message == "File/folder does not exist")){
              return(stop(paste("file.path '", path, "' not proper directory", sep=""), call. = FALSE))
            } else {
              return(stop("improper username/password combination", call. = FALSE))
            }
          } else {
            return(stop(paste("file.path '", path, "' not proper directory", sep=""), call. = FALSE))
          }
        }
      } else {
        if (path == ""){
          web <- paste(rplant.env$weblist, shared.username, name, sep="/")
        } else {
          web <- paste(rplant.env$weblist, shared.username, path, name, sep="/")
        }
        file.exist <- fromJSON(getURL(web, curl=rplant.env$curl.call))
      }
      if (check){
        if (length(file.exist$result) != 0){
          return(stop(paste("file '", name, "' already exists in '", path, "' directory", sep=""), call. = FALSE))
        }
      } else {
        if (length(file.exist$result) == 0){
          return(stop(paste("file '", name, "' doesn't exist in '", path, "' directory", sep=""), call. = FALSE))
        }
      }
    } else {
      if (is.null(shared.username)){
        web <- paste(rplant.env$weblist, rplant.env$user, sep="/")
        main.exist <- fromJSON(getURL(paste(web, path, sep="/"), curl=rplant.env$curl.call)) 
        if (length(main.exist$result) != 0){
          if (path==""){
            dir.exist <- fromJSON(getURL(paste(web, name, sep="/"), curl=rplant.env$curl.call))
          } else {
            dir.exist <- fromJSON(getURL(paste(web, path, name, sep="/"), curl=rplant.env$curl.call))
          }
        } else {
          if (dir.exist$status == "error"){
            if ((dir.exist$message == "File does not exist") || (dir.exist$message == "File/folder does not exist")){
              return(stop(paste("dir.path '", path, "' not proper directory", sep=""), call. = FALSE))
            } else {
              return(stop("improper username/password combination", call. = FALSE))
            }
          } else {
            return(stop(paste("dir.path '", path, "' not proper directory", sep=""), call. = FALSE))
          }
        }
      } else {
        if (path == ""){
          web <- paste(rplant.env$weblist, shared.username, name, sep="/")
        } else {
          web <- paste(rplant.env$weblist, shared.username, path, name, sep="/")
        }
        dir.exist <- fromJSON(getURL(web, curl=rplant.env$curl.call))
      }
      
      if (check){
        if (length(dir.exist$result) != 0){
          return(stop(paste("directory '", name, "' already exists in '", path, "' directory", sep=""), call. = FALSE))
        }
      } else {
        if (length(dir.exist$result) == 0){
          if (dir.exist$status == "error"){
            if ((dir.exist$message == "File does not exist") || (dir.exist$message == "File/folder does not exist")){
              return(stop(paste("directory '", name, "' doesn't exist in '", path, "' directory", sep=""), call. = FALSE))
            } else {
              return(stop("improper username/password combination", call. = FALSE))
            }
          } else {
            return(stop(paste("directory '", name, "' doesn't exist in '", path, "' directory", sep=""), call. = FALSE))
          }
        }
      }
    }
  }
}

Wait <- function(job.id, minWaitsec, maxWaitsec, print=FALSE){
  currentStatus= ''
  currentWait = minWaitsec
  while (( currentStatus != 'FAILED' ) && (currentStatus != 'ARCHIVING_FINISHED') && (currentStatus != 'FINISHED')) {
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
    message(paste("Job number: '", job.id, "' has status: ", currentStatus, sep=""))
  }
}

# -- END -- #




# -- MAIN FUNCTIONS -- #

#################
#################
#### Rename #####
#################
#################

Rename <- function(name, new.name, path="", print.curl=FALSE, suppress.Warnings=FALSE) {

  content <- c()
  if (rplant.env$api == "f") {
    content[1] <- "action=rename"
    content[2] <- paste("newName=", new.name, sep="")
  } else {
    if (path == ""){
      content[1] <- paste("path=", rplant.env$user, new.name, sep="/")
    } else {
      content[1] <- paste("path=", rplant.env$user, path, new.name, sep="/")
    }
    content[2] <- "action=move"
  }

  if (path == ""){
    web <- paste(rplant.env$webio, name, sep="/")
  } else {
    web <- paste(rplant.env$webio, path, name, sep="/")
  }

  if (print.curl){
    curl.string <- paste(rplant.env$first, " -X PUT -d '", paste(content, collapse = "&"), "' ", web, sep="")
    print(curl.string)
  }

  val <- charToRaw(paste(content, collapse = "&"))
  Renew()
  res <- tryCatch(fromJSON(httpPUT(web, content=val, curl=rplant.env$curl.call)), error = function(err) {return(paste(err))})
  if (!suppress.Warnings){Error(res)}
}


#################
#################
##### Move ######
#################
#################

Move <- function(name, org.path="", end.path="", print.curl=FALSE, suppress.Warnings=FALSE) {

  if (rplant.env$api == "f") {
    path <- "newPath="
  } else {
    path <- "path="
  }

  content <- c()
  if (end.path == ""){
    content[1] <- paste(path, rplant.env$user, name, sep="/")
  } else {
    content[1] <- paste(path, rplant.env$user, end.path, name, sep="/")
  }
  content[2] <- "action=move"

  if (org.path == ""){
    web <- paste(rplant.env$webio, name, sep="/")
  } else {
    web <- paste(rplant.env$webio, org.path, name, sep="/")
  }

  if (print.curl){
    curl.string <- paste(rplant.env$first, " -X PUT -d '", paste(content, collapse = "&"), "' ", rplant.env$webio, sep="")
    print(curl.string)
  }

  val <- charToRaw(paste(content, collapse = "&"))
  Renew()
  res <- tryCatch(fromJSON(httpPUT(web, content=val, curl=rplant.env$curl.call)), error = function(err) {return(paste(err))})
  if (!suppress.Warnings){Error(res)}
}

#################
#################
#### Delete #####
#################
#################

Delete <- function(name, path="", print.curl=FALSE, suppress.Warnings=FALSE) {

  if (path == "") {
    web <- paste(rplant.env$webio, name, sep="/")
  } else {
    web <- paste(rplant.env$webio, path, name, sep="/")
  }

  if (print.curl) {
    curl.string <- paste(rplant.env$first, " -X DELETE ", web, sep="")
    print(curl.string)
  }
  Renew()
  res <- tryCatch(fromJSON(httpDELETE(web, curl = rplant.env$curl.call)), error = function(err) {return(paste(err))})
  if (!suppress.Warnings){Error(res)}
}

#################
#################
##### Share #####
#################
#################

Share <- function(name, path="", shared.username, read=TRUE, execute=TRUE, write=TRUE, print.curl=FALSE, suppress.Warnings=FALSE, D=FALSE) {

  content <- c()
  if (rplant.env$api == "f") {
    if (read == TRUE) {content <- append(content, "can_read=true")} 
    if (execute == TRUE) {content <- append(content, "can_execute=true")}
    if (write == TRUE) {content <- append(content, "can_write=true")}
    if ((read == FALSE) && (execute == FALSE) && (write == FALSE)) {
      return(stop("Must select some permissions", call. = FALSE))
    }
  } else {
    if ((read == TRUE) && (execute == TRUE) && (write == TRUE)) {
      content[1] <- "permission=all"
    } else if ((read == TRUE) && (execute == TRUE) && (write == FALSE)) {
      content[1] <- "permission=read_execute"
    } else if ((read == TRUE) && (execute == FALSE) && (write == TRUE)) {
      content[1] <- "permission=read_write"
    } else if ((read == TRUE) && (execute == FALSE) && (write == FALSE)) {
      content[1] <- "permission=read"
    } else if ((read == FALSE) && (execute == TRUE) && (write == TRUE)) {
      content[1] <- "permission=write_execute"
    } else if ((read == FALSE) && (execute == TRUE) && (write == FALSE)) {
      content[1] <- "permission=execute"
    } else if ((read == FALSE) && (execute == FALSE) && (write == TRUE)) {
      content[1] <- "permission=write"
    } else {
      return(stop("Must select some permissions", call. = FALSE))
    }
  }

  content <- append(content,paste("username=", shared.username, sep=""))

  if(D){content <- append(content,"recursive=true")}

  if (path == "") {
    web <- paste(rplant.env$webshare, name, sep="/")
  } else {
    web <- paste(rplant.env$webshare, path, name, sep="/")
  }

  if (print.curl){
    curl.string <- paste(rplant.env$first," -X POST -d '", paste(content, collapse = "&"), "' ", web, sep="")
    print(curl.string)
  }

  val <- charToRaw(paste(content, collapse = "&"))
  Renew()
  res <- tryCatch(fromJSON(getURLContent(web, curl=rplant.env$curl.call,  infilesize=length(val), readfunction=val, upload=TRUE, customrequest="POST")), error = function(err) {return(paste(err))})
  if (!suppress.Warnings){Error(res)}
}

#####################
#####################
#### Permissions ####
#####################
#####################

Pems <- function(name, path="", print.curl=FALSE, suppress.Warnings=FALSE) {

  if (rplant.env$api == "f"){
    tmp_string <- "tmp$result$permissions"
  } else {
    tmp_string <- "tmp$result"
  }
    
  if (path == ""){
    web <- paste(rplant.env$webshare, name, sep="/")
  } else {
    web <- paste(rplant.env$webshare, path, name, sep="/")
  }

  if (print.curl){
    curl.string <- paste(rplant.env$first, web)
    print(curl.string)
  }

  Renew()
  tmp <- tryCatch(fromJSON(getURL(web, curl=rplant.env$curl.call)), error = function(err) {return(paste(err))})
  if (!suppress.Warnings){Error(tmp)}

  if (rplant.env$api == "a"){
    len <- length(eval(parse(text=tmp_string))) - 1
    first <- 2
  } else {
    len <- length(eval(parse(text=tmp_string))) - 12
    first <- 11
  }
  
  if (len == 0){ res <- matrix(, len + 1, 3) } else { res <- matrix(, len, 3) }
  colnames(res) <- c("Name", "Username", "Permissions")
  res[1, 1] <- name
  if (len == 0){
    res[1, 2] <- "None"
    res[1, 3] <- "None"
  } else {
    cnt = 1
    for (i in first:(first + len - 1)) {
      if (i != first){res[cnt,1] <- ""}
      res[cnt, 2] <- eval(parse(text=paste(tmp_string, "[[", i, "]]$username", sep="")))
      if (eval(parse(text=paste(tmp_string, "[[", i, "]]$permission$read", sep=""))) == TRUE) {
        R <- TRUE
      } else {
        R <- FALSE
      }
      if (eval(parse(text=paste(tmp_string, "[[", i, "]]$permission$write", sep=""))) == TRUE) {
        W <- TRUE        
      } else {
        W <- FALSE
      }
      if (eval(parse(text=paste(tmp_string, "[[", i, "]]$permission$execute", sep=""))) == TRUE) {
        E <- TRUE        
      } else {
        E <- FALSE
      }

      if ((R == TRUE) && (E == TRUE) && (W == TRUE)) {
        str <- "All"
      } else if ((R == TRUE) && (E == TRUE) && (W == FALSE)) {
        str <- "R/E"
      } else if ((R == TRUE) && (E == FALSE) && (W == TRUE)) {
        str <- "R/W"
      } else if ((R == TRUE) && (E == FALSE) && (W == FALSE)) {
        str <- "R"
      } else if ((R == FALSE) && (E == TRUE) && (W == TRUE)) {
        str <- "W/E"
      } else if ((R == FALSE) && (E == TRUE) && (W == FALSE)) {
        str <- "E"
      } else if ((R == FALSE) && (E == FALSE) && (W == TRUE)) {
        str <- "W"
      }

      res[cnt, 3] <- str
      cnt = cnt + 1
    }
  }
  return(res)
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

  if (rplant.env$api == "f") {
    options <- list(userpwd=paste(rplant.env$user, rplant.env$pwd, sep=":"), ssl.verifypeer=FALSE, httpauth=AUTH_BASIC, useragent="R", followlocation=TRUE)
  } else {
    options <- list(httpheader=c(paste("Authorization: Bearer ", rplant.env$access_token, sep="")), ssl.verifypeer=FALSE, httpauth=AUTH_BASIC, useragent="R", followlocation=TRUE)
  }

  Check(local.file.name, suppress.Warnings=suppress.Warnings, check=TRUE)

  if (!is.null(filetype)){
    res <- tryCatch(fromJSON(postForm(rplant.env$webio, style="httppost", fileToUpload=fileUpload(file.path), fileType=filetype, .opts=options)), error = function(err) {return(paste(err))})
    if (!suppress.Warnings){Error(res)}
    curl.string <- paste(rplant.env$first, " -F 'fileToUpload=@", file.path, "' -F 'fileType=", filetype, "' ", rplant.env$webio, sep="")
  } else {
    res <- tryCatch(fromJSON(postForm(rplant.env$webio, style="httppost", fileToUpload=fileUpload(file.path), .opts=options)), error = function(err) {return(paste(err))})
    if (!suppress.Warnings){Error(res)}
    curl.string <- paste(rplant.env$first," -F 'fileToUpload=@", file.path, "' ", rplant.env$webio, sep="")
  }

  if (print.curl==TRUE){
    print(curl.string)
  }
}

#################
#################
### ShareFile ###
#################
#################

ShareFile <- function(file.name, file.path="", shared.username, read=TRUE, execute=TRUE, write=TRUE, print.curl=FALSE, suppress.Warnings=FALSE) {
 
  Check(file.name, file.path, suppress.Warnings)

  Share(file.name, file.path, shared.username, read, execute, write, print.curl)
}

#####################
#####################
## PermissionsFile ##
#####################
#####################

PermissionsFile <- function(file.name, file.path="", print.curl=FALSE, suppress.Warnings=FALSE) {

    Check(file.name, file.path, suppress.Warnings)
    
    Pems(file.name, file.path, print.curl, suppress.Warnings)
}

#################
#################
## RenameFile ###
#################
#################

RenameFile <- function(file.name, new.file.name, file.path="", print.curl=FALSE, suppress.Warnings=FALSE) {

  Check(file.name, file.path, suppress.Warnings)

  Check(new.file.name, file.path, suppress.Warnings, check=TRUE)

  Rename(file.name, new.file.name, file.path, print.curl) 
}

#################
#################
### MoveFile ####
#################
#################

MoveFile <- function(file.name, file.path="", end.path="", print.curl=FALSE, suppress.Warnings=FALSE) {

  Check(file.name, file.path, suppress.Warnings)

  Check(file.name, end.path, suppress.Warnings, check=TRUE)

  Move(file.name, file.path, end.path, print.curl)
}

#################
#################
## DeleteFile ###
#################
#################

DeleteFile <- function(file.name, file.path="", print.curl=FALSE, suppress.Warnings=FALSE) {

  Check(file.name, file.path, suppress.Warnings)

  Delete(file.name, file.path, print.curl)
}

#################
#################
## SupportFile ##
#################
#################

SupportFile <- function(print.curl=FALSE, suppress.Warnings=FALSE) {  

  Time()
  Renew()
  res <- tryCatch(fromJSON(getForm(rplant.env$webtransforms, .checkparams=FALSE, curl=rplant.env$curl.call)), error = function(err) {return(paste(err))})
  if (!suppress.Warnings){Error(res)}

  if (print.curl) {
    curl.string <- paste(rplant.env$first, "-X GET", rplant.env$webtransforms)
    print(curl.string)
  }

  file.types <- c()
  for(i in 1:length(res$result)) {
    file.types <- c(file.types, res$result[[i]]$name)
  }
  return(file.types)
}

# -- END -- #




# -- DIRECTORY FUNCTIONS -- #

#################
#################
#### ListDir ####
#################
#################

ListDir <- function(dir.name, dir.path="", print.curl=FALSE, shared.username=NULL, suppress.Warnings=FALSE) {

  if (is.null(shared.username)){
    web <- paste(rplant.env$weblist, rplant.env$user, sep="/")
  } else {
    web <- paste(rplant.env$weblist, shared.username, sep="/")
  }

  if (dir.path == ""){
    web <- paste(web, dir.name, sep="/")
  } else {
    web <- paste(web, dir.path, dir.name, sep="/")
  }

  Check(dir.name, dir.path, suppress.Warnings, shared.username, dir=TRUE) 

  if (print.curl){
    curl.string <- paste(rplant.env$first, " ", web, sep="")
    print(curl.string)
  }
  Renew()
  tmp <- tryCatch(fromJSON(getURL(web, curl=rplant.env$curl.call)), error = function(err) {return(paste(err))})
  if (!suppress.Warnings){Error(tmp)}

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

  Check(dir.name, dir.path, suppress.Warnings, dir=TRUE)

  Share(dir.name, dir.path, shared.username, read, execute, write, print.curl, TRUE)
}

#####################
#####################
## PermissionsDir ###
#####################
#####################

PermissionsDir <- function(dir.name, dir.path="", print.curl=FALSE, suppress.Warnings=FALSE) {

    Check(dir.name, dir.path, suppress.Warnings, dir=TRUE)
    
    Pems(dir.name, dir.path, print.curl, suppress.Warnings)
}

#################
#################
### RenameDir ###
#################
#################

RenameDir <- function(dir.name, new.dir.name, dir.path="", print.curl=FALSE, suppress.Warnings=FALSE) {
  Check(dir.name, dir.path, suppress.Warnings, dir=TRUE)

  Check(new.dir.name, dir.path, suppress.Warnings, check=TRUE, dir=TRUE)

  Rename(dir.name, new.dir.name, dir.path, print.curl) 
}

#################
#################
#### MoveDir ####
#################
#################

MoveDir <- function(dir.name, dir.path="", end.path="", print.curl=FALSE, suppress.Warnings=FALSE) {

  Check(dir.name, dir.path, suppress.Warnings, dir=TRUE)

  Check(dir.name, end.path, suppress.Warnings, check=TRUE, dir=TRUE)

  Move(dir.name, dir.path, end.path, print.curl)
}

#################
#################
### DeleteDir ###
#################
#################

DeleteDir <- function(dir.name, dir.path="", print.curl=FALSE, suppress.Warnings=FALSE) {

  Check(dir.name, dir.path, suppress.Warnings, dir=TRUE)

  Delete(dir.name, dir.path, print.curl)
}

#################
#################
#### MakeDir ####
#################
#################

MakeDir <- function(dir.name, dir.path="", print.curl=FALSE, suppress.Warnings=FALSE) {

  content <- c()
  if (rplant.env$api == "f") {
    content[1] <- paste("dirName=", dir.name, sep="")
    if (dir.path==""){
      web <- rplant.env$webio
    } else {
      web <- paste(rplant.env$webio, dir.path, sep="/")
    }
  } else {
    web <- rplant.env$webio
    if (dir.path==""){
      content[1] <- paste("path=", dir.name, sep="") 
    } else {
      content[1] <- paste("path=", dir.path, "/", dir.name, sep="") 
    }
  }

  Check(dir.name, dir.path, suppress.Warnings, check=TRUE, dir=TRUE)

  content[2] <- "action=mkdir"

  if (print.curl) {
    curl.string <- paste(rplant.env$first, " -d '", paste(content, collapse = "&"), "' ", web, sep="")
    print(curl.string)
  }

  val <- charToRaw(paste(content, collapse = "&"))
  Renew()
  res <- tryCatch(fromJSON(httpPUT(web, content=val, curl=rplant.env$curl.call)), error = function(err) {return(paste(err))})
  if (!suppress.Warnings){Error(res)}
}

# -- END -- #




# -- APPLICATION FUNCTIONS -- #

#################
#################
### ListApps ####
#################
#################

ListApps<- function (description=FALSE, print.curl=FALSE, suppress.Warnings=FALSE) 
{

  Time()
  Renew()
  tmp <- tryCatch(fromJSON(getForm(rplant.env$webappslist, .checkparams=FALSE, curl=rplant.env$curl.call)), error = function(err) {return(paste(err))})
  if (!suppress.Warnings){Error(tmp)}

  if (print.curl) {
    curl.string <- paste(rplant.env$first, "-X GET", rplant.env$webappslist)
    print(curl.string)
  }

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

#################
#################
## GetAppInfo ###
#################
#################

GetAppInfo <- function(application, return.json=FALSE, print.curl=FALSE) {

  if (rplant.env$api == "f"){
    tmp_string <- "tmp$result[[len]]"
  } else {
    tmp_string <- "tmp$result"
  }

  result <- appINFO(application)
  text <- result[[1]]
  priv.APP <- result[[2]]

  if (text == "Public App"){
    v.text <- result[[3]]
    APP <- result[[4]]
    tmp <- result[[5]]
  } else {
    tmp <- result[[3]]
  }

  if (print.curl) {
    curl.string <- paste(rplant.env$first, " -X GET ", rplant.env$webappsname, "/", priv.APP, sep="")
    print(curl.string)
  }
  
  if (return.json) {
    return(tmp)
  } else {
    app.info<-c()
    len <- length(tmp$result)
    for (input in sequence(length(eval(parse(text=paste(tmp_string, "$inputs", sep="")))))) {
      app.info <- rbind(app.info, c("input", eval(parse(text=paste(tmp_string, "$inputs[[input]]$id", sep=""))), eval(parse(text=paste(tmp_string, "$inputs[[input]]$semantics$fileTypes[1]", sep=""))), eval(parse(text=paste(tmp_string, "$inputs[[input]]$details$label", sep="")))))
    }

    for (output in sequence(length(eval(parse(text=paste(tmp_string, "$outputs", sep="")))))) {
      app.info <- rbind(app.info, c("output", eval(parse(text=paste(tmp_string, "$outputs[[output]]$id", sep=""))), eval(parse(text=paste(tmp_string, "$outputs[[output]]$semantics$fileTypes[1]", sep=""))), eval(parse(text=paste(tmp_string, "$outputs[[output]]$details$label", sep=""))))) 
    }

    for (parameter in sequence(length(eval(parse(text=paste(tmp_string, "$parameters", sep="")))))) {
      app.info <- rbind(app.info, c("output", eval(parse(text=paste(tmp_string, "$parameters[[parameter]]$id", sep=""))), eval(parse(text=paste(tmp_string, "$parameters[[parameter]]$value$type", sep=""))), eval(parse(text=paste(tmp_string, "$parameters[[parameter]]$details$label", sep=""))))) 
    }
    shortd <- eval(parse(text=paste(tmp_string, "$shortDescription", sep="")))
    shortn <- nchar(shortd)
    longd <- eval(parse(text=paste(tmp_string, "$longDescription", sep="")))
    if (is.null(longd)) {longn = 0} else {longn <- nchar(longd)}
    if (longn >= shortn) {
      description <- longd
    } else {
      description <- shortd
    }
    colnames(app.info)<-c("kind", "id", "fileType/value", "details")
    if (text == "Private App"){
      return(list(Description=description, Application=c(application, text), Information=app.info))
    } else {
      return(list(Description=description, Application=c(application, text, v.text), Information=app.info))
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
    tmp_string <- "tmp$result[[len]]"
    eml_string <- "callbackUrl="
    content[1] <- paste("jobName=", job.name, sep="")
    content[2] <- paste("softwareName=", application, sep="")
    content[3] <- paste("processorCount=", nprocs, sep="")
    content[4] <- "requestedTime=24:00:00"
  } else {
    tmp_string <- "tmp$result"
    eml_string <- "callbackURL="
    content[1] <- paste("name=", job.name, sep="")
    content[2] <- paste("appId=", application, sep="")
    content[3] <- paste("nodeCount=", nprocs, sep="")
    content[4] <- "maxRunTime=24:00:00"
  }

  for (i in 1:length(file.list)){
    Check(file.list[[i]], file.path, suppress.Warnings, shared.username)
  }

  if (suppress.Warnings == FALSE){

    result <- appINFO(application, FALSE, TRUE)

    if (result[[1]] == "Public App"){
      input <- result[[6]]
      set <- result[[7]]
    } else {
      input <- result[[4]]
      set <- result[[5]]
    }

    test.input <- rep(0, length(input.list))
    for (j in 1:length(input.list)){
      for (i in 1:length(input)){
        if (input.list[[j]] != input[i]){
          test.input[j] <- 1
          break;
        }
      }
    }

    if (sum(test.input) > 0 ){
      return(stop("At least one of the inputs in 'input.list' is incorrect, check GetAppInfo function for proper inputs", call. = FALSE))
    }
  }

  if (suppress.Warnings==TRUE){
    result <- appINFO(application)
    if (result[[1]] == "Public App"){
      set <- result[[6]]
    } else {
      set <- result[[4]]
    }
  }

  if (private.APP==FALSE){
    if (result[[1]] == "Private App"){
      return(stop("Private application, not valid for SubmitJob.  If it is your own private application use private.APP=TRUE", call. = FALSE))
    }
  }

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

  if(!is.null(args.list)){m <- length(args.list)} else {m <- 0}

  if(!is.null(file.list)){n <- length(file.list)} else {n <- 0}

  # Automatically make analyses directory; will not overwrite if already present
  MakeDir("analyses", suppress.Warnings=TRUE)

  content[5] <- "archive=1"
  content[6] <- paste("archivePath=", rplant.env$user, "analyses", job.name, sep="/"); x <- 6;

  if (email==TRUE){
    Renew()
    res <- tryCatch(fromJSON(getURLContent(rplant.env$webprofiles, curl=rplant.env$curl.call)), error = function(err) {return(paste(err))})
    Error(res)
    content[7] <- paste(eml_string, res$result[[1]]$email, sep=""); x <- 7;
  }

  if (n > 0){
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

  if (m > 0){
    for (i in c(1:m)){
      content[x+n+i] <- paste(args.list[[i]][1],"=", 
                               args.list[[i]][2], sep="")
    }
  }

  if (print.curl) {
    curl.string <- paste(rplant.env$first," -X POST -d '", paste(content, collapse = "&"), "' ", rplant.env$webjob, sep="")
    print(curl.string)
  }

  val <- charToRaw(paste(content, collapse = "&"))
  Renew()
  res <- tryCatch(fromJSON(getURLContent(rplant.env$webjob, curl=rplant.env$curl.call,  infilesize=length(val), readfunction=val, upload=TRUE, customrequest="POST")), error = function(err) {return(paste(err))})
  if (!suppress.Warnings){Error(res)}

  cat(paste("Job submitted. You can check your job using CheckJobStatus(", 
      res$result$id, ")", sep=""), "\n")
  return(res$result$id)
  # return(list(res$result$id, job.name))
}

####################
####################
## CheckJobStatus ##
####################
####################

CheckJobStatus <- function(job.id, history=FALSE, print.curl=FALSE) {

  Time()
  Renew()

  web <- paste(rplant.env$webjob, job.id, sep="/")

  if (!(((rplant.env$api == "f") && (history == TRUE)) || (history == FALSE))){
     web <- paste(web, "history", sep="")
  }

  res <- tryCatch(fromJSON(getForm(web, .checkparams=FALSE, curl=rplant.env$curl.call)), error = function(err) {return(paste(err))})
  Error(res)

  if (print.curl) {
    curl.string <- paste(rplant.env$first, web)
    print(curl.string)
  }

  if (!(((rplant.env$api == "f") && (history == TRUE)) || (history == FALSE))){
    return(res$result)
  } else { 
    return(res$result$status)
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

  web <- paste(rplant.env$webjob, job.id, sep="/")

  content <- c()
  content[1] <- "action=stop"

  val <- charToRaw(paste(content, collapse = "&"))

  res <- tryCatch(fromJSON(getURLContent(web, curl=rplant.env$curl.call,  infilesize=length(val), readfunction=val, upload=TRUE, customrequest="POST")), error = function(err) {return(paste(err))})
  Error(res)

  if (print.curl) {
    curl.string <- paste(rplant.env$first, " -X POST -d '", paste(content, collapse = "&"), "' ",  web, sep="")
    print(curl.string)
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

  res <- tryCatch(fromJSON(getForm(rplant.env$webjoblist, .checkparams=FALSE, curl=rplant.env$curl.call)), error = function(err) {return(paste(err))})
  Error(res)

  if (length(res$result) == 0) {
    return(stop("No jobs in job history", call. = FALSE))
  } else {
    for (i in 1:length(res$result)){
      DeleteOne(res$result[[i]]$id)
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

  web <- paste(rplant.env$webjob, job.id, sep="/")

  JS <- tryCatch(fromJSON(getForm(web, .checkparams=FALSE, curl=rplant.env$curl.call)), error = function(err) {return(paste(err))})
  Error(JS)

  if ((JS$result$status == "FINISHED") || (JS$result$status == "STOPPED") || (JS$result$status == "ARCHIVING_FINISHED") || (JS$result$status == "FAILED")){
    dir.name <- unlist(strsplit(JS$result$archivePath, "/"))[length(unlist(strsplit(JS$result$archivePath, "/")))]

    dir.path <- substr(JS$result$archivePath, nchar(rplant.env$user) + 3, nchar(JS$result$archivePath)-nchar(dir.name)-1)

    Check(dir.name, dir.path, dir=TRUE)

    tmp <- tryCatch(fromJSON(httpDELETE(paste(rplant.env$webio, dir.path, dir.name, sep="/"), curl=rplant.env$curl.call)), error = function(err) {return(paste(err))})
    Error(tmp)

    tmp <- tryCatch(fromJSON(httpDELETE(web, curl = rplant.env$curl.call)), error = function(err) {return(paste(err))})
    Error(tmp)
  
    if (print.curl) {
      curl.string <- paste(rplant.env$first, "-X DELETE", web)
      print(curl.string)
    }

  } else {
    return(stop(paste("Error: Could not delete, job status:", JS$result$status), call. = FALSE))
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
      message("No curl statement to print")
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

  web <- paste(rplant.env$webio1, archive.path, "/", file, sep="")

  curlPerform(url=web, curl=rplant.env$curl.call, writedata = CFILE(file.path(file.path,file), mode="wrb")@ref)
  gc()

  if (print.curl) {
    curl.string <- paste(rplant.env$first, "-X GET", web)
    print(curl.string)
  }
}

####################
####################
### RetrieveJob ####
####################
####################

RetrieveJob <- function(job.id, file.vec=NULL, print.curl=FALSE, verbose=FALSE) {  

  Time()
  Renew()

  web <- paste(rplant.env$webjob, job.id, sep="/")

  JS <- tryCatch(fromJSON(getForm(web, .checkparams=FALSE, curl=rplant.env$curl.call)), error = function(err) {return(paste(err))})
  Error(JS)

  if ((JS$res$status == "ARCHIVING_FINISHED") || (JS$res$status == "FINISHED")) {

    dir.path <- file.path(getwd(), JS$result[[2]])
    if(!file.exists(dir.path)){
      if (.Platform$OS.type=="windows") {
        invisible(shell(paste("mkdir ", JS$result[[2]], sep="")))
      } else {
        dir.create(dir.path)
      }
    }
  if(is.null(file.vec)){
    file.vec <- ListJobOutput(job.id)
  }  
    fileList <- ListJobOutput(job.id, print.total=FALSE)
    for (file in 1:length(file.vec)) {
      # if file exists in output then download
      if (file.vec[file] %in% fileList) {

        RetrieveOne(file.vec[file], JS$result$archivePath, dir.path, print.curl)

        if (verbose==TRUE) {
          message(paste("Downloaded", file.vec[file], "to", dir.path))
        }
      } else {
        return(stop(paste("`",file.vec[file], "' is not found within `", job.id,"'", sep=""), call. = FALSE))
      }
    }
  } else {
    return(stop(paste("Job is", JS$res$status), call. = FALSE))
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

  JS <- tryCatch(fromJSON(getForm(paste(rplant.env$webjob, job.id, sep="/"), .checkparams=FALSE, curl=rplant.env$curl.call)), error = function(err) {return(paste(err))})
  Error(JS)

  file.vec <- c()
  if ((JS$res$status == "FINISHED") || (JS$res$status == "ARCHIVING_FINISHED")) {

    web <- paste(rplant.env$weblist, JS$result$archivePath, sep="")
    res <- fromJSON(getURLContent(web, curl=rplant.env$curl.call))

    if (print.curl) {
      curl.string <- paste(rplant.env$first, "-X GET", web)
      print(curl.string)
    }

    len <- length(res$result)
    if (len == 0){
      return(paste("There are ", len, " output files for job '", job.id,"'", sep=""))
    }

    if (print.total == TRUE) {
      message(paste("There are ", len-1, " output files for job '", job.id,"'", sep=""))
    }
    for (i in 2:length(res$result)) {
      file.vec <- append(file.vec, res$result[[i]]$name)
    }
    return(file.vec)
  } else {
    return(stop(paste("Job is", JS$res$status), call. = FALSE))
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
    tmp_string <- "res$result[[i]]$software"
  } else {
    tmp_string <- "res$result[[i]]$appId"
  }

  jobList <- c()

  res <- tryCatch(fromJSON(getForm(rplant.env$webjoblist, .checkparams=FALSE, curl=rplant.env$curl.call)), error = function(err) {return(paste(err))})
  Error(res)

  if (print.curl) {
    curl.string <- paste(rplant.env$first, "-X GET", rplant.env$webjoblist)
    print(curl.string)
  }

  if (length(res$result) == 0){
    return("No jobs in history")
  } else {
    if (return.json) 
      return(res)

    for (i in 1: length(res$result)) {
      job <- c(res$result[[i]]$id, res$result[[i]]$name, 
               eval(parse(text=tmp_string)), res$result[[i]]$status) 
      jobList <- rbind(jobList, job)
      colnames(jobList) <- c("job.id", "job.name", "application", "status")
    } 
    return(jobList)
  }
}

# -- END -- #
