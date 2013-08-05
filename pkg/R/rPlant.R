# Copyright (c) 2012, University of Tennessee
# rPlant directly interacts with iplant's command-line API for the 
# Discovery Environment (DE)

# -- AUTHENTICATION FUNCTIONS -- #

TestApp <- function(APP){
  web <- paste(rplant.env$webapps, "apps/name", sep="")
  if (substring(APP,nchar(APP)-1,nchar(APP)-1) == "u"){
    priv.APP <- substring(APP,1,nchar(APP)-2)
  } else if (substring(APP,nchar(APP)-2,nchar(APP)-2) == "u"){
    priv.APP <- substring(APP,1,nchar(APP)-3)
  } else {
    priv.APP <- APP
  }
  Renew()
  res <- fromJSON(getForm(paste(web, priv.APP, sep="/"), .checkparams=FALSE, curl=rplant.env$curl.call))
  if ((res$result[[1]]$public == FALSE) || (length(res) == 0)){
    return(NULL)
  } else {
    return(c(res$result[[1]]$id))
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

Renew <- function(){
  assign("curl.call", getCurlHandle(userpwd=paste(rplant.env$user, 
         rplant.env$pwd, sep=":"), httpauth=1L, ssl.verifypeer=FALSE),
         envir=rplant.env)
}

Validate <- function(user, pwd, api="iplant", print.curl=FALSE) {
  
  api <- match.arg(api, c("iplant", "cipres", "tnrs"))

  curl.call <- getCurlHandle(userpwd=paste(user, pwd, sep=":"), httpauth=1L, ssl.verifypeer=FALSE)

  res <- fromJSON(getURL("https://foundation.iplantc.org/auth-v1/", curl=curl.call)) 

  if (print.curl){
    curl.string <- paste("curl -X '", rplant.env$user, "' ", rplant.env$webauth, sep="")
 
    print(curl.string)
  }

  if (res$status == "success"){
    assign("rplant.env", new.env(hash = TRUE), envir = .GlobalEnv)
    assign("webio", "https://foundation.iplantc.org/io-v1/", envir=rplant.env)
    assign("webapps", "https://foundation.iplantc.org/apps-v1/", envir=rplant.env)
    assign("user", user, envir=rplant.env)
    assign("pwd", pwd, envir=rplant.env) 
  } else {
    return(res$message)
  }
}
# -- END -- #


# -- FILE AND DATA FUNCTIONS -- #
UploadFile <- function(local.file.name, local.file.path="", file.type,
                       print.curl=FALSE) {

  web <- paste(rplant.env$webio, "io/", rplant.env$user, sep="")

  if (print.curl) {
    curl.string <- paste("curl -sku '", rplant.env$user, "' -F 'fileToUpload=@",
                         local.file.path, local.file.name, "' -F 'fileType=",
                         file.type, "' ", web, sep="")

    print(curl.string)
  }

  if (local.file.path == "") {
    tryCatch(res <<- fromJSON(postForm(web, style="httppost",
                              fileToUpload=fileUpload(local.file.name), 
                              fileType=file.type, 
                              .opts=list(userpwd=paste(rplant.env$user, 
                                                       rplant.env$pwd, sep=":"),
                              ssl.verifypeer=FALSE, httpauth=AUTH_BASIC,
                              useragent="R", followlocation=TRUE))),
              error=function(x){return(res <<- data.frame(status=paste(x)))})
  } else {
    tryCatch(res <<- fromJSON(postForm(web, style="httppost",
                              fileToUpload=fileUpload(paste(local.file.path, 
                              local.file.name, sep="/")), fileType=file.type,
                              .opts=list(userpwd=paste(rplant.env$user, 
                                                       rplant.env$pwd, sep=":"),
                              ssl.verifypeer=FALSE, httpauth=AUTH_BASIC, 
                              useragent="R", followlocation=TRUE))),
              error=function(x){return(res <<- data.frame(status=paste(x)))})
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

RenameFile <- function(file.name, new.file.name, file.path="", print.curl=FALSE, suppress.Warnings=FALSE) {
  web <- paste(rplant.env$webio, "io", sep="")

  if (print.curl){
    curl.string <- paste("curl -sku '", rplant.env$user, "' -X PUT -d 'newName=",
                         new.file.name, "&action=rename", "' ", web, "/", 
                         rplant.env$user, "/", file.path, "/", file.name, sep="")

    print(curl.string)
  }

  if (suppress.Warnings == FALSE){
    Renew()

    dir.exist <- fromJSON(getURL(paste(rplant.env$webio, "io/list/", rplant.env$user, "/", file.path, sep=""), curl=rplant.env$curl.call)) 

    if (length(dir.exist$result) != 0){
      if (file.path==""){
        file.exist <- fromJSON(getURL(paste(rplant.env$webio, "io/list/", rplant.env$user, "/", file.name, sep=""), curl=rplant.env$curl.call))
        new.file.exist <- fromJSON(getURL(paste(rplant.env$webio, "io/list/", rplant.env$user, "/", new.file.name, sep=""), curl=rplant.env$curl.call))    
      } else {
        file.exist <- fromJSON(getURL(paste(rplant.env$webio, "io/list/", rplant.env$user, "/", file.path, "/", file.name, sep=""), curl=rplant.env$curl.call))
        new.file.exist <- fromJSON(getURL(paste(rplant.env$webio, "io/list/", rplant.env$user, "/", file.path, "/", new.file.name, sep=""), curl=rplant.env$curl.call)) 
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

  Renew()

  content <- c()
  content[1] <- "action=rename"
  content[2] <- paste("newName=", new.file.name, sep="")
  val <- charToRaw(paste(content, collapse = "&"))
  if (file.path == "") {
    res <- fromJSON(httpPUT(paste(web, rplant.env$user, file.name, 
                     sep="/"), content=val, curl=rplant.env$curl.call))
  } else {
    res <- fromJSON(httpPUT(paste(web, rplant.env$user, file.path, 
                     file.name, sep="/"), content=val, curl=rplant.env$curl.call))
  }
}

MoveFile <- function(file.name, file.path="", end.path="", print.curl=FALSE, suppress.Warnings=FALSE) {
  web <- paste(rplant.env$webio, "io", sep="")

  if (print.curl){
    curl.string <- paste("curl -sku '", rplant.env$user, "' -X PUT -d 'newPath=",
                         rplant.env$user, "/", end.path, "/", file.name,
                         "&action=move", "' ", web, "/", rplant.env$user, "/",
                         file.path, "/", file.name, sep="")

    print(curl.string)
  }

  if (suppress.Warnings == FALSE){
    Renew()

    dir.exist <- fromJSON(getURL(paste(rplant.env$webio, "io/list/", rplant.env$user, "/", file.path, sep=""), curl=rplant.env$curl.call)) 

    if (length(dir.exist$result) != 0){

      if (file.path==""){
        file.exist <- fromJSON(getURL(paste(rplant.env$webio, "io/list/", rplant.env$user, "/", file.name, sep=""), curl=rplant.env$curl.call)) 
      } else {
        file.exist <- fromJSON(getURL(paste(rplant.env$webio, "io/list/", rplant.env$user, "/", file.path, "/", file.name, sep=""), curl=rplant.env$curl.call))
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

    dir2.exist <- fromJSON(getURL(paste(rplant.env$webio, "io/list/", rplant.env$user, "/", end.path, sep=""), curl=rplant.env$curl.call)) 

    if (length(dir2.exist$result) != 0){

      if (end.path==""){
        file2.exist <- fromJSON(getURL(paste(rplant.env$webio, "io/list/", rplant.env$user, "/", file.name, sep=""), curl=rplant.env$curl.call)) 
      } else {
        file2.exist <- fromJSON(getURL(paste(rplant.env$webio, "io/list/", rplant.env$user, "/", end.path, "/", file.name, sep=""), curl=rplant.env$curl.call))
      }
    } else {
      return("Error: `end.path' not proper directory")
    }

    if (length(file2.exist$result) != 0){
      return(paste("Error: Cannot move file because `", file.name,"' is already in the directory `", end.path, "'", sep=""))
    }
  }
 
  content <- c()
  content[1] <- "action=move"
  content[2] <- paste("newPath=", rplant.env$user, "/", end.path, "/", 
                      file.name, sep="")
  val <- charToRaw(paste(content, collapse = "&"))

  if (file.path == "") {
    res <- fromJSON(httpPUT(paste(web, rplant.env$user, file.name, 
             sep="/"), content=val, curl=rplant.env$curl.call))
  } else {
    res <- fromJSON(httpPUT(paste(web, rplant.env$user, file.path,
             file.name, sep="/"), content=val, curl=rplant.env$curl.call))
  }
}

DeleteFile <- function(file.name, file.path="", print.curl=FALSE, suppress.Warnings=FALSE) {
  web <- paste(rplant.env$webio, "io", sep="")

  if (print.curl) {
    curl.string <- paste("curl -sku '", rplant.env$user, "' -X DELETE", " ", 
                         web, "/", rplant.env$user,  "/", file.path, "/", 
                         file.name, "/", sep="")

    print(curl.string)
  }

  if (suppress.Warnings == FALSE){
    Renew()

    dir.exist <- fromJSON(getURL(paste(rplant.env$webio, "io/list/", rplant.env$user, "/", file.path, sep=""), curl=rplant.env$curl.call)) 

    if (length(dir.exist$result) != 0){

      if (file.path==""){
      file.exist <- fromJSON(getURL(paste(rplant.env$webio, "io/list/", rplant.env$user, "/", file.name, sep=""), curl=rplant.env$curl.call)) 
      } else {
        file.exist <- fromJSON(getURL(paste(rplant.env$webio, "io/list/", rplant.env$user, "/", file.path, "/", file.name, sep=""), curl=rplant.env$curl.call))
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
    res <- fromJSON(httpDELETE(paste(web, rplant.env$user, 
                    file.name, sep="/"), curl = rplant.env$curl.call))
  } else {
    res <- fromJSON(httpDELETE(paste(web, rplant.env$user, 
                    file.path, file.name, sep="/"), curl=rplant.env$curl.call))
  }
}

SupportFile <- function(print.curl=FALSE) {  
  web <- paste(rplant.env$webio, "data/transforms/", sep="")

  if (print.curl) {
    curl.string <- paste("curl -X GET -sku '", rplant.env$user, "' ", web, sep="")
    print(curl.string)
  }

  Renew()

  tryCatch(res <<- fromJSON(getForm(paste(web, sep="/"), .checkparams=FALSE,
           curl=rplant.env$curl.call)), 
           error=function(x){return(res <<- data.frame(status=paste(x)))})

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
    for(i in 1:length(res[[3]])) {
      file.types <- c(file.types, res[[3]][[i]]$name)
    }
    return(file.types)
  }
}
# -- END -- #

# -- DIRECTORY FUNCTIONS -- #
ListDir <- function(dir.path="", print.curl=FALSE, shared.user=NULL, suppress.Warnings=FALSE) {
  web <- paste(rplant.env$webio, "io/list", sep="")

  if (suppress.Warnings == FALSE){
    Renew()
    dir.exist <- fromJSON(getURL(paste(rplant.env$webio, "io/list/", rplant.env$user, "/", dir.path, sep=""), curl=rplant.env$curl.call)) 

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

  if (print.curl) {
    if (is.null(shared.user)){
      curl.string <- paste("curl -sku '", rplant.env$user, "' ", web, "/", 
                           rplant.env$user, "/", dir.path, sep="")
    } else {
      curl.string <- paste("curl -sku '", rplant.env$user, "' ", web, "/",
                           shared.user, "/", dir.path, sep="")
    }
    print(curl.string)
  }

  Renew()

  if (is.null(shared.user)) {
    tmp <<- fromJSON(getURL(paste(web, rplant.env$user, dir.path, 
                     sep="/"), curl=rplant.env$curl.call))
  } else {
    tmp <<- fromJSON(getURL(paste(web, shared.user, dir.path, 
                     sep="/"), curl=rplant.env$curl.call))
  }

  res <- matrix(, length(tmp$result), 2)
  colnames(res) <- c("name", "type")
  for (i in 1:length(tmp$result)) {
    res[i, 1] <- tmp$result[[i]]$name
    res[i, 2] <- tmp$result[[i]]$type
  }
  return(res)
}

MakeDir <- function(dir.name, dir.path="", print.curl=FALSE, suppress.Warnings=FALSE) {
  web <- paste(rplant.env$webio, "io", sep="")

  if (print.curl) {
    curl.string <- paste("curl -X PUT -u '", rplant.env$user, "'  -d 'dirName=",
                         dir.name, "&action=mkdir' ", web, "/", rplant.env$user, 
                         "/", dir.path, sep="")
    print(curl.string)
  }

  if (suppress.Warnings == FALSE){
    Renew()
    dir.exist <- fromJSON(getURL(paste(rplant.env$webio, "io/list/", rplant.env$user, "/", dir.path, sep=""), curl=rplant.env$curl.call)) 

    if (length(dir.exist$result) != 0){

      if (dir.path==""){
        dir2.exist <- fromJSON(getURL(paste(rplant.env$webio, "io/list/", rplant.env$user, "/", dir.name, sep=""), curl=rplant.env$curl.call)) 
      } else {
        dir2.exist <- fromJSON(getURL(paste(rplant.env$webio, "io/list/", rplant.env$user, "/", dir.path, "/", dir.name, sep=""), curl=rplant.env$curl.call))
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

  content <- c()
  content[1] <- "action=mkdir"
  content[2] <- paste("dirName=", dir.name, sep="")
  val <- charToRaw(paste(content, collapse = "&"))
  Renew()
  res <<- fromJSON(httpPUT(paste(web, rplant.env$user, dir.path, 
                   sep="/"), content=val, curl=rplant.env$curl.call))
}

DeleteDir <- function(dir.name, dir.path="", print.curl=FALSE, suppress.Warnings=FALSE) {
  web <- paste(rplant.env$webio, "io", sep="")

  if (print.curl) {
    curl.string <- paste("curl -sku '", rplant.env$user, "' -X DELETE ", web, "/",
                         rplant.env$user, "/", dir.path, "/", dir.name, sep="")
    print(curl.string)
  }

  if (suppress.Warnings == FALSE){
    Renew()
    dir.exist <- fromJSON(getURL(paste(rplant.env$webio, "io/list/", rplant.env$user, "/", dir.path, sep=""), curl=rplant.env$curl.call)) 

    if (length(dir.exist$result) != 0){

      if (dir.path==""){
        dir2.exist <- fromJSON(getURL(paste(rplant.env$webio, "io/list/", rplant.env$user, "/", dir.name, sep=""), curl=rplant.env$curl.call)) 
      } else {
        dir2.exist <- fromJSON(getURL(paste(rplant.env$webio, "io/list/", rplant.env$user, "/", dir.path, "/", dir.name, sep=""), curl=rplant.env$curl.call))
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
  Renew()
  if (dir.path == "") {
    res <<- fromJSON(httpDELETE(paste(web, rplant.env$user, dir.name,
                     sep="/"), curl=rplant.env$curl.call))
  } else {
    res <<- fromJSON(httpDELETE(paste(web,rplant.env$user, dir.path,
                     dir.name, sep="/"), curl=rplant.env$curl.call))
  }
}

# -- END -- #


# -- APPLICATION FUNCTIONS -- #
ListApps<- function (print.curl=FALSE) 
{
  web <- paste(rplant.env$webapps, "apps/list", sep="")

  if (print.curl) {
    curl.string <- paste("curl -X GET -sku '", rplant.env$user, "' ", web, sep="")
    print(curl.string)
  }

  Renew()

  tryCatch(tmp <<- fromJSON(getForm(web, .checkparams=FALSE, 
           curl=rplant.env$curl.call)), 
           error=function(x){return(tmp <<- data.frame(status=paste(x)))})

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
      if ((j != 1) & (!is.null(ans))){
        for (k in 1:length(Apps)){
          if (ans == Apps[k]){
            ans <- NULL
            break
          }
        }
      }
      Apps <- append(Apps,ans)
    }
    res <- matrix(, length(Apps))
    colnames(res) <- "Application"
    for (i in 1:length(Apps)) res[i, 1] <- Apps[[i]]
    return(sort(res))
  }
}

GetAppInfo <- function(application, verbose=FALSE, print.curl=FALSE) {

  # This needs to be cleaned up. I think the relevant info is 
        # a) inputs, 
        # b) possible input parameters, and 
        # c) outputs

  if (substring(application,nchar(application)-1,nchar(application)-1) == "u"){
    priv.APP <- substring(application,1,nchar(application)-2)
  } else if (substring(application,nchar(application)-2,nchar(application)-2) == "u"){
    priv.APP <- substring(application,1,nchar(application)-3)
  } else {
    return("Not a valid application: a private app, must be public for use")
  }

  Renew()
  web <- paste(rplant.env$webapps, "apps/name", sep="")
  tryCatch(res <<- fromJSON(getForm(paste(web, priv.APP, sep="/"), 
                  .checkparams=FALSE, curl=rplant.env$curl.call)), 
           error=function(x){return(res <<- data.frame(status=paste(x)))})

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
    if (res$result[[1]]$public == FALSE) {
      return("Not a valid application: a private app, must be public for use")
    }  else if (length(res) == 0) {
      return("No information on application")
    }

    if (print.curl) {
      curl.string <- paste("curl -X GET -sku '", rplant.env$user, "' ", web, 
                           "/", priv.APP, sep="")
      print(curl.string)
    }
  
    if (verbose) {
      return(res)
    } else {
      app.info<-c()
      for (input in sequence(length(res$result[[1]]$inputs))) {
        app.info <- rbind(app.info, c("input", res$result[[1]]$inputs[[input]]$id,
                          res$result[[1]]$inputs[[input]]$semantics$fileTypes[1]))
      }
      for (output in sequence(length(res$result[[1]]$output))) {
        app.info <- rbind(app.info, c("output", res$result[[1]]$output[[output]]$id,
                          res$result[[1]]$output[[output]]$semantics$fileTypes[1])) 
      }
      colnames(app.info)<-c("kind", "id", "fileType")
      return(list(application=res$result[[1]]$id, app.info))
    }
  }
}
# -- END -- #


# -- JOB FUNCTIONS -- #
SubmitJob <- function(application, file.path="", file.list=NULL, input.list, 
                      options.list=NULL, job.name, args=NULL, nprocs=1, 
                      print.curl=FALSE, shared.user=NULL, suppress.Warnings=FALSE) {

  if (suppress.Warnings == FALSE){
    Renew()
    if (is.null(shared.user)){
      dir.exist <- fromJSON(getURL(paste(rplant.env$webio, "io/list/", rplant.env$user, "/", file.path, sep=""), curl=rplant.env$curl.call)) 
    } else {
      dir.exist <- fromJSON(getURL(paste(rplant.env$webio, "io/list/", shared.user, "/", file.path, sep=""), curl=rplant.env$curl.call)) 
    }

    if (length(dir.exist$result) != 0){
      if (is.null(shared.user)){
        file.exist <- list()
        for (i in 1:length(file.list)){
          if (file.path==""){
            file.exist[[i]] <- fromJSON(getURL(paste(rplant.env$webio, "io/list/", rplant.env$user, "/", file.list[[i]], sep=""), curl=rplant.env$curl.call)) 
          } else {
            file.exist[[i]] <- fromJSON(getURL(paste(rplant.env$webio, "io/list/", rplant.env$user, "/", file.path, "/", file.list[[i]], sep=""), curl=rplant.env$curl.call)) 
          }
        }
      } else {
        file.exist <- list()
        for (i in 1:length(file.list)){
          if (file.path==""){
            file.exist[[i]] <- fromJSON(getURL(paste(rplant.env$webio, "io/list/", shared.user, "/", file.list[[i]], sep=""), curl=rplant.env$curl.call)) 
          } else {
            file.exist[[i]] <- fromJSON(getURL(paste(rplant.env$webio, "io/list/", shared.user, "/", file.path, "/", file.list[[i]], sep=""), curl=rplant.env$curl.call)) 
          }
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
      return("Not a valid application: it's private, must be public.  Check ListApps function")
    }

    Renew()
    tmp <- fromJSON(getForm(paste(paste(rplant.env$webapps, "apps/name", sep=""), priv.APP, sep="/"), .checkparams=FALSE, curl=rplant.env$curl.call))

    if (tmp$result[[1]]$public == FALSE) {
      return("Not a valid application: it's private, must be public.  Check ListApps function")
    } else if (length(tmp) == 0) {
      return("No information on application: not valid")
    } else if (tmp$result[[1]]$id != application){
      return(paste("Wrong version of application, should be:",tmp$result[[1]]$id))
    }

    app.info<-c()
    for (input in sequence(length(tmp$result[[1]]$inputs))) {
      app.info <- rbind(app.info, c("input", tmp$result[[1]]$inputs[[input]]$id,
                        tmp$result[[1]]$inputs[[input]]$semantics$fileTypes[1]))
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

    Renew()
    tmp <- fromJSON(getForm(paste(paste(rplant.env$webapps, "apps/name", sep=""), priv.APP, sep="/"), .checkparams=FALSE, curl=rplant.env$curl.call))
  }

  if (tmp$result[[2]]$parallelism == "PARALLEL"){
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

  if(!is.null(options.list)){
    m <- length(options.list)
    m1 <- length(options.list)
  } else {
    m <- NULL
    m1 <- 0
  }
  if(!is.null(file.list)){
    n <- length(file.list)
    n1 <- length(file.list)
  } else {
    n <- NULL
    n1 <- 0
  }
 
  # Automatically make analyses directory; will not overwrite if already present
  # MakeDir(user, token, "analyses", dir.path="")

  MakeDir("analyses", suppress.Warnings=TRUE)

  web <- paste(rplant.env$webapps, "job", sep="")

  if (print.curl) {
    curl.string <- paste("curl -X POST -sku '", rplant.env$user,"' -d 'jobName=",
                         job.name, "&softwareName=", application,
                         "&archive=1&processorCount=", nprocs, "&archivePath=/",
                         rplant.env$user, "/analyses/", job.name,
                         "&requestedTime=24:00:00", sep="")
    if (!is.null(n)){
      for (i in c(1:n)){
        if (is.null(shared.user)){
          curl.string <- paste(curl.string,"&",input.list[[i]],"=/", 
                               rplant.env$user, "/", file.path, "/", 
                               file.list[[i]], sep="")
        } else {
          curl.string <- paste(curl.string,"&",input.list[[i]],"=/", 
                               shared.user, "/", file.path, "/", 
                               file.list[[i]], sep="")
        }
      }
    }

    if (!is.null(m)){
      for (i in c(1:m)){
        curl.string <- paste(curl.string,"&",options.list[[i]][1],"=", 
                             options.list[[i]][2], sep="")
      }
    }

    if (is.null(args)){
      curl.string <- paste(curl.string, "' ", web, sep="")
    } else {
      curl.string <- paste(curl.string,"&",args, "' ", web, sep="")
    }

    print(curl.string)
  }

  content <- c()
  content[1] <- paste("jobName=", job.name, sep="")
  content[2] <- paste("softwareName=", application, sep="")
  content[3] <- "archive=1"
  content[4] <- paste("processorCount=", nprocs, sep="")
  content[5] <- paste("archivePath=/", rplant.env$user, "/analyses/", job.name, 
                        sep="")
  content[6] <- "requestedTime=24:00:00"

  if (!is.null(n)){
    for (i in c(1:n)){
      if (file.path=="") {
        if (is.null(shared.user)){
          content[6+i] <- paste(input.list[[i]],"=/", rplant.env$user, 
                                "/", file.list[[i]], sep="")
        } else {
          content[6+i] <- paste(input.list[[i]],"=/", shared.user, 
                                "/", file.list[[i]], sep="")
        }
      } else {
        if (is.null(shared.user)){
          content[6+i] <- paste(input.list[[i]],"=/", rplant.env$user, "/",
                                file.path, "/", file.list[[i]], sep="")
        } else {
          content[6+i] <- paste(input.list[[i]],"=/", shared.user, "/",
                                file.path, "/", file.list[[i]], sep="")
        }
      }
    }
  }

  if (!is.null(m)){
    for (i in c(1:m)){
      content[6+n1+i] <- paste(options.list[[i]][1],"=", 
                               options.list[[i]][2], sep="")
    }
  }

  if (!is.null(args)){  
    content[7+n1+m1] <- args
  }

  val <- charToRaw(paste(content, collapse = "&"))

  Renew()
  tryCatch(res <<- fromJSON(getURLContent(web, curl=rplant.env$curl.call, 
           infilesize=length(val), readfunction=val, upload=TRUE,
           customrequest="POST")), 
           error=function(x){return(res <<- data.frame(status=paste(x)))})

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
    cat("Job submitted. You can check the status of your job using this id:", 
        res$result$id, "\n")
    return(res$result$id)
  }
}

CheckJobStatus <- function(job.id, verbose=FALSE, print.curl=FALSE) {
  web <- paste(rplant.env$webapps, "job", sep="")

  if (print.curl) {
    curl.string <- paste("curl -X GET -sku '", rplant.env$user, "' ", web, "/",
                         job.id, sep="")
    print(curl.string)
  }

  Renew()

  tryCatch(res <<- fromJSON(getForm(paste(web, job.id, sep="/"), 
           .checkparams=FALSE, curl=rplant.env$curl.call)), 
           error=function(x){return(res <<- data.frame(status=paste(x)))})

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
    if (verbose){
      return(res)
    } else {
      return(res$result$status)
    }
  }
}

DeleteJob <- function(job.id, print.curl=FALSE) {
  web <- paste(rplant.env$webapps, "job", sep="")

  if (print.curl) {
    curl.string <- paste("curl -X DELETE -sku '", rplant.env$user, "' ", web, "/",
                         job.id, sep="")
    print(curl.string)
  }

  Renew()

  tryCatch(res <<- fromJSON(httpDELETE(paste(web, job.id, sep="/"), 
           curl = rplant.env$curl.call)), 
           error=function(x){return(res <<- data.frame(status=paste(x)))})

  if ((res$status != "success") || (length(res$result) == 0)) {
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

RetrieveJob <- function(job.id, file.vec, zip=TRUE, print.curl=FALSE, verbose=FALSE) {  
  web <- paste(rplant.env$webio, "io", sep="")

  Renew()
  tryCatch(JS <<- fromJSON(getForm(paste(paste(rplant.env$webapps, "job", 
           sep=""), job.id, sep="/"), .checkparams=FALSE, 
           curl=rplant.env$curl.call)), 
           error=function(x){return(JS <<- data.frame(status=paste(x)))})

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
    fileList <- ListJobOutput(job.id, print.total=FALSE)
    if (JS$res$status == "ARCHIVING_FINISHED") {
      for (file in 1:length(file.vec)) {
        # if file exists in output then download
        if (file.vec[file] %in% fileList) {

          if (print.curl) {
            curl.string <- paste("curl -X GET -sku '", rplant.env$user, "' ", web, 
                                 "/", JS$result$archivePath, "/", file.vec[file], 
                                 " -o ", file.vec[file], sep="")
            print(curl.string)
          }

          out <<- getForm(paste(web, JS$result$archivePath, "/", 
                   file.vec[file], sep=""), .checkparams=FALSE, 
                   curl=rplant.env$curl.call)

          if (is.raw(out))
            out <- rawToChar(out)
          write(out, file=file.vec[file])
          if (verbose==TRUE)
            print(paste("Downloaded", file.vec[file], "to", getwd(), "directory"))
        } else {
          return(paste("`",file.vec[file], "' is not found within `", job.id,"'", sep=""))
        }
      }

      if (.Platform$OS.type=="windows") {
        zip=FALSE
        invisible(shell(paste("mkdir job_",job.id,sep="")))
        for (i in c(1:length(file.vec))) {
          args <- c(shQuote(file.vec[i]), shQuote(paste("job_",job.id,sep="")))
          system2("xcopy", args, stdout=FALSE)
          file.remove(file.vec[i])
        }
      }
      if (zip) {
        zip(paste("job_",job.id,".zip",sep=""), files=file.vec)
        for (i in c(1:length(file.vec))) {
          file.remove(file.vec[i])
        }
      }
    } else {
      warning("Job is ", JS$res$status)
    }
  }
}

ListJobOutput <- function(job.id, print.curl=FALSE, print.total=TRUE) {
  web <- paste(rplant.env$webapps, "job", sep="")

  Renew()
 
  tryCatch(JS <<- fromJSON(getForm(paste(paste(rplant.env$webapps, "job", 
           sep=""), job.id, sep="/"), .checkparams=FALSE, 
           curl=rplant.env$curl.call)), 
           error=function(x){return(JS <<- data.frame(status=paste(x)))})

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
    if (JS$res$status == "ARCHIVING_FINISHED") {

      if (print.curl) {
        curl.string <- paste("curl -X GET -sku '", rplant.env$user, "' ", web, "/",
                             job.id, "/output/list", sep="")
        print(curl.string)
      }

      res <- fromJSON(getForm(paste(web, job.id, "output/list",
                      sep="/"), .checkparams=FALSE, curl=rplant.env$curl.call))

      if (print.total ==TRUE)
        print(paste("There are ", length(res$result), "output file.vec for job", 
              job.id))

      for (i in 1:length(res$result)) {
        file.vec <- append(file.vec, res$result[[i]]$name)
      }
      return(file.vec)
    } else {
      warning("Job is ", JS$res$status)
    }
  } 
}

GetJobHistory <- function(verbose=FALSE, print.curl=FALSE) {
  web <- paste(rplant.env$webapps, "jobs/list", sep="")

  if (print.curl) {
    curl.string <- paste("curl -X GET -sku '", rplant.env$user, "' ", web, 
                         "/", sep="")
    print(curl.string)
  }

  jobList <- c()

  Renew()
  tryCatch(res <<- fromJSON(getForm(web, .checkparams=FALSE, 
           curl=rplant.env$curl.call)), 
           error=function(x){return(res <<- data.frame(status=paste(x)))})

  if ((res$status != "success") || (length(res$result) == 0)) {
    return("Error: Invalid username/password combination")
  } else {
    if (verbose) 
      return(res)
    if (length(res$result) != 0) {
      for (i in 1: length(res$result)) {
        job <- c(res$result[[i]]$id, res$result[[i]]$name, 
                 res$result[[i]]$software, res$result[[i]]$status)  
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
