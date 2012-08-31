RunRAxMLNucleo <- function(user.name, token, DE.file.name="", DE.file.path="", job.name=NULL, model=c("GTRCAT", "GTRGAMMA", "GTRCAT.GAMMA", "GTRGAMMAI", "GTRCAT.GAMMAI"), bootstrap=NULL, numcat=25, nprocs=1) {
  #Automatically make analyses directory; will not overwrite if already present
  MakeDir(user.name, token, "analyses", DE.dir.path="")

  web <- "https://foundation.iplantc.org/apps-v1/job"
  numcat <- deparse(substitute(numcat))
  nprocs <- deparse(substitute(nprocs))
  model <- match.arg(model)
  if (is.null(job.name)){job.name <- paste(user.name, "RAxMLviaAPI_",model, sep="")}
  if (is.null(bootstrap)){
  curl.string <- paste("curl -X POST -sku '", user.name, ":", token, 
                       "' -d 'jobName=", job.name,
                       "&softwareName=raxml-lonestar-7.2.8",
                       "&archive=1&inputSeqs=", "/", 
                       user.name, "/", DE.file.path, "/", DE.file.name, 
                       "&processorCount=", nprocs, "&archivePath=/", 
                       user.name, "/analyses/", job.name, 
                       "&requestedTime=24:00:00&outputFormat=fasta&mode=auto&",
                       "arguments=-m ", model," -c ",numcat,"' ", web, sep="")
  }else{
  rns <- deparse(substitute(bootstrap))
  curl.string <- paste("curl -X POST -sku '", user.name, ":", token, 
                       "' -d 'jobName=", job.name,
                       "&softwareName=raxml-lonestar-7.2.8",
                       "&archive=1&inputSeqs=", "/", 
                       user.name, "/", DE.file.path, "/", DE.file.name, 
                       "&processorCount=", nprocs, "&archivePath=/", 
                       user.name, "/analyses/", job.name, 
                       "&requestedTime=24:00:00&outputFormat=fasta&mode=auto&",
                       "arguments=-m ", model," -c ",numcat," -b ",rns,"' ", web, sep="")

  }
  print(curl.string)
  res <- fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse=""))
  if (res$status == "success")
     cat("Job submitted. You can check the status of your job using this id:", 
        res$result$id, "\n")
  else
    cat("Error.", res$message, "\n")
    return(res$result$id)
}
