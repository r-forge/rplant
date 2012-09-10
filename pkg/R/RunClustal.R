RunClustal <- function(user.name, token, DE.file.name="", DE.file.path="", job.name=NULL, version=c("ClustalW2-2.1", "clustalw2-lonestar-2.1", "clustalw-ranger-1.0")) {
  version <- match.arg(version)
  if (is.null(job.name))
    job.name <- paste(user.name,"_", version, "viaAPI", sep="")
  myJob<-SubmitJob(user.name, token, application=version, 
                   DE.file.name=DE.file.name, DE.file.path=DE.file.path, 
                   job.name=job.name, nprocs="1")
  return(myJob)
}
