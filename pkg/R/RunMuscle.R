RunMuscle <- function(user.name, token, DE.file.name="", DE.file.path="", job.name=NULL, version="muscle-ranger-2.0") {
  if (is.null(job.name))
    job.name <- paste(user.name, "MUSCLEviaAPI", sep="")
  myJob<-SubmitJob(user.name, token, application=version, 
                   DE.file.name=DE.file.name, DE.file.path=DE.file.path, 
                   job.name=job.name, nprocs="1")
  return(myJob)
}
