Muscle <- function(file.name, file.path="", job.name=NULL, args=NULL,
                   version="Muscle-3.8.32u1", print.curl=FALSE,
                   shared.username=NULL, suppress.Warnings=FALSE) {

  nprocs <- 1
  App <- GetAppInfo(version)[[3]]
  input.list <- vector("list",1)
  input.list[[1]] <- App[,2][1]

  if (is.null(job.name))
    job.name <- paste(rplant.env$user,"_",version,"_viaR", sep="")

  if (!is.null(job.name)){
    args <- list(c("arguments",args))
  }

  myJob<-SubmitJob(application=version, job.name=job.name, nprocs=nprocs,
                   file.list=list(file.name), file.path=file.path, 
                   input.list=input.list, suppress.Warnings=suppress.Warnings,
                   print.curl=print.curl, shared.username=shared.username,
                   args.list=args)

  return(myJob)
}
