RunFasttree <- function(file.name, file.path="", job.name=NULL, print.curl=FALSE,
                        version="fasttreeDispatcher-1.0.0u1", args=NULL,
                        shared.user.name=NULL, suppress.Warnings=FALSE) {

  nprocs <- 1
  App <- GetAppInfo(version)[[2]]
  input.list <- vector("list",1)
  input.list[[1]] <- App[,2][1]

  if (is.null(job.name)){
    job.name <- paste(rplant.env$user,"_",version,"_viaR", sep="")
  }

  if (!is.null(args)){
    args <- paste("arguments=,",args)
  }

  myJob<-SubmitJob(application=version, job.name=job.name, nprocs=nprocs,
                   file.list=list(file.name), file.path=file.path, 
                   input.list=input.list, suppress.Warnings=suppress.Warnings,
                   print.curl=print.curl, shared.user.name=shared.user.name,
                   args=args)

  return(myJob)
}
