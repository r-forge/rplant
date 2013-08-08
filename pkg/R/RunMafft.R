RunMafft <- function(file.name, file.path="", job.name=NULL, print.curl=FALSE,
                     version="mafft-lonestar-6.864u1",  args=NULL,
                     shared.username=NULL, suppress.Warnings=FALSE) {

  nprocs <- 1
  App <- GetAppInfo(version)[[2]]
  input.list <- vector("list",1)
  input.list[[1]] <- App[,2][1]

  if (is.null(job.name)){
    job.name <- paste(rplant.env$user,"_",version,"_viaR", sep="")
  }

  if (!is.null(args)){
    args <- list(c("arguments",args))
  }

  myJob<-SubmitJob(application=version, job.name=job.name, nprocs=nprocs,
                   file.list=list(file.name), file.path=file.path, 
                   input.list=input.list, suppress.Warnings=suppress.Warnings,
                   print.curl=print.curl, shared.username=shared.username,
                   args.list=args)

  return(myJob)
}
