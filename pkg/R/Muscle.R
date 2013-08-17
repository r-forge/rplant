RunMuscle <- function(file.name, file.path="", job.name=NULL, args=NULL,
                      version="muscle-lonestar-3.8.31u2", print.curl=FALSE,
                      shared.username=NULL, suppress.Warnings=FALSE) {

  if (version == "Muscle-3.8.32"){
    private.APP = TRUE
  } else {
    private.APP = FALSE
  }

  nprocs <- 1
  App <- GetAppInfo(version)[[3]]
  input.list <- vector("list",1)
  input.list[[1]] <- App[,2][1]

  if (is.null(job.name))
    job.name <- paste(rplant.env$user,"_",version,"_viaR", sep="")

  args <- list(c("arguments",paste("-msf -msfout msf.aln", args)))

  myJob<-SubmitJob(application=version, job.name=job.name, nprocs=nprocs,
                   file.list=list(file.name), file.path=file.path, 
                   input.list=input.list, suppress.Warnings=suppress.Warnings,
                   print.curl=print.curl, shared.username=shared.username,
                   args.list=args, private.APP=private.APP)

  return(myJob)
}
