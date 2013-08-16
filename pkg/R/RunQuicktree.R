RunQuicktree <- function(file.name, file.path="", job.name=NULL, UPGMA=FALSE,
                         kimura=FALSE, boot=0, output.file.name=NULL,
                         version="quicktree-tree-lonestar-1.1u2", 
                         print.curl=FALSE, shared.username=NULL, 
                         suppress.Warnings=FALSE) {

  flag <- 0
  if (UPGMA){
    args=c("-upgma")
    flag <- 1
  } else if (kimura){
    args <- append(args,"-kimura")
    flag <- 1
  } else if (boot>0){
    args <- append(args,paste("-boot",boot))
    flag <- 1
  } else if (!is.null(output.file.name)){
    args <- append(args,paste("-out",output.file.name))
    flag <- 1
  }

  # make a single statement
  if (flag == 1){
    args <- paste(args, collapse=" ")
  }

  nprocs <- 1
  App <- GetAppInfo(version)[[3]]
  input.list <- vector("list",1)
  input.list[[1]] <- App[,2][1]

  if (is.null(job.name)){
    job.name <- paste(rplant.env$user,"_",version,"_viaR", sep="")
  }

  if (flag == 1){
    args <- list("arguments", args)
  } else {
    args <- NULL
  }

  myJob<-SubmitJob(application=version, job.name=job.name, nprocs=nprocs,
                   file.list=list(file.name), file.path=file.path, 
                   input.list=input.list, suppress.Warnings=suppress.Warnings,
                   print.curl=print.curl, shared.username=shared.username,
                   args.list=args)

  return(myJob)
}
