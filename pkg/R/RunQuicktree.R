RunQuicktree <- function(file.name, file.path="", job.name=NULL, UPGMA=FALSE,
                         kimura=FALSE, boot=0, output.file.name=NULL,
                         version="quicktree-tree-lonestar-1.1u1", 
                         print.curl=FALSE, shared.username=NULL, 
                         suppress.Warnings=FALSE) {

  if (UPGMA){
    args=c("-upgma")
  } else if (kimura){
    args <- append(args,"-kimura")
  } else if (boot>0){
    args <- append(args,paste("-boot",boot))
  } else if (!is.null(output.file.name)){
    args <- append(args,paste("-out",output.file.name))
  }

  # make a single statement
  args <- paste(args, collapse=" ") 

  nprocs <- 1
  App <- GetAppInfo(version)[[2]]
  input.list <- vector("list",1)
  input.list[[1]] <- App[,2][1]

  if (is.null(job.name)){
    job.name <- paste(rplant.env$user,"_",version,"_viaR", sep="")
  }

  if (!is.null(args)){
    args <- list("arguments", args)
  }

  myJob<-SubmitJob(application=version, job.name=job.name, nprocs=nprocs,
                   file.list=list(file.name), file.path=file.path, 
                   input.list=input.list, suppress.Warnings=suppress.Warnings,
                   print.curl=print.curl, shared.username=shared.username,
                   args.list=args)

  return(myJob)
}
