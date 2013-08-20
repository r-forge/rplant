Mafft <- function(file.name, file.path="", type="DNA",  print.curl=FALSE,
                  version="mafftDispatcher-1.0.13100u1", args=NULL, 
                  job.name=NULL, aln.filetype="FASTA", shared.username=NULL,
                  suppress.Warnings=FALSE) {

  type <- match.arg(type, c("DNA", "PROTEIN"))

  aln.filetype <- match.arg(aln.filetype, c("CLUSTALW", "FASTA"))

  if (type == "DNA"){
    args <- append(args, "--nuc")
  } else {
    args <- append(args, "--amino")
  }

  if (aln.filetype == "CLUSTALW"){
    args <- append(args, "--clustalout")
  }

  nprocs <- 1
  App <- GetAppInfo(version)[[3]]
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

  cat("Result file: mafft.fa\n")
  return(myJob)
}
