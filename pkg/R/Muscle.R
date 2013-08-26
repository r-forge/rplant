Muscle <- function(file.name, file.path="", job.name=NULL, args=NULL,
                   version="Muscle-3.8.32", print.curl=FALSE,
                   aln.filetype="PHYLIP_INT", shared.username=NULL,
                   suppress.Warnings=FALSE) {

  private.APP <- TRUE
  suppress.Warnings <- TRUE
  aln.filetype <- match.arg(aln.filetype, c("PHYLIP_INT", "PHYLIP_SEQ", "PHYLIP_PARS", "FASTA", "CLUSTALW", "MSF"))

  if (aln.filetype == "PHYLIP_INT"){
    args <- append(args, "-phyiout")
    aln.name <- "phylip_interleaved.aln"
  } else if (aln.filetype == "PHYLIP_SEQ"){
    args <- append(args, "-physout")
    aln.name <- "phylip_sequential.aln"
  } else if (aln.filetype == "FASTA"){
    args <- append(args, "-fastaout")
    aln.name <- "fasta.aln"
  } else if (aln.filetype == "CLUSTALW"){
    args <- append(args, "-clwout")
    aln.name <- "clustalw.aln"
  } else if (aln.filetype == "MSF"){
    args <- append(args, "-msfout")
    aln.name <- "msf.aln"
  } else if (aln.filetype == "PHYLIP_PARS"){
    args <- append(args, "-parsout")
    aln.name <- "phylip_pars.aln"
  }

  args <- paste(args, collapse=" ")  # make a single statement

  nprocs <- 1
  App <- GetAppInfo(version)[[3]]
  input.list <- vector("list",1)
  input.list[[1]] <- App[,2][1]

  if (is.null(job.name))
    job.name <- paste(rplant.env$user,"_",version,"_viaR", sep="")

  if (!is.null(args)){
    args <- list(c("arguments",args))
  }

  myJob<-SubmitJob(application=version, job.name=job.name, nprocs=nprocs,
                   file.list=list(file.name), file.path=file.path, 
                   input.list=input.list, suppress.Warnings=suppress.Warnings,
                   print.curl=print.curl, shared.username=shared.username,
                   args.list=args, private.APP=private.APP)

  cat(paste("Result file: ", aln.name, "\n", sep=""))
  return(myJob)
}
