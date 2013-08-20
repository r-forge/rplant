ClustalW <- function(file.name, file.path="", type="DNA", job.name=NULL,
                     version="ClustalW2-2.1u1", print.curl=FALSE,  args=NULL,
                     aln.filetype="PHYLIP", shared.username=NULL,
                     suppress.Warnings=FALSE) {

  type <- match.arg(type, c("DNA", "PROTEIN"))

  aln.filetype <- match.arg(aln.filetype, c("CLUSTAL", "PHYLIP", "NEXUS", "GCG", "GDE", "PIR"))

  if (type == "DNA"){
    args <- append(args, "-TYPE=DNA")
  } else {
    args <- append(args, "-TYPE=PROTEIN")
  }

  if (aln.filetype == "PHYLIP"){
    args <- append(args, "-OUTPUT=PHYLIP")
  } else if (aln.filetype == "NEXUS"){
    args <- append(args, "-OUTPUT=NEXUS")
  } else if (aln.filetype == "GCG"){
    args <- append(args, "-OUTPUT=GCG")
  } else if (aln.filetype == "GDE"){
    args <- append(args, "-OUTPUT=GDE")
  } else if (aln.filetype == "PIR"){
    args <- append(args, "-OUTPUT=PIR")
  }

  nprocs=1
  App <- GetAppInfo(version)[[3]]
  input.list <- vector("list",1)
  input.list[[1]] <- App[,2][1]

  if (is.null(job.name))
    job.name <- paste(rplant.env$user,"_",version,"_viaR", sep="")

  args <- paste(args, collapse=" ")  # make a single statement

  args <- list(c("arguments",args))

  myJob<-SubmitJob(application=version, job.name=job.name, nprocs=nprocs, 
                   file.list=list(file.name), file.path=file.path, 
                   input.list=input.list, suppress.Warnings=suppress.Warnings,
                   print.curl=print.curl, shared.username=shared.username,
                   args.list=args)

  cat("Result file: clustalw2.fa\n")
  return(myJob)
}
