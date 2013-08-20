Fasttree <- function(file.name, file.path="", job.name=NULL, args=NULL, 
                     type="DNA", model=NULL, gamma=FALSE, stat=FALSE,
                     print.curl=FALSE, version="fasttreeDispatcher-1.0.0u1", 
                     shared.username=NULL, suppress.Warnings=FALSE) {

  type <- match.arg(type, c("DNA", "PROTEIN"))

  if (type == "DNA"){

    if (is.null(model)){
      model="GTRCAT"
    }

    model <- match.arg(model, c("GTRCAT", "JCCAT"))

    if (is.null(job.name)){
      job.name <- paste(rplant.env$user, "_Fasttreedna_", model, "_viaR", sep="")
    }

  } else {

    if (is.null(model)){
      model="JTTCAT"
    }

    model <- match.arg(model, c("JTTCAT", "WAGCAT"))

    if (is.null(job.name)){
      job.name <- paste(rplant.env$user, "_Fastreeprotein_", model, "_viaR", sep="")
    }
  }

  if (model == "GTRCAT"){
    args <- append(args, "-gtr -nt")
  } else if (aln.filetype == "WAGCAT"){
    args <- append(args, "-wag")
  }

  if (gamma == TRUE) {
    args <- append(args, "-gamma")
  }

  if (stat == TRUE) {
    args <- append(args, "-log logfile")
  }
  args <- paste(args, collapse=" ")  # make a single statement

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

  return(myJob)
}
