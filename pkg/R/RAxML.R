RAxML <- function(file.name, file.path="", job.name=NULL, type="DNA",
                  model=NULL, bootstrap=NULL, algorithm="d",
                  multipleModelFileName=NULL, args=NULL, numcat=25,
                  nprocs=12, version="raxml-lonestar-7.2.8u1",
                  print.curl=FALSE, shared.username=NULL,
                  suppress.Warnings=FALSE) {

  type <- match.arg(type, c("DNA", "PROTEIN"))

  if (type == "DNA"){

    if (is.null(model)){
      model="GTRCAT"
    }

    model <- match.arg(model, c("GTRCAT", "GTRGAMMA", "GTRCAT.GAMMA", "GTRGAMMAI", "GTRCAT.GAMMAI"))

    if (is.null(job.name)){
      job.name <- paste(rplant.env$user, "_RAxMLdna_", model, "_viaR", sep="")
    }

  } else {

    if (is.null(model)){
      model="PROTCAT"
    }

    model <- match.arg(model, c("PROTCAT", "PROTGAMMA", "PROTCAT.GAMMA", "PROTGAMMAI", "PROTCAT.GAMMAI"))

    if (is.null(job.name)){
      job.name <- paste(rplant.env$user, "_RAxMLprotein_", model, "_viaR", sep="")
    }
  }

  App <- GetAppInfo(version)[[3]]
  input.list <- vector("list",1)
  input.list[[1]] <- App[,2][1]

  #initialize arguments
  args <- c(args)
  args <- append(args, c("-m", model))
  args <- append(args, c("-N", 10))
  #args <- append(args, c("-T", numberOfThreads))
  if (!is.null(bootstrap)) {
    args <- append(args, c("-b", bootstrap))
  }
  args <- append(args, c("-f", algorithm))
  args <- append(args, c("-p", floor(runif(1, 1, 10^6))))
  #args <- append(args, c("-#", numberOfRuns))
  args <- append(args, c("-c", numcat))
  args <- append(args, c("-s", file.name))
  args <- append(args, c("-n", job.name))
  if(!is.null(multipleModelFileName)) {
    args <- append(args, c("-q", multipleModelFileName))
  }
  args <- paste(args, collapse=" ")  # make a single statement
 
  args <- list(c("arguments", args))


  # Submit
  myJob<-SubmitJob(application=version, job.name=job.name, nprocs=nprocs,
                   file.list=list(file.name), file.path=file.path, 
                   input.list=input.list, suppress.Warnings=suppress.Warnings,
                   print.curl=print.curl, shared.username=shared.username,
                   args.list=args)
  return(myJob)
}
