RAxML <- function(file.name, file.path="", job.name=NULL, type="DNA",
                  model=NULL, bootstrap=NULL, algorithm="d",
                  multipleModelFileName=NULL, args=NULL, numcat=25,
                  nprocs=12, version="raxml-lonestar-7.2.8u1",
                  print.curl=FALSE, shared.username=NULL, 
                  substitution_matrix=NULL, empirical.frequencies=FALSE,
                  suppress.Warnings=FALSE) {

  type <- match.arg(type, c("DNA", "PROTEIN"))

  if (type == "DNA"){

    if (is.null(model)){
      model="GTRCAT"
    }

    model <- match.arg(model, c("GTRCAT", "GTRGAMMA", "GTRCATI", "GTRGAMMAI"))

    if (is.null(job.name)){
      job.name <- paste(rplant.env$user, "_RAxMLdna_", model, "_viaR", sep="")
    }

  } else {

    if (!is.null(substitution_matrix)){
      substitution_matrix  <- match.arg(substitution_matrix, c("DAYHOFF", "DCMUT", "JTT", "MTREV", "WAG", "RTREV", "CPREV", "VT", "BLOSUM62", "MTMAM", "LG", "MTART", "MTZOA", "PMB", "HIVB", "HIVW", "JTTDCMUT", "FLU", "GTR"))
    }

    if (is.null(model)){
      if (empirical.frequencies==FALSE){
        if (is.null(substitution_matrix)){
          model="PROTCATBLOSUM62"
          other.models <- c("PROTCATBLOSUM62", "PROTGAMMABLOSUM62", "PROTCATIBLOSUM62", "PROTGAMMAIBLOSUM62")
        } else {
          model=paste("PROTCAT", substitution_matrix, sep="")
          other.models <- c(paste("PROTCAT", substitution_matrix, sep=""), paste("PROTGAMMA", substitution_matrix, sep=""), paste("PROTCATI", substitution_matrix, sep=""), paste("PROTGAMMAI", substitution_matrix, sep=""))
        }
      } else {
        if (is.null(substitution_matrix)){
          model="PROTCATBLOSUM62F"
          other.models <- c("PROTCATBLOSUM62F", "PROTGAMMABLOSUM62F", "PROTCATIBLOSUM62F", "PROTGAMMAIBLOSUM62F")
        } else {
          model=paste("PROTCAT", substitution_matrix, "F", sep="")
          other.models <- c(paste("PROTCAT", substitution_matrix, "F", sep=""), paste("PROTGAMMA", substitution_matrix, "F", sep=""), paste("PROTCATI", substitution_matrix, "F", sep=""), paste("PROTGAMMAI", substitution_matrix, "F", sep=""))
        }
      }
    } else {
      if (empirical.frequencies==FALSE){
        if (is.null(substitution_matrix)){
          model=paste(model, "BLOSUM62", sep="")
          other.models <- c("PROTCATBLOSUM62", "PROTGAMMABLOSUM62", "PROTCATIBLOSUM62", "PROTGAMMAIBLOSUM62")
        } else {
          model=paste(model, substitution_matrix, sep="")
          other.models <- c(paste("PROTCAT", substitution_matrix, sep=""), paste("PROTGAMMA", substitution_matrix, sep=""), paste("PROTCATI", substitution_matrix, sep=""), paste("PROTGAMMAI", substitution_matrix, sep=""))
        }
      } else {
        if (is.null(substitution_matrix)){
          model=paste(model, "BLOSUM62F", sep="")
          other.models <- c("PROTCATBLOSUM62F", "PROTGAMMABLOSUM62F", "PROTCATIBLOSUM62F", "PROTGAMMAIBLOSUM62F")
        } else {
          model=paste(model, substitution_matrix, "F", sep="")
          other.models <- c(paste("PROTCAT", substitution_matrix, "F", sep=""), paste("PROTGAMMA", substitution_matrix, "F", sep=""), paste("PROTCATI", substitution_matrix, "F", sep=""), paste("PROTGAMMAI", substitution_matrix, "F", sep=""))
        }
      }
    }

    model <- match.arg(model, other.models)

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
