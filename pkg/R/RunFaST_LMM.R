RunFaST_LMM <- function(input.file.list="", ALL.file.path="", print.curl=FALSE,
                        sim.file.list=NULL, pheno.file.name=NULL, mpheno=1,
                        args=NULL, covar.file.name=NULL, job.name=NULL, 
                        version="FaST-LMM-1.09", shared.user.name=NULL,
                        suppress.Warnings=FALSE) {
  nprocs <- 1
  private.APP <- TRUE
  input.len <- length(input.file.list)
  input.list <- list()
  if ((input.len) == 3){
    ext1 <- unlist(strsplit(input.file.list[[1]], "\\."))[2]
    ext2 <- unlist(strsplit(input.file.list[[2]], "\\."))[2]
    ext3 <- unlist(strsplit(input.file.list[[3]], "\\."))[2]
    input.type="B"
    input.list[[1]] <- find.input(ext1)
    input.list[[2]] <- find.input(ext2)
    input.list[[3]] <- find.input(ext3)
  } else {
    ext1 <- unlist(strsplit(input.file.list[[1]], "\\."))[2]
    ext2 <- unlist(strsplit(input.file.list[[2]], "\\."))[2]
    input.list[[1]] <- find.input(ext1)
    input.list[[2]] <- find.input(ext2)
    if ((ext1 == "tfam" ) || (ext1 == "tped")) {
      input.type="T"
    } else {
      input.type="R"
    }
  }

  args <- c("arguments=",args)

  if (input.type=="T"){
    options <- list(c("T",TRUE))
    if (!is.null(sim.file.list)){
      input.file.list <- append(input.file.list,sim.file.list)
      input.list <- append(input.list,c("Sim1","Sim2"))
      options <- append(options,list(c("S",TRUE)))
    }
  } else if (input.type=="B") {
    options <- list(c("B",TRUE))
    if (!is.null(sim.file.list)){
      input.file.list <- append(input.file.list,sim.file.list)
      input.list <- append(input.list,c("Sim1","Sim2","Sim3"))
      options <- append(options,list(c("S",TRUE)))
    }
  } else {
    options <- NULL
    if (!is.null(sim.file.list)){
      input.file.list <- append(input.file.list,sim.file.list)
      input.list <- append(input.list,c("Sim1","Sim2"))
      options <- append(options,list(c("S",TRUE)))
    }
  }

  if (is.null(job.name)){
    job.name <- unlist(strsplit(input.file.list[[1]], "\\."))[1]

  }

  args <- append(args, c("-out",job.name))

  if (!is.null(pheno.file.name)){
    input.file.list <- append(input.file.list, pheno.file.name)
    input.list <- append(input.list,"inputPHENO")
    options <- append(options,list(c("P",TRUE),c("mpheno",mpheno)))
  }

  if (!is.null(covar.file.name)){
    input.file.list <- append(input.file.list, covar.file.name)
    input.list <- append(input.list,"inputCOVAR")
    options <- append(options,list(c("C",TRUE)))
  }

  # make a single statement
  args <- paste(args, collapse=" ") 

  # Submit
  myJob<-SubmitJob(application=version, options.list=options, args=args, 
                   file.list=input.file.list, file.path=ALL.file.path, private.APP=private.APP,
                   input.list=input.list, job.name=job.name, nprocs=nprocs, 
                   print.curl=print.curl, shared.user.name=shared.user.name,
                   suppress.Warnings=suppress.Warnings)

  return(list(myJob,job.name))

}
