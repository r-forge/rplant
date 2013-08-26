PLINKConversion <- function(file.list="", file.path="", output.type="--recode",
                            job.name=NULL, shared.username=NULL,
                            print.curl=FALSE, version="plink-1.07u1", 
                            suppress.Warnings=FALSE) {

  input.len <- length(file.list)
  input.list <- list()
  if ((input.len) == 3){
    ext1 <- unlist(strsplit(file.list[[1]], "\\."))[2]
    ext2 <- unlist(strsplit(file.list[[2]], "\\."))[2]
    ext3 <- unlist(strsplit(file.list[[3]], "\\."))[2]
    input.type="B"
    input.list[[1]] <- find.input(ext1)
    input.list[[2]] <- find.input(ext2)
    input.list[[3]] <- find.input(ext3)
  } else {
    ext1 <- unlist(strsplit(file.list[[1]], "\\."))[2]
    ext2 <- unlist(strsplit(file.list[[2]], "\\."))[2]
    input.list[[1]] <- find.input(ext1)
    input.list[[2]] <- find.input(ext2)
    if ((ext1 == "tfam" ) || (ext1 == "tped")) {
      input.type="T"
    } else {
      input.type="R"
    }
  }

  args <- c(output.type)

  if (input.type=="T"){
    options <- list(c("T",TRUE))
    if (is.null(job.name)){
      BASE1 <- substr(file.list[[1]],1,nchar(file.list[[1]])-5)
      EXT1 <- substr(file.list[[1]],nchar(file.list[[1]])-3,nchar(file.list[[1]]))
      BASE2 <- substr(file.list[[2]],1,nchar(file.list[[2]])-5)
      if (EXT1 == "tfam"){
        job.name <- paste(BASE1,"_",BASE2, sep="")
      } else {
        job.name <- paste(BASE2,"_",BASE1, sep="")
      }
    }
    args <- append(args, c("--out",job.name))
  } else if (input.type=="B") {
    options <- list(c("B",TRUE))
    if (is.null(job.name)){
      BASE1 <- substr(file.list[[1]],1,nchar(file.list[[1]])-4)
      EXT1 <- substr(file.list[[1]],nchar(file.list[[1]])-2,nchar(file.list[[1]]))
      BASE2 <- substr(file.list[[2]],1,nchar(file.list[[2]])-4)
      EXT2 <- substr(file.list[[2]],nchar(file.list[[2]])-2,nchar(file.list[[1]]))
      BASE3 <- substr(file.list[[3]],1,nchar(file.list[[3]])-4)
      EXT3 <- substr(file.list[[3]],nchar(file.list[[3]])-2,nchar(file.list[[1]]))
      if (EXT1 == "bed" && EXT3 == "bam"){
        job.name <- paste(BASE1, "_", BASE3, "_", BASE2, sep="")
      } else if (EXT1 == "bed" && EXT2 == "bam") {
        job.name <- paste(BASE1, "_", BASE2, "_", BASE3, sep="")
      } else if (EXT2 == "bed" && EXT3 == "bam"){
        job.name <- paste(BASE2, "_", BASE3, "_", BASE1, sep="")
      } else if (EXT3 == "bed" && EXT1 == "bam"){
        job.name <- paste(BASE3, "_", BASE1, "_", BASE2, sep="")
      } else if (EXT2 == "bed" && EXT1 == "bam") {
        job.name <- paste(BASE2, "_", BASE1, "_", BASE3, sep="")
      } else {
        job.name <- paste(BASE3, "_", BASE2, "_", BASE1, sep="")
      }
    }
    args <- append(args, c("--out",job.name))
  } else {
    options <- NULL
    if (is.null(job.name)){
      BASE1 <- substr(file.list[[1]],1,nchar(file.list[[1]])-4)
      EXT1 <- substr(file.list[[1]],nchar(file.list[[1]])-2,nchar(file.list[[1]]))
      BASE2 <- substr(file.list[[2]],1,nchar(file.list[[2]])-4)
      if (EXT1 == "map"){
        job.name <- paste(BASE1,"_",BASE2,"_", sep="")
      } else {
        job.name <- paste(BASE2,"_",BASE1,"_", sep="")
      }
    }
    args <- append(args, c("--out",job.name))
  }

  # make a single statement
  args <- paste(args, collapse=" ") 
  options <- append(options, list(c("arguments",args)))

  # Submit
  myJob<-SubmitJob(application=version, args.list=options, job.name=job.name,
                   file.list=file.list, file.path=file.path,   
                   input.list=input.list, suppress.Warnings=suppress.Warnings,
                   shared.username=shared.username, print.curl=print.curl)

  return(myJob)
}
