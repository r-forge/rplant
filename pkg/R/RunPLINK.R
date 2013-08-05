RunPLINK <- function(DE.file.list="", DE.file.path="", job.name=NULL,
                     association.method="--assoc", no.sex=TRUE, args=NULL,
                     print.curl=FALSE, multi.adjust=TRUE, version="plink-1.07", 
                     shared.user.name=NULL, suppress.Warnings=FALSE) {

  nprocs <- 1
  private.APP <- TRUE
  input.len <- length(DE.file.list)
  input.list <- list()
  if ((input.len) == 3){
    ext1 <- unlist(strsplit(DE.file.list[[1]], "\\."))[2]
    ext2 <- unlist(strsplit(DE.file.list[[2]], "\\."))[2]
    ext3 <- unlist(strsplit(DE.file.list[[3]], "\\."))[2]
    input.type="B"
    input.list[[1]] <- find.input(ext1)
    input.list[[2]] <- find.input(ext2)
    input.list[[3]] <- find.input(ext3)
  } else {
    ext1 <- unlist(strsplit(DE.file.list[[1]], "\\."))[2]
    ext2 <- unlist(strsplit(DE.file.list[[2]], "\\."))[2]
    input.list[[1]] <- find.input(ext1)
    input.list[[2]] <- find.input(ext2)
    if ((ext1 == "tfam" ) || (ext1 == "tped")) {input.type="T"}
  }  

  args <- c(paste("arguments=", association.method), args)

  if (multi.adjust){args <- append(args, c("--adjust"))}

  if (no.sex){args <- append(args, c("--allow-no-sex"))}

  if (input.type=="T"){
    options <- list(c("T",TRUE))
    if (is.null(job.name)){
      BASE1 <- substr(DE.file.list[[1]],1,nchar(DE.file.list[[1]])-5)
      EXT1 <- substr(DE.file.list[[1]],nchar(DE.file.list[[1]])-3,nchar(DE.file.list[[1]]))
      BASE2 <- substr(DE.file.list[[2]],1,nchar(DE.file.list[[2]])-5)
      if (EXT1 == "tfam"){
        job.name <- paste(BASE1,"_",BASE2,"_", association.method, sep="")
      } else {
        job.name <- paste(BASE2,"_",BASE1,"_", association.method, sep="")
      }
    }
    args <- append(args, c("--out",job.name))
  } else if (input.type=="B") {
    options <- list(c("B",TRUE))
    if (is.null(job.name)){
      BASE1 <- substr(DE.file.list[[1]],1,nchar(DE.file.list[[1]])-4)
      EXT1 <- substr(DE.file.list[[1]],nchar(DE.file.list[[1]])-2,nchar(DE.file.list[[1]]))
      BASE2 <- substr(DE.file.list[[2]],1,nchar(DE.file.list[[2]])-4)
      EXT2 <- substr(DE.file.list[[2]],nchar(DE.file.list[[2]])-2,nchar(DE.file.list[[1]]))
      BASE3 <- substr(DE.file.list[[3]],1,nchar(DE.file.list[[3]])-4)
      EXT3 <- substr(DE.file.list[[3]],nchar(DE.file.list[[3]])-2,nchar(DE.file.list[[1]]))
      if (EXT1 == "bed" && EXT3 == "bam"){
        job.name <- paste(BASE1, "_", BASE3, "_", BASE2, "_", association.method, sep="")
      } else if (EXT1 == "bed" && EXT2 == "bam") {
        job.name <- paste(BASE1, "_", BASE2, "_", BASE3, "_", association.method, sep="")
      } else if (EXT2 == "bed" && EXT3 == "bam"){
        job.name <- paste(BASE2, "_", BASE3, "_", BASE1, "_", association.method, sep="")
      } else if (EXT3 == "bed" && EXT1 == "bam"){
        job.name <- paste(BASE3, "_", BASE1, "_", BASE2, "_", association.method, sep="")
      } else if (EXT2 == "bed" && EXT1 == "bam") {
        job.name <- paste(BASE2, "_", BASE1, "_", BASE3, "_", association.method, sep="")
      } else {
        job.name <- paste(BASE3, "_", BASE2, "_", BASE1, "_", association.method, sep="")
      }
    }
    args <- append(args, c("--out",job.name))
  } else {
    options <- NULL
    if (is.null(job.name)){
      BASE1 <- substr(DE.file.list[[1]],1,nchar(DE.file.list[[1]])-4)
      job.name <- paste(BASE1,"_", association.method, sep="")
    }
    args <- append(args, c("--out",job.name))
  }

  # make a single statement
  args <- paste(args, collapse=" ") 

  # Submit
  myJob<-SubmitJob(application=version, options.list=options, private.APP=private.APP,
                   DE.file.list=DE.file.list, DE.file.path=DE.file.path,
                   input.list=input.list, job.name=job.name, nprocs=nprocs, 
                   print.curl=print.curl, shared.user.name=shared.user.name,
                   args=args, suppress.Warnings=suppress.Warnings)

  return(list(myJob,job.name))

}
