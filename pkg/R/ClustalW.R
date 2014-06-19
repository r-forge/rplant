ClustalW <- function(file.name, file.path="", type="DNA", job.name=NULL,
                     version="ClustalW2-2.1u1", print.curl=FALSE,  args=NULL,
                     aln.filetype="PHYLIP_INT", shared.username=NULL,
                     suppress.Warnings=FALSE, email=TRUE) {

  #  An approach for performing multiple alignments of large numbers of
  #   amino acid or nucleotide sequences is described. Input is a fasta file
  #   output is an alignment.
  #
  # Args:
  #   file.name (type = string): File name of input fasta file
  #   file.path (type = string): Path to where input file is located
  #   type (type = string): Either 'DNA' or 'PROTEIN'
  #   args (type = string): Optional for arguments (i.e. flags, like on command 
  #     line).  One string with commands separated by spaces. see help(ClustalW)
  #     for details.
  #   job.name (type = string): Job name adds a time stamp to make them unique
  #   aln.filetype (type = string): Six options: 'CLUSTALW', "PHYLIP_INT', 
  #     'NEXUS', 'GCG', 'GDE', 'PIR'.  This is output filetype
  #   print.curl (type = string): Prints the associated curl statement
  #   version (type = string): ClustalW version
  #   shared.username (type = string): Valid iPlant username with whom the files 
  #     are being shared.
  #   suppress.Warnings (type = boolean): Don't do any error checking
  #   email (type = boolean): By default an email will be sent to the user when 
  #     the job finishes.
  #
  # Returns:
  #   Returns the job id (number).  o/w an error

  type <- match.arg(type, c("DNA", "PROTEIN"))

  aln.filetype <- match.arg(aln.filetype, c("CLUSTALW", "PHYLIP_INT", "NEXUS", 
                                            "GCG", "GDE", "PIR"))

  # Get the appropriate flags for the function
  if (type == "DNA"){
    args <- append(args, "-TYPE=DNA")
  } else {
    args <- append(args, "-TYPE=PROTEIN")
  }

  if (aln.filetype == "PHYLIP_INT"){
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

  # Get proper input.list
  nprocs=1
  App <- GetAppInfo(version)[[3]]
  input.list <- vector("list",1)
  input.list[[1]] <- App[,2][1]

  if (is.null(job.name))
    job.name <- paste(rplant.env$user,"_",version,"_viaR", sep="")

  args <- paste(args, collapse=" ")  # make a single statement

  args <- list(c("arguments",args))

  myJob<-SubmitJob(application=version, job.name=job.name, nprocs=nprocs, 
                   file.list=list(file.name), file.path=file.path, email=email,
                   input.list=input.list, suppress.Warnings=suppress.Warnings,
                   print.curl=print.curl, shared.username=shared.username,
                   args.list=args)

  cat("Result file: clustalw2.fa\n")
  return(myJob)
}
