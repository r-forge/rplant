require(rjson)

##########################################AUTHENTICATION FUNCTIONS###########################################
token.get<-function(user.name, user.pwd, API=c("iplant", "cipres", "tnrs")){
#a function for obtaining a token -- works!!
	if (is.character(API)) {
		if (API=="iplant") {
			#Pastes together the string used to call a token -- not optimal, but easier to edit:
			curl.string<-"curl -X POST -sku" 
			curl.string<-paste(curl.string, user.name, sep=" '")
			curl.string<-paste(curl.string, user.pwd, sep=":")
			#Is the url always going to be the one used here?
			curl.string<-paste(curl.string, "https://foundation.iplantc.org/auth-v1/", sep="' ")
			#print(curl.string)
			res<-fromJSON(system(curl.string,intern=TRUE))
			#Seems like the token is the only thing of interest to output:
			return(res$result$token)
		}
		else {
			warning("Not yet implemented")
		}
	}
}

token.renew<-function(user.name, user.pwd, token, API=c("iplant", "cipres", "tnrs")){
#a function for renewing a token -- works!!
	if (is.character(API)) {
		if (API=="iplant") {
			#Pastes together the string used to renew a token.
			#Seems clunky like this, but only way to incorporate user-supplied arguments:
			curl.string<-"curl -X POST -sku" 
			curl.string<-paste(curl.string, user.name, sep=" '")
			curl.string<-paste(curl.string, user.pwd, sep=":")
			curl.string<-paste(curl.string, "-d 'token=", sep="' ")
			curl.string<-paste(curl.string, token, sep="")
			#Is the url always going to be the one used here?
			curl.string<-paste(curl.string, "https://foundation.iplantc.org/auth-v1/renew", sep="' ")
			res<-fromJSON(system(curl.string,intern=TRUE))		
			#Outputs a message of whether or not renewal was a success:
			res$status
		}
		else {
			warning("Not yet implemented")
		}
	}
}
##############################################################################################################


############################################FILE AND DATA FUNCTIONS###########################################
file.upload<-function(user.name, token, file2upload, fileType){
#a function for uploading a file -- works!!
	curl.string<-"curl -sku"
	curl.string<-paste(curl.string, user.name, sep=" '")
	curl.string<-paste(curl.string, token, sep=":")
	curl.string<-paste(curl.string, "-F 'fileToUpload=", sep="' ")
	curl.string<-paste(curl.string, file2upload, sep="@")
	curl.string<-paste(curl.string, "-F 'fileType=", sep="' ")
	curl.string<-paste(curl.string, fileType, sep="")
	curl.string<-paste(curl.string, "https://foundation.iplantc.org/io-v1/io/", sep="' ")
	curl.string<-paste(curl.string, user.name, sep="")
	res<-fromJSON(system(curl.string,intern=TRUE))
	#Should output a status of success:
	res$status
}

file.rename<-function(user.name, token, oldName, newName){
#a function for uploading a file -- works!!
	curl.string<-"curl -sku"
	curl.string<-paste(curl.string, user.name, sep=" '")
	curl.string<-paste(curl.string, token, sep=":")	
	curl.string<-paste(curl.string, "-X PUT -d 'newName=", sep="' ")
	curl.string<-paste(curl.string, newName, sep="")
	curl.string<-paste(curl.string, "&action=rename", sep="")
	curl.string<-paste(curl.string, "https://foundation.iplantc.org/io-v1/io", sep="' ")
	curl.string<-paste(curl.string, user.name, sep="/")
	curl.string<-paste(curl.string,	oldName, sep="/")
	res<-fromJSON(system(curl.string,intern=TRUE))
	#Should output a status saying "success" or something:
	res$status
}

file.move<-function(user.name, token, fileName, path2newdir){
#move a file -- works!!
	curl.string<-"curl -sku"
	curl.string<-paste(curl.string, user.name, sep=" '")
	curl.string<-paste(curl.string, token, sep=":")	
	curl.string<-paste(curl.string, "-X PUT -d 'newPath=", sep="' ")
	curl.string<-paste(curl.string, user.name, sep="")
	curl.string<-paste(curl.string, path2newdir, sep="/")
	curl.string<-paste(curl.string, fileName, sep="/")
	curl.string<-paste(curl.string, "&action=move", sep="")
	curl.string<-paste(curl.string, "https://foundation.iplantc.org/io-v1/io", sep="' ")
	curl.string<-paste(curl.string, user.name, sep="/")
	curl.string<-paste(curl.string,	fileName, sep="/")
	res<-fromJSON(system(curl.string,intern=TRUE))
	#Should output a status saying "success" or something:
	res$status
}

file.delete<-function(user.name, token, file2delete){
#delete a file -- works!!
	curl.string<-"curl -sku"
	curl.string<-paste(curl.string, user.name, sep=" '")
	curl.string<-paste(curl.string, token, sep=":")	
	curl.string<-paste(curl.string, "-X DELETE", sep="' ")
	curl.string<-paste(curl.string, "https://foundation.iplantc.org/io-v1/io", sep=" ")
	curl.string<-paste(curl.string, user.name, sep="/")
	curl.string<-paste(curl.string,	file2delete, sep="/")
	res<-fromJSON(system(curl.string,intern=TRUE))
	#Should output a status saying "success" or "error"
	res$status
}

file.support<-function(user.name, token){
#lists the supported file types -- does not work! It should. 
	curl.string<-"curl -X GET -sku"
	curl.string<-paste(curl.string, user.name, sep=" '")
	curl.string<-paste(curl.string, token, sep=":")
	curl.string<-paste(curl.string, "https://foundation.iplantc.org/data-v1/data/tranforms", sep="' ")
	res<-fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse=""))
	
	res
}
##############################################################################################################


############################################DIRECTORY FUNCTIONS###############################################
list.dir<-function(user.name, token, path2directory=NULL){
#list a directory -- works!!
	if(is.null(path2directory)){
		#if path2directory is NULL then you only want to list the items in the root directory:
		curl.string<-"curl -sku"
		curl.string<-paste(curl.string, user.name, sep=" '")
		curl.string<-paste(curl.string, token, sep=":")
		curl.string<-paste(curl.string, "https://foundation.iplantc.org/io-v1/io/list/", sep="' ")
		curl.string<-paste(curl.string, user.name, sep="")
		tmp<-fromJSON(system(curl.string,intern=TRUE))
	}
	else{
		#else, you want to list some directory contained within the root, which you need to supply the path:
		curl.string<-"curl -sku"
		curl.string<-paste(curl.string, user.name, sep=" '")
		curl.string<-paste(curl.string, token, sep=":")
		curl.string<-paste(curl.string, "https://foundation.iplantc.org/io-v1/io/list/", sep="' ")
		curl.string<-paste(curl.string, user.name, sep="")
		curl.string<-paste(curl.string, path2directory, sep="/")
		tmp<-fromJSON(system(curl.string,intern=TRUE))
	}
	#Output should be a JSON listing all the items in the directory of interest, I modified to just output name and filetype
	res<-matrix(,length(tmp$result),2)
	colnames(res)<-c("name", "type")
	for (i in 1:length(tmp$result)){
		res[i,1]<-tmp$result[[i]]$name
		res[i,2]<-tmp$result[[i]]$type
	}
	res
}

make.dir<-function(user.name, token, newDirect, sub.dir=FALSE, path2parent=NULL){
#make a directory -- works!!
	if(sub.dir==FALSE){
		#if sub.dir is false, then you want to create a main directory:
		curl.string<-"curl -sku"
		curl.string<-paste(curl.string, user.name, sep=" '")
		curl.string<-paste(curl.string, token, sep=":")
		curl.string<-paste(curl.string, "-X PUT -d 'dirName=", sep="' ")
		curl.string<-paste(curl.string, newDirect, sep="")
		curl.string<-paste(curl.string, "&action=mkdir", sep="")
		curl.string<-paste(curl.string, "https://foundation.iplantc.org/io-v1/io/", sep="' ")
		curl.string<-paste(curl.string, user.name, sep="", "/")
		res<-fromJSON(system(curl.string,intern=TRUE))
	}
	else{
		#else, then you want to create a subdirectory within a parent directory or a series of parent directories:
		curl.string<-"curl -sku"
		curl.string<-paste(curl.string, user.name, sep=" '")
		curl.string<-paste(curl.string, token, sep=":")
		curl.string<-paste(curl.string, "-X PUT -d 'dirName=", sep="' ")
		curl.string<-paste(curl.string, newDirect, sep="")
		curl.string<-paste(curl.string, "&action=mkdir", sep="")
		curl.string<-paste(curl.string, "https://foundation.iplantc.org/io-v1/io/", sep="' ")
		curl.string<-paste(curl.string, user.name, sep="")
		curl.string<-paste(curl.string, path2parent, sep="/")
		res<-fromJSON(system(curl.string,intern=TRUE))
	}
	res$status
}

delete.dir<-function(user.name, token, delDirect){
#delete a (sub)directory -- works!!
	curl.string<-"curl -sku"
	curl.string<-paste(curl.string, user.name, sep=" '")
	curl.string<-paste(curl.string, token, sep=":")
	curl.string<-paste(curl.string, "-X DELETE", sep="' ")
	curl.string<-paste(curl.string, "https://foundation.iplantc.org/io-v1/io/", sep=" ")
	curl.string<-paste(curl.string, user.name, sep="")
	curl.string<-paste(curl.string, delDirect, sep="/")
	res<-fromJSON(system(curl.string,intern=TRUE))
	#Output should be a JSON listing all the items in the directory of interest:
	res$status
}
##############################################################################################################


#############################################APPLICATION FUNCTIONS############################################
app.list<-function(user.name, token){
#provides a list of applications owned or shared with user -- works!!
	curl.string<-"curl -sku"
	curl.string<-paste(curl.string, user.name, sep=" '")
	curl.string<-paste(curl.string, token, sep=":")
	curl.string<-paste(curl.string, "https://foundation.iplantc.org/apps-v1/apps/share/list", sep="' ")
	#Added in the suppressWarnings due to strange errors that print. The errors are anything of concern as far as I can tell:
	tmp<-suppressWarnings(fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse="")))
	res<-matrix(,length(tmp$result))
	colnames(res)="Application"
	for (i in 1:length(tmp$result)){
		res[i,1]<-tmp$result[[i]]$id
	}
	res
}

app.info<-function(user.name, token, application){
#provides information regarding an application -- works!!
	curl.string<-"curl -X GET -sku"
	curl.string<-paste(curl.string, user.name, sep=" '")
	curl.string<-paste(curl.string, token, sep=":")
	curl.string<-paste(curl.string, "https://foundation.iplantc.org/apps-v1/apps/share/name", sep="' ")
	curl.string<-paste(curl.string, application, sep="/")
	res<-fromJSON(system(curl.string,intern=TRUE))
	#This needs to be cleaned up. I think the relevant info is a) inputs, b) possible input parameters, and c) outputs:
	res
}
##############################################################################################################


#################################################JOB FUNCTIONS################################################

job.submit<-function(user.name, token, application, path2inputSeqs, jobName, nprocs){
#submit a job using an existing application -- works!! (I tried to minimize the input by the user here.). And yes, the 'ransom note' code here is not optimal, but it works and it is easy for me to edit:
	curl.string<-"curl -X POST -sku"
	curl.string<-paste(curl.string, user.name, sep=" '")
	curl.string<-paste(curl.string, token, sep=":")
	curl.string<-paste(curl.string, "-d 'jobName=", sep="' ")
	curl.string<-paste(curl.string, jobName, sep="")
	curl.string<-paste(curl.string, "&softwareName=", sep="")
	curl.string<-paste(curl.string, application, sep="")
	curl.string<-paste(curl.string, "&archive=1", sep="")
	curl.string<-paste(curl.string, "&inputSeqs=", sep="")
	curl.string<-paste(curl.string, path2inputSeqs, sep="")
	curl.string<-paste(curl.string, "&processorCount=", sep="")
	curl.string<-paste(curl.string, nprocs, sep="")
	curl.string<-paste(curl.string, "&archivePath=", sep="")
	curl.string<-paste(curl.string, user.name, sep="/")
	curl.string<-paste(curl.string, "analyses", sep="/")
	curl.string<-paste(curl.string, jobName, sep="/")
	curl.string<-paste(curl.string, "&requestedTime=24:00:00", sep="")
	curl.string<-paste(curl.string, "&outputFormat=fasta&mode=auto", sep="")
	curl.string<-paste(curl.string, "https://foundation.iplantc.org/apps-v1/job", sep="' ")
print(curl.string)

	res<-fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse=""))
	#Just a thought I had on the output:
	if(res$status=="success"){
		cat("Job submitted. You can check the status of your job using this id:", res$result$id, "\n")
	}
	else{
		cat("Error.", res$message, "\n")
	}
	return(res$result$id)
}

job.status<-function(user.name, token, jobID, verbose=FALSE){
#check the status of a job using the jobID from jobSubmit -- works!!
	curl.string<-"curl -X GET -sku"
	curl.string<-paste(curl.string, user.name, sep=" '")
	curl.string<-paste(curl.string, token, sep=":")
	curl.string<-paste(curl.string, "https://foundation.iplantc.org/apps-v1/job/", sep="' ")
	curl.string<-paste(curl.string, jobID, sep="")
	res<-fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse=""))
	if (res$status == "error"){
		return(paste("Error: ", res$message))
	}
	else {
		if(verbose){
			return(res)
		}
		else{
			return(res$result$status)
		}
	}
}

job.delete<-function(user.name, token, jobID){
#delete the status of a job using the jobID from jobSubmit -- works!!
	curl.string<-"curl -X DELETE -sku"
	curl.string<-paste(curl.string, user.name, sep=" '")
	curl.string<-paste(curl.string, token, sep=":")
	curl.string<-paste(curl.string, "https://foundation.iplantc.org/apps-v1/job/", sep="' ")
	curl.string<-paste(curl.string, jobID, sep="")
	print(curl.string)
	res<-fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse=""))
	res
}

job.retrieve<-function(user.name, token, jobID, file2retrieve){
#retrieves a file from the archive directory -- works!!
	job.status(user.name, token, jobID, verbose=T)->JS
	if (JS$res$status == "ARCHIVING_FINISHED") {
		curl.string<-"curl -X GET -sku"
		curl.string<-paste(curl.string, user.name, sep=" '")
		curl.string<-paste(curl.string, token, sep=":")
		curl.string<-paste(curl.string, "https://foundation.iplantc.org/io-v1/io", sep="' ")
		curl.string<-paste(curl.string, JS$result$archivePath, sep="")  #path to results directory
		curl.string<-paste(curl.string, file2retrieve, sep="/")
		curl.string<-paste(curl.string, "-o", file2retrieve)
		res2<-paste(system(curl.string,intern=TRUE),sep="", collapse="")
		print(paste("Downloaded", file2retrieve, "to", getwd(), "directory"))
	}
	else {
		warning("Job is ", JS)
	}	
}

job.output.list<-function(user.name, token, jobID){
#Lists the output from a given job -- works!!
	job.status(user.name, token, jobID, verbose=T)->JS
	if (JS$res$status == "ARCHIVING_FINISHED") {
		curl.string<-"curl -X GET -sku"
		curl.string<-paste(curl.string, user.name, sep=" '")
		curl.string<-paste(curl.string, token, sep=":")
		curl.string<-paste(curl.string, "https://foundation.iplantc.org/apps-v1/job/", sep="' ")
		#curl.string<-paste(curl.string, res$result$path, sep="' ")  #add path after res here
		curl.string<-paste(curl.string, jobID, sep="")
		curl.string<-paste(curl.string, "output/list", sep="/")
		#print(curl.string)
		res<-fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse=""))
		print(paste("There are ", length(res$result), "output files"))
		for(i in 1:length(res$result)){
			print(res$result[[i]]$name)
		}
	}
	else {
		warning("Job is ", JS)
	}	
}


job.history<-function(user.name, token, verbose=F){
#List of jobs and their status
	jobList<-c()
	curl.string<-"curl -X GET -sku"
	curl.string<-paste(curl.string, user.name, sep=" '")
	curl.string<-paste(curl.string, token, sep=":")
	curl.string<-paste(curl.string, "https://foundation.iplantc.org/apps-v1/jobs/list", sep="' ")
	print(curl.string)
	res<-fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse=""))
	for (i in 1: length(res$result)){
		job<-c(res$result[[i]]$id, res$result[[i]]$software, res$result[[i]]$status)	
		jobList<-rbind(idList, id)
	}
	if (verbose){
		return(res)
	}
	return(jobList)
}

##############################################################################################################

#################################################TNRS FUNCTIONS################################################

resolveNames<-function(names,maxPerCall=100,verbose=TRUE) {
	#takes a list of names and sends it to the iPlant TNRS site (http://tnrs.iplantcollaborative.org/)
	#names<-c("zea mays","acacia","solanum","saltea","rosa_rugoso")
	#returnedNames<-resolveNames(names)
	#print(returnedNames)
	names<-sapply(names,sub,pattern="_",replacement=" ")
	names<-sapply(names,URLencode)
	callBase<-'http://tnrs.iplantc.org/tnrsm-svc/matchNames?retrieve=best&names='
	newNames<-rep(NA,length(names))
	namesInCall<-0
	callActual<-callBase
	startingPosition<-1
	for (nameIndex in sequence(length(names))) {
		namesInCall<-namesInCall+1
		callActual<-paste(callActual,names[nameIndex],",",sep="")
		if (namesInCall==maxPerCall || nameIndex==length(names)) {
			returnedValues<-fromJSON(file=callActual)$items
			for (returnIndex in sequence(length(returnedValues))) {
				newNames[startingPosition+returnIndex-1]<-returnedValues[[returnIndex]]$nameScientific 
			}
			if(verbose) {
				print(paste("finished ",nameIndex,"of ",length(names),"names")) 
			}
			startingPosition<-nameIndex+1
			namesInCall<-0
			callActual<-callBase
		}
	}
	print("Ignore a warning message about incomplete final line")
	if (length(newNames)!=length(names)) {
		warning(paste("the input name list was",length(names),"long but the new one is ",length(newNames),"long"))
	}
	newNames<-sapply(newNames, sub, pattern=" ",replacement="_", USE.NAMES=F)
	return(newNames)
}



compareTNRS<-function(original, TNRS, verbose=TRUE) {
#takes a list of original taxonomic names (same ones given as "names in resolveNames) and compares to the returned names from TNRS
#note that names are changed back to include an "_" instead of the " " they come with out of TNRS first, so that they do not count as taxonomic name changes
	d=0
	names2<-sapply(TNRS, sub, pattern=" ",replacement="_", USE.NAMES=F)
	comp<-cbind(original, names2)
	for (i in 1: dim(comp)[1]){
		if(comp[i,1] != comp[i,2]){
			d<-d+1
			if(verbose){
				print(paste(comp[i,1], " was changed to ", comp[i,2], cat("\n")))
			}
		}
	}
	print(paste(d, "taxa changed names according to TNRS"))
}