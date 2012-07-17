
##AUTHENTICATION FUNCTIONS##
token.get<-function(user.name, user.pwd, API=c("iplant", "cipres", "tnrs")){
	if (is.character(API)) {
		if (API=="iplant") {
			curl.string<-paste("curl -X POST -sku '", user.name, ":", user.pwd, "' https://foundation.iplantc.org/auth-v1/", sep="")
			res<-fromJSON(system(curl.string,intern=TRUE))
			if (res$status == "error"){return(res$message)} #returns error
			else (return(res$result$token)) #returns token
		}
		else {
			warning("Not yet implemented")
		}
	}
}

token.renew<-function(user.name, user.pwd, token, API=c("iplant", "cipres", "tnrs")){
	if (is.character(API)) {
		if (API=="iplant") {
			curl.string<-paste("curl -X POST -sku '", user.name, ":", user.pwd, "' -d 'token=", token, "' https://foundation.iplantc.org/auth-v1/renew",sep="")
			res<-fromJSON(system(curl.string,intern=TRUE))		
			res$status #Outputs a message renewal success
		}
		else {
			warning("Not yet implemented")
		}
	}
}
##END##


##FILE AND DATA FUNCTIONS##
file.upload<-function(user.name, token, file2upload, fileType){
	curl.string<-paste("curl -sku '", user.name, ":", token, "' -F 'fileToUpload=@", file2upload, "' -F 'fileType=", fileType, "' https://foundation.iplantc.org/io-v1/io/", user.name, sep="")
	res<-fromJSON(system(curl.string,intern=TRUE))
	res$status #Should output a status of success:
}

file.rename<-function(user.name, token, oldName, newName){
	curl.string<-paste("curl -sku '", user.name, ":", token, "' -X PUT -d 'newName=", newName, " &action=rename", "' https://foundation.iplantc.org/io-v1/io/", user.name, "/", oldName, sep="")
	res<-fromJSON(system(curl.string,intern=TRUE))
	if (res$status == "error"){return(paste(res$status, ":", res$message))}
	else {return(res$status)} 
}

file.move<-function(user.name, token, fileName, path2newdir){
	curl.string<-paste("curl -sku '", user.name, ":", token, "' -X PUT -d 'newPath=", user.name, "/", path2newdir, "/", fileName, "&action=move", "' https://foundation.iplantc.org/io-v1/io/", user.name, "/", fileName, sep="")
	res<-fromJSON(system(curl.string,intern=TRUE))
	if (res$status == "error"){return(paste(res$status, ":", res$message))}
	else {return(res$status)} 
}

file.delete<-function(user.name, token, file2delete){
	curl.string<-paste("curl -sku '", user.name, ":", token, "' -X DELETE", " https://foundation.iplantc.org/io-v1/io/", user.name, "/", file2delete, "/", sep="")
	res<-fromJSON(system(curl.string,intern=TRUE))
	if (res$status == "error"){return(paste(res$status, ":", res$message))}
	else {return(res$status)} 
}

file.support<-function(user.name, token){ #lists the supported file types -- does not work! It should. 

	curl.string<-"curl -X GET -sku"
	curl.string<-paste(curl.string, user.name, sep=" '")
	curl.string<-paste(curl.string, token, sep=":")
	curl.string<-paste(curl.string, "https://foundation.iplantc.org/data-v1/data/tranforms", sep="' ")
	res<-fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse=""))
	
	res
}
##END##


##DIRECTORY FUNCTIONS##
list.dir<-function(user.name, token, path2directory=""){ 
	curl.string<-paste("curl -sku '", user.name, ":", token, "' https://foundation.iplantc.org/io-v1/io/list/", user.name, "/", path2directory, sep="")
	tmp<-fromJSON(system(curl.string,intern=TRUE))

	res<-matrix(,length(tmp$result),2)
	colnames(res)<-c("name", "type")
	for (i in 1:length(tmp$result)){
		res[i,1]<-tmp$result[[i]]$name
		res[i,2]<-tmp$result[[i]]$type
	}
	return(res)
}

make.dir<-function(user.name, token, newDirect, path2parent=""){
	curl.string<-paste("curl -sku '", user.name, ":", token, "' -X PUT -d 'dirName=", newDirect, "&action=mkdir' ", "https://foundation.iplantc.org/io-v1/io/", user.name, "/", path2parent, sep="")
	res<-fromJSON(system(curl.string,intern=TRUE))
	if (res$status == "error"){return(paste(res$status, ":", res$message))}
	else {return(res$status)} 
}

delete.dir<-function(user.name, token, delDirect){
	curl.string<-paste("curl -sku '", user.name, ":", token, "' -X DELETE https://foundation.iplantc.org/io-v1/io/", user.name, "/", delDirect, sep="")
	res<-fromJSON(system(curl.string,intern=TRUE))
	if (res$status == "error"){return(paste(res$status, ":", res$message))}
	else {return(res$status)} 
}
##END##


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
		print(paste("Error: ", res$message))
		if(verbose){return(res)}
	}
	else {
		if(verbose){return(res)}
		else{
			return(res$result$status)
		}
	}
}

job.delete<-function(user.name, token, jobID){
#delete the status of a job using the jobID from jobSubmit -- works!!
	for(job in 1:length(jobID)){
		curl.string<-"curl -X DELETE -sku"
		curl.string<-paste(curl.string, user.name, sep=" '")
		curl.string<-paste(curl.string, token, sep=":")
		curl.string<-paste(curl.string, "https://foundation.iplantc.org/apps-v1/job/", sep="' ")
		curl.string<-paste(curl.string, jobID[job], sep="")
		print(curl.string)
		res<-fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse=""))
		res
	}
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
	combRes<-vector("list", length=length(jobID))
	names(combRes)<-paste("jobID", jobID, sep="")
	for (job in 1:length(jobID)){
		files<-c()
		job.status(user.name, token, jobID[job], verbose=T)->JS
		if (JS$res$status == "ARCHIVING_FINISHED") {
			curl.string<-"curl -X GET -sku"
			curl.string<-paste(curl.string, user.name, sep=" '")
			curl.string<-paste(curl.string, token, sep=":")
			curl.string<-paste(curl.string, "https://foundation.iplantc.org/apps-v1/job/", sep="' ")
			#curl.string<-paste(curl.string, res$result$path, sep="' ")  #add path after res here
			curl.string<-paste(curl.string, jobID[job], sep="")
			curl.string<-paste(curl.string, "output/list", sep="/")
			res<-fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse=""))
			print(paste("There are ", length(res$result), "output files for job", jobID))
			for(i in 1:length(res$result)){
				files<-append(files, res$result[[i]]$name)
			}
		}
		else {files<-paste("Job is ", JS$res$status)}
	combRes[[job]]<-files
	}
	return(combRes)	
}


job.history<-function(user.name, token, verbose=F){
#List of jobs and their status
	jobList<-c()
	curl.string<-"curl -X GET -sku"
	curl.string<-paste(curl.string, user.name, sep=" '")
	curl.string<-paste(curl.string, token, sep=":")
	curl.string<-paste(curl.string, "https://foundation.iplantc.org/apps-v1/jobs/list", sep="' ")
	#print(curl.string)
	res<-fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse=""))
	if (verbose){
		return(res)
	}
	if(length(res$result) != 0){
		for (i in 1: length(res$result)){
			job<-c(res$result[[i]]$id, res$result[[i]]$software, res$result[[i]]$status)	
			jobList<-rbind(jobList, job)
			colnames(jobList)<-c("jobID", "application", "status")
		}	
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