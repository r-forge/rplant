require(rjson)

#a function for obtaining a token -- works!!
token.get<-function(user.name, user.pwd, API=match.arg("iPlant, cipres, TNRS")){
	#Pastes together the string used to call a token:
	#Seems clunky like this, but only way to incorporate user-supplied arguments.
	if (API=="iPlant") {
		curl.string<-"curl -X POST -sku" 
		curl.string<-paste(curl.string, user.name, sep=" '")
		curl.string<-paste(curl.string, user.pwd, sep=":")
		#Is the url always going to be the one used here?
		curl.string<-paste(curl.string, "https://foundation.iplantc.org/auth-v1/", sep="' ")
		res<-fromJSON(system(curl.string,intern=TRUE))
		#Seems like the token is the only thing of interest to output:
		return(res$result$token)
	}
	else {
		warning("Not yet implemented")
	}
}

#a function for renewing a token -- works!!
token.renew<-function(user.name, user.pwd, token){
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

#a function for uploading a file -- works!!
file.upload<-function(user.name, token, fileToUpload, fileType){
	curl.string<-"curl -sku"
	curl.string<-paste(curl.string, user.name, sep=" '")
	curl.string<-paste(curl.string, token, sep=":")
	curl.string<-paste(curl.string, "-F 'fileToUpload=", sep="' ")
	curl.string<-paste(curl.string, fileToUpload, sep="@")
	curl.string<-paste(curl.string, "-F 'fileType=", sep="' ")
	curl.string<-paste(curl.string, fileType, sep="")
	curl.string<-paste(curl.string, "https://foundation.iplantc.org/io-v1/io/", sep="' ")
	curl.string<-paste(curl.string, user.name, sep="")
	res<-fromJSON(system(curl.string,intern=TRUE))
	#Should output a status of success:
	res$status
}

#a function for uploading a file -- works!!
file.rename<-function(user.name, token, oldName, newName){
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

#move a file -- works!!
file.move<-function(user.name, token, fileName, path2directory){
	curl.string<-"curl -sku"
	curl.string<-paste(curl.string, user.name, sep=" '")
	curl.string<-paste(curl.string, token, sep=":")	
	curl.string<-paste(curl.string, "-X PUT -d 'newPath=", sep="' ")
	curl.string<-paste(curl.string, user.name, sep="")
	curl.string<-paste(curl.string, path2directory, sep="/")
	curl.string<-paste(curl.string, fileName, sep="/")
	curl.string<-paste(curl.string, "&action=move", sep="")
	curl.string<-paste(curl.string, "https://foundation.iplantc.org/io-v1/io", sep="' ")
	curl.string<-paste(curl.string, user.name, sep="/")
	curl.string<-paste(curl.string,	fileName, sep="/")
	res<-fromJSON(system(curl.string,intern=TRUE))
	#Should output a status saying "success" or something:
	res$status
}

#delete a file -- works!!
file.delete<-function(user.name, token, fileDeleted){
	curl.string<-"curl -sku"
	curl.string<-paste(curl.string, user.name, sep=" '")
	curl.string<-paste(curl.string, token, sep=":")	
	curl.string<-paste(curl.string, "-X DELETE", sep="' ")
	curl.string<-paste(curl.string, "https://foundation.iplantc.org/io-v1/io", sep=" ")
	curl.string<-paste(curl.string, user.name, sep="/")
	curl.string<-paste(curl.string,	fileDeleted, sep="/")
	res<-fromJSON(system(curl.string,intern=TRUE))
	#Should output a status saying "success" or "error"
	res$status
}

#make a directory -- works!!
make.dir<-function(user.name, token, newDirect, sub.dir=FALSE, path2parent=NULL){
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

#list a directory -- works!!
list.dir<-function(user.name, token, path2directory=NULL){
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
	#Output should be a JSON listing all the items in the directory of interest:
	res<-matrix(,length(tmp$result),2)
	colnames(res)<-c("name", "type")
	for (i in 1:length(tmp$result)){
		res[i,1]<-tmp$result[[i]]$name
		res[i,2]<-tmp$result[[i]]$type
	}
	res
}

#delete a (sub)directory -- works!!
delete.dir<-function(user.name, token, dirDelete, path2directory=NULL){
	if(is.null(path2directory)){
		#if NULL then directory to be deleted is in the parent directory
		curl.string<-"curl -sku"
		curl.string<-paste(curl.string, user.name, sep=" '")
		curl.string<-paste(curl.string, token, sep=":")
		curl.string<-paste(curl.string, "-X DELETE", sep="' ")
		curl.string<-paste(curl.string, "https://foundation.iplantc.org/io-v1/io/", sep=" ")
		curl.string<-paste(curl.string, user.name, sep="")
		curl.string<-paste(curl.string, dirDelete, sep="/")
		res<-fromJSON(system(curl.string,intern=TRUE))
	}
	else{
		#else then directory to be deleted is nested somewhere in the parent directory, user-supplies the path:
		curl.string<-"curl -sku"
		curl.string<-paste(curl.string, user.name, sep=" '")
		curl.string<-paste(curl.string, token, sep=":")
		curl.string<-paste(curl.string, "-X DELETE", sep="' ")
		curl.string<-paste(curl.string, "https://foundation.iplantc.org/io-v1/io/", sep=" ")
		curl.string<-paste(curl.string, user.name, sep="")
		curl.string<-paste(curl.string, path2directory, sep="/")
		curl.string<-paste(curl.string, dirDelete, sep="/")
		res<-fromJSON(system(curl.string,intern=TRUE))
	}
	#Output should be a JSON listing all the items in the directory of interest:
	res$status
}

#provides a list of applications owned or shared with user -- works!!
app.list<-function(user.name, token){
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

#provides information regarding an application:
app.info<-function(user.name, token, application){
	curl.string<-"curl -X GET -sku"
	curl.string<-paste(curl.string, user.name, sep=" '")
	curl.string<-paste(curl.string, token, sep=":")
	curl.string<-paste(curl.string, "https://foundation.iplantc.org/apps-v1/apps/share/name", sep="' ")
	curl.string<-paste(curl.string, application, sep="/")
	res<-fromJSON(system(curl.string,intern=TRUE))
	#This needs to be cleaned up. I think the relevant info is a) inputs, b) possible input parameters, and c) outputs:
	res
}

#submit a job using an existing application -- works!! (I tried to minimize the input by the user here. Will need more works
#at customizing as we move forward:
job.submit<-function(user.name, token, application, inputSeqs, jobName, nprocs){
	curl.string<-"curl -X POST -sku"
	curl.string<-paste(curl.string, user.name, sep=" '")
	curl.string<-paste(curl.string, token, sep=":")
	curl.string<-paste(curl.string, "-d 'jobName=", sep="' ")
	curl.string<-paste(curl.string, jobName, sep="")
	curl.string<-paste(curl.string, "&softwareName=", sep="")
	curl.string<-paste(curl.string, application, sep="")
	curl.string<-paste(curl.string, "&archive=1", sep="")
	curl.string<-paste(curl.string, "&inputSeqs=", sep="")
	curl.string<-paste(curl.string, inputSeqs, sep="")
	curl.string<-paste(curl.string, "&processorCount=", sep="")
	curl.string<-paste(curl.string, nprocs, sep="")
	curl.string<-paste(curl.string, "&archivePath=", sep="")
	curl.string<-paste(curl.string, user.name, sep="/")
	curl.string<-paste(curl.string, "analyses", sep="/")
	curl.string<-paste(curl.string, jobName, sep="/")
	curl.string<-paste(curl.string, "&requestedTime=01:00:00", sep="")
	curl.string<-paste(curl.string, "&outputFormat=fasta&mode=auto", sep="")
	curl.string<-paste(curl.string, "https://foundation.iplantc.org/apps-v1/job", sep="' ")
	res<-fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse=""))
	#Verbose output at this point. Status is a must, but everything else seems worth outputting as well
	res
}

job.status<-function(user.name, token, jobName){
	curl.string<-"curl -X GET -sku"
	curl.string<-paste(curl.string, user.name, sep=" '")
	curl.string<-paste(curl.string, token, sep=":")
	curl.string<-paste(curl.string, "https://foundation.iplantc.org/apps-v1/job", sep="' ")
	curl.string<-paste(curl.string, jobName, sep="/")
	res<-fromJSON(paste(system(curl.string,intern=TRUE),sep="", collapse=""))
	res
}

#NOTES:
#Goal to login, upload a file, run muscle, and suck it back up in R, possibly even run a quick parsimony run to show how we can
#interface between the two platforms.
