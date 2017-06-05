#Fall2014 - Hang Lab
#Analysis of Enterococcus Faecalis SIM images (converted to plot profiles
#by user in ImageJ) to observe patterns of staining by Click Chem to
#propargylglycine (metabolic labeling).

#Ratio3/30 represents the width of the cell along this profile (or axis).
#Later stages of bacterial division exhibit more 'pinching', so bining
#each image based on this.

#Set out to compare staining patterns along axis profile for datasets with
#and without SagA expression (separated by bin).

#-------------

#Build list of raw data from each line and data frame of index, name, and BinInfo
#for a list of txt files in a folder.
ConcFiles <- function(input, filename) {
	#open current filename at imports
	CurrLine <- read.table(paste(input, filename, sep=""), header=T)
	
	#print(CurrData)
	LineData[[i]] <<-CurrLine
	
	#Get index axis at zero. Avg ratio of 5 ponits nearest that point
#	if (length((CurrLine$Axis))%%2 ==0) {
#		MidInd<-which((CurrLine)[1]==0)
#	} else {
#		MidInd<-which((CurrLine)[1]==0.5)
#	}
	MidInd <- which(trunc(CurrLine$Axis)==0)[[1]]
	
	#find center
	QualRange<-((MidInd-2):(MidInd+2))
	MidMean <- mean(CurrLine[QualRange, "Rat3.30"])
	
	#determine bin
	if (MidMean >150 & MidMean < 200) {
		bin <- "M"
	} else if (MidMean >=200) {
		bin <- "L"
	} else if (MidMean <=150) {
		bin <- "E"
	}
	#attach line vector to dataframe(LineDetails)
	rbind(LineDetails, data.frame(i, filename, MidInd, MidMean, bin)) ->>LineDetails
}

#initiate empty raw data list and dataframe
LineData<-list(NULL)
LineDetails <-data.frame(NULL)

#input and output files and iterating over files
input = "/home/jacob/documents/Test2/"
output = "/home/jacob/documents/Test3/"
FileList = list.files(input)
for (i in 1:length(FileList)) {
	ConcFiles(input, FileList[i])
}

#export LineData(necessary??) and LineDetails
#write.table(LineData, paste(output, "LineData.txt", sep=""))
write.csv(LineDetails, paste(output, "LineDetails.txt", sep=""))

#separate list of dataframe (LineData) into list of columns (list of list)
Axes <-lapply(LineData, "[[", 1)
#not Avg30 anymore (would be column 2, actually the userdefined linewidth
Avg30s <-lapply(LineData, "[[", 3)
Avg3s <-lapply(LineData, "[[", 5)
Rat3.30s <-lapply(LineData, "[[", 7)

#Defines list of indices in LineData for each bin
Early <- which(LineDetails$bin =="E")
Middle <- which(LineDetails$bin =="M")
Late <- which(LineDetails$bin =="L")

#assign shared limits of x and y axis for all plots
Axeslim<-range(unlist(Axes))
Avg30slim<-range(unlist(Avg30s))
Rat3.30slim<-range(unlist(Rat3.30s))
Avg3slim<-range(unlist(Avg3s))

#setup export and grid of plots
#pdf(paste(output, "FullBinPlot.pdf", sep=""), width=8,height=6)
png(paste(output, "FullBinPlot.png", sep=""), width=10, height=10, units="in", res=300)
par(mfrow=c(3,3))

#plot first row of plots
#setup plot parameters
plot(NA,xlim=Axeslim, ylim=Avg30slim, main="Early, Avg30Pix", xlab="Axis", ylab="Fluor. Int.")
#mapply-multivariate apply- to iterate along elements of lists( x and y and color and style
mapply(points, x=Axes[Early], y=Avg30s[Early], type="l", col=c(1:length(Early)), lty=c(1:length(Early)))
plot(NA,xlim=Axeslim, ylim=Avg3slim, main="Early, Avg3Pix", xlab="Axis", ylab="Fluor. Int.")
mapply(points, x=Axes[Early], y=Avg3s[Early], type="l", col=c(1:length(Early)), lty=c(1:length(Early)))
plot(NA,xlim=Axeslim, ylim=Rat3.30slim, main="Early, Ratio3/30", xlab="Axis", ylab="Fluor. Int.")
mapply(points, x=Axes[Early], y=Rat3.30s[Early], type="l", col=c(1:length(Early)), lty=c(1:length(Early)))

#second row
plot(NA,xlim=Axeslim, ylim=Avg30slim, main="Middle, Avg30Pix", xlab="Axis", ylab="Fluor. Int.")
mapply(points, x=Axes[Middle], y=Avg30s[Middle], type="l", col=c(1:length(Middle)), lty=c(1:length(Middle)))
plot(NA,xlim=Axeslim, ylim=Avg3slim, main="Middle, Avg3Pix", xlab="Axis", ylab="Fluor. Int.")
mapply(points, x=Axes[Middle], y=Avg3s[Middle], type="l", col=c(1:length(Middle)), lty=c(1:length(Middle)))
plot(NA,xlim=Axeslim, ylim=Rat3.30slim, main="Middle, Ratio3/30", xlab="Axis", ylab="Fluor. Int.")
mapply(points, x=Axes[Middle], y=Rat3.30s[Middle], type="l", col=c(1:length(Middle)), lty=c(1:length(Middle)))

#third row
plot(NA,xlim=Axeslim, ylim=Avg30slim, main="Late, Avg30Pix", xlab="Axis", ylab="Fluor. Int.")
mapply(points, x=Axes[Late], y=Avg30s[Late], type="l", col=c(1:length(Late)), lty=c(1:length(Late)))
plot(NA,xlim=Axeslim, ylim=Avg3slim, main="Late, Avg3Pix", xlab="Axis", ylab="Fluor. Int.")
mapply(points, x=Axes[Late], y=Avg3s[Late], type="l", col=c(1:length(Late)), lty=c(1:length(Late)))
plot(NA,xlim=Axeslim, ylim=Rat3.30slim, main="Late, Ratio3/30", xlab="Axis", ylab="Fluor. Int.")
mapply(points, x=Axes[Late], y=Rat3.30s[Late], type="l", col=c(1:length(Late)), lty=c(1:length(Late)))

#finish export of plot.png
dev.off()
