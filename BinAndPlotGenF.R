#Analysis of Enterococcus Faecalis SIM images (converted to plot profiles
#by user in ImageJ with the LineMeasure.ijm macro) to observe patterns of 
#cell wall propagation from septum.

#Ratio3/30 represents the width of the cell along this profile (or axis).
#Later stages of bacterial division exhibit more 'pinching', so bins
#each image based on this into Early, Middle, or Late fission.

#Align profiles to septum (center of fission) and compare signal patterns
#along axis profile by bin.

#Output is a .png plot of the average wide signal, the average narrow signal
#and the ratio signal for all images of each bin. Additionally a list of the
#cells is given with their assigned bin is exported. Input and ouput must be
#folders in working directory

#-------------

#Takes ImageJ processed cell as a table. Identifies center of Axis (nearest zero)
#and categorizes into a stage of fission based on the ratio of narrow:wide at center.
#Adds all to Line Data and to LineDetails with centerpoint and fission stage details
ConcFiles <- function(input, filename) {
	#reads file
	CurrLine <- read.table(paste(input, filename, sep=""), header=T)
	
	#adds current file to list of tables
	LineData[[i]] <<-CurrLine
	
	#Get index of where axis is close to zero or zero.
  #MidInd <- which(abs(CurrLine$Axis) <= 0.5)[1]
	MidInd <- which(trunc(CurrLine$Axis)==0)[[1]]

	# Avg narrow:wide ratio of 5 points nearest that point
	QualRange<-((MidInd-2):(MidInd+2))
	MidMean <- mean(CurrLine[QualRange, "Rat3.30"])
	
	#determine bin
	if (MidMean > 150 & MidMean < 200) {
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

#input and output paths
input = "input/"
output = "output/"

# Categorizes all processed image data from input folder and adds
FileList = list.files(input)
for (i in 1:length(FileList)) {
	ConcFiles(input, FileList[i])
}

#exports LineDetails
#write.table(LineData, paste(output, "LineData.txt", sep=""))
write.csv(LineDetails, paste(output, "LineDetails.txt", sep=""))

#Preparing Plot

#Aggregates all Axis, Avg30, Avg3, and Ratio data into individual lists of lists
#so that limits of x and y axis for all plots can be specified
Axes <- lapply(LineData, "[[", "Axis")
Avg30s <- lapply(LineData, "[[", "Avg30")
Avg3s <- lapply(LineData, "[[", "Avg3")
Rat3.30s <-lapply(LineData, "[[", "Rat3.30")
Axeslim <- range(unlist(Axes))
Avg30slim <- range(unlist(Avg30s))
Rat3.30slim <- range(unlist(Rat3.30s))
Avg3slim <- range(unlist(Avg3s))

#Defines list of indices for each bin
Early <- which(LineDetails$bin =="E")
Middle <- which(LineDetails$bin =="M")
Late <- which(LineDetails$bin =="L")

#setup export and grid of plots
#pdf(paste(output, "FullBinPlot.pdf", sep=""), width=8,height=6)
png(paste(output, "FullBinPlot.png", sep=""), width=10, height=10, units="in", res=300)
par(mfrow=c(3,3))

#plot first row of plots
  #setup plot parameters
plot(NA,xlim=Axeslim, ylim=Avg30slim, main="Early, Avg30Pix", xlab="Axis", ylab="Fluor. Int.")
  #mapply-multivariate apply- to iterate along elements of lists( x and y and color and style)
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
