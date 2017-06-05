//User defines input, output folders. User aligns cell in image to linear or angular axis.
//Returns intensity along axis at narrow or wide pixel widths  from midpoint along wih narrow/wide ratio.

function LinMeas(input, output, filename) {

	open(input + filename);
	run("Line Width...", "line=30"); 
	waitForUser("Select view from stack with axis of bacterial fission parallel to field.");
	Dialog.create("Axis type selection.");
	Dialog.addChoice("Is the axis of bacterial fission linear or along a segmented line?", newArray("linear", "segmented", "skip"));
	Dialog.show();
	choice = Dialog.getChoice();
	if (choice == "segmented") {
	} else if (choice == "linear") {

	} else {
		close();
		return;
	}
	run("Clear Results");

// for determining joint of segmented line
	if (choice=="segmented") {
		setTool("polyline");
		waitForUser("Draw a wide segmented line along central axis of fission, then click OK.");

//get 3 points defining two segment line (x and y coordinates)
		getSelectionCoordinates(Prex, Prey);
		WideProf = getProfile();

//array of splined points created by profile of wide segmented line (x and y coordinates)
		run("Fit Spline", "straighten"); 
		getSelectionCoordinates(Posx, Posy);

//find (mid)point in length-adjusted spline (Posx) closest to midpoint of segmented line (Prex)
		BestDist = 1000;
		midpoint = 1000;
		for (i=0; i<Posx.length; i++) {
			dist = sqrt(pow(Prex[1]-Posx[i], 2)+ pow(Prey[1]-Posy[i], 2));
			
			if (dist < BestDist) {
				BestDist = dist;
				midpoint = i;
			}
		//print (i, "Prex[1]", Prex[1], "Prey[1]", Prey[1]);
		//print (i, "Posx[i]", Posx[i], "Posy[i]", Posy[i]);
		//print (i, "Current Midpoint", midpoint, "BestDist", BestDist, "CurrentDist", dist);
		}

//Segmented Linear Axis
		for (i=0; i<WideProf.length; i++) {
			setResult("Axis", i, i-(midpoint));
		}
//Single Linear Axis
	} else if (choice=="linear") {	
		setTool("line");
		waitForUser("Draw a line along the central axis of fission, then click OK.");
		WideProf = getProfile();
		for (i=0; i<WideProf.length; i++)
			setResult("Axis", i, i-WideProf.length/2);
//for skipping cel
	} else {
		close();
		return;
	}
//All lines wide
	for (i=0; i<WideProf.length; i++)
		setResult("Avg30", i, WideProf[i]);

//All lines narrow
	Roi.setStrokeWidth(3);
	NarrProf = getProfile();
	for (i=0; i<NarrProf.length; i++)
		setResult("Avg3", i, NarrProf[i]);

//All lines ratio
	for (i=0; i<NarrProf.length; i++) {
		//setResult("Rat3/30", i, getResult("Avg3", i)/getResult("Avg30", i)*100);
		Ratio = (getResult("Avg3", i)/getResult("Avg30", i)*100);
		//Sets 0/0 to 0, not NaN!
		if (isNaN(Ratio)) {
			setResult("Rat3/30", i, 0);
		} else {
			setResult("Rat3/30", i, Ratio);
		}
	}
	updateResults;
	saveAs("Measurements", output+filename+".txt");
	close();
}

input = getDirectory("Choose folder with files to measure");
output = getDirectory("Choose output folder.");
//input="C:\\Users\\Hanglab\\Desktop\\Jake\\SIM\\Test1\\"
//output="C:\\Users\\Hanglab\\Desktop\\Jake\\SIM\\Test2\\"

list = getFileList(input);
for (i = 0; i < list.length; i++)
        LinMeas(input, output, list[i]);

