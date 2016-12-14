/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package simpleviewshed;

import java.awt.geom.Ellipse2D;
import java.awt.geom.Point2D;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FilenameFilter;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;

/**
 *
 * @author SMI
 */
public class Main {

    //bh_raster is same but with building heights added. Will use both where possible.
    public static float[][] raster, bh_raster;
    //switch for trying to use building heights (turn off for e.g. centroid runs that have none)
    public static boolean weHaveBuildingHeightData = true;

    //If building height data is present for this batch...
    public static boolean thisBatchHasBuildingHeights = false;
    //This marks whether a run is taking place using those building heights, for data output porpoises.
    public static boolean buildingHeightRun = false;

    //boolean[][] viewShed;
    //BresenhamLine results. Will contain one or two ArrayLists of points
    //Two if using building heights
    ArrayList<Float>[] heights;
    float[] fheights;
    //Line of sight between observer and target
    float[] lineOfSight;

    Ellipse2D viewCircle;
    int observerX, observerY, targetX, targetY;
    float observerHeight;
    //will need to be scaled down by five but leave here for distance band calcs
//    static float radius = 15000;
    static float radius = 1000;

    boolean canISeeYou = false;

//    ArrayList<Point> observers = new ArrayList<Point>();
//    ArrayList<Point> targets = new ArrayList<Point>();
    //allHouses and allHouses_BH will store all per-property results 
    //for vanilla DEM and building-height DEM results
    DataStore targets, observers, allHouses, allHouses_BH;
    ArrayList<TargetPoint> targetsInRadius = new ArrayList<>();

    //int batchcount = 1;
    double distance2D;
    TargetPoint house;
    String fileName;

    //current file batch number.
    //Will start from one if first run
    //or get batch number from serialised data and then start with next batch
    int batchNumber = 1;
    //if previous data has been serialised, pick up from where we left off
    boolean useSerialisedIfAvailable = false;
    boolean useSampleHousingData = false;
    //Set to batch number point to serialise results. Minus one to turn off.
    int serialiseResultsAsWeGo = -1;

    //Index adjust for the target file.
    //E.g. single sales start at id = 1000000 at the zeroth row index
    int indexAdjust = 0;

    //Root folder (relative to java viewshed folder) for getting/setting data
    //Use different folder for e.g. centroids analysis vs points analysis
    public static String rootfolder = "data";

    public Main() {

        //can be any of the three data folders, just need to get the number of files in there
        File folder = new File(rootfolder + "/observers");
//        File[] listOfFiles = folder.listFiles();

        //http://stackoverflow.com/questions/2102952/listing-files-in-a-directory-matching-a-pattern-in-java
        List<File> list = Arrays.asList(folder.listFiles(new FilenameFilter() {
            @Override
            public boolean accept(File dir, String name) {
                return name.endsWith(".csv");
            }
        }));

        System.out.println(list.size() + " file groups to process.");

        long startTime = System.currentTimeMillis();

        //model run
        if (true) {

            //Which I'm doing, if I remember rightly, so I can add results to ALL housing
            //flag for "building height data"; only used if loading serialised
            //Otherwise it'll just load the raw data twice
//            allHouses = loadHousingData(false);
            if (weHaveBuildingHeightData) {
                allHouses_BH = loadHousingData(true);
//                System.out.println("loaded all housing data twice. Total size: " + allHouses.points.size() + "," + allHouses_BH.points.size());
            } else {
                System.out.println("loaded all housing data once (no building data). Total size: " + allHouses.points.size());
            }

            for (int fileIndex = batchNumber; fileIndex < list.size() + 1; fileIndex++) {

                //There'll always be one non-building-height run
//                buildingHeightRun = false;
                System.out.println("Loading fileset " + fileIndex + ", " + ((System.currentTimeMillis() - startTime) / 1000) + " seconds elapsed");

                //memory checks
                System.out.println("Total memory: " + (Runtime.getRuntime().maxMemory()) / 1073741824f
                        + "gb , available memory: "
                        + (Runtime.getRuntime().freeMemory()) / 1073741824f + "gb");

                loadRasterData(fileIndex);
                //Flag: if true, try and load sample version of batch file (e.g. "47sample.csv")
                //Sampling done in viewshed_R/sampleHousingTargetFiles.R
                loadPointsData(fileIndex, useSampleHousingData);

                //if available, re-run using building heights
                if (thisBatchHasBuildingHeights) {
//                if (false) {

                    buildingHeightRun = true;

                    //Easiest way to re-set the points. Not large files...
                    //Flag: if true, try and load sample version of batch file (e.g. "47sample.csv")
                    //Sampling done in viewshed_R/sampleHousingTargetFiles.R
                    loadPointsData(fileIndex, useSampleHousingData);

                    interViz(allHouses_BH);

                }

                batchNumber++;//to get access to loop index in other methods

            }//end for

        //output results
            try {
                DataOutput.outputData(allHouses_BH, rootfolder + "/output/resultFile.csv");
//                DataOutput.outputData(allHouses_BH, "data/output/allHouses_buildingHeights_CathkinBraes125mTest_BH_edgeWalkTest.csv");
            } catch (Exception e) {
                System.out.println("Data output booboo: " + e);
            }

        }//end if true/false

    }

    private void interViz(DataStore d) {

        int obcount = 0, targetcount = 0;
        long before = System.currentTimeMillis();

        ObserverPoint observer;

        for (Point ob : observers.points) {

            observer = (ObserverPoint) ob;

            //op = (ObserverPoint) ob;
            observerX = (int) ob.x;
            observerY = (int) ob.y;
            observerHeight = ob.height;

            //e.g. 15000 metres needs scaling down to 1 unit per 5 metres to match raster
            //subtract 5cm too, in case a target is exactly on 15km. Which one has been...
            float scaledRadius = (radius - 0.05f) / 5;

            viewCircle = new Ellipse2D.Float(
                    (float) observerX - scaledRadius,
                    (float) observerY - scaledRadius,
                    scaledRadius * 2, scaledRadius * 2);

            //subset targets in view circle - passing by reference, will update correctly
            targetsInRadius.clear();
            TargetPoint p;

            System.out.println("Houses total: " + targets.points.size());

            for (Point target : targets.points) {

                p = (TargetPoint) target;

                if (viewCircle.contains(p.x, p.y)) {
                    targetsInRadius.add(p);
                }
            }

            System.out.println("target subset in " + radius + " metre view: " + targetsInRadius.size());

            int timer = 0;

//        for (int i = 0; i < targets.points.size(); i++) {
            for (TargetPoint target : targetsInRadius) {

                //Get reference to parent house where we'll add data
                house = (TargetPoint) d.points.get(target.id - indexAdjust);

                //get matching house object where we'll store the data
                //note the times 5 to take it back up to metres again!
                distance2D = target.twoDLocation.distance(ob.twoDLocation) * 5;

//                heights = BresenhamLine.findLine(observerX, observerY, (int) target.x, (int) target.y);
                //Try reversing order. Should be exactly same outcome.
                heights = BresenhamLine.findLine((int) target.x, (int) target.y, observerX, observerY, distance2D);

                //System.out.println("length of returned heights array:" + heights.length);
                //lineOfSight = getLineOfSight(100, 2);
                //Oops: 5 metre units. That was a half-km high turbine and 10 metre high human!
                //System.out.println("using ob and target heights: " + observerHeight + ", " + target.height);
                //Needs to be in the same order as "heights"
                //In this case from target to observer - not very logical-sounding!
                //if building height run, vanilla DEM will be in index 1. Slight hacking now!
                //We need to base the line of sight on vanilla DEM - 
                //otherwise we're looking from 2 metres above a roof!                
                //Update: no it won't. The start-point at the building is set to 
                //The same height as vanilla in BresenhamLine.
                //And large turbines are of course never on top of buildings!
                //So it was correct but perhaps for the wrong reason -
                //I'll leave it picking a line on the vanilla DEM. (Is still working.)
                lineOfSight = getLineOfSight(
                        (buildingHeightRun ? 1 : 0),
                        ((float) target.height / 5f), (observerHeight / 5f));
//                lineOfSight = getLineOfSight(1, (observerHeight / 5f), ((float) target.height / 5f));

                //Apply distance to nearest to parent property object
                if (distance2D < house.distanceToNearest) {
                    house.distanceToNearest = distance2D;
                }

                //Count of all distances in 1km distance bands
                //see testIndexCount method
//                if(distance2D /1000 == 15) {
////                    System.out.println("this broke it! " + distance2D);
//                    //It was exactly 15km. There should be none over that, so...
//                    //substract 50cm. If anything is still over this, we'll know about it.
//                    distance2D -= 0.5;
//                }
                house.allObsDistanceBandCounts[(int) distance2D / 1000]++;

                boolean isThisOneSeen = false;

                //enter index of bresenham line to use
                if (canISeeYou(0)) {

                    house.amISeen = true;
                    //For line-of-sight data output
                    target.amISeen = true;
                    //Or not. Let's try this one instead
                    isThisOneSeen = true;

                    if (distance2D < house.distanceToNearestVisible) {
                        house.distanceToNearestVisible = distance2D;
                    }

                    //Add observer ID that can be seen
                    house.ICanSeeThisObserver.add(observer.id);

                    //Count of lines of sight 1km per distance band
                    //see testIndexCount method
                    house.visibleObsDistanceBandCounts[(int) distance2D / 1000]++;

                }

                //Distance from observer to target, both along ground and accounting for height
                //Scale back up to metres again!
                //target.distance3D = target.distance(ob) * 5;
                //Add distance to observer, regardless of visible or not
                //Just use 2d distance for now
                //Order will match observer file order
                house.distanceToObservers2D.add(distance2D);

                //look at some
//                if (target.amISeen) {
//                    System.out.println("targetcount: " + targetcount);
//                    if (targetcount++ < 20) {
//                if (targetcount++ % 1 == 0) {
                if (targetcount++ % 100000 == 0) {
                    try {

                        String type = (buildingHeightRun ? "withBuildingHeights" : "noBuildingHeights");

                        //DataOutput.outputHeightsAndLineOfSight(target.amISeen, heights, lineOfSight, distance2D,
                        DataOutput.outputHeightsAndLineOfSight(isThisOneSeen, heights, lineOfSight, distance2D,
                                ("data/lineofsight/" + type + "/lineOfSight_target" + target.id
                                + "_obs" + observer.id
                                + "_batch_" + batchNumber
                                + ".csv"));

                    } catch (Exception e) {
                        System.out.println(e.getMessage());
                    }
//
                }
//                }//if i can see you
            }//for target points

            System.out.println("batch " + batchNumber + ", buildingHeightRun " + buildingHeightRun + ", observer " + obcount++ + ": " + observerX + "," + observerY
                    + ", time: "
                    //+ ((System.currentTimeMillis() - before) / 60000) + " mins "
                    + ((System.currentTimeMillis() - before) / 1000) + " secs");

        }//for ob points

    }

//    private void testIndexCount() {
//
//        //Making sure distance band array index increment does what it should
//        //First, is this the right number of indices e.g. if view radius is 15km?
//        //0-1 / 1-2 / ... / 14-15km
//        int visibleObsDistanceBandCounts[] = new int[(int) radius / 1000];
//
//        //Yup.
//        System.out.println("index num: " + visibleObsDistanceBandCounts.length);
//
//        //Then - can distance be simply converted to int to find the right one?
//        Random randDist = new Random(1);
//
//        for (double d = 0; d < radius; d += (randDist.nextDouble() * 20)) {
//
//            int index = (int) d / 1000;
//
//            visibleObsDistanceBandCounts[index]++;
//
//            //Yup!
//            System.out.println("distance: " + d + ", index: " + index);
//
//        }
//
//    }
    private boolean canISeeYou(int index) {

//        System.out.println("------------");
        //If any points on same line point are higher on the landscape
        //My view is blocked
        for (int i = 0; i < heights[index].size(); i++) {
//        for (int i = 0; i < fheights.length; i++) {

//            System.out.println(lineOfSight[i] + ":" + heights.get(i));
            //Will need to check this still works if ob and target height are zero
            if (heights[index].get(i) > lineOfSight[i]) {
//            if (fheights[i] > lineOfSight[i]) {
//                System.out.println("can't see. Height here: " + heights.get(i));

                return false;
            }

        }
//        System.out.println("can't see. Height here: " + heights.get(i));
        return true;

    }

    private float[] getLineOfSight(int index, float startHeight, float endHeight) {

        //We know the length of line we need.
        //Just need to interpolate values between the two endpoints
        startHeight += heights[index].get(0);
        endHeight += heights[index].get(heights[index].size() - 1);

        //Which of course will always be the same because I SET THEM THE SAME!
//        if (buildingHeightRun) {
//            if (heights[0].get(0)!=heights[1].get(0)) {
//                System.out.println("getLineOfSight: index 0 and 1 of heights, first values if values differ: "
//                        + heights[0].get(0)
//                        + ","
//                        + heights[1].get(0));
//            }
//        }
//        System.out.println("ob and target height: " + obHeight + "," + targetHeight);
        float[] line = new float[heights[index].size()];

//        obHeight += fheights[0];
//        targetHeight += fheights[fheights.length - 1];
//
//        System.out.println("ob and target height: " + obHeight + "," + targetHeight);
//        float[] line = new float[fheights.length];
        //So e.g. ob at 250, target at 150, line of sight drops -100
//        float rangepos = targetHeight - obHeight;
//        float range = targetHeight - obHeight;
//        float rangepos = obHeight + range;
//        
//        System.out.println("rangepos: " + rangepos);
//
//        //No trig required - straight line between two points, one dimension (well, one and a bit!)
//        for (int i = 0; i < line.length; i++) {
//
//            //line[i] = (range / (float) line.length) * (float) i;
//            line[i] = obHeight + (range * (  (float)i/(float)line.length ));
////                    ((float) i/(float) line.length);
//            
//            System.out.println("line of sight point before height adj: "
//                    + line[i]
//                    + " ---- step: " + (rangepos / (float) line.length)
//                    + ", next would be: " + (line[i] + (rangepos / (float) line.length)));
//
//        }
        //So e.g. ob at 250, target at 150, line of sight drops -100
        //Just use ob height for first index 0
        //For subsequent ones, starting from one, subtract length of index -1 so you hit target height at the last one.
        //Why? Cos the sums work then!
        float range = endHeight - startHeight;

//        System.out.println("range: " + range);
//        System.out.println("line length: " + line.length);
        line[0] = startHeight;

        float stepSize = (range / (float) (line.length - 1));
//        System.out.println("stepSize: " + stepSize);

        //No trig required - straight line between two points, one dimension (well, one and a bit!)
        for (int i = 1; i < line.length; i++) {

            //Exclude the first value - just use obHeight for this
            line[i] = startHeight + (stepSize * (float) (i));

//            System.out.println("line of sight point before height adj: "
//                    + line[i]
//                    + " ---- step: " + (range / (float) line.length)
//                    + ", next would be: " + (line[i] + (range / (float) line.length)));
        }

        return line;

    }

    private void loadRasterData(int fileNum) {

        //VERY IMPORTANT NOTE AT TOP!
        //OS Terrain 5 data is all 5 metre grids. This code as it stands HARD-CODES for that
        //Arrays/lines etc are all naturally 1-unit, of course. So values are converted:
        //e.g. 100 metre observe point is 100/5, same for target.
        //And most importantly: in the BresenhamLine class, the raster height value
        //Is divided by 5 *AS IT'S BEING COPIED INTO THE LINE ARRAY*.
        //(Or isn't, actually, any more. Must be doing it somewhere else.)
        //
        //Basically, everything is 1/5th scale to make array work easy 
        //- which national grid also makes easy by using metric
        //That's just the sort of thing I'd forget and have to spend a day tracking down.
        //
        long before = System.currentTimeMillis();

        //load DEM without and with building heights
        raster = Landscape.readTiff(rootfolder + "/rasters/" + fileNum + ".tif");
        System.out.println("Vanilla DEM loaded");

        //Try loading building height raster. There may not be one 
        //so we need to work with both possibilities
        thisBatchHasBuildingHeights = false;

        //If we want to try and use building heights, where available
        //*and* they exist...        
        if (new File(rootfolder + "/rasters_w_buildingheight/" + fileNum + ".tif").exists()) {

            thisBatchHasBuildingHeights = true;
            bh_raster = Landscape.readTiff(rootfolder + "/rasters_w_buildingheight/" + fileNum + ".tif");

            System.out.println("DEM-plus-building-heights loaded");

        } else {
            System.out.println("No building height raster.");
        }

        System.out.println("Raster load time: " + ((System.currentTimeMillis() - before) / 1000) + " secs");
        //Load raster coordinate reference
        try {

            Landscape.origin = DataInput.loadCoords(fileNum);

        } catch (Exception e) {
            System.out.println("coord load failz: " + e.getLocalizedMessage());
        }

        System.out.println("origin: " + Landscape.origin[0] + "," + Landscape.origin[1]);

    }

    /**
     * useHousingSample: if true, will look for a version of the numbered CSV
     * file e.g. "46sample.csv".
     *
     * @param fileNum
     * @param useHousingSample
     */
    private void loadPointsData(int fileNum, boolean useHousingSample) {

        try {
            //last integers: id, column index of eastings/northings and, for observers, tip height column
            //-1: ignore height column, default to 2m
            targets = (useHousingSample ? DataInput.loadData(rootfolder + "/targets/" + fileNum + "sample.csv", "Target", 0, 1, 2, -1)
                    : DataInput.loadData(rootfolder + "/targets/" + fileNum + ".csv", "Target", 0, 1, 2, -1));

        } catch (Exception e) {
            System.out.println("Target load fail: " + e.getMessage());
        }

        //test with single target
//        targets.points.subList(1, targets.points.size()).clear();
//        System.out.println("single target: " + targets.points.get(0).attributes);
        //System.out.println(Point.fieldNames);
//        for (Point t : targets.points) {//
//            System.out.println("Target: " + t.attributes + "; stored locations: " + t.xloc + "," + t.yloc);//
//        }
        try {

            //last integers: id, column index of eastings/northings and, for observers, tip height column
            //For individual turbines, tip height is in column 7
            observers = DataInput.loadData(rootfolder + "/observers/" + fileNum + ".csv", "Observer", 0, 1, 2, 3);
            //for centroid-windfarms, tip height is in column 5
//            observers = DataInput.loadData(rootfolder + "/observers/" + fileNum + ".csv", "Observer", 0, 2, 3, 5);
//            observers = DataInput.loadData("data/observers/singleTurbine.csv", "Observer", 2, 3);

        } catch (Exception e) {
            System.out.println("Observer load fail: " + e.getMessage());
        }

//        for (Point p : observers.points) {
//
//            System.out.println("Observer height: " + p.height);
//
//        }
        //test with single turbine
        //Nice! http://stackoverflow.com/questions/3099527/how-to-remove-everything-from-an-arraylist-in-java-but-the-first-element
        //Clear out all elements not wanted, leaving the first
//        observers.points.subList(1, observers.points.size()).clear();
//        System.out.println("single observer: " + observers.points.get(0).attributes);
//        for(Point p : observers.points) {
//            System.out.println("turbine: " + p.attributes);
//        }
    }

    private DataStore loadHousingData(boolean buildingHeight) {

        //Either load from file initially
        //Or reload serialised version if we're part way through a run
        File folder = (buildingHeight ? new File(rootfolder + "/serialised/building") : new File(rootfolder + "/serialised/nonBuilding"));

        System.out.println("Load housing data from folder: " + folder.getName());

        //http://stackoverflow.com/questions/2102952/listing-files-in-a-directory-matching-a-pattern-in-java
        List<File> list = Arrays.asList(folder.listFiles(new FilenameFilter() {
            @Override
            public boolean accept(File dir, String name) {
                return name.endsWith(".ser");
            }
        }));

        //It'll either be one file or none...
        if (list.size() > 0 && useSerialisedIfAvailable) {

            System.out.println(list.size() + " file batches already processed / serialised. Start from next...");

            DataStore d;

            //there should only be the one file. We'll need the number from the filename.
            System.out.println("Filename: " + list.get(0).getAbsolutePath());
            String fileNameWithOutExt = FilenameUtils.removeExtension(list.get(0).getName());
            System.out.println("Last batch number: " + fileNameWithOutExt);

            //Get batch number only from non-building - we'll definitely have that each batch
            System.out.println("Parent folder: " + list.get(0).getParentFile().getName());

            if (list.get(0).getParentFile().getName().equals("nonBuilding")) {
                batchNumber = (Integer.parseInt(fileNameWithOutExt)) + 1;
                System.out.println("Set next batch number to process: " + batchNumber);
            }

            //set batch number: 1 more than last file.
            //Reload and set the batch point from where we last saved
            try {
                System.out.println("Loading previously serialised data...");
                FileInputStream fileIn = new FileInputStream(list.get(0).getAbsolutePath());
                ObjectInputStream in = new ObjectInputStream(fileIn);
                d = (DataStore) in.readObject();
                in.close();
                fileIn.close();
            } catch (IOException i) {
                i.printStackTrace();
                return null;
            } catch (ClassNotFoundException c) {
                c.printStackTrace();
                return null;
            }

            return d;

            //List size matches batch number we got to. Load the last.
        } else {

            System.out.println("Starting at beginning of batches...");

            DataStore d = new DataStore();

            try {
                //last integers: id, column index of eastings/northings and, for observers, tip height column
                //-1: ignore height column, default to 2m

                //Postcode centroids                
//                d = DataInput.loadData("C:\\Data\\WindFarmViewShed\\ViewshedPython\\Data\\postcode_centroids.csv", "Target", 0, 2, 3, -1);
                //
//                d = DataInput.loadData("C:\\Data\\WindFarmViewShed\\ViewshedPython\\Data\\houses_finalMay2016.csv", "Target", 0, 2, 3, -1);
                //Extra single-sales run (index starts at a million, not zero).
//                d = DataInput.loadData("C:\\Data\\WindFarmViewShed\\ViewshedPython\\Data\\singleSales\\singleSalesHouses_July2016.csv", "Target", 0, 2, 3, -1);
                //For toronto trees, just use the same target file...?
                d = DataInput.loadData("C:\\Data\\WindFarmViewShed\\ViewShedJava\\SimpleViewShed\\data\\targets\\1.csv", "Target", 0, 1, 2, -1);

//            d = DataInput.loadData("C:\\Data\\WindFarmViewShed\\ViewshedPython\\Data\\geocodedOldNewRoS.csv", "Target", 0, 2, 3, -1);
//            d = DataInput.loadData("C:/Data/WindFarmViewShed/ViewshedPython/Data/geocodedOldNewRoS.csv", "Target", 0, 2, 3, -1);
//            targets = DataInput.loadData("data/targets/1.csv", "Target", 2, 3);
            } catch (Exception e) {
                System.out.println("Target load fail: " + e.getMessage());
            }

            return d;

        }

    }

    private void serialise(DataStore d, String filename) {

        try {

            File file = new File(filename);
            String directory = file.getParent();
            System.out.println("Emptying directory: " + directory);
            //get rid of existing files first. They're each 0.2 gig, we don't want 140 of those!
            FileUtils.cleanDirectory(new File(directory));

            System.out.println("Writing new serialised...");

            FileOutputStream fileOut
                    = new FileOutputStream(filename);
            ObjectOutputStream out = new ObjectOutputStream(fileOut);

            out.writeObject(d);
            out.close();
            fileOut.close();

        } catch (IOException i) {
            i.printStackTrace();
        }

    }

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        // TODO code application logic here

        new Main();

    }

}


/*
 Cuttinz

 //System.out.println(Point.fieldNames);
 //        for (Point t : observers.points) {
 //            System.out.println("Observer: " + t.attributes + "; stored locations: " + t.xloc + "," + t.yloc);
 //        }
 //System.out.println("ob field: " + observers.fields);
 //test writing
 //        try {
 //            
 //            DataOutput.outputData(observers, "data/testSave.csv");            
 ////            DataOutput.outputData(targets, "data/testSave.csv");
 //            
 //        } catch (Exception e) {
 //            System.out.println("Data write fail: " + e.getLocalizedMessage());
 //        }
 //
 //        //Test intervisibility timings.
 //        testInterViz();
 //
 //        //check true/false vals
 //        int tru = 0;
 //
 //        for (Target v : targets) {
 //            //Huh: tru++ doesn't work here.
 //            tru = (v.amISeen ? tru + 1 : tru);
 //
 //        }
 //
 //        System.out.println("True: " + tru + ", false: " + (targets.size() - tru));
 //
 //        try {
 //            DataOutput.writeIntervizCSV(targets, "data/pythonOutputRasterTest.csv");
 //        } catch (Exception e) {
 //            System.out.println("Data output booboo: " + e);
 //        }
 //viewshed from this coordinate, within circle radius
 //        getViewShed((Images.width / 2) - 1, (Images.height / 2) - 1, (Images.height / 2) - 1);
 //        Images.outputBinaryTiff(viewShed);
 */
