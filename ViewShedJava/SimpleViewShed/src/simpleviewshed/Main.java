/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package simpleviewshed;

import java.awt.geom.Ellipse2D;
import java.util.ArrayList;
import java.util.Random;

/**
 *
 * @author SMI
 */
public class Main {

    float[][] raster;
    boolean[][] viewShed;
    //BresenhamLine results
    ArrayList<Float> heights;
    float[] fheights;
    //Line of sight between observer and target
    float[] lineOfSight;

    Ellipse2D viewCircle;
    int observerX, observerY, targetX, targetY;
    //scale down by five
    float radius = 15000 / 5;

    boolean canISeeYou = false;

//    ArrayList<Point> observers = new ArrayList<Point>();
//    ArrayList<Point> targets = new ArrayList<Point>();
    DataStore targets, observers;
    ArrayList<TargetPoint> targetsInView = new ArrayList<>();

    public Main() {

        //VERY IMPORTANT NOTE AT TOP!
        //OS Terrain 5 data is all 5 metre grids. This code as it stands HARD-CODES for that
        //Arrays/lines etc are all naturally 1-unit, of course. So values are converted:
        //e.g. 100 metre observe point is 100/5, same for target.
        //And most importantly: in the BresenhamLine class, the raster height value
        //Is divided by 5 *AS IT'S BEING COPIED INTO THE LINE ARRAY*.
        //Basically, everything is 1/5th scale to make array work easy 
        //- which national grid also makes easy by using metric
        //That's just the sort of thing I'd forget and have to spend a day tracking down.
        //
        long before = System.currentTimeMillis();

        int fileNum = 3;

        raster = Landscape.readTiff(fileNum);

        System.out.println("Raster load time: " + ((System.currentTimeMillis() - before) / 1000) + " secs");
        //Load raster coordinate reference
        try {

            Landscape.origin = DataInput.loadCoords(fileNum);

        } catch (Exception e) {
            System.out.println("coord load failz: " + e.getLocalizedMessage());
        }

        System.out.println("origin: " + Landscape.origin[0] + "," + Landscape.origin[1]);

        try {
            targets = DataInput.loadData("data/targets/" + fileNum + ".csv", "Target", 2, 3);
//            targets = DataInput.loadData("data/targets/1.csv", "Target", 2, 3);
        } catch (Exception e) {
            System.out.println("Target load fail: " + e.getMessage());
        }

        //System.out.println(Point.fieldNames);
//        for (Point t : targets.points) {//
//            System.out.println("Target: " + t.attributes + "; stored locations: " + t.xloc + "," + t.yloc);//
//        }
        try {
            observers = DataInput.loadData("data/observers/" + fileNum + ".csv", "Observer", 2, 3);
//            targets = DataInput.loadData("data/targets/1.csv", "Target", 2, 3);
        } catch (Exception e) {
            System.out.println("Observer load fail: " + e.getMessage());
        }

        interViz();

        try {
            DataOutput.outputData(targets, "data/housingTarget3.csv");
        } catch (Exception e) {
            System.out.println("Data output booboo: " + e);
        }

    }

    private void interViz() {

        int obcount = 0;
        long before = System.currentTimeMillis();

        for (Point ob : observers.points) {

            //op = (ObserverPoint) ob;
            observerX = ob.xloc;
            observerY = ob.yloc;

            viewCircle = new Ellipse2D.Float((float) observerX - radius, (float) observerY - radius, radius * 2, radius * 2);

            //subset targets in view circle - passing by reference, will update correctly
            targetsInView.clear();
            TargetPoint p;

            System.out.println("Houses total: " + targets.points.size());

            for (Point target : targets.points) {

                p = (TargetPoint) target;

                if (viewCircle.contains(p.xloc, p.yloc)) {
                    targetsInView.add(p);
                }
            }

            System.out.println("houses subset in 15km view: " + targetsInView.size());

            int timer = 0;

//        for (int i = 0; i < targets.points.size(); i++) {
            for (TargetPoint target : targetsInView) {

//                timer++;
//
//                if (timer % 5000 == 0) {
//                    
//                }
                heights = BresenhamLine.findLine(raster, observerX, observerY, target.xloc, target.yloc);

                //            lineOfSight = getLineOfSight(100, 2);
                //Oops: 5 metre units. That was a half-km high turbine and 10 metre high human!
                lineOfSight = getLineOfSight(20, 0.2f);

                target.amISeen = canISeeYou();

//            }//end if viewcircle contains
            }

            System.out.println("observer " + ++obcount + ": " + observerX + "," + observerY
                    + ", time: " + ((System.currentTimeMillis() - before) / 1000) + " secs");

        }//for ob points

    }

    private void testInterViz() {

        long before = System.currentTimeMillis();
        int id = 0;

        Random randTarget = new Random(1);

        float radius = (Landscape.height / 2) - 1;
        System.out.println("radius: " + radius);

        observerX = (Landscape.width / 2) - 1;
        observerY = (Landscape.height / 2) - 1;

        System.out.println("observer: " + observerX + "," + observerY);

        viewCircle = new Ellipse2D.Float((float) observerX - radius, (float) observerY - radius, radius * 2, radius * 2);

        for (int i = 0; i < 500000; i++) {

            if (i % 5000 == 0) {
                System.out.println("Interviz test, target:" + i + " ,"
                        + ((System.currentTimeMillis() - before) / 1000) + " secs");
            }

            targetX = -1;
            targetY = -1;

//            random points for observer to look at 
            while (!viewCircle.contains(targetX, targetY)) {

                targetX = randTarget.nextInt(Landscape.width);
                targetY = randTarget.nextInt(Landscape.height);

            }

//            System.out.println("targetX: " + targetX + ", targetY: " + targetY);
            heights = BresenhamLine.findLine(raster, observerX, observerY, targetX, targetY);
//                    fheights = BresenhamLine.findLine2(raster, x, y, i, j);

//            lineOfSight = getLineOfSight(100, 2);
            //Oops: 5 metre units. That was a half-km high turbine and 10 metre high human!
            lineOfSight = getLineOfSight(20, 0.2f);

            //do bespoke coordinate conversion to match raster in QGIS
            targetX = 235000 + (targetX * 5);
            targetY = 670000 - (targetY * 5);

            canISeeYou = canISeeYou();

            //targets.add(new Point(id++, targetX, targetY, canISeeYou));
            //look at some
//            if (canISeeYou) {
//            if (i % 500 == 0) {
//                try {
//                    DataOutput.outputHeightsAndLineOfSight(canISeeYou, heights, lineOfSight, ("data/lineofsight/lineOfSight_z_" + i + ".csv"));
//                } catch (Exception e) {
//                    System.out.println(e.getMessage());
//                }
//
//            }
        }

    }//end method

    private void getViewShed(int x, int y, float radius) {

        int fcount = 0, tcount = 0;

        viewShed = new boolean[Landscape.width][Landscape.height];
        viewCircle = new Ellipse2D.Float((float) x - radius, (float) y - radius, radius * 2, radius * 2);

        long before = System.currentTimeMillis();

        //Work out visibility of every point
        for (int i = 0; i < Landscape.width; i++) {

            if (i % 10 == 0) {
                System.out.println("Viewshed processing: row " + i + ", time: "
                        + ((System.currentTimeMillis() - before) / 1000) + " secs");
            }

            for (int j = 0; j < Landscape.height; j++) {

                //within view radius?
                if (viewCircle.contains(i, j)) {

                    heights = BresenhamLine.findLine(raster, x, y, i, j);
//                    fheights = BresenhamLine.findLine2(raster, x, y, i, j);
                    lineOfSight = getLineOfSight(100, 2);

                    viewShed[i][j] = canISeeYou();

                }

                //Simple check
//                if(viewShed[i][j]) {
//                    tcount++;
//                } else {
//                    fcount++;
//                }
            }
        }

//        System.out.println("True: " + tcount + ", False: " + fcount);
        System.out.println("Viewshed processing time: "
                + ((System.currentTimeMillis() - before) / 1000) + " secs");

    }

    private boolean canISeeYou() {

        //If any points on same line point are higher on the landscape
        //My view is blocked
        for (int i = 0; i < heights.size(); i++) {
//        for (int i = 0; i < fheights.length; i++) {

            //Will need to check this still works if ob and target height are zero
            if (heights.get(i) > lineOfSight[i]) {
//            if (fheights[i] > lineOfSight[i]) {
                //System.out.println("can't see. Height here: " + heights.get(i) + );

                return false;
            }

        }

        return true;

    }

    private float[] getLineOfSight(float obHeight, float targetHeight) {

        //We know the length of line we need.
        //Just need to interpolate values between the two endpoints
        obHeight += heights.get(0);
        targetHeight += heights.get(heights.size() - 1);

//        System.out.println("ob and target height: " + obHeight + "," + targetHeight);
        float[] line = new float[heights.size()];

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
        float range = targetHeight - obHeight;

//        System.out.println("range: " + range);
//        System.out.println("line length: " + line.length);
        line[0] = obHeight;

        float stepSize = (range / (float) (line.length - 1));
//        System.out.println("stepSize: " + stepSize);

        //No trig required - straight line between two points, one dimension (well, one and a bit!)
        for (int i = 1; i < line.length; i++) {

            //Exclude the first value - just use obHeight for this
            line[i] = obHeight + (stepSize * (float) (i));

//            System.out.println("line of sight point before height adj: "
//                    + line[i]
//                    + " ---- step: " + (range / (float) line.length)
//                    + ", next would be: " + (line[i] + (range / (float) line.length)));
        }

        return line;

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
