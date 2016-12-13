package simpleviewshed;

import java.util.ArrayList;
import java.util.List;

/**
 * Implementation of the Bresenham line algorithm.
 *
 * @author fragkakis
 *
 * https://github.com/fragkakis/bresenham/blob/master/src/main/java/org/fragkakis/Bresenham.java
 */
public class BresenhamLine {

    //Don't know length of line yet
    public static ArrayList line = new ArrayList<Float>();
    //same line for building-height version. Can do both at same time.
    public static ArrayList bh_line = new ArrayList<Float>();
    //adjusted version with 'levelled' area near target
    //Actually, just going to level bh_line
    //public static ArrayList bh_line_levelled = new ArrayList<Float>();

    //Switch determining method for dealing with building heights
    //If true, the first building is flattened to ground level to allow line of sight through
    //by 'walking' to the building's edge (the point that the two rasters have equal values)
    //If false, just replace the first part of the line from non-BH raster
    //Using buildingLevelRadius
    private static boolean flattenFirstBuilding = true;
    private static int step;

    //If NOT using flattenFirstBuilding, flatten a set radius instead.
    //How much to flatten buildings around target point so they can "see"
    //In metres
    private static int buildingLevelRadius = 20;
    //Fraction of line to level at target end
    private static double fraction;
    //And actual count of indices to drop
    private static int dropIndices;

    /**
     * Returns the list of array elements that comprise the line.
     *
     * @param grid the 2d array
     * @param x0 the starting point x
     * @param y0 the starting point y
     * @param x1 the finishing point x
     * @param distance between the two points
     * @param y1 the finishing point y
     * @return the line as a list of array elements
     */
    //return height values along the line
    //Return two arraylists if finding for building height raster also
    public static ArrayList<Float>[] findLine(int x0, int y0, int x1, int y1, double distance) {

        line.clear();

        //If using building heights too
        //I can grab both at the same time rather than calculate twice. 
        //It should be a little faster.
        if (Main.buildingHeightRun) {
            bh_line.clear();
        }
//        ArrayList line = new ArrayList<>();        

        int dx = Math.abs(x1 - x0);
        int dy = Math.abs(y1 - y0);

        int sx = x0 < x1 ? 1 : -1;
        int sy = y0 < y1 ? 1 : -1;

        int err = dx - dy;
        int e2;
        int currentX = x0;
        int currentY = y0;

        while (true) {

            //System.out.println(currentX+","+currentY);
            line.add(Main.raster[currentX][currentY]);

            //find same point height on building-height version, if using
            if (Main.buildingHeightRun) {
                bh_line.add(Main.bh_raster[currentX][currentY]);
            }

//            System.out.println("grid: " + grid[currentX][currentY] + ", grid/5: " + grid[currentX][currentY]/5f);
            if (currentX == x1 && currentY == y1) {
                break;
            }

            e2 = 2 * err;
            if (e2 > -1 * dy) {
                err -= dy;
                currentX += sx;
            }

            if (e2 < dx) {
                err += dx;
                currentY += sy;
            }
        }

//        System.out.println("line lengths: " + line.size() + ", bh: " + bh_line.size());
//
//        System.out.println("---");
        //checking the broken one
        //Ah: it's building all the way to the edge!
//        if (line.size() == 5) {
//            for (int i = 0; i < line.size(); i++) {
//                System.out.println("vals. " + line.get(i) + " bh: " + bh_line.get(i));
//            }
//        }
//        System.out.println("||||");
        //If using building heights:
        //Adjust BH line using vanilla DEM so target can "see out of" building.
        if (Main.buildingHeightRun) {

            if (flattenFirstBuilding) {

                //Discount first pixel - replace with vanilla
                bh_line.set(0, line.get(0));

                //If next pixel differs, we're inside a building.
                //Presume this is the building in question and walk to the edge
                //If the first step is equal, there's level ground in the next pixel
                //And the while loop will skip, leaving the rest of the bh line unchanged
                //So if pixel 3 has building height data, that will still be there.
                step = 1;

//                while (bh_line.get(step) != line.get(step)) {
                //correct float comparison
                //Sometimes (for the tree analysis random points test)
                //Buildings go over the edge of the Bresenham Line
                //So you can't walk to the edge
                //This shouldn't happen with the actual data
                //But needs checking here with the step size check
                //Oh and check they're not on the same pixel 
                //(again, a random test point problem only)
                if (line.size() > 1) {

                    while (!bh_line.get(step).equals(line.get(step)) && (step + 1 < line.size())) {

//                    System.out.println("walking to edge of building. Step " + step + ", values: "
//                            + bh_line.get(step) + ","
//                            + line.get(step)
//                    );
//                    if(!bh_line.get(step).equals(line.get(step))){
//                        System.out.println("equals method not working");
//                    } else{
//                        System.out.println("working!");
//                    }
                        bh_line.set(step, line.get(step));

                        step++;

                    }

                }//if line.size>1

                //if we found equal in both on pixel 2 / step 1 just let me know
//                if (step == 1) {
//                    System.out.println("second pixel was flat land. What's the next one? "
//                            + bh_line.get(2) + ","
//                            + line.get(2));
//                }
            } else {

                //start of line is house/target, end is observer/turbine.
                //We want the building-end to have very nearby buildings removed
                //To make sure "I" can see out of my own building.
                //Use passed in distance - we can't tell that from the line itself
                //but we can work out proportions
//            System.out.println("distance: " + distance);
//            System.out.println("Levelling radius: " + buildingLevelRadius);
//            System.out.println("Line length: " + line.size());
                fraction = (double) buildingLevelRadius / distance;

//            System.out.println("fraction: " + fraction);
                //in case we're levelling more than the actual line distance...
                fraction = (fraction > 1 ? 1 : fraction);

                dropIndices = (int) ((double) line.size() * fraction);

//            System.out.println("dropping this number of indices from line: " + dropIndices);
                //Replace that small area nearby from vanilla DEM
                for (int i = 0; i < dropIndices; i++) {
                    bh_line.set(i, line.get(i));
                }

            }

            return new ArrayList[]{bh_line, line};

        } else {
            return new ArrayList[]{line};
        }

    }

    //return height values along the line
//    public static float[] findLine2(float[][] grid, int x0, int y0, int x1, int y1) {
//
//        //Bresenham line is only ever the max out of x and y
//        //Plus one!
//        float[] line
//                = (Math.abs(x0 - x1) > Math.abs(y0 - y1)
//                        ? new float[Math.abs(x0 - x1) + 1]
//                        : new float[Math.abs(y0 - y1) + 1]);
//
////        System.out.println("line length: " + line.length);
//        int index = 0;
//
//        int dx = Math.abs(x1 - x0);
//        int dy = Math.abs(y1 - y0);
//
//        int sx = x0 < x1 ? 1 : -1;
//        int sy = y0 < y1 ? 1 : -1;
//
//        int err = dx - dy;
//        int e2;
//        int currentX = x0;
//        int currentY = y0;
//
//        while (true) {
//
//            line[index++] = (grid[currentX][currentY]);
//
//            if (currentX == x1 && currentY == y1) {
//                break;
//            }
//
//            e2 = 2 * err;
//            if (e2 > -1 * dy) {
//                err = err - dy;
//                currentX = currentX + sx;
//            }
//
//            if (e2 < dx) {
//                err = err + dx;
//                currentY = currentY + sy;
//            }
//        }
//
//        return line;
//
//    }
}
