/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package simpleviewshed;

import java.io.BufferedReader;
import java.io.FileReader;

/**
 *
 * @author Dan Olner
 */
public class DataInput {

    static int x, y, height, id;

    /**
     * Don't need to mark the ID field. It's presumed these are all unique
     * points And all input fields will be returned in the final output
     *
     * Gets x and y coordinates by field index starting at zero. Gets height of
     * point from heighIndex column. If -1, ignore
     *
     * @param filename
     * @param xFieldIndex
     * @param yFieldIndex
     * @param heightIndex
     * @return
     * @throws Exception
     */
    public static DataStore loadData(String filename, String pointType, int idFieldIndex, int xFieldIndex, int yFieldIndex, int heightIndex) throws Exception {

        System.out.println("Loading: " + filename);
//        ArrayList<Point> points = new ArrayList<Point>();
        DataStore data = new DataStore();

        BufferedReader reader = new BufferedReader(new FileReader(filename));
//        List<String> lines = new ArrayList<>();
        String line;
        String[] cells;

        boolean firstLine = true;

        while ((line = reader.readLine()) != null) {

            //read header if on first line
            if (firstLine) {
                data.fields = line;
                System.out.println("fields: " + line);

                firstLine = false;
            } else {

                cells = line.trim().split(",");

//                for (String s : cells) {
//                    System.out.println(s);
//                }

                //raw values to be adjusted to 0,0 origin landscape image
                x = (int) Float.parseFloat(cells[xFieldIndex]);
                y = (int) Float.parseFloat(cells[yFieldIndex]);
                id = (int) Float.parseFloat(cells[idFieldIndex]);

                //default height is 2 metres if -1 passed in as column ref
//                System.out.println("atttempting to use this as height: " + (int) Float.parseFloat(cells[heightIndex]));
//                height = (heightIndex == -1 ? 2 : 3);
                height = (heightIndex == -1 ? 2 : (int) Float.parseFloat(cells[heightIndex]));
                //System.out.println("height index:" + heightIndex);

//                if (heightIndex != -1) {
//
////                    System.out.println("heightIndex: " + heightIndex + ", col: " + 5);
//                    System.out.println("heightIndex: " + heightIndex + ", col: " + (int) Float.parseFloat(cells[heightIndex]));
//
//                }

                //x adjustment relatively easy
                x -= Landscape.origin[0];
                //Divide by 5 to get down to 1 index being 5 metres
                x /= 5;
                //y a bit fiddlier as we have to flip
                y -= Landscape.origin[1];
                y /= 5;
                //then flip
                y = Landscape.height - y;

                //Quick and dirty way to load housing data that doesn't have/need a raster reference
                //(Which is null to start with)
                try {
                    data.points.add(DataStore.createPoint(pointType, line, id, x, y, Main.raster[x][y], height));
                } catch (Exception e) {
                    data.points.add(DataStore.createPoint(pointType, line, id, x, y, -1, height));
                }
                //System.out.println("here?");
//                data.points.add(DataStore.createPoint(pointType, line,
//                        (int) Float.parseFloat(cells[xFieldIndex]),
//                        (int) Float.parseFloat(cells[yFieldIndex])
//                ));
            }
        }

        return data;

    }

    public static int[] loadCoords(int index) throws Exception {

        BufferedReader reader = new BufferedReader(new FileReader(Main.rootfolder + "/coords/" + index + ".txt"));
//        List<String> lines = new ArrayList<>();
        String line;
        String[] cells;

        boolean firstLine = true;

        //only the one line
        cells = reader.readLine().trim().split(",");

        return new int[]{(int) Float.parseFloat(cells[0]), (int) Float.parseFloat(cells[1])};

    }

}
