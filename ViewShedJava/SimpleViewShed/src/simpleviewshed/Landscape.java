package simpleviewshed;

import java.awt.Color;
import java.awt.image.*;
import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import javax.imageio.ImageIO;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 *
 * @author Dan Olner
 */
public class Landscape {

    public static int width, height;
    public static int[] origin = {-1,-1};    

    public static float[][] readTiff(String filename) {

        BufferedImage image = new BufferedImage(1, 1, 1);

        try {

            //Needs https://github.com/jai-imageio/jai-imageio-core
            //I used standalone jar from
            //https://bintray.com/jai-imageio/maven/jai-imageio-core-standalone/view
            
            image = ImageIO.read(new File(filename));


//            image = ImageIO.read(new File("data/rasters/" + fileNum + ".tif"));

            //RGB outputs as a 32 bit int. We want 32 bit float.
//            float fl = Float.intBitsToFloat(i);
//            System.out.println("And in float form: " + fl);
//            
        } catch (Exception e) {
            System.out.println("uh oh: " + e.getLocalizedMessage());
        }

        //System.out.println("Image info: " + image.);
        width = image.getWidth();
        height = image.getHeight();

        System.out.println("height: " + height);
        System.out.println("width: " + width);

//        int i = image.getRGB(1000, 1000);
//        System.out.println("RGB: " + i);
        Raster r = image.getData();
        
        //Pixels are 32 bit float encoded, metres above sea level
//        float[] pix = r.getPixel(2001, 0, (float[]) null);
//
//        for (float f : pix) {
//            System.out.println("pix: " + f);
//        }
        //Bucket into array (much faster access than r.getPixel, tested)
        float pixels[][] = new float[image.getWidth()][image.getHeight()];

        for (int i = 0; i < image.getWidth(); i++) {
            for (int j = 0; j < image.getHeight(); j++) {
                //NOTE: DIVIDED BY 5. Scaling down to match rest of data
                pixels[i][j] = r.getPixel(i, j, (float[]) null)[0]/5;
                
//                if(pixels[i][j]!=0){
//                    System.out.println("pixel! " + pixels[i][j]);
//                }
                
            }
        }

        //System.out.println("pixtest: " + pixels[2002][0]);
        return pixels;

    }

    public static boolean outputBinaryTiff(boolean[][] bits) {

        BufferedImage output = new BufferedImage(width, height, BufferedImage.TYPE_BYTE_BINARY);

        for (int i = 0; i < width; i++) {
            for (int j = 0; j < height; j++) {

                //http://stackoverflow.com/questions/27633299/bufferedimage-type-byte-binary-change-color-of-pixel
                output.setRGB(i, j, bits[i][j] ? Color.WHITE.getRGB() : Color.BLACK.getRGB());

            }
        }

        //http://stackoverflow.com/questions/30320434/write-a-tiff-with-jai
        File outputfile = new File("rasters/viewshed.tif");

        try {
            ImageIO.write(output, "TIFF", outputfile);
        } catch (Exception e) {
            System.out.println("Bah! " + e.getMessage());
            return false;
        }

        return true;

    }

    //http://stackoverflow.com/questions/5386991/java-most-efficient-method-to-iterate-over-all-elements-in-a-org-w3c-dom-docume
    private static List<Node> asList(NodeList nodes) {
        List<Node> list = new ArrayList<Node>(nodes.getLength());
        for (int i = 0, l = nodes.getLength(); i < l; i++) {
            list.add(nodes.item(i));
        }
        return list;
    }

    private static List<Node> getChildren(Node n) {
        List<Node> children = asList(n.getChildNodes());
        Iterator<Node> it = children.iterator();
        while (it.hasNext()) {
            if (it.next().getNodeType() != Node.ELEMENT_NODE) {
                it.remove();
            }
        }
        return children;
    }

}
