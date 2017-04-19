package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import scala.math._
import observatory.Visualization._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  /**
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    tileLocationD(zoom: Int, x, y)
  }
  
  def tileLocationD(zoom: Int, x: Double, y: Double): Location = {
    val lat = toDegrees(atan(sinh(Pi * (1.0 - 2.0 * y / (1 << zoom)))))
    val lon = x / (1 << zoom) * 360.0 - 180.0
    Location(lat, lon)    
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    val imgWidth = 256
    val imgHeight = 256  
    val alpha = 127
    val pixels = for {
      j <- 0 until imgHeight
      i <- 0 until imgWidth      
    } yield {
      val ii = x + i.toDouble / imgWidth
      val jj = y + j.toDouble / imgHeight
      val l = tileLocationD(zoom, ii, jj)
      val c = interpolateColor(colors, predictTemperature(temperatures, l))
      Pixel(c.red, c.green, c.blue, alpha)
    }
    //println(s"Image($imgWidth, $imgHeight, ${pixels.size})")
    Image(imgWidth, imgHeight, pixels.toArray)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {
    yearlyData.foreach {
      case (year, data) => {
        for {
          z <- 0 to 3
          x <- 0 until 1 << z         
          y <- 0 until 1 << z           
        } generateImage(year, z, x, y, data)
      } 
    }
  }
}
