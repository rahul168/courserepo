package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import Visualization._
import Interaction._
import scala.math._

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param x X coordinate between 0 and 1
    * @param y Y coordinate between 0 and 1
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    x: Double,
    y: Double,
    d00: Double,
    d01: Double,
    d10: Double,
    d11: Double
  ): Double = {
    val (x0, y0, x1, y1) = (0.0, 0.0, 1.0, 1.0)
    val v1 = linearInterpolatedValueScalar(x0, d00, x1, d10, x)
    val v2 = linearInterpolatedValueScalar(x0, d01, x1, d11, x)
    val v =  linearInterpolatedValueScalar(y0, v1, y1, v2, y)
    v
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param zoom Zoom level of the tile to visualize
    * @param x X value of the tile to visualize
    * @param y Y value of the tile to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: (Int, Int) => Double,
    colors: Iterable[(Double, Color)],
    zoom: Int,
    x: Int,
    y: Int
  ): Image = {
    val alpha = 127
    val imgWidth, imgHeight = 256
   val pixels = for {
      j <- 0 until imgHeight
      i <- 0 until imgWidth      
    } yield {
      val ii = x + i.toDouble / imgWidth
      val jj = y + j.toDouble / imgHeight
      val l = tileLocationD(zoom, ii, jj)
      // In terms of (x, y) d00 = (0, 0), d01 = (0, 1), d10 = (1, 0), d11 = (1, 1)
      // In terms of (Lat, Lon) = (0.5 - y, x - 0.5)  d00 = (0.5, -0.5), d01 = (-0.5, -0.5), d10 = (0.5, 0.5), d11 = (-0.5, 0.5)
      // According to this d00 = (ceil, floor), d01 = (floor, floor), d10 = (ceil, ceil), d11 = floor, ceil)
      val c = interpolateColor(colors, 
          bilinearInterpolation(l.lon - l.lon.floor.toInt, l.lat.ceil.toInt - l.lat, 
              grid(l.lat.ceil.toInt, l.lon.floor.toInt),
              grid(l.lat.floor.toInt, l.lon.floor.toInt),              
              grid(l.lat.ceil.toInt, l.lon.ceil.toInt),
              grid(l.lat.floor.toInt, l.lon.ceil.toInt)
              ))
      Pixel(c.red, c.green, c.blue, alpha)
    }
    Image(imgWidth, imgHeight, pixels.toArray)    
  }

}
