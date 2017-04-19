package observatory

import com.sksamuel.scrimage.{ Image, Pixel }
import scala.math

/**
 * 2nd milestone: basic visualization
 */
object Visualization {

  val earthR = 6371000 // Meters

  /**
   * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
   * @param location Location where to predict the temperature
   * @return The predicted temperature at `location`
   */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    val p = 3
    interpolatedTemp(temperatures, location, p)
  }

  /**
   * @param points Pairs containing a value and its associated color
   * @param value The value to interpolate
   * @return The color that corresponds to `value`, according to the color scale defined by `points`
   */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    points.find(_._1 == value) match {
      case Some((_, c)) => c
      case None => {
        val (smallerTempList, greaterTempList) = points.toList.sortBy(_._1).partition(_._1 < value)
        if (smallerTempList.isEmpty) greaterTempList.head._2
        else if (greaterTempList.isEmpty) smallerTempList.reverse.head._2
        else linearInterpolatedColor(smallerTempList.reverse.head, greaterTempList.head, value)
      }
    }
  }

  /**
   * @param temperatures Known temperatures
   * @param colors Color scale
   * @return A 360×180 image where each pixel shows the predicted temperature at its location
   */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val imgWidth = 360
    val imgHeight = 180
    val p = for {
      y <- 0 until imgHeight
      x <- 0 until imgWidth
    } yield {
      val lon = x - 180
      val lat = 90 - y
      // Latitude is represented on Y and Longitude is represented on X
      interpolateColor(colors, predictTemperature(temperatures, Location(lat, lon)))
    }
    val alpha = 255
    val pixels = p.map((c: Color) => Pixel(c.red, c.green, c.blue, alpha))
    Image(imgWidth, imgHeight, pixels.toArray)
  }

  //
  // http://www.movable-type.co.uk/scripts/latlong.html
  //
  def distance(locA: Location, locB: Location): Double = {
    val (lat1, lon1) = (locA.lat.toRadians, locA.lon.toRadians)
    val (lat2, lon2) = (locB.lat.toRadians, locB.lon.toRadians)
    val dlon = math.abs(locA.lon - locB.lon).toRadians
    val centralAngle = math.acos(math.sin(lat1) * math.sin(lat2) + math.cos(lat1) * math.cos(lat2) * math.cos(dlon))
    val d = earthR * centralAngle
    d
  }

  def idw(locA: Location, locX: Location, p: Double): Double = {
    val d = distance(locA, locX)
    idw(d, p)
  }

  def idw(d: Double, p: Double): Double = {
    val w = 1 / math.pow(d, p)
    w
  }

  def interpolatedTemp(samples: Iterable[(Location, Double)], locX: Location, p: Double): Double = {
    val p = 3
    val u = samples.map(a => (distance(a._1, locX), a))
    val v = u.filter(_._1 < 1).toList.sortBy(_._1)
    if (v.size > 0) {
      v.head._2._2
    } else {
      val x = u.map {
        a =>
          {
            val w = idw(a._1, p)
            (w * a._2._2, w)
          }
      }.foldLeft((0.0, 0.0))((a, b) => (a._1 + b._1, a._2 + b._2)) match { case (a, b) => a / b }
      x
    }
  }

  def linearInterpolatedValue(pointA: (Double, Int), pointB: (Double, Int), value: Double): Int = {
    val ((x0, y0), (x1, y1)) = (pointA, pointB)
    val y = math.round(linearInterpolatedValueScalar(x0, y0, x1, y1, value)).toInt
    y
  }
  
  def linearInterpolatedValueScalar(x0: Double, y0: Double, x1: Double, y1: Double, x: Double): Double = {    
    val t = (x - x0) / (x1 - x0)
    val y = y0 + (y1 - y0) * t
    y
  }

  def linearInterpolatedColor(p1: (Double, Color), p2: (Double, Color), value: Double): Color = {
    val c = Color(
      linearInterpolatedValue((p1._1, p1._2.red), (p2._1, p2._2.red), value),
      linearInterpolatedValue((p1._1, p1._2.green), (p2._1, p2._2.green), value),
      linearInterpolatedValue((p1._1, p1._2.blue), (p2._1, p2._2.blue), value))
    c
  }

}

