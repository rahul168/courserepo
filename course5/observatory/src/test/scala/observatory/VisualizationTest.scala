package observatory


import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import Visualization._
import Extraction._

@RunWith(classOf[JUnitRunner])
class VisualizationTest extends FunSuite with Checkers {
   
  val year = 1971
  val debug = false
  val stationsPath = "/stations.csv"
  val temperaturePath = s"/$year.csv"

  lazy val locationTemperatures = locateTemperatures(year, stationsPath, temperaturePath)
  lazy val locationAverage = locationYearlyAverageRecords(locationTemperatures)

  test("locationTemperaturesSize") {
    if (debug) locationTemperatures.take(20).foreach(println)    
    assert(locationTemperatures.size === 20000)
  }
  
  test("locationAverageSize") {
    if (debug) locationAverage.take(20).foreach(println)       
    assert(locationAverage.size === 79)
  }


  test("Distance between 40.7486, -73.9864 and 44.7486, -74.9864 is approx 452202 ") {
    val p1 = Location(40.7486, -73.9864) 
    val p2 = Location(44.7486, -74.9864) 
    val d = distance(p1, p2)
    assert( d < 452.210 && d > 452.200, "distance not valid")
  }

  test("idw") {
    val p1 = Location(40.7486, -73.9864) 
    val p2 = Location(44.7486, -74.9864)    
    assert(idw(p1, p2, 32) > 0.0d)
    assert(idw(p1, p2, 32) < 1.0d)
    assert(idw(p1, p2, 0) === 1.0d)
  }
  
  test("Distance 0.0") {    
    val (l: Location, t: Double) = locationAverage.toSeq(0)    
    assert(predictTemperature(locationAverage, l).round === t.round)    
  }

  test("Distance != 0.0") {
    val (l: Location, t: Double) = locationAverage.toSeq(0) // Location(58.117,6.567)
    assert(predictTemperature(locationAverage, Location(l.lat + 10, l.lon + 10)).round === 3)
    assert(predictTemperature(locationAverage, Location(l.lat + 10, 0 - l.lon )).round === 2)
  }

  test("predictTemperature small sets") {
    assert(predictTemperature(List((Location(45.0, -90.0), 10.0), (Location(-45.0, 0.0), 20.0)), Location(0.0, -45.0)) === 15.0)
    assert(predictTemperature(List((Location(0.0, 0.0), 10.0)), Location(0.0, 0.0)) === 10.0)    
    assert(predictTemperature(List((Location(45.0, -90.0), 0.0), (Location(-45.0, 0.0), 59.028308521858634)), Location(0.0, 0.0)).round === 52)
  }

  test("linearInterpolatedValue") {
    assert(linearInterpolatedValue((10.0, 1), (40.0, 4), 10.0) === 1)
    assert(linearInterpolatedValue((10.0, 1), (40.0, 4), 20.0) === 2)
    assert(linearInterpolatedValue((10.0, 1), (40.0, 4), 30.0) === 3)
    assert(linearInterpolatedValue((10.0, 1), (40.0, 4), 40.0) === 4)
  }

  test("interpolateColorFeedback") {
    val scale = List((-1.0,Color(255,0,0)), (2.147483647E9,Color(0,0,255)))
    val value = 5.36870911E8
    assert(interpolateColor(scale, value) === Color(191,0,64))    
  }

  
  test("interpolateColor") {
    val scale = List(
      (100.0, Color(255, 255, 255)),
      (50.0, Color(0, 0, 0)),
      (0.0, Color(255, 0, 128))
    )

    assert(interpolateColor(scale, 50.0) === Color(0, 0, 0))
    assert(interpolateColor(scale, 0.0) === Color(255, 0, 128))
    assert(interpolateColor(scale, -10.0) === Color(255, 0, 128))
    assert(interpolateColor(scale, 200.0) === Color(255, 255, 255))
    assert(interpolateColor(scale, 75.0) === Color(128, 128, 128))
    assert(interpolateColor(scale, 25.0) === Color(128, 0, 64))
  }

  test("visualize") {
    val palette = List(
      (60.0, Color(255, 255, 255)),
      (32.0, Color(255, 0, 0)),
      (12.0, Color(255, 255, 0)),
      (0.0, Color(0, 255, 255)),
      (-15.0, Color(0, 0, 255)),
      (-27.0, Color(255, 0, 255)),
      (-50.0, Color(33, 0, 107)),
      (-60.0, Color(0, 0, 0))
    )

    val img = visualize(locationAverage, palette)    
    assert(img.pixels.length === 360 * 180)
  }
  
}
