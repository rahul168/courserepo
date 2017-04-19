package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers
import scala.collection.concurrent.TrieMap
import Extraction._
import Visualization._
import Interaction._ 

@RunWith(classOf[JUnitRunner])
class InteractionTest extends FunSuite with Checkers {
  val year = 1971
  val debug = false
  val stationsPath = "/stations.csv"
  val temperaturePath = s"/$year.csv"
  
  lazy val locationTemperatures = locateTemperatures(year, stationsPath, temperaturePath)
  lazy val locationAverage = locationYearlyAverageRecords(locationTemperatures)
  

  test("tileLocation") {
    assert(tileLocation(0, 0, 0) === Location(85.05112877980659, -180.0))
    assert(tileLocation(10, 10, 10) === Location(84.7383871209534, -176.484375))
    assert(tileLocation(5, 100, 100) === Location(-89.99999212633796, 945.0))
  }

   test("generateTiles") {
    def testData(year: Int, zoom: Int, x: Int, y: Int, data: String) = {
      println(s"$year, $zoom, $x, $y, $data")
    }

    val data = Set((1971, "Test"))
    generateTiles(data, testData)
  }

  test("generate1971") {
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
    
    def saveImage(year: Int, zoom: Int, x: Int, y: Int, data: Iterable[(Location, Double)]) = {
      println(s"target/temperatures/$year/$zoom/$x-$y.png")
      val img = tile(data, palette, zoom, x, y)
      val _ = img.output(new java.io.File(s"./target/temperatures/$year/$zoom/$x-$y.png"))
    }

    val data: Set[(Int, Iterable[(Location, Double)])] = Set((year, locationAverage)) 
    generateTiles(data, saveImage)
  }
}
