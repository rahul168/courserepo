package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.apache.spark.sql.DataFrame
import Utility._

@RunWith(classOf[JUnitRunner])
class ExtractionTest extends FunSuite {
  val year = 1975
  val debug = false

  val stationsPath: String = "/stations.csv"
  val resource: String = s"/$year.csv"

  lazy val stations = readStations(stationsPath)
  lazy val temperatures = readStationData(year, resource)

  lazy val locateTemperatures = Extraction.locateTemperatures(year, stationsPath, resource)
  lazy val locationAverage = Extraction.locationYearlyAverageRecords(locateTemperatures)

  test("stations") {
    if (debug) stations.show()
    assert(stations.count() === 27708, "Stations row count is not 27,708")
  }

  test("temperatures") {
    if (debug) temperatures.show()
    assert(temperatures.count() === 2190974, "Temperature row count is not 2,190,974")
  }

  test("locateTemperatures") {
    if (debug) locateTemperatures.take(20).foreach(println)    
    //assert(locateTemperatures.size === 2176493, "Locate Temperature did not return 2,176,493 rows")
  }

  test("locationYearlyAverageRecords") {
    if (debug) locationAverage.take(20).foreach(println)    
    //assert(locationAverage.size === 8268, "Location Yearly Average did not return 8,268 rows")
  }  
}