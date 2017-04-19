package observatory

import java.time.LocalDate


/**
 * 1st milestone: data extraction
 */
object Extraction {
  import Utility._
  import org.apache.spark.sql._
  import spark.implicits._

  /**
   * @param year             Year number
   * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
   * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
   * @return A sequence containing triplets (date, location, temperature)
   */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    // |id|latitude|longitude|
    val stations = readStations(stationsFile)
    // |id|year|month|day|temp|
    val tempData = readStationData(year, temperaturesFile)
    // |id|latitude|longitude|year|month|day|temp|  
    val joined = stations.join(tempData, "id")
    val r = joined.rdd.map {
      case Row(id: Long, latitude: Double, longitude: Double, year: Int, month: Int, day: Int, temp: Double) =>
        (LocalDate.of(year, month, day), Location(latitude, longitude), temp)
    }
    r.collect
  }

  /**
   * @param records A sequence containing triplets (date, location, temperature)
   * @return A sequence containing, for each location, the average temperature over the year.
   */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    val r = records.par.map(a => (a._2, a._3)).groupBy(_._1).mapValues( a => a.foldLeft(0.0)((a, b) => a + b._2)/a.size )
    r.seq
  }
}

