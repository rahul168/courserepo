package observatory

/**
 * @author U0084429
 */
object Utility {

  import java.nio.file.Paths
  import org.apache.spark.sql.SparkSession
  import org.apache.spark.sql._
  import org.apache.spark.sql.functions._
  import org.apache.spark.sql.types._
  import org.apache.log4j.{Level, Logger}

  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  val spark: SparkSession = SparkSession
    .builder()
    .appName("Observatory")
    .config("spark.master", "local")
    .config("spark.executor.heartbeatInterval", "60s")
    .getOrCreate()

  import spark.implicits._

  /** @return The read DataFrame along with its column names. */
  def readStationData(year: Int, resource: String): Dataset[Temperature] = {    
    val df = spark.read.schema(temperatureSchema).csv(fsPath(resource))
    df.createOrReplaceTempView("temperature")  
    val dataFrame = spark.sql(
       s"""SELECT ((coalesce(sid, 0) * 1000000) + coalesce(wid, 0)) as id, $year as year, month, day, (((temp - 32.0) * 5.0) / 9.0) AS temp
          FROM temperature
          WHERE temp != 9999.9
       """
       )
    dataFrame.as[Temperature].persist
  }

  /** @return The read DataFrame along with its column names. */
  def readStations(resource: String): Dataset[Station] = {
   val df = spark.read.schema(stationSchema).csv(fsPath(resource))
   df.createOrReplaceTempView("station")
   val dataFrame = spark.sql(
       """SELECT ((coalesce(sid, 0) * 1000000) + coalesce(wid, 0)) as id, latitude, longitude
          FROM station
          WHERE latitude is not null and longitude is not null and latitude != 0 and longitude != 0
       """
       )
    dataFrame.as[Station].persist
  }

  def stationSchema(): StructType = {
    val sid = StructField("sid", LongType, true)    
    val wid = StructField("wid", LongType, true)
    val lt = StructField("latitude", DoubleType, true)
    val lo = StructField("longitude", DoubleType, true)
    StructType(List(sid, wid, lt, lo))
  }

  def temperatureSchema(): StructType = {
    val sid = StructField("sid", LongType, true)    
    val wid = StructField("wid", LongType, true)
    val m = StructField("month", IntegerType, false)
    val d = StructField("day", IntegerType, false)
    val t = StructField("temp", DoubleType, false)
    StructType(List(sid, wid, m, d, t))
  }  

  def fsPath(resource: String): String = Paths.get(getClass.getResource(resource).toURI).toString  
  def toCelsius(f: Double): Double = (f - 32) * 5 / 9
}
