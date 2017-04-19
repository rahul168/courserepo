package observatory

import Visualization._

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Double)]): (Int, Int) => Double = {
    val width = 360
    val height = 180    
    val locations = (for {
      y <- 0 until height
      x <- 0 until width      
    } yield {
      val lon = x - 180
      val lat = 90 - y
      // Latitude is represented on Y and Longitude is represented on X
      (lat, lon) -> predictTemperature(temperatures, Location(lat, lon))
    }).toMap
    (a: Int, b: Int) => locations((a, b))
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Double)]]): (Int, Int) => Double = {
    val gg = temperaturess.map(makeGrid)
    
    (a: Int, b: Int) => {
      val x = gg.map(g => g(a, b))
      x.sum / x.size
    }    
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A sequence of grids containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Double)], normals: (Int, Int) => Double): (Int, Int) => Double = {
    val g = makeGrid(temperatures)    
    (a: Int, b: Int) => g(a, b) - normals(a, b)
  }
}

