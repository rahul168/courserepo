package observatory

import java.time.LocalDate

case class Location(lat: Double, lon: Double)

case class Color(red: Int, green: Int, blue: Int)

case class Station(id: Long, latitude: Double, longitude: Double)

case class Temperature(id: Long, year: Int, month: Int, day: Int, temp: Double)

