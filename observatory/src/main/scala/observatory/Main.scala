package observatory

object Main extends App {

  val s=Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(1975,"/stations.csv","/1975.csv"))

  s.foreach(println)
}
