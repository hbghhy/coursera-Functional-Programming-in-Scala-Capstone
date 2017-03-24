package observatory

import java.time.LocalDate

import scala.collection.immutable.HashMap



/**
  * 1st milestone: data extraction
  */
object Extraction {

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val stationsInput=scala.io.Source.fromInputStream(getClass.getResourceAsStream(stationsFile)).getLines()
    val temperaturesInput=scala.io.Source.fromInputStream(getClass.getResourceAsStream(temperaturesFile)).getLines()
    val idToLocation=stationsInput.flatMap(x=>{
      val temp=x.split(",")
      if(temp.length!=4){
        None
      } else if(temp(2)!="" && temp(3)!=""){
        Some((new Stn_Wban(temp(0),temp(1)),new Location(temp(2).toDouble,temp(3).toDouble)))
      }else{
        None
      }
    }).toMap
    temperaturesInput.flatMap(x=>{
      val temp=x.split(",")
      if(temp.length!=5){
        None
      } else
      {
        val identifier=new Stn_Wban(temp(0),temp(1))
        if(idToLocation.contains(identifier) && temp(4)!="9999.9"){
          Some((LocalDate.of(year,temp(2).toInt,temp(3).toInt),idToLocation.get(identifier).get,(temp(4).toDouble-32)/1.8))
        }else{
          None
        }
      }
    }).toIterable
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    def avg(iterable: Iterable[(LocalDate, Location, Double)])={
      val temp=iterable.par.map(x=>(x._3,1.0)).reduce((x,y)=>(x._1+y._1,x._2+y._2))
      temp._1/temp._2
    }
    records.groupBy(_._2).mapValues(avg)
  }

}
