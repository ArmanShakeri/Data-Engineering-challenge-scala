import org.json4s._
import org.json4s.jackson.JsonMethods._
import java.io._

object MetricsCalculator {
  case class Impression(id: String, app_id: Int, country_code: String, advertiser_id: Int)
  case class Click(impression_id: String, revenue: Double)

  def main(args: Array[String]): Unit = {
    val impressionFiles = "impressions.json".split(",").toList
    val clickFiles = "clicks.json".split(",").toList

    val impressions = readEvents[Impression](impressionFiles)
    val clicks = readEvents[Click](clickFiles)

    val groupedImpressions = impressions.groupBy(impression => (impression.app_id, impression.country_code))
    val groupedClicks = clicks.groupBy(click => impressions.find(_.id == click.impression_id).map(impression => (impression.app_id, impression.country_code)))

    val impressionCounts = groupedImpressions.mapValues(_.size)
    val clickCounts = groupedClicks.collect {
      case (Some(dimensions), clicks) => (dimensions, clicks.size)
    }.toMap
    val revenueSums = groupedClicks.collect {
      case (Some(dimensions), clicks) => (dimensions, clicks.map(_.revenue).sum)
    }.toMap

    val data = impressionCounts.map { case (dimensions, impressions) =>
      val clicks = clickCounts.getOrElse(dimensions, 0)
      val revenue = revenueSums.getOrElse(dimensions, 0.0)
      val json =
        s"""
               {
                 "app_id": ${dimensions._1},
                 "country_code": "${dimensions._2}",
                 "impressions": $impressions,
                 "clicks": $clicks,
                 "revenue": $revenue
               }
             """
      parse(json)
    }.toList

    val result = pretty(render(JArray(data)))

    writeToFile("section2.json",result)

    val groupedImpressions2 = impressions.groupBy(impression => (impression.app_id, impression.country_code,impression.advertiser_id))
    val impressionCounts2 = groupedImpressions2.mapValues(_.size)
    val groupedClicks2 = clicks.groupBy(click => impressions.find(_.id == click.impression_id).map(impression =>
      (impression.app_id, impression.country_code,impression.advertiser_id)))

    val revenueSums2 = groupedClicks2.collect {
      case (Some(dimensions), clicks) => (dimensions, clicks.map(_.revenue).sum)
    }.toMap

    val data2 = impressionCounts2.flatMap{case (dimensions, impressions) =>
    revenueSums2.get(dimensions).map{revenueSums2 =>
      dimensions -> (impressions,revenueSums2)
    }
    }
    val data3 = data2.map{case (dimensions, (impressions,revenueSums2))=>
      (dimensions,revenueSums2/impressions)
    }


    val groupedMap = data3.groupBy{ case ((app_id, country_code, _), _) => (app_id, country_code) }

    val top5Advertisers = groupedMap.mapValues{ rates =>
      rates.toSeq.sortBy{ case (_, rate) => -rate }.take(5).map{ case ((_, _, advertiser_id), _) => advertiser_id }
    }

    
    val output = top5Advertisers.map { case ((app_id, country_code), advertisers) =>
      Map("app_id" -> app_id, "country_code" -> country_code, "recommended_advertiser_ids" -> advertisers)
    }

    val json2 = output.map{ map =>
      val recommendedAdIds = map("recommended_advertiser_ids") match {
        case seq: Seq[Int] => seq.mkString(", ")
        case other => throw new IllegalArgumentException(s"Invalid type for recommended_advertiser_ids: ${other.getClass}")
      }
      s"""
             {
               "app_id": ${map("app_id")},
               "country_code": "${map("country_code")}",
               "recommended_advertiser_ids": [$recommendedAdIds]
             }
           """
    }.mkString("[", ",\n", "]")

    writeToFile("section3.json",json2)
  }

  def writeToFile(filePath: String, output: String): Unit = {
    val pw = new PrintWriter(new File(filePath))
    pw.write(output)
    pw.close()
  }

  private def readEvents[T: Manifest](files: List[String]): List[T] = {
    implicit val formats: Formats = DefaultFormats
    files.flatMap { file =>
      val jsonString = scala.io.Source.fromFile(file).mkString
      parseOpt(jsonString) match {
        case Some(JArray(arr)) =>
          val extracted = arr.flatMap(_.extractOpt[T])
          extracted
        case _ => Nil
      }
    }
  }
}