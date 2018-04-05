import java.io.FileWriter
import java.net.URLEncoder

import com.merlin.common.AsciiFolder

import scala.io.Source

object Fantasy {
  val ParenthesisRE = "\\(.*\\)".r

  val words = """([a-zA-Z]+)""".r

  val pathPrefix = "/Users/hkang/Dropbox/fantasy/2018/"

  implicit class StringPimps(val s: String) extends AnyVal {
    def doubleString(default: Double = 0): String = {
      scala.util.Try {
        val n  = s.trim.toDouble
        if (n >= 0)
          (n * 10).toLong / 10.0
        else
          n.toLong / 10.0
      }.toOption.getOrElse(default).toString
    }

    def zeroPatchedIntString(default: Int = 0): String = {
      scala.util.Try {
        s.trim.toDouble.toInt
      }.toOption.getOrElse(default) match { 
        case n if n < 10 => ("00" + n.toString)
        case n if n < 100 => ("0" + n.toString)
        case n => n.toString
      }
    }

    def intString(default: Int = 0): String = {
      scala.util.Try {
        s.trim.toDouble.toInt
      }.toOption.getOrElse(default).toString
    }

    def normalize: String = {
      AsciiFolder(s.toLowerCase).trim.replaceAll("[^a-z ]", "")
    }

    def camel: String = {
      words.replaceAllIn(s, _.matched.toLowerCase.capitalize)
    }

    def name: String = {
      s.normalize.trim match {
        case "vincent velasquez"   => "vince velasquez"
        case "seunghwan oh"        => "seung hwan oh"
        case "jackie bradley jr"      => "jackie bradley"
        case "lance mccullers jr"     => "lance mccullers"
        case "kenneth giles"       => "ken giles"
        case "troy tuitzki"        => "troy tulowitzki"
        case "alexander colome"    => "alex colome"
        case "jonathan gray"       => "jon gray"
        case "gregory bird"        => "greg bird"
        case "michael foltynewicz" => "mike foltynewicz"
        case "cameron bedrosian"   => "cam bedrosian"
        case "jungho kang"         => "jung ho kang"
        case "nicholas castellanos" => "nick castellanos"
        case "jacob faria"         => "jake faria"
        case s                     => s
      }
    }
  }

  type Data = Seq[(String, Seq[(String, String)])]

  def parseYahooRank(file: String): Data = {
    // https://sports.yahoo.com/blogs/Roto%20Arcade/2017-fantasy-baseball-preseason-rankings--back-in-business-230459824.html
    val path = pathPrefix + file
    Source.fromFile(path).getLines.toSeq.flatMap { line =>
      line.split('\t') match {
        case arr if arr.size >= 4 =>
          val rAll   = arr(0)
          val player = arr(1)
          val rPiano = arr(3)
          val name = ParenthesisRE.replaceFirstIn(player, "").name
          val data = Seq(
            "r_y_all"   -> rAll.trim.intString(400),
            "r_y_piano" -> rPiano.trim.intString(400)
          )
          Some(name -> data)
        case arr if arr.size >= 2 =>
          val rAll   = arr(0)
          val player = arr(1)
          val name = ParenthesisRE.replaceFirstIn(player, "").name
          val data = Seq(
            "r_y_all"   -> rAll.trim.intString(400)
          )
          Some(name -> data)
        case arr =>
          println("FAILED: " + arr.mkString("[", ", ", "]").toString)
          None
      }
    }
  }

  def parseYahooValue(file: String): Data = {
    val path = pathPrefix + file
    val SuffixRegex = " [A-Z][A-Za-z][A-Za-z]? - .*".r
    val players = Source.fromFile(path).getLines.filter { line =>
      line.startsWith("$") || line.contains(" - ")
    }.toList.grouped(3)
    players.flatMap {
      case player :: v1 :: v2 :: Nil =>
        val name = SuffixRegex.replaceAllIn(player, "").name
        val pos  = player.split(' ').last.replace(',', '/').trim
        val data = Seq(
          "pos"      -> pos,
          "v_y_proj" -> v1.trim.stripPrefix("$").intString(0),
          "v_y_avg"  -> v2.trim.stripPrefix("$").doubleString(0)
        )
        Some(name -> data)
      case _ =>
        println("FAILED yahooValue")
        None
    }
  }.toSeq

  def parsePianoValue(file: String): Data = {
    val path = pathPrefix + file
    val SuffixRegex = " [A-Z][A-Za-z][A-Za-z]? - .*".r

    Source.fromFile(path).getLines.toSeq.flatMap { line =>
      line.stripPrefix("$").split(' ').toList match {
        case v :: nameList =>
          val name = nameList.mkString(" ").name
          Some(name -> Seq("v_piano" -> v.toString))
        case _ =>
          println("parse pianoValue Failed: " + line)
          None
      }
    }
  }

  def parseRotoBaller(file: String): Data = {
    val path = pathPrefix + file
    // https://www.rotoballer.com/2017-fantasy-baseball-rankings/253440#!/
    // Rank,Tier,Player,$,Target Round,AVG,Diff,NFBC,Y!,RTS
    Source.fromFile(path).getLines.toSeq.flatMap { line =>
      line.split(',').toList match {
        case r :: _ :: name :: value :: _ :: adp :: _ :: _ :: _ :: _ =>
          val data = Seq(
            "r_rb" -> r.trim.intString(400),
            "v_rb" -> value.trim.intString(0),
            "r_adp_rb"  -> adp.trim.doubleString(400)
          )
          Some(name.name -> data)
        case arr =>
          println("FAILED RB: " + arr.mkString("[", ", ", "]").toString)
          None
      }
    }
  }

  def parseEspn(file: String): Data = {
    val path = pathPrefix + file
    // http://www.espn.com/fantasy/baseball/story/_/page/mlbdk2k17_draftkit/mlb-2017-fantasy-baseball-draft-kit-rankings-projections-cheat-sheets-strategy
    Source.fromFile(path).getLines.toSeq.flatMap { line =>
      line.split('\t').toList match {
        case key :: _ :: _ :: _ :: _ :: _  :: v :: _ =>
          val split = key.split(" ")
          val r = split.head.stripPrefix(".")
          val name = split.tail.mkString(" ")
          val data = Seq(
            "r_espn" -> r.trim.stripSuffix(".").intString(400),
            "v_espn" -> v.stripPrefix("$").intString(0)
          )
          Some(name.name -> data)
        case arr =>
          println("FAILED ESPN : " + arr.mkString("[", ", ", "]").toString)
          None
      }
    }
  }

  def parseKarabell(file: String): Data = {
    val path = pathPrefix + file
    Source.fromFile(path).getLines.toSeq.flatMap { line =>
      line.split('\t').toList match {
        case key :: _ =>
          val split = key.split(" ")
          val r = split.head.stripPrefix(".")
          val name = split.tail.mkString(" ")
          val data = Seq(
            "r_karabell" -> r.trim.stripSuffix(".").intString(400)
          )
          Some(name.name -> data)
        case arr =>
          println("FAILED karabel: " + arr.mkString("[", ", ", "]").toString)
          None
      }
    }
  }

  def parseFProRank(file: String): Data = {
    val path = pathPrefix + file
    Source.fromFile(path).getLines.toSeq.flatMap { line =>
      line.split("\",\"").toList match {
        case r :: key :: _ :: pos :: best :: worst :: rAvg :: _ =>
          val name = key.normalize.trim.name
          val data = Seq(
            "r_fpro_rank"  -> r.stripPrefix("\"").trim.intString(400),
            "r_fpro_best"  -> best.trim.intString(400),
            "r_fpro_worst" -> worst.trim.intString(400),
            "pos"     -> pos.replace(',', '/').trim,
            "r_fpro_avg"   -> rAvg.trim.doubleString(400)
          )
          Some(name -> data)
        case failure =>
          println("FAILED fpro rank: " + failure.mkString("[", ", ", "]").toString)
          None
      }
    }
  }.toSeq

  def parseFProValue(file: String): Data = {
    val path = pathPrefix + file
    Source.fromFile(path).getLines.map { line =>
      val split = line.split("\t")
      val name = ParenthesisRE.replaceFirstIn(split.head, "").name
      val value = split.last
      val data = Seq(
        "v_fpro" -> value.stripPrefix("$").trim.doubleString(0)
      )
      name -> data
    }.toSeq
  }

  def parseFanProj(file: String, colName: String): Data = {
    //"PlayerName","Team","POS","ADP","PA","mAVG","mRBI","mR","mSB","mHR","PTS","aPOS","Dollars"
    val path = pathPrefix + file
    Source.fromFile(path).getLines.toList.tail.map { line =>
      val split = line.split(',')
      val name = split.head.name
      val value = split.last.stripPrefix("\"$").stripSuffix("\"").replaceAll("\"\\(\\$", "-").stripSuffix(")").trim.doubleString(0)
      name -> Seq(colName -> value)
    }
  }

  def parseFansPos(file: String, position: String): Data = {
    val path = pathPrefix + file
    Source.fromFile(path).getLines.toList.map { line =>
      val split = line.split('\t')
      val name = split(1).name
      val paul = split(2).trim.zeroPatchedIntString(400)
      val jeff = split(4).trim.zeroPatchedIntString(400)
      val rank = split.reverse(2).trim.zeroPatchedIntString(400)
      name -> Seq(
        "pos_r_fans" -> (position + " " + rank),
        "pos_r_paul" -> (position + " " + paul),
        "pos_r_jeff" -> (position + " " + jeff)
      )
    }
  }

  def parse(file: String) = {
    val path = pathPrefix + file
    val dataList = List(
      parseYahooRank("yahoo_rank.txt"),
      parseYahooValue("yahoo_value.txt"),
      parsePianoValue("piano_value.txt"),
//      parseRotoBaller("rotoballer.csv"),
//      parseEspn("espn_cockcroft.txt"),
      parseKarabell("espn_karabell.txt"),
      parseFProRank("fpro_rank.csv"),
      parseFProValue("fpro_value.txt"),
      parseFanProj("steamer_batters.csv",  "v_steamer"),
      parseFanProj("steamer_pitchers.csv", "v_steamer"),
//      parseFanProj("fans_batters.csv",  "v_fans"),
//      parseFanProj("fans_pitchers.csv", "v_fans"),
//      parseFanProj("depth_batters.csv",  "v_depth"),
//      parseFanProj("depth_pitchers.csv", "v_depth")
      parseFansPos("fans_1b.tsv", "1B"),
      parseFansPos("fans_2b.tsv", "2B"),
      parseFansPos("fans_of.tsv", "OF"),
      parseFansPos("fans_3b.tsv", "3B"),
      parseFansPos("fans_ss.tsv", "SS"),
      parseFansPos("fans_c.tsv",  "C "),
      parseFansPos("fans_rp.tsv", "RP"),
      parseFansPos("fans_sp.tsv", "SP")
    )

    val Fields = Seq(
      "pos", "pos_r_fans", "pos_r_paul", "pos_r_jeff",  "r_karabell", "r_y_all", "r_y_piano", "r_fpro_avg", "r_fpro_rank", "avg_r",
      "gang9_v", "avg_v", "v_y_proj", "v_y_avg", "v_fpro", "v_piano", "v_steamer"
    )
    val writer = new FileWriter(path, false)
    writer.write((Seq("name") ++ Fields).mkString("\t") + "\n")
    dataList.flatten.groupBy(_._1).toSeq.sortBy(_._1).foreach { case (name, values) =>
      val m = values.map(_._2).flatten.toMap
      if (m.size > 5) {
        val nameEnc   = URLEncoder.encode(name.toLowerCase, "UTF-8")
        val lastName  = name.toLowerCase.trim.split(' ').last
        val firstName  = name.toLowerCase.trim.split(' ').head
        val fangraphs = s"http://www.fangraphs.com/players.aspx?lastname=${nameEnc}"
        val mlb       = s"http://www.google.com/search?q=mlb.com+${nameEnc}&btnI"
        val rotoworld_google = s"http://www.google.com/search?q=rotoworld.com+profile+${nameEnc}&btnI"
        val rotoworld = s"http://www.rotoworld.com/content/playersearch.aspx?searchname=${lastName},+${firstName}&sport=mlb"
        val fields = Fields.map { f =>
          m.getOrElse(f, {
            if (f.startsWith("r_")) {
              "400"
            } else if (f == "v_steamer" || f == "v_fans" || f == "v_depth") {
              "-5"
            } else if (f.startsWith("v_")) {
              "0"
            } else {
              ""
            }
          })
        }
        val output = (Seq(name.camel) ++ fields ++ Seq(fangraphs, rotoworld, mlb)).mkString("\t")
        writer.write(output)
        writer.write('\n')
      }
    }
    writer.close
  }
}