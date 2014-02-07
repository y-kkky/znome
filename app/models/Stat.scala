package models

import play.api.db._
import play.api.Play.current
import play.api.cache._
import scala.slick.driver.PostgresDriver.simple._
import Helper.dbs

case class Stat(user_id: Long, bilet_id: Long, question_id: Long, right: Int, answer: String)

case class DailyStat(user_id: Long, question_id: Long, time: String, right: Int, answer: String, typ: Int)

case class microDailyStat(user_id: Long, time: String, result_time: Long, score: Double, ids: String, typ: Int)

case class BiletStat(user_id: Long, bilet_id: Long, ra: Int, max: Int)

class Stats(tag: Tag) extends Table[(Long, Long, Long, Int, String)](tag, "Stat") {
  def user_id = column[Long]("user_id", O.NotNull)
  def bilet_id = column[Long]("bilet_id", O.NotNull)
  def question_id = column[Long]("question_id", O.NotNull)
  def right = column[Int]("right_a", O.NotNull)
  def answer = column[String]("answer", O.NotNull)
  def * = (user_id, bilet_id, question_id, right, answer)
}
 
object Stat{
  
  def obertka(x: Option[(Long, Long, Long, Int, String)]): Stat = {
    val obj: (Long, Long, Long, Int, String) = x.getOrElse((0, 0, 0, 0, ""))
    Stat(obj._1, obj._2, obj._3, obj._4, obj._5)
  }
  
  def obertkaList(neobr: List[(Long, Long, Long, Int, String)]): List[Stat] = {
    for(each <- neobr) yield obertka(Some(each))
  }   
          
  val stats = TableQuery[Stats]
  
  def newStat(user_id: Long, bilet_id: Long, question_id: Long, right: Int, answer: String) = dbs.withSession { implicit session => 
    stats += (user_id, bilet_id, question_id, right, answer)
  }

  def find(user_id: Long, bilet_id: Long): List[Stat] = dbs.withSession { implicit session => 
    obertkaList(stats.filter(_.user_id === user_id).filter(_.bilet_id === bilet_id).sortBy(_.question_id).list)
  }
  
  def exists(user_id: Long, bilet_id: Long): Boolean = dbs.withSession { implicit session => 
    if (find(user_id, bilet_id).length>0)true else false
  }

  def deleteResolve(user_id: Long, bilet_id: Long) = dbs.withSession { implicit session => 
    stats.filter(_.user_id === user_id).filter(_.bilet_id === bilet_id).delete
    BiletStat.biletstats.filter(_.user_id === user_id).filter(_.bilet_id === bilet_id).delete
  }
} 

class BiletStats(tag: Tag) extends Table[(Long, Long, Int, Int)](tag, "BiletStat") {
  def user_id = column[Long]("user_id", O.NotNull)
  def bilet_id = column[Long]("bilet_id", O.NotNull)
  def ra = column[Int]("ra", O.NotNull)
  def max = column[Int]("max_a", O.NotNull)
  def * = (user_id, bilet_id, ra, max)
}

object BiletStat{
  
  def obertka(x: Option[(Long, Long, Int, Int)]): BiletStat = {
    val obj: (Long, Long, Int, Int) = x.getOrElse((0, 0, 0, 0))
    BiletStat(obj._1, obj._2, obj._3, obj._4)
  }
  
  def obertkaList(neobr: List[(Long, Long, Int, Int)]): List[BiletStat] = {
    for(each <- neobr) yield obertka(Some(each))
  }   
  
  val biletstats = TableQuery[BiletStats] 
  
  def newBiletStat(user_id: Long, bilet_id: Long, ra: Int, max: Int) = dbs.withSession { implicit session => 
    val cached = Cache.getOrElse("test_count"){
      count
    }
    Cache.set("test_count", cached+1, 60*120)
    biletstats += (user_id, bilet_id, ra, max)    
  }
  
  def find(user_id: Long, bilet_id: Long): BiletStat = dbs.withSession { implicit session => 
    obertka(biletstats.filter(_.user_id === user_id).where(_.bilet_id === bilet_id).firstOption)
  }

  def solved(user_id: Long, bilet_id: Long): Boolean = dbs.withSession { implicit session => 
    val bilStat = find(user_id, bilet_id)
    var max = 1
    if(bilStat.max != 0) max = bilStat.max
    val perc = (bilStat.ra*100)/max
    if(perc > 70) true else false
  }

  def count: Long = dbs.withSession { implicit session => 
    Cache.getOrElse("test_count", 60*120) {
      biletstats.list.length
    }
  }

}

class DailyStats(tag: Tag) extends Table[(Long, Long, String, Int, String, Int)](tag, "DailyStat") {
  def user_id = column[Long]("user_id", O.NotNull)
  def question_id = column[Long]("question_id", O.NotNull)
  def time = column[String]("curr_time", O.NotNull)
  def right = column[Int]("right_a", O.NotNull)
  def answer = column[String]("answer", O.NotNull)
  def typ = column[Int]("typ", O.NotNull)
  def * = (user_id, question_id, time, right, answer, typ)
}

object DailyStat{
  
  def obertka(x: Option[(Long, Long, String, Int, String, Int)]): DailyStat = {
    val obj: (Long, Long, String, Int, String, Int) = x.getOrElse((0, 0, "", 0, "", 0))
    DailyStat(obj._1, obj._2, obj._3, obj._4, obj._5, obj._6)
  }
  
  def obertkaList(neobr: List[(Long, Long, String, Int, String, Int)]): List[DailyStat] = {
    for(each <- neobr) yield obertka(Some(each))
  }   
    
  val dailystats = TableQuery[DailyStats]
  
  def newDailyStat(user_id: Long, question_id: Long, time: String, right: Int, answer: String, typ: Int) = dbs.withSession { implicit session => 
    dailystats += (user_id, question_id, time, right, answer, typ)
  }

  def find(user_id: Long, question_id: Long, time: String, typ: Int): DailyStat = dbs.withSession { implicit session => 
    obertka(dailystats.filter(_.user_id === user_id).filter(_.question_id === question_id)
        .filter(_.time === time).where(_.typ === typ).firstOption)
  }
}

class microDailyStats(tag: Tag) extends Table[(Long, String, Long, Double, String, Int)](tag, "microDailyStat") {
  def user_id = column[Long]("user_id", O.NotNull)
  def time = column[String]("curr_time", O.NotNull)
  def result_time = column[Long]("res_time", O.NotNull)
  def score = column[Double]("score", O.NotNull)
  def ids = column[String]("ids", O.NotNull)
  def typ = column[Int]("typ", O.NotNull)
  def * = (user_id, time, result_time, score, ids, typ)
}
  
object microDailyStat{ 

  def obertka(x: Option[(Long, String, Long, Double, String, Int)]): microDailyStat = {
    val obj: (Long, String, Long, Double, String, Int) = x.getOrElse((0, "", 0, 0, "", 0))
    microDailyStat(obj._1, obj._2, obj._3, obj._4, obj._5, obj._6)
  }
  
  def obertkaList(neobr: List[(Long, String, Long, Double, String, Int)]): List[microDailyStat] = {
    for(each <- neobr) yield obertka(Some(each))
  }   
  
  val microdailystats = TableQuery[microDailyStats]
  
  def create(user_id: Long, time: String, score: Double, ids: String, typ: Int) = dbs.withSession { implicit session =>
    val cached = Cache.getOrElse("daily_count"){
      count
    }
    Cache.set("daily_count", cached+1, 60*120)
    microdailystats.map(m => (m.user_id, m.time, m.result_time, m.score, m.ids, m.typ)) += (user_id, time, 0, score, ids, typ)
  }

  def update(user_id: Long, time: Long, current_date: String, score: Double, typ: Int) = dbs.withSession { implicit session => 
    val q = for { m <- microdailystats if m.user_id === user_id; if m.time === current_date;if m.typ === typ} yield (m.user_id, m.result_time, m.score, m.typ)
    // ВОЗМЖОНО ОШИБКА
    q.update(user_id, time, score, typ)
  }

  def getForCheck(user_id: Long, time: String, typ: Int): List[microDailyStat] = dbs.withSession { implicit session => 
    obertkaList(microdailystats.filter(_.user_id === user_id).filter(_.time === time).filter(_.typ === typ).list)
  }
  
  def getByUser(user_id: Long, typ: Int): List[microDailyStat] = dbs.withSession { implicit session => 
    obertkaList(microdailystats.filter(_.user_id === user_id).filter(_.typ === typ).sortBy(_.time).list)
  }

  def getByUserOnly(user_id: Long): List[microDailyStat] = dbs.withSession { implicit session => 
    obertkaList(microdailystats.filter(_.user_id === user_id).sortBy(_.time).list)
  }


  def rate(pattern: String, page: Long, typ: Int): List[microDailyStat] = dbs.withSession { implicit session => 
    obertkaList(microdailystats.filter(_.typ === typ).filter(_.time like pattern).sortBy(x => (x.score.desc, x.result_time.asc)).drop(page.toInt).take(15).list)
  }

  def getMaxResults(user_id: Long): List[microDailyStat] = dbs.withSession { implicit session => 
    obertkaList(microdailystats.filter(_.user_id === user_id).filter(_.score.equals(100)).list)
  }

  def count: Long = dbs.withSession { implicit session =>
    Cache.getOrElse("daily_count", 60*120) {
      microdailystats.list.length
    }
  }
}
