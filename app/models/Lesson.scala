package models

import play.api.db._
import play.api.Play.current
import play.api.cache._
import scala.slick.driver.PostgresDriver.simple._
import Helper.dbs

case class Lesson(id: Long, name: String)

case class Bilet(id: Long, lesson_id: Long, time: String, name: String)

case class Question(id: Long, bilet_id: Long, typ: Int, text: String, image: String, answer: String)

case class Variant(id: Long, question_id: Long, text: String)

case class Stat(user_id: Long, bilet_id: Long, question_id: Long, right: Int, answer: String)

case class DailyStat(user_id: Long, question_id: Long, time: String, right: Int, answer: String, typ: Int)

case class microDailyStat(user_id: Long, time: String, result_time: Long, score: Double, ids: String, typ: Int)

case class BiletStat(user_id: Long, bilet_id: Long, ra: Int, max: Int)

class Lessons(tag: Tag) extends Table[(Long, String)](tag, "Lessons"){
  def id = column[Long]("id", O.AutoInc, O.PrimaryKey)
  def name = column[String]("name", O.NotNull)
  def * = (id, name)
}

object Lesson{
  def obertka(x: Option[(Long, String)]): Lesson = {
    val obj: (Long, String) = x.getOrElse((0, ""))
    Lesson(obj._1, obj._2)
  }
  
  def obertkaList(neobr: List[(Long, String)]): List[Lesson] = {
    for(each <- neobr) yield obertka(Some(each))
  } 
  
  val lessons = TableQuery[Lessons]
  
  def find(id: Long): Lesson = dbs.withSession { implicit session => 
    Cache.getOrElse("lesson_cache" + id, 60*120){
        obertka(lessons.where(_.id === id).firstOption)
    }
  }

  def findByName(name: String): Lesson = dbs.withSession { implicit session => 
    obertka(lessons.where(_.name === name).firstOption)
  }

  def findAll: List[Lesson] = dbs.withSession { implicit session => 
    obertkaList(lessons.sortBy(_.id).list)
  }
}

class Bilets(tag: Tag) extends Table[(Long, Long, String, String)](tag, "Bilets"){
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def lesson_id = column[Long]("lesson_id", O.NotNull)
  def time = column[String]("time", O.NotNull)
  def name = column[String]("name", O.NotNull)
  def * = (id, lesson_id, time, name)
}

object Bilet{ 
  def obertka(x: Option[(Long, Long, String, String)]): Bilet = {
    val obj: (Long, Long, String, String) = x.getOrElse((0, 0, "", ""))
    Bilet(obj._1, obj._2, obj._3, obj._4)
  }
  
  def obertkaList(neobr: List[(Long, Long, String, String)]): List[Bilet] = {
    for(each <- neobr) yield obertka(Some(each))
  } 
  
  val bilets = TableQuery[Bilets]
  
  def find(id: Long): Bilet = dbs.withSession { implicit session => 
    Cache.getOrElse("bilet_cache" + id, 60*120){
        obertka(bilets.where(_.id === id).firstOption)
    }
  }

  def create(lesson_id: Long, time: String, name: String): Long = dbs.withSession { implicit session => 
    val biletId = 
      (bilets.map(b => (b.lesson_id, b.time, b.name)) returning bilets.map(_.id)) += (lesson_id, time, name)
      biletId
  }

  def getLast: Bilet = dbs.withSession { implicit session => 
    obertka(bilets.sortBy(_.id.desc).firstOption)
  }

  def inLesson(lesson_id: Long): List[Bilet] = dbs.withSession { implicit session => 
    obertkaList(bilets.filter(_.lesson_id === lesson_id).list)
  }
}

class Questions(tag: Tag) extends Table[(Long, Long, Int, String, String, String)](tag, "Questions") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def bilet_id = column[Long]("bilet_id", O.NotNull)
  def typ = column[Int]("typ", O.NotNull)
  def text = column[String]("text", O.NotNull)
  def image = column[String]("image", O.NotNull)
  def answer = column[String]("answer", O.NotNull)
  def * = (id, bilet_id, typ, text, image, answer)
}

object Question{
  
  def obertka(x: Option[(Long, Long, Int, String, String, String)]): Question = {
    val obj: (Long, Long, Int, String, String, String) = x.getOrElse(0, 0, 0, "", "", "")
    Question(obj._1, obj._2, obj._3, obj._4, obj._5, obj._6)
  }
  
  def obertkaList(neobr: List[(Long, Long, Int, String, String, String)]): List[Question] = {
    for(each <- neobr) yield obertka(Some(each))
  }   
  
  val questions = TableQuery[Questions]
  
  def create(bilet_id: Long, typ: Int, text: String, image: String, answer: String): Long = dbs.withSession { implicit session => 
    val questId = 
      (questions.map(q => (q.bilet_id, q.typ, q.text, q.image, q.answer)) returning questions.map(_.id)) += (bilet_id, typ, text, image, answer)
      questId
  }

  def getLast: Question = dbs.withSession { implicit session => 
    obertka(questions.sortBy(_.id.desc).firstOption)
  }

  def find(id: Long): Question = dbs.withSession { implicit session => 
    Cache.getOrElse("question_cache" + id, 60*120){
        obertka(questions.where(_.id === id).firstOption)
    }
  }

  def findByBilet(bilet_id: Long): List[Question] = dbs.withSession { implicit session => 
    obertkaList(questions.filter(_.bilet_id === bilet_id).list)
  }

  // Генерит рандомные вопросы
  
  def random(typ: Int, bilet_id: Long): Question = dbs.withSession { implicit session => 
    scala.util.Random.shuffle(obertkaList(questions.filter(_.typ === typ).filter(_.bilet_id === bilet_id).list)).head
  }

}

class Variants(tag: Tag) extends Table[(Long, Long, String)](tag, "Variants") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def question_id = column[Long]("question_id", O.NotNull)
  def text = column[String]("text", O.NotNull)
  def * = (id, question_id, text)
}
 
object Variant{
  
  def obertka(x: Option[(Long, Long, String)]): Variant = {
    val obj: (Long, Long, String) = x.getOrElse((0, 0, ""))
    Variant(obj._1, obj._2, obj._3)
  }
  
  def obertkaList(neobr: List[(Long, Long, String)]): List[Variant] = {
    for(each <- neobr) yield obertka(Some(each))
  }   
  
  val variants = TableQuery[Variants]
    
  def create(question_id: Long, text: String) = dbs.withSession { implicit session => 
    variants.map(v => (v.question_id, v.text)) += (question_id, text)
  }

  def find(id: Long): Variant = dbs.withSession { implicit session => 
    obertka(variants.where(_.id === id).firstOption)
  }

  def findByQuestion(question_id: Long): List[Variant] = dbs.withSession { implicit session => 
    Cache.getOrElse("variant_cache" + question_id, 60*120){
        obertkaList(variants.filter(_.question_id === question_id).list)
      }	
    }
}

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
  def result_time = column[Long]("res_time")
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
    microdailystats.map(m => (m.user_id, m.time, m.score, m.ids, m.typ)) += (user_id, time, score, ids, typ)
  }

  def update(user_id: Long, time: Long, current_date: String, score: Double, typ: Int) = dbs.withSession { implicit session => 
    val q = for { m <- microdailystats if m.user_id === user_id; if m.time === current_date;if m.typ === typ} yield (m.user_id, m.time, m.score, m.typ)
    // ВОЗМЖОНО ОШИБКА
    q.update(user_id, current_date, score, typ)
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
//      SQL("select user_id, curr_time, res_time, score, ids, typ from microDailyStat where typ ={typ} and curr_time LIKE {pattern} ORDER BY score DESC, res_time ASC LIMIT 15 OFFSET {page}").on(
  }

  def getMaxResults(user_id: Long): List[microDailyStat] = dbs.withSession { implicit session => 
    obertkaList(microdailystats.filter(_.user_id === user_id).filter(_.score == 100).list)
  }

  def count: Long = dbs.withSession { implicit session =>
    Cache.getOrElse("daily_count", 60*120) {
      microdailystats.list.length
    }
  }
 

}
