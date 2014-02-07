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

  def update(id: Long, name: String, lesson_id: Long, time: String) = dbs.withSession { implicit session =>
    val q = for { b <- bilets if b.id === id } yield (b.lesson_id, b.name, b.time)
    q.update(lesson_id, name, time)
    val cached = Cache.getOrElse("bilet_cache" + id){
      Bilet.find(id)
    }
    Cache.set("bilet_cache" + id, cached.copy(lesson_id = lesson_id, name= name, time = time), 60*120)
  }

  def getLast: Bilet = dbs.withSession { implicit session => 
    obertka(bilets.sortBy(_.id.desc).firstOption)
  }

  def inLesson(lesson_id: Long): List[Bilet] = dbs.withSession { implicit session => 
    obertkaList(bilets.filter(_.lesson_id === lesson_id).sortBy(_.id).list)
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
    obertkaList(questions.filter(_.bilet_id === bilet_id).sortBy(_.id).list)
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
        obertkaList(variants.filter(_.question_id === question_id).sortBy(_.id).list)
      }	
    }
}
