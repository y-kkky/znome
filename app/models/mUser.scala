package models

import play.api.db._
import play.api.Play.current
import play.api.cache._
import scala.slick.driver.PostgresDriver.simple._
import Helper.dbs
import akka.util.Crypt

case class mUser(id: Long, regtime: String, email: String, name: String,
                 city: String, school: String, comments: String, lessons: String, pass: String, rank: Int)

class Users(tag: Tag) extends Table[(Long, String, String, String, String, String, String, String, String, Int)](tag, "Users"){
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def regtime = column[String]("regtime", O.NotNull)
  def email = column[String]("email", O.NotNull)
  def name = column[String]("name", O.NotNull)
  def city = column[String]("city")
  def school = column[String]("school")
  def comments = column[String]("comments")
  def lessons = column[String]("lessons")
  def pass = column[String]("pass", O.NotNull)
  def rank = column[Int]("rank", O.NotNull)
  def * = (id, regtime, email, name, city, school, comments, lessons, pass, rank)
}
 
object mUser{
  
  def obertka(x: Option[(Long, String, String, String, String, String, String, String, String, Int)]): mUser = {
    val obj: (Long, String, String, String, String, String, String, String, String, Int) = x.getOrElse((0, "", "", "", "", "", "", "", "", 1))
    mUser(obj._1, obj._2, obj._3, obj._4, obj._5, obj._6, obj._7, obj._8, obj._9, obj._10)
  }
  
  def obertkaList(neobr: List[(Long, String, String, String, String, String, String, String, String, Int)]): List[mUser] = {
    for(each <- neobr) yield obertka(Some(each))
  } 
  
  val users = TableQuery[Users]

  def findByEmail(email: String): mUser = dbs.withSession { implicit session =>
    obertka(users.where(x => x.email === email).firstOption)
  }

  def find(user_id: Long): mUser = dbs.withSession { implicit session =>  
    Cache.getOrElse("user_cache" + user_id, 60*120) {
      obertka(users.where(_.id === user_id).firstOption)
    }
  }

  def findPattern(name: String, city: String): List[mUser] = dbs.withSession { implicit session =>
    obertkaList(users.filter(_.name like name).filter(_.city like city).sortBy(_.id.desc).take(20).list)
  }
  
  def findAll: List[mUser] = dbs.withSession { implicit session =>  
    obertkaList(users.sortBy(_.id).list)
  }
  
  def checkUser(email: String): Boolean = dbs.withSession { implicit session =>  
    if(mUser.findByEmail(email).email==email) false else true
  }
  
  /**
   * Authenticate a User.
   */
  def authenticate(email: String, pass: String): Boolean = dbs.withSession { implicit session =>  
    if (obertka(users.filter(_.email === email).where(_.pass === hashPass(pass)).firstOption).id==0) false else true
  }

  def create(email: String, name: String, pass: String): Long = dbs.withSession { implicit session =>  
    val cached = Cache.getOrElse("user_count"){
      mUser.count
    }
    Cache.set("user_count", cached+1, 60*120)
    val timestamp: String = System.currentTimeMillis.toString
    val id = (users.map(u => (u.regtime, u.email, u.name, u.city, u.school, u.comments, u.lessons, u.pass, u.rank)) 
        returning users.map(_.id)) += (timestamp, email, name, "", "", "", "", pass, 1)
    id
  }
  
  def edit(id: Long, email: String, name: String, pass: String, city: String, school: String, comments: String) = dbs.withSession { implicit session =>  
    // ОШИБКА 
    val q = for { u <- users if u.id === id } yield (u.id, u.email, u.name, u.city, u.school, u.comments, u.pass, u.rank)
    q.update(id, email.trim(), name, city, school, comments, pass.trim(), 0)
  }

  def editLessons(id: Long, lessons: String) = dbs.withSession { implicit session =>  
    val q = for { u <- users if u.id === id } yield u.lessons
    q.update(lessons)
    }

  def updateRank(user_id: Long, rank: Int) = dbs.withSession { implicit session =>  
	val q = for { u <- users if u.id === user_id } yield u.rank
    q.update(rank)
  }

  def count: Long = dbs.withSession { implicit session =>  
    Cache.getOrElse("user_count", 60*120) {
       users.list.length
    }
  }

  def hashPass(password: String): String =
    Crypt.sha1(Crypt.md5(Crypt.sha1(password)))

}
