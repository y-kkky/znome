package models

import play.api.db._
import play.api.Play.current
import play.api.cache._
import scala.slick.driver.PostgresDriver.simple._
import Helper.dbs
import akka.util.Crypt

case class mUser(id: Long, regtime: String, email: String, pass: String, rank: Int, role: Int)
case class mProfile(user_id: Long, name: String, city: String,
  school: String, comments: String, lessons: String)

class Users(tag: Tag) extends Table[(Long, String, String, String, Int, Int)](tag, "Users"){
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def regtime = column[String]("regtime", O.NotNull)
  def email = column[String]("email", O.NotNull)
  def pass = column[String]("pass", O.NotNull)
  def rank = column[Int]("rank", O.NotNull)
  def role = column[Int]("role", O.NotNull)
  def * = (id, regtime, email, pass, rank, role)
}
 
object mUser{
  
  def obertka(x: Option[(Long, String, String, String, Int, Int)]): mUser = {
    val obj: (Long, String, String, String, Int, Int) = x.getOrElse((0, "", "", "", 1, 0))
    mUser(obj._1, obj._2, obj._3, obj._4, obj._5, obj._6)
  }
  
  def obertkaList(neobr: List[(Long, String, String, String, Int, Int)]): List[mUser] = {
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
    val id = (users.map(u => (u.regtime, u.email, u.pass, u.rank, u.role)) 
        returning users.map(_.id)) += (timestamp, email.trim(), hashPass(pass.trim()), 1, 0)
    mProfile.create(id, name)
    id
  }
  
  def edit(id: Long, email: String, pass: String) = dbs.withSession { implicit session =>
    val q = for { u <- users if u.id === id } yield (u.email, u.pass)
    q.update(email.trim, pass)
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

// *************
// *ADMIN PANEL*
// *************
}

class Profiles(tag: Tag) extends Table[(Long, String, String, String, String, String)](tag, "Profiles"){
  def user_id = column[Long]("user_id", O.PrimaryKey)
  def name = column[String]("name", O.NotNull)
  def city = column[String]("city")
  def school = column[String]("school")
  def comments = column[String]("comments")
  def lessons = column[String]("lessons")
  def * = (user_id, name, city, school, comments, lessons)
}

object mProfile{

  def obertka(x: Option[(Long, String, String, String, String, String)]): mProfile = {
    val obj: (Long, String, String, String, String, String) = x.getOrElse((0, "", "", "", "", ""))
    mProfile(obj._1, obj._2, obj._3, obj._4, obj._5, obj._6)
  }
  
  def obertkaList(neobr: List[(Long, String, String, String, String, String)]): List[mProfile] = {
    for(each <- neobr) yield obertka(Some(each))
  } 

  val profiles = TableQuery[Profiles]

  def find(id: Long): mProfile = dbs.withSession { implicit session =>
    Cache.getOrElse("profile_cache" + id, 60*120) {
      obertka(profiles.where(x => x.user_id === id).firstOption)
    }
  }

  def create(user_id: Long, name: String) = dbs.withSession { implicit session =>  
    val timestamp: String = System.currentTimeMillis.toString
    profiles.map(p => (p.user_id, p.name, p.city, p.school, p.comments, p.lessons)) += (user_id, name, "", "", "", "")
  }

  def edit(id: Long, name: String, city: String, school: String, comments: String) = dbs.withSession { implicit session =>  
    val q2 = for {p <- mProfile.profiles if p.user_id === id} yield (p.name, p.city, p.school, p.comments)
    q2.update(name, city, school, comments)
  }


  def editLessons(id: Long, lessons: String) = dbs.withSession { implicit session =>  
    val q = for { p <- profiles if p.user_id === id } yield p.lessons
    q.update(lessons)
  }

  def findPattern(name: String, city: String, dropping: Int = 0): List[mUser] = dbs.withSession { implicit session =>
    //    for {a
    //      u <- users
    //      p <- Profiles.profiles if (p.name like name && p.city like city)
    //    }
    val finders = obertkaList(profiles.filter(_.name like name).filter(_.city like city).sortBy(_.user_id.desc).drop(dropping).take(20).list)
    for (f <- finders) yield (mUser.find(f.user_id))
    //    obertkaList(users.filter(_.name like name).filter(_.city like city).sortBy(_.id.desc).take(20).list)
  }
}
