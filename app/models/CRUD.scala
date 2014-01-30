package models

import play.api.db._
import play.api.Play.current
import scala.slick.driver.PostgresDriver.simple._
import Database.dynamicSession

object Helper {
  
  val dbs = Database.forDataSource(play.api.db.DB.getDataSource())
  //def database = Database.forUrl("postgres://postgres:last:partizan@localhost:5432/testplay", driver = "org.postgresql.Driver")
  
}
