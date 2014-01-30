package controllers

import play.api._
import play.api.mvc._
import play.api.data.Forms._
import controllers.User.peopleForm
import java.util.Random

object Static extends Controller {

  def home = Action { implicit request =>
    val user_count = models.mUser.count
    val test_count = models.BiletStat.count
    val daily_count = models.microDailyStat.count
    Ok(views.html.static_pages.home("", peopleForm.fill("", ""), user_count, test_count, daily_count))
  }

  def about = Action {
    implicit request => Ok(views.html.static_pages.rules())
  }
  
  def help = Action {
    implicit request => Ok(views.html.static_pages.help())
  }

  def contact = Action {
    implicit request => Ok(views.html.static_pages.contact())
  }

  def zno = Action {
    implicit request => Ok(views.html.static_pages.zno())
  }

  def noscript = Action {
    implicit request => Ok(views.html.static_pages.noscript())
  }
  
  def presentation = Action {
    implicit request => Ok(views.html.static_pages.presentation())
  }

  def generator = Action {
    implicit request => Ok(views.html.static_pages.generator())
  }

  def randProverb = {
    val proverbs = scala.xml.XML.loadFile("public/proverb.xml")
    // Число пословиц
    val result = proverbs \ ("p" + new Random().nextInt(28).toString)
    result.text
  }
}
