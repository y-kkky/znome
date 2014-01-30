package controllers

import play.api._
import play.api.mvc._
import play.api.data.Forms._

object Admin extends Controller{
  
  def news = Action { implicit request =>
    Ok(views.html.admin.news())
  }
  
}