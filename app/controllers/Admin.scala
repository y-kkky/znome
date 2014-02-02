package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import controllers.User.withUser
import models._

object Admin extends Controller{

  def news = Action { implicit request =>
    Ok(views.html.admin.news())
  }

  def main = withUser { user => implicit request =>
    Ok(views.html.admin.main(user))
  }

  
}
