package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import controllers.User.withAdmin
import models._

object Admin extends Controller{

  def news = Action { implicit request =>
    Ok(views.html.admin.news())
  }

  def main = withAdmin(1) { user => implicit request =>
    Ok(views.html.admin.main(user))
  }

  def biletlist = withAdmin(1) { user => implicit request =>
    Ok(views.html.admin.biletlist(user))
  }

  def changebilet(bilet_id: Long) = withAdmin(1) { user => implicit request =>
    Ok(views.html.admin.changebilet(user))
  }

  def newbilet = withAdmin(1) { user => implicit request =>
    Ok(views.html.admin.newbilet(user))
  }

  def userlist = withAdmin(2) { user => implicit request =>
    Ok(views.html.admin.userlist(user))
  }

  def changeuser(user_id: Long) = withAdmin(2) { user => implicit request =>
    Ok(views.html.admin.changeuser(user))
  }

  def io = withAdmin(2) { user => implicit request =>
    Ok(views.html.admin.io(user))
  }

  
}
