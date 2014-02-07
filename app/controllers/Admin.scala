package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.format.Formats._
import play.api.Play.current
import controllers.User.withAdmin
import models._

object Admin extends Controller{

  val changeBiletForm = Form(
    tuple(
      "name" -> text,
      "lesson_id" -> of[Long],
      "time" -> text
    )
  )

  def news = Action { implicit request =>
    Ok(views.html.admin.news())
  }

  def main = withAdmin(1) { user => implicit request =>
    Ok(views.html.admin.main(user))
  }

  def biletlist = withAdmin(1) { user => implicit request =>
    val lessons = models.Lesson.findAll
    Ok(views.html.admin.biletlist(user, lessons))
  }

  def changebilet(bilet_id: Long) = withAdmin(1) { user => implicit request =>
    val bilet = Bilet.find(bilet_id)
    Ok(views.html.admin.changebilet(user, bilet, changeBiletForm.fill(bilet.name, bilet.lesson_id, bilet.time)))
  }

  def changeBiletMain(bilet_id: Long) = withAdmin(1){ admin => 
    val bilet = Bilet.find(bilet_id)
    implicit request =>
    changeBiletForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.admin.changebilet(admin, bilet, formWithErrors)),
      bilet_form => {
        Bilet.update(bilet_id, bilet_form._1, bilet_form._2, bilet_form._3)
        Redirect(routes.Admin.changebilet(bilet_id))
      })
  }

  def newbilet = withAdmin(1) { user => implicit request =>
    Ok(views.html.admin.newbilet(user))
  }

  def userlist = withAdmin(2) { user => implicit request =>
    Ok(views.html.admin.userlist(user, List(), 0, "", ""))
  }

  def peopleSearch = withAdmin(2) { user =>
    implicit request =>
      User.peopleForm.bindFromRequest.fold(
        formWithErrors => Redirect(routes.Admin.main).flashing("error" -> "Введіть прізвище або оберіть область!"),
        pattern => {
          // Тут будет отсеиванье ненужных людей
	  var users: List[mUser] = List()
          var pattern1 = "%"
          var pattern2 = "%"
	  if(pattern._1 == "" && pattern._2 == ""){
	    users = mProfile.findPattern("%", "%")
	  }else{
	    val name = "%" + pattern._1 + "%"
	    val city = "%" + pattern._2 + "%"
            pattern1 = name
            pattern2 = city
	    users = mProfile.findPattern(name, city)
	  }
	  Ok(views.html.admin.userlist(user, users, 1, pattern1, pattern2))
        })
  }


  def changeUser(user_id: Long) = withAdmin(2) { user => implicit request =>
    val user_editable = mUser.find(user_id)
    val user_profile = mProfile.find(user_id)
      Ok(views.html.admin.changeuser(user, user_id, User.gravatarFor(user_editable.email), User.editForm.fill(user_editable.email, user_profile.name, "", "", "", user_profile.city, user_profile.school, user_profile.comments), User.lessonForm))
  }

  def changeuser_post(user_id: Long) = withAdmin(2) { admin =>
    implicit request =>
    val user = mUser.find(user_id)
    val profile = mProfile.find(user_id)
      User.editForm.bindFromRequest.fold(
        formWithErrors => BadRequest(views.html.admin.changeuser(admin, user_id, User.gravatarFor(user.email), formWithErrors, User.lessonForm)),
        user_form => {
          if(user_form._4 != "" && user_form._5 != "") {
            if(user_form._4 != user_form._5)
              Redirect(routes.Admin.changeUser(user_id)).flashing("error" -> "Ви неправильно ввели підтвердження пароля")
            else {
	      val hashedPass = mUser.hashPass(user_form._4)
              mUser.edit(user.id, user_form._1, hashedPass)
	      val cached = cache.Cache.getOrElse("user_cache" + user.id){
		mUser.find(user.id)
	      }
	      cache.Cache.set("user_cache" + user.id, cached.copy(pass = hashedPass), 60*120)
              Redirect(routes.User.profile(user.id)).flashing("success" -> "Пароль був успішно змінений").withSession(Security.username -> user.id.toString, "id" -> (user.id).toString)
            }
          }else{
	    if(user_form._4 != user_form._5 || user_form._3 != "")
	      Redirect(routes.Admin.changeUser(user_id)).flashing("error" -> "Ви не повністю заповнили форму зміни пароля")
	    else{
              mUser.edit(user.id, user_form._1, user.pass)
              mProfile.edit(user.id, user_form._2, user_form._6, user_form._7 , user_form._8)
	      val cached = cache.Cache.getOrElse("user_cache" + user.id){
		mUser.find(user.id)
	      }
              val cached_profile = cache.Cache.getOrElse("profile_cache" + user.id){
                mProfile.find(user.id)
              }
	      cache.Cache.set("user_cache" + user.id, cached.copy(email = user_form._1), 60*120)
              cache.Cache.set("profile_cache" + user.id, cached_profile.copy(name = user_form._2, city = user_form._6, school = user_form._7, comments = user_form._8))
              Redirect(routes.User.profile(user.id)).flashing("success" -> "Профіль був успішно змінений")
	    }
          }
        })
  }

  def changeLesson(user_id: Long) = withAdmin(2) { admin =>
    implicit request =>
    val user = mUser.find(user_id)
    val profile = mProfile.find(user_id)
      User.lessonForm.bindFromRequest.fold(
        formWithErrors => BadRequest(views.html.admin.changeuser(admin, user_id, User.gravatarFor(user.email), User.editForm.fill(user.email, profile.name, "", "", "", profile.city, profile.school, profile.comments), User.lessonForm)),
        lessons => {
          val newlist = lessons.productIterator.toList
          val listComp = List(1, 2, 3, 4, 5, 6, 7, 8, 9, "a", "b", "c")
          var string: String = ""
          for(index <- 0 to 11)
            if(newlist(index)==true) string+=listComp(index)
          mProfile.editLessons(profile.user_id, string)
          val cached_profile = cache.Cache.getOrElse("profile_cache" + user.id){
            mProfile.find(user.id)
          }
	  cache.Cache.remove("profile_cache" + user.id)
	  cache.Cache.set("profile_cache" + profile.user_id, cached_profile.copy(lessons = string), 60*120)
          Redirect(routes.User.profile(user.id)).flashing(
            "success" -> "Інформація про предмети була успішно змінена."
          )
	}
      )
    }


  def deleteUser(user_id: Long) = withAdmin(2) { user => implicit request =>
    mUser.delete(user_id)
    Redirect(routes.Admin.userlist).flashing("success" -> "Користувач був успішно видалений")
  }

  def io = withAdmin(2) { user => implicit request =>
    Ok(views.html.admin.io(user))
  }

  // СОЗДАНИЕ БИЛЕТА

  // КОНЕЦ СОЗДАНИЯ БИЛЕТА
}
