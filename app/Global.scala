import play.api._
import play.api.mvc._
import play.api.mvc.Results._
import scala.concurrent.Future
import scala.slick.driver.PostgresDriver.simple._
import play.filters.gzip.GzipFilter
//import Database.dynamicSession
//import models.Helper.dbs

object Global extends WithFilters(new GzipFilter()) with GlobalSettings {

  /*
  override def onStart(app: Application){
  import models._
  
  dbs withDynSession{
  (mUser.users.ddl ++ Lesson.lessons.ddl ++ Bilet.bilets.ddl ++ Question.questions.ddl ++ Variant.variants.ddl 
      ++ Stat.stats.ddl ++ BiletStat.biletstats.ddl ++ DailyStat.dailystats.ddl ++ microDailyStat.microdailystats.ddl).create
  }
  }
  
  */
  /*
  // called when a route is found, but it was not possible to bind the request parameters
  override def onBadRequest(request: RequestHeader, error: String) = {
    Future.successful(BadRequest("Сталася жахлива помилка"))
  }

  // 500 - internal server error
  override def onError(request: RequestHeader, ex: Throwable) = {
    Future.successful(InternalServerError(
      views.html.errors.onError(request, ex)
    ))
  }

  // 404 - page not found error
  override def onHandlerNotFound(request: RequestHeader)= {
    Future.successful(NotFound(views.html.errors.onHandlerNotFound(request)))
  }
	*/
}
