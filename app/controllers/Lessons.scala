package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.libs.json._
import models._
import controllers.User.{withUser, withProfile}

object Lessons extends Controller{
  
  def allLessons = Action {implicit request =>
    Ok(views.html.lessons.all(Lesson.findAll))
  }
  
  def lesson(id: Long) = Action {implicit request =>
    val lesson = Lesson.find(id)
    Ok(views.html.lessons.lesson(lesson))
  }

  def materials(id: Long) = Action {implicit request =>
    Ok(views.html.lessons.materials(id))
  }

  def prep(id: Long) = withUser {user => implicit request =>
    val lesson = Lesson.find(id)
    val bilets: List[Bilet] = Bilet.inLesson(lesson.id)
    var counter: Int = 0
    Ok(views.html.lessons.prep(user, lesson, bilets, counter))
  }

  def lessonStat(user_id: Long, lesson_id: Long):Int = {
    val bilets: List[Bilet] = Bilet.inLesson(lesson_id)
    var bilLength = 0
    if(bilets.length == 0) bilLength = 1 else bilLength = bilets.length 
    var counter: Int = 0
    for(bilet <- bilets)
      if(BiletStat.solved(user_id, bilet.id)) counter += 1
    val result = (counter*100)/bilLength
    // bilLength - 100 %
    // counter - x %
    result
  }

  def bilet(bilet_id: Long) = withUser{ user => implicit request =>
    // На случай, если глобал не будет работать
    /*if(Bilet.exists(bilet_id))
      Ok(views.html.lessons.bilet(Bilet.find(bilet_id)))
    else
      NotFound(views.html.errors.onHandlerNotFound(request))*/
    // val bilet = Bilet.find(bilet_id)
    val questions = Question.findByBilet(bilet_id)
    val time = (Bilet.find(bilet_id)).time
    if(questions.isEmpty)
      NotFound(views.html.errors.onHandlerNotFound(request))
    else {
      val shuffled = scala.util.Random.shuffle(questions)
      Ok(views.html.lessons.bilet(shuffled, bilet_id, time))
    }
  }

  def biletStat(bilet_id: Long) = withUser{ user => implicit request =>
    val questions = Question.findByBilet(bilet_id)
    val stat = Stat.find(user.id, bilet_id)
    if(questions.isEmpty || stat.isEmpty)
      NotFound(views.html.errors.onHandlerNotFound(request))
    else{
      Ok(views.html.lessons.biletstat(questions, bilet_id, stat))
    }
  }

  def showQuestion(question: Question, number: Int) = {
    var result = s"""<div id='question'>${number+1}. ${question.text}<br><img src='${question.image}'/>"""
    val variants = Variant.findByQuestion(question.id)
    if(question.typ==1){
      var varstring = ""
      val shuffled = scala.util.Random.shuffle(variants)
      for(variant <- shuffled) varstring += s"<input type='radio' name='f${question.id}' value='${variant.text}'>${variant.text}</input><br>"
      result += s"""
      <br>$varstring
      """
    }else if(question.typ==2){
      result = s"""<div id='question'>${number+1}. ${question.text}
<img src='${question.image}'/>"""
      var varstring = ""
      val lis = split(variants, variants.length/2)
      val shuffled = scala.util.Random.shuffle(lis(1))
      for(variant <- lis(0)){
	varstring += s"<tr><td align=left style='width: 59%;'>${variant.text}</td><td align=center style='width: 41%;'><select name='${variant.id}s${question.id}'><option disabled selected>Оберіть відповідь</option>"
	for(option <- shuffled)
	  varstring += "<option>" + option.text + "</option>"
	varstring += "</select></td><br>"
      }
      result += s"<table>$varstring</table>"
    }else if(question.typ==3){
      result += s"""
      <br><input type='text' name='t${question.id}'/>
      """
    }else if(question.typ==4){
      var varstring = ""
      val shuffled = scala.util.Random.shuffle(variants)
      for(variant <- shuffled) varstring += s"<input type='checkbox' name='4f${question.id}' value='${variant.text}'>${variant.text}</input><br>"
      result += s"""
      <br>$varstring
      """
    }
    result += "</div><div id='line'></div>"
    result
  }
  
  def parseQuestion = withUser {user => request =>
    var ra = 0
    var max = 0
    val forma = request.body.asFormUrlEncoded
    val bilet = Bilet.find((forma.get("bilet_id")(0)).toLong)
    Stat.deleteResolve(user.id, bilet.id)
    val questions = Question.findByBilet(bilet.id)
    for(question <- questions){
      var answer = ""
      var right = 0
      if(question.typ==1){
	try{
	  answer = forma.get("f"+question.id)(0)
	  if(question.answer == answer) {ra+=1; right=1}
	}catch {
	  case _: Throwable => answer = "none"
	}
	max += 1
      }else if(question.typ==2){
	val variants = Variant.findByQuestion(question.id)
	val lis = split(variants, variants.length/2)
	answer = ""
	for(variant <- lis(0)){
	  try{
	    answer += (forma.get(variant.id + "s" + question.id)(0)) + "~"
	  }catch {
	    case _: Throwable => answer += "none~"
	  }
	}
	val right_answer = (question.answer).split("~")
	val user_answer = answer.split("~")
	val length = right_answer.length
	for(i <- 0 to (length-1))
	  if(right_answer(i)==user_answer(i)) ra += 1
	max += length
	if(ra == max) right = 1
      }else if(question.typ==3){
	answer = forma.get("t"+question.id)(0)
	if(answer == question.answer) {ra += 2; right = 1}
	max += 2
      }else if(question.typ==4) {
	try{
	  val useranswer = forma.get("4f"+question.id)
          val ansmassive = question.answer.split("~")
          val anslength = ansmassive.length
          max += anslength
          if(useranswer.isEmpty){
            right = 0
            answer = "none"
          }else{
            var ress = 0
            useranswer.foreach{x =>
              if(ansmassive.contains(x))
                ress += 1
              else
                ress -= 1
              answer += x + "~"
            }
            if(ress > 0){
              ra += ress
              right = 1
            }
          }
	}catch {
	  case _: Throwable => answer = "none"
	}
      }
      Stat.newStat(user.id, bilet.id, question.id, right, answer)
    }
    BiletStat.newBiletStat(user.id, bilet.id, ra, max)
    val perc: Double = if(max == 0) 0
    else{
      val rre = "%1.2f" format (ra.toDouble*100)/max.toDouble
      if(rre.toString.contains(",")){
        val tempMass = rre.split(",")
        (tempMass(0) + "." + tempMass(1)).toDouble
      }else{
        rre.toDouble
      }
    }
    if(perc>70)
      Redirect(routes.Lessons.biletStat(bilet.id)).flashing(
	"success" -> s"Зараховано! Ви набрали ${perc}%." 
      )
    else
      Redirect(routes.Lessons.biletStat(bilet.id)).flashing(
	"error" -> s"Не зараховано :(. Ви набрали ${perc}%."
      )
  }

  // Ежедневное соревнование
  def daily(typ: Int) = withProfile { (user, profile) => implicit request =>
    // Шаг первый: проверяем, не проходил ли принимал ли участие пользователь сегодня в ежедневке
    val current_day = currentDate
    //Текущий день уже есть. Найдем предметы юзера
    val lessons = User.codeToLessonsList(profile.lessons)
    val microStat = microDailyStat.getForCheck(user.id, current_day, typ)
    if(microStat.length != 0){
      Redirect(routes.Lessons.profDaily).flashing("error" -> "Ви вже брали участь у змаганнях цього типу сьогодні.")
    }else{
      //Предметы есть. Теперь создадим список id Доступных предметов
      import scala.util.Random
      val rand = new Random()
      if(typ == 0) {
        if(lessons.length < 3){
          Redirect(routes.User.edit).flashing(
	    "error" -> "Для участі в щоденних змаганнях оберіть щонайменше 3 предмети."
          )
        }else {
	  // Начинаем по крупицам собирать нужные 18 вопросов
          var questionsAllowed: List[Question] = List()
          for(lesson <- lessons){
            for(bilet <- Bilet.inLesson(lesson.id)){
	      questionsAllowed = questionsAllowed ::: Question.findByBilet(bilet.id)
            }
          }
          if(questionsAllowed.length < 18)
            Redirect(routes.Lessons.profDaily()).flashing("error" -> "Обрані у вашому профілі предмети містять недостатньо питань!")
          else{
	    var randQuestions: List[Question] = List()
	    for(i <- 1 to 18){
	      var que = questionsAllowed(rand.nextInt(questionsAllowed.length))
	      if(randQuestions.contains(que)){
	        while(randQuestions.contains(que)){
		  que = questionsAllowed(rand.nextInt(questionsAllowed.length))
	        }
	      }
	      randQuestions = rand.shuffle(randQuestions :+ que)
	    }
	    var bilet_ids_last = ""
	    randQuestions.foreach(x => bilet_ids_last+=(x.id+"~"))
	    microDailyStat.create(user.id, current_day, 0, bilet_ids_last, typ)
	    Ok(views.html.lessons.daily(randQuestions, "00:12:00", typ))
          }
	}
      }else{
        var questionsAllowed: List[Question] = List()
        for(bilet <- Bilet.inLesson(typ)){
	  questionsAllowed = questionsAllowed ::: Question.findByBilet(bilet.id)
        }
        if(questionsAllowed.length < 12){
          Redirect(routes.Lessons.profDaily()).flashing("error" -> "Обраний вами предмеет містить менше 18 питань!")
        }else{
	  var randQuestions: List[Question] = List()
	  for(i <- 1 to 18){
	    var que = questionsAllowed(rand.nextInt(questionsAllowed.length))
	    if(randQuestions.contains(que)){
	      while(randQuestions.contains(que)){
	        que = questionsAllowed(rand.nextInt(questionsAllowed.length))
	      }
	    }
	    randQuestions = rand.shuffle(randQuestions :+ que)
	  }
	  var bilet_ids_last = ""
	  randQuestions.foreach(x => bilet_ids_last+=(x.id+"~"))
	  microDailyStat.create(user.id, current_day, 0, bilet_ids_last, typ)
	  Ok(views.html.lessons.daily(randQuestions, "00:12:00", typ))
        }
      }
    }
  }

   // Вспомогательная функция, опредетяет, записано ли в масиве уже значение

  def dailyEngine(typ: Int) = withUser { user => request =>
    // Находим текущую дату
    var ra: Float = 0
    var max: Float = 0			     
    val current_date = currentDate
    val microDaily = microDailyStat.getForCheck(user.id, current_date, typ)
    // Получаем вопросы, на которые отвечал пользователь
    val ids_list: List[String] = (microDaily(0).ids).split("~").toList
    var questions: List[Question] = List()
    for(id <- ids_list)
      questions = questions :+ Question.find(id.toLong)
//    ids_list.foreach(id => questions :+ Question.find(id.toLong))
    val forma = request.body.asFormUrlEncoded
    for(question <- questions){
      var answer = ""
      var right = 0
      if(question.typ==1){
	try{
	  answer = forma.get("f"+question.id)(0)
	  if(question.answer == answer) {ra+=1; right=1}
	}catch {
	  case _: Throwable => answer = "none"
	}
	max += 1
      }else if(question.typ==2){
	val variants = Variant.findByQuestion(question.id)
	val lis = split(variants, variants.length/2)
	answer = ""
	for(variant <- lis(0)){
	  try{
	    answer += (forma.get(variant.id + "s" + question.id)(0)) + "~"
	  }catch {
	    case _: Throwable => answer += "none~"
	  }
	}
	val right_answer = (question.answer).split("~")
	val user_answer = answer.split("~")
	val length = right_answer.length
	for(i <- 0 to (length-1))
	  if(right_answer(i)==user_answer(i)) ra += 1
	max += length
	if(ra == max) right = 1
      }else if(question.typ==3){
	answer = forma.get("t"+question.id)(0)
	if(answer == question.answer) {ra += 2; right = 1}
	max += 2
      }else if(question.typ==4){
	try{
	  val useranswer = forma.get("4f"+question.id)
          val ansmassive = question.answer.split("~")
          val anslength = ansmassive.length
          max += anslength
          var ress = 0
          useranswer.foreach{x => 
           if(ansmassive.contains(x))
             ress += 1
           else
             ress -= 1
            answer += x + "~"
          }
          if(ress > 0){
            ra += ress
            right = 1
          }
	}catch {
	  case _: Throwable => answer = "none"
	}
      }
      DailyStat.newDailyStat(user.id, question.id, current_date, right, answer, typ)
    }
    val startTime = request.cookies.get("tiz").getOrElse(new play.api.mvc.Cookie("tiz", "0")).value
    val endTime = request.cookies.get("zit").getOrElse(new play.api.mvc.Cookie("vit", "0")).value
    val resultTime = endTime.toLong - startTime.toLong
    // Время есть, теперь находим процент
//  val perc = ("%1.2f".format((ra * 100)/max)).toDouble
    val perc: Double = if(max == 0) 0
    else{
      val rre = "%1.2f" format (ra*100)/max
      if(rre.toString.contains(",")){
        val tempMass = rre.split(",")
        (tempMass(0) + "." + tempMass(1)).toDouble
      }else{
        rre.toDouble
      }
    }
    // Тут будет смена ранга
    if(perc == 100.0 && user.rank <9){
      val maxResults = microDailyStat.getMaxResults(user.id)
      var rank = 1
      if(maxResults.length == 2){
	rank = 2
      }else if(maxResults.length == 4){
	rank=3
      }else if(maxResults.length == 7){
	rank=4
      }else if(maxResults.length == 12){
	rank=5
      }else if(maxResults.length == 20){
	rank=6
      }else if(maxResults.length == 33){
	rank=7
      }else if(maxResults.length == 54){
	rank=8
      }else if(maxResults.length == 88){
	rank=9
      }
      mUser.updateRank(user.id, rank)
    }
    if(resultTime < 0 || resultTime > 720000 || startTime == "" || endTime == ""){
      microDailyStat.update(user.id, 99999999, current_date, perc, typ)
    }else{
      microDailyStat.update(user.id, resultTime, current_date, perc, typ)
    }
    Redirect(routes.Stats.dailyStatGet(current_date, typ)).flashing("success" -> s"Ваш результат: ${perc} балів зі 100")
  }

  def dailyRates(typ: String, page: Long, typpy: Int) = withUser{ user => implicit request =>
    if(typ != "day" && typ != "month" && typ != "year" && typ != "all"){
      Redirect(routes.Lessons.profDaily()).flashing("error" -> "Рейтинг за цей проміжок часу не існує.")
    }else{
      var pattern: String = currentDate
      if(typ == "month"){
	pattern = pattern.drop(2)
      }else if(typ == "year"){
	pattern = pattern.drop(4)
      }else if(typ == "all"){
	pattern = ""
      }
      var pag: Long = (page-1)*15
      val statList = microDailyStat.rate(("%" + pattern), pag, typpy)
      if(statList.length==0 || page < 1)
        Redirect(routes.Lessons.profDaily()).flashing("error" -> "Ми все перерили, але так і не змогли знайти рейтинг за цей проміжок часу.")
      else
	Ok(views.html.lessons.rates(statList, typ, page, typpy))
    }
  }

  def javascriptRoutes = Action { implicit request =>
    import routes.javascript._
    Ok(
      Routes.javascriptRouter("jsRoutes")(
        routes.javascript.Lessons.jsTime
      )
    ).as("text/javascript")
  }
  
  def jsTime(typ: Int) = withUser { user => implicit request =>
    val statList: List[microDailyStat] = microDailyStat.getByUser(user.id, typ)
    var stringList: List[String] = List()
    var hel = ""
    for(stat <- statList){
      var hel = (stat.time).split("/")
      stringList = stringList :+ (hel(1)+"/"+hel(0)+"/"+hel(2))
    }
    var resultString = ""
    for(st <- stringList)
      resultString += (st+"~")
    Ok(resultString)
  }

  def profDaily = withUser {user => implicit request => 
    Ok(views.html.lessons.profdaily())
  }

  // Подгрузка билетов 
  //------------------------------------------------------------
  def getLoad = withUser { user => implicit request =>
    if(user.id != 1) Redirect(routes.Static.home)
    else Ok(views.html.lessons.load("Upload"))
  }

  def postLoad = Action(parse.multipartFormData) { request =>
    request.body.file("xml").map { xm =>
      import java.io.File
      val time = System.currentTimeMillis
      xm.ref.moveTo(new File("/tmp/xml/" + time + ".xml"))
      val xmll = scala.xml.XML.loadFile("/tmp/xml/"+ time + ".xml")
      parseBilet(xmll)
      Redirect(routes.Lessons.getLoad).flashing(
	"success" -> "File was uploaded"
      )
    }.getOrElse{
      Redirect(routes.Lessons.getLoad).flashing(
	"error" -> "Missing file"
      )
    }
  }
  
  // Функция парсит xml документ и создает билет
  def parseBilet(data: scala.xml.Elem){
    // Шаг первый. Получаем id предмета
    val lesson_id = ((data \ "@lesson_id").text).toInt
    val time = ((data \ "@time").text)
    val name = ((data \ "@name").text)
    // Создаем билет
    val lastBiletid = Bilet.create(lesson_id, time, name)
    // Получаем последнее айди вопроса (для оптимизации)
    var thisQuestId = Question.getLast.id + 1
    // Шаг третий. Проходимся по всем вопросам
    for (question <- (data \\ "question")){
      // Получаем тип вопроса
      val typ = ((question \ "@type").text).toInt
      // Не зависимо от вопроса получаем картинку и текст вопроса
      var text = (question \\ "text").text
      var textArr = text.split("~")
      // Если нам требуется скрыть текст, то ставим символ ~ перед текстом
      if(textArr.length == 2){
      text = textArr(0) + "<br><a href=\"javascript:look('"+thisQuestId+"ol1')\">Натисніть для показу тексту</a><div id='"+thisQuestId+"ol1' style='display: none;'>"+textArr(1)+"</div>"
      }else{
	text = textArr(0)
      }
      val image = (question \\ "image").text
      var right = ""
      // В зависимости от типа по разному обрабатываем вопросы
      if(typ == 3){
	right = (question \\ "right").text
      }else{
	// Получаем правильный ответ
	if(typ == 1)
	  right = (question \\ "variant")(((question \ "@right").text).toInt).text
	else if(typ == 2 || typ == 4){
	  val massive = for(variant <- (question \\ "variant"); if((variant \ "@answer").text == "true")) yield variant.text
	  for(mas <- massive) right+= mas+"~"
	}
	// Получаем варианты
	val variants = for(variant <- (question \\ "variant")) yield variant.text
	for(variant <- variants)
	  Variant.create(thisQuestId, variant)
      }
      Question.create(lastBiletid, typ, text, image, right)
      thisQuestId += 1;
    }
  }
  

  def split[A](xs: List[A], n: Int): List[List[A]] = {
    if (xs.size <= n) xs :: Nil
    else (xs take n) :: split(xs drop n, n)
  }

  def currentDate = {
    import java.util.Calendar
    import java.text.SimpleDateFormat
    val today = Calendar.getInstance().getTime()
    val formatter = new SimpleDateFormat("dd/MM/YYYY")
    val current_day = formatter.format(today)
    current_day
  }
  
  def normalizeTime(time: Long) = (time.toDouble / 1000)
}
