package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.libs.json._
import models._
import controllers.Lessons.split
import controllers.User.withUser

object Stats extends Controller {

  def showQuestionStat(question: Question, number: Int, stat: Stat) = {
    var result = ""
    if(question.image!="")
      result = s"""<div id='questionstat' class='well'>${number+1}. ${question.text}<br><img src='${question.image}'/>"""
    else 
      result = s"""<div id='questionstat' class='well'>${number+1}. ${question.text}"""
    val variants = Variant.findByQuestion(question.id)
    if(question.typ == 1 || question.typ == 3 || question.typ == 4){
      if(stat.answer == "none" || stat.answer == ""){
        if(question.typ == 4){
	  result += "<p>Правильні відповіді:<br>"
          question.answer.split("~").foreach(x => result += x+"<br>")
          result += "</p>"
        }
        else
          result += "<p>Правильна відповідь: " + question.answer + "</p>"
	result += "<p>Ви не відповідали на це питання.</p>"
      }
      else{
        if(question.typ == 4){
          result += "<p>Правильні відповіді:<br>"
          val rightma = question.answer.split("~")
          rightma.foreach(x => result += x+"<br>")
          result += "</p><p>Ваші відповіді:<br>"
          val answerma = stat.answer.split("~")
          if(answerma.isEmpty){
            result += "Ви не відповідали на це запитання.</p>"
          }else{
            stat.answer.split("~").foreach{x =>
              if(rightma.contains(x)) result += "<span style='color:green;'>" + x+"</span><br>"
              else
                result += "<span style='color: #A80000;'>" + x+"</span><br>"
            }
            result += "</p>"
          }
        }else
	  result += "<p>Правильна відповідь: " + question.answer + "</p><p>Ваша відповідь: "
        if(question.typ != 4){
	  if(stat.right==1)
	    result+="<span style='color: green;'>"
	  else
	    result+="<span style='color: #A80000;'>"
	  result += stat.answer
	  result += "</span></p>"
        }
	}
    }else if(question.typ==2){
      result = s"""<div id='question' class='well'>${number+1}. ${question.text}
<img src='${question.image}'/>"""
      var varstring = ""
      val lis = split(variants, variants.length/2)
      val right_answer = (question.answer).split("~")
      val user_answer = (stat.answer).split("~")
      val length = user_answer.length
      var color = ""
      for(i <- 0 to (length-1)){
	if(user_answer(i)==right_answer(i))
	  color = "green"
	else
	  color = "#A80000"
	varstring += s"<tr><td align=left style='width: 65%'><font color="+color+ s">${(lis(0))(i).text}</font></td><td align=center style='width: 35%;'>"
	if(user_answer(i)=="none")
	  varstring += s"<font color=${color}>Ви не відповіли.</font><br>(Правильно - ${(lis(1))(i).text})</td></tr><br>"
	else
	  if(user_answer(i)==right_answer(i))
	    varstring += s"<font color=${color}>${user_answer(i)}</font></td></tr><br>"
	  else
	    varstring += s"<font color=${color}>${user_answer(i)}</font><br>(Правильно - ${(lis(1))(i).text})</td></tr><br>"
      }
      result += s"<table style='width: 100%;'>$varstring</table>"
    }
    result += "</div><div id='line'></div>"
    result
  }

  // Отображение страницы статистики
  def dailyStatGet(time: String, typ: Int) = withUser {user => implicit request =>
    val microDaily = microDailyStat.getForCheck(user.id, time, typ)
    if(microDaily.length == 0)
      NotFound(views.html.errors.onHandlerNotFound(request))
    else{
      // Получаем вопросы, на которые отвечал пользователь
      val ids_list: List[String] = (microDaily(0).ids).split("~").toList
      var questions: List[Question] = List()
      var tempQuest: Question = new Question(0, 0, 0, "", "", "")
      var dailyStatList: List[DailyStat] = List()
      for(id <- ids_list){
	tempQuest = Question.find(id.toLong)  
	questions = questions :+ tempQuest
	dailyStatList = dailyStatList :+ DailyStat.find(user.id, id.toLong, time, typ)
      }					     
      Ok(views.html.lessons.dailystat(questions, dailyStatList))
    }
  }

  def dailyStatRedirect(typ: Int) = withUser{ user => implicit request =>
    val forma = request.body.asFormUrlEncoded
    val dateArray = (forma.get("datepicker")(0)).split("/")
    if(dateArray.length != 3){
      Redirect(routes.Lessons.profDaily())
    }else{
      val resultDate = dateArray(1) + "/" + dateArray(0) + "/" + dateArray(2)	   
      Redirect(routes.Stats.dailyStatGet(resultDate, typ))
    }
  }

  // Показ вопросов по статистике ежедневных соревнований
  def showDailyQuestionStat(question: Question, number: Int, stat: DailyStat) = {
    var result = ""
    if(question.image!="")
      result = s"""<div id='questionstat' class='well'>${number+1}. ${question.text}<br><img src='${question.image}'/>"""
    else 
      result = s"""<div id='questionstat' class='well'>${number+1}. ${question.text}"""
    val variants = Variant.findByQuestion(question.id)
     if(question.typ == 1 || question.typ == 3 || question.typ == 4){
      if(stat.answer == "none" || stat.answer == ""){
        if(question.typ == 4){
	  result += "<p>Правильні відповіді:<br>"
          question.answer.split("~").foreach(x => result += x+"<br>")
          result += "</p>"
        }
        else
          result += "<p>Правильна відповідь: " + question.answer + "</p>"
	result += "<p>Ви не відповідали на це питання.</p>"
      }
      else{
        if(question.typ == 4){
          result += "<p>Правильні відповіді:<br>"
          question.answer.split("~").foreach(x => result += x+"<br>")
          result += "</p><p>Ваша відповідь:<br>"
          stat.answer.split("~").foreach(x => result += x+"<br>")
          result += "</p>"
        }else
	  result += "<p>Правильна відповідь: " + question.answer + "</p><p>Ваша відповідь: "
	if(stat.right==1)
	  result+="<span style='color: green;'>"
	else
	  result+="<span style='color: red;'>"
	result += stat.answer
	result += "</span></p>"
	}
    }else if(question.typ==2){
      result = s"""<div id='question' class='well'>${number+1}. ${question.text}
<img src='${question.image}'/>"""
      var varstring = ""
      val lis = split(variants, variants.length/2)
      val right_answer = (question.answer).split("~")
      val user_answer = (stat.answer).split("~")
      val length = user_answer.length
      var color = ""
      for(i <- 0 to (length-1)){
	if(user_answer(i)==right_answer(i))
	  color = "green"
	else
	  color = "red"
        varstring += s"<tr><td align=left style='width: 65%'><font color="+color+ s">${(lis(0))(i).text}</font></td><td align=center style='width: 35%;'>"
	if(user_answer(i)=="none")
	  varstring += s"<font color=${color}>Ви не відповіли.</font><br>(Правильно - ${(lis(1))(i).text})</td></tr><br>"
	else
	  if(user_answer(i)==right_answer(i))
	    varstring += s"<font color=${color}>${user_answer(i)}</font></td></tr><br>"
	  else
	    varstring += s"<font color=${color}>${user_answer(i)}</font><br>(Правильно - ${(lis(1))(i).text})</td></tr><br>"
      }
      result += s"<table style='width: 100%;'>$varstring</table>"
    }
    result += "</div><div id='line'></div>"
    result
   }

}
