$(function() {
    $('#datepicker').datepicker({
	beforeShowDay: highlightOdds
    });
});
var sel = $("#sell").val();
var ajaxMassive = Array();
$("#sell").change(function(){
    sel = $("#sell").val();
    seltext = $("#sell option:selected").text();
    var succ = function(data){
	ajaxMassive = Array();
	massive = data.split("~");
	for(var i=0; i < massive.length; i++){
            ajaxMassive.push(new Date(Date.parse(massive[i])))
	}
    }
    ajax1 = {
	success: succ
    }
    jsRoutes.controllers.Lessons.jsTime(sel).ajax(ajax1);
    $("#bl").text(seltext);
    $(".well").css("display", "block");
    $("#formm").attr("action", ("/challenge/stat/redirect/"+sel));
});
var knopka = $("#knopka");
knopka.click(function(){
    document.location.href = ("/challenge/"+sel);
});
function highlightOdds(date) {    
    var arr = ajaxMassive;
    //var boole = (day.getYear() == date.getYear() && day.getMonth() == date.getMonth() && day.getDate() == date.getDate())
    var ppp = new Date(date.getYear(), date.getMonth(), date.getDate())
    var boole = contains(arr, date);
    return [true, (boole) ? 'odd' : ''];
}
function contains(a, obj) {
    for (var i = 0; i < a.length; i++) {
        if (a[i].getYear() == obj.getYear() && a[i].getMonth() == obj.getMonth() && a[i].getDate() == obj.getDate() ) {
            return true;
        }
    }
    return false;
}
function funn(){
    $("#aa1").attr("href", "/challenge/rate/day/"+sel+"/1");
    $("#aa2").attr("href", "/challenge/rate/month/"+sel+"/1");
    $("#aa3").attr("href", "/challenge/rate/year/"+sel+"/1");
    $("#aa4").attr("href", "/challenge/rate/all/"+sel+"/1");
}
