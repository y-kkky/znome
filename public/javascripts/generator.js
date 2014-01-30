var MYAPP = {};
MYAPP.counter = 1;
function addquest(){
    // get type
    var counter = MYAPP.counter;
    var sel = document.getElementById("selector");
    var type = sel.options[sel.selectedIndex].value;
    var newdiv = document.createElement('div');
    var divClassName = type + "question";
    newdiv.setAttribute('class', divClassName);
    var html =  "<div class='question' id='"+ counter  +"'>"+counter+". <input type='text' class='text' placeholder='Текст питання' autofocus='autofocus' style='width: 500px;' id='text"+counter+"'><br><input type='text' placeholder='Посилання на малюнок' id='image"+counter+"'><span id='strange"+counter+"' style='display: none;'>"+type+"</span>";
    if(type == 1||type == 4){
	var variants = $("#numvar").val();
	html += "<br>";
        html += "<input type='text' placeholder='номер правильної відповіді' class='"+variants+"' id='right"+counter+"'><br>";
	for(var i=0; i< variants; i++){
	    html += "<input type='text' placeholder='варіант' id='"+ counter + "first"+ i + "'><br>";
	}
	html += "<hr>";
    }else if(type == 2){
	var variants = $("#numvar").val();
	html += "<br>"
	html += "<div id='secret"+counter+"' class='"+variants+"' style='display: none;'></div>"
	for(var i=0; i < variants; i++){
	    html += "<input type='text' placeholder='варіант' id='"+counter+"left"+i+"'> <input type='text' placeholder='варіант' id='"+counter+"right" + i + "'></br>";
	}		    
	html += "<hr>";
    }else if(type == 3){
	html += "<br><input type='text' placeholder='відповідь' id='"+counter+"third'<br><hr>";
    }
    if(type != 1 && type != 2 && type != 3 && type != 4){
	
    }else{
        html += "</div>";
	if(type == 1 || type == 2 || type == 4){
	    if(variants != 0){
		newdiv.innerHTML = html;
		document.getElementById("containero").appendChild(newdiv);  
		MYAPP.counter += 1;
	    }
	}else{
	    newdiv.innerHTML = html;
	    document.getElementById("containero").appendChild(newdiv);
	    MYAPP.counter += 1;
	}
    }
    }
function onChangeChecker(){
    var select = document.getElementById("selector");
    if(select.options[select.selectedIndex].value == 3){
	document.getElementById("numvar").value = "";		     
	$("#numvar").css("display", "none");
    }else{
	$("#numvar").css("display", "inline");
    }
}

function formxml(){
    var counter = MYAPP.counter;
    if(counter != 1){
	var xml = "<bilet>";
	for(var i = 1; i < counter; i++){
	    var text = $("#text"+i).val();
	    var image = $("#image"+i);
            var type = $("#strange"+i).text();
            xml += "<question type='"+type+"'";
            if(type == 1){
		var right = $("#right"+i).val();
		xml += " right='"+(right-1)+"'>";
	    }else
		xml += ">";
            xml += "<text>"+text+"</text>";
	    xml += "<image>"+image.val()+"</image>";
            if(type == 1|| type == 4){
		var numvars = $("#right"+i).attr("class");
		for(var c = 0; c < numvars; c++){
		    if(type == 1){
			var variant = $("#"+i + "first" + c).val();
			xml += "<variant>" + variant + "</variant>";
		    }else if(type == 4){
			var numvar = $("#right"+i).val();
			var variant = $("#"+i + "first" + c).val();
			if(c < numvar)
			    xml += "<variant answer='true'>" + variant + "</variant>";			
			    else
				xml += "<variant>" + variant + "</variant>";							
		    }
		}
	    }else if(type == 2){
		var numvars = $("#secret"+i).attr("class");
		for(var x = 0; x < numvars; x++){
		    var leftvariant = $("#" + i + "left" + x).val();
		    xml += "<variant>"+leftvariant.trim()+"</variant>";
		}
		for(var y = 0; y < numvars; y++){
		    var rightvariant = $("#" + i + "right" + y).val();	
		    xml += "<variant answer='true'>"+rightvariant.trim()+"</variant>";
		}
	    }else if(type == 3){
		var answer = $("#" + i + "third").val();
		xml += "<right>" + answer + "</right>";
	    }
	    xml += "</question>";
	}
	xml += "</bilet>";
	$("#xmlfield").text(xml);
    }
}



