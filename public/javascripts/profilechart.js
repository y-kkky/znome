$(function () {
    $('#charts').highcharts({
	title: {
            text: 'Участь в щоденних змаганнях',
            x: -20 //center
	},
	xAxis: {
            categories: []
	    //@if(stats.length<5){@for(stat <- stats){'@stat.time',}}else{@for(stat <- stats){@if(stat.score>50){'+',}else{'-',}}}
	},
	yAxis: {
            min: 0,
            max: 100,
            title: {
		text: 'Кількість балів'
            },
            plotLines: [{
		value: 0,
		width: 1,
		color: '#808080'
            }]
	},
	tooltip: {
            valueSuffix: ' балів'
	},
	series: [{
	    name: "1",
            data: []		   
	}]
	    //@for(stat <- stats){@stat.score,}
    });
});
