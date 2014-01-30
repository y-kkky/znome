$( ".selectpicker" ).change(function() {
    $("table").css("display", "none");
    $("body").css({"padding-top":"0px"});
    $("#changer").css("display", "block");
    $("#"+$(this).val()).css("display", "table");
});
