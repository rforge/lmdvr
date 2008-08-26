

/*
   Compatibility notes:

   (1) The following works in Mozilla/Konqueror, but not in IE:

       e.style.setProperty("display", "inline", "");

       So, need to use constructs like e.style.display = "inline";

   (2) Naming convention: properties with hyphens should be replaced
       by corresponding camel-case (?) names, e.g.:

       font-size -> fontSize

*/


/*
Important fact: scope
=====================

  A variable that is declared outside any functions is GLOBAL. This
  means it can be referenced anywhere in the current document.

    * Declared outside any functions, with or without the var keyword.

    * Declared inside a function without using the var keyword, but
      only once the function is called.

  A variable that is declared inside a function USING THE var KEYWORD
  is LOCAL.

*/

var global_theme;
var cur_chap, cur_fig, cur_code;
var loc_base;

/* For now, we'll serve images from the Hutch, as we don't want to put
   a bunch into SVN unless we're pretty sure that these are the final
   versions (which they probably are not, as we want to get better
   anti-aliasing in place)  */
var image_src;

function setLocBase() {
    var pos;
    loc_base = location.href;
    pos = srcString.indexOf("?");
    if (pos != -1) {
	loc_base = loc_base.substring(0, pos);
    }
}

function getValueFromString(key, srcString) {
    /* alert(srcString); */
    var pos, start, end, value;
    var len = key.length;
    pos = srcString.indexOf(key);
    if (pos != -1) {
	start = pos + len;
	end = srcString.indexOf(";", start);
	if (end == -1) end = srcString.length;
	value = srcString.substring(start, end);
	return value;
    }
    else return "NA";
}

function getCookieValue(key) {
    return getValueFromString(key, document.cookie);
}
function getLocationValue(key) {
    return getValueFromString(key, location.href);
}
function getKeyValue(key) {
    // first try location.  If that's empty, try cookies
    var ans = getLocationValue(key);
    if (ans == "NA") ans = getCookieValue(key);
    if (ans == "NA") ans = "";
    return ans;
}
function setPermaLink() {
    document.getElementById("PERMALINK").href = "figures.html?" + // loc_base + 
	"chapter=" + cur_chap + ";figure=" + cur_fig + ";theme=" + global_theme + ";code=" + cur_code;
}

function initialize(theme) {

    image_src = "http://dsarkar.fhcrc.org/lattice/book/images/";
    // for relative links: image_src = "images/" 

    var cookieValue;
    global_theme = "";
    cur_chap = "";
    cur_fig = "";
    cur_code = "";
    // read cookies, if any, and act accordingly
    cookieValue = getKeyValue("chapter=");
    if (cookieValue != "") chapterClicked(cookieValue, 0);
    cookieValue = getKeyValue("figure=");
    if (cookieValue != "") figureClicked(cookieValue);
    cookieValue = getKeyValue("code=");
    if (cookieValue != "") setLayout(cookieValue);
    else setLayout("right");
    cookieValue = getKeyValue("theme=");
    if (cookieValue != "") setTheme(cookieValue);
    else setTheme(theme);
}

function setCookie(key, value) {
    setPermaLink();
    document.cookie = key + value + "; max-age=" + (60 * 60 * 24 * 366);
}

function resetCookies() {
    document.cookie = "chapter=";
    document.cookie = "figure=";
    document.cookie = "theme=";
    document.cookie = "code=";
}


function setTheme(theme) {
    if (global_theme == theme) return;
    global_theme = theme;
    if (cur_fig != "") {
	document.getElementById("IMAGE").src = image_src + "loading.png";
	document.getElementById("IMAGE").src = image_src + "Figure_" +
	    cur_fig + "_" + global_theme + ".png";
    }
    if (0) // (theme == "stdClassic")
	document.body.style.background = "#909090";
    else 
	document.body.style.background = "#fffdfb";
    document.getElementById("stdBW").className = "topic";
    document.getElementById("stdColor").className = "topic";
    document.getElementById("stdClassic").className = "topic";
    document.getElementById(theme).className = "htopic";
    setCookie("theme=", theme);
}


function showErrata() {
    if (cur_chap != "") {
	document.getElementById("FIGURELIST." + cur_chap).className = "FigureListHidden";
	document.getElementById("CHAPTER." + cur_chap).className = "topic";
	document.getElementById("SUMMARY." + cur_chap).className = "summaryHidden";
	document.getElementById("CODE." + cur_chap).className = "codeListingHidden";
    }
    if (cur_fig != "") {
	document.getElementById("FIGURE." + cur_fig).className = "topic";
	document.getElementById("IMAGE.CONTAINER").className = "imageHidden";
    }
/*     global_theme = ""; */
/*     cur_chap = ""; */
/*     cur_fig = ""; */
/*     cur_code = ""; */
    resetCookies();
    setPermaLink();
    document.getElementById("CHAPTER.ERRATA").className = "htopic";
    document.getElementById("SWITCH.LAYOUT").style.display = "none";
    document.getElementById("SUMMARY.00").className = "summaryHidden";
    document.getElementById("SUMMARY.ERRATA").className = "summary";
    document.getElementById("SUMMARY.CONTAINER").className = "summaryContainer";
}

function hideErrata() {
    document.getElementById("CHAPTER.ERRATA").className = "topic";
    document.getElementById("SUMMARY.ERRATA").className = "summaryHidden";
}


function chapterClicked(chap, resetFig) {
    var showFigList = 1;
    if (resetFig) {
	setCookie("figure=", "");
	/* document.getElementById("SWITCH.LAYOUT").style.display = "none"; */
    }
    hideErrata();
    /* if (chap == cur_chap) return null; */
    if (cur_chap != "") {
	if (cur_chap == chap && cur_fig == "" && document.getElementById("FIGURELIST." + cur_chap).className == "FigureList") {
	    showFigList = 0;
	}
	document.getElementById("FIGURELIST." + cur_chap).className = "FigureListHidden";
	document.getElementById("CHAPTER." + cur_chap).className = "topic";
	document.getElementById("SUMMARY." + cur_chap).className = "summaryHidden";
	document.getElementById("CODE." + cur_chap).className = "codeListingHidden";
    }
    if (cur_fig != "") {
	document.getElementById("FIGURE." + cur_fig).className = "topic";
	cur_fig = "";
    }
    cur_chap = chap;
    document.getElementById("IMAGE.CONTAINER").className = "imageHidden";
    document.getElementById("SUMMARY.00").className = "summaryHidden";
    document.getElementById("SUMMARY." + cur_chap).className = "summary";
    document.getElementById("SUMMARY.CONTAINER").className = "summaryContainer";
    document.getElementById("CODE." + cur_chap).className = "codeListing";
    document.getElementById("SWITCH.LAYOUT").style.display = "inline";
    if (showFigList) document.getElementById("FIGURELIST." + cur_chap).className = "FigureList";
    document.getElementById("CHAPTER." + cur_chap).className = "htopic";
    setCookie("chapter=", chap);
}



function figureClicked(fig) {
    if (fig == cur_fig) return null;
    document.getElementById("IMAGE").src = image_src + "loading.png";
    document.getElementById("IMAGE").src = image_src + "Figure_" +
	fig + "_" + global_theme + ".png";
    if (cur_chap != "") {
	/* document.getElementById("CHAPTER." + cur_chap).className = "topic"; */
	document.getElementById("SUMMARY." + cur_chap).className = "summaryHidden";
	/* cur_chap = ""; */
    }
    if (cur_fig != "") {
	document.getElementById("FIGURE." + cur_fig).className = "topic";
	document.getElementById("CODE.00").className = "codeListingHidden";
    }
    else {
	document.getElementById("SUMMARY.CONTAINER").className = "summaryContainerHidden";
	document.getElementById("IMAGE.CONTAINER").className = "image";
    }
    document.getElementById("FIGURE." + fig).className = "htopic";
    cur_fig = fig;
    setCookie("figure=", fig);
}


function switchLayout() {
    if (cur_code == "right") setLayout("bottom");
    else if (cur_code == "bottom") setLayout("right");
    else setLayout("bottom");
}


function setLayout(code) {
    if (cur_code == code) return;
    cur_code = code;
    /* alert(cur_code); */
    if (code == "right") {
	document.getElementById("CODE.CONTAINER").style.marginLeft = "740px";
	document.getElementById("CODE.CONTAINER").style.clear = "none";
    }
    else if (code == "bottom") {
	document.getElementById("CODE.CONTAINER").style.marginLeft = "150px";
	document.getElementById("CODE.CONTAINER").style.clear = "left";
    }
    setCookie("code=", cur_code);
}
