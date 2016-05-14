/* yee yee */
Array.prototype.slice.call(document.querySelectorAll("button.magic-auto-fill"))
  .forEach(function (button) {
    button.onclick = function (e) {
      var field = document.querySelector("#who");
      field.value = button.innerText;

      e.preventDefault();
      return false;
    };
  });

(function () {
  var lastclick = 0;

  var form = document.querySelector("form");
  var submit = form.querySelector("input[type=submit");

  var baseurl = document.location.toString();
  if (baseurl[baseurl.length - 1] !== "/") {
    baseurl = baseurl + "/";
  }

  function updateSubmit() {
    if (lastclick <= 0) {
      submit.value = "Lähetä";
      submit.disabled = form["what"].value === "";
    } else if (lastclick > 0) {
      submit.value = "Lähetä (" + lastclick + ")";
      submit.disabled = true;
    }
  }

  Array.prototype.slice.call(form["what"])
    .forEach(function (element) {
      console.log(element);
      element.onchange = updateSubmit;
    });

  submit.onclick = function (e) {
    var who = form["who"].value;
    var what = form["what"].value;

    if (what !== "") {
      var xhttp = new XMLHttpRequest();
      xhttp.open("POST", baseurl + "ajax" , true);
      xhttp.setRequestHeader("Content-Type", "application/json;charset=UTF-8");
      xhttp.send(JSON.stringify({
        member: who,
        action: what,
      }));
      xhttp.onreadystatechange = function() {
        if (xhttp.readyState == 4 && xhttp.status == 200) {
          var json = JSON.parse(xhttp.responseText);

          document.querySelector("#actions-table").outerHTML = json.data;
        }
      };

      lastclick = 20;
      updateSubmit();
    }

    setInterval(function () {
        lastclick -= 1;
        updateSubmit();
    }, 1000);

    e.preventDefault();
    return false;
  };

  console.log(form, submit, baseurl);
  updateSubmit();
})();

(function () {
    var baseurl = document.location.toString();
    if (baseurl[baseurl.length - 1] !== "/") {
      baseurl = baseurl + "/";
    }

    var graph = document.querySelector("#graph-image");
    var graphSrc = graph.src;

    function seconds(s) { return s * 1000; }
    function updateGraph() {
        console.info("Update graph and table");
        graph.src = graphSrc + "?" + (new Date().getTime());

        var xhttp = new XMLHttpRequest();
        xhttp.open("GET", baseurl + "table" , true);
        xhttp.send();
        xhttp.onreadystatechange = function() {
          if (xhttp.readyState == 4 && xhttp.status == 200) {
            var json = JSON.parse(xhttp.responseText);
            document.querySelector("#actions-table").outerHTML = json.data;
          }
        };
    }

    setTimeout(function () {
        setInterval(updateGraph, seconds(10));
    }, seconds(30));
})();
