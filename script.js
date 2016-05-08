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
  var form = document.querySelector("form");
  var submit = form.querySelector("input[type=submit");

  submit.onclick = function (e) {
    var who = form["who"].value;
    var what = form["what"].value;

    if (what !== "") {
      var xhttp = new XMLHttpRequest();
      xhttp.open("POST", document.location.toString() + "ajax" , true);
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
    }

    e.preventDefault();
    return false;
  };
  console.log(form, submit);
})();

(function () {
    var graph = document.querySelector("#graph-image");
    var graphSrc = graph.src;

    function seconds(s) { return s * 1000; }
    function updateGraph() {
        console.info("Update graph");
        graph.src = graphSrc + "?" + (new Date().getTime());
    }

    setTimeout(function () {
        setInterval(updateGraph, seconds(10));
    }, seconds(30));
})();
