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
