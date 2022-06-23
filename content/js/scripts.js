// Need this to show animations when going back in browser
window.onunload = function() {};

// Add lightbox class to all image links
$("a[href$='.jpg'],a[href$='.jpeg'],a[href$='.JPG'],a[href$='.png'],a[href$='.gif']").addClass("image-popup");

window.MathJax = {
  tex: {
    inlineMath: [['$', '$'], ['\\(', '\\)']]
  }
};

$(document).ready(function() {
    $("body").addClass("fadedIn").removeClass("hidden");
    $(document).on("click", "a", function () {
      var newUrl = $(this).attr("href");
      if (!newUrl || newUrl[0] === "#") {
          location.hash = newUrl;
          return;
      }
      if ($('<a>').prop('href', newUrl).prop('hostname') !== window.location.hostname) {
        return;
      }
      $("body").removeClass("fadedIn").addClass("fadedOut");
      setTimeout(function () {
          location = newUrl;
      }, 750);
      return false;
  });
});
