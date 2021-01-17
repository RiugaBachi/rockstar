// Need this to show animation when go back in browser
window.onunload = function() {};

// Add lightbox class to all image links
$("a[href$='.jpg'],a[href$='.jpeg'],a[href$='.JPG'],a[href$='.png'],a[href$='.gif']").addClass("image-popup");

window.MathJax = {
  tex: {
    inlineMath: [['$', '$'], ['\\(', '\\)']]
  }
};

// FitVids options
//$(function() {
//  $(".content").fitVids();
//});

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
	//$('.image-popup').magnificPopup({
  //  type: 'image',
  //  tLoading: 'Loading image #%curr%...',
  //  gallery: {
  //    enabled: true,
  //    navigateByImgClick: true,
  //    preload: [0,1] // Will preload 0 - before current, and 1 after the current image
  //  },
  //  image: {
  //    tError: '<a href="%url%">Image #%curr%</a> could not be loaded.',
  //  },
  //  removalDelay: 300, // Delay in milliseconds before popup is removed
  //  // Class that is added to body when popup is open. 
  //  // make it unique to apply your CSS animations just to this exact popup
  //  mainClass: 'mfp-fade'
  //});
});
