(function () {
    var node = document.getElementById("container");
    var app = Elm.Presentation.init({ 
        node: node,
        flags: {url: "presentation.md"}
    });
})();