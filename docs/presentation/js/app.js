(function () {
    var node = document.getElementById("container");
    var app = Elm.Presentation.init({ 
        node: node,
        flags: {url: "presentation.md"}
    });
    app.ports.slideChanged.subscribe(function(data){
        document.querySelectorAll('pre code').forEach(function(block){
            hljs.highlightBlock(block);
        });
    });
})();  