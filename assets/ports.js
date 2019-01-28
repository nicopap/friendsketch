function stashAndOpen(args) {
    var stashList = args[0];
    var link = args[1];
    stashList.forEach(function(stash) {
        sessionStorage.setItem(stash[0], stash[1]);
    });
    window.open(link, '_self', false);
}


// Thank you so much, dotnetCarpenter on stackoverflow <3
function bottomScrollChat() {
    setTimeout(function() {
        var out = document.getElementById("messages");
        out.scrollTop = out.scrollHeight - out.clientHeight;
    }, 50);
}

function openLink(link) {
    window.open(link, '_self', false);
}

function copyCatch(args) {
    var elemId = args[0];
    var content = args[1];
    setTimeout(function() {
        var elem = document.getElementById(elemId);
        elem.addEventListener("copy" , function(event) {
            event.clipboardData.setData("text/plain", content);
            event.preventDefault();
        });
    }, 100);
}
