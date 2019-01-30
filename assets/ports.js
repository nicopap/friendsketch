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

function selectRoomid() {
    setTimeout(function() {
        function selectText(e) {
            this.focus();
            this.select();
        };
        document.getElementById("hidden-roomid").onclick = selectText;
    }, 100);
}
