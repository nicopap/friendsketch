function stashAndOpen(args) {
    var stashList = args[1];
    var link = args[0];
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
    var spoilerField = document.getElementById("roomid");
    spoilerField.focus();
    spoilerField.select();
}
