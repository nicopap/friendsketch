function stashAndOpen(args) {
	var stashList = args[0];
	var link = args[1];
	stashList.forEach(function(stash) {
		sessionStorage.setItem(stash[0], stash[1]);
	});
	window.open(link, '_self', false);
}

function openLink(link) {
	window.open(link, '_self', false);
}
