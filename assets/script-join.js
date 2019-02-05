function sendJoinRequest() {
	var xmlHttp = new XMLHttpRequest();
	var inputUsername = document.getElementById("username").value;
	var params = JSON.stringify({
		roomid: roomToJoin,
		username: inputUsername
	});

	xmlHttp.open('POST', "/friendk/rooms/join", true);
	xmlHttp.onreadystatechange = function() {
		if (xmlHttp.readyState === 4) {
			switch (xmlHttp.status) {
			case 200 :
				stashAndOpen([[
					[ "roomid", roomToJoin ],
					[ "username", inputUsername ],
					[ "retries", 0 ],
				], "/friendk/games/classic/index.html"]);
				break;
			case 404 :
				document.getElementById("request-join").style['display'] = "none";
				document.getElementById("error-display").style['display'] = "";
				break;
			case 409 :
				document.getElementById("username").classList.add("taken");
				document.getElementById("request-join").classList.add("taken");
				break;
			case 400 :
				document.getElementById("username").classList.add("invalid");
				document.getElementById("request-join").classList.add("invalid");
				break;
			default:
				document.getElementById("request-join").style['display'] = "none";
				document.getElementById("error-display").style['display'] = "";
			}
		}
	};
	xmlHttp.setRequestHeader("Content-Type", "application/json");
	xmlHttp.send(params);
}
window.addEventListener("load", function() {
	var form = document.getElementById("request-join");
	form.addEventListener("submit", function(event) {
		event.preventDefault();
		sendJoinRequest();
	})
})
