function sendJoinRequest(roomToJoin) {
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
				var connectionId = xmlHttp.response;
				stashAndOpen([[
					[ "roomid", roomToJoin ],
					[ "connid", connectionId ],
					[ "username", inputUsername ],
					[ "retries", 0 ],
				], "/friendk/games/classic/index.html"]);
				break;
			case 409 :
				document.getElementById("username").classList.add("taken");
				document.getElementById("request").classList.add("taken");
				break;
			case 400 :
				document.getElementById("username").classList.add("invalid");
				document.getElementById("request").classList.add("invalid");
				break;
			default:
				document.getElementById("request").style['display'] = "none";
				document.getElementById("error-display").style['display'] = "";
			}
		}
	};
	xmlHttp.setRequestHeader("Content-Type", "application/json");
	xmlHttp.send(params);
}
