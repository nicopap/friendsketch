function sendJoinRequest(roomToJoin) {
	var xmlHttp = new XMLHttpRequest();
	var inputUsername = document.getElementById("name-input").value;
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
				var field = document.getElementById("name-field");
				field.dataset.errmsg = "name already taken";
				field.classList.add("invalid");
				document.getElementById("name-input").classList.add("invalid");
				break;
			case 400 :
				var re = /Request body deserialize error: (.*) at line 1 column.*/;
				var cause = re.exec(xmlHttp.response)[1];
				var message = cause?cause:"Invalid display name";
				var field = document.getElementById("name-field");
				field.dataset.errmsg = message;
				field.classList.add("invalid");
				document.getElementById("name-input").classList.add("invalid");
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
