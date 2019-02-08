function sendCreateRequest() {
	var xmlHttp = new XMLHttpRequest();
	var inputUsername = document.getElementById("name-input").value;
	var params = JSON.stringify({
		game: "classic",
		username: inputUsername
	});

	xmlHttp.open('POST', "/friendk/rooms/create", true);
	xmlHttp.onreadystatechange = function() {
		if (xmlHttp.readyState === 4) {
			switch (xmlHttp.status) {
			case 201 :
				var roomToJoin = xmlHttp.response;
				sendJoinRequest(roomToJoin);
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
