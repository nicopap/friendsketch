function sendCreateRequest() {
	var xmlHttp = new XMLHttpRequest();
	var inputUsername = document.getElementById("username").value;
	var params = JSON.stringify({
		game: "classic",
		username: inputUsername
	});

	xmlHttp.open('POST', "/friendk/rooms/create", true);
	xmlHttp.onreadystatechange = function() {
		if (xmlHttp.readyState === 4) {
			switch (xmlHttp.status) {
			case 201 :
				var roomToJoin = JSON.parse(xmlHttp.response);
                sendJoinRequest(roomToJoin);
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
