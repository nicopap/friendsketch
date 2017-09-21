# Trivial copy-paste manual test template PLEASE DO !!NOT!! RUN

cabal build
cabal run &
roomName=$(curl -X POST "localhost:8080/rooms/create")
#vv Can connect to the open room
wscat --connect "ws://localhost:8080/ws/games/pintclone/$roomName/info/test1"
#vv Cannot connect with an already taken username
wscat --connect "ws://localhost:8080/ws/games/pintclone/$roomName/info/test1"
#vv Can connect to occupied room with new username
wscat --connect "ws://localhost:8080/ws/games/pintclone/$roomName/info/test3"

kill %+
