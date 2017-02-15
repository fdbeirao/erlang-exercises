-module(rooms).

-export([initial_room_id/0, get_room_description/1, get_room_exits/1]).

-record(room, { id, descr="", exits=[] }).

initial_room_id() ->
  [#room{ id=InitialRoomId }|_] = rooms(),
  InitialRoomId.

rooms() ->
  [
    #room{ id=start, descr="You find yourself in a round room with a pillar in the middle.", exits=[closet]},
    #room{ id=closet, descr="You are in a cramped closet", exits=[start] }
  ].

get_room_by_id(RoomId) ->
  lists:keyfind(RoomId, #room.id, rooms()).

get_room_description(RoomId) ->
  #room{ descr=Descr } = get_room_by_id(RoomId),
  Descr.

get_room_exits(RoomId) ->
  #room{ exits=RoomExits } = get_room_by_id(RoomId),
  RoomExits.