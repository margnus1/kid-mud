Kid-MUD
=======

_KID is dirty_  
_Because he is playing in the MUD_


Project Description
-------------------

A MUD (Multi User Dungeon) is a text- based online game where players perform
actions by typing them (such as “go north” or “attack goblin”).  The world is
partitioned into zones, and each player exists in one zone at any one time, and
can move to neighboring zones. Zones can be occupied by several players as well
as one or more NPC (Non player characters) that can be interacted with
(e.g. attacked).

Kid-MUD is an Erlang MUD with a WebSocket interface. It is currently early in
development.


Who are we?
-----------

Eric Arnerlöv	 <eric.arnerlov.4839@student.uu.se>  
Michael Bergroth <michael.bergroth.5739@student.uu.se>  
Magnus Lång      <magnus.lang.7837@student.uu.se>  
Mikael Wiberg    <mikael.wiberg.6269@studnet.uu.se>


May the source be with you
--------------------------

Everything you need to compile and run the system is included in this directory.

However, you might want to get the most up to date version of this directory.

To fetch the source code for this project type the following command:

FIRST TIME  
$ git clone git://github.com/margnus1/kid-mud.git

AFTERWARS  
$ git pull

Dependencies 
------------
This software was developed and tested using Erlang R15B01 and Yaws 1.94
     	      	  	    	       
Make it happen
--------------
Using the make utility you can perform the following actions:

make                 ==> Compiles the Erlang source files if necessary.  
make clean           ==> Removes all beam files and html files generated by Edoc.  
make doc             ==> Generates Edoc documentation in the doc/html directory.  
make setup	         ==> Performs first-time setup of database.  
make start_server    ==> Starts the server.  
make start_webserver ==> Starts the WebSocket based Yaws interface.  
make start_client    ==> Starts the client.  
make test            ==> Runs all Eunit tests.


To run and test the server 
--------------------------

To use the MUD, you need to start the server. The server needs a database, so
begin by typing "make setup". This is nessescary only once.  
You can then use either the terminal or websocket interfaces
*   If you want to use the terminal interface, you start the server by typing 
    "make start_server".  
    To connect to it from another terminal, type "make start_client" and use the
    supplied command to start a client sesion against the server.
*   If you want to use the WebSocket interface, you start the server by typing 
    "make start_webserver".  
    You can then connect to it on adress "http://localhost:8080"

When you have logged in, you can write this, among other things:  
go north        ==> Moves to zone adjacent to the north  
say Hi!	        ==> Says Hi! to the other players  
attack Peter    ==> Attacks Peter  
stop            ==> You just wanted to intimidate him, so you stop attacking  
logout	        ==> Peter is mad with you, so you better leave
