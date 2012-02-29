Troggie
=======

Troggie is an IRC Bot that was originally written to implement Factoids, the
main feature of [infobot](http://infobot.org/). It quickly expanded to include
several useful plugins such as a last time seen plugin, displaying titles of
URLs, weather lookups, Google search/translate/calc, along with a few lesser ones.

[PircBot](http://www.jibble.org/pircbot.php) is the base IRC library. Troggie was
originally written in Java using Apache Derby (now JavaDB). It then was migrated
to SQLite for the reduced memory footprint. Now it's being rewritten in Scala
using [Akka](http://akka.io/) for better concurrency (and to increase my Scala
proficiency).

Development TODO list:
----------------------

* handle shutdown nicely (ctrl-c as well as shutdown from within sbt) - have a
nice quit message
* implement ignore list. possibly in the router? subclass it?
* finish Seen plugin implementation
* implement important or working plugins in Java version before releasing beta:
	* UrlTitle
	* Seen
	* Factoid
* do we really need the extended driver? try without autoinc