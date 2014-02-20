# What I want

* Auto-complete for available members of an object.
* Should work for partially-typed import lines too.

# Requirements

* Needs to be able to use a custom python interpreter.
* Failover to default auto-complete.

# How?

* Two cases: A dot autocomplete, or a partial import line.
* Determine if we're on an import line.
* If we are, run the import of the "from" module, then do a dir on the object.
* If not, get the word just behind the period, try to import it or look for it in globals(), and then do a dir on it.
