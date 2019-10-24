# Hand-built materialized views

PostgreSQL < 9.3 did not support matviews directly.
Instead, one had to build the mechanism for creating and maintaining matviews
into a database by hand. 

There are doubtless several ways to do this. The way adopted here is described
in http://tech.jonathangardner.net/wiki/PostgreSQL/Materialized_Views.