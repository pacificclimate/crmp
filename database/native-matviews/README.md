# Native materialized views

PostgreSQL has directly supported (native) matviews since ver. 9.3. 
We now have databases on later versions than that.

This directory contains SQL for creating native matviews that are equivalent
to the hand-built matviews implemented in `../matviews`.

The code in `../matviews` does not appear to be exactly what is now in `crmp`,
or at least some inference and digging would be needed to establish that.

Therefore we copy the code (views, functions, etc.) from the authoritative source,
`crmp`, into code here. Any disparity between this code and `../matviews` is due to
this difference.
