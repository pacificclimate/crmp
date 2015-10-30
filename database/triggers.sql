--Must be created by database superuser due to c function
CREATE OR REPLACE FUNCTION moddatetime()
    RETURNS trigger AS
'$libdir/moddatetime', 'moddatetime'
    LANGUAGE c VOLATILE;