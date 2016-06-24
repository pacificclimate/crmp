DROP FUNCTION season(TIMESTAMP);

CREATE OR REPLACE FUNCTION season(d TIMESTAMP)
RETURNS DATE AS
$BODY$
DECLARE
    m DOUBLE PRECISION;
BEGIN
    m := date_part('month', d);
    CASE m
        WHEN 12, 1, 2 THEN RETURN ('' || extract(year from d) || '-01-15')::DATE;
        WHEN 3, 4, 5 THEN RETURN ('' || extract(year from d) || '-04-15')::DATE;
        WHEN 6, 7, 8 THEN RETURN ('' || extract(year from d) || '-07-15')::DATE;
        WHEN 9, 10, 11 THEN RETURN ('' || extract(year from d) || '-10-15')::DATE;
    END CASE;
END
$BODY$
LANGUAGE plpgsql;

-- Test queries
SELECT season('2001-05-01'::TIMESTAMP);
SELECT season('2001-08-01'::TIMESTAMP);
SELECT season('2002-10-31'::TIMESTAMP);
SELECT season('2008-12-31'::TIMESTAMP);
