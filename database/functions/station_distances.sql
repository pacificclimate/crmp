-- Helper functions to assist in checking existing stations


--This returns the history_id of the closest station within a threshold of a given lat/lon
-- SLOW VERSION!!!
--CREATE OR REPLACE FUNCTION closest_stn_within_threshold(IN X numeric, IN Y numeric, IN thres integer, OUT history_id integer, OUT lat numeric, OUT lon numeric, OUT dist double precision) 
--RETURN record AS 
--$BODY$
--WITH overlapping_points AS (
--    SELECT history_id, lat, lon, Geography(ST_Transform(the_geom,4326)) as p_existing, Geography(ST_SetSRID(ST_MakePoint($1,$2),4326)) as p_new
--    FROM meta_history
--    WHERE the_geom && ST_Buffer(Geography(ST_SetSRID(ST_MakePoint($1,$2),4326)),$3)
--)

--SELECT history_id, lat, lon, ST_Distance(p_existing,p_new) as dist
--FROM overlapping_points
--ORDER BY dist
--LIMIT 1
--$BODY$
--LANGUAGE SQL;

-- Returns a set of all stations sorted by ascending distance within threshold
-- SLOW VERSION!!!!!
--CREATE OR REPLACE FUNCTION closest_stns_within_threshold(IN X numeric, IN Y numeric, IN thres integer, OUT history_id integer, OUT lat numeric, OUT lon numeric, OUT dist double precision)
--RETURNS SETOF record AS 
--$BODY$
--WITH overlapping_points AS (
--    SELECT history_id, lat, lon, Geography(ST_Transform(the_geom,4326)) as p_existing, Geography(ST_SetSRID(ST_MakePoint($1,$2),4326)) as p_new
--    FROM meta_history
--    WHERE the_geom && ST_Buffer(Geography(ST_SetSRID(ST_MakePoint($1,$2),4326)),$3)
--)

--SELECT history_id, lat, lon, ST_Distance(p_existing,p_new) as dist
--FROM overlapping_points
--ORDER BY dist
--$BODY$
--LANGUAGE SQL;

-- Returns a set of all stations sorted by ascending distance within threshold
CREATE OR REPLACE FUNCTION closest_stns_within_threshold(X numeric, Y numeric, thres integer)
RETURNS TABLE(history_id integer, lat numeric, lon numeric, dist double precision) AS
$BODY$

DECLARE
    mystr TEXT;
BEGIN
    mystr = 'WITH stns_in_thresh AS (
    SELECT history_id, lat, lon, Geography(ST_Transform(the_geom,4326)) as p_existing, Geography(ST_SetSRID(ST_MakePoint('|| X ||','|| Y ||'),4326)) as p_new
    FROM meta_history
    WHERE the_geom && ST_Buffer(Geography(ST_SetSRID(ST_MakePoint('|| X || ','|| Y ||'),4326)),'|| thres ||')
)
SELECT history_id, lat, lon, ST_Distance(p_existing,p_new) as dist
FROM stns_in_thresh
ORDER BY dist';
    RETURN QUERY EXECUTE mystr;
END;
$BODY$
LANGUAGE plpgsql
SECURITY DEFINER;

