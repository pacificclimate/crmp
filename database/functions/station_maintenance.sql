CREATE OR REPLACE FUNCTION updateSdateEdate() RETURNS void AS $$
DECLARE
	sid  integer;
	min_date timestamp without time zone;
	max_date timestamp without time zone;
BEGIN
	FOR sid IN SELECT DISTINCT station_id FROM meta_history
	LOOP
		SELECT min(obs_time), max(obs_time) INTO min_date, max_date
			   FROM obs_raw NATURAL JOIN meta_history WHERE station_id = sid;

		UPDATE meta_station
		       SET (min_obs_time, max_obs_time) = (min_date, max_date)
		       WHERE station_id = sid;
END LOOP;
RETURN;
END;
$$ LANGUAGE 'plpgsql';

