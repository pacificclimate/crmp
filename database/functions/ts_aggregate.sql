CREATE OR REPLACE FUNCTION DaysInMonth(date) RETURNS double precision AS '
SELECT EXTRACT(DAY FROM CAST(date_trunc(''month'', $1) + interval ''1 month''
- interval ''1 day'' as timestamp));
' LANGUAGE sql;

CREATE OR REPLACE FUNCTION LastDateOfMonth(date)
RETURNS date AS
$BODY$
SELECT CAST(date_trunc('month', $1) + interval '1 month' - interval '1 day' as date);
$BODY$
LANGUAGE sql;

CREATE OR REPLACE FUNCTION monthly_ts(station_id integer, vars_id integer, percent_obs real,
       	  	  	   		OUT monthly_time timestamp, OUT monthly_mean real, OUT percent_obs_available real, OUT monthly_count integer)
					RETURNS SETOF RECORD AS $$
DECLARE
	the_month date;
BEGIN
	RAISE DEBUG 'Running monthly_ts "%" "%" "%"', station_id, vars_id, percent_obs;
	FOR monthly_time, monthly_mean, monthly_count IN EXECUTE
	    E'SELECT date_trunc(\'month\', obs_time) as obs_time_trunc, avg(datum) as obs_datum, count(datum) as obs_count FROM obs_raw WHERE station_id = ' || station_id || ' AND vars_id = ' || vars_id || ' GROUP BY obs_time_trunc ORDER BY obs_time_trunc'
	LOOP
		RAISE DEBUG 'In loop, Row: "%" "%" "%"', monthly_time, monthly_mean, monthly_count;
		the_month := CAST(monthly_time AS date);
		percent_obs_available := monthly_count / (DaysInMonth(the_month));
	    	IF percent_obs_available >= percent_obs THEN
		   RAISE DEBUG 'Conditional is TRUE';
		   RETURN NEXT;
		END IF;
	END LOOP;
	RETURN;

END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION daily_ts(station_id integer, vars_id integer, percent_obs real,
       	  	  	   	    OUT daily_time timestamp, OUT daily_mean real, OUT percent_obs_available real, OUT daily_count integer)
				    RETURNS SETOF RECORD AS $$
DECLARE
BEGIN
	RAISE DEBUG 'Running daily_ts "%" "%" "%"', station_id, vars_id, percent_obs;
	FOR daily_time, daily_mean, daily_count IN EXECUTE
	    E'SELECT date_trunc(\'day\', obs_time) as obs_time_trunc, avg(datum) as obs_datum, count(datum) as obs_count FROM obs_raw WHERE station_id = ' || station_id || ' AND vars_id = ' || vars_id || ' GROUP BY obs_time_trunc ORDER BY obs_time_trunc'
	LOOP
		RAISE DEBUG 'In loop, Row: "%" "%" "%"', daily_time, daily_mean, daily_count;
		percent_obs_available := daily_count / 24.0;
	    	IF percent_obs_available >= percent_obs THEN
		   RAISE DEBUG 'Conditional is TRUE';
		   RETURN NEXT;
		END IF;
	END LOOP;
	RETURN;
	    
END;
$$ LANGUAGE plpgsql;
