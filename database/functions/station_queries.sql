-- This would be better if it could just return the data
-- unfortunately, it's not possible to return a record set w/o predefining
-- the number, and types of colums returned
-- See http://stackoverflow.com/questions/1613921/can-i-have-a-postgres-plpgsql-function-return-variable-column-records
-- for more info

CREATE OR REPLACE FUNCTION query_one_station(station_id integer)
       	  RETURNS varchar(4096) AS $$
DECLARE
	vars_id integer;
	v integer;
	var_name varchar(128);
	q varchar(4096);
	alias_id varchar(64);
	my_table record;
	return_value record;
BEGIN
	RAISE DEBUG 'Running station_table "%"', station_id;
	alias_id := '';
	FOR v, var_name IN
	    EXECUTE E'SELECT vars_id, net_var_name FROM meta_vars NATURAL JOIN meta_station WHERE cell_method !~ \'(within|over)\' AND station_id = ' || station_id
	LOOP
	    alias_id := alias_id || 'a';
	    RAISE DEBUG 'In loop, Row: "%" "%"', v, alias_id;

	    IF q is null
	    THEN
	       q := '(SELECT obs_time, datum AS ' || var_name || ', flag_name as ' || var_name || '_flag FROM obs_with_flags WHERE vars_id = ' || v || ' AND station_id = ' || station_id || ') AS ' || alias_id;
	    ELSE
	       q := q || ' FULL OUTER JOIN (SELECT obs_time, datum AS ' || var_name || ', flag_name as ' || var_name || '_flag FROM obs_with_flags WHERE vars_id = ' || v || ' AND station_id = ' || station_id || ') AS ' || alias_id || ' USING (obs_time)';
	    END IF;
	    RAISE DEBUG '%', q;	    
	END LOOP;
	q := 'SELECT * FROM' || q;
	RAISE DEBUG '%', q;
	RETURN q;
END;
$$ LANGUAGE plpgsql;

-- This article
-- http://okbob.blogspot.com/2008/08/using-cursors-for-generating-cross.html
-- Explains how to generate a query w/ a dynamic number of columns
-- Usage for the whole thing should be something like this:
-- BEGIN
-- do_query_one_station(1074);
-- FETCH ALL FROM result;
-- END

CREATE OR REPLACE FUNCTION do_query_one_station(station_id integer) RETURNS refcursor AS $$
DECLARE
	query text;
	result refcursor := 'result';
BEGIN
	query := query_one_station(station_id);
	OPEN result NO SCROLL FOR EXECUTE query;
	RETURN result;
END;
$$ LANGUAGE plpgsql;

-- This is exactly the same as query_one_station, but only uses climatological vars
CREATE OR REPLACE FUNCTION query_one_station_climo(station_id integer)
       	  RETURNS varchar(4096) AS $$
DECLARE
	vars_id integer;
	v integer;
	var_name varchar(128);
	q varchar(4096);
	alias_id varchar(64);
	my_table record;
	return_value record;
BEGIN
	RAISE DEBUG 'Running station_table "%"', station_id;
	alias_id := '';
	FOR v, var_name IN
	    EXECUTE E'SELECT vars_id, net_var_name FROM meta_vars NATURAL JOIN meta_station WHERE cell_method ~ \'(within|over)\' AND station_id = ' || station_id
	LOOP
	    alias_id := alias_id || 'a';
	    RAISE DEBUG 'In loop, Row: "%" "%"', v, alias_id;

	    IF q is null
	    THEN
	       q := '(SELECT obs_time, datum AS ' || var_name || ', flag_name as ' || var_name || '_flag FROM obs_with_flags WHERE vars_id = ' || v || ' AND station_id = ' || station_id || ') AS ' || alias_id;
	    ELSE
	       q := q || ' FULL OUTER JOIN (SELECT obs_time, datum AS ' || var_name || ', flag_name as ' || var_name || '_flag FROM obs_with_flags WHERE vars_id = ' || v || ' AND station_id = ' || station_id || ') AS ' || alias_id || ' USING (obs_time)';
	    END IF;
	    RAISE DEBUG '%', q;	    
	END LOOP;
	q := 'SELECT * FROM' || q;
	RAISE DEBUG '%', q;
	RETURN q;
END;
$$ LANGUAGE plpgsql;
