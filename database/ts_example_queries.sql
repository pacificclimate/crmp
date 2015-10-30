-- PURE SQL MONTHLY
SELECT station_id,
	  date_trunc('month', obs_time) AS obs_time_trunc,
       avg(datum) AS obs_datum,
       count(datum) AS obs_count
FROM obs_raw
WHERE vars_id = 427 AND station_id = 1000
GROUP BY obs_time_trunc, station_id
HAVING count(datum) / DaysInMonth(CAST(date_trunc('month', obs_time) AS date)) > .85
ORDER BY obs_time_trunc

-- PURE SQL DAILY
SELECT station_id,
	  date_trunc('day', obs_time) AS obs_time_trunc,
       avg(datum) AS obs_datum,
       count(datum) AS obs_count
FROM obs_raw
WHERE vars_id = 427 AND station_id = 1000
GROUP BY obs_time_trunc, station_id
HAVING count(datum) / 24 > .85
ORDER BY obs_time_trunc

-- PURE SQL HOURLY
SELECT station_id,
	  date_trunc('hour', obs_time) AS obs_time_trunc,
       avg(datum) AS obs_datum,
       count(datum) AS obs_count
FROM obs_raw
WHERE vars_id = 427 AND station_id = 1593
GROUP BY obs_time_trunc, station_id
ORDER BY obs_time_trunc

-- COALESCEing multiple variables and flags into one table by timestamp
SELECT tmin, tmin_flag, tmax, tmax_flag, COALESCE(x.obs_time, y.obs_time) as obs_time FROM
(SELECT datum as tmin, obs_time, flag_name as tmin_flag from obs_with_flags WHERE station_id = 1074 AND vars_id = 427 ORDER BY obs_time) AS x
FULL OUTER JOIN
(SELECT datum as tmax, obs_time, flag_name as tmax_flag from obs_with_flags WHERE station_id = 1074 AND vars_id = 428 ORDER BY obs_time) AS y
ON (x.obs_time = y.obs_time)