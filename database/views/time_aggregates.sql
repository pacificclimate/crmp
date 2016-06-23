CREATE OR REPLACE VIEW monthly_agg_v AS (
SELECT station_id,
       vars_id,
       date_trunc('month', obs_time) AS obs_month,
       CASE cell_method
            WHEN 'time: mean'::text THEN avg(datum)
            WHEN 'time: sum'::text THEN sum(datum)
            WHEN 'time: maximum'::text THEN avg(datum)
            WHEN 'time: minimum'::text THEN avg(datum)
            WHEN 'time: point'::text THEN avg(datum)
            ELSE 'NaN'::double precision
        END AS datum
FROM obs_raw
    NATURAL JOIN meta_vars
    NATURAL JOIN meta_history
    NATURAL JOIN meta_station
    LEFT JOIN obs_raw_native_flags USING (obs_raw_id)
    LEFT JOIN meta_native_flag USING (native_flag_id)
    LEFT JOIN obs_raw_pcic_flags USING (obs_raw_id)
    LEFT JOIN meta_pcic_flag USING (pcic_flag_id)
WHERE freq = '1-hourly'
    AND meta_native_flag.discard IS NOT TRUE
    AND meta_pcic_flag.discard IS NOT TRUE
    AND cell_method in ('time: mean','time: sum','time: maximum','time: minimum','time: point')
GROUP BY obs_month, station_id, vars_id, cell_method
HAVING (count(datum) / daysinmonth(obs_month)*24.0 > 0.85)
UNION ALL
SELECT station_id,
       vars_id,
       date_trunc('month', obs_time) AS obs_month,
       CASE cell_method
            WHEN 'time: mean'::text THEN avg(datum)
            WHEN 'time: sum'::text THEN sum(datum)
            WHEN 'time: maximum'::text THEN avg(datum)
            WHEN 'time: minimum'::text THEN avg(datum)
            WHEN 'time: point'::text THEN avg(datum)
            ELSE 'NaN'::double precision
        END AS datum
FROM obs_raw
    NATURAL JOIN meta_vars
    NATURAL JOIN meta_history
    NATURAL JOIN meta_station
    LEFT JOIN obs_raw_native_flags USING (obs_raw_id)
    LEFT JOIN meta_native_flag USING (native_flag_id)
    LEFT JOIN obs_raw_pcic_flags USING (obs_raw_id)
    LEFT JOIN meta_pcic_flag USING (pcic_flag_id)
WHERE freq = 'daily'
    AND meta_native_flag.discard IS NOT TRUE
    AND meta_pcic_flag.discard IS NOT TRUE
    AND cell_method in ('time: mean','time: sum','time: maximum','time: minimum','time: point')
GROUP BY obs_month, station_id, vars_id, cell_method
HAVING (count(datum) / daysinmonth(obs_month)::float > 0.85)
ORDER BY vars_id, station_id, obs_month
);

CREATE OR REPLACE VIEW hourly_view AS (
    SELECT obs_raw.obs_raw_id, 
        date_trunc('hour', obs_raw.obs_time) AS obs_hour, 
        obs_raw.datum, 
        meta_history.freq, 
        meta_vars.cell_method, 
        obs_raw_native_flags.native_flag_id, 
        obs_raw_pcic_flags.pcic_flag_id
    FROM meta_vars
    JOIN obs_raw ON meta_vars.vars_id = obs_raw.vars_id
    JOIN meta_history ON obs_raw.history_id = meta_history.history_id
    LEFT JOIN obs_raw_native_flags USING (obs_raw_id)
    LEFT JOIN meta_native_flag USING (native_flag_id)
    LEFT JOIN obs_raw_pcic_flags USING (obs_raw_id)
    LEFT JOIN meta_pcic_flag USING (pcic_flag_id)
    WHERE meta_history.freq = '1-hourly' AND meta_native_flag.discard IS NOT TRUE AND meta_pcic_flag.discard IS NOT TRUE
);

CREATE OR REPLACE VIEW daily_view AS (
    SELECT obs_raw.obs_raw_id, 
        date_trunc('day', obs_raw.obs_time) AS obs_day, 
        obs_raw.datum, 
        meta_history.freq, 
        meta_vars.cell_method, 
        obs_raw_native_flags.native_flag_id, 
        obs_raw_pcic_flags.pcic_flag_id
    FROM meta_vars
    JOIN obs_raw ON meta_vars.vars_id = obs_raw.vars_id
    JOIN meta_history ON obs_raw.history_id = meta_history.history_id
    LEFT JOIN obs_raw_native_flags USING (obs_raw_id)
    LEFT JOIN meta_native_flag USING (native_flag_id)
    LEFT JOIN obs_raw_pcic_flags USING (obs_raw_id)
    LEFT JOIN meta_pcic_flag USING (pcic_flag_id)
    WHERE meta_history.freq = 'daily' AND meta_native_flag.discard IS NOT TRUE AND meta_pcic_flag.discard IS NOT TRUE
);

CREATE OR REPLACE VIEW daily_agg AS (
    SELECT meta_history.station_id, obs_raw.vars_id, daily.obs_day, daily.datum, daily.cell_method
    FROM daily_view daily
    NATURAL JOIN obs_raw
    JOIN meta_history ON obs_raw.history_id = meta_history.history_id
    UNION 
    SELECT meta_history.station_id, 
        obs_raw.vars_id, 
        date_trunc('day', hourly.obs_hour) AS obs_day, 
        CASE hourly.cell_method
            WHEN 'time: mean'::text THEN avg(hourly.datum)
            WHEN 'time: sum'::text THEN sum(hourly.datum)::double precision
            WHEN 'time: maximum'::text THEN max(hourly.datum)::double precision
            WHEN 'time: minimum'::text THEN min(hourly.datum)::double precision
            WHEN 'time: point'::text THEN avg(hourly.datum)
            ELSE 'NaN'::double precision
        END AS datum, 
        hourly.cell_method
    FROM hourly_view hourly
    NATURAL JOIN obs_raw
    JOIN meta_history ON obs_raw.history_id = meta_history.history_id
    NATURAL JOIN meta_vars
    GROUP BY date_trunc('day', hourly.obs_hour), meta_history.station_id, obs_raw.vars_id, hourly.cell_method
    HAVING (count(hourly.datum) / 24) > 0.85
    ORDER BY 2, 1, 3
);

CREATE OR REPLACE VIEW monthly_agg AS (
    SELECT daily_agg.station_id, daily_agg.vars_id, date_trunc('month', daily_agg.obs_day) AS obs_month, 
        CASE daily_agg.cell_method
            WHEN 'time: mean'::text THEN avg(daily_agg.datum)
            WHEN 'time: sum'::text THEN sum(daily_agg.datum)
            WHEN 'time: maximum'::text THEN avg(daily_agg.datum)
            WHEN 'time: minimum'::text THEN avg(daily_agg.datum)
            WHEN 'time: point'::text THEN avg(daily_agg.datum)
            ELSE 'NaN'::double precision
        END AS datum, daily_agg.cell_method
    FROM daily_agg
    NATURAL JOIN meta_vars
    GROUP BY date_trunc('month', daily_agg.obs_day), daily_agg.station_id, daily_agg.vars_id, daily_agg.cell_method
    HAVING (count(daily_agg.datum) / daysinmonth(date_trunc('month', daily_agg.obs_day)::date)) > 0.85
    ORDER BY daily_agg.station_id, daily_agg.vars_id, obs_month
);

CREATE OR REPLACE VIEW mof_derived_daily_temp AS (
    SELECT * FROM
    (
        SELECT station_id,  
            626 as vars_id, 
            date_trunc('day', obs_time) AS obs_day,
            min(datum) as datum,
            'time: minimum'::text as cell_method
        FROM meta_vars
            NATURAL JOIN obs_raw
            NATURAL JOIN meta_history mh
            NATURAL JOIN meta_station ms
        WHERE ms.network_id = 12
            AND freq='1-hourly'
            AND standard_name = 'air_temperature'
            AND cell_method = 'time: point'
        GROUP BY station_id, vars_id, obs_day
        HAVING (count(datum) / 24 > 0.85)
    ) as tn
    UNION (
        SELECT station_id,
            625 as vars_id,
            date_trunc('day', obs_time) AS obs_day,
            max(datum) as datum,
            'time: maximum'::text as cell_method
        FROM meta_vars
            NATURAL JOIN obs_raw
            NATURAL JOIN meta_history mh
            NATURAL JOIN meta_station ms
        WHERE ms.network_id = 12
            AND freq='1-hourly'
            AND standard_name = 'air_temperature'
            AND cell_method = 'time: point'
        GROUP BY station_id, vars_id, obs_day
        HAVING (count(datum) / 24 > 0.85)
    )
    ORDER BY station_id, vars_id, obs_day
);

CREATE OR REPLACE VIEW mof_derived_monthly_temp AS (
    SELECT station_id,
        vars_id,
        date_trunc('month', obs_day) as obs_month,
        avg(datum) as datum,
        cell_method
    FROM
        mof_derived_daily_temp ddt
    GROUP BY station_id, vars_id, obs_month, cell_method
    HAVING count(datum) / crmp.daysinmonth(date_trunc('month', obs_day)) > 0.85
    ORDER BY station_id, vars_id, date_trunc('month', obs_day)
);

CREATE OR REPLACE VIEW env_aqn_derived_daily_temp AS (
    SELECT * FROM
    (
        SELECT station_id,
            628 as vars_id,
            date_trunc('day', obs_time) AS obs_day,
            min(datum) as datum,
            'time: minimum'::text as cell_method
        FROM meta_vars
            NATURAL JOIN obs_raw
            NATURAL JOIN meta_history mh
            NATURAL JOIN meta_station ms
        WHERE ms.network_id = 9
            AND freq='1-hourly'
            AND standard_name = 'air_temperature'
            AND cell_method = 'time: mean'
        GROUP BY station_id, vars_id, obs_day
        HAVING (count(datum) / 24 > 0.85)
    ) as tn
    UNION (
        SELECT station_id,
            627 as vars_id,
            date_trunc('day', obs_time) AS obs_day,
            max(datum) as datum,
            'time: maximum'::text as cell_method
        FROM meta_vars
            NATURAL JOIN obs_raw
            NATURAL JOIN meta_history mh
            NATURAL JOIN meta_station ms
        WHERE ms.network_id = 9
            AND freq='1-hourly'
            AND standard_name = 'air_temperature'
            AND cell_method = 'time: mean'
        GROUP BY station_id, vars_id, obs_day
        HAVING (count(datum) / 24 > 0.85)
    )
    ORDER BY station_id, vars_id, obs_day
);

CREATE OR REPLACE VIEW env_aqn_derived_monthly_temp AS (
    SELECT station_id,
        vars_id,
        date_trunc('month', obs_day) as obs_month,
        avg(datum) as datum,
        cell_method
    FROM
        env_aqn_derived_daily_temp ddt
    GROUP BY station_id, vars_id, obs_month, cell_method
    HAVING count(datum) / crmp.daysinmonth(date_trunc('month', obs_day)::date) > 0.85
    ORDER BY station_id, vars_id, date_trunc('month', obs_day)
);

CREATE OR REPLACE VIEW frbc_derived_daily_temp AS (
    SELECT * FROM
    (
        SELECT station_id,
            630 as vars_id,
            date_trunc('day', obs_time) AS obs_day,
            min(datum) as datum,
            'time: minimum'::text as cell_method
        FROM meta_vars
            NATURAL JOIN obs_raw
            NATURAL JOIN meta_history mh
            NATURAL JOIN meta_station ms
        WHERE ms.network_id = 16
            AND freq='1-hourly'
            AND standard_name = 'air_temperature'
            AND cell_method = 'time: mean'
        GROUP BY station_id, vars_id, obs_day
        HAVING (count(datum) / 24 > 0.85)
    ) as tn
    UNION (
        SELECT station_id,
            629 as vars_id,
            date_trunc('day', obs_time) AS obs_day,
            max(datum) as datum,
            'time: maximum'::text as cell_method
        FROM meta_vars
            NATURAL JOIN obs_raw
            NATURAL JOIN meta_history mh
            NATURAL JOIN meta_station ms
        WHERE ms.network_id = 16
            AND freq='1-hourly'
            AND standard_name = 'air_temperature'
            AND cell_method = 'time: mean'
        GROUP BY station_id, vars_id, obs_day
        HAVING (count(datum) / 24 > 0.85)
    )
    ORDER BY station_id, vars_id, obs_day
);

CREATE OR REPLACE VIEW frbc_derived_monthly_temp AS (
    SELECT station_id,
        vars_id,
        date_trunc('month', obs_day) as obs_month,
        avg(datum) as datum,
        cell_method
    FROM
        frbc_derived_daily_temp ddt
    GROUP BY station_id, vars_id, obs_month, cell_method
    HAVING count(datum) / crmp.daysinmonth(date_trunc('month', obs_day)::date) > 0.85
    ORDER BY station_id, vars_id, date_trunc('month', obs_day)
);
