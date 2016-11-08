-- Weather anomaly views
------------------------

-- Weather anomalies are, for each station, the departure of selected weather statistics from a baseline.
-- Currently those statistics are computed for each month, but there is no reason that other periods could not be used.
--
-- Weather anomalies are calculated for:
--  - Monthly average of daily maximum temperature
--  - Monthly average of daily minimum temperature
--  - Monthly total precipitation
--
-- The following views support those computations.


-- View: Daily maximum temperature
-- View: Daily minimum temperature
--  - These views support views that deliver monthly average of daily max/min temperature.
--  - These views may or may not be materialized. They may be useful by themselves or provide a basis for defining other
--      views (e.g., seasonal Tmax/Tmin).
--  - In the case that none of these things are true, they can be folded into monthly average view queries
--      as a subquery or CTE.
--  - Observations flagged with meta_native_flag.discard or meta_pcic_flag.discard are not included in the view.
--  - data_coverage is the fraction of observations actually available in a day relative to those potentially available
--      in a day. The computation is correct for a given day if and only if the observation frequency does not change
--      during that day. If such a change does occur, the data_coverage fraction for the day will be > 1, which is not
--      fatal to distinguishing adequate coverage.
CREATE OR REPLACE VIEW daily_max_temperature_v AS (
    SELECT
        obs.station_id,
        obs.vars_id,
        date_trunc('day', obs_time) AS obs_day,
        max(datum) AS statistic,
        sum(
            CASE hx.freq
            WHEN 'daily' THEN 1.0
            WHEN '1-hourly' THEN 1.0 / 24.0
            END
        ) AS data_coverage
    FROM
        obs_raw AS obs,
        INNER JOIN meta_vars AS vars USING (vars_id),
        INNER JOIN meta_history AS hx USING (history_id),
        LEFT JOIN meta_native_flag AS mnf USING (native_flag_id)
        LEFT JOIN meta_pcic_flag AS mpf USING (pcic_flag_id)
    WHERE
        mnf.discard IS NOT TRUE
        AND mpf.discard IS NOT TRUE
        AND vars.standard_name = 'air_temperature'
        AND vars.cell_method IN ('time:maximum', 'time:point') -- possibly include time:mean
        AND hx.freq IN ('1-hourly', 'daily')
    GROUP BY
        obs_day, station_id, vars_id
);

CREATE OR REPLACE VIEW daily_min_temperature_v AS (
    SELECT
        obs.station_id,
        obs.vars_id,
        date_trunc('day', obs_time) AS obs_day,
        min(datum) AS statistic,
        sum(
            CASE hx.freq
            WHEN 'daily' THEN 1.0
            WHEN '1-hourly' THEN 1.0 / 24.0
            END
        ) AS data_coverage
    FROM
        obs_raw AS obs,
        INNER JOIN meta_vars AS vars USING (vars_id),
        INNER JOIN meta_history AS hx USING (history_id),
        LEFT JOIN meta_native_flag AS mnf USING (native_flag_id)
        LEFT JOIN meta_pcic_flag AS mpf USING (pcic_flag_id)
    WHERE
        mnf.discard IS NOT TRUE
        AND mpf.discard IS NOT TRUE
        AND vars.standard_name = 'air_temperature'
        AND vars.cell_method IN ('time:minimum', 'time:point') -- possibly include time:mean
        AND hx.freq IN ('1-hourly', 'daily')
    GROUP BY
        obs_day, station_id, vars_id
);

-- View: Monthly average of daily maximum temperature
-- View: Monthly average of daily minimum temperature
--  - These views will be materialized.
--  - data_coverage is the fraction of of observations actually available in a month relative to those potentially available
--      in a month, and is robust to varying reporting frequencies on different days in the month (but see caveat for
--      daily data coverage above).
CREATE OR REPLACE VIEW monthly_average_of_daily_max_temperature_v AS (
    SELECT
        station_id,
        vars_id,
        date_trunc('month', obs_day) AS obs_month,
        avg(statistic) as statistic,
        sum(data_coverage) / DaysInMonth(obs_month) AS data_coverage
    FROM
        daily_max_temperature_v
    GROUP BY
        obs_month, station_id, vars_id
);

CREATE OR REPLACE VIEW monthly_average_of_daily_min_temperature_v AS (
    SELECT
        station_id,
        vars_id,
        date_trunc('month', obs_day) AS obs_month,
        avg(statistic) as statistic,
        sum(data_coverage) / DaysInMonth(obs_month) AS data_coverage
    FROM
        daily_min_temperature_v
    GROUP BY
        obs_month, station_id, vars_id
);

-- View: Monthly total precipitation
--  - This view will be materialized.
--  - Observations flagged with meta_native_flag.discard or meta_pcic_flag.discard are not included in the view.
--  - data_coverage is the fraction of observations actually available in a month relative to those potentially
--      available in a month. This computation is correct if and only if the observation frequency does not change
--      during any one day in the month. It remains approximately correct if such days are rare, and remains valid
--      for the purpose of distinguishing adequate coverage.
CREATE OR REPLACE VIEW monthly_total_precipitation AS (
    SELECT
        obs.station_id,
        obs.vars_id,
        date_trunc('month', obs_time) AS obs_month,
        sum(datum) AS statistic,
        sum(
            CASE hx.freq
            WHEN 'daily' THEN 1.0 / DaysInMonth(obs_time)
            WHEN '1-hourly' THEN 1.0 / (DaysInMonth(obs_time) * 24.0)
            END
        ) AS data_coverage
    FROM
        obs_raw AS obs,
        INNER JOIN meta_vars AS vars USING (vars_id),
        INNER JOIN meta_history AS hx USING (history_id),
        LEFT JOIN meta_native_flag AS mnf USING (native_flag_id)
        LEFT JOIN meta_pcic_flag AS mpf USING (pcic_flag_id)
    WHERE
        mnf.discard IS NOT TRUE
        AND mpf.discard IS NOT TRUE
        AND vars.standard_name IN (
            'lwe_thickness_of_precipitation_amount',
            'thickness_of_rainfall_amount',
            'thickness_of_snowfall_amount'  -- verify that this is rainfall equiv!
        )
        AND vars.cell_method = 'time:sum'
        AND hx.freq IN ('1-hourly', 'daily')
    GROUP BY
        obs_month, station_id, vars_id
);

