/* Now create the views we need to speed up performance */
CREATE OR REPLACE VIEW vars_per_history_v AS
  SELECT DISTINCT history_id, vars_id FROM obs_raw;

SELECT create_matview('vars_per_history_mv', 'vars_per_history_v');
CREATE INDEX var_hist_idx ON vars_per_history_mv(history_id, vars_id);

CREATE OR REPLACE VIEW station_obs_stats_v AS
  SELECT station_id, history_id, min_obs_time, max_obs_time, obs_count
    FROM (
      SELECT min(obs_time) AS min_obs_time, max(obs_time) AS max_obs_time, history_id, count(DISTINCT obs_time) AS obs_count
      FROM obs_raw
      GROUP BY history_id
    ) AS foo
  NATURAL JOIN meta_history;

SELECT create_matview('station_obs_stats_mv', 'station_obs_stats_v');
CREATE INDEX station_obs_stats_mv_idx ON station_obs_stats_mv(min_obs_time, max_obs_time, station_id, history_id);

CREATE OR REPLACE VIEW collapsed_vars_v AS 
  SELECT history_id, array_to_string(array_agg(standard_name || regexp_replace(cell_method, 'time: ', '_', 'g')), ', '::text) AS vars,
         array_to_string(array_agg(display_name), '|') AS display_names
  FROM vars_per_history_mv NATURAL JOIN meta_vars
  GROUP BY history_id;

SELECT create_matview('collapsed_vars_mv', 'collapsed_vars_v');
CREATE INDEX collapsed_vars_idx ON collapsed_vars_mv(history_id);

CREATE OR REPLACE VIEW obs_count_per_month_history_v AS
  SELECT count(*) AS count, date_trunc('month'::text, obs_raw.obs_time) AS date_trunc, obs_raw.history_id
  FROM obs_raw
  GROUP BY date_trunc('month'::text, obs_raw.obs_time), obs_raw.history_id;

SELECT create_matview('obs_count_per_month_history_mv', 'obs_count_per_month_history_v');
CREATE INDEX obs_count_per_month_history_idx ON obs_count_per_month_history_mv(date_trunc, history_id);

CREATE OR REPLACE VIEW climo_obs_count_v AS
  SELECT count(*) AS count, history_id
  FROM obs_raw NATURAL JOIN meta_vars
  WHERE cell_method ~ '(within|over)'
  GROUP BY history_id;

SELECT create_matview('climo_obs_count_mv', 'climo_obs_count_v');
CREATE INDEX climo_obs_count_idx ON climo_obs_count_mv(history_id);
