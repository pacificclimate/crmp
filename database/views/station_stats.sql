CREATE OR REPLACE VIEW obs_count_per_day_history_v AS (
 SELECT count(*) AS count, date_trunc('day', obs_raw.obs_time) AS date_trunc, obs_raw.history_id
   FROM obs_raw
  GROUP BY date_trunc('day', obs_raw.obs_time), obs_raw.history_id
);