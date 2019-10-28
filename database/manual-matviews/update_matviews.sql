BEGIN;
DROP INDEX IF EXISTS var_hist_idx;
SELECT refresh_matview('vars_per_history_mv');
CREATE INDEX var_hist_idx ON vars_per_history_mv(history_id, vars_id);
COMMIT;

BEGIN;
DROP INDEX IF EXISTS station_obs_stats_mv_idx;
SELECT refresh_matview('station_obs_stats_mv');
CREATE INDEX station_obs_stats_mv_idx ON station_obs_stats_mv(min_obs_time, max_obs_time, obs_count, station_id, history_id);
COMMIT;

BEGIN;
DROP INDEX IF EXISTS collapsed_vars_idx;
SELECT refresh_matview('collapsed_vars_mv');
CREATE INDEX collapsed_vars_idx ON collapsed_vars_mv(history_id);
COMMIT;

BEGIN;
DROP INDEX IF EXISTS obs_count_per_month_history_idx;
SELECT refresh_matview('obs_count_per_month_history_mv');
CREATE INDEX obs_count_per_month_history_idx ON obs_count_per_month_history_mv(date_trunc, history_id);
COMMIT;

BEGIN;
DROP INDEX IF EXISTS climo_obs_count_idx;
SELECT refresh_matview('climo_obs_count_mv');
CREATE INDEX climo_obs_count_idx ON climo_obs_count_mv(history_id);
COMMIT;

