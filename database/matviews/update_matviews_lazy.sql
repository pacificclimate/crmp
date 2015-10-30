BEGIN;
SELECT vars_per_history_mv_refresh();
COMMIT;

BEGIN;
SELECT station_obs_stats_mv_refresh();
COMMIT;

BEGIN;
SELECT collapsed_vars_mv_refresh();
COMMIT;

BEGIN;
SELECT obs_count_per_month_history_mv_refresh();
COMMIT;

BEGIN;
SELECT climo_obs_count_mv_refresh();
COMMIT;

