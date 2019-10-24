-- This script is a one-shot to remove the manual (normal table) matviews
-- that were created in dbnorth/metnorth by PyCDS. These tables are empty, and
-- will be replaced by native matviews of the same names. Not all of these tables
-- were created in metnorth.
DROP TABLE IF EXISTS vars_per_history_mv;
DROP TABLE IF EXISTS station_obs_stats_mv;
DROP TABLE IF EXISTS collapsed_vars_mv;
DROP TABLE IF EXISTS obs_count_per_month_history_mv;
DROP TABLE IF EXISTS climo_obs_count_mv;