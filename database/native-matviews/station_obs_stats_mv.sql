CREATE MATERIALIZED VIEW crmp.station_obs_stats_mv
TABLESPACE pg_default
AS
    SELECT
           meta_history.station_id,
           foo.history_id,
           foo.min_obs_time,
           foo.max_obs_time,
           foo.obs_count
    FROM (
        SELECT min(obs_raw.obs_time) AS min_obs_time,
               max(obs_raw.obs_time) AS max_obs_time,
               obs_raw.history_id, count(DISTINCT obs_raw.obs_time) AS obs_count
       FROM obs_raw
       GROUP BY obs_raw.history_id
    ) foo
        NATURAL JOIN meta_history
WITH NO DATA;

ALTER TABLE crmp.station_obs_stats_mv
    OWNER TO metnorth;

GRANT SELECT ON TABLE crmp.station_obs_stats_mv TO metnorth_ro;
GRANT INSERT, SELECT, UPDATE, DELETE ON TABLE crmp.station_obs_stats_mv TO metnorth_rw;
GRANT ALL ON TABLE crmp.station_obs_stats_mv TO metnorth;
GRANT SELECT ON TABLE crmp.station_obs_stats_mv TO viewer;

CREATE INDEX station_obs_stats_mv_idx
    ON station_obs_stats_mv
        USING btree
        (min_obs_time, max_obs_time, obs_count, station_id, history_id);