-- View: obs_count_per_month_history_mv

-- DROP MATERIALIZED VIEW obs_count_per_month_history_mv;

CREATE MATERIALIZED VIEW obs_count_per_month_history_mv
AS
    SELECT count(*) AS count,
           date_trunc('month'::text, obs_raw.obs_time) AS date_trunc,
           obs_raw.history_id
    FROM obs_raw
    GROUP BY date_trunc('month'::text, obs_raw.obs_time), obs_raw.history_id
WITH NO DATA;

CREATE INDEX obs_count_per_month_history_idx
    ON obs_count_per_month_history_mv
    USING btree
    (date_trunc, history_id);