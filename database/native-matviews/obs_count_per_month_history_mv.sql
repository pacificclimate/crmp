-- View: obs_count_per_month_history_mv

-- DROP MATERIALIZED VIEW obs_count_per_month_history_mv;

CREATE MATERIALIZED VIEW obs_count_per_month_history_mv
TABLESPACE pg_default
AS
    SELECT count(*) AS count,
           date_trunc('month'::text, obs_raw.obs_time) AS date_trunc,
           obs_raw.history_id
    FROM obs_raw
    GROUP BY date_trunc('month'::text, obs_raw.obs_time), obs_raw.history_id
WITH NO DATA;

ALTER TABLE obs_count_per_month_history_mv
    OWNER TO metnorth;

GRANT SELECT ON TABLE obs_count_per_month_history_mv TO metnorth_ro;
GRANT INSERT, SELECT, UPDATE, DELETE ON TABLE obs_count_per_month_history_mv TO metnorth_rw;
GRANT ALL ON TABLE obs_count_per_month_history_mv TO metnorth;
GRANT SELECT ON TABLE obs_count_per_month_history_mv TO viewer;

CREATE INDEX obs_count_per_month_history_idx
    ON obs_count_per_month_history_mv
    USING btree
    (date_trunc, history_id)
    TABLESPACE pg_default;