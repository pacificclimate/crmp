-- View: vars_per_history_mv

-- DROP MATERIALIZED VIEW vars_per_history_mv;

CREATE MATERIALIZED VIEW vars_per_history_mv
TABLESPACE pg_default
AS
    SELECT DISTINCT obs_raw.history_id,
    obs_raw.vars_id
    FROM obs_raw
WITH NO DATA;

ALTER TABLE vars_per_history_mv
    OWNER TO metnorth;

GRANT SELECT ON TABLE vars_per_history_mv TO metnorth_ro;
GRANT INSERT, SELECT, UPDATE, DELETE ON TABLE vars_per_history_mv TO metnorth_rw;
GRANT ALL ON TABLE vars_per_history_mv TO metnorth;
GRANT SELECT ON TABLE vars_per_history_mv TO viewer;

CREATE INDEX var_hist_idx
    ON vars_per_history_mv USING btree
    (history_id, vars_id)
    TABLESPACE pg_default;