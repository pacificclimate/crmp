-- View: vars_per_history_mv

-- DROP MATERIALIZED VIEW vars_per_history_mv;

CREATE MATERIALIZED VIEW vars_per_history_mv
AS
    SELECT DISTINCT obs_raw.history_id,
    obs_raw.vars_id
    FROM obs_raw
WITH NO DATA;

CREATE INDEX var_hist_idx
    ON vars_per_history_mv USING btree
    (history_id, vars_id);