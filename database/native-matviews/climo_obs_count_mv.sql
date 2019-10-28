-- View: climo_obs_count_mv

-- DROP MATERIALIZED VIEW climo_obs_count_mv;

CREATE MATERIALIZED VIEW climo_obs_count_mv
AS
    SELECT count(*) AS count, obs_raw.history_id
    FROM obs_raw NATURAL JOIN meta_vars
    WHERE meta_vars.cell_method::text ~ '(within|over)'::text
    GROUP BY obs_raw.history_id
WITH NO DATA;

ALTER TABLE climo_obs_count_mv
    OWNER TO metnorth;

CREATE INDEX climo_obs_count_idx
    ON climo_obs_count_mv
    USING btree
    (history_id);