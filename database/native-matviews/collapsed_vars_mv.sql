-- View: collapsed_vars_mv

-- DROP MATERIALIZED VIEW collapsed_vars_mv;

CREATE MATERIALIZED VIEW collapsed_vars_mv
AS
    SELECT vars_per_history_mv.history_id,
           array_to_string(array_agg(meta_vars.standard_name::text || regexp_replace(meta_vars.cell_method::text, 'time: '::text, '_'::text, 'g'::text)), ', '::text) AS vars,
           array_to_string(array_agg(meta_vars.display_name), '|'::text) AS display_names
    FROM vars_per_history_mv
             NATURAL JOIN meta_vars
    GROUP BY vars_per_history_mv.history_id
WITH NO DATA;

ALTER TABLE collapsed_vars_mv
    OWNER TO metnorth;

CREATE INDEX collapsed_vars_idx
  ON collapsed_vars_mv
  USING btree
  (history_id);