CREATE OR REPLACE VIEW collapsed_flags_v AS (
    SELECT obs_raw_id, array_to_string(ARRAY[foo.flag_name, bar.flag_name], ','::text) AS flags
    FROM 
    ( 
        SELECT obs_raw_pcic_flags.obs_raw_id, array_to_string(array_agg(meta_pcic_flag.flag_name), ','::text) AS flag_name
        FROM obs_raw_pcic_flags
        NATURAL JOIN meta_pcic_flag
        GROUP BY obs_raw_pcic_flags.obs_raw_id
    ) AS foo
    FULL JOIN 
    ( 
        SELECT obs_raw_native_flags.obs_raw_id, array_to_string(array_agg(meta_native_flag.flag_name), ','::text) AS flag_name
        FROM obs_raw_native_flags
        NATURAL JOIN meta_native_flag
        GROUP BY obs_raw_native_flags.obs_raw_id
    ) AS bar USING (obs_raw_id)
);

CREATE OR REPLACE VIEW obs_with_flags AS (
    SELECT meta_vars.vars_id, meta_vars.network_id, meta_vars.unit, meta_vars.standard_name, meta_vars.cell_method, meta_vars.net_var_name, x.obs_raw_id, h.station_id, x.obs_time, x.mod_time, x.datum, y.native_flag_id, z.flag_name, z.description, z.value AS flag_value
    FROM meta_vars
    NATURAL JOIN obs_raw x
    JOIN meta_history h ON h.history_id = x.history_id
    LEFT JOIN obs_raw_native_flags y ON x.obs_raw_id = y.obs_raw_id
    LEFT JOIN 
    (
        meta_native_flag z 
        LEFT JOIN meta_network ON z.network_id = meta_network.network_id
    ) ON y.native_flag_id = z.native_flag_id
);

CREATE OR REPLACE VIEW obs_with_flags_history AS (
    SELECT meta_vars.vars_id, meta_vars.network_id, meta_vars.unit, meta_vars.standard_name, meta_vars.cell_method, meta_vars.net_var_name, x.obs_raw_id, h.history_id, x.obs_time, x.mod_time, x.datum, y.native_flag_id, z.flag_name, z.description, z.value AS flag_value
    FROM meta_vars
    NATURAL JOIN obs_raw x
    JOIN meta_history h ON h.history_id = x.history_id
    LEFT JOIN obs_raw_native_flags y ON x.obs_raw_id = y.obs_raw_id
    LEFT JOIN 
    (
        meta_native_flag z 
        LEFT JOIN meta_network ON z.network_id = meta_network.network_id
    ) ON y.native_flag_id = z.native_flag_id
);