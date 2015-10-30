/*
This file sets up permissions for the group and task specific roles

Requires that root_permissions.sql have already been run

Run as database owner
*/

-- Viewer
GRANT SELECT ON spatial_ref_sys, meta_native_flag, meta_vars, meta_station, meta_pcic_flag, meta_alias, meta_history, obs_raw_pcic_flags, meta_sensor, obs_raw_native_flags, meta_network, geometry_columns, station_obs_stats_mv, meta_network_geoserver, stats_station_var, obs_count_per_month_history_mv, climo_obs_count_mv, time_bounds, obs_raw, vars_per_history_mv, collapsed_vars_mv, obs_with_flags to Viewer;
GRANT USAGE ON SCHEMA crmp to Viewer;

-- Inspector
GRANT ALL ON meta_history, meta_station, meta_vars, meta_network, meta_pcic_flag, obs_raw_pcic_flags, meta_native_flag, obs_raw_native_flags to Inspector;
GRANT INSERT ON TABLE obs_raw TO Inspector;
GRANT SELECT ON monthly_agg, daily_agg, daily_view, hourly_view, mof_derived_daily_temp, mof_derived_monthly_temp to Inspector;
GRANT SELECT ON env_aqn_derived_daily_temp, env_aqn_derived_monthly_temp to Inspector;
GRANT SELECT ON frbc_derived_daily_temp, frbc_derived_monthly_temp to Inspector;
GRANT USAGE ON history_id_seq, station_id_seq, vars_id_seq, network_id_seq, pcic_flag_id_seq, native_flag_id_seq, obs_raw_id_seq to Inspector;


-- Steward
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA crmp to Steward;
GRANT USAGE ON ALL sequences IN SCHEMA crmp TO Steward;
    
-- crmprtd
GRANT USAGE ON SCHEMA crmp to crmprtd;
GRANT SELECT, UPDATE, INSERT on meta_history, meta_station to crmprtd;
GRANT SELECT on meta_network, meta_vars to crmprtd;
GRANT USAGE, SELECT on obs_raw_id_seq, history_id_seq, station_id_seq to crmprtd;
GRANT SELECT, INSERT on obs_raw to crmprtd;
GRANT CONNECT ON DATABASE crmp to crmprtd;

-- httpd
GRANT USAGE ON SCHEMA crmp to httpd;
GRANT SELECT ON spatial_ref_sys, meta_native_flag, meta_vars, meta_station, meta_pcic_flag, meta_history, obs_raw_pcic_flags, obs_raw_native_flags, meta_network, geometry_columns, geography_columns, station_obs_stats_mv, meta_network_geoserver, crmp_network_geoserver, stats_station_var, obs_count_per_month_history_mv, climo_obs_count_mv, obs_raw, vars_per_history_mv, collapsed_vars_mv, obs_with_flags to httpd;
