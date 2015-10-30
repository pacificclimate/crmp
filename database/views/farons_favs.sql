CREATE OR REPLACE VIEW farons_favs AS (
    SELECT meta_station.network_id, meta_history.station_id, meta_history.history_id, meta_history.station_name, meta_history.lon, meta_history.lat, meta_history.elev, meta_history.sdate, meta_history.edate, meta_history.tz_offset, meta_history.province, meta_history.country, meta_history.comments, meta_history.the_geom, meta_history.sensor_id, meta_history.freq, meta_station.native_id, meta_station.min_obs_time, meta_station.max_obs_time, meta_network_geoserver.network_name, meta_network_geoserver.col_hex
    FROM meta_history
    NATURAL JOIN meta_station
    NATURAL JOIN meta_network_geoserver
    NATURAL JOIN meta_network
    WHERE meta_station.native_id = ANY (ARRAY['1100120', '1100119', '1101530', '1200560', '1060841', '1160899', '1021261', '1152102', '1192340', '119BLM0', '1032730', '1192940', '1192950', '1092970', '1183000', '1183090', '1173210', '1163780', '1163842', '116C8P0', '1123939', '11239R0', '1123993', '1054500', '1054503', '1114619', '1184791', '1184790', '1025370', '10253G0', '1145M29', '1145442', '1085836', '1125852', '1125865', '112G8L1', '1126150', '120619?', '1026270', '1046392', '1046391', '1096439', '1096453', '1096468', '1066482', '1066483', '1066488', '1096629', '1096631', '1176749', '1057052', '1057050', '1057055', '1077500', '1068130', '1068131', '1038205', '1108447', '1108446', '1168520', '1128582', '1128583', '1128584', '1018620', '1048898', '1108906', '1098940'])
);


