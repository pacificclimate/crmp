-- Run as database superuser to add all users/roles

DROP ROLE drews;
DROP ROLE ssobie;
DROP ROLE acannon;
DROP ROLE fanslow;
DROP ROLE bronaugh;
DROP ROLE hiebert;
DROP ROLE bveerman;

DROP ROLE Steward;
DROP ROLE Inspector;
DROP ROLE Viewer;

-- Base groups
CREATE ROLE Viewer;
CREATE ROLE Inspector IN ROLE Viewer;
CREATE ROLE Steward IN ROLE Inspector;

-- Add person-specific roles to groups
CREATE ROLE bveerman LOGIN IN ROLE Steward;
CREATE ROLE hiebert LOGIN IN ROLE Steward;
CREATE ROLE bronaugh LOGIN IN ROLE Steward;

CREATE ROLE fanslow LOGIN IN ROLE Inspector;

CREATE ROLE acannon LOGIN IN ROLE Viewer;
ALTER ROLE acannon SET search_path="$user", crmp, public;
CREATE ROLE drews LOGIN IN ROLE Viewer;
ALTER ROLE drews SET search_path="$user", crmp, public;
CREATE ROLE ssobie LOGIN IN ROLE Viewer;
ALTER ROLE ssobie SET search_path="$user", crmp, public;
CREATE ROLE tmurdock LOGIN IN ROLE Viewer;
ALTER ROLE tmurdock SET search_path="$user", crmp, public;

-- Add task specific roles
CREATE ROLE httpd LOGIN;
ALTER ROLE httpd SET statement_timeout='45000';
ALTER ROLE httpd SET search_path="$user", crmp, osm, public;

CREATE ROLE httpd_meta LOGIN;
ALTER ROLE httpd_meta SET statement_timeout='45000';
ALTER ROLE httpd_meta SET search_path=pcic_meta, public;

CREATE ROLE httpd_osm LOGIN;
ALTER ROLE httpd_osm SET statement_timeout='45000';
ALTER ROLE httpd_osm SET search_path=osm, public;
    
CREATE ROLE crmp LOGIN;
CREATE ROLE crmprtd LOGIN;
CREATE ROLE pcic_meta LOGIN;
CREATE ROLE redmine LOGIN;
CREATE ROLE osm LOGIN;
