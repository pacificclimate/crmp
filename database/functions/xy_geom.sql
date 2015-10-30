CREATE OR REPLACE FUNCTION public.x(geometry)
  RETURNS double precision AS
'$libdir/postgis-2.0', 'LWGEOM_x_point'
  LANGUAGE c IMMUTABLE STRICT
  COST 1;
ALTER FUNCTION public.x(geometry)
  OWNER TO postgres;
COMMENT ON FUNCTION public.x(geometry) IS 'args: a_point - Return the X coordinate of the point, or NULL if not available. Input must be a point.';
    
CREATE OR REPLACE FUNCTION public.y(geometry)
  RETURNS double precision AS
'$libdir/postgis-2.0', 'LWGEOM_y_point'
  LANGUAGE c IMMUTABLE STRICT
  COST 1;
ALTER FUNCTION public.y(geometry)
  OWNER TO postgres;
COMMENT ON FUNCTION public.st_y(geometry) IS 'args: a_point - Return the Y coordinate of the point, or NULL if not available. Input must be a point.';