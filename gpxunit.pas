unit GPXUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  DEFAULTGPX = 'default.gpx';
  GPXTEMPLATE =
    '<?xml version="1.0" encoding="UTF-8" standalone="no" ?>' +
    '  <gpx version="1.1" creator="ALB42">' +
    '  <metadata>' +
    '    <name>default.gpx</name>' +
    '    <desc>Default Waypoints for Mapparium</desc>' +
    '  </metadata>' +
    '</gpx>';
  LISTTEMPLATE =
    '<?xml version="1.0" encoding="UTF-8" standalone="no" ?>' +
    '<list>' +
    '</list>';


implementation




initialization

finalization
end.

