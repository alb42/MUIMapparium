unit defimageunit;

{$mode objfpc}{$H+}

interface
uses
  Classes, sysutils, fpreadpng, fpimage, imagesunit;

var
  EmptyBitmap, ErrorBitmap: TFPAMemImage;

implementation

{$include emptypng.inc}
{$include errorpng.inc}

procedure OpenImages();
var
  Reader: TFPReaderPNG;
  Mem: TMemoryStream;
begin
  // Create Pictures
  ErrorBitmap := TFPAMemImage.Create(256, 256);
  EmptyBitmap := TFPAMemImage.Create(256, 256);
  // A steam for our data
  Mem := TMemoryStream.Create;
  Reader := TFPReaderPNG.Create;
  try
    //CopyData to Stream
    Mem.Write(ErrorPng[0], Length(ErrorPNG));
    Mem.Position := 0; // reset position -> loadfrom stream expect pointer on start
    ErrorBitmap.LoadFromStream(Mem, Reader); // read the actual image
    //
    Mem.Clear; // clean for next image
    //
    Mem.Write(EmptyPng[0], Length(EmptyPNG)); // load empty picture
    Mem.Position := 0;                        // reset position
    EmptyBitmap.LoadFromStream(Mem, Reader);  // load picture
  except
    on e:Exception do
      writeln('Exception when opening default PNGs: ', E.message);
  end;
  Mem.Free;
  Reader.Free;
end;


initialization
  OpenImages;
finalization
  ErrorBitmap.Free;
  EmptyBitmap.Free;
end.
