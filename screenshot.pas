unit screenshot;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure CreateScreenshot(aStream: TStream);

implementation

uses
  Graphics, LCLIntf, LCLType, Interfaces, Forms
  ;


Procedure CreateScreenshotBitmap(VAR mypic:Graphics.TBitmap);
{$IFDEF WINDOWS}
 var
  ScreenDC: HDC;
begin
  mypic.Width:=Screen.DesktopWidth;
  mypic.Height:=Screen.DesktopHeight;
  mypic.Canvas.Brush.Color := clWhite;
  mypic.Canvas.FillRect(0, 0, mypic.Width, mypic.Height);
  ScreenDC:=GetDC(0{GetDesktopWindow});
  BitBlt(mypic.Canvas.Handle, 0, 0, mypic.Width, mypic.Height, ScreenDC, Screen.DesktopLeft, Screen.DesktopTop, SRCCOPY);
  ReleaseDC(0, ScreenDC);
end;
{$ENDIF}

{$IFDEF LINUX}
var
  ScreenDC: HDC;
begin
  ScreenDC := GetDC(0);
  Mypic.LoadFromDevice(ScreenDC);
  ReleaseDC(0,ScreenDC);
end;
{$ENDIF}


procedure CreateScreenshot(aStream: TStream);
var
  MyBitmap: TBitmap;
  ScreenDC: HDC;
begin
  ScreenDC := GetDC(0);
  MyBitmap := TBitmap.Create;
  try
    CreateScreenshotBitmap(MyBitmap);
    MyBitmap.LoadFromDevice(ScreenDC);
    MyBitmap.SaveToStream(aStream);
  finally
    MyBitmap.Free;
    ReleaseDC(0,ScreenDC);
  end;
end;

end.

