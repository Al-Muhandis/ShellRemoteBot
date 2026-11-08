unit pathrefs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  ;

const
  MaxPathRefs = 1024;    // size of the callback token -> path ring map

type

  { TPathRefs }

  { Maps monotonically growing tokens (0, 1, 2, ...) to full paths. The map
    is a ring: when it outgrows MaxPathRefs the oldest token is evicted, so
    inline buttons of an outdated listing resolve to nothing instead of
    acting on a wrong path }

  TPathRefs = class
  private
    FMap: TStringList;
    FSeq: Integer;       // monotonically growing token counter
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    { Registers APath and returns its fresh token }
    function Add(const APath: String): String;
    { Resolves AToken; returns False when the token is unknown or already
      evicted, in that case APath is empty }
    function Find(const AToken: String; out APath: String): Boolean;
    property Count: Integer read GetCount;
  end;

implementation

{ TPathRefs }

constructor TPathRefs.Create;
begin
  inherited Create;
  FMap:=TStringList.Create;   // token -> path map, unsorted: oldest first
  FSeq:=0;
end;

destructor TPathRefs.Destroy;
begin
  FreeAndNil(FMap);
  inherited Destroy;
end;

function TPathRefs.GetCount: Integer;
begin
  Result:=FMap.Count;
end;

function TPathRefs.Add(const APath: String): String;
begin
  Result:=IntToStr(FSeq);
  Inc(FSeq);
  FMap.Values[Result]:=APath;
  if FMap.Count>MaxPathRefs then
    FMap.Delete(0); // the oldest token has the smallest number
end;

function TPathRefs.Find(const AToken: String; out APath: String): Boolean;
var
  i: Integer;
begin
  i:=FMap.IndexOfName(AToken);
  if i>=0 then
  begin
    APath:=FMap.ValueFromIndex[i];
    Result:=True;
  end
  else
  begin
    APath:=EmptyStr;
    Result:=False;
  end;
end;

end.
