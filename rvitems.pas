unit RVItems;

{$IFDEF FPC}
  {$mode Delphi}
{$ENDIF}

interface

{$I RV_Defs.inc}

uses
  SysUtils, Classes, Controls;

type

  TRVItem = class
  public
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
    ParentItem: TRVItem;
    IsSubItem: Boolean;
    // text item
    StyleNo: Integer;
    JumpId: Integer;
    SameAsPrev: Boolean;
    Alignment: TAlignment;
    //LineNo: Integer;
    TextOffs: Integer;
    FromNewLine: Boolean;
    Text: string;
  end;

  TRVItemList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    { when item deleted, free only items with IsSubItem=True }
    OwnSubitems: Boolean;
    function GetItem(Index: Integer): TRVItem;
    property Items[Index: Integer]: TRVItem read GetItem; default;
    { Remove temporary subitems }
    procedure ClearSubitems();
    { Remove item and following subitems }
    procedure RemoveItem(AItem: TRVItem);
  end;

  {------------------------------------------------------------------}
  { Images, Controls, Drawables }
  TRVVisualItem = class(TRVItem)
  public
    imgNo: Integer; { for rvsJump# used as jump id }
    gr: TPersistent;
    Shared: Boolean;
    DataPtr: Pointer;
  end;

  {------------------------------------------------------------------}
  { Checkpoint }
  TCPInfo = class
  public
    Y: Integer;
    ItemIndex: Integer;
    Text: string;
  end;

  { TCPInfoList }

  TCPInfoList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function GetItem(Index: Integer): TCPInfo;
    property Items[Index: Integer]: TCPInfo read GetItem; default;
  end;
  {------------------------------------------------------------------}
  TJumpInfo = class
  public
    l,t,w,h: Integer;
    id: Integer;
    VisibleItemId: Integer;
    Text: string;
  end;

  { TJumpInfoList }

  TJumpInfoList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    function GetItem(Index: Integer): TJumpInfo;
    property Items[Index: Integer]: TJumpInfo read GetItem; default;
  end;

implementation

{ TJumpInfoList }

function TJumpInfoList.GetItem(Index: Integer): TJumpInfo;
begin
  Result := TJumpInfo(Get(Index));
end;

procedure TJumpInfoList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = lnDeleted then
    TJumpInfo(Ptr).Free();
end;

{ TCPInfoList }

function TCPInfoList.GetItem(Index: Integer): TCPInfo;
begin
  Result := TCPInfo(Get(Index));
end;

procedure TCPInfoList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = lnDeleted then
    TCPInfo(Ptr).Free();
end;

{ TRVItemList }

procedure TRVItemList.ClearSubitems();
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
  begin
    if Items[i].IsSubItem then
      Delete(i);
  end;
end;

function TRVItemList.GetItem(Index: Integer): TRVItem;
begin
  Result := TRVItem(Get(Index));
end;

procedure TRVItemList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if (Action = lnDeleted) and ((not OwnSubitems) or (TRVItem(Ptr).IsSubItem)) then
    TRVItem(Ptr).Free();
end;

procedure TRVItemList.RemoveItem(AItem: TRVItem);
var
  n: Integer;
  IsRemoved: Boolean;
begin
  n := IndexOf(AItem);
  IsRemoved := False;
  while (n > 0) and (n < Count) do
  begin
    if Items[n].IsSubItem then
      Delete(n)
    else
    begin
      if (not IsRemoved) then
      begin
        Delete(n);
        IsRemoved := True;
      end
      else
        Break;
    end;
    Inc(n);
  end;
end;

end.