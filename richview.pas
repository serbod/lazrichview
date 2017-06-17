unit RichView;

{$IFDEF FPC}
  {$mode Delphi}
{$ENDIF}

interface
{$I RV_Defs.inc}
uses
  {$IFDEF FPC}
  RVLazIntf, LCLType, LCLIntf,
  {$ELSE}
  Windows, Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms,
  RVStyle, RVScroll, ClipBrd,
  {$IFDEF RICHVIEWDEF4}
  ImgList,
  {$ENDIF}
  ExtCtrls;
  {------------------------------------------------------------------}

const
  rvsBreak      = -1;
  rvsCheckPoint = -2;
  rvsPicture    = -3;
  rvsHotSpot    = -4;
  rvsComponent  = -5;
  rvsBullet     = -6;
type

  TCustomRichView = class;
  TRVSaveFormat = (rvsfText,
                   rvsfHTML,
                   rvsfRTF, //<---not yet implemented
                   rvsfRVF  //<---not yet implemented
                   );
  TRVSaveOption = (rvsoOverrideImages);
  TRVSaveOptions = set of TRVSaveOption;
  {------------------------------------------------------------------}
  TDrawLineInfo = class
  public
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
    LineNo: Integer;
    Offs: Integer;
    FromNewLine: Boolean;
    Text: string;
  end;

  { TDrawLineInfoList }

  TDrawLineInfoList = class(TList)
  public
    function GetItem(Index: Integer): TDrawLineInfo;
    procedure Delete(Index: Integer);
    property Items[Index: Integer]: TDrawLineInfo read GetItem; default;
  end;
  {------------------------------------------------------------------}
  TLineInfo = class
  public
    StyleNo: Integer;
    SameAsPrev: Boolean;
    Center: Boolean;
    imgNo: Integer; { for rvsJump# used as jump id }
    gr: TPersistent;
    DataPtr: Pointer;
    Text: string;
  end;

  { TLineInfoList }

  TLineInfoList = class(TList)
  public
    function GetItem(Index: Integer): TLineInfo;
    procedure Delete(Index: Integer);
    property Items[Index: Integer]: TLineInfo read GetItem; default;
  end;

  {------------------------------------------------------------------}
  TCPInfo = class
  public
    Y: Integer;
    LineNo: Integer;
    Text: string;
  end;

  { TCPInfoList }

  TCPInfoList = class(TList)
  public
    function GetItem(Index: Integer): TCPInfo;
    procedure Delete(Index: Integer);
    property Items[Index: Integer]: TCPInfo read GetItem; default;
  end;
  {------------------------------------------------------------------}
  TJumpInfo = class
  public
    l,t,w,h: Integer;
    id, idx: Integer;
    Text: string;
  end;

  { TJumpInfoList }

  TJumpInfoList = class(TList)
  public
    function GetItem(Index: Integer): TJumpInfo;
    procedure Delete(Index: Integer);
    property Items[Index: Integer]: TJumpInfo read GetItem; default;
  end;
  {------------------------------------------------------------------}
  TJumpEvent = procedure (Sender: TObject; id: Integer) of object;
  TRVMouseMoveEvent = procedure (Sender: TObject; id: Integer) of object;
  TRVSaveComponentToFileEvent = procedure (Sender: TCustomRichView; Path: String; SaveMe: TPersistent; SaveFormat: TRVSaveFormat; var OutStr:String) of object;
  TRVURLNeededEvent = procedure (Sender: TCustomRichView; id: Integer; var url:String) of object;
  TRVDblClickEvent = procedure (Sender: TCustomRichView; ClickedWord: String; Style: Integer) of object;
  TRVRightClickEvent = procedure (Sender: TCustomRichView; ClickedWord: String; Style, X, Y: Integer) of object;
  {------------------------------------------------------------------}
  TBackgroundStyle = (bsNoBitmap, bsStretched, bsTiled, bsTiledAndScrolled);
  {------------------------------------------------------------------}
  TRVDisplayOption = (rvdoImages, rvdoComponents, rvdoBullets);
  TRVDisplayOptions = set of TRVDisplayOption;
  {------------------------------------------------------------------}
  TScreenAndDevice = record
    ppixScreen: Integer;
    ppiyScreen: Integer;
    ppixDevice: Integer;
    ppiyDevice: Integer;
    LeftMargin: Integer;
  end;
  {------------------------------------------------------------------}
  TRVInteger2 = class
  public
    val: Integer;
  end;

  {------------------------------------------------------------------}

  { TCustomRichView }

  TCustomRichView = class(TRVScroller)
  private
    { Private declarations }
    ScrollDelta: Integer;
    ScrollTimer: TTimer;
    FAllowSelection: Boolean;
    FSingleClick: Boolean;
    FDelimiters: String;
    DrawHover: Boolean;
    Selection: Boolean;
    FOnJump: TJumpEvent;
    FOnRVMouseMove: TRVMouseMoveEvent;
    FOnSaveComponentToFile: TRVSaveComponentToFileEvent;
    FOnURLNeeded: TRVURLNeededEvent;
    FOnRVDblClick: TRVDblClickEvent;
    FOnRVRightClick: TRVRightClickEvent;
    FOnSelect: TNotifyEvent;
    FOnResized: TNotifyEvent;
    FFirstJumpNo: Integer;
    FMaxTextWidth: Integer;
    FMinTextWidth: Integer;
    FLeftMargin: Integer;
    FRightMargin: Integer;
    FBackBitmap: TBitmap;
    FBackgroundStyle: TBackgroundStyle;
    OldWidth: Integer;
    OldHeight: Integer;
    FSelStartNo: Integer;
    FSelEndNo: Integer;
    FSelStartOffs: Integer;
    FSelEndOffs: Integer;
    procedure InvalidateJumpRect(no: Integer);
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    function FindItemAtPos(X,Y: Integer): Integer;
    procedure FindItemForSel(X,Y: Integer; var No, Offs: Integer);
    function GetLineCount: Integer;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure GetSelBounds(out StartNo, EndNo, StartOffs, EndOffs: Integer);
    procedure StoreSelBounds(var StartNo, EndNo, StartOffs, EndOffs: Integer);
    procedure RestoreSelBounds(StartNo, EndNo, StartOffs, EndOffs: Integer);
  protected
    { Protected declarations }
    FLines: TLineInfoList;
    Lines: TLineInfoList;
    DrawLines: TDrawLineInfoList;
    CheckPoints: TCPInfoList;
    Jumps: TJumpInfoList;
    FStyle: TRVStyle;
    nJmps: Integer;

    skipformatting: Boolean;

    TextWidth: Integer;
    TextHeight: Integer;

    LastJumpMovedAbove: Integer;
    LastLineFormatted: Integer;
    LastJumpDowned, XClicked, YClicked, XMouse, YMouse: Integer;

    imgSavePrefix: String;
    imgSaveNo: Integer;
    SaveOptions: TRVSaveOptions;

    ShareContents: Boolean;
    FClientTextWidth: Boolean;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Click(); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;    
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure FormatLine(no: Integer; var x, baseline, prevdesc, prevabove: Integer; Canvas: TCanvas;
                         var sad: TScreenAndDevice);
    procedure AdjustJumpsCoords;
    procedure AdjustChildrenCoords;
    procedure ClearTemporal;
    function GetFirstVisible(TopLine: Integer): Integer;
    function GetFirstLineVisible: Integer;
    function GetLastLineVisible: Integer;
    function GetDrawLineNo(BoundLine: Integer; Option: Integer): Integer;
    procedure Format_(OnlyResized:Boolean; depth: Integer; Canvas: TCanvas; OnlyTail: Boolean);
    procedure SetBackBitmap(Value: TBitmap);
    // draw background
    procedure DrawBack(DC: HDC; Rect: TRect; Width, Height: Integer);
    procedure SetBackgroundStyle(Value: TBackgroundStyle);
    procedure SetVSmallStep(Value: Integer);
    function GetNextFileName(const Path: string): string; virtual;
    procedure ShareLinesFrom(Source: TCustomRichView);
    function FindClickedWord(var sClickedWord: string; var StyleNo: Integer): Boolean;
    procedure OnScrollTimer(Sender: TObject);
    procedure Loaded; override;    
    
  protected // to be published properties
    function GetCredits: string; virtual;
    { Published declarations }
    //property PopupMenu;
    //property OnClick;
    //property OnKeyDown;
    //property OnKeyUp;
    //property OnKeyPress;

    { You can use this property to set base value of hypertext link indices.
      It will allow you use sole handlers of OnJump and OnRVMouseMove events for
      several TRichView controls.}
    property FirstJumpNo: Integer read FFirstJumpNo write FFirstJumpNo;
    { When user clicks at hypertext link. id - index of link }
    property OnJump: TJumpEvent read FOnJump write FOnJump;
    { When mouse pointer moves above control.
      id - index of link (-1 if mouse pointer is not above any link).
      Not sent twice with equal id one after another. }
    property OnRVMouseMove: TRVMouseMoveEvent read FOnRVMouseMove write FOnRVMouseMove;
    property OnSaveComponentToFile: TRVSaveComponentToFileEvent read FOnSaveComponentToFile write FOnSaveComponentToFile;
    property OnURLNeeded: TRVURLNeededEvent read FOnURLNeeded write FOnURLNeeded;
    property OnRVDblClick: TRVDblClickEvent read FOnRVDblClick write FOnRVDblClick;
    property OnRVRightClick: TRVRightClickEvent read FOnRVRightClick write FOnRVRightClick;
    { When selection is made (user releases mouse button, or SelectAll() or Deselect() is called. }
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property OnResized: TNotifyEvent read FOnResized write FOnResized;
    property Style: TRVStyle read FStyle write FStyle;
    { Limitation of text width in pixels. Default value = 0. }
    property MaxTextWidth:Integer read FMaxTextWidth write FMaxTextWidth;
    { Limitation of text width in pixels. Default value = 0. }
    property MinTextWidth:Integer read FMinTextWidth write FMinTextWidth;
    { Margin in pixels. Default value = 5 pixels. }
    property LeftMargin: Integer read FLeftMargin write FLeftMargin;
    { Margin in pixels. Default value = 5 pixels. }
    property RightMargin: Integer read FRightMargin write FRightMargin;
    { Background image appeared according to BackgroundStyle property }
    property BackgroundBitmap: TBitmap read FBackBitmap write SetBackBitmap;
    { Background image style:
      bsNoBitmap - bitmap is ignored
      bsStretched - bitmap is stretched to fit component size and do not scrolled
      bsTiled - bitmap is tiled and do not scrolled
      bsTiledAndScrolled - bitmap is tiled and scrolls with other contents of component }
    property BackgroundStyle: TBackgroundStyle read FBackgroundStyle write SetBackgroundStyle;
    property Delimiters: string read FDelimiters write FDelimiters;
    { Permits or forbids selecting }
    property AllowSelection: Boolean read FAllowSelection write FAllowSelection;
    { If True then OnRVDblClick is triggered on click, not on doubleclick.
      You can use this property to create dialogs like Syntax Highlighting Dialog in Delphi. }
    property SingleClick: Boolean read FSingleClick write FSingleClick;

  public
    { Public declarations }
    DisplayOptions: TRVDisplayOptions;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
    procedure Paint(); override;
    { Add one new line of text (line must not contain CR-LF characters) }
    procedure AddFromNewLine(const s: string; StyleNo: Integer);
    { Append text to the end of last line (line must not contain CR-LF characters) }
    procedure Add(const s: string; StyleNo: Integer);
    { Add one new line of text with center alignment (line must not contain CR-LF characters) }
    procedure AddCenterLine(const s: string; StyleNo: Integer);
    { Add one or more lines. First line appends to the end of previous text }
    procedure AddText(s: string; StyleNo: Integer);
    { Add one or more lines }
    procedure AddTextFromNewLine(s: string; StyleNo: Integer);
    { Adds horizontal line with color of text style rvsNormal }
    procedure AddBreak();
    { Adds invisible label (checkpoint). Method returns index of checkpoint
      (First checkpoint has index 0, second- 1, ...).
      You can get Y coordinate of checkpoint by method GetCheckPointY. }
    function AddCheckPoint(): Integer;
    { Adds new checkpoint with name CpName and returns index of added checkpoint.
      This checkpoint works just as normal one (added by method AddCheckPoint),
      but also defines a section of document. }
    function AddNamedCheckPoint(const CpName: string): Integer;
    { Returns Y coordinate (in pixels) of hypertext link from hypertext link index.
      You can use this method for scrolling. }
    function GetCheckPointY(no: Integer): Integer;
    { Returns Y coordinate (in pixels) of checkpoint from checkpoint index.
      You can use this method for scrolling. }
    function GetJumpPointY(no: Integer): Integer;
    { Adds picture with center alignment. This method DOES NOT COPY picture from
      argument, only makes pointer to it. Memory is released when you call method Clear()
      or when control is destroyed. Do not destroy this picture yourself!
      TRichView control provides flicker-free scrolling of pictures. }
    procedure AddPicture(gr: TGraphic);
    { Adds image-hypertext link. Parameters are the same as in the method AddBullet. }
    procedure AddHotSpot(imgNo: Integer; lst: TImageList; FromNewLine: Boolean);
    { Adds picture imgNo from ImageList at the new line or not. }
    procedure AddBullet(imgNo: Integer; lst: TImageList; FromNewLine: Boolean);
    { Adds ANY visible Delphi control with center (True) or left (False) alignment.
      This method adds element, which is drawed and processed by control itself,
      not by RichView. It works the same way as if it was inserted in any other Delphi control.
      WARNING - These componets will be destroyed when you call method Clear()
      or when TRichView control is destroyed. Do not destroy them yourself! }
    procedure AddControl(ctrl: TControl; center: Boolean); reintroduce;

    function GetMaxPictureWidth(): Integer;
    { Deletes all text, graphic and other objects from TRichView control }
    procedure Clear();
    { Prepares control to display text and graphics. You must call it after
      - you have added text and graphics (main reason)
      - you have changed text styles of linked TRVStyle control
      - you have modified LeftMargin, RightMargin, MaxTextWidth or MinTextWidth properties
      - you have resized controls inserted in this TRichView control
      Method is called automatically when TRichView control is resized. }
    procedure Format();
    { Formats ONLY NEW items added after last calling of Format() or FormatTail() methods.
      The first of these items should be added from new line. This method also scrolls to
      the end of document. This method does not reformat whole document so it
      works quickly. Like Format() method, FormatTail() does not perform repainting.
      You should call Repaint() method of TRichView after it. }
    procedure FormatTail();

    procedure AppendFrom(Source: TCustomRichView);
    function GetLastCP(): Integer;

    function SaveHTML(const FileName, Title, ImagesPrefix: string; Options: TRVSaveOptions): Boolean;
    function SaveText(const FileName: string; LineWidth: Integer): Boolean;

    { Removes section of document starting with checkpoint named CpName
      till the next named checkpoint (if exists) or the end of document.
      Before calling this method TRichView MUST be formatted.
      This method does not perform any formatting. You should call Format() and Refresh()
      methods after it. So this method needs reformatting of whole document and works rather slowly. }
    procedure DeleteSection(const ACheckPointName: string);
    { Removes Count lines starting with FirstLine (from 0).
      Before calling this method TRichView may be formatted or not. In both cases method runs
      correctly. This method does not perform any formatting. You should call Format() and Refresh()
      methods after it. So this method needs reformatting of whole document and works rather slowly. }
    procedure DeleteLines(FirstLine, Count: Integer);

    //use this only inside OnSaveComponentToFile event handler:
    function SavePicture(DocumentSaveFormat: TRVSaveFormat; const Path: string; gr: TGraphic): string; virtual;

    { Copies selection to clipboard as text. If nothing is selected, does nothing. }
    procedure CopyText();
    { Returns selected text as string. You have no needs to
      use this function because you can copy selected text to clipboard by CopyText(). }
    function GetSelText(): String;
    { True if some contents of component selected }
    function SelectionExists(): Boolean;
    { Clears selection. Does not repaint, so you should call Refresh() after it. }
    procedure Deselect();
    { Selects all contents of component. Does not repaint, so you should call Refresh() after it. }
    procedure SelectAll();

    { Number of pixels in 1 MSU. Important: if you change it when TRichView
      component is already displayed, you must call Format and Refresh methods after. }
    property VSmallStep: Integer read SmallStep write SetVSmallStep;
    property LineCount: Integer read GetLineCount;
    property FirstLineVisible: Integer read GetFirstLineVisible;
    property LastLineVisible: Integer read GetLastLineVisible;
  end;
  
  TRichView = class(TCustomRichView)
  published
    // published from TRVScroller
    property Visible;
    property TabStop;
    property TabOrder;
    property Align;
    property HelpContext;
    property Tracking;
    property VScrollVisible;
    property OnVScrolled;
    
    // published from TCustomRichView
    property PopupMenu;
    property OnClick;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property FirstJumpNo;
    property OnJump;
    property OnRVMouseMove;
    property OnSaveComponentToFile;
    property OnURLNeeded;
    property OnRVDblClick;
    property OnRVRightClick;
    property OnSelect;
    property OnResized;
    property Style;
    property MaxTextWidth;
    property MinTextWidth;
    property LeftMargin;
    property RightMargin;
    property BackgroundBitmap;
    property BackgroundStyle;
    property Delimiters;
    property AllowSelection;
    property SingleClick;
  end;

procedure InfoAboutSaD(var sad: TScreenAndDevice; Canvas: TCanvas);

implementation

uses Printers;

{$IFDEF FPC}
{-------------------------------------}
procedure InfoAboutSaD(var sad: TScreenAndDevice; Canvas: TCanvas);
var
  screenDC: HDC;
begin
  if Canvas is TPrinterCanvas then
  begin
    sad.ppixDevice := Printer.XDPI;
    sad.ppiyDevice := Printer.YDPI;
  end
  else
  begin
    sad.ppixDevice := GetDeviceCaps(Canvas.Handle, LOGPIXELSX);
    sad.ppiyDevice := GetDeviceCaps(Canvas.Handle, LOGPIXELSY);
  end;
  screenDc := CreateCompatibleDC(0);
  sad.ppixScreen := GetDeviceCaps(screenDC, LOGPIXELSY);
  sad.ppiyScreen := GetDeviceCaps(screenDC, LOGPIXELSY);
  DeleteDC(screenDC);
end;
{$ELSE}
{-------------------------------------}
procedure InfoAboutSaD(var sad: TScreenAndDevice; Canvas: TCanvas);
var
  screenDC: HDC;
begin
  sad.ppixDevice := GetDeviceCaps(Canvas.Handle, LOGPIXELSX);
  sad.ppiyDevice := GetDeviceCaps(Canvas.Handle, LOGPIXELSY);
  screenDc := CreateCompatibleDC(0);
  sad.ppixScreen := GetDeviceCaps(screenDC, LOGPIXELSX);
  sad.ppiyScreen := GetDeviceCaps(screenDC, LOGPIXELSY);
  DeleteDC(screenDC);
end;
{$ENDIF}

{ TJumpInfoList }

function TJumpInfoList.GetItem(Index: Integer): TJumpInfo;
begin
  Result := TJumpInfo(Get(Index));
end;

procedure TJumpInfoList.Delete(Index: Integer);
begin
  Items[Index].Free();
  inherited Delete(Index);
end;

{ TCPInfoList }

function TCPInfoList.GetItem(Index: Integer): TCPInfo;
begin
  Result := TCPInfo(Get(Index));
end;

procedure TCPInfoList.Delete(Index: Integer);
begin
  Items[Index].Free();
  inherited Delete(Index);
end;

{ TDrawLineInfoList }

function TDrawLineInfoList.GetItem(Index: Integer): TDrawLineInfo;
begin
  Result := TDrawLineInfo(Get(Index));
end;

procedure TDrawLineInfoList.Delete(Index: Integer);
begin
  Items[Index].Free();
  inherited Delete(Index);
end;

{ TLineInfoList }

function TLineInfoList.GetItem(Index: Integer): TLineInfo;
begin
  Result := TLineInfo(Get(Index));
end;

procedure TLineInfoList.Delete(Index: Integer);
begin
  Items[Index].Free();
  inherited Delete(Index);
end;

{==================================================================}

{ TCustomRichView }

constructor TCustomRichView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClientTextWidth := False;
  FLeftMargin    := 5;
  FRightMargin   := 5;
  FMaxTextWidth  := 0;
  FMinTextWidth  := 0;
  TextWidth      := -1;
  TextHeight     := 0;
  LastJumpMovedAbove := -1; 
  FStyle         := nil;
  LastJumpDowned := -1;
  FLines         := TLineInfoList.Create();
  Lines          := FLines;
  DrawLines      := TDrawLineInfoList.Create();
  CheckPoints    := TCPInfoList.Create();
  Jumps          := TJumpInfoList.Create();
  FBackBitmap    := TBitmap.Create();
  FBackGroundStyle := bsNoBitmap;
  nJmps          := 0;
  FirstJumpNo    := 0;
  skipformatting := False;
  OldWidth       := 0;
  OldHeight      := 0;
  Width          := 100;
  Height         := 40;
  DisplayOptions := [rvdoImages, rvdoComponents, rvdoBullets];
  ShareContents  := False;
  FDelimiters    := ' .;,:(){}"';
  DrawHover      := False;
  FSelStartNo    := -1;
  FSelEndNo      := -1;
  FSelStartOffs  := 0;
  FSelEndOffs    := 0;
  Selection      := False;
  FAllowSelection:= True;
  LastLineFormatted := -1;
  ScrollTimer    := nil;
  //Format_(False,0, Canvas, False);
end;
{-------------------------------------}
destructor TCustomRichView.Destroy;
begin
  FreeAndNil(FBackBitmap);
  Clear();
  FreeAndNil(DrawLines);
  FreeAndNil(CheckPoints);
  FreeAndNil(jumps);
  FreeAndNil(FLines);
  inherited Destroy;
end;
{-------------------------------------}
procedure TCustomRichView.WMSize(var Message: TWMSize);
begin
  Format_(True, 0, Canvas, False);
  if Assigned(FOnResized) then FOnResized(Self);
end;
{-------------------------------------}
procedure TCustomRichView.Format;
begin
  Format_(False, 0, Canvas, False);
end;
{-------------------------------------}
procedure TCustomRichView.FormatTail;
begin
  Format_(False, 0, Canvas, True);
end;
{-------------------------------------}
procedure TCustomRichView.ClearTemporal();
begin
  if Assigned(ScrollTimer) then
  begin
    FreeAndNil(ScrollTimer);
  end;

  DrawLines.Clear();

  CheckPoints.Clear();

  Jumps.Clear();
  nJmps :=0;
end;
{-------------------------------------}
procedure TCustomRichView.Deselect();
begin
  Selection := False;
  FSelStartNo := -1;
  FSelEndNo := -1;
  FSelStartOffs := 0;
  FSelEndOffs := 0;
  if Assigned(FOnSelect) then OnSelect(Self);  
end;
{-------------------------------------}
procedure TCustomRichView.SelectAll();
begin
  FSelStartNo := 0;
  FSelEndNo := DrawLines.Count-1;
  FSelStartOffs := 0;
  FSelEndOffs := 0;
  if Lines[DrawLines[FSelEndNo].LineNo].StyleNo >= 0 then
    FSelEndOffs := Length(DrawLines[FSelEndNo].Text) + 1;
  if Assigned(FOnSelect) then OnSelect(Self);
end;
{-------------------------------------}
procedure TCustomRichView.Clear();
var
  i: Integer;
  LineInfo: TLineInfo;
begin
  Deselect();
  if not ShareContents then
  begin
    for i:=0 to Lines.Count-1 do
    begin
      LineInfo := Lines[i];
      if LineInfo.StyleNo = rvsPicture then { image}
      begin
        LineInfo.gr.Free;
        LineInfo.gr := nil;
      end;
      if LineInfo.StyleNo = rvsComponent then {control}
      begin
        RemoveControl(TControl(LineInfo.gr));
        LineInfo.gr.Free();
        LineInfo.gr := nil;
      end;
      //LineInfo.Free();
      //Lines.Objects[i] := nil;
    end;
    Lines.Clear();
  end;
  ClearTemporal();
end;
{-------------------------------------}
procedure TCustomRichView.AddFromNewLine(const s: string; StyleNo: Integer);
var
  LineInfo: TLineInfo;
begin
  LineInfo := TLineInfo.Create();
  LineInfo.StyleNo := StyleNo;
  LineInfo.SameAsPrev := False;
  LineInfo.Center := False;
  LineInfo.Text := s;
  Lines.Add(LineInfo);
end;
{-------------------------------------}
procedure TCustomRichView.Add(const s: string; StyleNo: Integer);
var
  LineInfo: TLineInfo;
begin
  LineInfo := TLineInfo.Create();
  LineInfo.StyleNo := StyleNo;
  LineInfo.SameAsPrev := (Lines.Count <> 0);
  LineInfo.Center := False;
  LineInfo.Text := s;
  Lines.Add(LineInfo);
end;
{-------------------------------------}
procedure TCustomRichView.AddText(s: string; StyleNo: Integer);
var
  p: Integer;
begin
  s := AdjustLineBreaks(s);
  p := Pos(sLineBreak, s);
  if p = 0 then
  begin
    if s <> '' then
      Add(s, StyleNo);
    Exit;
  end;
  Add(Copy(s, 1, p-1), StyleNo);
  Delete(s, 1, p+1);
  while s <> '' do
  begin
    p := Pos(sLineBreak, s);
    if p = 0 then
    begin
      AddFromNewLine(s, StyleNo);
      Break;
    end;
    AddFromNewLine(Copy(s, 1, p-1), StyleNo);
    Delete(s, 1, p+1);
  end;
end;
{-------------------------------------}
procedure TCustomRichView.AddTextFromNewLine(s: string; StyleNo: Integer);
var
  p: Integer;
begin
  s := AdjustLineBreaks(s);
  p := Pos(sLineBreak, s);
  if p=0 then
  begin
   AddFromNewLine(s, StyleNo);
   Exit;
  end;
  while s <> '' do
  begin
   p := Pos(sLineBreak, s);
   if p = 0 then
   begin
     AddFromNewLine(s, StyleNo);
     Break;
   end;
   AddFromNewLine(Copy(s, 1, p-1), StyleNo);
   Delete(s, 1, p+1);
  end;
end;
{-------------------------------------}
procedure TCustomRichView.AddCenterLine(const s: string; StyleNo: Integer);
var
  LineInfo: TLineInfo;
begin
  LineInfo := TLineInfo.Create();
  LineInfo.StyleNo := StyleNo;
  LineInfo.SameAsPrev := False;
  LineInfo.Center := True;
  LineInfo.Text := s;
  Lines.Add(LineInfo);
end;
{-------------------------------------}
procedure TCustomRichView.AddBreak();
var
  LineInfo: TLineInfo;
begin
  LineInfo := TLineInfo.Create();
  LineInfo.StyleNo := rvsBreak;
  LineInfo.Text := '';
  Lines.Add(LineInfo);
end;
{-------------------------------------}
function TCustomRichView.AddNamedCheckPoint(const CpName: String): Integer;
var
  LineInfo: TLineInfo;
  CPInfo: TCPInfo;
begin
  LineInfo := TLineInfo.Create();
  LineInfo.StyleNo := rvsCheckPoint;
  LineInfo.Text := CpName;
  Lines.Add(LineInfo);

  CPInfo := TCPInfo.Create();
  CPInfo.Y := 0;
  CPInfo.Text := CpName;
  CheckPoints.Add(CPInfo);
  AddNamedCheckPoint := CheckPoints.Count-1;
end;
{-------------------------------------}
function TCustomRichView.AddCheckPoint(): Integer;
begin
  AddCheckPoint := AddNamedCheckPoint('');
end;
{-------------------------------------}
function TCustomRichView.GetCheckPointY(no: Integer): Integer;
begin
  GetCheckPointY := CheckPoints[no].Y;
end;
{-------------------------------------}
function TCustomRichView.GetJumpPointY(no: Integer): Integer;
var
  i, n: Integer;
begin
  GetJumpPointY := 0;
  n := no-FirstJumpNo;
  for i:=0 to Jumps.Count-1 do
  begin
    if Jumps[i].id = n then
    begin
      GetJumpPointY := Jumps[i].t;
      Exit;
    end;
  end;
end;
{-------------------------------------}
procedure TCustomRichView.AddPicture(gr: TGraphic); { gr not copied, do not free it!}
var
  LineInfo: TLineInfo;
begin
  LineInfo := TLineInfo.Create();
  LineInfo.StyleNo := rvsPicture;
  LineInfo.gr := gr;
  LineInfo.SameAsPrev := False;
  LineInfo.Center := True;
  LineInfo.Text := '';
  Lines.Add(LineInfo);
end;
{-------------------------------------}
procedure TCustomRichView.AddHotSpot(imgNo: Integer; lst: TImageList;
  FromNewLine: Boolean);
var
  LineInfo: TLineInfo;
begin
  LineInfo := TLineInfo.Create();
  LineInfo.StyleNo := rvsHotSpot;
  LineInfo.gr := lst;
  LineInfo.imgNo := imgNo;
  LineInfo.SameAsPrev := not FromNewLine;
  LineInfo.Text := '';
  Lines.Add(LineInfo);
end;
{-------------------------------------}
procedure TCustomRichView.AddBullet(imgNo: Integer; lst: TImageList;
  FromNewLine: Boolean);
var
  LineInfo: TLineInfo;
begin
  LineInfo := TLineInfo.Create();
  LineInfo.StyleNo := rvsBullet;
  LineInfo.gr := lst;
  LineInfo.imgNo := imgNo;
  LineInfo.SameAsPrev := not FromNewLine;
  LineInfo.Text := '';
  Lines.Add(LineInfo);
end;
{-------------------------------------}
procedure TCustomRichView.AddControl(ctrl: TControl; center: Boolean); { do not free ctrl! }
var
  LineInfo: TLineInfo;
begin
  LineInfo := TLineInfo.Create();
  LineInfo.StyleNo := rvsComponent;
  LineInfo.gr := ctrl;
  LineInfo.SameAsPrev := False;
  LineInfo.Center := center;
  LineInfo.Text := '';
  Lines.Add(LineInfo);
  InsertControl(ctrl);
end;
{-------------------------------------}
function TCustomRichView.GetMaxPictureWidth(): Integer;
var
  i: Integer;
  LineInfo: TLineInfo;
begin
  Result := 0;
  for i := 0 to lines.Count-1 do
  begin
   LineInfo := Lines[i];
   if LineInfo.StyleNo = rvsPicture then
   begin
     if Result < TGraphic(LineInfo.gr).Width then
       Result := TGraphic(LineInfo.gr).Width;
   end;
   if LineInfo.StyleNo = rvsComponent then
   begin
     if Result < TControl(LineInfo.gr).Width then
       Result := TControl(LineInfo.gr).Width;
   end;
 end;
end;
{-------------------------------------}
function max(a, b: Integer): Integer;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;
{-------------------------------------}
procedure TCustomRichView.Format_(OnlyResized: Boolean; depth: Integer; Canvas: TCanvas;
          OnlyTail: Boolean);
var
  i: Integer;
  x, b, d, a: Integer;
  mx: Integer;
  oldy, oldtextwidth, cw, ch: Integer;
  sad: TScreenAndDevice;
  StyleNo: Integer;
  StartLine: Integer;
  StartNo, EndNo, StartOffs, EndOffs: Integer;
begin
  if (smallstep = 0)
  or (csDesigning in ComponentState)
  or (not Assigned(FStyle))
  or skipformatting
  or (depth > 1)
  then
    Exit;
  skipformatting := True;

  if depth = 0 then
  begin
    StartNo := 0;
    EndNo := 0;
    StartOffs := 0;
    EndOffs := 0;
    StoreSelBounds(StartNo, EndNo, StartOffs, EndOffs);
  end;

  OldY := VPos * SmallStep;

  oldtextwidth := TextWidth;

  mx := max(ClientWidth-(FLeftMargin+FRightMargin), GetMaxPictureWidth);
  if mx < FMinTextWidth then
    mx := FMinTextWidth;
  if FClientTextWidth then
  begin { widths of pictures and maxtextwidth are ignored }
    TextWidth := ClientWidth-(FLeftMargin+FRightMargin);
    if TextWidth < FMinTextWidth then
      TextWidth := FMinTextWidth;
  end
  else
  begin
    if (mx > FMaxTextWidth) and (FMaxTextWidth > 0) then
      TextWidth := FMaxTextWidth
    else
      TextWidth := mx;
  end;
  if not (OnlyResized and (TextWidth = OldTextWidth)) then
  begin
    if OnlyTail then
    begin
      StartLine := LastLineFormatted+1;
      b := TextHeight;
    end
    else
    begin
      StartLine := 0;
      b := 0;
      ClearTemporal();
    end;

    x := 0;
    d := 0;
    a := 0;
    sad.LeftMargin := 0;
    InfoAboutSaD(sad, Canvas);
    sad.LeftMargin := MulDiv(FLeftMargin, sad.ppixDevice, sad.ppixScreen);
    for i := StartLine to lines.Count-1 do
    begin
      StyleNo := Lines[i].StyleNo;
      if not (((StyleNo = rvsPicture) and (not (rvdoImages in DisplayOptions)))
      or ((StyleNo = rvsComponent) and (not (rvdoComponents in DisplayOptions)))
      or (((StyleNo = rvsBullet) or (StyleNo = rvsHotspot)) and (not (rvdoBullets in DisplayOptions))))
      then
        FormatLine(i, x, b, d, a, Canvas, sad);
    end;
    TextHeight := b + d + 1;
    if TextHeight div SmallStep > 30000 then
      SmallStep := TextHeight div 30000;
    AdjustJumpsCoords();
  end
  else
    AdjustChildrenCoords();

  HPos := 0;
  VPos := 0;
  cw := ClientWidth;
  ch := ClientHeight;
  UpdateScrollBars(mx + FLeftMargin + FRightMargin, TextHeight div SmallStep);
  if (cw <> ClientWidth) or (ch <> ClientHeight) then
  begin
    skipformatting := False;
    ScrollTo(OldY);
    Format_(OnlyResized, depth+1, Canvas, False);
  end;
  if OnlyResized then
    ScrollTo(OldY);
  if OnlyTail then
    ScrollTo(TextHeight);
  if depth=0 then
    RestoreSelBounds(StartNo, EndNo, StartOffs, EndOffs);

  skipformatting := False;
  LastLineFormatted := Lines.Count-1;
end;
{-------------------------------------}
procedure TCustomRichView.AdjustChildrenCoords();
var
  i: Integer;
  dli: TDrawLineInfo;
  li: TLineInfo;
begin
  for i := 0 to DrawLines.Count-1 do
  begin
    dli := DrawLines[i];
    li := Lines[dli.LineNo];
    if li.StyleNo = rvsComponent then {control}
    begin
      TControl(li.gr).Left := dli.Left;
      TControl(li.gr).Tag := dli.Top;
      Tag2Y(TControl(li.gr));
    end;
  end;
end;
{-------------------------------------}
procedure TCustomRichView.FormatLine(no: Integer; var x, baseline, prevdesc, prevabove: Integer;
          Canvas: TCanvas; var sad: TScreenAndDevice);
var sourceStrPtr, strForAdd, strSpacePos: PChar;
    sourceStrPtrLen: Integer;
    sz: TSIZE;
    max, j, y, ctrlw, ctrlh : Integer;
{$IFNDEF RICHVIEWDEF4}
    arr: array[0..1000] of integer;
{$ENDIF}
    char_arr: array[0..1000] of char;
    LineInfo: TLineInfo;
    DLInfo: TDrawLineInfo;
    TextMetr: TTextMetric;
    StyleNo: Integer;
    newline, center:Boolean;
    CPInfo: TCPInfo;
    JmpInfo: TJumpInfo;
    width, y5, Offs : Integer;
begin
  width := TextWidth;
  LineInfo := Lines[no];
  case LineInfo.StyleNo of
    rvsComponent: { Control }
    begin
      ctrlw := TControl(LineInfo.gr).Width;
      ctrlh := TControl(LineInfo.gr).Height;
      ctrlw := MulDiv(ctrlw, sad.ppixDevice, sad.ppixScreen);
      ctrlh := MulDiv(ctrlh, sad.ppiyDevice, sad.ppiyScreen);

      DLInfo        := TDrawLineInfo.Create();
      DLInfo.LineNo := no;
      DLInfo.Top    := baseline + prevdesc + 1;
      DLInfo.Width  := ctrlw;
      DLInfo.Height := ctrlh+1;
      if LineInfo.Center then
      begin
        DLInfo.Left   := (width-ctrlw) div 2;
        if DLInfo.Left < 0 then
          DLInfo.Left := 0;
        Inc(DLInfo.Left, sad.LeftMargin);
      end
      else
      begin
        DLInfo.Left := sad.LeftMargin;
      end;
      DLInfo.Text := '';
      DrawLines.Add(DLInfo);
      TControl(LineInfo.gr).Left := DLInfo.Left;
      TControl(LineInfo.gr).Tag := DLInfo.Top;
      Tag2Y(TControl(LineInfo.gr));
      Inc(baseline, prevdesc + ctrlh + 1);
      prevdesc := 1;
      prevabove := ctrlh + 1;
    end;

    rvsHotSpot, rvsBullet:
    begin
      ctrlw := TImageList(LineInfo.gr).Width;
      ctrlh := TImageList(LineInfo.gr).Height;
      ctrlw := MulDiv(ctrlw, sad.ppixDevice, sad.ppixScreen);
      ctrlh := MulDiv(ctrlh, sad.ppiyDevice, sad.ppiyScreen);
      DLInfo := TDrawLineInfo.Create();
      DLInfo.Width  := ctrlw+1;
      DLInfo.Height := ctrlh+1;
      if (not LineInfo.SameAsPrev) or (x + ctrlw + 2 > width) then
      begin
        x := 0;
        y := baseline + prevdesc;
        Inc(baseline, prevdesc + ctrlh + 1);
        prevdesc := 1;
        prevabove := ctrlh + 1;
      end
      else
      begin
        if prevabove < ctrlh + 1 then
        begin
          j := DrawLines.Count-1;
          if j >= 0 then
          begin
            repeat
              Inc(DrawLines[j].Top, ctrlh+1-prevabove);
              Dec(j);
            until DrawLines[j+1].FromNewLine;
          end;
          Inc(baseline, ctrlh + 1 - prevabove);
          prevabove := ctrlh + 1;
          y := baseline - (ctrlh + 1);
        end;
      end;
      if LineInfo.StyleNo = rvsHotSpot then
      begin
        JmpInfo     := TJumpInfo.Create();
        JmpInfo.l   := x + 1 + sad.LeftMargin;;
        JmpInfo.t   := y + 1;
        JmpInfo.w   := ctrlw;
        JmpInfo.h   := ctrlh;
        JmpInfo.id  := nJmps;
        JmpInfo.idx := DrawLines.Count;
        JmpInfo.Text:= '';
        Jumps.Add(JmpInfo);
        Inc(nJmps);
      end;
      DLInfo.Left := x + 1 + sad.LeftMargin;;
      Inc(x, ctrlw+2);
      DLInfo.Top := y + 1;
      DLInfo.LineNo := no;
      DLInfo.FromNewLine := not LineInfo.SameAsPrev;
      DLInfo.Text := '';
      DrawLines.Add(DLInfo);
    end;

    rvsPicture:  { graphics}
    begin
      ctrlw       := TGraphic(LineInfo.gr).Width;
      ctrlh       := TGraphic(LineInfo.gr).Height;
      ctrlw       := MulDiv(ctrlw, sad.ppixDevice, sad.ppixScreen);
      ctrlh       := MulDiv(ctrlh, sad.ppiyDevice, sad.ppiyScreen);

      DLInfo        := TDrawLineInfo.Create();
      DLInfo.Width  := ctrlw;
      DLInfo.Height := ctrlh+1;
      DLInfo.Left   := (width-ctrlw) div 2;
      if DLInfo.Left < 0 then
        DLInfo.Left := 0;
      Inc(DLInfo.Left, sad.LeftMargin);
      DLInfo.Top    := baseline + prevdesc + 1;
      DLInfo.LineNo := no;
      DLInfo.Text := '';
      DrawLines.Add(DLInfo);
      Inc(baseline, prevdesc + ctrlh + 1);
      prevdesc    := 1;
      prevabove   := ctrlh+1;
    end;

    rvsCheckPoint: { check point}
    begin
      CPInfo   := TCPInfo.Create();
      CPInfo.Y := baseline + prevDesc;
      CPInfo.LineNo := no;
      CPInfo.Text := LineInfo.Text;
      CheckPoints.Add(CPInfo);
    end;

    rvsBreak: { break line}
    begin
      y5            := MulDiv(5, sad.ppiyDevice, sad.ppiyScreen);
      DLInfo        := TDrawLineInfo.Create();
      DLInfo.Left   := sad.LeftMargin;
      DLInfo.Top    := baseline + prevdesc;
      DLInfo.LineNo := no;
      DLInfo.Width  := Width;
      DLInfo.Height := y5 + y5 + 1;
      DLInfo.Text := LineInfo.Text;
      DrawLines.Add(DLInfo);
      Inc(baseline, prevdesc + y5 + y5 + 1);
      prevdesc  := y5;
      prevabove := y5;
    end;

  else
    begin
      sourceStrPtr := PChar(LineInfo.Text);
      char_arr[1] := #0; // eliminate warning
      strForAdd := char_arr;
      sourceStrPtrLen := StrLen(sourceStrPtr);

      StyleNo := LineInfo.StyleNo;
      with FStyle.TextStyles[StyleNo] do
      begin
        Canvas.Font.Style := Style;
        Canvas.Font.Size  := Size;
        Canvas.Font.Name  := FontName;
        {$IFDEF RICHVIEWDEF3}
        Canvas.Font.CharSet  := CharSet;
        {$ENDIF}
      end;
      TextMetr.tmAscent := 0;
      GetTextMetrics(Canvas.Handle, TextMetr);
      newline := not LineInfo.SameAsPrev;
      Center := LineInfo.Center;
      while sourceStrPtrLen > 0 do
      begin
        if newline then
          x := 0;
        sz.cx := 0;
        sz.cy := 0;
        {$IFDEF FPC}
        MyGetTextExtentExPoint(Canvas.Handle,  sourceStrPtr,  sourceStrPtrLen, Width-x,
        {$ELSE}
        GetTextExtentExPoint(Canvas.Handle,  sourceStrPtr,  sourceStrPtrLen, Width-x,
        {$ENDIF}
                            {$IFDEF RICHVIEWDEF4}
                            @max, nil,
                            {$ELSE}
                            max, arr[0],
                            {$ENDIF}
                            sz);

        if max = 0 then
          max := 1;
        StrLCopy(strForAdd, sourceStrPtr, max);
        if max < sourceStrPtrLen then
        begin
          {if  sourceStrPtr[max] <> ' ' then }
          StrLCopy(strForAdd, sourceStrPtr, max);
          strSpacePos := StrRScan(strForAdd, ' ');
          if strSpacePos <> nil then
          begin
            max := strSpacePos - strForAdd;
            StrLCopy(strForAdd, sourceStrPtr,max);
            Inc(max);
          end
          else
          begin
            if not newline then
            begin
              x := 0;
              newline := true;
              Continue;
            end;
          end;
        end;
        Offs := sourceStrPtr - PChar(LineInfo.Text) + 1;
        sourceStrPtr := @(sourceStrPtr[max]);
        DLInfo := TDrawLineInfo.Create();
        DLInfo.LineNo := no;
        DLInfo.Offs := Offs;
        {$IFDEF FPC}
        MyGetTextExtentExPoint(Canvas.Handle,  strForAdd,  StrLen(strForAdd), Width-x,
        {$ELSE}
        GetTextExtentExPoint(Canvas.Handle,  strForAdd,  StrLen(strForAdd), Width-x,
        {$ENDIF}
           {$IFDEF RICHVIEWDEF4}
           @max, nil,
           {$ELSE}
           max,arr[0],
           {$ENDIF}
           sz);
        if not newline then
        begin
          {continue line}
          if prevabove < TextMetr.tmExternalLeading+TextMetr.tmAscent then
          begin
            j := DrawLines.Count-1;
            if j >= 0 then
            begin
              repeat
                Inc(DrawLines[j].Top, TextMetr.tmExternalLeading + TextMetr.tmAscent - prevabove);
                Dec(j);
              until DrawLines[j+1].FromNewLine or (j < 0);
            end;
            Inc(baseline, TextMetr.tmExternalLeading + TextMetr.tmAscent - prevabove);
            prevabove := TextMetr.tmExternalLeading + TextMetr.tmAscent;
          end;
          y := baseline - TextMetr.tmAscent;
          DLInfo.FromNewLine := False;
        end
        else
        begin
          { new line }
          DLInfo.FromNewLine := True;
          if Center then
            x := (Width - sz.cx) div 2
          else
            x :=0;
          y := baseline + prevDesc + TextMetr.tmExternalLeading;
          Inc(baseline, prevDesc + TextMetr.tmExternalLeading + TextMetr.tmAscent);
          prevabove := TextMetr.tmExternalLeading + TextMetr.tmAscent;
        end;
        DLInfo.Left   := x + sad.LeftMargin;
        DLInfo.Top    := y;
        DLInfo.Width  := sz.cx;
        DLInfo.Height := sz.cy;
        DLInfo.Text := strForAdd;
        DrawLines.Add(DLInfo);
        if (StyleNo = rvsJump1) or (StyleNo = rvsJump2) then
        begin
          JmpInfo := TJumpInfo.Create();
          JmpInfo.l := x + sad.LeftMargin;
          JmpInfo.t := y;
          JmpInfo.w := sz.cx;
          JmpInfo.h := sz.cy;
          JmpInfo.id := nJmps;
          JmpInfo.idx := DrawLines.Count-1;
          JmpInfo.Text := '';
          LineInfo.imgNo := nJmps;
          Jumps.Add(JmpInfo);
        end;
        sourceStrPtrLen := StrLen(sourceStrPtr);
        if newline or (prevDesc < TextMetr.tmDescent) then
          prevDesc := TextMetr.tmDescent;
        Inc(x, sz.cx);
        newline := True;
      end; {while sourceStrPtrLen > 0 do}
      if (StyleNo = rvsJump1) or (StyleNo = rvsJump2) then
        Inc(nJmps);
    end;
  end;
end;
{-------------------------------------}
procedure TCustomRichView.AdjustJumpsCoords();
var
  i: Integer;
  JumpInfo: TJumpInfo;
begin
  for i:=0 to jumps.Count-1 do
  begin
    JumpInfo := Jumps[i];
    JumpInfo.l := DrawLines[JumpInfo.idx].left;
    JumpInfo.t := DrawLines[JumpInfo.idx].top;
  end;
end;
{-------------------------------------}
const gdlnFirstVisible = 1;
const gdlnLastCompleteVisible = 2;
const gdlnLastVisible = 3;
{-------------------------------------}
function TCustomRichView.GetFirstVisible(TopLine: Integer): Integer;
begin
   Result := GetDrawLineNo(TopLine, gdlnFirstVisible);
end;
{-------------------------------------}
function TCustomRichView.GetFirstLineVisible(): Integer;
var
  v: Integer;
begin
  v := GetDrawLineNo(VPos*SmallStep, gdlnFirstVisible);
  if v >= DrawLines.Count then
    v := DrawLines.Count-1;
  if v < 0 then
    Result := -1
  else
    Result := DrawLines[v].LineNo;
end;
{-------------------------------------}
function TCustomRichView.GetLastLineVisible(): Integer;
var
  v: Integer;
begin
  v := GetDrawLineNo(VPos*SmallStep + ClientHeight, gdlnLastVisible);
  if v >= DrawLines.Count then
    v := DrawLines.Count-1;
  if v < 0 then
    Result := -1
  else
    Result := DrawLines[v].LineNo;
end;
{-------------------------------------}
function TCustomRichView.GetDrawLineNo(BoundLine: Integer; Option: Integer): Integer;
var
  a, b, mid: Integer;
begin
  if DrawLines.Count = 0 then
  begin
    Result := 0;
    Exit;
  end;
  if DrawLines[0].Top >= BoundLine then
  begin
    Result := 0;
    Exit;
  end;
  if (Option = gdlnLastVisible) and (DrawLines[DrawLines.Count-1].Top < BoundLine) then
  begin
    Result := DrawLines.Count-1;
    Exit;
  end;
  a := 1;
  b := DrawLines.Count-1;
  mid := a;
  if Option = gdlnLastCompleteVisible then
  begin
  {
    while (b-a) > 1 do
    begin
      mid := (a+b) div 2;
      if (TDrawLineInfo(DrawLines.Objects[mid]).Top + TDrawLineInfo(DrawLines.Objects[mid]).Height > BoundLine) then
        b := mid
      else
        a := mid;
    end;
    if mid >= DrawLines.Count then
      mid := DrawLines.Count-1;
    while (mid > 0) and (TDrawLineInfo(DrawLines.Objects[mid]).Top + TDrawLineInfo(DrawLines.Objects[mid]).Height>BoundLine) do
      Dec(mid);

      if (mid > 0) then
        Dec(mid);
      while (mid > 0) and (not TDrawLineInfo(DrawLines.Objects[mid]).FromNewLine) do
        Dec(mid);
      if (mid > 0) then
        Dec(mid);
    end
  }
  end
  else
  begin
    while (b-a) > 1 do
    begin
      mid := (a+b) div 2;
      if (DrawLines[mid].Top >= BoundLine) then
      begin
        if (DrawLines[mid-1].Top < BoundLine) then
          Break;
        b := mid;
      end
      else
        a := mid;
    end;
    if mid >= DrawLines.Count then
      mid := DrawLines.Count-1;
    if Option = gdlnFirstVisible then
    begin
      while (mid > 0) and (not DrawLines[mid].FromNewLine) do
        Dec(mid);
      if (mid > 0) then
        Dec(mid);
      while (mid > 0) and (not DrawLines[mid].FromNewLine) do
        Dec(mid);
      if (mid > 0) then
        Dec(mid);
    end
    else
    begin
      while DrawLines[mid].Top < BoundLine do
        Inc(mid);
    end;
  end;
  Result := mid;
end;
{
function TCustomRichView.GetFirstVisible(TopLine: Integer): Integer;
var
  a, b, mid: Integer;
begin
  if DrawLines.Count = 0 then
  begin
    Result := 0;
    Exit;
  end;
  if TDrawLineInfo(DrawLines.Objects[0]).Top >= TopLine then
  begin
    Result := 0;
    Exit;
  end;
  a := 1;
  b := DrawLines.Count-1;
  mid := a;
  while (b-a) > 1 do
  begin
    mid := (a+b) div 2;
    if (TDrawLineInfo(DrawLines.Objects[mid]).Top >= TopLine) then
    begin
      if (TDrawLineInfo(DrawLines.Objects[mid-1]).Top < TopLine) then
        Break;
      b := mid;
    end
    else
      a := mid;
  end;
  Dec(mid);
  while (mid >= 2) and (TDrawLineInfo(DrawLines.Objects[mid]).Left > TDrawLineInfo(DrawLines.Objects[mid-1]).Left) do
    Dec(mid);
  if mid = 0 then
  begin
    Result := mid;
    Exit;
  end;
  Dec(mid);
  while (mid >= 1) and (TDrawLineInfo(DrawLines.Objects[mid]).Left > TDrawLineInfo(DrawLines.Objects[mid-1]).Left) do
    Dec(mid);
  GetFirstVisible := mid;
end;
}
{$IFDEF FPC}
procedure TxtOut(Canvas: Tcanvas; X,Y: Integer; const Text: String);
var
  Sz: TSize;
  R: TRect;
  ts: TTextStyle;
begin
  Sz := Canvas.TextExtent(Text);
  R := Bounds(X, Y, Sz.cx, Sz.cy);
  ts := Canvas.TextStyle;
  ts.Opaque := Canvas.Brush.Style <> bsClear;
  Canvas.TextRect(R, R.Left, R.Top, Text, ts);
end;
{$ENDIF}

{-------------------------------------}
procedure TCustomRichView.Paint();
var
  i, no, yshift, xshift: Integer;
  cl, textcolor: TColor;
  dli: TDrawLineInfo;
  li: TLineInfo;
  IsLastLine, IsHoverNow: Boolean;
  r: TRect;
  buffer: TBitmap;
  canv: TCanvas;
  s, s1: String;
  StartNo, EndNo, StartOffs, EndOffs: Integer;
  {$IFDEF FPC}
  St: string;
  {$ENDIF}
begin
  if (csDesigning in ComponentState) or not Assigned(FStyle) then
  begin
    cl := Canvas.Brush.Color;
    if Assigned(FStyle) then
      Canvas.Brush.Color := FStyle.Color
    else
      Canvas.Brush.Color := clWindow;
    Canvas.Brush.Style := bsSolid;
    Canvas.Pen.Color := clWindowText;
    Canvas.Font.Color := clWindowText;
    Canvas.Font.Name := 'MS Sans Serif';
    Canvas.Font.Size := 8;
    Canvas.Font.Style := [];
    Canvas.FillRect(Canvas.ClipRect);
    if (csDesigning in ComponentState) then
      Canvas.TextOut(ClientRect.Left+1, ClientRect.Top+1, GetCredits)
    else
      Canvas.TextOut(ClientRect.Left+1, ClientRect.Top+1, 'Error: style is not assigned');
    Canvas.Brush.Color := clWindowText;
    Canvas.FrameRect(ClientRect);
    Canvas.Brush.Color := cl;
    Exit;
  end;

  GetSelBounds(StartNo, EndNo, StartOffs, EndOffs);
  IsLastLine := False;
  r := Canvas.ClipRect;
  buffer := TBitmap.Create();
  try
    buffer.Width := r.Right-r.Left+1;
    buffer.Height := r.Bottom-r.Top+1;
    canv := buffer.Canvas;
    DrawBack(canv.Handle, Canvas.ClipRect, ClientWidth, ClientHeight);
    yshift := VPos * SmallStep;
    Inc(r.Top, yshift);
    Inc(r.Bottom, yshift);
    Inc(yshift, Canvas.ClipRect.Top);
    xshift := HPos + Canvas.ClipRect.Left;
    canv.Brush.Style := bsClear;

    for i := GetFirstVisible(r.Top) to DrawLines.Count-1 do
    begin
      dli := DrawLines[i];
      if IsLastLine and (dli.Left <= DrawLines[i-1].left) then
        Break;
      if dli.Top > r.Bottom then
        IsLastLine := True;

      li := Lines[dli.LineNo];
      no := li.StyleNo;

      if no >= 0 then
      begin { text }
        canv.Font.Style := FStyle.TextStyles[no].Style;
        canv.Font.Size := FStyle.TextStyles[no].Size;
        canv.Font.Name := FStyle.TextStyles[no].FontName;
        {$IFDEF RICHVIEWDEF3}
        canv.Font.CharSet := FStyle.TextStyles[no].CharSet;
        {$ENDIF}

        if not ((no in [rvsJump1, rvsJump2]) and DrawHover and (LastJumpMovedAbove <> -1) and (li.ImgNo = LastJumpMovedAbove)) then
        begin
          textcolor := FStyle.TextStyles[no].Color;
          IsHoverNow := False;
        end
        else
        begin
          textcolor := FStyle.HoverColor;
          IsHoverNow := True;
          canv.Font.Color := textcolor;
        end;

        if (StartNo > i) or (EndNo < i) then
        begin
          canv.Font.Color := textcolor;
          canv.TextOut(dli.Left-xshift, dli.Top-yshift, dli.Text);
        end
        else
        if ((StartNo < i) and (EndNo > i))
        or ((StartNo = i) and (EndNo <> i) and (StartOffs <= 1))
        or ((StartNo <> i) and (EndNo = i) and (EndOffs > Length(DrawLines[i].Text)))
        then
        begin
          canv.Brush.Style := bsSolid;
          canv.Brush.Color := FStyle.SelColor;
          if not IsHoverNow then
            canv.Font.Color := FStyle.SelTextColor;
          {$IFDEF FPC}
          TxtOut(canv, dli.Left-xshift, dli.Top-yshift, dli.Text);
          {$ELSE}
          canv.TextOut(dli.Left-xshift, dli.Top-yshift, dli.Text);
          {$ENDIF}
          canv.Brush.Style := bsClear;
        end
        else
        if (StartNo = i) then
        begin
          canv.Font.Color := textcolor;
          s := Copy(dli.Text, 1, StartOffs-1);
          canv.TextOut(dli.Left-xshift, dli.Top-yshift, s);
          canv.Brush.Style := bsSolid;
          canv.Brush.Color := FStyle.SelColor;
          if not IsHoverNow then
            canv.Font.Color := FStyle.SelTextColor;
          if (i <> EndNo) or (EndOffs > Length(dli.Text)) then
          begin
            {$IFDEF FPC}
            St := Copy(dli.Text, StartOffs, Length(dli.Text));
            TxtOut(canv, dli.Left-xshift + canv.TextWidth(s), dli.Top-yshift, st);
            {$ELSE}
            canv.TextOut(dli.Left-xshift + canv.TextWidth(s), dli.Top-yshift,
                         Copy(dli.Text, StartOffs, Length(dli.Text)));
            {$ENDIF}
            canv.Brush.Style := bsClear;
          end
          else
          begin
            s1 := Copy(dli.Text, StartOffs, EndOffs-StartOffs);
            {$IFDEF FPC}
            TxtOut(canv, dli.Left - xshift + canv.TextWidth(s), dli.Top-yshift, s1);
            {$ELSE}
            canv.TextOut(dli.Left - xshift + canv.TextWidth(s), dli.Top-yshift, s1);
            {$ENDIF}
            canv.Font.Color := textcolor;
            canv.Brush.Style := bsClear;
            canv.TextOut(dli.Left - xshift + canv.TextWidth(s+s1), dli.Top-yshift,
                         Copy(dli.Text, EndOffs, Length(dli.Text)));
          end;
        end
        else
        if (EndNo = i) then
        begin
          s := Copy(dli.Text, 1, EndOffs-1);
          canv.Brush.Style := bsSolid;
          canv.Brush.Color := FStyle.SelColor;
          if (not IsHoverNow) then
            canv.Font.Color := FStyle.SelTextColor;
          {$IFDEF FPC}
          TxtOut(canv, dli.Left - xshift, dli.Top - yshift, s);
          {$ELSE}
          canv.TextOut(dli.Left - xshift, dli.Top - yshift, s);
          {$ENDIF}
          canv.Brush.Style := bsClear;
          canv.Font.Color := textcolor;
          canv.TextOut(dli.Left - xshift + canv.TextWidth(s), dli.Top - yshift,
                       Copy(dli.Text, EndOffs, Length(dli.Text)));
        end;
        Continue;
      end;

      if (no = rvsPicture)  then
      begin { graphics }
        canv.Draw(dli.Left-xshift, dli.Top-yshift, TGraphic(li.gr));
        Continue;
      end;

      if (no = rvsHotSpot) or (no = rvsBullet)  then
      begin { hotspots and bullets }
        if (StartNo <= i) and (EndNo >= i)
        and (not ((EndNo = i) and (EndOffs = 0)))
        and  (not ((StartNo = i) and (StartOffs = 2)))
        then
        begin
          TImageList(li.gr).BlendColor := FStyle.SelColor;
          TImageList(li.gr).DrawingStyle := dsSelected;
        end;
        TImageList(li.gr).Draw(canv, dli.Left-xshift, dli.Top-yshift, li.imgNo);
        TImageList(li.gr).DrawingStyle := ImgList.dsNormal;
        Continue;
      end;

      if no = rvsCheckPoint then
        Continue; { check point }

      if no = rvsBreak then
      begin {break line}
        canv.Pen.Color := FStyle.TextStyles[0].Color;
        canv.MoveTo(dli.Left + 5 - xshift, dli.Top + 5 - yshift);
        canv.LineTo(XSize - 5 - xshift - FRightMargin, dli.Top + 5 - yshift);
      end;
      { controls ignored }
    end;
    Canvas.Draw(Canvas.ClipRect.Left, Canvas.ClipRect.Top, buffer);
  finally
    buffer.Free();
  end;
end;
{------------------------------------------------------------------}
procedure TCustomRichView.InvalidateJumpRect(no: Integer);
var
  r: TRect;
  i, id : Integer;
  JumpInfo: TJumpInfo;
begin
  if Style.FullRedraw then
    Invalidate()
  else
  begin
    id := no;
    for i := 0 to Jumps.Count - 1 do
    begin
      JumpInfo := Jumps[i];
      if id = JumpInfo.id then
      begin
        r.Left := JumpInfo.l - Hpos - 5;
        r.Top  := JumpInfo.t - VPos * SmallStep - 5;
        r.Right := JumpInfo.l + JumpInfo.w - Hpos + 5;
        r.Bottom := JumpInfo.t + JumpInfo.h - VPos * SmallStep + 5;
        InvalidateRect(Handle, @r, False);
      end;
    end;
  end;
  Update();
end;
{------------------------------------------------------------------}
procedure TCustomRichView.CMMouseLeave(var Message: TMessage);
begin
  if DrawHover and (LastJumpMovedAbove <> -1) then
  begin
    DrawHover := False;
    InvalidateJumpRect(LastJumpMovedAbove);
  end;

  if Assigned(FOnRVMouseMove) and (LastJumpMovedAbove <> -1) then
  begin
    LastJumpMovedAbove := -1;
    OnRVMouseMove(Self, -1);
  end;
end;
{------------------------------------------------------------------}
procedure TCustomRichView.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i, no, offs, ys: Integer;
  JumpInfo: TJumpInfo;
begin
  ScrollDelta := 0;
  if Y < 0 then
    ScrollDelta := -1;
  if Y < -20 then
    ScrollDelta := -10;
  if Y > ClientHeight then
    ScrollDelta := 1;
  if Y > (ClientHeight + 20) then
    ScrollDelta := 10;

  inherited MouseMove(Shift, X, Y);

  if Selection then
  begin
    XMouse := x;
    YMouse := y;
    ys := y;
    if ys < 0 then
      y := 0;
    if ys > ClientHeight then
      ys := ClientHeight;
    no := 0;
    offs := 0;
    FindItemForSel(X + HPos, ys + VPos * SmallStep, no, offs);
    FSelEndNo := no;
    FselEndOffs := offs;
    Invalidate();
  end;

  for i := 0 to jumps.Count-1 do
  begin
    JumpInfo := Jumps[i];
    if  (X >= JumpInfo.l - HPos)
    and (X <= JumpInfo.l + JumpInfo.w - HPos)
    and (Y >= JumpInfo.t - VPos * SmallStep)
    and (Y <= JumpInfo.t + JumpInfo.h - VPos*SmallStep)
    then
    begin
      Cursor :=  FStyle.JumpCursor;
      if Assigned(FOnRVMouseMove) and (LastJumpMovedAbove <> JumpInfo.id) then
      begin
        OnRVMouseMove(Self, JumpInfo.id + FirstJumpNo);
      end;

      if DrawHover and (LastJumpMovedAbove <> -1) and (LastJumpMovedAbove <> JumpInfo.id) then
      begin
        DrawHover := False;
        InvalidateJumpRect(LastJumpMovedAbove);
      end;

      LastJumpMovedAbove := JumpInfo.id;
      if (Style <> nil) and (Style.HoverColor <> clNone) and (not DrawHover) then
      begin
        DrawHover := True;
        InvalidateJumpRect(LastJumpMovedAbove);
      end;

      Exit;
    end;
  end;

  Cursor :=  crDefault;
  if DrawHover and (LastJumpMovedAbove <> -1) then
  begin
    DrawHover := False;
    InvalidateJumpRect(LastJumpMovedAbove);
  end;

  if Assigned(FOnRVMouseMove) and (LastJumpMovedAbove <> -1) then
  begin
    LastJumpMovedAbove := -1;
    OnRVMouseMove(Self, -1);
  end;

  if Selection then
    Invalidate();
end;
{-------------------------------------}
procedure TCustomRichView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i, StyleNo, no, offs, ys: Integer;
  sClickedWord: string;
  p: TPoint;
  JumpInfo: TJumpInfo;
begin
  if Assigned(ScrollTimer) then
  begin
    FreeAndNil(ScrollTimer);
  end;
  XClicked := X;
  YClicked := Y;
  if Selection and (Button = mbLeft) then
  begin
    ys := y;
    if ys < 0 then
      y:=0;
    if ys > ClientHeight then
      ys := ClientHeight;
    no := 0;
    offs := 0;
    FindItemForSel(XClicked + HPos, ys + VPos * SmallStep, no, offs);
    FSelEndNo := no;
    FselEndOffs := offs;
    Selection := False;
    Invalidate();
    if Assigned(FOnSelect) then
      FOnSelect(Self);
  end;

  if Button = mbRight then
  begin
    inherited MouseUp(Button, Shift, X, Y);
    if not Assigned(FOnRVRightClick) then Exit;

    p := ClientToScreen(Point(X,Y));
    sClickedWord := '';
    StyleNo := 0;
    if FindClickedWord(sClickedWord, StyleNo) then
      FOnRVRightClick(Self, sClickedWord, StyleNo,p.X,p.Y);
    Exit;
  end;

  if Button <> mbLeft then
    Exit;

  if (LastJumpDowned = -1) or (not Assigned(FOnJump)) then
  begin
    Exit;
  end;

  for i:=0 to Jumps.Count-1 do
  begin
    JumpInfo := Jumps[i];
    if (LastJumpDowned = JumpInfo.id)
    and (X >= JumpInfo.l - HPos)
    and (X <= JumpInfo.l + JumpInfo.w - HPos)
    and (Y >= JumpInfo.t - VPos * SmallStep)
    and (Y <= JumpInfo.t + JumpInfo.h - VPos * SmallStep)
    then
    begin
      OnJump(Self, JumpInfo.id + FirstJumpNo);
      Break;
    end;
  end;

  LastJumpDowned := -1;
  inherited MouseUp(Button, Shift, X, Y);
end;
{-------------------------------------}
procedure TCustomRichView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i, no, StyleNo: Integer;
  sClickedWord: string;
  JumpInfo: TJumpInfo;
begin
  if Button <> mbLeft then
    Exit;

  XClicked := X;
  YClicked := Y;
  //if Assigned(FOnJump) then begin
  LastJumpDowned := -1;
  for i := 0 to jumps.Count-1 do
  begin
    JumpInfo := Jumps[i];
    if  (X >= JumpInfo.l - HPos)
    and (X <= JumpInfo.l + JumpInfo.w - HPos)
    and (Y >= JumpInfo.t - VPos * SmallStep)
    and (Y <= JumpInfo.t + JumpInfo.h - VPos * SmallStep)
    then
    begin
      LastJumpDowned := JumpInfo.id;
      Break;
    end;
  end;

  //if LastJumpDowned=-1 then
  if AllowSelection then
  begin
    no := 0;
    FindItemForSel(XClicked + HPos, YClicked + VPos * SmallStep, no, FSelStartOffs);
    FSelStartNo := no;
    FSelEndNo   := no;
    Selection   := (no <> -1);
    FSelEndOffs := FSelStartOffs;
    Invalidate();
    if not Assigned(ScrollTimer) then
    begin
      ScrollTimer := TTimer.Create(nil);
      ScrollTimer.OnTimer := OnScrollTimer;
      ScrollTimer.Interval := 100;
    end;
  end;

  sClickedWord := '';
  StyleNo := 0;
  if SingleClick and Assigned(FOnRVDblClick) and FindClickedWord(sClickedWord, StyleNo) then
     FOnRVDblClick(Self, sClickedWord, StyleNo);

  inherited MouseDown(Button, Shift, X, Y);
end;
{-------------------------------------}
procedure TCustomRichView.AppendFrom(Source: TCustomRichView);
var
  i: Integer;
  gr: TGraphic;
  grclass: TGraphicClass;
  li: TLineInfo;
begin
  ClearTemporal();
  for i := 0 to Source.Lines.Count-1 do
  begin
    li := Source.Lines[i];
    case li.StyleNo of
      rvsBreak:
        AddBreak();

      rvsCheckPoint:
        AddCheckPoint();

      rvsPicture:
      begin
        grclass := TGraphicClass(li.gr.ClassType);
        gr := grclass.Create();
        gr.Assign(li.gr);
        AddPicture(gr);
      end;

      rvsHotSpot:
        AddHotSpot(li.imgNo, TImageList(li.gr), (not li.SameAsPrev));

      rvsComponent:
      begin
        { if li.gr is TControl then
        ctrlclass := TControlClass(li.gr.ClassType);
        ctrl := ctrlclass.Create(Self);
        ctrl.Assign(li.gr);
        AddControl(ctrl, li.Center); }
      end;

      rvsBullet:
        AddBullet(li.imgNo, TImageList(li.gr), (not li.SameAsPrev));
    else
      if li.Center then
        AddCenterLine(li.Text, li.StyleNo)
      else if li.SameAsPrev then
        Add(li.Text, li.StyleNo)
      else
        AddFromNewLine(li.Text, li.StyleNo);
    end;
  end;
end;
{-------------------------------------}
function TCustomRichView.GetLastCP(): Integer;
begin
  GetLastCP := CheckPoints.Count-1;
end;
{-------------------------------------}
procedure TCustomRichView.SetBackBitmap(Value: TBitmap);
begin
  FBackBitmap.Assign(Value);
  if (not Assigned(Value)) or (Value.Empty) then
    FullRedraw := False
  else
  begin
    case FBackgroundStyle of
     bsNoBitmap, bsTiledAndScrolled:
       FullRedraw := False;
     bsStretched, bsTiled:
       FullRedraw := True;
    end;
  end;
end;
{-------------------------------------}
procedure TCustomRichView.SetBackgroundStyle(Value: TBackgroundStyle);
begin
  FBackgroundStyle := Value;
  if FBackBitmap.Empty then
    FullRedraw := False
  else
  begin
    case FBackgroundStyle of
      bsNoBitmap, bsTiledAndScrolled:
        FullRedraw := False;
      bsStretched, bsTiled:
        FullRedraw := True;
    end;
  end;
end;
{-------------------------------------}
procedure TCustomRichView.DrawBack(DC: HDC; Rect: TRect; Width, Height: Integer);
var
  i, j, iMin, iMax, jMin, jMax: Integer;
  hbr: HBRUSH;
  bbHeight, bbWidth: Integer;
begin
  if FStyle = nil then Exit;

  if FBackBitmap.Empty or (FBackgroundStyle = bsNoBitmap) then
  begin
    hbr := CreateSolidBrush(ColorToRGB(FStyle.Color));
    Dec(Rect.Bottom, Rect.Top);
    Dec(Rect.Right, Rect.Left);
    Rect.Left := 0;
    Rect.Top := 0;
    FillRect(DC, Rect, hbr);
    DeleteObject(hbr);
  end
  else
  begin
    bbHeight := FBackBitmap.Height;
    bbWidth := FBackBitmap.Width;
    case FBackgroundStyle of
      bsTiled:
      begin
        iMin := (Rect.Top div bbHeight);
        iMax := (Rect.Bottom div bbHeight);
        for i := iMin to iMax do
        begin
          jMin := (Rect.Left div bbWidth);
          jMax := (Rect.Right div bbWidth);
          for j := jMin to jMax do
          begin
            BitBlt(DC,
                   j * FBackBitmap.Width - Rect.Left,
                   i * bbHeight - Rect.Top,
                   bbWidth,
                   bbHeight,
                   FBackBitmap.Canvas.Handle, 0, 0, SRCCOPY);
          end;
        end;
      end;

      bsStretched:
        StretchBlt(DC, -Rect.Left, -Rect.Top, Width, Height,
                   FBackBitmap.Canvas.Handle, 0, 0, bbWidth, bbHeight,
                   SRCCOPY);

      bsTiledAndScrolled:
      begin
        iMin := (Rect.Top + VPos * SmallStep) div bbHeight;
        iMax := (Rect.Bottom + VPos * SmallStep) div bbHeight;
        for i := iMin to iMax do
        begin
          jMin := (Rect.Left + HPos) div bbWidth;
          jMax := (Rect.Right + HPos) div bbWidth;
          for j := jMin to jMax do
          begin
            BitBlt(DC,
                   j * bbWidth - HPos - Rect.Left,
                   i * bbHeight - VPos * SmallStep - Rect.Top,
                   bbWidth, bbHeight,
                   FBackBitmap.Canvas.Handle, 0, 0, SRCCOPY);
          end;
        end;
      end;
    end
  end;
end;
{-------------------------------------}
procedure TCustomRichView.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  r1: TRect;
begin
  if (csDesigning in ComponentState) then Exit;

  Message.Result := 1;
  if (OldWidth < ClientWidth) or (OldHeight < ClientHeight) then
  begin
    {$IFDEF FPC}
    GetClipBox(Message.DC, @r1);
    {$ELSE}
    GetClipBox(Message.DC, r1);
    {$ENDIF}
    DrawBack(Message.DC, r1, ClientWidth, ClientHeight);
  end;
  OldWidth := ClientWidth;
  OldHeight := ClientHeight;
end;
{-------------------------------------}
procedure TCustomRichView.SetVSmallStep(Value: Integer);
begin
  if (Value <= 0) or (TextHeight div Value > 30000) then
    Exit;
  SmallStep := Value;
end;
{-------------------------------------}
procedure TCustomRichView.ShareLinesFrom(Source: TCustomRichView);
begin
  ShareContents := True;
  if ShareContents then
  begin
    Clear();
    lines := Source.Lines;
  end;
end;
{-------------------------------------}
function TCustomRichView.FindItemAtPos(X, Y: Integer): Integer;
var
  i, a, b, mid, midtop: Integer;
  dli: TDrawLineInfo;
begin
  if DrawLines.Count = 0 then
  begin
    Result := -1;
    Exit;
  end;

  dli := DrawLines[0];
  if  (dli.Top <= Y) and (dli.Top + dli.Height > Y)
  and (dli.Left <= X) and (dli.Left + dli.Width > X) then
  begin
    Result := 0;
    Exit;
  end;

  a := 1;
  b := DrawLines.Count-1;
  while (b - a) > 1 do
  begin
    mid := (a + b) div 2;
    if (DrawLines[mid].Top <= Y) then
      a := mid
    else
      b := mid;
  end;

  mid := a;
  midtop := DrawLines[mid].Top;
  while (mid >= 1) and (DrawLines[mid-1].Top + DrawLines[mid-1].Height > midtop) do
    Dec(mid);

  for i:=1 to 2 do
  begin
    if mid = DrawLines.Count then
      Break;

    dli := DrawLines[mid];
    midtop := dli.Top + dli.Height-1;
    while (mid < DrawLines.Count) do
    begin
      dli := DrawLines[mid];
      if (dli.Top > midtop) then
        Break;
      if (dli.Top <= Y) and (dli.Top + dli.Height > Y)
      and (dli.Left <= X) and (dli.Left + dli.Width>X) then
      begin
        Result := mid;
        Exit;
      end;
      Inc(mid);
    end;
  end;
  Result := -1;
end;
  {------------------------------------------------------------------}
procedure TCustomRichView.FindItemForSel(X, Y: Integer; var No, Offs: Integer);
var
  styleno, i, a, b: Integer;
  mid, midtop, midbottom, midleft, midright: Integer;
  beginline, endline: Integer;
  dli, dliMid: TDrawLineInfo;
  {$IFNDEF RICHVIEWDEF4}
  arr: array[0..1000] of integer;
  {$ENDIF}
  sz: TSIZE;
begin
  if DrawLines.Count = 0 then
  begin
    No := -1;
    Exit;
  end;

  dli := DrawLines[0];
  if {(dli.Top <= Y) and }(dli.Top + dli.Height > Y) {and
     (dli.Left <= X) and (dli.Left + dli.Width > X)} then
  begin
    mid := 0
  end
  else
  begin
    a := 1;
    b := DrawLines.Count-1;
    while (b - a) > 1 do
    begin
      mid := (a + b) div 2;
      if (DrawLines[mid].Top <= Y) then
        a := mid
      else
        b := mid;
    end;
    mid := a;
    if DrawLines[b].Top <= Y then
      mid := b;
  end;

  dliMid := DrawLines[mid];
  midtop := dliMid.Top;
  midbottom := midtop + dliMid.Height;

  // searching beginning of line "mid" belong to
  beginline := mid;
  while (beginline >= 1)
  and (DrawLines[beginline-1].Top + DrawLines[beginline-1].Height > midtop) do
    Dec(beginline);

  // searching end of line "mid" belong to
  endline := mid;
  while (endline < DrawLines.Count-1)
  and (DrawLines[endline+1].Top < midbottom) do
    Inc(endline);

  // calculating line bounds
  midleft := dliMid.Left;
  midright := midleft + dliMid.Width;
  for i := beginline to endline do
  begin
    dli := DrawLines[i];
    if dli.Top < midtop then
      midtop := dli.Top;
    if dli.Top + dli.Height > midbottom then
      midbottom := dli.Top + dli.Height;
    if dli.Left < midleft then
      midleft := dli.Left;
    if dli.Left + dli.Width > midright then
      midright := dli.Left + dli.Width;
  end;
  if (Y < midtop) or (X < midleft) then
  begin
  {
     No := beginline-1;
     if No<0 then begin
       No := 0;
       Offs := 1;
       end
     else begin
       if TLineInfo(Lines.Objects[TDrawLineInfo(DrawLines.Objects[No]).LineNo]).StyleNo<0 then
         Offs := 2
       else
         Offs := Length(DrawLines[No])+1;
     end;
     exit;
  }
    No := beginline;
    if Lines[DrawLines[No].LineNo].StyleNo < 0 then
      Offs := 0
    else
      Offs := 1;
    Exit;
  end;

  if (Y > midbottom) or (X > midright) then
  begin
    No := endline + 1;
    Offs := 1;
    if No >= DrawLines.Count then
    begin
      No := DrawLines.Count-1;
      Offs := Length(DrawLines[No].Text)+1;
    end
    else
    begin
      if Lines[DrawLines[No].LineNo].StyleNo < 0 then
        Offs := 0;
    end;
    Exit;
  end;

  for i := beginline to endline do
  begin
    dli := DrawLines[i];
    if (dli.Left <= X) and (dli.Left + dli.Width >= X) then
    begin
      styleno := Lines[dli.LineNo].StyleNo;
      No := i;
      Offs := 0;
      if styleno >= 0 then
      begin
        with FStyle.TextStyles[StyleNo] do
        begin
          Canvas.Font.Style := Style;
          Canvas.Font.Size  := Size;
          Canvas.Font.Name  := FontName;
          {$IFDEF RICHVIEWDEF3}
          Canvas.Font.CharSet  := CharSet;
          {$ENDIF}
        end;
        sz.cx := 0;
        sz.cy := 0;
        {$IFDEF FPC}
        MyGetTextExtentExPoint(Canvas.Handle, PChar(dli.Text), Length(dli.Text),
        {$ELSE}
        GetTextExtentExPoint(Canvas.Handle, PChar(dli.Text), Length(dli.Text),
        {$ENDIF}
                            X-dli.Left,
                            {$IFDEF RICHVIEWDEF4}
                            @Offs, nil,
                            {$ELSE}
                            Offs, arr[0],
                            {$ENDIF}
                             sz);
        Inc(Offs);
        if Offs > Length(dli.Text) then
          Offs := Length(dli.Text);
        if (Offs < 1) and (Length(dli.Text) > 0) then
          Offs := 1;
      end
      else
        Offs := 1;
    end;
  end;
end;
{------------------------------------------------------------------}
function TCustomRichView.FindClickedWord(var sClickedWord: String;
  var StyleNo: Integer): Boolean;
var
  no, lno: Integer;
  {$IFNDEF RICHVIEWDEF4}
  arr: array[0..1000] of integer;
  {$ENDIF}
  sz: TSIZE;
  max, first, len: Integer;
begin
  Result := False;
  no := FindItemAtPos(XClicked + HPos, YClicked + VPos * SmallStep);
  if no <> -1 then
  begin
    lno := DrawLines[no].LineNo;
    sClickedWord := DrawLines[no].Text;
    styleno := Lines[lno].StyleNo;
    if styleno >= 0 then
    begin
      with FStyle.TextStyles[StyleNo] do
      begin
        Canvas.Font.Style := Style;
        Canvas.Font.Size  := Size;
        Canvas.Font.Name  := FontName;
        {$IFDEF RICHVIEWDEF3}
        Canvas.Font.CharSet  := CharSet;
        {$ENDIF}
      end;
      sz.cx := 0;
      sz.cy := 0;
      {$IFDEF FPC}
      MyGetTextExtentExPoint(Canvas.Handle, PChar(sClickedWord), Length(sClickedWord),
      {$ELSE}
      GetTextExtentExPoint(Canvas.Handle, PChar(sClickedWord), Length(sClickedWord),
      {$ENDIF}
                          XClicked + HPos - DrawLines[no].Left,
                          {$IFDEF RICHVIEWDEF4}
                          @max, nil,
                          {$ELSE}
                          max, arr[0],
                          {$ENDIF}
                          sz);
      Inc(max);
      if max > Length(sClickedWord) then
        max := Length(sClickedWord);
      first := max;
      if (Pos(sClickedWord[first], Delimiters) <> 0) then
      begin
        sClickedWord := '';
        Result := True;
        Exit;
      end;

      while (first > 1) and (Pos(sClickedWord[first-1], Delimiters) = 0) do
        Dec(first);
      len := max - first + 1;
      while (first + len- 1 < Length(sClickedWord)) and (Pos(sClickedWord[first + len], Delimiters) = 0) do
        Inc(len);
      sClickedWord := Copy(sClickedWord, first, len);
    end;
    Result := True;
  end;
end;
{------------------------------------------------------------------}
procedure TCustomRichView.DblClick();
var
  StyleNo: Integer;
  sClickedWord: string;
begin
  inherited DblClick;
  if SingleClick or (not Assigned(FOnRVDblClick)) then
    Exit;
  sClickedWord := '';
  StyleNo := 0;
  if FindClickedWord(sClickedWord, StyleNo) then
     FOnRVDblClick(Self, sClickedWord, StyleNo);
end;
  {------------------------------------------------------------------}
procedure TCustomRichView.DeleteSection(const ACheckPointName: string);
var
  i, j, startno, endno: Integer;
begin
  if ShareContents then
    Exit;

  for i := 0 to CheckPoints.Count-1 do
  begin
    if CheckPoints[i].Text = ACheckPointName then
    begin
      startno := CheckPoints[i].LineNo;
      endno := Lines.Count-1;
      for j := i+1 to CheckPoints.Count-1 do
      begin
        if CheckPoints[j].Text <> '' then
        begin
          endno := CheckPoints[j].LineNo-1;
          Break;
        end;
      end;
      DeleteLines(startno, endno-startno+1);
      Exit;
    end;
  end;
end;
  {------------------------------------------------------------------}
procedure TCustomRichView.DeleteLines(FirstLine, Count: Integer);
var
  i: Integer;
  LineInfo: TLineInfo;
begin
  if ShareContents then
    Exit;
  if FirstLine >= lines.Count then
    Exit;

  Deselect();
  if FirstLine + Count > lines.Count then
    Count := lines.Count - firstline;

  for i := FirstLine to FirstLine+Count-1 do
  begin
    LineInfo := Lines[i];
    if LineInfo.StyleNo = rvsPicture then { image}
    begin
      LineInfo.gr.Free;
      LineInfo.gr := nil;
    end;

    if LineInfo.StyleNo = rvsComponent then {control}
    begin
      RemoveControl(TControl(LineInfo.gr));
      LineInfo.gr.Free;
      LineInfo.gr := nil;
    end;

    //LineInfo.Free;
    //LineInfo := nil;
  end;
  for i := 1 to Count do
    lines.Delete(FirstLine);

end;
  {------------------------------------------------------------------}
procedure TCustomRichView.GetSelBounds(out StartNo, EndNo, StartOffs, EndOffs: Integer);
begin
  if FSelStartNo <= FSelEndNo then
  begin
    StartNo := FSelStartNo;
    EndNo   := FSelEndNo;
    if not ((StartNo = EndNo) and (FSelStartOffs > FSelEndOffs)) then
    begin
      StartOffs := FSelStartOffs;
      EndOffs   := FSelEndOffs;
    end
    else
    begin
      StartOffs := FSelEndOffs;
      EndOffs   := FSelStartOffs;
    end;
  end
  else
  begin
    StartNo   := FSelEndNo;
    EndNo     := FSelStartNo;
    StartOffs := FSelEndOffs;
    EndOffs   := FSelStartOffs;
  end;
end;
{------------------------------------------------------------------}
procedure TCustomRichView.StoreSelBounds(var StartNo, EndNo, StartOffs, EndOffs: Integer);
var
  dli: TDrawLineInfo;
begin
  GetSelBounds(StartNo, EndNo, StartOffs, EndOffs);
  if StartNo <> -1 then
  begin
    dli := DrawLines[StartNo];
    if Lines[dli.LineNo].StyleNo >= 0 then
      Inc(StartOffs, dli.Offs-1);
    StartNo := dli.LineNo;
    dli := DrawLines[EndNo];
    if Lines[dli.LineNo].StyleNo >= 0 then
      Inc(EndOffs, dli.Offs-1);
    EndNo := dli.LineNo;
  end;
end;
{------------------------------------------------------------------}
procedure TCustomRichView.RestoreSelBounds(StartNo, EndNo, StartOffs, EndOffs: Integer);
var
  i: Integer;
  dli, dli2, dli3: TDrawLineInfo;
begin
  if StartNo = -1 then
    Exit;
  for i :=0 to DrawLines.Count-1 do
  begin
    dli := DrawLines[i];
    if dli.LineNo = StartNo then
    begin
      if Lines[dli.LineNo].StyleNo < 0 then
      begin
        FSelStartNo := i;
        FSelStartOffs := StartOffs;
      end
      else
      begin
        if i <> DrawLines.Count-1 then
          dli2 := DrawLines[i+1]
        else
          dli2 := nil;

        if i <> 0 then
          dli3 := DrawLines[i-1]
        else
          dli3 := nil;

        if ((dli.Offs <= StartOffs) and (Length(dli.Text) + dli.Offs > StartOffs))
        or ((StartOffs > Length(Lines[dli.LineNo].Text)) and ((dli2 = nil) or (dli2.LineNo <> dli.LineNo)))
        or ((dli.Offs > StartOffs) and ((dli3 = nil) or (dli3.LineNo <> dli.LineNo)))
        then
        begin
          FSelStartNo := i;
          FSelStartOffs := StartOffs - dli.Offs + 1;
          if FSelStartOffs < 0 then
            FSelStartOffs := 0;
          if FSelStartOffs > dli.Offs + Length(dli.Text) then
            FSelStartOffs := dli.Offs + Length(dli.Text);
        end;
      end;
    end;

    if dli.LineNo = EndNo then
    begin
      if Lines[dli.LineNo].StyleNo < 0 then
      begin
        FSelEndNo := i;
        FSelEndOffs := EndOffs;
      end
      else
      begin
        if i <> DrawLines.Count-1 then
          dli2 := DrawLines[i+1]
        else
          dli2 := nil;

        if i <> 0 then
          dli3 := DrawLines[i-1]
        else
          dli3 := nil;

        if ((dli.Offs <= EndOffs) and (Length(dli.Text) + dli.Offs > EndOffs))
        or ((EndOffs > Length(Lines[dli.LineNo].Text)) and ((dli2 = nil)or(dli2.LineNo <> dli.LineNo)))
        or ((dli.Offs > EndOffs) and ((dli3 = nil)or(dli3.LineNo <> dli.LineNo)))
        then
        begin
          FSelEndNo := i;
          FSelEndOffs := EndOffs - dli.Offs + 1;
          if FSelEndOffs < 0 then
            FSelEndOffs := 0;
          if FSelEndOffs > dli.Offs + Length(dli.Text) then
            FSelEndOffs := dli.Offs + Length(dli.Text);
        end;
      end;
    end;
  end;
end;
  {------------------------------------------------------------------}
function TCustomRichView.GetLineCount(): Integer;
begin
  Result := lines.Count;
end;
  {------------------------------------------------------------------}
function TCustomRichView.SelectionExists(): Boolean;
var
  StartNo, EndNo, StartOffs, EndOffs: Integer;
begin
  GetSelBounds(StartNo, EndNo, StartOffs, EndOffs);
  if (StartNo = -1) or (EndNo = -1) or ((StartNo = EndNo) and (StartOffs = EndOffs)) then
    Result := False
  else
    Result := True;
end;
{------------------------------------------------------------------}
function TCustomRichView.GetSelText(): string;
var
  StartNo, EndNo, StartOffs, EndOffs, i: Integer;
  s : string;
  li : TLineInfo;
begin
  Result := '';
  if not SelectionExists then
    Exit;

  { getting selection as Lines indices }
  StartNo := 0;
  EndNo := 0;
  StartOffs := 0;
  EndOffs := 0;
  StoreSelBounds(StartNo, EndNo, StartOffs, EndOffs);
  if StartNo = EndNo then
  begin
    li := Lines[StartNo];
    if li.StyleNo < 0 then
      Exit;
    Result := Copy(li.Text, StartOffs, EndOffs-StartOffs);
    Exit;
  end
  else
  begin
    li := Lines[StartNo];
    if li.StyleNo < 0 then
      s := ''
    else
      s := Copy(li.Text, StartOffs, Length(li.Text));

    for i := StartNo + 1 to EndNo do
    begin
      li := Lines[i];
      if (li.StyleNo <> rvsCheckpoint) and (not li.SameAsPrev) then
        s := s + chr(13);
      if li.StyleNo >= 0 then
      begin
        if i <> EndNo then
          s := s + li.Text
        else
          s := s + Copy(li.Text, 1, EndOffs-1);
      end;
    end;
    {$IFDEF FPC}
    Result := AdjustLineBreaks(s, tlbsCRLF);
    {$ELSE}
    Result := AdjustLineBreaks(s);
    {$ENDIF}
  end;
end;
{------------------------------------------------------------------}
procedure TCustomRichView.CopyText();
begin
  if SelectionExists then
  begin
    ClipBoard.Clear();
    Clipboard.SetTextBuf(PChar(GetSelText));
  end;
end;
  {------------------------------------------------------------------}
procedure TCustomRichView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if SelectionExists and (ssCtrl in Shift) then
  begin
    if (Key = ord('C')) or (Key = VK_INSERT) then
      CopyText();
  end
  else
    inherited KeyDown(Key, Shift);
end;
{------------------------------------------------------------------}
procedure TCustomRichView.OnScrollTimer(Sender: TObject);
begin
  if ScrollDelta <> 0 then
  begin
    VScrollPos := VScrollPos + ScrollDelta;
    MouseMove([], XMouse, YMouse);
  end;
end;
  {------------------------------------------------------------------}
procedure TCustomRichView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FStyle) then
  begin
    Style := nil;
  end;
end;
  {------------------------------------------------------------------}
procedure TCustomRichView.Click();
begin
  SetFocus();
  inherited;
end;
  {------------------------------------------------------------------}
procedure TCustomRichView.Loaded();
begin
  inherited Loaded;
  Format();
end;

function TCustomRichView.GetCredits(): string;
begin
  Result := 'Lazarus TRichView based on RichView v0.5.1 (www.TCustomRichView.com)';
end;

{------------------------------------------------------------------}
{$I RV_Save.inc}
{------------------------------------------------------------------}

end.
