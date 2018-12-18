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
  RVItems, RVStyle, RVScroll, ClipBrd,
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
    { Return visual item index (-1 if cursor above limits and -2 if below limits) }
    function FindVisItemAtPos(X, Y: Integer): Integer;
    { ItemNo - original item index (-1 if cursor above limits and -2 if below limits)
      TextOffs - text offset inside item }
    procedure FindOrigItemForSel(X, Y: Integer; var ItemNo, TextOffs: Integer);
    { ItemNo - visible item index
      TextOffs - text offset inside item}
    procedure FindVisItemForSel(X, Y: Integer; var ItemNo, TextOffs: Integer);
    function GetLineCount: Integer;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    { StartNo, EndNo - visible item index
      StartOffs, EndOffs - text offset inside item }
    procedure GetVisibleSelBounds(out StartNo, EndNo, StartOffs, EndOffs: Integer);
    procedure GetItemsSelBounds(var StartNo, EndNo, StartOffs, EndOffs: Integer);
    procedure SetItemsSelBounds(StartNo, EndNo, StartOffs, EndOffs: Integer);
  protected
    { Protected declarations }
    FItems: TRVItemList;
    Items: TRVItemList;
    FVisibleItems: TRVItemList;

    CheckPoints: TCPInfoList;
    FJumpList: TJumpInfoList;
    FStyle: TRVStyle;
    nJmps: Integer;

    skipformatting: Boolean;

    TextWidth: Integer;
    TextHeight: Integer;

    LastJumpMovedAbove: Integer;
    LastLineFormatted: Integer;
    LastJumpDowned: Integer;
    XMouse, YMouse: Integer;
    XClicked, YClicked: Integer;

    imgSavePrefix: String;
    imgSaveNo: Integer;
    SaveOptions: TRVSaveOptions;

    ShareContents: Boolean;
    FClientTextWidth: Boolean;

    { default text alignment - taLeftJustify, taRightJustify, taCenter }
    FDefaultAlignment: TAlignment;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Click(); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;    
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure FormatLine(no: Integer; var x, baseline, prevdesc, prevabove: Integer; Canvas: TCanvas;
                         var sad: TScreenAndDevice);
    procedure AdjustJumpsCoords();
    { set position for embedded controls }
    procedure AdjustChildrenCoords();
    procedure ClearTemporal();
    function GetFirstVisible(TopLine: Integer): Integer;
    { get Items[] index for FVisibleItem[] index }
    function GetItemIdByVisibleId(AVisibleId: Integer): Integer;
    function GetFirstLineVisible(): Integer;
    function GetLastLineVisible(): Integer;
    { Option: gdlnFirstVisible, gdlnLastCompleteVisible, gdlnLastVisible }
    function GetVisibleItemIndex(BoundLine: Integer; Option: Integer): Integer;
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
    procedure AddPicture(gr: TGraphic; AShared: Boolean = False);
    { Adds image-hypertext link. Parameters are the same as in the method AddBullet. }
    procedure AddHotSpot(imgNo: Integer; lst: TImageList; FromNewLine: Boolean);
    { Adds picture imgNo from ImageList at the new line or not. }
    procedure AddBullet(imgNo: Integer; lst: TImageList; FromNewLine: Boolean);
    { Adds ANY visible Delphi control with center (True) or left (False) alignment.
      This method adds element, which is drawed and processed by control itself,
      not by RichView. It works the same way as if it was inserted in any other Delphi control.
      WARNING - These componets will be destroyed when you call method Clear()
      or when TRichView control is destroyed. Do not destroy them yourself! }
    procedure AddControl(ACtrl: TControl; ACentered: Boolean; AShared: Boolean = False); reintroduce;

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
    procedure DeleteItems(AFirstIndex, ACount: Integer);

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

    function GetDebugInfo(): string;

    { Number of pixels in 1 MSU. Important: if you change it when TRichView
      component is already displayed, you must call Format and Refresh methods after. }
    property VSmallStep: Integer read SmallStep write SetVSmallStep;
    property LineCount: Integer read GetLineCount;
    //property FirstLineVisible: Integer read GetFirstLineVisible;
    //property LastLineVisible: Integer read GetLastLineVisible;
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

const
  gdlnFirstVisible = 1;
  gdlnLastCompleteVisible = 2;
  gdlnLastVisible = 3;

{$IFDEF FPC}
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
  FItems         := TRVItemList.Create();
  Items          := FItems;
  FVisibleItems  := TRVItemList.Create();
  FVisibleItems.OwnSubitems := True;
  CheckPoints    := TCPInfoList.Create();
  FJumpList      := TJumpInfoList.Create();
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
  FDefaultAlignment := taLeftJustify;
  //Format_(False,0, Canvas, False);
end;

destructor TCustomRichView.Destroy;
begin
  FreeAndNil(FBackBitmap);
  Clear();
  FreeAndNil(CheckPoints);
  FreeAndNil(FJumpList);
  FreeAndNil(FItems);
  FreeAndNil(FVisibleItems);
  inherited Destroy;
end;

procedure TCustomRichView.WMSize(var Message: TWMSize);
begin
  Format_(True, 0, Canvas, False);
  if Assigned(FOnResized) then FOnResized(Self);
end;

procedure TCustomRichView.Format;
begin
  Format_(False, 0, Canvas, False);
end;

procedure TCustomRichView.FormatTail;
begin
  Format_(False, 0, Canvas, True);
end;

procedure TCustomRichView.ClearTemporal();
begin
  if Assigned(ScrollTimer) then
  begin
    FreeAndNil(ScrollTimer);
  end;

  FVisibleItems.Clear();

  CheckPoints.Clear();

  FJumpList.Clear();
  nJmps :=0;
end;

procedure TCustomRichView.Deselect();
begin
  Selection := False;
  FSelStartNo := -1;
  FSelEndNo := -1;
  FSelStartOffs := 0;
  FSelEndOffs := 0;
  if Assigned(FOnSelect) then OnSelect(Self);  
end;

procedure TCustomRichView.SelectAll();
begin
  FSelStartNo := 0;
  FSelEndNo := FVisibleItems.Count-1;
  FSelStartOffs := 0;
  FSelEndOffs := 0;
  if FVisibleItems[FSelEndNo].StyleNo >= 0 then
    FSelEndOffs := Length(FVisibleItems[FSelEndNo].Text) + 1;
  if Assigned(FOnSelect) then OnSelect(Self);
end;

procedure TCustomRichView.Clear();
var
  i: Integer;
  Item: TRVItem;
  VisItem: TRVVisualItem;
begin
  Deselect();
  if not ShareContents then
  begin
    for i:=0 to Items.Count-1 do
    begin
      Item := Items[i];
      if (Item is TRVVisualItem) then
      begin
        VisItem := (Item as TRVVisualItem);
        if VisItem.StyleNo = rvsPicture then { image}
        begin
          if not VisItem.Shared then
            VisItem.gr.Free();
          VisItem.gr := nil;
        end;
        if VisItem.StyleNo = rvsComponent then {control}
        begin
          RemoveControl(TControl(VisItem.gr));
          if not VisItem.Shared then
            VisItem.gr.Free();
          VisItem.gr := nil;
        end;
      end;
    end;
    Items.Clear();
  end;
  ClearTemporal();
end;

procedure TCustomRichView.AddFromNewLine(const s: string; StyleNo: Integer);
var
  Item: TRVItem;
begin
  Item := TRVItem.Create();
  Item.StyleNo := StyleNo;
  Item.SameAsPrev := False;
  Item.Alignment := FDefaultAlignment;
  Item.Text := s;
  Items.Add(Item);
end;

procedure TCustomRichView.Add(const s: string; StyleNo: Integer);
var
  Item: TRVItem;
begin
  Item := TRVItem.Create();
  Item.StyleNo := StyleNo;
  Item.SameAsPrev := (Items.Count <> 0);
  Item.Alignment := FDefaultAlignment;
  Item.Text := s;
  Items.Add(Item);
end;

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

procedure TCustomRichView.AddCenterLine(const s: string; StyleNo: Integer);
var
  Item: TRVItem;
begin
  Item := TRVItem.Create();
  Item.StyleNo := StyleNo;
  Item.SameAsPrev := False;
  Item.Alignment := taCenter;
  Item.Text := s;
  Items.Add(Item);
end;

procedure TCustomRichView.AddBreak();
var
  Item: TRVItem;
begin
  Item := TRVItem.Create();
  Item.StyleNo := rvsBreak;
  Item.Text := '';
  Items.Add(Item);
end;

function TCustomRichView.AddNamedCheckPoint(const CpName: String): Integer;
var
  Item: TRVItem;
  CPInfo: TCPInfo;
begin
  Item := TRVItem.Create();
  Item.StyleNo := rvsCheckPoint;
  Item.Text := CpName;
  Items.Add(Item);

  CPInfo := TCPInfo.Create();
  CPInfo.Y := 0;
  CPInfo.Text := CpName;
  CheckPoints.Add(CPInfo);
  AddNamedCheckPoint := CheckPoints.Count-1;
end;

function TCustomRichView.AddCheckPoint(): Integer;
begin
  AddCheckPoint := AddNamedCheckPoint('');
end;

function TCustomRichView.GetCheckPointY(no: Integer): Integer;
begin
  GetCheckPointY := CheckPoints[no].Y;
end;

function TCustomRichView.GetJumpPointY(no: Integer): Integer;
var
  i, n: Integer;
begin
  GetJumpPointY := 0;
  n := no-FirstJumpNo;
  for i:=0 to FJumpList.Count-1 do
  begin
    if FJumpList[i].id = n then
    begin
      GetJumpPointY := FJumpList[i].t;
      Exit;
    end;
  end;
end;

procedure TCustomRichView.AddPicture(gr: TGraphic; AShared: Boolean); { gr not copied, do not free it!}
var
  Item: TRVVisualItem;
begin
  Item := TRVVisualItem.Create();
  Item.StyleNo := rvsPicture;
  Item.gr := gr;
  Item.SameAsPrev := False;
  Item.Alignment := taCenter;
  Item.Text := '';
  Item.Shared := AShared;
  Items.Add(Item);
end;

procedure TCustomRichView.AddHotSpot(imgNo: Integer; lst: TImageList;
  FromNewLine: Boolean);
var
  Item: TRVVisualItem;
begin
  Item := TRVVisualItem.Create();
  Item.StyleNo := rvsHotSpot;
  Item.gr := lst;
  Item.imgNo := imgNo;
  Item.SameAsPrev := not FromNewLine;
  Item.Text := '';
  Items.Add(Item);
end;

procedure TCustomRichView.AddBullet(imgNo: Integer; lst: TImageList;
  FromNewLine: Boolean);
var
  Item: TRVVisualItem;
begin
  Item := TRVVisualItem.Create();
  Item.StyleNo := rvsBullet;
  Item.gr := lst;
  Item.imgNo := imgNo;
  Item.SameAsPrev := not FromNewLine;
  Item.Text := '';
  Items.Add(Item);
end;

procedure TCustomRichView.AddControl(ACtrl: TControl; ACentered: Boolean;
  AShared: Boolean);
var
  Item: TRVVisualItem;
begin
  Item := TRVVisualItem.Create();
  Item.StyleNo := rvsComponent;
  Item.gr := ACtrl;
  Item.SameAsPrev := False;
  if ACentered then
    Item.Alignment := taCenter
  else
    Item.Alignment := FDefaultAlignment;
  Item.Text := '';
  Item.Shared := AShared;
  Items.Add(Item);
  InsertControl(ACtrl);
end;

function TCustomRichView.GetMaxPictureWidth(): Integer;
var
  i: Integer;
  Item: TRVItem;
begin
  Result := 0;
  for i := 0 to Items.Count-1 do
  begin
   Item := Items[i];
   if Item.StyleNo = rvsPicture then
   begin
     if Result < TGraphic((Item as TRVVisualItem).gr).Width then
       Result := TGraphic((Item as TRVVisualItem).gr).Width;
   end;
   if Item.StyleNo = rvsComponent then
   begin
     if Result < TControl((Item as TRVVisualItem).gr).Width then
       Result := TControl((Item as TRVVisualItem).gr).Width;
   end;
 end;
end;

function Max(a, b: Integer): Integer;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

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
    GetItemsSelBounds(StartNo, EndNo, StartOffs, EndOffs);
  end;

  OldY := VPos * SmallStep;

  oldtextwidth := TextWidth;

  mx := Max(ClientWidth - (FLeftMargin + FRightMargin), GetMaxPictureWidth);
  if mx < FMinTextWidth then
    mx := FMinTextWidth;
  if FClientTextWidth then
  begin { widths of pictures and maxtextwidth are ignored }
    TextWidth := ClientWidth - (FLeftMargin + FRightMargin);
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
  // format lines
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
    for i := StartLine to Items.Count-1 do
    begin
      StyleNo := Items[i].StyleNo;
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

  // Update scrollbars and visible position
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
  //!!!if depth=0 then
  //  SetItemsSelBounds(StartNo, EndNo, StartOffs, EndOffs);

  skipformatting := False;
  LastLineFormatted := Items.Count-1;
end;

procedure TCustomRichView.AdjustChildrenCoords();
var
  i: Integer;
  Item: TRVItem;
  VisItem: TRVVisualItem;
begin
  for i := 0 to Items.Count-1 do
  begin
    Item := Items[i];
    if Item.StyleNo = rvsComponent then {control}
    begin
      VisItem := (Item as TRVVisualItem);
      TControl(VisItem.gr).Left := VisItem.Left;
      TControl(VisItem.gr).Tag := VisItem.Top;
      Tag2Y(TControl(VisItem.gr));
    end;
  end;
end;

{ x        - X position
  baseline - base Y position of line
  prevDesc - Descent (units below the base line) of characters
  prevAbove - ascent (units above the base line) of characters }
procedure TCustomRichView.FormatLine(no: Integer; var x, baseline, prevDesc, prevAbove: Integer;
          Canvas: TCanvas; var sad: TScreenAndDevice);
var sourceStrPtr, strForAdd, strSpacePos: PChar;
    sourceStrPtrLen: Integer;
    sz: TSIZE;
    maxChars, j, y, ctrlw, ctrlh : Integer;
{$IFNDEF RICHVIEWDEF4}
    arr: array[0..1000] of integer;
{$ENDIF}
    char_arr: array[0..1000] of char;
    strForAddMaxLen: Integer;
    Item, SubItem, CurItem: TRVItem;
    VisItem: TRVVisualItem;
    TextMetr: TTextMetric;
    StyleNo: Integer;
    IsNewLine, IsCenter: Boolean;
    CPInfo: TCPInfo;
    JmpInfo: TJumpInfo;
    width, y5, Offs: Integer;
    hBetweenLines: Integer;
begin
  width := TextWidth;
  y := 0;
  Item := Items[no];
  case Item.StyleNo of
    rvsComponent: { Control }
    begin
      FVisibleItems.Add(Item);
      VisItem := (Item as TRVVisualItem);
      ctrlw := TControl(VisItem.gr).Width;
      ctrlh := TControl(VisItem.gr).Height;
      ctrlw := MulDiv(ctrlw, sad.ppixDevice, sad.ppixScreen);
      ctrlh := MulDiv(ctrlh, sad.ppiyDevice, sad.ppiyScreen);

      Item.Top    := baseline + prevDesc + 1;
      Item.Width  := ctrlw;
      Item.Height := ctrlh+1;
      if Item.Alignment = taCenter then
      begin
        Item.Left   := (width-ctrlw) div 2;
        if Item.Left < 0 then
          Item.Left := 0;
        Inc(Item.Left, sad.LeftMargin);
      end
      else
      begin
        Item.Left := sad.LeftMargin;
      end;
      TControl(VisItem.gr).Left := VisItem.Left;
      TControl(VisItem.gr).Tag := VisItem.Top;
      Tag2Y(TControl(VisItem.gr));
      Inc(baseline, prevDesc + ctrlh + 1);
      prevDesc := 1;
      prevAbove := ctrlh + 1;
    end;

    rvsHotSpot, rvsBullet:
    begin
      FVisibleItems.Add(Item);
      VisItem := (Item as TRVVisualItem);
      ctrlw := TImageList(VisItem.gr).Width;
      ctrlh := TImageList(VisItem.gr).Height;
      ctrlw := MulDiv(ctrlw, sad.ppixDevice, sad.ppixScreen);
      ctrlh := MulDiv(ctrlh, sad.ppiyDevice, sad.ppiyScreen);
      VisItem.Width  := ctrlw+1;
      VisItem.Height := ctrlh+1;
      if (not Item.SameAsPrev) or (x + ctrlw + 2 > width) then
      begin
        x := 0;
        y := baseline + prevDesc;
        Inc(baseline, prevDesc + ctrlh + 1);
        prevDesc := 1;
        prevAbove := ctrlh + 1;
      end
      else
      begin
        if prevAbove < ctrlh + 1 then
        begin
          j := FVisibleItems.Count-1;
          if j >= 0 then
          begin
            repeat
              Inc(FVisibleItems[j].Top, ctrlh + 1 - prevAbove);
              Dec(j);
            until FVisibleItems[j+1].FromNewLine;
          end;
          Inc(baseline, ctrlh + 1 - prevAbove);
          prevAbove := ctrlh + 1;
          y := baseline - (ctrlh + 1);
        end;
      end;
      if Item.StyleNo = rvsHotSpot then
      begin
        JmpInfo     := TJumpInfo.Create();
        JmpInfo.l   := x + 1 + sad.LeftMargin;;
        JmpInfo.t   := y + 1;
        JmpInfo.w   := ctrlw;
        JmpInfo.h   := ctrlh;
        JmpInfo.id  := nJmps;
        JmpInfo.VisibleItemId := no;
        JmpInfo.Text:= '';
        FJumpList.Add(JmpInfo);
        Inc(nJmps);
      end;
      Item.Left := x + 1 + sad.LeftMargin;;
      Inc(x, ctrlw+2);
      Item.Top := y + 1;
      Item.FromNewLine := not Item.SameAsPrev;
    end;

    rvsPicture:  { graphics}
    begin
      FVisibleItems.Add(Item);
      VisItem := (Item as TRVVisualItem);
      ctrlw       := TGraphic(VisItem.gr).Width;
      ctrlh       := TGraphic(VisItem.gr).Height;
      ctrlw       := MulDiv(ctrlw, sad.ppixDevice, sad.ppixScreen);
      ctrlh       := MulDiv(ctrlh, sad.ppiyDevice, sad.ppiyScreen);

      Item.Width  := ctrlw;
      Item.Height := ctrlh+1;
      Item.Left   := (width-ctrlw) div 2;
      if Item.Left < 0 then
        Item.Left := 0;
      Inc(Item.Left, sad.LeftMargin);
      Item.Top    := baseline + prevDesc + 1;
      Inc(baseline, prevDesc + ctrlh + 1);
      prevDesc    := 1;
      prevAbove   := ctrlh+1;
    end;

    rvsCheckPoint: { check point}
    begin
      CPInfo   := TCPInfo.Create();
      CPInfo.Y := baseline + prevDesc;
      CPInfo.ItemIndex := no;
      CPInfo.Text := Item.Text;
      CheckPoints.Add(CPInfo);
    end;

    rvsBreak: { break line}
    begin
      FVisibleItems.Add(Item);
      y5            := MulDiv(5, sad.ppiyDevice, sad.ppiyScreen);
      Item.Left   := sad.LeftMargin;
      Item.Top    := baseline + prevDesc;
      Item.Width  := Width;
      Item.Height := y5 + y5 + 1;
      Inc(baseline, prevDesc + y5 + y5 + 1);
      prevDesc  := y5;
      prevAbove := y5;
    end;

  else // rvsText
    begin
      // length of source string
      sourceStrPtr := PChar(Item.Text);
      char_arr[0] := #0; // eliminate warning
      strForAdd := char_arr;
      strForAddMaxLen := SizeOf(char_arr) - 1;
      sourceStrPtrLen := StrLen(sourceStrPtr);

      StyleNo := Item.StyleNo;
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
      hBetweenLines := TextMetr.tmExternalLeading + TextMetr.tmAscent;
      IsNewLine := not Item.SameAsPrev;
      IsCenter := (Item.Alignment = taCenter);
      CurItem := Item;
      while sourceStrPtrLen > 0 do
      begin
        if IsNewLine then
          x := 0;
        sz.cx := 0;
        sz.cy := 0;
        // get number of characters that will fit in specified width
        {$IFDEF FPC}
        MyGetTextExtentExPoint(Canvas.Handle,  sourceStrPtr,  sourceStrPtrLen, Width-x,
        {$ELSE}
        GetTextExtentExPoint(Canvas.Handle,  sourceStrPtr,  sourceStrPtrLen, Width-x,
        {$ENDIF}
                            {$IFDEF RICHVIEWDEF4}
                            @maxChars, nil,
                            {$ELSE}
                            maxChars, arr[0],
                            {$ENDIF}
                            sz);

        if maxChars = 0 then
          maxChars := 1;
        if maxChars > strForAddMaxLen then
          maxChars := strForAddMaxLen;
        if maxChars < sourceStrPtrLen then
        begin
          // source text not fit to line, need split at space char
          {if  sourceStrPtr[max] <> ' ' then }
          StrLCopy(strForAdd, sourceStrPtr, maxChars);
          strSpacePos := StrRScan(strForAdd, ' ');
          if strSpacePos <> nil then
          begin
            maxChars := strSpacePos - strForAdd;
            StrLCopy(strForAdd, sourceStrPtr, maxChars);
            Inc(maxChars);
          end
          else
          begin
            if not IsNewLine then
            begin
              x := 0;
              IsNewLine := true;
              Continue;
            end;
          end;
        end
        else
        begin
          StrLCopy(strForAdd, sourceStrPtr, maxChars);
        end;
        Offs := sourceStrPtr - PChar(Item.Text) + 1;
        sourceStrPtr := @(sourceStrPtr[maxChars]);
        if (Offs > 1) or (maxChars < Length(Item.Text)) then
        begin
          // add subitem
          SubItem := TRVItem.Create();
          SubItem.StyleNo := Item.StyleNo;
          SubItem.ParentItem := Item;
          SubItem.IsSubItem := True;
          CurItem := SubItem;
          CurItem.Text := strForAdd;
        end;
        CurItem.TextOffs := Offs; // offset in source text
        {$IFDEF FPC}
        MyGetTextExtentExPoint(Canvas.Handle,  strForAdd,  StrLen(strForAdd), Width-x,
        {$ELSE}
        GetTextExtentExPoint(Canvas.Handle,  strForAdd,  StrLen(strForAdd), Width-x,
        {$ENDIF}
           {$IFDEF RICHVIEWDEF4}
           @maxChars, nil,
           {$ELSE}
           maxChars, arr[0],
           {$ENDIF}
           sz);
        if not IsNewLine then
        begin
          {continue line}
          if prevAbove < hBetweenLines then
          begin
            j := FVisibleItems.Count-1;
            if j >= 0 then
            begin
              repeat
                Inc(FVisibleItems[j].Top, hBetweenLines - prevAbove);
                Dec(j);
              until FVisibleItems[j+1].FromNewLine or (j < 0);
            end;
            Inc(baseline, hBetweenLines - prevAbove);
            prevAbove := hBetweenLines;
          end;
          y := baseline - TextMetr.tmAscent;
          CurItem.FromNewLine := False;
        end
        else
        begin
          { new line }
          CurItem.FromNewLine := True;
          if IsCenter then
            x := (Width - sz.cx) div 2
          else
            x :=0;
          y := baseline + prevDesc + TextMetr.tmExternalLeading;
          Inc(baseline, prevDesc + hBetweenLines);
          prevAbove := hBetweenLines;
        end;
        CurItem.Left   := x + sad.LeftMargin;
        CurItem.Top    := y;
        CurItem.Width  := sz.cx;
        CurItem.Height := sz.cy;
        FVisibleItems.Add(CurItem);
        if (StyleNo = rvsJump1) or (StyleNo = rvsJump2) then
        begin
          JmpInfo := TJumpInfo.Create();
          JmpInfo.l := x + sad.LeftMargin;
          JmpInfo.t := y;
          JmpInfo.w := sz.cx;
          JmpInfo.h := sz.cy;
          JmpInfo.id := nJmps;
          JmpInfo.VisibleItemId := FVisibleItems.Count-1;
          JmpInfo.Text := '';
          Item.JumpId := nJmps;
          FJumpList.Add(JmpInfo);
        end;
        sourceStrPtrLen := StrLen(sourceStrPtr);
        if IsNewLine or (prevDesc < TextMetr.tmDescent) then
          prevDesc := TextMetr.tmDescent;
        Inc(x, sz.cx);
        IsNewLine := True;
      end; {while sourceStrPtrLen > 0 do}
      if (StyleNo = rvsJump1) or (StyleNo = rvsJump2) then
        Inc(nJmps);
    end;
  end;
end;

procedure TCustomRichView.AdjustJumpsCoords();
var
  i: Integer;
  JumpInfo: TJumpInfo;
begin
  for i:=0 to FJumpList.Count-1 do
  begin
    JumpInfo := FJumpList[i];
    JumpInfo.l := FVisibleItems[JumpInfo.VisibleItemId].left;
    JumpInfo.t := FVisibleItems[JumpInfo.VisibleItemId].top;
  end;
end;

function TCustomRichView.GetFirstVisible(TopLine: Integer): Integer;
begin
  Result := GetVisibleItemIndex(TopLine, gdlnFirstVisible);
end;

function TCustomRichView.GetItemIdByVisibleId(AVisibleId: Integer): Integer;
begin
  if AVisibleId >= FVisibleItems.Count then
    AVisibleId := FVisibleItems.Count-1;
  while AVisibleId >= 0 do
  begin
    if FVisibleItems[AVisibleId].IsSubItem then
      Dec(AVisibleId)
    else
      Break;
  end;

  if AVisibleId < 0 then
    Result := -1
  else
    Result := Items.IndexOf(FVisibleItems[AVisibleId]);
end;

function TCustomRichView.GetFirstLineVisible(): Integer;
var
  v: Integer;
begin
  v := GetVisibleItemIndex(VPos * SmallStep, gdlnFirstVisible);
  Result := GetItemIdByVisibleId(v);
end;

function TCustomRichView.GetLastLineVisible(): Integer;
var
  v: Integer;
begin
  v := GetVisibleItemIndex(VPos * SmallStep + ClientHeight, gdlnLastVisible);
  Result := GetItemIdByVisibleId(v);
end;

function TCustomRichView.GetVisibleItemIndex(BoundLine: Integer; Option: Integer): Integer;
var
  a, b, mid: Integer;
begin
  if FVisibleItems.Count = 0 then
  begin
    Result := 0;
    Exit;
  end;
  if FVisibleItems[0].Top >= BoundLine then
  begin
    Result := 0;
    Exit;
  end;
  if (Option = gdlnLastVisible) and (FVisibleItems[FVisibleItems.Count-1].Top < BoundLine) then
  begin
    Result := FVisibleItems.Count-1;
    Exit;
  end;
  a := 1;
  b := FVisibleItems.Count-1;
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
      if (FVisibleItems[mid].Top >= BoundLine) then
      begin
        if (FVisibleItems[mid-1].Top < BoundLine) then
          Break;
        b := mid;
      end
      else
        a := mid;
    end;
    if mid >= FVisibleItems.Count then
      mid := FVisibleItems.Count-1;
    if Option = gdlnFirstVisible then
    begin
      while (mid > 0) and (not FVisibleItems[mid].FromNewLine) do
        Dec(mid);
      if (mid > 0) then
        Dec(mid);
      while (mid > 0) and (not FVisibleItems[mid].FromNewLine) do
        Dec(mid);
      if (mid > 0) then
        Dec(mid);
    end
    else
    begin
      while FVisibleItems[mid].Top < BoundLine do
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


procedure TCustomRichView.Paint();
var
  i, StyleNo, yshift, xshift: Integer;
  cl, TextColor: TColor;
  Item: TRVItem;
  VisItem: TRVVisualItem;
  IsLastLine, IsHoverNow: Boolean;
  rc, r: TRect;
  buffer: TBitmap;
  canv: TCanvas;
  s, s1: String;
  StartNo, EndNo, StartOffs, EndOffs: Integer;
  {$IFDEF FPC}
  St: string;
  {$ENDIF}
  MouseItemId: Integer;
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

  GetVisibleSelBounds(StartNo, EndNo, StartOffs, EndOffs);
  IsLastLine := False;
  rc := Canvas.ClipRect;
  buffer := TBitmap.Create();
  try
    buffer.Width := rc.Right-rc.Left+1;
    buffer.Height := rc.Bottom-rc.Top+1;
    canv := buffer.Canvas;
    DrawBack(canv.Handle, Canvas.ClipRect, ClientWidth, ClientHeight);
    yshift := VPos * SmallStep;
    Inc(rc.Top, yshift);
    Inc(rc.Bottom, yshift);
    Inc(yshift, Canvas.ClipRect.Top);
    xshift := HPos + Canvas.ClipRect.Left;
    canv.Brush.Style := bsClear;

    MouseItemId := FindVisItemAtPos(XMouse, YMouse);

    for i := GetFirstVisible(rc.Top) to FVisibleItems.Count-1 do
    begin
      Item := FVisibleItems[i];
      if IsLastLine and (Item.Left <= FVisibleItems[i-1].left) then
        Break;
      if Item.Top > rc.Bottom then
        IsLastLine := True;

      StyleNo := Item.StyleNo;

      if StyleNo >= 0 then
      begin { text }
        canv.Font.Style := FStyle.TextStyles[StyleNo].Style;
        canv.Font.Size := FStyle.TextStyles[StyleNo].Size;
        canv.Font.Name := FStyle.TextStyles[StyleNo].FontName;
        {$IFDEF RICHVIEWDEF3}
        canv.Font.CharSet := FStyle.TextStyles[StyleNo].CharSet;
        {$ENDIF}

        if not ((StyleNo in [rvsJump1, rvsJump2]) and DrawHover and (LastJumpMovedAbove <> -1) and (Item.JumpId = LastJumpMovedAbove)) then
        begin
          TextColor := FStyle.TextStyles[StyleNo].Color;
          IsHoverNow := False;
        end
        else
        begin
          TextColor := FStyle.HoverColor;
          IsHoverNow := True;
          canv.Font.Color := TextColor;
        end;

        if (StartNo > i) or (EndNo < i) then
        begin
          canv.Font.Color := TextColor;
          canv.TextOut(Item.Left-xshift, Item.Top-yshift, Item.Text);
        end
        else
        if ((StartNo < i) and (EndNo > i))
        or ((StartNo = i) and (EndNo <> i) and (StartOffs <= 1))
        or ((StartNo <> i) and (EndNo = i) and (EndOffs > Length(FVisibleItems[i].Text)))
        then
        begin
          canv.Brush.Style := bsSolid;
          canv.Brush.Color := FStyle.SelColor;
          if not IsHoverNow then
            canv.Font.Color := FStyle.SelTextColor;
          {$IFDEF FPC}
          TxtOut(canv, Item.Left-xshift, Item.Top-yshift, Item.Text);
          {$ELSE}
          canv.TextOut(Item.Left-xshift, Item.Top-yshift, Item.Text);
          {$ENDIF}
          canv.Brush.Style := bsClear;
        end
        else
        if (StartNo = i) then
        begin
          canv.Font.Color := TextColor;
          s := Copy(Item.Text, 1, StartOffs-1);
          canv.TextOut(Item.Left-xshift, Item.Top-yshift, s);
          canv.Brush.Style := bsSolid;
          canv.Brush.Color := FStyle.SelColor;
          if not IsHoverNow then
            canv.Font.Color := FStyle.SelTextColor;
          if (i <> EndNo) or (EndOffs > Length(Item.Text)) then
          begin
            {$IFDEF FPC}
            St := Copy(Item.Text, StartOffs, Length(Item.Text));
            TxtOut(canv, Item.Left-xshift + canv.TextWidth(s), Item.Top-yshift, st);
            {$ELSE}
            canv.TextOut(Item.Left-xshift + canv.TextWidth(s), Item.Top-yshift,
                         Copy(Item.Text, StartOffs, Length(Item.Text)));
            {$ENDIF}
            canv.Brush.Style := bsClear;
          end
          else
          begin
            s1 := Copy(Item.Text, StartOffs, EndOffs-StartOffs);
            {$IFDEF FPC}
            TxtOut(canv, Item.Left - xshift + canv.TextWidth(s), Item.Top-yshift, s1);
            {$ELSE}
            canv.TextOut(Item.Left - xshift + canv.TextWidth(s), Item.Top-yshift, s1);
            {$ENDIF}
            canv.Font.Color := TextColor;
            canv.Brush.Style := bsClear;
            canv.TextOut(Item.Left - xshift + canv.TextWidth(s+s1), Item.Top-yshift,
                         Copy(Item.Text, EndOffs, Length(Item.Text)));
          end;
        end
        else
        if (EndNo = i) then
        begin
          s := Copy(Item.Text, 1, EndOffs-1);
          canv.Brush.Style := bsSolid;
          canv.Brush.Color := FStyle.SelColor;
          if (not IsHoverNow) then
            canv.Font.Color := FStyle.SelTextColor;
          {$IFDEF FPC}
          TxtOut(canv, Item.Left - xshift, Item.Top - yshift, s);
          {$ELSE}
          canv.TextOut(Item.Left - xshift, Item.Top - yshift, s);
          {$ENDIF}
          canv.Brush.Style := bsClear;
          canv.Font.Color := TextColor;
          canv.TextOut(Item.Left - xshift + canv.TextWidth(s), Item.Top - yshift,
                       Copy(Item.Text, EndOffs, Length(Item.Text)));
        end;
        // ~~~
        if i = MouseItemId then
        begin
          r.Left := Item.Left - xshift;
          r.Top := Item.Top - yshift;
          r.Right := r.Left + Item.Width;
          r.Bottom := r.Top + Item.Height;
          canv.Brush.Style := bsSolid;
          canv.Brush.Color := clBlue;
          canv.FrameRect(r);
          canv.Brush.Style := bsClear;
        end;
        Continue;
      end;

      if (StyleNo = rvsPicture)  then
      begin { graphics }
        canv.Draw(Item.Left-xshift, Item.Top-yshift, TGraphic(TRVVisualItem(Item).gr));
        Continue;
      end;

      if (StyleNo = rvsHotSpot) or (StyleNo = rvsBullet)  then
      begin { hotspots and bullets }
        VisItem := TRVVisualItem(Item);
        if (StartNo <= i) and (EndNo >= i)
        and (not ((EndNo = i) and (EndOffs = 0)))
        and  (not ((StartNo = i) and (StartOffs = 2)))
        then
        begin
          TImageList(VisItem.gr).BlendColor := FStyle.SelColor;
          TImageList(VisItem.gr).DrawingStyle := dsSelected;
        end;
        TImageList(VisItem.gr).Draw(canv, Item.Left-xshift, Item.Top-yshift, VisItem.imgNo);
        TImageList(VisItem.gr).DrawingStyle := ImgList.dsNormal;
        Continue;
      end;

      if StyleNo = rvsCheckPoint then
        Continue; { check point }

      if StyleNo = rvsBreak then
      begin {break line}
        canv.Pen.Color := FStyle.TextStyles[0].Color;
        canv.MoveTo(Item.Left + 5 - xshift, Item.Top + 5 - yshift);
        canv.LineTo(XSize - 5 - xshift - FRightMargin, Item.Top + 5 - yshift);
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
    for i := 0 to FJumpList.Count - 1 do
    begin
      JumpInfo := FJumpList[i];
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

  XMouse := x;
  YMouse := y;

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
    //FindVisItemForSel(X + HPos, ys + VPos * SmallStep, no, offs);
    FindOrigItemForSel(X + HPos, ys + VPos * SmallStep, no, offs);
    FSelEndNo := no;
    FselEndOffs := offs;
    Invalidate();
  end;

  for i := 0 to FJumpList.Count-1 do
  begin
    JumpInfo := FJumpList[i];
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

  //if Selection then
    Invalidate();
end;

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
    //FindVisItemForSel(XClicked + HPos, ys + VPos * SmallStep, no, offs);
    FindOrigItemForSel(XClicked + HPos, ys + VPos * SmallStep, no, offs);
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

  for i:=0 to FJumpList.Count-1 do
  begin
    JumpInfo := FJumpList[i];
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
  for i := 0 to FJumpList.Count-1 do
  begin
    JumpInfo := FJumpList[i];
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
    //FindVisItemForSel(XClicked + HPos, YClicked + VPos * SmallStep, no, FSelStartOffs);
    FindOrigItemForSel(XClicked + HPos, YClicked + VPos * SmallStep, no, FSelStartOffs);
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

procedure TCustomRichView.AppendFrom(Source: TCustomRichView);
var
  i: Integer;
  gr: TGraphic;
  grclass: TGraphicClass;
  Item: TRVItem;
  VisItem: TRVVisualItem;
begin
  ClearTemporal();
  for i := 0 to Source.Items.Count-1 do
  begin
    Item := Source.Items[i];
    case Item.StyleNo of
      rvsBreak:
        AddBreak();

      rvsCheckPoint:
        AddCheckPoint();

      rvsPicture:
      begin
        VisItem := (Item as TRVVisualItem);
        grclass := TGraphicClass(VisItem.gr.ClassType);
        gr := grclass.Create();
        gr.Assign(VisItem.gr);
        AddPicture(gr, False);
      end;

      rvsHotSpot:
      begin
        VisItem := (Item as TRVVisualItem);
        AddHotSpot(VisItem.imgNo, TImageList(VisItem.gr), (not VisItem.SameAsPrev));
      end;

      rvsComponent:
      begin
        { if li.gr is TControl then
        ctrlclass := TControlClass(li.gr.ClassType);
        ctrl := ctrlclass.Create(Self);
        ctrl.Assign(li.gr);
        AddControl(ctrl, li.Center); }
      end;

      rvsBullet:
      begin
        VisItem := (Item as TRVVisualItem);
        AddBullet(VisItem.imgNo, TImageList(VisItem.gr), (not VisItem.SameAsPrev));
      end;
    else
      if Item.Alignment = taCenter then
        AddCenterLine(Item.Text, Item.StyleNo)
      else if Item.SameAsPrev then
        Add(Item.Text, Item.StyleNo)
      else
        AddFromNewLine(Item.Text, Item.StyleNo);
    end;
  end;
end;

function TCustomRichView.GetLastCP(): Integer;
begin
  GetLastCP := CheckPoints.Count-1;
end;

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

procedure TCustomRichView.SetVSmallStep(Value: Integer);
begin
  if (Value <= 0) or (TextHeight div Value > 30000) then
    Exit;
  SmallStep := Value;
end;

procedure TCustomRichView.ShareLinesFrom(Source: TCustomRichView);
begin
  ShareContents := True;
  if ShareContents then
  begin
    Clear();
    Items := Source.Items;
  end;
end;

function TCustomRichView.FindVisItemAtPos(X, Y: Integer): Integer;
var
  i, a, b, mid, midtop: Integer;
  Item: TRVItem;
begin
  Result := -1;
  if FVisibleItems.Count = 0 then Exit;

  a := 0;
  b := FVisibleItems.Count-1;
  if (FVisibleItems[b].Top + FVisibleItems[b].Height) < Y then
  begin
    Result := -2;
    Exit;
  end;
    
  while (b - a) > 1 do
  begin
    mid := (a + b) div 2;
    if (FVisibleItems[mid].Top <= Y) then
      a := mid
    else
      b := mid;
  end;
  if FVisibleItems[b].Top <= Y then
    mid := b
  else
    mid := a;

  midtop := FVisibleItems[mid].Top;
  while (mid >= 1) and (FVisibleItems[mid-1].Top + FVisibleItems[mid-1].Height > midtop) do
    Dec(mid);

  Item := FVisibleItems[mid];
  midtop := Item.Top + Item.Height-1;
  while (mid < FVisibleItems.Count) do
  begin
    Item := FVisibleItems[mid];
    if (Item.Top > midtop) then
      Break;
    if (Item.Top <= Y) and (Item.Top + Item.Height > Y)
    and (Item.Left <= X) and (Item.Left + Item.Width > X) then
    begin
      Result := mid;
      Exit;
    end;
    Inc(mid);
  end;
end;

procedure TCustomRichView.FindVisItemForSel(X, Y: Integer; var ItemNo, TextOffs: Integer);
var
  StyleNo, i, a, b: Integer;
  mid, midtop, midbottom, midleft, midright: Integer;
  beginline, endline: Integer;
  Item, ItemMid: TRVItem;
  {$IFNDEF RICHVIEWDEF4}
  arr: array[0..1000] of integer;
  {$ENDIF}
  sz: TSIZE;
begin
  mid := FindVisItemAtPos(X, Y);
  if mid < 0 then
  begin
    if mid = -2 then
    begin
      mid := FVisibleItems.Count - 1;
      TextOffs := Length(FVisibleItems[mid].Text)+1;
    end;
    ItemNo := mid;
    Exit;
  end;

  ItemMid := FVisibleItems[mid];
  midtop := ItemMid.Top;
  midbottom := midtop + ItemMid.Height;

  // searching beginning of line "mid" belong to
  beginline := mid;
  while (beginline >= 1)
  and (FVisibleItems[beginline-1].Top + FVisibleItems[beginline-1].Height > midtop) do
    Dec(beginline);

  // searching end of line "mid" belong to
  endline := mid;
  while (endline < FVisibleItems.Count-1)
  and (FVisibleItems[endline+1].Top < midbottom) do
    Inc(endline);

  // calculating line bounds
  midleft := ItemMid.Left;
  midright := midleft + ItemMid.Width;
  for i := beginline to endline do
  begin
    Item := FVisibleItems[i];
    if Item.Top < midtop then
      midtop := Item.Top;
    if Item.Top + Item.Height > midbottom then
      midbottom := Item.Top + Item.Height;
    if Item.Left < midleft then
      midleft := Item.Left;
    if Item.Left + Item.Width > midright then
      midright := Item.Left + Item.Width;
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
    ItemNo := beginline;
    if FVisibleItems[ItemNo].StyleNo < 0 then
      TextOffs := 0
    else
      TextOffs := 1;
    Exit;
  end;

  if (Y > midbottom) or (X > midright) then
  begin
    ItemNo := endline + 1;
    TextOffs := 1;
    if ItemNo >= FVisibleItems.Count then
    begin
      ItemNo := FVisibleItems.Count-1;
      TextOffs := Length(FVisibleItems[ItemNo].Text)+1;
    end
    else
    begin
      if FVisibleItems[ItemNo].StyleNo < 0 then
        TextOffs := 0;
    end;
    Exit;
  end;

  for i := beginline to endline do
  begin
    Item := FVisibleItems[i];
    if (Item.Left <= X) and (Item.Left + Item.Width >= X) then
    begin
      StyleNo := Item.StyleNo;
      ItemNo := i;
      TextOffs := 0;
      if StyleNo >= 0 then
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
        MyGetTextExtentExPoint(Canvas.Handle, PChar(Item.Text), Length(Item.Text),
        {$ELSE}
        GetTextExtentExPoint(Canvas.Handle, PChar(Item.Text), Length(Item.Text),
        {$ENDIF}
                            X-Item.Left,
                            {$IFDEF RICHVIEWDEF4}
                            @TextOffs, nil,
                            {$ELSE}
                            TextOffs, arr[0],
                            {$ENDIF}
                             sz);
        Inc(TextOffs);
        if TextOffs > Length(Item.Text) then
          TextOffs := Length(Item.Text);
        if (TextOffs < 1) and (Length(Item.Text) > 0) then
          TextOffs := 1;
      end
      else
        TextOffs := 1;
    end;
  end;
end;

procedure TCustomRichView.FindOrigItemForSel(X, Y: Integer; var ItemNo, TextOffs: Integer);
var
  Item: TRVItem;
  n, offs: Integer;
begin
  FindVisItemForSel(X, Y, n, offs);
  if (n >= 0) and (n < FVisibleItems.Count) then
  begin
    Item := FVisibleItems[n];
    if Item.IsSubItem then
      ItemNo := Items.IndexOf(Item.ParentItem)
    else
      ItemNo := Items.IndexOf(Item);

    TextOffs := Item.TextOffs - 1 + offs;
  end
  else
  begin
    ItemNo := -1;
    TextOffs := 0;
  end;
end;

{------------------------------------------------------------------}
function TCustomRichView.FindClickedWord(var sClickedWord: String;
  var StyleNo: Integer): Boolean;
var
  no: Integer;
  {$IFNDEF RICHVIEWDEF4}
  arr: array[0..1000] of integer;
  {$ENDIF}
  sz: TSIZE;
  max, first, len: Integer;
begin
  Result := False;
  no := FindVisItemAtPos(XClicked + HPos, YClicked + VPos * SmallStep);
  if no <> -1 then
  begin
    sClickedWord := FVisibleItems[no].Text;
    StyleNo := FVisibleItems[no].StyleNo;
    if StyleNo >= 0 then
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
                          XClicked + HPos - FVisibleItems[no].Left,
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
      startno := CheckPoints[i].ItemIndex;
      endno := Items.Count-1;
      for j := i+1 to CheckPoints.Count-1 do
      begin
        if CheckPoints[j].Text <> '' then
        begin
          endno := CheckPoints[j].ItemIndex-1;
          Break;
        end;
      end;
      DeleteItems(startno, endno-startno+1);
      Exit;
    end;
  end;
end;
  {------------------------------------------------------------------}
procedure TCustomRichView.DeleteItems(AFirstIndex, ACount: Integer);
var
  i, LastIndex: Integer;
  Item: TRVItem;
  VisItem: TRVVisualItem;
begin
  if ShareContents then
    Exit;
  if AFirstIndex >= Items.Count then
    Exit;

  Deselect();
  if AFirstIndex + ACount > Items.Count then
    ACount := Items.Count - AFirstIndex;

  LastIndex := AFirstIndex + ACount - 1;
  if LastIndex > Items.Count then
    LastIndex := Items.Count-1;

  for i := LastIndex downto AFirstIndex do
  begin
    Item := Items[i];
    FVisibleItems.RemoveItem(Item);

    if Item.StyleNo = rvsPicture then { image}
    begin
      VisItem := (Item as TRVVisualItem);
      if not VisItem.Shared then
        VisItem.gr.Free();
      VisItem.gr := nil;
    end;

    if Item.StyleNo = rvsComponent then {control}
    begin
      VisItem := (Item as TRVVisualItem);
      RemoveControl(TControl(VisItem.gr));
      if not VisItem.Shared then
        VisItem.gr.Free();
      VisItem.gr := nil;
    end;

    Items.Delete(i);
  end;
end;
{
procedure TCustomRichView.GetVisibleSelBounds(out StartNo, EndNo, StartOffs, EndOffs: Integer);
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
  if EndNo >= FVisibleItems.Count then
    EndNo := FVisibleItems.Count-1;
end;
}
procedure TCustomRichView.GetVisibleSelBounds(out StartNo, EndNo, StartOffs, EndOffs: Integer);
var
  Item, SelStartItem, SelEndItem: TRVItem;
  i, n, offs: Integer;
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
  if StartNo >= Items.Count then
    StartNo := Items.Count-1;
  if EndNo >= Items.Count then
    EndNo := Items.Count-1;

  if (StartNo <> -1) and (EndNo <> -1) then
  begin
    SelStartItem := Items[StartNo];
    SelEndItem := Items[EndNo];
    // Start
    n := StartNo;
    for i := StartNo to FVisibleItems.Count-1 do
    begin
      Item := FVisibleItems[i];
      if (Item = SelStartItem) or (Item.ParentItem = SelStartItem) then
      begin
        n := i;
        Break;
      end;
    end;
    StartNo := n;
    offs := StartOffs;
    while n < FVisibleItems.Count do
    begin
      Item := FVisibleItems[n];
      if Item.IsSubItem then
      begin
        if Item.TextOffs < offs then
        begin
          StartOffs := (offs - Item.TextOffs) + 1;
          StartNo := n;
        end
        else
          Break;
      end
      else
        Break;
      Inc(n);
    end;

    // End
    n := EndNo;
    for i := EndNo to FVisibleItems.Count-1 do
    begin
      Item := FVisibleItems[i];
      if (Item = SelEndItem) or (Item.ParentItem = SelEndItem) then
      begin
        n := i;
        Break;
      end;
    end;
    EndNo := n;
    offs := EndOffs;
    while n < FVisibleItems.Count do
    begin
      Item := FVisibleItems[n];
      if Item.IsSubItem then
      begin
        if Item.TextOffs < offs then
        begin
          EndOffs := (offs - Item.TextOffs) + 1;
          EndNo := n;
        end
        else
          Break;
      end
      else
        Break;
      Inc(n);
    end;
  end;
end;

procedure TCustomRichView.GetItemsSelBounds(var StartNo, EndNo, StartOffs, EndOffs: Integer);
var
  Item: TRVItem;
  //n: Integer;
begin
  GetVisibleSelBounds(StartNo, EndNo, StartOffs, EndOffs);
  if StartNo <> -1 then
  begin
    Item := FVisibleItems[StartNo];
    if Item.StyleNo >= 0 then
      Inc(StartOffs, Item.TextOffs-1);
    // find parent item
    if Item.IsSubItem then
      StartNo := Items.IndexOf(Item.ParentItem)
    else
      StartNo := Items.IndexOf(Item);

    Item := FVisibleItems[EndNo];
    if Item.StyleNo >= 0 then
      Inc(EndOffs, Item.TextOffs-1);
    // find parent item
    if Item.IsSubItem then
      EndNo := Items.IndexOf(Item.ParentItem)
    else
      EndNo := Items.IndexOf(Item);
  end;
end;
{------------------------------------------------------------------}
procedure TCustomRichView.SetItemsSelBounds(StartNo, EndNo, StartOffs, EndOffs: Integer);
var
  i: Integer;
  Item: TRVItem;
  ItemStart, ItemEnd: TRVItem;
begin
  if StartNo = -1 then
    Exit;
  ItemStart := Items[StartNo];
  ItemEnd := Items[EndNo];
  for i := 0 to FVisibleItems.Count-1 do
  begin
    Item := FVisibleItems[i];
    if (Item = ItemStart) or (Item.ParentItem = ItemStart) then
    begin
      if Item.StyleNo < 0 then
      begin
        FSelStartNo := i;
        FSelStartOffs := StartOffs;
      end
      else
      begin
        if (Item.TextOffs <= StartOffs) then
        begin
          FSelStartNo := i;
          FSelStartOffs := StartOffs;
        end;
      end;
    end;

    if (Item = ItemEnd) or (Item.ParentItem = ItemEnd) then
    begin
      if Item.StyleNo < 0 then
      begin
        FSelEndNo := i;
        FSelEndOffs := EndOffs;
      end
      else
      begin
        if ((Item.TextOffs <= EndOffs) and (Length(Item.Text) + Item.TextOffs > EndOffs))
        then
        begin
          FSelEndNo := i;
          FSelEndOffs := EndOffs;
        end;
      end;
    end;
  end;
end;
  {------------------------------------------------------------------}
function TCustomRichView.GetLineCount(): Integer;
begin
  Result := Items.Count;
end;
  {------------------------------------------------------------------}
function TCustomRichView.SelectionExists(): Boolean;
var
  StartNo, EndNo, StartOffs, EndOffs: Integer;
begin
  GetVisibleSelBounds(StartNo, EndNo, StartOffs, EndOffs);
  if (StartNo = -1) or (EndNo = -1) or ((StartNo = EndNo) and (StartOffs = EndOffs)) then
    Result := False
  else
    Result := True;
end;
{------------------------------------------------------------------}
function TCustomRichView.GetSelText(): string;
var
  StartNo, EndNo, StartOffs, EndOffs, i: Integer;
  s: string;
  Item: TRVItem;
begin
  Result := '';
  if not SelectionExists then
    Exit;

  { getting selection as Lines indices }
  StartNo := 0;
  EndNo := 0;
  StartOffs := 0;
  EndOffs := 0;
  GetItemsSelBounds(StartNo, EndNo, StartOffs, EndOffs);
  if StartNo = EndNo then
  begin
    Item := Items[StartNo];
    if Item.StyleNo < 0 then
      Exit;
    Result := Copy(Item.Text, StartOffs, EndOffs-StartOffs);
    Exit;
  end
  else
  begin
    Item := Items[StartNo];
    if Item.StyleNo < 0 then
      s := ''
    else
      s := Copy(Item.Text, StartOffs, Length(Item.Text));

    for i := StartNo + 1 to EndNo do
    begin
      Item := Items[i];
      if (Item.StyleNo <> rvsCheckpoint) and (not Item.SameAsPrev) then
        s := s + sLineBreak;
      if Item.StyleNo >= 0 then
      begin
        if i <> EndNo then
          s := s + Item.Text
        else
          s := s + Copy(Item.Text, 1, EndOffs-1);
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

function TCustomRichView.GetDebugInfo(): string;
var
  VisItemId, VisPos: Integer;
  ItemId, TextPos: Integer;
  StartNo, EndNo, StartOffs, EndOffs: Integer;
  Item: TRVItem;
begin
  FindVisItemForSel(XMouse, YMouse, VisItemId, VisPos);

  FindOrigItemForSel(XMouse, YMouse, ItemId, TextPos);

  GetVisibleSelBounds(StartNo, EndNo, StartOffs, EndOffs);
  Result := 'Items.Count: '+IntToStr(Items.Count) + sLineBreak
  + 'FVItems.Count: ' + IntToStr(FVisibleItems.Count) + sLineBreak
  + 'MouseXY: '+ IntToStr(XMouse) + '.' + IntToStr(YMouse) + sLineBreak
  + 'VisItem(Id/Pos): '+ IntToStr(VisItemId) + '/' + IntToStr(VisPos) + sLineBreak
  + 'VisSelBounds: ' + IntToStr(StartNo) + '.' + IntToStr(StartOffs)
  + ' - ' + IntToStr(EndNo) + '.' + IntToStr(EndOffs) + sLineBreak
  + 'OrigSelBounds: ' + IntToStr(FSelStartNo) + '.' + IntToStr(FSelStartOffs)
  + ' - ' + IntToStr(FSelEndNo) + '.' + IntToStr(FSelEndOffs) + sLineBreak;
  if (VisItemId >= 0) and (VisItemId < FVisibleItems.Count) then
  begin
    Item := FVisibleItems[VisItemId];
    Result := Result + 'VisItem: X=' + IntToStr(Item.Left) + ' Y=' + IntToStr(Item.Top)
    + ' Offs=' + IntToStr(Item.TextOffs) + sLineBreak;

    Result := Result + 'Item ID/Pos:' + IntToStr(ItemId) + '/' + IntToStr(TextPos);
    Item := Items[ItemId];
    Result := Result + ' X=' + IntToStr(Item.Left) + ' Y=' + IntToStr(Item.Top)
    + ' Offs=' + IntToStr(Item.TextOffs) + sLineBreak;
  end;
end;

{------------------------------------------------------------------}
{$I RV_Save.inc}
{------------------------------------------------------------------}

end.
