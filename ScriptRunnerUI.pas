unit ScriptRunnerUI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  CheckLst, ButtonPanel, BaseForm, fpjson, Math, LazUTF8, Spin, ResTranslator;

resourcestring
  SRequiredScriptParameter = 'Enter a value for "%s"';
  SInvalidScriptParameter = 'Invalid value for "%s"';
  SHostScriptSettingsHint = 'These values are stored on the daemon host and are shared by all clients';
  SScriptParameters = 'Script parameters';
  SNoExternalHosts = 'No external hosts are available for "%s"';
  SScriptSettingsUnavailable = 'Script settings are unavailable';

type
  TScriptParamKind = (spkString, spkInteger, spkNumber, spkBoolean, spkHost);

  TScriptParamRow = class(TPanel)
  private
    FParam: TJSONObject;
    FKind: TScriptParamKind;
    FArray: Boolean;
    FLabel: TLabel;
    FDescLabel: TLabel;
    FEdit: TEdit;
    FSpin: TSpinEdit;
    FFloatSpin: TFloatSpinEdit;
    FMemo: TMemo;
    FCheck: TCheckBox;
    FCombo: TComboBox;
    FCheckList: TCheckListBox;
    FControl: TWinControl;
    FHostIds: TStringList;
    FLoading: Boolean;
    FOnChanged: TNotifyEvent;
    procedure BuildHostChoices(AExternalHosts: TJSONArray);
    procedure ApplyValue(AData: TJSONData);
    procedure DoChanged(Sender: TObject);
    procedure SetError(Const AMessage: String; Out AError: String);
    function ParseNumber(Const S: String; Out AValue: Double): Boolean;
    function CheckNumber(AValue: Double; Out AError: String): Boolean;
    function CollectArray(Out AValue: TJSONData; Out AError: String): Boolean;
    function CollectScalar(Out AValue: TJSONData; Out AError: String): Boolean;
  protected
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent; AParam: TJSONObject;
      AExternalHosts: TJSONArray; AValue: TJSONData); reintroduce;
    destructor Destroy; override;
    function Collect(Out AValue: TJSONData; Out AError: String): Boolean;
    function ParamKey: String;
    function ParamName: String;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  TScriptParamsForm = class(TBaseForm)
  private
    FScroll: TScrollBox;
    FButtons: TButtonPanel;
    FRows: TList;
    FCollected: TJSONObject;
    procedure BuildRows(AScript: TJSONObject; AExternalHosts: TJSONArray; ASaved: TJSONObject);
    procedure OKClick(Sender: TObject);
    function CollectAll: TJSONObject;
    function CollectAll(Out AError: String): TJSONObject;
    function TakeParams: TJSONObject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function Execute(AOwner: TComponent; AScript: TJSONObject;
      AExternalHosts: TJSONArray; ASaved: TJSONObject; Out AParams: TJSONObject): Boolean;
    class function BuildFromSaved(AOwner: TComponent; AScript: TJSONObject;
      AExternalHosts: TJSONArray; ASaved: TJSONObject; Out AParams: TJSONObject;
      Out AError: String): Boolean;
  end;

  TScriptResultForm = class(TBaseForm)
  private
    FMemo: TMemo;
    FButtons: TButtonPanel;
    FRunning: Boolean;
    FCancelRequested: Boolean;
    procedure FormClose(Sender: TObject; Var ACloseAction: TCloseAction);
    procedure OKClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetOutput(Const AText: String; AFollow: Boolean = False);
    procedure SetRunning(AValue: Boolean);
    property CancelRequested: Boolean read FCancelRequested;
  end;

  TScriptSettingsFrame = class(TPanel)
  private
    FHint: TLabel;
    FScroll: TScrollBox;
    FGroups: TList;
    FScripts: TJSONArray;
    FExternalHosts: TJSONArray;
    FRevision: String;
    FHasHostParams: Boolean;
    procedure ClearData;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadData(AScripts: TJSONArray; ASettings: TJSONObject;
      AExternalHosts: TJSONArray);
    function BuildRequest(Out ARequest: TJSONObject; Out AError: String): Boolean;
    procedure SetHintText(Const AText: String);
    property HasHostParams: Boolean read FHasHostParams;
  end;

function ScriptString(Obj: TJSONObject; Const AName, ADefault: String): String;
function ScriptParamStorage(Obj: TJSONObject): String;
function ScriptParamType(Obj: TJSONObject): String;
function ScriptParamIsArray(Obj: TJSONObject): Boolean;
function ScriptParamRequired(Obj: TJSONObject): Boolean;
function ScriptHasRuntimeParams(AScript: TJSONObject): Boolean;
function ScriptHasHostParams(AScript: TJSONObject): Boolean;

implementation

function ScriptString(Obj: TJSONObject; Const AName, ADefault: String): String;
var
  d: TJSONData;
begin
  Result := ADefault;
  if Obj = nil then
    exit;
  d := Obj.Find(AName);
  if (d <> nil) and (d.JSONType = jtString) then
    Result := d.AsString;
end;

function ScriptParamStorage(Obj: TJSONObject): String;
begin
  Result := ScriptString(Obj, 'storage', '');
  if Result = '' then
    Result := ScriptString(Obj, 'scope', 'runtime');
  if Result <> 'host' then
    Result := 'runtime';
end;

function ScriptParamType(Obj: TJSONObject): String;
begin
  Result := ScriptString(Obj, 'type', 'string');
end;

function ScriptParamIsArray(Obj: TJSONObject): Boolean;
var
  d: TJSONData;
begin
  Result := False;
  if Obj = nil then
    exit;
  d := Obj.Find('array');
  Result := (d <> nil) and (d.JSONType = jtBoolean) and d.AsBoolean;
end;

function ScriptParamRequired(Obj: TJSONObject): Boolean;
var
  d: TJSONData;
begin
  Result := False;
  if Obj = nil then
    exit;
  d := Obj.Find('required');
  Result := (d <> nil) and (d.JSONType = jtBoolean) and d.AsBoolean;
end;

function ScriptParamDefault(Obj: TJSONObject): TJSONData;
begin
  Result := nil;
  if Obj <> nil then
    Result := Obj.Find('default');
end;

function ScriptHasRuntimeParams(AScript: TJSONObject): Boolean;
var
  i: Integer;
  p: TJSONObject;
  d: TJSONData;
begin
  Result := False;
  if AScript = nil then
    exit;
  d := AScript.Find('parameters');
  if not (d is TJSONArray) then
    exit;
  for i := 0 to TJSONArray(d).Count - 1 do
  begin
    p := TJSONArray(d).Objects[i];
    if (p <> nil) and (ScriptParamStorage(p) = 'runtime') then
      exit(True);
  end;
end;

function ScriptHasHostParams(AScript: TJSONObject): Boolean;
var
  i: Integer;
  p: TJSONObject;
  d: TJSONData;
begin
  Result := False;
  if AScript = nil then
    exit;
  d := AScript.Find('parameters');
  if not (d is TJSONArray) then
    exit;
  for i := 0 to TJSONArray(d).Count - 1 do
  begin
    p := TJSONArray(d).Objects[i];
    if (p <> nil) and (ScriptParamStorage(p) = 'host') then
      exit(True);
  end;
end;

function ParamMin(Obj: TJSONObject; Out AValue: Double): Boolean;
var
  d: TJSONData;
begin
  Result := False;
  AValue := 0;
  if Obj = nil then
    exit;
  d := Obj.Find('min');
  if (d <> nil) and (d.JSONType = jtNumber) then
  begin
    AValue := d.AsFloat;
    Result := True;
  end;
end;

function ParamMax(Obj: TJSONObject; Out AValue: Double): Boolean;
var
  d: TJSONData;
begin
  Result := False;
  AValue := 0;
  if Obj = nil then
    exit;
  d := Obj.Find('max');
  if (d <> nil) and (d.JSONType = jtNumber) then
  begin
    AValue := d.AsFloat;
    Result := True;
  end;
end;

function HostDisplayName(Const AId: String; AExternalHosts: TJSONArray): String;
var
  i: Integer;
  o: TJSONObject;
  Id, Name: String;
begin
  Result := AId;
  if AExternalHosts = nil then
    exit;
  for i := 0 to AExternalHosts.Count - 1 do
  begin
    o := AExternalHosts.Objects[i];
    if o = nil then
      continue;
    Id := ScriptString(o, 'id', '');
    if Id <> AId then
      continue;
    Name := ScriptString(o, 'name', '');
    if Name <> '' then
      Result := Name;
    exit;
  end;
end;

{ TScriptParamRow }

constructor TScriptParamRow.Create(AOwner: TComponent; AParam: TJSONObject;
  AExternalHosts: TJSONArray; AValue: TJSONData);
var
  Kind: String;
  Def: TJSONData;
  H: Integer;
  MinV, MaxV: Double;
begin
  inherited Create(AOwner);
  FLoading := True;
  FParam := AParam;
  FHostIds := TStringList.Create;
  BevelOuter := bvNone;
  Align := alTop;
  BorderSpacing.Top := 6;
  BorderSpacing.Left := 8;
  BorderSpacing.Right := 8;
  Kind := ScriptParamType(AParam);
  if Kind = 'integer' then
    FKind := spkInteger
  else if Kind = 'number' then
    FKind := spkNumber
  else if Kind = 'boolean' then
    FKind := spkBoolean
  else if Kind = 'host' then
    FKind := spkHost
  else
    FKind := spkString;
  FArray := ScriptParamIsArray(AParam);

  FLabel := TLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.AutoSize := False;
  FLabel.WordWrap := True;
  FLabel.Width := 500;
  FLabel.Height := 32;
  FLabel.Caption := TranslateString(ParamName, True);
  if ScriptParamRequired(AParam) then
    FLabel.Caption := FLabel.Caption + ' *';
  FLabel.Top := 0;
  FLabel.Left := 0;
  FDescLabel := TLabel.Create(Self);
  FDescLabel.Parent := Self;
  FDescLabel.AutoSize := False;
  FDescLabel.WordWrap := True;
  FDescLabel.Left := 0;
  FDescLabel.Top := FLabel.Height + 2;
  FDescLabel.Width := 500;
  FDescLabel.Font.Color := clGrayText;
  FDescLabel.Caption := TranslateString(
    ScriptString(AParam, 'description', ''), True);
  if FDescLabel.Caption <> '' then
    FDescLabel.Height := 34
  Else
    FDescLabel.Height := 0;

  if FArray and (FKind = spkHost) then
  begin
    FCheckList := TCheckListBox.Create(Self);
    FCheckList.Parent := Self;
    FCheckList.Left := 0;
    FCheckList.Top := FLabel.Height + 4;
    FCheckList.Height := 90;
    FCheckList.Anchors := [akLeft, akTop, akRight];
    BuildHostChoices(AExternalHosts);
  end
  else if FArray then
  begin
    FMemo := TMemo.Create(Self);
    FMemo.Parent := Self;
    FMemo.Left := 0;
    FMemo.Top := FLabel.Height + 4;
    FMemo.Height := 72;
    FMemo.ScrollBars := ssBoth;
    FMemo.WordWrap := False;
    FMemo.Anchors := [akLeft, akTop, akRight];
  end
  else if FKind = spkInteger then
  begin
    FSpin := TSpinEdit.Create(Self);
    FSpin.Parent := Self;
    FSpin.Left := 0;
    FSpin.Top := FLabel.Height + 4;
    FSpin.Height := 23;
    FSpin.MinValue := Low(Integer);
    FSpin.MaxValue := High(Integer);
    if ParamMin(AParam, MinV) then
      FSpin.MinValue := Trunc(MinV);
    if ParamMax(AParam, MaxV) then
      FSpin.MaxValue := Trunc(MaxV);
    FSpin.ValueEmpty := True;
    FSpin.Anchors := [akLeft, akTop, akRight];
  end
  else if FKind = spkNumber then
  begin
    FFloatSpin := TFloatSpinEdit.Create(Self);
    FFloatSpin.Parent := Self;
    FFloatSpin.Left := 0;
    FFloatSpin.Top := FLabel.Height + 4;
    FFloatSpin.Height := 23;
    FFloatSpin.MinValue := -1.0e308;
    FFloatSpin.MaxValue := 1.0e308;
    if ParamMin(AParam, MinV) then
      FFloatSpin.MinValue := MinV;
    if ParamMax(AParam, MaxV) then
      FFloatSpin.MaxValue := MaxV;
    FFloatSpin.DecimalPlaces := 6;
    FFloatSpin.ValueEmpty := True;
    FFloatSpin.Anchors := [akLeft, akTop, akRight];
  end
  else if FKind = spkBoolean then
  begin
    FCheck := TCheckBox.Create(Self);
    FCheck.Parent := Self;
    FCheck.Left := 0;
    FCheck.Top := FLabel.Height + 4;
    FCheck.Height := 19;
    FCheck.Caption := '';
    FCheck.AllowGrayed := False;
    FCheck.State := cbUnchecked;
    FCheck.Anchors := [akLeft, akTop, akRight];
  end
  else if FKind = spkHost then
  begin
    FCombo := TComboBox.Create(Self);
    FCombo.Parent := Self;
    FCombo.Left := 0;
    FCombo.Top := FLabel.Height + 4;
    FCombo.Height := 23;
    FCombo.Style := csDropDownList;
    FCombo.Anchors := [akLeft, akTop, akRight];
    BuildHostChoices(AExternalHosts);
  end
  else
  begin
    FEdit := TEdit.Create(Self);
    FEdit.Parent := Self;
    FEdit.Left := 0;
    FEdit.Top := FLabel.Height + 4;
    FEdit.Height := 23;
    FEdit.Anchors := [akLeft, akTop, akRight];
  end;

  if FCheckList <> nil then
    H := FCheckList.Top + FCheckList.Height
  else if FMemo <> nil then
    H := FMemo.Top + FMemo.Height
  else if FSpin <> nil then
    H := FSpin.Top + FSpin.Height
  else if FFloatSpin <> nil then
    H := FFloatSpin.Top + FFloatSpin.Height
  else if FCheck <> nil then
    H := FCheck.Top + FCheck.Height
  else if FCombo <> nil then
    H := FCombo.Top + FCombo.Height
  else
    H := FEdit.Top + FEdit.Height;
  if FCheckList <> nil then
    FControl := FCheckList
  else if FMemo <> nil then
    FControl := FMemo
  else if FSpin <> nil then
    FControl := FSpin
  else if FFloatSpin <> nil then
    FControl := FFloatSpin
  else if FCheck <> nil then
    FControl := FCheck
  else if FCombo <> nil then
    FControl := FCombo
  else
    FControl := FEdit;
  FControl.Left := FLabel.Width + 12;
  FControl.Top := 0;
  if FControl.Width <= 0 then
    FControl.Width := 220;
  H := FLabel.Height;
  if FDescLabel.Height > 0 then
    Inc(H, FDescLabel.Height + 2);
  H := Max(H, FControl.Top + FControl.Height);
  Height := H + 6;

  if FCheckList <> nil then
    FCheckList.OnClickCheck := @DoChanged
  else if FMemo <> nil then
    FMemo.OnChange := @DoChanged
  else if FSpin <> nil then
    FSpin.OnChange := @DoChanged
  else if FFloatSpin <> nil then
    FFloatSpin.OnChange := @DoChanged
  else if FCheck <> nil then
    FCheck.OnChange := @DoChanged
  else if FCombo <> nil then
    FCombo.OnChange := @DoChanged
  else if FEdit <> nil then
    FEdit.OnChange := @DoChanged;

  Def := ScriptParamDefault(AParam);
  if (AValue <> nil) and (AValue.JSONType <> jtNull) then
    ApplyValue(AValue)
  else if Def <> nil then
    ApplyValue(Def);
  FLoading := False;
end;

destructor TScriptParamRow.Destroy;
begin
  FHostIds.Free;
  inherited Destroy;
end;

procedure TScriptParamRow.Resize;
var
  LabelWidth: Integer;
begin
  inherited Resize;
  if (FControl = nil) or (ClientWidth <= 0) then
    exit;
  LabelWidth := Min(520, Max(160, ClientWidth - 240));
  FLabel.Width := LabelWidth;
  FDescLabel.Width := LabelWidth;
  FControl.Left := LabelWidth + 12;
  FControl.Width := Max(120, Min(220, ClientWidth - FControl.Left));
end;

function TScriptParamRow.ParamKey: String;
begin
  Result := ScriptString(FParam, 'key', '');
end;

function TScriptParamRow.ParamName: String;
begin
  Result := TranslateString(ScriptString(FParam, 'name', ParamKey), True);
end;

procedure TScriptParamRow.BuildHostChoices(AExternalHosts: TJSONArray);

  procedure AddHost(const AId: String);
  var
    D: String;
  begin
    if (AId = '') or (FHostIds.IndexOf(AId) >= 0) then
      exit;
    D := HostDisplayName(AId, AExternalHosts);
    if D = AId then
      D := AId;
    FHostIds.Add(AId);
    if FCombo <> nil then
      FCombo.Items.Add(D)
    else if FCheckList <> nil then
      FCheckList.Items.Add(D);
  end;

var
  i: Integer;
  d: TJSONData;
  o: TJSONObject;
  Id: String;
begin
  d := FParam.Find('hosts');
  if d is TJSONArray then
  begin
    for i := 0 to TJSONArray(d).Count - 1 do
      if TJSONArray(d).Items[i].JSONType = jtString then
        AddHost(TJSONArray(d).Items[i].AsString);
    exit;
  end;
  if AExternalHosts = nil then
    exit;
  for i := 0 to AExternalHosts.Count - 1 do
  begin
    o := AExternalHosts.Objects[i];
    if o = nil then
      continue;
    Id := ScriptString(o, 'id', '');
    AddHost(Id);
  end;
end;

procedure TScriptParamRow.DoChanged(Sender: TObject);
begin
  if FLoading then
    exit;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TScriptParamRow.ApplyValue(AData: TJSONData);
var
  i, idx: Integer;
  a: TJSONArray;
  s: String;
begin
  if (AData = nil) or (AData.JSONType = jtNull) then
    exit;
  if FArray then
  begin
    if not (AData is TJSONArray) then
      exit;
    a := TJSONArray(AData);
    if FKind = spkHost then
    begin
      for i := 0 to a.Count - 1 do
      begin
        if a.Items[i].JSONType <> jtString then
          continue;
        idx := FHostIds.IndexOf(a.Items[i].AsString);
        if (idx >= 0) and (FCheckList <> nil) then
          FCheckList.Checked[idx] := True;
      end;
    end
    else if FMemo <> nil then
    begin
      FMemo.Lines.BeginUpdate;
      try
        FMemo.Lines.Clear;
        for i := 0 to a.Count - 1 do
          case FKind of
            spkString: if a.Items[i].JSONType = jtString then
              FMemo.Lines.Add(a.Items[i].AsString);
            spkInteger: if a.Items[i].JSONType = jtNumber then
              FMemo.Lines.Add(IntToStr(a.Items[i].AsInt64));
            spkNumber: if a.Items[i].JSONType = jtNumber then
              FMemo.Lines.Add(FloatToStr(a.Items[i].AsFloat));
            spkBoolean: if a.Items[i].JSONType = jtBoolean then
              if a.Items[i].AsBoolean then
                FMemo.Lines.Add('true')
              else
                FMemo.Lines.Add('false');
          end;
      finally
        FMemo.Lines.EndUpdate;
      end;
    end;
    exit;
  end;

  case FKind of
    spkString:
      if (FEdit <> nil) and (AData.JSONType = jtString) then
        FEdit.Text := AData.AsString;
    spkInteger:
      if (FSpin <> nil) and (AData.JSONType = jtNumber) then
      begin
        FSpin.ValueEmpty := False;
        FSpin.Value := AData.AsInt64;
      end;
    spkNumber:
      if (FFloatSpin <> nil) and (AData.JSONType = jtNumber) then
      begin
        FFloatSpin.ValueEmpty := False;
        FFloatSpin.Value := AData.AsFloat;
      end;
    spkBoolean:
      if FCheck <> nil then
      begin
        if AData.JSONType = jtBoolean then
        begin
          if AData.AsBoolean then
            FCheck.State := cbChecked
          else
            FCheck.State := cbUnchecked;
        end;
      end;
    spkHost:
      if (FCombo <> nil) and (AData.JSONType = jtString) then
      begin
        s := AData.AsString;
        idx := FHostIds.IndexOf(s);
        if idx >= 0 then
          FCombo.ItemIndex := idx;
      end;
  end;
end;

procedure TScriptParamRow.SetError(Const AMessage: String; Out AError: String);
begin
  AError := Format(AMessage, [ParamName]);
end;

function TScriptParamRow.ParseNumber(Const S: String; Out AValue: Double): Boolean;
var
  v: String;
begin
  v := Trim(S);
  AValue := 0;
  if v = '' then
    exit(False);
  v := StringReplace(v, ',', FormatSettings.DecimalSeparator, [rfReplaceAll]);
  v := StringReplace(v, '.', FormatSettings.DecimalSeparator, [rfReplaceAll]);
  Result := TryStrToFloat(v, AValue);
end;

function TScriptParamRow.CheckNumber(AValue: Double; Out AError: String): Boolean;
var
  MinValue, MaxValue: Double;
begin
  Result := False;
  AError := '';
  if ParamMin(FParam, MinValue) and (AValue < MinValue) then
  begin
    SetError(SInvalidScriptParameter, AError);
    exit;
  end;
  if ParamMax(FParam, MaxValue) and (AValue > MaxValue) then
  begin
    SetError(SInvalidScriptParameter, AError);
    exit;
  end;
  Result := True;
end;

function TScriptParamRow.CollectArray(Out AValue: TJSONData; Out AError: String): Boolean;
var
  i: Integer;
  a: TJSONArray;
  s: String;
  v: Int64;
  d: Double;
  b: Boolean;
begin
  AValue := nil;
  AError := '';
  a := TJSONArray.Create;
  try
    if FKind = spkHost then
    begin
      if FCheckList <> nil then
        for i := 0 to FCheckList.Items.Count - 1 do
          if FCheckList.Checked[i] then
            a.Add(TJSONString.Create(UTF8Decode(FHostIds[i])));
    end
    else if FMemo <> nil then
    begin
      for i := 0 to FMemo.Lines.Count - 1 do
      begin
        s := Trim(FMemo.Lines[i]);
        if s = '' then
          continue;
        case FKind of
          spkString:
            a.Add(TJSONString.Create(UTF8Decode(s)));
          spkInteger:
            begin
              if not TryStrToInt64(s, v) then
              begin
                SetError(SInvalidScriptParameter, AError);
                exit;
              end;
              if not CheckNumber(Double(v), AError) then
                exit;
              a.Add(TJSONInt64Number.Create(v));
            end;
          spkNumber:
            begin
              if not ParseNumber(s, d) then
              begin
                SetError(SInvalidScriptParameter, AError);
                exit;
              end;
              if not CheckNumber(d, AError) then
                exit;
              a.Add(TJSONFloatNumber.Create(d));
            end;
          spkBoolean:
            begin
              if SameText(s, 'true') then
                b := True
              else if SameText(s, 'false') then
                b := False
              else
              begin
                SetError(SInvalidScriptParameter, AError);
                exit;
              end;
              a.Add(TJSONBoolean.Create(b));
            end;
        end;
      end;
    end;
    if a.Count = 0 then
    begin
      if ScriptParamRequired(FParam) then
      begin
        SetError(SRequiredScriptParameter, AError);
        exit;
      end;
      FreeAndNil(a);
      exit(True);
    end;
    AValue := a;
    a := nil;
    Result := True;
  finally
    a.Free;
  end;
end;

function TScriptParamRow.CollectScalar(Out AValue: TJSONData; Out AError: String): Boolean;
var
  s: String;
  v: Int64;
  d: Double;
  idx: Integer;
begin
  AValue := nil;
  AError := '';
  case FKind of
    spkString:
      begin
        if FEdit = nil then
          exit(True);
        s := FEdit.Text;
        if Trim(s) = '' then
        begin
          if ScriptParamRequired(FParam) then
          begin
            SetError(SRequiredScriptParameter, AError);
            exit;
          end;
          exit(True);
        end;
        AValue := TJSONString.Create(UTF8Decode(s));
      end;
    spkInteger:
      begin
        if FSpin = nil then
          exit(True);
        if FSpin.ValueEmpty then
        begin
          if ScriptParamRequired(FParam) then
          begin
            SetError(SRequiredScriptParameter, AError);
            exit;
          end;
          exit(True);
        end;
        v := FSpin.Value;
        if not CheckNumber(Double(v), AError) then
          exit;
        AValue := TJSONInt64Number.Create(v);
      end;
    spkNumber:
      begin
        if FFloatSpin = nil then
          exit(True);
        if FFloatSpin.ValueEmpty then
        begin
          if ScriptParamRequired(FParam) then
          begin
            SetError(SRequiredScriptParameter, AError);
            exit;
          end;
          exit(True);
        end;
        d := FFloatSpin.Value;
        if not CheckNumber(d, AError) then
          exit;
        AValue := TJSONFloatNumber.Create(d);
      end;
    spkBoolean:
      begin
        if FCheck = nil then
          exit(True);
        AValue := TJSONBoolean.Create(FCheck.Checked);
      end;
    spkHost:
      begin
        if (FCombo = nil) or (FCombo.ItemIndex < 0) then
        begin
          if ScriptParamRequired(FParam) then
          begin
            SetError(SRequiredScriptParameter, AError);
            exit;
          end;
          exit(True);
        end;
        idx := FCombo.ItemIndex;
        if (idx < 0) or (idx >= FHostIds.Count) then
        begin
          SetError(SNoExternalHosts, AError);
          exit;
        end;
        AValue := TJSONString.Create(UTF8Decode(FHostIds[idx]));
      end;
  end;
  Result := True;
end;

function TScriptParamRow.Collect(Out AValue: TJSONData; Out AError: String): Boolean;
begin
  if FArray then
    Result := CollectArray(AValue, AError)
  else
    Result := CollectScalar(AValue, AError);
end;

  { TScriptParamsForm }

constructor TScriptParamsForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRows := TList.Create;
end;

destructor TScriptParamsForm.Destroy;
begin
  FCollected.Free;
  FRows.Free;
  inherited Destroy;
end;

class function TScriptParamsForm.Execute(AOwner: TComponent;
  AScript: TJSONObject; AExternalHosts: TJSONArray; ASaved: TJSONObject; Out AParams: TJSONObject): Boolean;
var
  F: TScriptParamsForm;
begin
  Result := False;
  AParams := nil;
  F := TScriptParamsForm.Create(AOwner);
  try
    F.BuildRows(AScript, AExternalHosts, ASaved);
    if F.FRows.Count = 0 then
    begin
      AParams := TJSONObject.Create;
      Result := True;
      exit;
    end;
    if F.ShowModal = mrOk then
    begin
      AParams := F.TakeParams;
      Result := AParams <> nil;
    end;
  finally
    F.Free;
  end;
end;

class function TScriptParamsForm.BuildFromSaved(AOwner: TComponent;
  AScript: TJSONObject; AExternalHosts: TJSONArray; ASaved: TJSONObject;
  Out AParams: TJSONObject; Out AError: String): Boolean;
var
  F: TScriptParamsForm;
begin
  Result := False;
  AParams := nil;
  AError := '';
  F := TScriptParamsForm.Create(AOwner);
  try
    F.BuildRows(AScript, AExternalHosts, ASaved);
    if F.FRows.Count = 0 then
      AParams := TJSONObject.Create
    else
      AParams := F.CollectAll(AError);
    Result := AParams <> nil;
  finally
    F.Free;
  end;
end;

procedure TScriptParamsForm.BuildRows(AScript: TJSONObject; AExternalHosts: TJSONArray; ASaved: TJSONObject);
var
  d: TJSONData;
  i, Total: Integer;
  Param: TJSONObject;
  Row: TScriptParamRow;
  Desc: String;
  Header: TLabel;
  Value: TJSONData;
begin
  Caption := TranslateString(
    ScriptString(AScript, 'name', ScriptString(AScript, 'id', '')), True);
  BorderStyle := bsDialog;
  Position := poMainFormCenter;
  ClientWidth := 820;
  ShowHint := True;
  Constraints.MinWidth := 600;
  Constraints.MinHeight := 180;

  FScroll := TScrollBox.Create(Self);
  FScroll.Parent := Self;
  FScroll.Align := alClient;
  FScroll.BorderSpacing.Around := 8;

  FButtons := TButtonPanel.Create(Self);
  FButtons.Parent := Self;
  FButtons.Align := alBottom;
  FButtons.ShowButtons := [pbOK, pbCancel];
  FButtons.ShowBevel := False;
  FButtons.BorderSpacing.Left := 8;
  FButtons.BorderSpacing.Right := 8;
  FButtons.BorderSpacing.Bottom := 8;
  FButtons.OKButton.ModalResult := mrNone;
  FButtons.OKButton.OnClick := @OKClick;

  Desc := TranslateString(ScriptString(AScript, 'description', ''), True);
  if Desc <> '' then
  begin
    Header := TLabel.Create(FScroll);
    Header.Parent := FScroll;
    Header.Align := alTop;
    Header.AutoSize := False;
    Header.Height := 34;
    Header.WordWrap := True;
    Header.Caption := Desc;
    Header.BorderSpacing.Left := 8;
    Header.BorderSpacing.Right := 8;
  end;

  d := AScript.Find('parameters');
  if not (d is TJSONArray) then
    exit;
  Total := 0;
  for i := 0 to TJSONArray(d).Count - 1 do
  begin
    Param := TJSONArray(d).Objects[i];
    if (Param = nil) or (ScriptParamStorage(Param) <> 'runtime') then
      continue;
    Value := nil;
    if ASaved <> nil then
      Value := ASaved.Find(ScriptString(Param, 'key', ''));
    Row := TScriptParamRow.Create(FScroll, Param, AExternalHosts, Value);
    Row.Parent := FScroll;
    Row.Align := alTop;
    FRows.Add(Row);
    Inc(Total, Row.Height + 6);
  end;
  FScroll.VertScrollBar.Position := 0;
  if FRows.Count > 0 then
    ClientHeight := Min(600, Total + 110);
end;

procedure TScriptParamsForm.OKClick(Sender: TObject);
begin
  FreeAndNil(FCollected);
  FCollected := CollectAll;
  if FCollected = nil then
    exit;
  ModalResult := mrOk;
end;

function TScriptParamsForm.CollectAll(Out AError: String): TJSONObject;
var
  i: Integer;
  Row: TScriptParamRow;
  Data: TJSONData;
begin
  AError := '';
  Result := TJSONObject.Create;
  for i := 0 to FRows.Count - 1 do
  begin
    Row := TScriptParamRow(FRows[i]);
    Data := nil;
    if not Row.Collect(Data, AError) then
    begin
      FreeAndNil(Result);
      exit;
    end;
    if Data <> nil then
      Result.Add(Row.ParamKey, Data);
  end;
end;

function TScriptParamsForm.CollectAll: TJSONObject;
var
  Err: String;
begin
  Result := CollectAll(Err);
  if Result = nil then
    MessageDlg('', Err, mtError, [mbOK], 0);
end;

function TScriptParamsForm.TakeParams: TJSONObject;
begin
  Result := FCollected;
  FCollected := nil;
end;

{ TScriptResultForm }

constructor TScriptResultForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := 'Script result';
  AutoSize := False;
  BorderStyle := bsSizeable;
  Position := poMainFormCenter;
  Width := 900;
  Height := 620;
  Constraints.MinWidth := 700;
  Constraints.MinHeight := 420;

  FButtons := TButtonPanel.Create(Self);
  FButtons.Parent := Self;
  FButtons.Align := alBottom;
  FButtons.ShowButtons := [pbOK, pbCancel];
  FButtons.ShowBevel := False;
  FButtons.BorderSpacing.Left := 8;
  FButtons.BorderSpacing.Right := 8;
  FButtons.BorderSpacing.Bottom := 8;
  FButtons.OKButton.ModalResult := mrNone;
  FButtons.OKButton.OnClick := @OKClick;
  FButtons.CancelButton.ModalResult := mrNone;
  FButtons.CancelButton.OnClick := @CancelClick;
  OnClose := @FormClose;

  FMemo := TMemo.Create(Self);
  FMemo.Parent := Self;
  FMemo.Align := alClient;
  FMemo.ReadOnly := True;
  FMemo.ScrollBars := ssBoth;
  FMemo.WordWrap := False;
  FMemo.BorderSpacing.Around := 8;
end;

procedure TScriptResultForm.SetOutput(Const AText: String; AFollow: Boolean);
begin
  FMemo.Lines.BeginUpdate;
  Try
    FMemo.Text := AText;
    FMemo.WordWrap := False;
    FMemo.ScrollBars := ssBoth;
  Finally
    FMemo.Lines.EndUpdate;
  End;
  if AFollow Then
  begin
    FMemo.SelStart := Length(FMemo.Text);
    FMemo.SelLength := 0;
    {$ifdef windows}
    FMemo.Perform(183, 0, 0);
    FMemo.Perform(277, 7, 0);
    {$endif}
  end
  Else
  begin
    FMemo.SelStart := 0;
    FMemo.SelLength := 0;
  end;
end;

procedure TScriptResultForm.SetRunning(AValue: Boolean);
begin
  FRunning := AValue;
  If AValue Then
    FCancelRequested := False;
  FButtons.OKButton.Enabled := Not AValue;
  FButtons.CancelButton.Enabled := AValue;
end;

procedure TScriptResultForm.OKClick(Sender: TObject);
begin
  If Not FRunning Then
    Close;
end;

procedure TScriptResultForm.CancelClick(Sender: TObject);
begin
  If FRunning Then
  Begin
    FCancelRequested := True;
    FButtons.CancelButton.Enabled := False;
  End;
end;

procedure TScriptResultForm.FormClose(Sender: TObject;
  Var ACloseAction: TCloseAction);
begin
  If FRunning Then
    ACloseAction := caNone
  Else
    ACloseAction := caFree;
end;

{ TScriptSettingsFrame }

type
  TScriptSettingsGroup = class(TGroupBox)
  public
    ScriptId: String;
    Rows: TList;
    Dirty: Boolean;
    constructor Create(AOwner: TComponent); reintroduce;
    destructor Destroy; override;
    procedure RowChanged(Sender: TObject);
  end;

constructor TScriptSettingsGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Rows := TList.Create;
  Dirty := False;
end;

procedure TScriptSettingsGroup.RowChanged(Sender: TObject);
begin
  Dirty := True;
end;

destructor TScriptSettingsGroup.Destroy;
begin
  Rows.Free;
  inherited Destroy;
end;

constructor TScriptSettingsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Align := alClient;
  FGroups := TList.Create;
  FHint := TLabel.Create(Self);
  FHint.Parent := Self;
  FHint.Align := alTop;
  FHint.AutoSize := False;
  FHint.Height := 32;
  FHint.WordWrap := True;
  FHint.Caption := SHostScriptSettingsHint;
  FHint.BorderSpacing.Left := 8;
  FHint.BorderSpacing.Right := 8;
  FScroll := TScrollBox.Create(Self);
  FScroll.Parent := Self;
  FScroll.Align := alClient;
  FScroll.BorderSpacing.Around := 8;
  FScroll.AutoScroll := True;
end;

destructor TScriptSettingsFrame.Destroy;
begin
  ClearData;
  FGroups.Free;
  inherited Destroy;
end;

procedure TScriptSettingsFrame.ClearData;
begin
  If FScroll <> nil Then
    While FScroll.ControlCount > 0 Do
      FScroll.Controls[0].Free;
  If FGroups <> nil Then
    FGroups.Clear;
  FreeAndNil(FScripts);
  FreeAndNil(FExternalHosts);
  FRevision := '';
  FHasHostParams := False;
end;

function FindSettingsParams(ASettings: TJSONObject; Const AScriptId: String): TJSONObject;
var
  d: TJSONData;
  i: Integer;
  o: TJSONObject;
begin
  Result := nil;
  if ASettings = nil then
    exit;
  d := ASettings.Find('scripts');
  if not (d is TJSONArray) then
    exit;
  for i := 0 to TJSONArray(d).Count - 1 do
  begin
    o := TJSONArray(d).Objects[i];
    if (o <> nil) and (ScriptString(o, 'id', '') = AScriptId) then
    begin
      d := o.Find('parameters');
      if d is TJSONObject then
        Result := TJSONObject(d);
      exit;
    end;
  end;
end;

procedure TScriptSettingsFrame.LoadData(AScripts: TJSONArray;
  ASettings: TJSONObject; AExternalHosts: TJSONArray);
var
  i, j, Total: Integer;
  Script, Param, SettingsParams: TJSONObject;
  d: TJSONData;
  Group: TScriptSettingsGroup;
  Row: TScriptParamRow;
    Value: TJSONData;
begin
  ClearData;
  FHint.Caption := SHostScriptSettingsHint;
  if (AScripts = nil) or (AScripts.Count = 0) then
    exit;
  FScripts := TJSONArray(AScripts.Clone);
  if AExternalHosts <> nil then
    FExternalHosts := TJSONArray(AExternalHosts.Clone);
  FRevision := ScriptString(ASettings, 'revision', '');

  for i := 0 to FScripts.Count - 1 do
  begin
    Script := FScripts.Objects[i];
    if (Script = nil) or not ScriptHasHostParams(Script) then
      continue;

    FHasHostParams := True;
    SettingsParams := FindSettingsParams(ASettings, ScriptString(Script, 'id', ''));
    Group := TScriptSettingsGroup.Create(FScroll);
    Group.Parent := FScroll;
    Group.Align := alTop;
    Group.Caption := TranslateString(
      ScriptString(Script, 'name', ScriptString(Script, 'id', '')), True);
    Group.ScriptId := ScriptString(Script, 'id', '');
    Group.BorderSpacing.Top := 8;
    Group.BorderSpacing.Left := 8;
    Group.BorderSpacing.Right := 8;
    Total := 28;

    d := Script.Find('parameters');
    if d is TJSONArray then
      for j := 0 to TJSONArray(d).Count - 1 do
      begin
        Param := TJSONArray(d).Objects[j];
        if (Param = nil) or (ScriptParamStorage(Param) <> 'host') then
          continue;
        Value := nil;
        if SettingsParams <> nil then
          Value := SettingsParams.Find(ScriptString(Param, 'key', ''));
        Row := TScriptParamRow.Create(Group, Param, FExternalHosts, Value);
        Row.Parent := Group;
        Row.Align := alTop;
        Row.OnChanged := @Group.RowChanged;
        Group.Rows.Add(Row);
        Inc(Total, Row.Height + 6);
      end;

    if Group.Rows.Count = 0 then
    begin
      Group.Free;
      continue;
    end;
    Group.Height := Total + 8;
    FGroups.Add(Group);
  end;
end;

procedure TScriptSettingsFrame.SetHintText(Const AText: String);
begin
  FHint.Caption := AText;
end;

function TScriptSettingsFrame.BuildRequest(Out ARequest: TJSONObject;
  Out AError: String): Boolean;
var
  i, j: Integer;
  Group: TScriptSettingsGroup;
  Row: TScriptParamRow;
  Params, Item: TJSONObject;
  Scripts: TJSONArray;
  Data: TJSONData;
begin
  ARequest := nil;
  AError := '';
  if not FHasHostParams then
    exit(True);

  Scripts := TJSONArray.Create;
  try
    for i := 0 to FGroups.Count - 1 do
    begin
      Group := TScriptSettingsGroup(FGroups[i]);
      if not Group.Dirty then
        continue;
      Params := TJSONObject.Create;
      try
        for j := 0 to Group.Rows.Count - 1 do
        begin
          Row := TScriptParamRow(Group.Rows[j]);
          Data := nil;
          if not Row.Collect(Data, AError) then
          begin
            Params.Free;
            exit(False);
          end;
          if Data <> nil then
            Params.Add(Row.ParamKey, Data);
        end;
      except
        Params.Free;
        raise;
      end;
      Item := TJSONObject.Create;
      Item.Add('id', Group.ScriptId);
      Item.Add('parameters', Params);
      Scripts.Add(Item);
    end;

    if Scripts.Count = 0 then
    begin
      Scripts.Free;
      Scripts := nil;
      exit(True);
    end;

    ARequest := TJSONObject.Create;
    if FRevision <> '' then
      ARequest.Add('expected_revision', FRevision);
    ARequest.Add('scripts', Scripts);
    Scripts := nil;
    Result := True;
  finally
    Scripts.Free;
  end;
end;

end.
