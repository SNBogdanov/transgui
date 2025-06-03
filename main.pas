

{*************************************************************************************
  This file is part of Transmission Remote GUI.
  Copyright (c) 2008-2019 by Yury Sidorov and Transmission Remote GUI working group.

  Transmission Remote GUI is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  Transmission Remote GUI is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Transmission Remote GUI; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

  In addition, as a special exception, the copyright holders give permission to 
  link the code of portions of this program with the
  OpenSSL library under certain conditions as described in each individual
  source file, and distribute linked combinations including the two.

  You must obey the GNU General Public License in all respects for all of the
  code used other than OpenSSL.  If you modify file(s) with this exception, you
  may extend this exception to your version of the file(s), but you are not
  obligated to do so.  If you do not wish to do so, delete this exception
  statement from your version.  If you delete this exception statement from all
  source files in the program, then also delete it here.
*************************************************************************************}

Unit Main;
{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, zstream, LResources, Forms, Controls, ImgList,
  dbugintf,
  {$ifdef windows}
windows, ShellApi,LCLIntf,
  {$else}
  lclintf,
  {$endif windows}
  Graphics, Dialogs, ComCtrls, Menus, ActnList, LCLVersion, httpsend, StdCtrls,
  fpjson, jsonparser, ExtCtrls, rpc, syncobjs, variants, varlist, IpResolver,
  zipper, ResTranslator, VarGrid, StrUtils, LCLProc, Grids, Buttons, BaseForm,
  utils, AddTorrent, Types, LazFileUtils, LazUTF8, StringToVK, passwcon,
  GContnrs, RegExpr, fgl;

Const
  AppName = 'Transmission Remote GUI';
  AppVersion = '5.19';

Resourcestring
  sAll = 'All torrents';
  sWaiting = 'Waiting';
  sVerifying = 'Verifying';
  sDownloading = 'Downloading';
  sSeeding = 'Seeding';
  sFinished = 'Finished';
  sStopped = 'Stopped';
  sUnknown = 'Unknown';
  sCompleted = 'Completed';
  sConnected = 'connected';
  sActive = 'Active';
  sInactive = 'Inactive';
  sErrorState = 'Error';
  sUpdating = 'Updating...';
  sFinishedDownload = '''%s'' has finished downloading';
  sDownloadComplete = 'Download complete';
  sUpdateComplete = 'Update complete.';
  sTorrentVerification = 'Torrent verification may take a long time.' +
    LineEnding + 'Are you sure to start verification of torrent ''%s''?';
  sTorrentsVerification = 'Torrents verification may take a long time.' +
    LineEnding + 'Are you sure to start verification of %d torrents?';
  sReconnect = 'Reconnect in %d seconds.';
  sDisconnected = 'Disconnected';
  sConnectingToDaemon = 'Connecting to daemon...';
  sLanguagePathFile = 'Language pathfile';
  sLanguagePath = 'Language path';
  sLanguageList = 'Language list';
  sSecs = '%ds';
  sMins = '%dm';
  sHours = '%dh';
  sDays = '%dd';
  sMonths = '%dmo';
  sYears = '%dy';
  sDownloadingSeeding = '%s%s%d downloading, %d seeding%s%s, %s';
  sDownSpeed = 'D: %s/s';
  sUpSpeed = 'U: %s/s';
  SFreeSpace = 'Free: %s';
  sTempSpace = 'Temp: %s';
  sNoPathMapping = 'Unable to find path mapping.' + LineEnding +
    'Use the application''s options to setup path mappings.';
  sGeoIPConfirm = 'Geo IP database is needed to resolve country by IP address.' +
    LineEnding + 'Download this database now?';
  sFlagArchiveConfirm =

    'Flag images archive is needed to display country flags.' +
    LineEnding + 'Download this archive now?';
  sInSwarm = 'in swarm';
  sHashfails = '%s (%d hashfails)';
  sDone = '%s (%s done)';
  sHave = '%d x %s (have %d)';
  sUnableExtractFlag = 'Unable to extract flag image.';
  sTrackerWorking = 'Working';
  sTrackerUpdating = 'Updating';
  sRestartRequired = 'You need to restart the application to apply changes.';
  sRemoveTorrentData =

    'Are you sure to remove torrent ''%s'' and all associated DATA?';
  sRemoveTorrentDataMulti =

    'Are you sure to remove %d selected torrents and all their associated DATA?';
  sRemoveTorrent = 'Are you sure to remove torrent ''%s''?';
  sRemoveTorrentMulti = 'Are you sure to remove %d selected torrents?';
  sUnableGetFilesList = 'Unable to get files list';
  sTrackerError = 'Tracker';
  sSkip = 'skip';
  sLow = 'low';
  sNormal = 'normal';
  sHigh = 'high';
  sByte = 'b';
  sKByte = 'KB';
  sMByte = 'MB';
  sGByte = 'GB';
  sTByte = 'TB';
  sPerSecond = '/s';
  sOf = 'of';
  sNoTracker = 'No tracker';
  sTorrents = 'Torrents';
  sBlocklistUpdateComplete = 'The block list has been updated successfully.' +
    LineEnding + 'The list entries count: %d.';
  sSeveralTorrents = '%d torrents';
  sUnableToExecute = 'Unable to execute "%s".';
  sSSLLoadError = 'Unable to load OpenSSL library files: %s and %s';
  SRemoveTracker = 'Are you sure to remove tracker ''%s''?';
  SUnlimited = 'Unlimited';
  SAverage = 'average';
  SCheckNewVersion =

    'Do you wish to enable automatic checking for a new version of %s?';
  SDuplicateTorrent = 'Torrent already exists in the list';
  SUpdateTrackers = 'Update trackers for the existing torrent?';
  SDownloadingTorrent = 'Downloading torrent file...';
  SConnectTo = 'Connect to %s';
  SEnterPassword = 'Please enter a password to connect to %s:';

  SDownloaded = 'Downloaded';
  SUploaded = 'Uploaded';
  SFilesAdded = 'Files added';
  SActiveTime = 'Active time';
  STotalSize = 'Total: %s';
  sTotalSizeToDownload = 'Selected: %s';
  sTotalDownloaded = 'Done: %s';
  sTotalRemain = 'Remaining: %s';

  sUserMenu = 'User Menu';

  sBiDiDefault = 'Default';
  sBiDiLeftRight = 'Left->Right';
  sBiDiRightLeft = 'Right->Left';
  sBiDiRightLeftNoAlign = 'Right->Left (No Align)';
  sBiDiRightLeftReadOnly = 'Right->Left (Reading Only)';

  sPrivateOn = 'ON';
  sPrivateOff = 'OFF';

Type

  { TMyHashMap example from hashmapdemo }
  TStringMap = Class(Specialize TFPGMap<String, String>)

  End;

  TMyHashMap = Class(Specialize TGenHashMap<Integer, Integer>)
    Function DefaultHashKey(Const Key: Integer): Integer; Override;
    Function DefaultKeysEqual(Const A, B: Integer): Boolean; Override;
    Function DefaultKeyToString(Const Key: Integer): String; Override;
    Function DefaultItemToString(Const Item: Integer): String; Override;
  End;

  // for torrent folder
  FolderData = Class
  Public
    Hit: Integer;
    Ext: String;
    Txt: String;
    Lst: TDate;
  End;

  CountData = Class
  Public
    Count: Integer;
    Size: Double;
  End;

  { TProgressImage }

  TProgressImage = Class(TGraphicControl)
  Private
    FBmp: TBitmap;
    FBorderColor: TColor;
    FEndIndex: Integer;
    FImageIndex: Integer;
    FImages: TImageList;
    FStartIndex: Integer;
    FTimer: TTimer;
    Function GetFrameDelay: Integer;
    Procedure SetBorderColor(Const AValue: TColor);
    Procedure SetEndIndex(Const AValue: Integer);
    Procedure SetFrameDelay(Const AValue: Integer);
    Procedure SetImageIndex(Const AValue: Integer);
    Procedure SetImages(Const AValue: TImageList);
    Procedure SetStartIndex(Const AValue: Integer);
    Procedure UpdateIndex;
    Procedure DoTimer(Sender: TObject);
  Protected
    Procedure Paint; Override;
    Procedure VisibleChanged; Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    Property Images: TImageList read FImages write SetImages;
    Property ImageIndex: Integer read FImageIndex write SetImageIndex;
    Property StartIndex: Integer read FStartIndex write SetStartIndex;
    Property EndIndex: Integer read FEndIndex write SetEndIndex;
    Property FrameDelay: Integer read GetFrameDelay write SetFrameDelay;
    Property BorderColor: TColor read FBorderColor write SetBorderColor;
  End;

  { TMainForm }

  TMainForm = Class(TBaseForm)
    acConnect: TAction;
    acAddTorrent: TAction;
    acExport: TAction;
    acImport: TAction;
    acStartTorrent: TAction;
    acForceStartTorrent: TAction;
    acStopTorrent: TAction;
    acStartTorrentFilter: TAction;
    acForceStartTorrentFilter: TAction;
    acStopTorrentFilter: TAction;
    acRemoveTorrent: TAction;
    acSetHighPriority: TAction;
    acSetNormalPriority: TAction;
    acSetLowPriority: TAction;
    acSetNotDownload: TAction;
    acOptions: TAction;
    acDaemonOptions: TAction;
    acStartAllTorrents: TAction;
    acStopAllTorrents: TAction;
    acExit: TAction;
    acResolveHost: TAction;
    acResolveCountry: TAction;
    acShowCountryFlag: TAction;
    acSetupColumns: TAction;
    acRemoveTorrentAndData: TAction;
    acOpenFile: TAction;
    acOpenContainingFolder: TAction;
    acAddLink: TAction;
    acReannounceTorrent: TAction;
    acMoveTorrent: TAction;
    acSelectAll: TAction;
    acShowApp: TAction;
    acHideApp: TAction;
    acAddTracker: TAction;
    acEditTracker: TAction;
    acReplaceTracker: TAction;
    acDelTracker: TAction;
    acConnOptions: TAction;
    acNewConnection: TAction;
    acDisconnect: TAction;
    acAltSpeed: TAction;
    acQMoveTop: TAction;
    acQMoveUp: TAction;
    acQMoveDown: TAction;
    acQMoveBottom: TAction;
    acCheckNewVersion: TAction;
    acFolderGrouping: TAction;
    acAdvEditTrackers: TAction;
    acFilterPane: TAction;
    acMenuShow: TAction;
    acBigToolbar: TAction;
    acSetLabels: TAction;
    acLabelGrouping: TAction;
    acFilterTorrentProps: TAction;
    ActionList2: TActionList;
    ImageList32: TImageList;
    MenuItem103: TMenuItem;
    MenuItem104: TMenuItem;
    MenuItem105: TMenuItem;
    MenuItem106: TMenuItem;
    MenuItem108: TMenuItem;
    MenuItem193: TMenuItem;
    MenuItem201: TMenuItem;
    MenuItem202: TMenuItem;
    MenuItem203: TMenuItem;
    MenuItem206: TMenuItem;
    MenuItem107: TMenuItem;
    MenuShow: TAction;
    ActionList1: TActionList;
    acToolbarShow: TAction;
    acInfoPane: TAction;
    acStatusBar: TAction;
    acCopyPath: TAction;
    acRename: TAction;
    acStatusBarSizes: TAction;
    acTrackerGrouping: TAction;
    acUpdateBlocklist: TAction;
    acUpdateGeoIP: TAction;
    acTorrentProps: TAction;
    acVerifyTorrent: TAction;
    ActionList: TActionList;
    ApplicationProperties: TApplicationProperties;
    MenuItem501: TMenuItem;
    MenuItem502: TMenuItem;
    miFiltered: TMenuItem;
    SearchToolbar: TToolBar;
    SearchToolbar1: TToolBar;
    Separator1: TMenuItem;
    tbSearch: TToolButton;
    tbSearchCancel: TToolButton;
    LocalWatchTimer: TTimer;
    ToolButton10: TToolButton;
    ToolButton30: TToolButton;
    txDummy1: TLabel;
    txMagLabel: TLabel;
    txMagnetLink: TEdit;
    MenuItem101: TMenuItem;
    MenuItem102: TMenuItem;
    edSearch: TEdit;
    imgSearch: TImage;
    imgFlags: TImageList;
    ImageList16: TImageList;
    CurImageList: TImageList;
    FilterTimer: TTimer;
    MenuItem100: TMenuItem;
    MenuItem122: TMenuItem;
    MenuItem1888: TMenuItem;
    MenuItem1889: TMenuItem;
    MenuItem1890: TMenuItem;
    MenuItem68: TMenuItem;
    MenuItem93: TMenuItem;
    MenuItem94: TMenuItem;
    MenuItem95: TMenuItem;
    MenuItem96: TMenuItem;
    MenuItem97: TMenuItem;
    MenuItem98: TMenuItem;
    MenuItem99: TMenuItem;
    miExport: TMenuItem;
    miImport: TMenuItem;
    OpenDialog1: TOpenDialog;
    panDetailsWait: TPanel;
    SaveDialog1: TSaveDialog;
    ToolButton5: TToolButton;
    txGlobalStats: TLabel;
    lvFilter: TVarGrid;
    lvTrackers: TVarGrid;
    MenuItem25: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem63: TMenuItem;
    MenuItem64: TMenuItem;
    MenuItem77: TMenuItem;
    MenuItem78: TMenuItem;
    MenuItem79: TMenuItem;
    MenuItem80: TMenuItem;
    MenuItem81: TMenuItem;
    MenuItem82: TMenuItem;
    MenuItem83: TMenuItem;
    MenuItem84: TMenuItem;
    MenuItem85: TMenuItem;
    MenuItem86: TMenuItem;
    MenuItem87: TMenuItem;
    MenuItem88: TMenuItem;
    MenuItem89: TMenuItem;
    MenuItem90: TMenuItem;
    MenuItem91: TMenuItem;
    MenuItem92: TMenuItem;
    miView: TMenuItem;
    goDevelopmentSite: TMenuItem;
    miHomePage: TMenuItem;
    pmiQueue: TMenuItem;
    miQueue: TMenuItem;
    pmFilter: TPopupMenu;
    sepTrackers: TMenuItem;
    MenuItem65: TMenuItem;
    MenuItem66: TMenuItem;
    MenuItem67: TMenuItem;
    MenuItem69: TMenuItem;
    MenuItem70: TMenuItem;
    MenuItem72: TMenuItem;
    MenuItem76: TMenuItem;
    pmiUpSpeedLimit: TMenuItem;
    pmiDownSpeedLimit: TMenuItem;
    pmDownSpeeds: TPopupMenu;
    pmUpSpeeds: TPopupMenu;
    sepCon2: TMenuItem;
    MenuItem71: TMenuItem;
    sepCon1: TMenuItem;
    MenuItem73: TMenuItem;
    MenuItem74: TMenuItem;
    MenuItem75: TMenuItem;
    pmSepOpen2: TMenuItem;
    MenuItem42: TMenuItem;
    pmSepOpen1: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem45: TMenuItem;
    MenuItem46: TMenuItem;
    MenuItem47: TMenuItem;
    MenuItem48: TMenuItem;
    MenuItem49: TMenuItem;
    MenuItem50: TMenuItem;
    MenuItem51: TMenuItem;
    MenuItem52: TMenuItem;
    MenuItem53: TMenuItem;
    MenuItem54: TMenuItem;
    MenuItem55: TMenuItem;
    MenuItem56: TMenuItem;
    MenuItem58: TMenuItem;
    MenuItem59: TMenuItem;
    MenuItem60: TMenuItem;
    MenuItem61: TMenuItem;
    MenuItem62: TMenuItem;
    miPriority: TMenuItem;
    pmiPriority: TMenuItem;
    MenuItem57: TMenuItem;
    pbDownloaded: TPaintBox;
    pmTrackers: TPopupMenu;
    pmConnections: TPopupMenu;
    tabStats: TTabSheet;
    tabTrackers: TTabSheet;
    tbConnect: TToolButton;
    tbtAltSpeed: TToolButton;
    sepAltSpeed: TToolButton;
    sepQueue: TToolButton;
    tbQMoveUp: TToolButton;
    tbQMoveDown: TToolButton;
    ToolButton9: TToolButton;
    txConnErrorLabel: TLabel;
    panSearch: TPanel;
    panFilter: TPanel;
    panReconnectFrame: TShape;
    txDummy: TLabel;
    txReconnectSecs: TLabel;
    txConnError: TLabel;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    panReconnect: TPanel;
    txLastActive: TLabel;
    txLastActiveLabel: TLabel;
    txLabels: TLabel;
    txLabelsLabel: TLabel;
    txTracker: TLabel;
    txTrackerLabel: TLabel;
    txCompletedOn: TLabel;
    txCompletedOnLabel: TLabel;
    txAddedOn: TLabel;
    txAddedOnLabel: TLabel;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    miTSep1: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    miAbout: TMenuItem;
    miHelp: TMenuItem;
    panTop: TPanel;
    pmTray: TPopupMenu;
    HSplitter: TSplitter;
    pmPeers: TPopupMenu;
    TrayIcon: TTrayIcon;
    txCreated: TLabel;
    txCreatedLabel: TLabel;
    txTorrentHeader: TPanel;
    txTorrentName: TLabel;
    txTorrentNameLabel: TLabel;
    txDownProgress: TLabel;
    txDownProgressLabel: TLabel;
    panProgress: TPanel;
    txMaxPeers: TLabel;
    txMaxPeersLabel: TLabel;
    txPeers: TLabel;
    txPeersLabel: TLabel;
    txSeeds: TLabel;
    txSeedsLabel: TLabel;
    txTrackerUpdate: TLabel;
    txRemaining: TLabel;
    txRemainingLabel: TLabel;
    txStatus: TLabel;
    txStatusLabel: TLabel;
    txRatio: TLabel;
    txRatioLabel: TLabel;
    txDownLimit: TLabel;
    txDownLimitLabel: TLabel;
    txTrackerUpdateLabel: TLabel;
    txTransferHeader: TPanel;
    txUpSpeed: TLabel;
    txUpLimit: TLabel;
    txUpSpeedLabel: TLabel;
    txDownSpeed: TLabel;
    txDownSpeedLabel: TLabel;
    txUploaded: TLabel;
    txUploadedLabel: TLabel;
    txDownloaded: TLabel;
    txDownloadedLabel: TLabel;
    txUpLimitLabel: TLabel;
    txWasted: TLabel;
    txWastedLabel: TLabel;
    miCopyLabel: TMenuItem;
    pmLabels: TPopupMenu;
    txError: TLabel;
    txErrorLabel: TLabel;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    miTools: TMenuItem;
    TickTimer: TTimer;
    MainToolBar: TToolBar;
    panTransfer: TPanel;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    tbStopTorrent: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton11: TToolButton;
    txComment: TLabel;
    txCommentLabel: TLabel;
    txHash: TLabel;
    txHashLabel: TLabel;
    panGeneralInfo: TPanel;
    lvFiles: TVarGrid;
    lvPeers: TVarGrid;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    miConnect: TMenuItem;
    miExit: TMenuItem;
    miTorrent: TMenuItem;
    OpenTorrentDlg: TOpenDialog;
    PageInfo: TPageControl;
    pmTorrents: TPopupMenu;
    pmFiles: TPopupMenu;
    sbGenInfo: TScrollBox;
    txPieces: TLabel;
    txPiecesLabel: TLabel;
    txTotalSize: TLabel;
    txTotalSizeLabel: TLabel;
    gTorrents: TVarGrid;
    gStats: TVarGrid;
    VSplitter: TSplitter;
    StatusBar: TStatusBar;
    tabPeers: TTabSheet;
    tabGeneral: TTabSheet;
    TorrentsListTimer: TTimer;
    tabFiles: TTabSheet;
    Procedure acAddLinkExecute(Sender: TObject);
    Procedure acAddTorrentExecute(Sender: TObject);
    Procedure acAddTrackerExecute(Sender: TObject);
    Procedure acAdvEditTrackersExecute(Sender: TObject);
    Procedure acAltSpeedExecute(Sender: TObject);
    Procedure acBigToolbarExecute(Sender: TObject);
    Procedure acFilterTorrentPropsExecute(Sender: TObject);
    Procedure ScaleImageList(ImgList: TImageList; Var ImgListOut: TImageList;
      NewWidth: Integer);
    Procedure acCheckNewVersionExecute(Sender: TObject);
    Procedure acConnectExecute(Sender: TObject);
    Procedure acConnOptionsExecute(Sender: TObject);
    Procedure acCopyPathExecute(Sender: TObject);
    Procedure acDelTrackerExecute(Sender: TObject);
    Procedure acEditTrackerExecute(Sender: TObject);
    Procedure acFilterPaneExecute(Sender: TObject);
    Procedure acFolderGroupingExecute(Sender: TObject);
    Procedure acHideAppExecute(Sender: TObject);
    Procedure acInfoPaneExecute(Sender: TObject);
    Procedure acLabelGroupingExecute(Sender: TObject);
    Procedure acMenuShowExecute(Sender: TObject);
    Procedure acMoveTorrentExecute(Sender: TObject);
    Procedure acNewConnectionExecute(Sender: TObject);
    Procedure acOpenContainingFolderExecute(Sender: TObject);
    Procedure acOpenFileExecute(Sender: TObject);
    Procedure acOptionsExecute(Sender: TObject);
    Procedure acDisconnectExecute(Sender: TObject);
    Procedure acExportExecute(Sender: TObject);
    Procedure acImportExecute(Sender: TObject);
    Procedure acExitExecute(Sender: TObject);
    Procedure acDaemonOptionsExecute(Sender: TObject);
    Procedure acQMoveBottomExecute(Sender: TObject);
    Procedure acQMoveDownExecute(Sender: TObject);
    Procedure acQMoveTopExecute(Sender: TObject);
    Procedure acQMoveUpExecute(Sender: TObject);
    Procedure acReannounceTorrentExecute(Sender: TObject);
    Procedure acRemoveTorrentAndDataExecute(Sender: TObject);
    Procedure acRemoveTorrentExecute(Sender: TObject);
    Procedure acRenameExecute(Sender: TObject);
    Procedure acResolveCountryExecute(Sender: TObject);
    Procedure acResolveHostExecute(Sender: TObject);
    Procedure acSelectAllExecute(Sender: TObject);
    Procedure acSetHighPriorityExecute(Sender: TObject);
    Procedure acSetLowPriorityExecute(Sender: TObject);
    Procedure acSetNormalPriorityExecute(Sender: TObject);
    Procedure acSetNotDownloadExecute(Sender: TObject);
    Procedure acSetLabelsExecute(Sender: TObject);
    Procedure acSetupColumnsExecute(Sender: TObject);
    Procedure acShowAppExecute(Sender: TObject);
    Procedure acShowCountryFlagExecute(Sender: TObject);
    Procedure acStartAllTorrentsExecute(Sender: TObject);
    Procedure acStatusBarExecute(Sender: TObject);
    Procedure acStatusBarSizesExecute(Sender: TObject);
    Procedure acStopAllTorrentsExecute(Sender: TObject);
    Procedure acStopTorrentExecute(Sender: TObject);
    Procedure acStartTorrentExecute(Sender: TObject);
    Procedure acForceStartTorrentExecute(Sender: TObject);
    Procedure acStopTorrentFilterExecute(Sender: TObject);
    Procedure acStartTorrentFilterExecute(Sender: TObject);
    Procedure acForceStartTorrentFilterExecute(Sender: TObject);
    Procedure gTorrentsMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    Procedure gTorrentsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure LocalWatchTimerTimer(Sender: TObject);
    Procedure lvFilesClick(Sender: TObject);
    Procedure lvFilesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure MenuItem107Click(Sender: TObject);
    Procedure MenuShowExecute(Sender: TObject);
    Procedure acToolbarShowExecute(Sender: TObject);
    Procedure acTorrentPropsExecute(Sender: TObject);
    Procedure acTrackerGroupingExecute(Sender: TObject);
    Procedure acUpdateBlocklistExecute(Sender: TObject);
    Procedure acUpdateGeoIPExecute(Sender: TObject);
    Procedure acVerifyTorrentExecute(Sender: TObject);
    Procedure ApplicationPropertiesEndSession(Sender: TObject);
    Procedure ApplicationPropertiesException(Sender: TObject; E: Exception);
    Procedure ApplicationPropertiesIdle(Sender: TObject; Var Done: Boolean);
    Procedure ApplicationPropertiesMinimize(Sender: TObject);
    Procedure ApplicationPropertiesRestore(Sender: TObject);
    Procedure edSearchChange(Sender: TObject);
    Procedure edSearchKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure FormActivate(Sender: TObject);
    Procedure FormDropFiles(Sender: TObject; Const FileNames: Array Of String);
    Procedure FormKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure FormWindowStateChange(Sender: TObject);
    Procedure gTorrentsCellAttributes(Sender: TVarGrid;
      ACol, ARow, ADataCol: Integer; AState: TGridDrawState;
      Var CellAttribs: TCellAttributes);
    Procedure gTorrentsClick(Sender: TObject);
    Procedure gTorrentsDblClick(Sender: TObject);
    Procedure gTorrentsDrawCell(Sender: TVarGrid; ACol, ARow, ADataCol: Integer;
      AState: TGridDrawState; Const R: TRect; Var ADefaultDrawing: Boolean);
    Procedure gTorrentsEditorHide(Sender: TObject);
    Procedure gTorrentsEditorShow(Sender: TObject);
    Procedure gTorrentsQuickSearch(Sender: TVarGrid; Var SearchText: String;
      Var ARow: Integer);
    Procedure gTorrentsResize(Sender: TObject);
    Procedure gTorrentsSetEditText(Sender: TObject; ACol, ARow: Integer;
      Const Value: String);
    Procedure gTorrentsSortColumn(Sender: TVarGrid; Var ASortCol: Integer);
    Procedure gTorrentsKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure HSplitterChangeBounds(Sender: TObject);
    Procedure lvFilesDblClick(Sender: TObject);
    Procedure lvFilesEditorHide(Sender: TObject);
    Procedure lvFilesEditorShow(Sender: TObject);
    Procedure lvFilesSetEditText(Sender: TObject; ACol, ARow: Integer;
      Const Value: String);
    Procedure lvFilesKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure lvFilterCellAttributes(Sender: TVarGrid;
      ACol, ARow, ADataCol: Integer; AState: TGridDrawState;
      Var CellAttribs: TCellAttributes);
    Procedure lvFilterClick(Sender: TObject);
    Procedure lvFilterDrawCell(Sender: TVarGrid; ACol, ARow, ADataCol: Integer;
      AState: TGridDrawState; Const R: TRect; Var ADefaultDrawing: Boolean);
    Procedure lvPeersCellAttributes(Sender: TVarGrid;
      ACol, ARow, ADataCol: Integer; AState: TGridDrawState;
      Var CellAttribs: TCellAttributes);
    Procedure lvTrackersCellAttributes(Sender: TVarGrid;
      ACol, ARow, ADataCol: Integer; AState: TGridDrawState;
      Var CellAttribs: TCellAttributes);
    Procedure lvTrackersDblClick(Sender: TObject);
    Procedure lvTrackersKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure goDevelopmentSiteClick(Sender: TObject);
    Procedure MainToolBarContextPopup(Sender: TObject; MousePos: TPoint;
      Var Handled: Boolean);
    Procedure MenuItem101Click(Sender: TObject);
    Procedure miHomePageClick(Sender: TObject);
    Procedure PageInfoResize(Sender: TObject);
    Procedure panReconnectResize(Sender: TObject);
    Procedure pbDownloadedPaint(Sender: TObject);
    Procedure StatusBarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure TickTimerTimer(Sender: TObject);
    Procedure FilterTimerTimer(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormResize(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure lvFilterResize(Sender: TObject);
    Procedure miAboutClick(Sender: TObject);
    Procedure miCopyLabelClick(Sender: TObject);
    Procedure PageInfoChange(Sender: TObject);
    Procedure tbSearchCancelClick(Sender: TObject);
    Procedure TorrentsListTimerTimer(Sender: TObject);
    Procedure pmFilesPopup(Sender: TObject);
    Procedure pmTorrentsPopup(Sender: TObject);
    Procedure TrayIconDblClick(Sender: TObject);
    Procedure VSplitterChangeBounds(Sender: TObject);
    Function CorrectPath(path: String): String;
    // PETROV
  Private
    showMaximized: Boolean;
    FStarted: Boolean;
    FTorrents: TVarList;
    FFiles: TVarList;
    FTrackers: TStringList;
    FResolver: TIpResolver;
    FUnZip: TUnZipper;
    FReconnectWaitStart: TDateTime;
    FReconnectTimeOut: Integer;
    FTorrentProgress: TBitmap;
    FLastPieces: String;
    FLastPieceCount: Integer;
    FLastDone: Double;
    FCurConn: String;
    FPathMap: TStringList;
    FLastFilterIndex: Integer;
    RebuildTree: Boolean;
    FFilterChanged: Boolean;
    FCurDownSpeedLimit: Integer;
    FCurUpSpeedLimit: Integer;
    FFlagsPath: String;
    FAddingTorrent: Integer;
    FPendingTorrents: TStringList;
    FLinksFromClipboard: Boolean;
    FLastClipboardLink: String;
    FLinuxOpenDoc: Integer;
    // no del!
    FFromNow: Boolean;
    FWatchLocalFolder: String;
    FWatchDestinationFolder: String;
    FWatchDownloading: Boolean;
    FRow: Integer;
    FCol: Integer;
    {$ifdef windows}
      FFileManagerDefault: string;
      FFileManagerDefaultParam: string;
      FFileManagerSelectParam: string;
      FGlobalHotkey: string;
      fGlobalHotkeyMod: string;
      FUserDefinedMenuEx: string;
      FUserDefinedMenuParam: string;
      FUserDefinedMenuParamType: string;
    {$endif windows}
    {$ifdef LCLcarbon}
      FFormActive: boolean;
    {$endif LCLcarbon}
    FSlowResponse: TProgressImage;
    FDetailsWait: TProgressImage;
    FDetailsWaitStart: TDateTime;
    FMainFormShown: Boolean;
    FFilesTree: TFilesTree;
    FFilesCapt: String;
    FCalcAvg: Boolean;
    FPasswords: TStringList;
    FAppProps: TApplicationProperties;

    Procedure UpdateUI;
    Procedure UpdateUIRpcVersion(RpcVersion: Integer);
    Function DoConnect: Boolean;
    Procedure DoCreateOutZipStream(Sender: TObject; Var AStream: TStream;
      AItem: TFullZipFileEntry);
    Procedure DoDisconnect;
    Procedure DoOpenFlagsZip(Sender: TObject; Var AStream: TStream);
    Procedure TorrentProps(PageNo: Integer; filtered: Boolean = False);
    Procedure ShowConnOptions(NewConnection: Boolean);
    Procedure SaveColumns(LV: TVarGrid; Const AName: String;
      FullInfo: Boolean = True);
    Procedure LoadColumns(LV: TVarGrid; Const AName: String;
      FullInfo: Boolean = True);
    Function GetTorrentError(t: TJSONObject; Status: Integer): String;
    Function SecondsToString(j: Integer): String;
    Function DoAddTorrent(Const FileName: Utf8string): Boolean;
    Procedure UpdateTray;
    Procedure HideApp;
    Procedure ShowApp;
    Procedure DownloadFinished(Const TorrentName: String);
    Function GetFlagImage(Const CountryCode: String): Integer;
    Procedure BeforeCloseApp;
    Function GetGeoIpDatabase: String;
    Function GetFlagsArchive: String;
    Function DownloadGeoIpDatabase(AUpdate: Boolean): Boolean;
    Procedure TorrentColumnsChanged;
    Function EtaToString(ETA: Integer): String;
    Function GetTorrentStatus(TorrentIdx: Integer): String;
    Function GetSeedsText(Seeds, SeedsTotal: Integer): String;
    Function GetPeersText(Peers, PeersTotal, Leechers: Integer): String;
    Function RatioToString(Ratio: Double): String;
    Function TorrentDateTimeToString(d: Int64; FromNow: Boolean = False): String;
    Procedure DoRefresh(All: Boolean = False);
    Function GetFilesCommonPath(files: TJSONArray): String;
    Procedure InternalRemoveTorrent(Const Msg, MsgMulti: String;
      RemoveLocalData: Boolean);
    Function IncludeProperTrailingPathDelimiter(Const s: String): String;
    Procedure UrlLabelClick(Sender: TObject);
    Procedure CenterReconnectWindow;
    Procedure ProcessPieces(Const Pieces: String; PieceCount: Integer;
      Const Done: Double);
    Function ExecRemoteFile(Const FileName: String; SelectFile: Boolean;
      TorrentId: Integer; Userdef: Boolean = False): Boolean;
    Function ExecRemoteFileArray(Const FileName: String; SelectFile: Boolean;
      Ids: Variant; Userdef: Boolean = False): Boolean;
    Function GetSelectedTorrents: Variant;
    Function GetFilteredTorrents: Variant;
    Function GetDisplayedTorrents: Variant;
    Procedure FillDownloadDirs(CB: TComboBox; Const CurFolderParam: String);
    Procedure SaveDownloadDirs(CB: TComboBox; Const CurFolderParam: String);
    Procedure DeleteDirs(CB: TComboBox; maxdel: Integer);
    Procedure SetRefreshInterval;
    Procedure AddTracker(EditMode: Boolean);
    Procedure UpdateConnections;
    Procedure DoConnectToHost(Sender: TObject);
    Procedure FillSpeedsMenu;
    Procedure DoSetDownloadSpeed(Sender: TObject);
    Procedure DoSetUploadSpeed(Sender: TObject);
    Procedure SetSpeedLimit(Const Dir: String; Speed: Integer);
    Function FixSeparators(Const p: String): String;
    Function MapRemoteToLocal(Const RemotePath: String): String;
    Procedure CheckAddTorrents;
    Procedure CheckClipboardLink;
    Procedure CenterDetailsWait;
    Procedure ReadLocalFolderWatch;
    Function GetPageInfoType(pg: TTabSheet): TAdvInfoType;
    Procedure DetailsUpdated;
    Function RenameTorrent(TorrentId: Integer;
      Const OldPath, NewName: String): Boolean;
    Procedure FilesTreeStateChanged(Sender: TObject);
    Function SelectTorrent(TorrentId, TimeOut: Integer): Integer;
    Procedure OpenCurrentTorrent(OpenFolderOnly: Boolean; UserDef: Boolean = False);
  Public
    Procedure FillTorrentsList(list: TJSONArray);
    Procedure FillPeersList(list: TJSONArray);
    Procedure FillFilesList(ATorrentId: Integer;
      list, priorities, wanted: TJSONArray; Const DownloadDir: WideString);
    Procedure FillGeneralInfo(t: TJSONObject);
    Procedure FillTrackersList(TrackersData: TJSONObject);
    Procedure FillSessionInfo(s: TJSONObject);
    Procedure FillStatistics(s: TJSONObject);
    Procedure CheckStatus(Fatal: Boolean = True);
    Function TorrentAction(Const TorrentIds: Variant; Const AAction: String;
      args: TJSONObject = nil): Boolean;
    Function SetFilePriority(TorrentId: Integer; Const Files: Array Of Integer;
      Const APriority: String): Boolean;
    Function SetCurrentFilePriority(Const APriority: String): Boolean;
    Procedure SetTorrentPriority(APriority: Integer);
    Procedure ClearDetailsInfo(Skip: TAdvInfoType = aiNone);
    Function SelectRemoteFolder(Const CurFolder, DialogTitle: String): String;
    Procedure ConnectionSettingsChanged(Const ActiveConnection: String;
      ForceReconnect: Boolean);
    Procedure StatusBarSizes;
  Private
    Procedure _onException(Sender: TObject; E: Exception);
  End;

Function ExcludeInvalidChar(path: String): String;
// PETROV
Function GetBiDi: TBiDiMode;
Function CheckAppParams: Boolean;
Procedure LoadTranslation;
Function GetHumanSize(sz: Double; RoundTo: Integer = 0;
  Const EmptyStr: String = '-'): String;
Function PriorityToStr(p: Integer; Var ImageIndex: Integer): String;
Procedure DrawProgressCell(Sender: TVarGrid; ACol, ARow, ADataCol: Integer;
  AState: TGridDrawState; Const ACellRect: TRect);

Var
  FreeSpacePaths: TStringMap;
  MainForm: TMainForm;
  RpcObj: TRpc;
  FTranslationFileName: String;
  FTranslationLanguage: String;
  FAlterColor: TColor;
  IsUnity: Boolean;
  Ini: TIniFileUtf8;
  FHomeDir: String;
  {$ifdef windows}
  PrevWndProc: windows.WNDPROC;
  HotKeyID: Integer;
  {$endif windows}

Const
  // Torrents list
  idxName = 0;
  idxSize = 1;
  idxDone = 2;
  idxStatus = 3;
  idxSeeds = 4;
  idxPeers = 5;
  idxDownSpeed = 6;
  idxUpSpeed = 7;
  idxETA = 8;
  idxRatio = 9;
  idxDownloaded = 10;
  idxUploaded = 11;
  idxTracker = 12;
  idxTrackerStatus = 13;
  idxAddedOn = 14;
  idxCompletedOn = 15;
  idxLastActive = 16;
  idxPath = 17;
  idxPriority = 18;
  idxSizeToDowload = 19;
  idxTorrentId = 20;
  idxQueuePos = 21;
  idxSeedingTime = 22;
  idxSizeLeft = 23;
  idxPrivate = 24;
  idxLabels = 25;


  idxTag = -1;
  idxSeedsTotal = -2;
  idxLeechersTotal = -3;
  idxStateImg = -4;
  idxDeleted = -5;
  idxDownSpeedHistory = -6;
  idxUpSpeedHistory = -7;
  TorrentsExtraColumns = 7;

  // Peers list
  idxPeerHost = 0;
  idxPeerPort = 1;
  idxPeerCountry = 2;
  idxPeerClient = 3;
  idxPeerFlags = 4;
  idxPeerDone = 5;
  idxPeerUpSpeed = 6;
  idxPeerDownSpeed = 7;
  idxPeerTag = -1;
  idxPeerIP = -2;
  idxPeerCountryImage = -3;
  PeersExtraColumns = 3;

  // Trackers list
  idxTrackersListName = 0;
  idxTrackersListStatus = 1;
  idxTrackersListUpdateIn = 2;
  idxTrackersListSeeds = 3;
  idxTrackerTag = -1;
  idxTrackerID = -2;
  TrackersExtraColumns = 2;

  // Filter idices
  fltAll = 0;
  fltDown = 1;
  fltDone = 2;
  fltActive = 3;
  fltInactive = 4;
  fltStopped = 5;
  fltError = 6;
  fltWaiting = 7;

  // Status images
  imgDown = 9;
  imgSeed = 10;
  imgDownError = 11;
  imgSeedError = 12;
  imgError = 13;
  imgDone = 14;
  imgStopped = 29;
  imgDownQueue = 16;
  imgSeedQueue = 17;
  imgAll = 19;
  imgActive = 20;
  imgInactive = 15;
  imgWaiting = 42;
  imgCheck = 49;
  imgCheckWaiting = 50;

  StatusFiltersCount = 8;

  TorrentFieldsMap: Array[idxName..idxLabels] Of String =
    ('', 'totalSize', '', 'status', 'peersSendingToUs,seeders',
    'peersGettingFromUs,leechers', '', '', 'eta', 'uploadRatio',
    'downloadedEver', 'uploadedEver', '', '', 'addedDate', 'doneDate',
    'activityDate', '', 'bandwidthPriority', '', '', 'queuePosition',
    'secondsSeeding', 'leftUntilDone', 'isPrivate', 'labels');

  FinishedQueue = 1000000;

  TR_PRI_SKIP = -1000;
  // psedudo priority
  TR_PRI_LOW = -1;
  TR_PRI_NORMAL = 0;
  TR_PRI_HIGH = 1;

Implementation

Uses
  {$ifdef linux}
process, dynlibs,
  {$endif linux}
  {$ifdef darwin}
urllistenerosx,
  {$endif darwin}
  synacode, ConnOptions, clipbrd, DateUtils, TorrProps, DaemonOptions, About,
  ToolWin, download, ColSetup, AddLink, MoveTorrent, ssl_openssl_lib, AddTracker,
  lcltype,
  Options, ButtonPanel, BEncode, synautil, Math;

{TMyHashMap}
Function TMyHashMap.DefaultHashKey(Const Key: Integer): Integer;
Begin
  Result := Key;
  If Odd(Result) Then
    Result := Result * 3;
End;

Function TMyHashMap.DefaultKeysEqual(Const A, B: Integer): Boolean;
Begin
  Result := A = B;
End;

Function TMyHashMap.DefaultKeyToString(Const Key: Integer): String;
Begin
  WriteStr(Result, Key);
End;

Function TMyHashMap.DefaultItemToString(Const Item: Integer): String;
Begin
  WriteStr(Result, Item);
End;

Const
  TR_STATUS_CHECK_WAIT_1 = (1 Shl 0);
  // Waiting in queue to check files
  TR_STATUS_CHECK_1 = (1 Shl 1);
  // Checking files
  TR_STATUS_DOWNLOAD_1 = (1 Shl 2);
  // Downloading
  TR_STATUS_SEED_1 = (1 Shl 3);
  // Seeding
  TR_STATUS_STOPPED_1 = (1 Shl 4);
  // Torrent is stopped

  TR_STATUS_STOPPED_2 = 0;
  // Torrent is stopped
  TR_STATUS_CHECK_WAIT_2 = 1;
  // Queued to check files
  TR_STATUS_CHECK_2 = 2;
  // Checking files
  TR_STATUS_DOWNLOAD_WAIT_2 = 3;
  // Queued to download
  TR_STATUS_DOWNLOAD_2 = 4;
  // Downloading
  TR_STATUS_SEED_WAIT_2 = 5;
  // Queued to seed
  TR_STATUS_SEED_2 = 6;
  // Seeding

  TR_STATUS_FINISHED = $100;
  // Torrent is finished (pseudo status)

  TR_SPEEDLIMIT_GLOBAL = 0;
  // only follow the overall speed limit
  TR_SPEEDLIMIT_SINGLE = 1;
  // only follow the per-torrent limit
  TR_SPEEDLIMIT_UNLIMITED = 2;
  // no limits at all

  SpeedHistorySize = 20;

Const
  SizeNames: Array[1..5] Of String = (sByte, sKByte, sMByte, sGByte, sTByte);

Var
  TR_STATUS_STOPPED, TR_STATUS_CHECK_WAIT, TR_STATUS_CHECK,
  TR_STATUS_DOWNLOAD_WAIT, TR_STATUS_DOWNLOAD, TR_STATUS_SEED_WAIT,
  TR_STATUS_SEED: Integer;


{$ifdef windows}
Function WndCallback(Ahwnd: HWND; uMsg: UINT; wParam: WParam; lParam: LParam):

                                                                         LRESULT
;
stdcall;
Begin
  If (uMsg=WM_HOTKEY) And (WParam=HotKeyID) Then
    Begin
      If (MainForm.Visible = false) Or (MainForm.WindowState = wsMinimized) Then
        MainForm.ShowApp
      Else
        MainForm.HideApp;
    End;
  result := CallWindowProc(PrevWndProc,Ahwnd, uMsg, WParam, LParam);
End;

  {$endif windows}

Function IsHash(Hash: String): Boolean;
Var
  i: Integer;
Begin
  Result := False;
  If Hash = '' Then exit;
  If Length(Hash) = 32 Then   // possible base32 encoded hash
  Try
    Hash := StrToHex(Base32Decode(UpperCase(Hash)));
  Except
    exit;
  End;
  If Length(Hash) <> 40 Then exit;
  Result := True;
  For i := 1 To 40 Do
    If Not (Hash[i] In ['a' .. 'f', 'A'..'F', '0'..'9']) Then Result := False;
End;

Procedure TMainForm.ReadLocalFolderWatch;
Var
  sr: TSearchRec;
Begin
  If FPendingTorrents.Count = 0 Then
  Begin
    If FindFirstUTF8(FWatchLocalFolder + '*.torrent', faAnyFile, sr) = 0 Then
      Repeat
        FPendingTorrents.Add(FWatchLocalFolder + sr.Name);
      Until FindNextUTF8(sr) <> 0;
    FindCloseUTF8(sr);
  End;
End;

Function GetHumanSize(sz: Double; RoundTo: Integer; Const EmptyStr: String): String;
Var
  i: Integer;
Begin
  If sz < 0 Then
  Begin
    Result := EmptyStr;
    exit;
  End;
  i := Low(SizeNames);
  If RoundTo > 0 Then
  Begin
    Inc(i);
    sz := sz / 1024;
  End;
  While i <= High(SizeNames) Do
  Begin
    If sz < 1024 Then
      break;
    sz := sz / 1024;
    Inc(i);
  End;
  If (RoundTo = 0) And (i > 3) Then
    RoundTo := i - 2;
  Result := Format('%.' + IntToStr(RoundTo) + 'f %s', [sz, SizeNames[i]]);
End;

Function AddToChannel(Clr: TColor; Value: Integer; Position: Byte): TColor;
Var
  i: Integer;
Begin
  i := (Clr Shr (Position * 8)) And $FF;
  i := i + Value;
  If i < 0 Then i := 0;
  If i > $FF Then i := $FF;
  Result := Clr And (Not (Cardinal($FF) Shl (Position * 8))) Or
    (Cardinal(i) Shl (Position * 8));
End;

Function AddToColor(Color: TColor; R, G, B: Integer): TColor;
Begin
  Result := ColorToRGB(Color);
  Result := AddToChannel(Result, R, 0);
  Result := AddToChannel(Result, G, 1);
  Result := AddToChannel(Result, B, 2);
End;

Function GetLikeColor(Color: TColor; Delta: Integer): TColor;
Var
  i, j: Integer;
Begin
  Result := ColorToRGB(Color);
  j := Result And $FF;
  //red
  i := (Result Shr 8) And $FF;
  // green
  If i > j Then
    j := i;
  i := ((Result Shr 16) And $FF) Shr 1;
  // blue
  If i > j Then
    j := i;
  If j < $80 Then
    i := (($80 - j) Div $20 + 1) * Delta
  Else
    i := Delta;
  If (i + j > 255) Or (i + j < 0) Then
    i := -Delta;

  Result := AddToColor(Result, i, i, i);
End;

Function LocateFile(Const FileName: String; Const Paths: Array Of String): String;
Var
  i: Integer;
Begin
  For i := Low(Paths) To High(Paths) Do
  Begin
    Result := IncludeTrailingPathDelimiter(Paths[i]) + FileName;
    If FileExistsUTF8(Result) Then
      exit;
  End;
  Result := '';
End;

Procedure OnTranslate(Sender: TResTranslator; Const ResourceName: Ansistring;
  Var Accept: Boolean);
Const
  IgnoreUnits: Array[0..12] Of String =
    ('fpjson', 'jsonparser', 'jsonscanner', 'lclstrconsts', 'math',
    'rtlconsts', 'sysconst', 'variants', 'zbase', 'zipper', 'zstream',
    'xmlcfg', 'registry');

  IgnoreControls: Array[0..3] Of String =
    ('AboutForm.txAuthor', 'MainForm.miLn', 'ConnOptionsForm.cbUseSocks5',
    'ConnOptionsForm.tabConnection');
Var
  i: Integer;
Begin
  Accept := Not AnsiMatchText(Copy2Symb(ResourceName, '.'), IgnoreUnits) Or
    AnsiStartsText('lclstrconsts.rsMb', ResourceName)
    //<-- dialog buttons
    Or AnsiStartsText('lclstrconsts.rsMt', ResourceName);
  //<-- dialog message
  If Accept Then
    For i := Low(IgnoreControls) To High(IgnoreControls) Do
      If AnsiStartsText(IgnoreControls[i], ResourceName) Then
      Begin
        Accept := False;
        exit;
      End;
  If Accept And (Copy(ResourceName, Length(ResourceName) - 8, MaxInt) =
    '.Category') Then
    Accept := False;
End;

Var
  FIPCFileName: String;
  FRunFileName: String;

Function IsProtocolSupported(Const url: String): Boolean;
Const
  Protocols: Array [1..3] Of String =
    ('http:', 'https:', 'magnet:');
Var
  i: Integer;
  s: String;
Begin
  s := AnsiLowerCase(url);
  For i := Low(Protocols) To High(Protocols) Do
    If Copy(s, 1, Length(Protocols[i])) = Protocols[i] Then
    Begin
      Result := True;
      exit;
    End;
  Result := False;
End;

Procedure AddTorrentFile(Const FileName: String);
Var
  h: System.THandle;
  t: TDateTime;
  s: String;
Begin
  If Not IsProtocolSupported(FileName) And Not FileExistsUTF8(FileName) Then
    exit;
  t := Now;
  Repeat
    If FileExistsUTF8(FIPCFileName) Then
      h := FileOpenUTF8(FIPCFileName, fmOpenWrite Or fmShareDenyRead Or
        fmShareDenyWrite)
    Else
      h := FileCreateUTF8(FIPCFileName);
    If h <> System.THandle(-1) Then
    Begin
      s := FileName + LineEnding;
      FileSeek(h, 0, soFromEnd);
      FileWrite(h, s[1], Length(s));
      FileClose(h);
      break;
    End;
    Sleep(20);
  Until Now - t >= 3 / SecsPerDay;
End;

Procedure LoadTranslation;
Begin
  If Ini.ReadBool('Translation', 'TranslateForm', True) = False Then
    FTranslationLanguage := 'English'
  Else
  Begin
    FTranslationFileName := Ini.ReadString('Interface', 'TranslationFile', '');
    If FTranslationFileName <> '-' Then
      If (FTranslationFileName = '') Or Not IsTranslationFileValid(
        DefaultLangDir + FTranslationFileName) Then
        FTranslationLanguage := LoadDefaultTranslationFile(@OnTranslate)
      Else
        FTranslationLanguage :=
          LoadTranslationFile(DefaultLangDir + FTranslationFileName,
          @OnTranslate);
    If FTranslationLanguage = '' Then
      FTranslationLanguage := 'English';
  End;
End;

Function CheckAppParams: Boolean;
Var
  i: Integer;
  s: String;
  h: System.THandle;
  pid: SizeUInt;
  {$ifdef linux}
  proc: TProcess;
  sr: TSearchRec;
  hLib: TLibHandle;
  {$endif linux}
Begin
  Application.Title := AppName;
  {$ifdef linux}
  IsUnity := CompareText(GetEnvironmentVariable('XDG_CURRENT_DESKTOP'), 'unity')
             = 0;
  If GetEnvironmentVariable('LIBOVERLAY_SCROLLBAR') <> '0' Then
    Begin
      i := FindFirstUTF8('/usr/lib/liboverlay-scrollbar*', faAnyFile, sr);
      FindClose(sr);
      hLib := LoadLibrary('liboverlay-scrollbar.so');
      If hLib <> 0 Then
        FreeLibrary(hLib);
      If (i = 0) Or (hLib <> 0) Then
        Begin
          // Turn off overlay scrollbars, since they are not supported yet.
          // Restart the app with the LIBOVERLAY_SCROLLBAR=0 env var.
          proc := TProcess.Create(Nil);
          Try
            proc.Executable := ParamStrUTF8(0);
            For i:=1 To ParamCount Do
              proc.Parameters.Add(ParamStrUTF8(i));
            For i:=0 To GetEnvironmentVariableCount - 1 Do
              proc.Environment.Add(GetEnvironmentString(i));
            proc.Environment.Values['LIBOVERLAY_SCROLLBAR'] := '0';
            proc.Execute;
          Finally
            proc.Free;
        End;
      Result := False;
      exit;
    End;
End;
  {$endif linux}
  FHomeDir := GetCmdSwitchValue('home');
  If FHomeDir = '' Then
  Begin
    If FileExistsUTF8(ChangeFileExt(ParamStrUTF8(0), '.ini')) Then
      FHomeDir := ExtractFilePath(ParamStrUTF8(0)) // Portable mode
    Else
      FHomeDir := IncludeTrailingPathDelimiter(GetAppConfigDirUTF8(False));
  End
  Else
    FHomeDir := IncludeTrailingPathDelimiter(FHomeDir);
  ForceDirectoriesUTF8(FHomeDir);
  FIPCFileName := FHomeDir + 'ipc.txt';
  FRunFileName := FHomeDir + 'run';

  Ini := TIniFileUtf8.Create(FHomeDir + ChangeFileExt(ExtractFileName(
    ParamStrUTF8(0)), '.ini'));
  Ini.CacheUpdates := True;

  // Check for outdated IPC file
  If FileExistsUTF8(FIPCFileName) Then
  Begin
    h := FileOpenUTF8(FIPCFileName, fmOpenRead Or fmShareDenyNone);
    If h <> INVALID_HANDLE_VALUE Then
    Begin
      i := FileGetDate(h);
      FileClose(h);
      If (i > 0) And (Abs(Now - FileDateToDateTime(i)) > 1 / MinsPerDay) Then
        DeleteFileUTF8(FIPCFileName);
    End;
  End;

  For i := 1 To ParamCount Do
  Begin
    s := ParamStrUTF8(i);
    If IsProtocolSupported(s) Or FileExistsUTF8(s) Then
      AddTorrentFile(s);
  End;

  If FileExistsUTF8(FRunFileName) Then
  Begin
    // Another process is running
    h := FileOpenUTF8(FRunFileName, fmOpenRead Or fmShareDenyNone);
    If FileRead(h, pid, SizeOf(pid)) = SizeOf(pid) Then
    Begin
      {$ifdef mswindows}
      AllowSetForegroundWindow(pid);
      {$endif mswindows}
    End;
    FileClose(h);

    If Not FileExistsUTF8(FIPCFileName) Then
      FileClose(FileCreateUTF8(FIPCFileName));
    For i := 1 To 50 Do
      If Not FileExistsUTF8(FIPCFileName) Then
      Begin
        // The running process works normally. Exit application.
        Result := False;
        exit;
      End
      Else
        Sleep(200);
    // The running process is not responding
    DeleteFileUTF8(FRunFileName);
    // Delete IPC file if it is empty
    h := FileOpenUTF8(FIPCFileName, fmOpenRead Or fmShareDenyNone);
    i := FileSeek(h, 0, soFromEnd);
    FileClose(h);
    If i = 0 Then
      DeleteFileUTF8(FIPCFileName);
  End;

  // Create a new run file
  h := FileCreateUTF8(FRunFileName);
  pid := GetProcessID;
  FileWrite(h, pid, SizeOf(pid));
  FileClose(h);

  LoadTranslation;

  GetBiDi;

  SizeNames[1] := sByte;
  SizeNames[2] := sKByte;
  SizeNames[3] := sMByte;
  SizeNames[4] := sGByte;
  SizeNames[5] := sTByte;

  IntfScale := Ini.ReadInteger('Interface', 'Scaling', 100);

  Result := True;
End;

Function PriorityToStr(p: Integer; Var ImageIndex: Integer): String;
Begin
  Case p Of
    TR_PRI_SKIP:
    Begin
      Result := sSkip;
      ImageIndex := 23;
    End;
    TR_PRI_LOW:
    Begin
      Result := sLow;
      ImageIndex := 24;
    End;
    TR_PRI_NORMAL:
    Begin
      Result := sNormal;
      ImageIndex := 25;
    End;
    TR_PRI_HIGH:
    Begin
      Result := sHigh;
      ImageIndex := 26;
    End;
    Else
      Result := '???';
  End;
End;

Procedure DrawProgressCell(Sender: TVarGrid; ACol, ARow, ADataCol: Integer;
  AState: TGridDrawState; Const ACellRect: TRect);
Var
  R, RR: TRect;
  i, j, h: Integer;
  s: String;
  cl: TColor;
  Progress: Double;
  sz: TSize;
  ts: TTextStyle;
Begin
  Progress := Double(Sender.Items[ADataCol, ARow]);
  With Sender.Canvas Do
  Begin
    R := ACellRect;
    FrameRect(R);
    s := Format('%.1f%%', [Progress]);
    sz := TextExtent(s);
    InflateRect(R, -1, -1);
    Pen.Color := clBtnFace;
    Rectangle(R);
    InflateRect(R, -1, -1);

    i := R.Left + Round(Progress * (R.Right - R.Left) / 100.0);
    j := (R.Top + R.Bottom) Div 2;
    h := (R.Top + R.Bottom - sz.cy) Div 2;
    cl := GetLikeColor(clHighlight, 70);
    GradientFill(Rect(R.Left, R.Top, i, j), cl, clHighlight, gdVertical);
    GradientFill(Rect(R.Left, j, i, R.Bottom), clHighlight, cl, gdVertical);

    ts := TextStyle;
    ts.Layout := tlTop;
    ts.Alignment := taLeftJustify;
    ts.Wordbreak := False;
    TextStyle := ts;
    j := (R.Left + R.Right - sz.cx) Div 2;
    If i > R.Left Then
    Begin
      RR := Rect(R.Left, R.Top, i, R.Bottom);
      Font.Color := clHighlightText;
      TextRect(RR, j, h, s);
    End;
    If i < R.Right Then
    Begin
      RR := Rect(i, R.Top, R.Right, R.Bottom);
      Brush.Color := Sender.Color;
      FillRect(RR);
      Font.Color := clWindowText;
      TextRect(RR, j, h, s);
    End;
  End;
End;

{ TProgressImage }

Procedure TProgressImage.SetImages(Const AValue: TImageList);
Begin
  If FImages = AValue Then exit;
  FImages := AValue;
  Width := FImages.Width;
  Height := FImages.Height;
End;

Procedure TProgressImage.SetStartIndex(Const AValue: Integer);
Begin
  If FStartIndex = AValue Then exit;
  FStartIndex := AValue;
  UpdateIndex;
End;

Procedure TProgressImage.UpdateIndex;
Begin
  If (FImageIndex < FStartIndex) Or (FImageIndex > FEndIndex) Then
    FImageIndex := FStartIndex;
  Invalidate;
End;

Procedure TProgressImage.DoTimer(Sender: TObject);
Begin
  ImageIndex := ImageIndex + 1;
End;

Procedure TProgressImage.SetImageIndex(Const AValue: Integer);
Begin
  If FImageIndex = AValue Then exit;
  FImageIndex := AValue;
  UpdateIndex;
End;

Procedure TProgressImage.SetEndIndex(Const AValue: Integer);
Begin
  If FEndIndex = AValue Then exit;
  FEndIndex := AValue;
  UpdateIndex;
End;

Function TProgressImage.GetFrameDelay: Integer;
Begin
  Result := FTimer.Interval;
End;

Procedure TProgressImage.SetBorderColor(Const AValue: TColor);
Begin
  If FBorderColor = AValue Then exit;
  FBorderColor := AValue;
End;

Procedure TProgressImage.SetFrameDelay(Const AValue: Integer);
Begin
  FTimer.Interval := AValue;
End;

Procedure TProgressImage.Paint;
Begin
  If FBmp = nil Then
  Begin
    FBmp := TBitmap.Create;
    FBmp.Width := Width;
    FBmp.Height := Height;
  End;
  With FBmp.Canvas Do
  Begin
    Brush.Color := clBtnFace;
    If FBorderColor <> clNone Then
    Begin
      Pen.Color := FBorderColor;
      Rectangle(0, 0, FBmp.Width, FBmp.Height);
    End
    Else
      FillRect(0, 0, FBmp.Width, FBmp.Height);
    If FImages <> nil Then
      FImages.Draw(FBmp.Canvas, (Self.Width - FImages.Width) Div
        2, (Self.Height - FImages.Height) Div 2, ImageIndex);
  End;
  Canvas.Draw(0, 0, FBmp);
End;

Procedure TProgressImage.VisibleChanged;
Begin
  Inherited VisibleChanged;
  If Visible Then
  Begin
    ImageIndex := StartIndex;
    FTimer.Enabled := True;
  End
  Else
    FTimer.Enabled := False;
End;

Constructor TProgressImage.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 100;
  FTimer.OnTimer := @DoTimer;
  FBorderColor := clNone;
  Visible := False;
End;

Destructor TProgressImage.Destroy;
Begin
  FBmp.Free;
  Inherited Destroy;
End;

{ TMainForm }

Procedure TMainForm.FormCreate(Sender: TObject);
Var
  ws: TWindowState;
  i, j, iTag: Integer;
  R: TRect;
  SL: TStringList;
  MI: TMenuItem;
  Ico: TIcon;
  LargeIco, SmallIco: hIcon;
  MenuCaption: String;
  {$ifdef darwin}
  s: string;
  pic: TPicture;
  {$endif darwin}
Begin
  {$ifdef darwin}
  // Load better icon if possible
  s := ExtractFilePath(ParamStrUTF8(0)) + '..' + DirectorySeparator +
       'Resources'
       + DirectorySeparator + ChangeFileExt(ExtractFileName(ParamStrUTF8(0)),
       '.icns');
  If FileExistsUTF8(s) Then
    Begin
      pic := TPicture.Create;
      Try
        pic.LoadFromFile(s);
        Try
          Application.Icon.Assign(pic.Graphic);
        Except
    End;
Finally
  pic.Free;
End;
End;

RegisterURLHandler(@AddTorrentFile);
  {$endif darwin}


  {$if FPC_FULlVERSION>=30101}
  //  AllowReuseOfLineInfoData:=false;
  {$endif}
  FAppProps := TApplicationProperties.Create(Self);
  FAppProps.OnException := @_onException;
  FAppProps.CaptureExceptions := True;


  Application.Title := AppName + ' v' + AppVersion;
  Caption := Application.Title;
  txTransferHeader.Font.Size := Font.Size + 2;
  txTorrentHeader.Font.Size := txTransferHeader.Font.Size;
  TrayIcon.Icon.Assign(Application.Icon);
  RpcObj := TRpc.Create;
  FTorrents := TVarList.Create(gTorrents.Columns.Count, 0);
  FTorrents.ExtraColumns := TorrentsExtraColumns;
  gTorrents.Items.ExtraColumns := TorrentsExtraColumns;
  lvFiles.Items.ExtraColumns := FilesExtraColumns;
  FFiles := lvFiles.Items;
  FFilesTree := TFilesTree.Create(lvFiles);
  FFilesTree.Checkboxes := True;
  FFilesTree.OnStateChange := @FilesTreeStateChanged;
  lvPeers.Items.ExtraColumns := PeersExtraColumns;
  lvTrackers.Items.ExtraColumns := TrackersExtraColumns;
  FTrackers := TStringList.Create;
  FTrackers.Sorted := True;
  FReconnectTimeOut := 0;
  FAlterColor := GetLikeColor(gTorrents.Color, -$10);
  lvFilter.Items.ExtraColumns := 2;
  gTorrents.AlternateColor := FAlterColor;
  lvPeers.AlternateColor := FAlterColor;
  lvTrackers.AlternateColor := FAlterColor;
  gStats.AlternateColor := FAlterColor;
  FPendingTorrents := TStringList.Create;
  FFilesCapt := tabFiles.Caption;
  FPasswords := TStringList.Create;

  FSlowResponse := TProgressImage.Create(MainToolBar);
  With FSlowResponse Do
  Begin
    Align := alRight;
    Images := ImageList16;
    StartIndex := 30;
    EndIndex := 37;
    Width := ScaleInt(24);
    Left := MainToolBar.ClientWidth;
    Parent := MainToolBar;
  End;

  FDetailsWait := TProgressImage.Create(panDetailsWait);
  With FDetailsWait Do
  Begin
    Images := ImageList16;
    StartIndex := FSlowResponse.StartIndex;
    EndIndex := FSlowResponse.EndIndex;
    Width := Images.Width * 2;
    Height := Width;
    BorderColor := clBtnShadow;
    panDetailsWait.Width := Width;
    panDetailsWait.Height := Height;
    Parent := panDetailsWait;
  End;

  DoDisconnect;
  PageInfo.ActivePageIndex := 0;
  PageInfoChange(nil);
  {$ifdef LCLgtk2}
With MainToolBar Do
  Begin
    EdgeBorders := [ebLeft, ebTop, ebRight, ebBottom];
    EdgeInner := esNone;
    EdgeOuter := esRaised;
    Flat := True;
  End;
i := acAltSpeed.ImageIndex;
acAltSpeed.ImageIndex := -1;
tbtAltSpeed.ImageIndex := i;
  {$endif}
  txTransferHeader.Color := GetLikeColor(clBtnFace, -15);
  txTorrentHeader.Color := txTransferHeader.Color;
  txTransferHeader.Caption := ' ' + txTransferHeader.Caption;
  txTorrentHeader.Caption := ' ' + txTorrentHeader.Caption;
  txTransferHeader.Height :=
    txTransferHeader.Canvas.TextHeight(txTransferHeader.Caption) + 2;
  txTorrentHeader.Height := txTorrentHeader.Canvas.TextHeight(
    txTorrentHeader.Caption) + 2;

  With gStats Do
  Begin
    BeginUpdate;
    Try
      Items[0, 0] := UTF8Decode(SDownloaded);
      Items[0, 1] := UTF8Decode(SUploaded);
      Items[0, 2] := UTF8Decode(SFilesAdded);
      Items[0, 3] := UTF8Decode(SActiveTime);
    Finally
      EndUpdate;
    End;
  End;

  If Ini.ReadInteger('MainForm', 'State', -1) = -1 Then
  Begin
    R := Screen.MonitorFromRect(BoundsRect).WorkareaRect;
    If R.Right - R.Left < 300 Then
      R := Rect(0, 0, Screen.Width, Screen.Height);
    j := R.Right - R.Left;
    i := j * 3 Div 4;
    j := j * 95 Div 100;
    If i > Width Then
      Width := i;
    If Width > j Then
      Width := j;
    Left := (R.Right - R.Left - Width) Div 2;
    j := R.Bottom - R.Top;
    i := j * 3 Div 4;
    j := j * 8 Div 10;
    If i > Height Then
      Height := i;
    If Height > j Then
      Height := j;
    Top := (R.Bottom - R.Top - Height) Div 2;
  End
  Else
  Begin
    ws := TWindowState(Ini.ReadInteger('MainForm', 'State', Integer(WindowState)));
    Left := Ini.ReadInteger('MainForm', 'Left', Left);
    Top := Ini.ReadInteger('MainForm', 'Top', Top);
    Width := Ini.ReadInteger('MainForm', 'Width', Width);
    Height := Ini.ReadInteger('MainForm', 'Height', Height);
    If ws = wsMaximized Then
      showMaximized := True;
    //       LCLIntf.ShowWindow(Handle, SW_MAXIMIZE);
    //        WindowState:=wsMaximized;
  End;

  If Ini.ReadBool('MainForm', 'FilterPane', acFilterPane.Checked) <>
    acFilterPane.Checked Then
    acFilterPane.Execute;
  If Ini.ReadBool('MainForm', 'InfoPane', acInfoPane.Checked) <>
    acInfoPane.Checked Then
    acInfoPane.Execute;

  If Ini.ReadBool('MainForm', 'Menu', acMenuShow.Checked) <> acMenuShow.Checked Then
    acMenuShow.Execute;
  If Ini.ReadBool('MainForm', 'Toolbar', acToolbarShow.Checked) <>
    acToolbarShow.Checked Then
    acToolbarShow.Execute;
  If Ini.ReadBool('MainForm', 'BigToolbar', acBigToolBar.Checked) <>
    acBigToolBar.Checked Then
    acBigToolbar.Execute;
  If Ini.ReadBool('MainForm', 'StatusBar', acStatusBar.Checked) <>
    acStatusBar.Checked Then
    acStatusBar.Execute;
  If Ini.ReadBool('MainForm', 'StatusBarSizes', acStatusBarSizes.Checked) <>
    acStatusBarSizes.Checked Then
    acStatusBarSizes.Execute;

  FFromNow := Ini.ReadBool('MainForm', 'FromNow', False);
  FWatchLocalFolder := Ini.ReadString('Interface', 'WatchLocalFolder', '');
  If FWatchLocalFolder <> '' Then
    If DirPathExists(FWatchLocalFolder) And DirectoryIsWritable(FWatchLocalFolder) Then
    Begin
      FWatchLocalFolder := AppendPathDelim(FWatchLocalFolder);
      FWatchDestinationFolder :=
        Ini.ReadString('Interface', 'WatchDestinationFolder', '');
      LocalWatchTimer.Interval :=
        trunc(Ini.ReadFloat('Interface', 'WatchInterval', 1) * 60000);
      LocalWatchTimer.Enabled := True;
    End;
  LoadColumns(gTorrents, 'TorrentsList');
  TorrentColumnsChanged;
  LoadColumns(lvFiles, 'FilesList');
  LoadColumns(lvPeers, 'PeerList');
  LoadColumns(lvTrackers, 'TrackersList');

  acResolveHost.Checked := Ini.ReadBool('PeersList', 'ResolveHost', True);
  acResolveCountry.Checked :=
    Ini.ReadBool('PeersList', 'ResolveCountry', True) And (GetGeoIpDatabase <> '');
  acShowCountryFlag.Checked :=
    Ini.ReadBool('PeersList', 'ShowCountryFlag', True) And (GetFlagsArchive <> '');
  acShowCountryFlag.Enabled := acResolveCountry.Checked;
  FCurConn := Ini.ReadString('Hosts', 'CurHost', '');
  If FCurConn = '' Then
    FCurConn := Ini.ReadString('Connection', 'Host', '');
  FPathMap := TStringList.Create;
  If Application.HasOption('hidden') Then
  Begin
    ApplicationProperties.ShowMainForm := False;
    TickTimer.Enabled := True;
    UpdateTray;
  End;
  UpdateConnections;

  i := Ini.ReadInteger('Interface', 'LastRpcVersion', -1);
  If i >= 0 Then
    UpdateUIRpcVersion(i);

  bidiMode := GetBiDi;

  acFolderGrouping.Checked := Ini.ReadBool('Interface', 'FolderGrouping', True);
  acLabelGrouping.Checked := Ini.ReadBool('Interface', 'LabelGrouping', True);
  acTrackerGrouping.Checked := Ini.ReadBool('Interface', 'TrackerGrouping', True);
  FLinksFromClipboard := Ini.ReadBool('Interface', 'LinksFromClipboard', True);
  Application.OnActivate := @FormActivate;
  Application.OnException := @ApplicationPropertiesException;

  {$ifdef windows}
FFileManagerDefault := Ini.ReadString('Interface','FileManagerDefault',
                       'explorer.exe');
FFileManagerDefaultParam := Ini.ReadString('Interface',
                            'FileManagerDefaultParam', '%s');
FFileManagerSelectParam := Ini.ReadString('Interface', 'FileManagerDefaultParam'
                           , '/select,%s');
FGlobalHotkey := Ini.ReadString('Interface','GlobalHotkey','');
FGlobalHotkeyMod := Ini.ReadString('Interface','GlobalHotkeyMod','0');
HotKeyID := GlobalAddAtom('TransGUIHotkey');
PrevWndProc := windows.WNDPROC(SetWindowLongPtr(Self.Handle,GWL_WNDPROC,PtrInt(@
               WndCallback)));
RegisterHotKey(Self.Handle,HotKeyID, VKStringToWord(FGlobalHotkeyMod),
VKStringToWord(FGlobalHotkey));
// Create UserMenus if any in [UserMenu]
iTag := 1001;
j := 1;
Repeat
  MenuCaption := TranslateString(Ini.ReadString('UserTorrentMenu','Caption'+
                 IntToStr(j),'nocaption'),True);
  inc(J);
Until MenuCaption = 'nocaption';
dec(j);
If j > 0 Then
  Begin
    If J > 2 Then
      Begin
        MI := TMenuItem.Create(Self);
        MI.Caption := sUserMenu;
        MI.ImageIndex := 6;
        pmTorrents.Items.Insert(2,MI);
      End;
    For i := 1 To j-1 Do
      Begin
        MI := TMenuItem.Create(Self);
        MI.Caption := TranslateString(Ini.ReadString('UserTorrentMenu','Caption'
                      +IntToStr(i),''),True);
        If MI.Caption <> '-' Then
          Begin
            MI.Tag := iTag;
            //1000+i;
            MI.OnClick := @acOpenFileExecute;
          End;
        Inc(iTag);
        Try
          If ExtractIconEx(PChar(Ini.ReadString('UserTorrentMenu','ExeName'+
             IntToStr(i),'')), 0, LargeIco, SmallIco, 1) > null Then
            Begin
              Ico := TIcon.Create;
              Try
                Ico.Handle := SmallIco;
                Ico.Transparent := True;
                Ico.Masked := True;
              Finally
                ImageList16.AddIcon(Ico);
                Ico.Free;
                MI.ImageIndex := ImageList16.Count-1;
            End;
      End;
  Except
End;
MI.Caption := MI.Caption;
MI.Tag := MI.Tag;
MI.OnClick := MI.OnClick;
MI.ImageIndex := MI.ImageIndex;
If j > 2 Then pmTorrents.Items[2].Add(MI)
Else pmTorrents.Items.Insert(2,MI);
End;
End;
iTag := 2001;
j := 1;
Repeat
  MenuCaption := TranslateString(Ini.ReadString('UserFileMenu','Caption'+
                 IntToStr(j),'nocaption'),True);
  inc(J);
Until MenuCaption = 'nocaption';
dec(j);
If j > 0 Then
  Begin
    If J > 2 Then
      Begin
        MI := TMenuItem.Create(Self);
        MI.Caption := sUserMenu;
        MI.ImageIndex := 6;
        pmFiles.Items.Insert(4,MI);
      End;
    For i := 1 To j-1 Do
      Begin
        MI := TMenuItem.Create(Self);
        MI.Caption := TranslateString(Ini.ReadString('UserFileMenu','Caption'+
                      IntToStr(i),''),True);
        If MI.Caption <> '-' Then
          Begin
            MI.Tag := iTag;
            //1000+i;
            //                  MI.Tag:= 1000+i;
            MI.OnClick := @acOpenFileExecute;
          End;
        Inc(iTag);
        Try
          If ExtractIconEx(PChar(Ini.ReadString('UserFileMenu','ExeName'+
             IntToStr(i),'')), 0, LargeIco, SmallIco, 1) > null Then
            Begin
              Ico := TIcon.Create;
              Try
                Ico.Handle := SmallIco;
                Ico.Transparent := True;
                Ico.Masked := True;
              Finally
                ImageList16.AddIcon(Ico);
                Ico.Free;
                MI.ImageIndex := ImageList16.Count-1;
            End;
      End;
  Except
End;
MI.Caption := MI.Caption;
MI.Tag := MI.Tag;
MI.OnClick := MI.OnClick;
MI.ImageIndex := MI.ImageIndex;
If j > 2 Then pmFiles.Items[4].Add(MI)
Else pmFiles.Items.Insert(4,MI);
End;
End;
// end Create UserMenu
  {$else}
  {$ifdef darwin}
FLinuxOpenDoc := Ini.ReadInteger('Interface','FileOpenDoc',0);
// macOS - OpenURL(s, p) = Original version TRGUI
  {$else}
  FLinuxOpenDoc := Ini.ReadInteger('Interface', 'FileOpenDoc', 1);
  {$endif darwin}
  Ini.WriteInteger('Interface', 'FileOpenDoc', FLinuxOpenDoc);
  {$endif windows}

  //Dynamic Associations of ShortCuts to Actions/Menus
  SL := TStringList.Create;
  Try
    Ini.ReadSectionValues('ShortCuts', SL);
    If (SL.Text = '') Or (SL.Count <> ActionList.ActionCount) Then
    Begin
      For i := 0 To ActionList.ActionCount - 1 Do
        Ini.WriteString('Shortcuts', StringReplace(ActionList.Actions[i].Name,
          'ac', '', []), ShortcutToText(
          TAction(ActionList.Actions[i]).ShortCut));
      If (i < SL.Count - 1) And (SL.Text <> '') And
        (ActionList.ActionByName(SL.Names[i]) = nil) Then
        Ini.WriteString('Shortcuts', StringReplace(
          ActionList.Actions[i].Name, 'ac', '', []),
          ShortcutToText(TAction(ActionList.Actions[i]).ShortCut));
    End
    Else
      For i := 0 To SL.Count - 1 Do
      Try
        TAction(ActionList.ActionbyName('ac' + SL.Names[i])).ShortCut :=

          TextToShortcut(SL.ValueFromIndex[i]);
      Except
      End;
  Finally
    SL.Free;
  End;
  // StatusBar Panels width
  i := Ini.ReadInteger('StatusBarPanels', 'ScreenWidth', 0);
  If Screen.Width <> i Then
  Begin
    Ini.EraseSection('StatusBarPanels');
    Ini.WriteInteger('StatusBarPanels', 'ScreenWidth', Screen.Width);
  End;
  For i := 0 To StatusBar.Panels.Count - 1 Do
  Begin
    j := Ini.ReadInteger('StatusBarPanels', IntToStr(i), 0);
    If j <> 0 Then StatusBar.Panels[i].Width := j
    Else
      Ini.WriteInteger('StatusBarPanels', IntToStr(i), Statusbar.Panels[i].Width);
  End;
  {$IF LCL_FULLVERSION >= 1080000}
  PageInfo.Options := PageInfo.Options + [nboDoChangeOnSetIndex];
  {$ENDIF}
End;

Procedure TMainForm.FormDestroy(Sender: TObject);

  Procedure _CreateAllForms;
  Begin
    // Create all application forms to properly update language files
    TAboutForm.Create(Self).Free;
    TAddLinkForm.Create(Self).Free;
    TAddTorrentForm.Create(Self).Free;
    TAddTrackerForm.Create(Self).Free;
    TColSetupForm.Create(Self).Free;
    TConnOptionsForm.Create(Self).Free;
    TDaemonOptionsForm.Create(Self).Free;
    TDownloadForm.Create(Self).Free;
    TMoveTorrentForm.Create(Self).Free;
    TOptionsForm.Create(Self).Free;
    TTorrPropsForm.Create(Self).Free;
  End;

Begin
  If Application.HasOption('updatelang') Then
  Begin
    _CreateAllForms;
    SupplementTranslationFiles;
  End;
  If Application.HasOption('makelang') Then
  Begin
    _CreateAllForms;
    MakeTranslationFile;
  End;

  DeleteFileUTF8(FRunFileName);
  FPasswords.Free;
  FResolver.Free;
  FTrackers.Free;
  FUnZip.Free;
  RpcObj.Free;
  FTorrentProgress.Free;
  FPathMap.Free;
  FTorrents.Free;
  FPendingTorrents.Free;
  Try
    Ini.UpdateFile;
  Except
  End;
  {$ifdef windows}
UnRegisterHotkey(Self.Handle,HotKeyID);
GlobalDeleteAtom(HotKeyID);
  {$endif windows}

End;

Procedure TMainForm.FormResize(Sender: TObject);
Begin
  If panReconnect.Visible Then
    CenterReconnectWindow;
End;

Procedure TMainForm.FormShow(Sender: TObject);
Begin
  If Not FMainFormShown Then
  Begin
    FMainFormShown := True;
    VSplitter.SetSplitterPosition(Ini.ReadInteger('MainForm',
      'VSplitter', VSplitter.GetSplitterPosition));
    HSplitter.SetSplitterPosition(Ini.ReadInteger('MainForm',
      'HSplitter', HSplitter.GetSplitterPosition));
    MakeFullyVisible;
    If showMaximized Then
    Begin
      WindowState := wsFullScreen;
      //      LCLIntf.ShowWindow(Handle, SW_MAXIMIZE);
      showMaximized := False;
    End;

  End;
  If Not FStarted Then
    TickTimer.Enabled := True;
  UpdateTray;
End;

Procedure TMainForm.lvFilterResize(Sender: TObject);
Begin
  lvFilter.Columns[0].Width := lvFilter.ClientWidth;
End;

Procedure TMainForm.miAboutClick(Sender: TObject);
Begin
  With TAboutForm.Create(Self) Do
  Try
    ShowModal;
  Finally
    Free;
  End;
End;

Procedure TMainForm.miCopyLabelClick(Sender: TObject);
Begin
  With TLabel(pmLabels.PopupComponent) Do
    If (Length(Name) > 5) And (Copy(Name, Length(Name) - 4, 5) = 'Label') Then
      Clipboard.AsText := TLabel(Parent.FindChildControl(
        Copy(Name, 1, Length(Name) - 5))).Caption
    Else
      Clipboard.AsText := Caption;
End;

Procedure TMainForm.acConnectExecute(Sender: TObject);
Begin
  If RpcObj.Connected Then
  Begin
    tbConnect.CheckMenuDropdown;
    exit;
  End;
  If FCurConn = '' Then
    ShowConnOptions(True)
  Else
    DoConnect;
End;

Procedure TMainForm.acConnOptionsExecute(Sender: TObject);
Begin
  ShowConnOptions(False);
End;

Procedure TMainForm.acCopyPathExecute(Sender: TObject);
Begin
  If lvFiles.Items.Count > 0 Then
    Clipboard.AsText := '"' + FFilesTree.GetFullPath(lvFiles.Row) + '"';
End;

Procedure TMainForm.acDelTrackerExecute(Sender: TObject);
Var
  req, args: TJSONObject;
  id, torid: Integer;
Begin
  id := lvTrackers.Items[idxTrackerID, lvTrackers.Row];
  torid := RpcObj.CurTorrentId;
  If MessageDlg('', Format(SRemoveTracker,
    [UTF8Encode(WideString(lvTrackers.Items[idxTrackersListName, lvTrackers.Row]))]),
    mtConfirmation, mbYesNo, 0, mbNo) <> mrYes Then exit;
  AppBusy;
  Self.Update;
  req := TJSONObject.Create;
  Try
    req.Add('method', 'torrent-set');
    args := TJSONObject.Create;
    args.Add('ids', TJSONArray.Create([torid]));
    args.Add('trackerRemove', TJSONArray.Create([id]));
    req.Add('arguments', args);
    args := nil;
    args := RpcObj.SendRequest(req, False);
    If args = nil Then
    Begin
      CheckStatus(False);
      exit;
    End;
  Finally
    args.Free;
    req.Free;
  End;
  DoRefresh;
  AppNormal;
End;

Procedure TMainForm.acEditTrackerExecute(Sender: TObject);
Begin
  AddTracker(True);
End;

Procedure TMainForm.acFilterPaneExecute(Sender: TObject);
Begin
  acFilterPane.Checked := Not acFilterPane.Checked;
  panFilter.Visible := acFilterPane.Checked;
  HSplitter.Visible := acFilterPane.Checked;
  HSplitter.Left := panFilter.Width;
  If lvFilter.Items.Count > 0 Then
    lvFilter.Row := 0;
End;

Procedure TMainForm.acFolderGroupingExecute(Sender: TObject);
Begin
  acFolderGrouping.Checked := Not acFolderGrouping.Checked;
  Ini.WriteBool('Interface', 'FolderGrouping', acFolderGrouping.Checked);
  RpcObj.RefreshNow := RpcObj.RefreshNow + [rtTorrents, rtSession];
End;

Procedure TMainForm.acStartTorrentExecute(Sender: TObject);
Begin
  TorrentAction(GetSelectedTorrents, 'torrent-start');
End;

Procedure TMainForm.acStopTorrentExecute(Sender: TObject);
Begin
  TorrentAction(GetSelectedTorrents, 'torrent-stop');
End;

Procedure TMainForm.acForceStartTorrentExecute(Sender: TObject);
Begin
  TorrentAction(GetSelectedTorrents, 'torrent-start-now');
End;

Procedure TMainForm.acStartTorrentFilterExecute(Sender: TObject);
Begin
  TorrentAction(GetFilteredTorrents, 'torrent-start');
End;

Procedure TMainForm.acStopTorrentFilterExecute(Sender: TObject);
Begin
  TorrentAction(GetFilteredTorrents, 'torrent-stop');
End;

Procedure TMainForm.acForceStartTorrentFilterExecute(Sender: TObject);
Begin
  TorrentAction(GetFilteredTorrents, 'torrent-start-now');
End;

Procedure TMainForm.acHideAppExecute(Sender: TObject);
Begin
  HideApp;
End;

Procedure TMainForm.acInfoPaneExecute(Sender: TObject);
Begin
  acInfoPane.Checked := Not acInfoPane.Checked;
  PageInfo.Visible := acInfoPane.Checked;
  VSplitter.Top := PageInfo.Top - VSplitter.Height;
  VSplitter.Visible := acInfoPane.Checked;
  If VSplitter.Visible Then
    PageInfoChange(nil)
  Else
    RpcObj.AdvInfo := aiNone;
End;

Procedure TMainForm.acLabelGroupingExecute(Sender: TObject);
Begin
  acLabelGrouping.Checked := Not acLabelGrouping.Checked;
  Ini.WriteBool('Interface', 'LabelGrouping', acLabelGrouping.Checked);
  RpcObj.RefreshNow := RpcObj.RefreshNow + [rtTorrents];
End;

Procedure TMainForm.acMenuShowExecute(Sender: TObject);
Begin
  acMenuShow.Checked := Not acMenuShow.Checked;
  If acMenuShow.Checked = False Then
    MainForm.Menu := nil
  Else
    MainForm.Menu := MainMenu;
End;

Procedure TMainForm.acMoveTorrentExecute(Sender: TObject);
Var
  ids: Variant;
  i: Integer;
  s: String;
  req: TJSONObject;
  aids: TJSONArray;
  args: TJSONObject;
  ok: Boolean;
  t: TDateTime;
Begin
  If gTorrents.Items.Count = 0 Then
    exit;
  AppBusy;
  With TMoveTorrentForm.Create(Self) Do
  Try
    gTorrents.Tag := 1;
    gTorrents.EnsureSelectionVisible;
    FillDownloadDirs(edTorrentDir, 'LastMoveDir');
    If gTorrents.SelCount = 0 Then
      gTorrents.RowSelected[gTorrents.Row] := True;
    ids := GetSelectedTorrents;
    i := gTorrents.Items.IndexOf(idxTorrentId, ids[0]);
    If VarIsEmpty(gTorrents.Items[idxPath, i]) Then
      exit;

    edTorrentDir.Text := UTF8Encode(WideString(gTorrents.Items[idxPath, i]));

    If gTorrents.SelCount > 1 Then
      s := Format(sSeveralTorrents, [gTorrents.SelCount])
    Else
      s := UTF8Encode(WideString(gTorrents.Items[idxName, i]));


    Caption := Caption + ' - ' + s;
    AppNormal;
    If ShowModal = mrOk Then
    Begin
      Application.ProcessMessages;
      AppBusy;
      req := TJSONObject.Create;
      Try
        req.Add('method', 'torrent-set-location');
        args := TJSONObject.Create;
        aids := TJSONArray.Create;
        For i := VarArrayLowBound(ids, 1) To VarArrayHighBound(ids, 1) Do
          aids.Add(Integer(ids[i]));
        args.Add('ids', aids);
        args.Add('location',
          TJSONString.Create(UTF8Decode(edTorrentDir.Text)));
        args.Add('move', TJSONIntegerNumber.Create(
          Integer(cbMoveData.Checked) And 1));
        req.Add('arguments', args);
        args := RpcObj.SendRequest(req, False);
      Finally
        args.Free;
        req.Free;
      End;
      gTorrents.Tag := 0;
      AppNormal;
      If args = nil Then
        CheckStatus(False)
      Else
      Begin
        SaveDownloadDirs(edTorrentDir, 'LastMoveDir');
        ok := False;
        t := Now;
        With gTorrents Do
          While Not ok And Not Application.Terminated And
            (Now - t < 20 / SecsPerDay) Do
          Begin
            RpcObj.RequestFullInfo := True;
            DoRefresh(True);
            Sleep(200);
            Application.ProcessMessages;
            ok := True;
            For i := 0 To Items.Count - 1 Do
              If RowSelected[i] Then
              Begin
                If VarIsEmpty(Items[idxPath, i]) Or
                  (AnsiCompareText(UTF8Encode(WideString(Items[idxPath, i])),
                  edTorrentDir.Text) <> 0) Then
                Begin
                  ok := False;
                  break;
                End;
              End;
          End;
      End;
    End;
  Finally
    gTorrents.Tag := 0;
    Free;
  End;
End;

Procedure TMainForm.acNewConnectionExecute(Sender: TObject);
Begin
  ShowConnOptions(True);
End;

Function TMainForm.CorrectPath(path: String): String;
Var
  l_old: Integer;
Begin
  path := StringReplace(path, '//', '/', [rfReplaceAll, rfIgnoreCase]);
  Result := path;
  l_old := length(path);
  If l_old >= 1 Then
  Begin
    If path[l_old] = '/' Then
      path := MidStr(path, 1, l_old - 1);
    Result := path;
  End;
  //  path  := StringReplace(path, '\\', '\', [rfReplaceAll, rfIgnoreCase]);
  Result := path;
  l_old := length(path);
  If l_old >= 1 Then
  Begin
    If path[l_old] = '\' Then
      path := MidStr(path, 1, l_old - 1);
    Result := path;
  End;
End;

Procedure TMainForm.acOpenContainingFolderExecute(Sender: TObject);
Var
  s, s1: String;
Begin
  s := '';
  If gTorrents.Items.Count = 0 Then
    exit;
  Application.ProcessMessages;
  If lvFiles.Focused And (lvFiles.Items.Count > 0) Then
  Begin
    AppBusy;
    If RpcObj.IncompleteDir <> '' Then
    Begin
      s1 := FFilesTree.GetIncompleteFullPath(lvFiles.Row);
      If FileExistsUTF8(MapRemoteToLocal(s1)) Or FileExistsUTF8(
        MapRemoteToLocal(s1 + '.part')) Then
        s := s1;
    End;
    If s = '' Then
    Begin
      s := FFilesTree.GetFullPath(lvFiles.Row);


      //      if (not (FileExistsUTF8(MapRemoteToLocal(s)) or DirectoryExistsUTF8(MapRemoteToLocal(s)) or DirectoryExistsUTF8(ExtractFilePath(MapRemoteToLocal(s)))))  then
    End;


    //    ExecRemoteFile(ExtractFileDir(s), not FFilesTree.IsFolder(lvFiles.Row));
    ExecRemoteFile(s, Not FFilesTree.IsFolder(lvFiles.Row), RpcObj.CurTorrentId
      );
    AppNormal;
  End
  Else
    OpenCurrentTorrent(True);
End;

Procedure TMainForm.acOpenFileExecute(Sender: TObject);
Var
  UserDef: Boolean;
  s: String;
  //    b:boolean;
Begin
  If gTorrents.Items.Count = 0 Then
    exit;
  Application.ProcessMessages;
  If (Sender Is TMenuItem) And (TMenuItem(Sender).Tag > 1999) Then
  Begin
    UserDef := True;
    {$ifdef windows}
      FUserDefinedMenuEx    := Ini.ReadString('UserFileMenu','ExeName'+IntToStr(
                               TMenuItem(Sender).Tag-2000),'');
      FUserDefinedMenuParam := Ini.ReadString('UserFileMenu','Params'+IntToStr(
                               TMenuItem(Sender).Tag-2000),'');
      FUserDefinedMenuParamType := Ini.ReadString('UserFileMenu','ParamType'+
                                   IntToStr(TMenuItem(Sender).Tag-2000),'file');
    {$endif windows}
  End
  Else If (Sender Is TMenuItem) And (TMenuItem(Sender).Tag > 999) Then
    Begin
      UserDef := True;
      {$ifdef windows}
           FUserDefinedMenuEx    := Ini.ReadString('UserTorrentMenu','ExeName'+
                                    IntToStr(TMenuItem(Sender).Tag-1000),'');
           FUserDefinedMenuParam := Ini.ReadString('UserTorrentMenu','Params'+
                                    IntToStr(TMenuItem(Sender).Tag-1000),'');
           FUserDefinedMenuParamType := 'id';
      {$endif windows}
    End
    Else
    Begin
      FUserDefinedMenuEx := '';
      FUserDefinedMenuParam := '';
      FUserDefinedMenuParamType := '';
      UserDef := False;
    End;
  If lvFiles.Focused Then
  Begin
    If lvFiles.Items.Count = 0 Then exit;
    s := FFilesTree.GetFullPath(lvFiles.Row);
    If FUserDefinedMenuParamType = 'id' Then
      ExecRemoteFile(s, Userdef, RpcObj.CurTorrentId, Userdef)
    Else If lvFiles.SelCount > 1 Then
        ExecRemoteFile('', Userdef, RpcObj.CurTorrentId, Userdef)
      Else If FileExistsUTF8(MapRemoteToLocal(s)) Or FileExistsUTF8(
          MapRemoteToLocal(s + '.part')) Or DirectoryExistsUTF8(
          MapRemoteToLocal(s)) Then
          ExecRemoteFile(s, Userdef, RpcObj.CurTorrentId, Userdef)
        Else
        Begin
          s := FFilesTree.GetIncompleteFullPath(lvFiles.Row);
          If FileExistsUTF8(MapRemoteToLocal(s)) Or FileExistsUTF8(
            MapRemoteToLocal(s + '.part')) Then
            ExecRemoteFile(s, Userdef, RpcObj.CurTorrentId, Userdef);
        End;

  End
  Else
    OpenCurrentTorrent(False, UserDef);
End;

Procedure TMainForm.acOptionsExecute(Sender: TObject);
Var
  OldCheckVer: Boolean;
Begin
  AppBusy;
  With TOptionsForm.Create(Self) Do
  Try
    ConnForm.ActiveConnection := FCurConn;
    edRefreshInterval.Value :=
      Ini.ReadInteger('Interface', 'RefreshInterval', 5);
    edRefreshIntervalMin.Value :=
      Ini.ReadInteger('Interface', 'RefreshIntervalMin', 20);
    cbCalcAvg.Checked := FCalcAvg;
    {$ifndef darwin}
    cbTrayMinimize.Checked := Ini.ReadBool('Interface', 'TrayMinimize', True);
    {$else}
      cbTrayMinimize.Enabled := False;
    {$endif}
    cbTrayClose.Checked := Ini.ReadBool('Interface', 'TrayClose', False);
    cbTrayIconAlways.Checked :=
      Ini.ReadBool('Interface', 'TrayIconAlways', True);
    cbTrayNotify.Checked := Ini.ReadBool('Interface', 'TrayNotify', True);

    cbShowAddTorrentWindow.Checked :=
      Ini.ReadBool('Interface', 'ShowAddTorrentWindow', True);
    cbDeleteTorrentFile.Checked :=
      Ini.ReadBool('Interface', 'DeleteTorrentFile', False);
    cbLinksFromClipboard.Checked :=
      Ini.ReadBool('Interface', 'LinksFromClipboard', True);
    edIntfScale.Value := Ini.ReadInteger('Interface', 'Scaling', 100);
    cbCheckNewVersion.Checked :=
      Ini.ReadBool('Interface', 'CheckNewVersion', False);
    edCheckVersionDays.Value :=
      Ini.ReadInteger('Interface', 'CheckNewVersionDays', 5);
    cbCheckNewVersionClick(nil);
    OldCheckVer := cbCheckNewVersion.Checked;
    {$ifdef linux}
      If IsUnity Then
        Begin
          cbTrayIconAlways.Enabled := False;
          cbTrayIconAlways.Checked := False;
          cbTrayMinimize.Enabled := False;
          cbTrayMinimize.Checked := False;
          cbTrayNotify.Enabled := False;
          cbTrayNotify.Checked := False;
        End;
    {$endif linux}
    AppNormal;
    If ShowModal = mrOk Then
    Begin
      AppBusy;
      Ini.WriteInteger('Interface', 'RefreshInterval',
        edRefreshInterval.Value);
      Ini.WriteInteger('Interface', 'RefreshIntervalMin',
        edRefreshIntervalMin.Value);
      Ini.WriteBool('Interface', 'CalcAvg', cbCalcAvg.Checked);
      {$ifndef darwin}
      Ini.WriteBool('Interface', 'TrayMinimize', cbTrayMinimize.Checked);
      {$endif}
      Ini.WriteBool('Interface', 'TrayClose', cbTrayClose.Checked);
      Ini.WriteBool('Interface', 'TrayIconAlways', cbTrayIconAlways.Checked);
      Ini.WriteBool('Interface', 'TrayNotify', cbTrayNotify.Checked);

      Ini.WriteBool('Interface', 'ShowAddTorrentWindow',
        cbShowAddTorrentWindow.Checked);
      Ini.WriteBool('Interface', 'DeleteTorrentFile',
        cbDeleteTorrentFile.Checked);
      Ini.WriteBool('Interface', 'LinksFromClipboard',
        cbLinksFromClipboard.Checked);
      FLinksFromClipboard := cbLinksFromClipboard.Checked;

      Ini.WriteInteger('Interface', 'Scaling', edIntfScale.Value);

      Ini.WriteBool('Interface', 'CheckNewVersion',
        cbCheckNewVersion.Checked);
      Ini.WriteInteger('Interface', 'CheckNewVersionDays',
        edCheckVersionDays.Value);

      If cbCheckNewVersion.Checked And Not OldCheckVer Then
        CheckNewVersion;
      Ini.UpdateFile;
      UpdateTray;
      AppNormal;
      With ConnForm Do
        ConnectionSettingsChanged(ActiveConnection, ActiveSettingChanged);
    End;
  Finally
    Free;
  End;
End;

Procedure TMainForm.acAddTorrentExecute(Sender: TObject);
Begin
  If Not OpenTorrentDlg.Execute Then exit;
  FPendingTorrents.AddStrings(OpenTorrentDlg.Files);
  TickTimerTimer(nil);
End;

Procedure TMainForm.acAddTrackerExecute(Sender: TObject);
Begin
  AddTracker(False);
End;

Procedure TMainForm.acAdvEditTrackersExecute(Sender: TObject);
Begin
  gTorrents.RemoveSelection;
  TorrentProps(1);
End;

Procedure TMainForm.acAltSpeedExecute(Sender: TObject);
Var
  req, args: TJSONObject;
Begin
  AppBusy;
  req := TJSONObject.Create;
  Try
    req.Add('method', 'session-set');
    args := TJSONObject.Create;
    args.Add('alt-speed-enabled', Integer(Not acAltSpeed.Checked) And 1);
    req.Add('arguments', args);
    args := RpcObj.SendRequest(req, False);
    If args = nil Then
    Begin
      CheckStatus(False);
      exit;
    End;
  Finally
    args.Free;
    req.Free;
  End;
  RpcObj.RefreshNow := RpcObj.RefreshNow + [rtSession];
  AppNormal;
End;

Procedure TMainForm.ScaleImageList(ImgList: TImageList; Var ImgListOut: TImageList;
  NewWidth: Integer);
Var
  I, J: Integer;
  TempBmp1: TBitmap;
  TempImgList: TImageList;
  Res: TCustomImageListResolution;
  //  R:TCustomImageListResolutionEnumerator;
Begin
  Try
    If ImgListOut = nil Then
      ImgListOut := TImageList.Create(nil);
    TempImgList := TImageList.Create(nil);
    TempBmp1 := TBitmap.Create;
    TempBmp1.PixelFormat := pf32bit;
    //    R:=ImgList.Resolutions;
    For I := 0 To ImgList.ResolutionCount - 1 Do
    Begin
      Res := ImgList.ResolutionByIndex[I];
      If Res.Width < NewWidth Then
        J := I;
    End;
    Res := ImgList.ResolutionByIndex[J];
    TempImgList.Width := Res.Width;
    TempImgList.Height := Res.Height;
    For I := 0 To ImgList.Count - 1 Do
    Begin
      // Load image for given index to temporary bitmap
      Res.GetBitmap(I, TempBmp1);
      TempImgList.Add(TempBmp1, nil);
    End;
    ImgListOut.Assign(TempImgList);
  Finally
    TempBmp1.Free;
    TempImgList.Free;
  End;
End;

Procedure TMainForm.acBigToolbarExecute(Sender: TObject);
Var
  i: Integer;
Begin
  acBigToolbar.Checked := Not acBigToolbar.Checked;
  If acBigToolbar.Checked Then
    MainToolBar.ButtonWidth := Ini.ReadInteger('MainForm', 'BigToolBarHeight', 64)
  Else
    MainToolBar.ButtonWidth := 23;
  ScaleImageList(ImageList16, CurImageList, MainToolBar.ButtonWidth);
  MainToolBar.ButtonHeight := MainToolBar.ButtonWidth;
  edSearch.Height := MainToolBar.ButtonWidth;
  panSearch.Height := MainToolBar.ButtonWidth;
  searchToolbar.Width := MainToolBar.ButtonWidth;
  imgSearch.Width := MainToolBar.ButtonWidth;
  imgSearch.Height := MainToolBar.ButtonWidth;
  SearchToolBar.ButtonWidth := MainToolBar.ButtonWidth;
  SearchToolBar.ButtonHeight := MainToolBar.ButtonWidth;
  SearchToolBar1.Width := MainToolBar.ButtonWidth;
  SearchToolBar1.ButtonWidth := MainToolBar.ButtonWidth;
  SearchToolBar1.ButtonHeight := MainToolBar.ButtonWidth;
  //gTorrents.SetDefRowHeight(MainToolBar.ButtonWidth);
  //gTorrents.DefaultRowHeight:=MainToolBar.ButtonWidth+2;


  SearchToolBar.Images := CurImageList;
  SearchToolBar1.Images := CurImageList;
  MainToolBar.Images := CurImageList;
  lvFilter.Images := CurImageList;
  lvFiles.Images := ImageList16;
  gTorrents.Images := CurImageList;
  PageInfo.Images := CurImageList;
  MainMenu.Images := CurImageList;
  pmTorrents.Images := CurImageList;
  pmTrackers.Images := CurImageList;
  pmFiles.Images := CurImageList;
  pmTray.Images := CurImageList;
  gTorrents.VisualChangeNew;
  lvFilter.VisualChangeNew;
  For i := 1 To gTorrents.RowCount - 1 Do
    gTorrents.RowHeights[i] := gTorrents.DefaultRowHeight;

  For i := 0 To lvFilter.RowCount - 1 Do
    lvFilter.RowHeights[i] := lvFilter.DefaultRowHeight;

  Ini.WriteBool('MainForm', 'BigToolbar', acBigToolbar.Checked);
End;

Procedure TMainForm.acFilterTorrentPropsExecute(Sender: TObject);
Begin
  TorrentProps(0, True);
End;



Procedure TMainForm.acCheckNewVersionExecute(Sender: TObject);
Begin
  Application.ProcessMessages;
  AppBusy;
  CheckNewVersion(False);
  AppNormal;
End;

Procedure TMainForm.acAddLinkExecute(Sender: TObject);
Begin
  AppBusy;
  With TAddLinkForm.Create(Self) Do
  Try
    AppNormal;
    If ShowModal = mrOk Then
    Begin
      If isHash(edLink.Text) Then
        edLink.Text := 'magnet:?xt=urn:btih:' + edLink.Text;
      DoAddTorrent(edLink.Text);
    End;
  Finally
    Free;
  End;
End;

Function TMainForm.DoAddTorrent(Const FileName: Utf8string): Boolean;
Var
  torrent: String;
  WaitForm: TBaseForm;
  IsAppHidden: Boolean;

  Procedure _AddTrackers(TorrentId: Integer);
  Var
    req, args: TJSONObject;
    fs: TFileStreamUTF8;
    TorData, AnnData, LData, LLData: TBEncoded;
    t: TJSONArray;
    tt: TJSONObject;
    trackers: TJSONArray;
    i, j: Integer;
    s, tfn, TorrentHash: String;
    TrackersList: TStringList;
  Begin
    RpcObj.Status := '';
    s := '';
    If TorrentId <> 0 Then
    Begin
      i := SelectTorrent(TorrentId, 2000);
      If i >= 0 Then
        s := Format(': %s', [UTF8Encode(
          WideString(gTorrents.Items[idxName, i]))]);

    End;
    ForceAppNormal;
    s := TranslateString(SDuplicateTorrent + s + '.', True);
    If RpcObj.RPCVersion < 10 Then
    Begin
      MessageDlg(s, mtError, [mbOK], 0);
      exit;
    End;
    If MessageDlg(s + LineEnding + LineEnding + SUpdateTrackers,
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes Then
      exit;
    Application.ProcessMessages;
    TrackersList := TStringList.Create;
    Try
      TorrentHash := '';
      If AnsiCompareText('magnet:?', Copy(FileName, 1, 8)) = 0 Then
      Begin
        // Get trackers from the magnet link
        TrackersList.Delimiter := '&';
        TrackersList.DelimitedText := Copy(FileName, 9, MaxInt);
        i := 0;
        While i < TrackersList.Count Do
        Begin
          s := TrackersList.Names[i];
          If (TorrentId = 0) And (CompareText(s, 'xt') = 0) Then
          Begin
            s := LowerCase(TrackersList.ValueFromIndex[i]);
            If (TorrentHash = '') And (Pos('urn:btih:', s) = 1) Then
            Begin
              s := Copy(s, 10, MaxInt);
              If Length(s) = 32 Then
                // base32 encoded hash
                TorrentHash := StrToHex(Base32Decode(UpperCase(s)))
              Else
                TorrentHash := s;
            End;
          End
          Else
            If CompareText(s, 'tr') = 0 Then
            Begin
              TrackersList[i] := DecodeURL(TrackersList.ValueFromIndex[i]);
              Inc(i);
              continue;
            End;
          TrackersList.Delete(i);
        End;
      End
      Else
      Try
        If IsProtocolSupported(FileName) Then
        Begin
          // Downloading torrent file
          tfn := SysToUTF8(GetTempDir(True)) + 'remote-torrent.torrent';
          If Not DownloadFile(FileName, ExtractFilePath(tfn),
            ExtractFileName(tfn), SDownloadingTorrent) Then
            exit;
        End
        Else
          tfn := FileName;
        // Read trackers from the torrent file...
        AppBusy;
        TorData := nil;
        fs := TFileStreamUTF8.Create(tfn, fmOpenRead Or fmShareDenyNone);
        Try
          TorData := TBEncoded.Create(fs);
          If TorrentId = 0 Then
          Begin
            // Calculate torrent hash
            LData := (TorData.ListData.FindElement('info') As TBEncoded);
            s := '';
            LData.Encode(LData, s);
            TorrentHash := StrToHex(SHA1(s));
          End;
          AnnData := (TorData.ListData.FindElement('announce-list', False) As
            TBEncoded);
          If AnnData <> nil Then
            For i := 0 To AnnData.ListData.Count - 1 Do
            Begin
              LData := AnnData.ListData.Items[i].Data As TBEncoded;
              For j := 0 To LData.ListData.Count - 1 Do
              Begin
                LLData := LData.ListData.Items[j].Data As TBEncoded;
                TrackersList.Add(LLData.StringData);
              End;
            End
          Else
          Begin
            AnnData := (TorData.ListData.FindElement('announce', False) As
              TBEncoded);
            If AnnData <> nil Then
              TrackersList.Add(AnnData.StringData);
          End;
        Finally
          TorData.Free;
          fs.Free;
        End;
      Finally
        // Delete temp file
        If (tfn <> '') And (tfn <> FileName) Then
          DeleteFileUTF8(tfn);
      End;

      // Request trackers from the existing torrent
      req := TJSONObject.Create;
      Try
        req.Add('method', 'torrent-get');
        args := TJSONObject.Create;
        If TorrentId = 0 Then
          args.Add('ids', TJSONArray.Create([TorrentHash]))
        Else
          args.Add('ids', TJSONArray.Create([TorrentId]));
        args.Add('fields', TJSONArray.Create(['id', 'trackers']));
        req.Add('arguments', args);
        args := RpcObj.SendRequest(req);
        If args = nil Then
        Begin
          CheckStatus(False);
          exit;
        End;
        Try
          t := args.Arrays['torrents'];
          If t.Count = 0 Then
            Raise Exception.Create('Torrent not found.');
          tt := t.Objects[0] As TJSONObject;
          i := tt.Integers['id'];
          If TorrentId = 0 Then
            SelectTorrent(i, 2000);
          TorrentId := i;
          trackers := tt.Arrays['trackers'];
          // Deleting existing trackers from the list
          For i := 0 To trackers.Count - 1 Do
          Begin
            s := UTF8Encode((Trackers.Items[i] As TJSONObject).Strings['announce']);
            j := TrackersList.IndexOf(s);
            If j >= 0 Then
              TrackersList.Delete(j);
          End;
        Finally
        End;
      Finally
        args.Free;
        req.Free;
      End;

      If TrackersList.Count > 0 Then
      Begin
        trackers := TJSONArray.Create;
        For i := 0 To TrackersList.Count - 1 Do
          trackers.Add(TrackersList[i]);
        args := TJSONObject.Create;
        args.Add('ids', TJSONArray.Create([TorrentId]));
        args.Add('trackerAdd', trackers);
        req := TJSONObject.Create;
        Try
          req.Add('method', 'torrent-set');
          req.Add('arguments', args);
          args := RpcObj.SendRequest(req, False);
          If args = nil Then
          Begin
            CheckStatus(False);
            exit;
          End;
        Finally
          args.Free;
          req.Free;
        End;
        DoRefresh;
      End;
    Finally
      TrackersList.Free;
      AppNormal;
    End;
  End;

  Function _AddTorrent(args: TJSONObject): Integer;
  Var
    req: TJSONObject;
  Begin
    Result := 0;
    req := TJSONObject.Create;
    Try
      req.Add('method', 'torrent-add');
      If torrent = '-' Then
        args.Add('filename', TJSONString.Create(FileName))
      Else
        args.Add('metainfo', TJSONString.Create(torrent));
      req.Add('arguments', args);
      args := RpcObj.SendRequest(req);
      If args <> nil Then
      Try
        If args.IndexOfName('torrent-duplicate') >= 0 Then
        Begin
          _AddTrackers(args.Objects['torrent-duplicate'].Integers['id']);
          exit;
        End;
        Result := args.Objects['torrent-added'].Integers['id'];
      Finally
      End
      Else
        If RpcObj.Status = 'duplicate torrent' Then
        Begin
          _AddTrackers(0);
          exit;
        End;
    Finally
      args.Free;
      req.Free;
    End;
    If Result = 0 Then
      CheckStatus(False);
  End;

  Procedure ShowWaitMsg(Const AText: String);
  Begin
    If WaitForm = nil Then
    Begin
      WaitForm := TBaseForm.CreateNew(Self);
      With WaitForm Do
      Begin
        {$ifndef windows}
        If IsAppHidden Then
          ShowInTaskBar := stAlways;
        {$endif windows}
        Caption := AppName;
        BorderStyle := bsToolWindow;
        BorderIcons := [];
        Position := poScreenCenter;
        Constraints.MinWidth := 400;
        AutoSize := True;
        BorderWidth := ScaleInt(16);
        With TLabel.Create(WaitForm) Do
        Begin
          Alignment := taCenter;
          Align := alClient;
          Parent := WaitForm;
        End;
      End;
    End;
    With WaitForm Do
    Begin
      TLabel(Controls[0]).Caption := AText + '...';
      Show;
      BringToFront;
      {$ifdef lclgtk2}
      Application.ProcessMessages;
      {$endif lclgtk2}
      Update;
      {$ifdef lclgtk2}
      sleep(100);
      Application.ProcessMessages;
      {$endif lclgtk2}
    End;
  End;

  Procedure HideWaitMsg;
  Begin
    If WaitForm <> nil Then
      FreeAndNil(WaitForm);
  End;

Var
  req, args: TJSONObject;
  id: Integer;
  t, files: TJSONArray;
  i: Integer;
  fs: TFileStreamUTF8;
  s, ss, OldDownloadDir, IniSec, OldName: String;
  ok: Boolean;
  pFD: FolderData;
  alabels: TJSONArray;
  slabels: TStringList;
Begin
  Result := False;
  If Not RpcObj.Connected And Not RpcObj.Connecting Then
    If Not DoConnect Then
      exit;
  WaitForm := nil;
  id := 0;
  Inc(FAddingTorrent);
  Try
    AppBusy;
    Try
      IsAppHidden := Not Self.Visible Or (Self.WindowState = wsMinimized);
      With TAddTorrentForm.Create(Self) Do
      Try
        If IsAppHidden Then
        Begin
          ShowWaitMsg(Caption);
          {$ifndef windows}
          ShowInTaskBar := stAlways;
          {$endif windows}
        End;

        Application.ProcessMessages;
        If IsProtocolSupported(FileName) Then
          torrent := '-'
        Else
        Begin
          Try
            fs := TFileStreamUTF8.Create(FileName, fmOpenRead Or
              fmShareDenyNone);
            // why isnt in try
          Except
            AppNormal;


            // if the clipboard garbage and file cant be created. just go out.
            HideWaitMsg;
            exit;
          End;

          Try
            SetLength(torrent, fs.Size);
            fs.ReadBuffer(PChar(torrent)^, Length(torrent));
          Finally
            fs.Free;
          End;
          torrent := EncodeBase64(torrent);
        End;

        IniSec := 'AddTorrent.' + FCurConn;
        FillDownloadDirs(cbDestFolder, 'LastDownloadDir');
        edLabel.Text := Ini.ReadString(IniSec, 'Label', '');
        If (FWatchDownloading) And (FWatchDestinationFolder <> '') Then cbDestFolder.
            Text := FWatchDestinationFolder;

        req := TJSONObject.Create;
        Try
          req.Add('method', 'session-get');
          args := RpcObj.SendRequest(req);
          If args = nil Then
          Begin
            CheckStatus(False);
            exit;
          End;
          s := CorrectPath(UTF8Encode(args.Strings['download-dir']));
          Try
            If cbDestFolder.Items.IndexOf(s) < 0 Then
            Begin
              pFD := FolderData.Create;
              pFD.Hit := 1;
              pFD.Ext := '';
              pFD.Txt := s;
              pFD.Lst := SysUtils.Date;
              cbDestFolder.Items.Add(s);
              i := cbDestFolder.Items.IndexOf(s);
              cbDestFolder.Items.Objects[i] := pFD;
            End;
          Except
            MessageDlg('Error: LS-005. Please contact the developer',
              mtError, [mbOK], 0
              );
          End;

          If RpcObj.RPCVersion < 15 Then
            If args.IndexOfName('download-dir-free-space') >= 0 Then
              txDiskSpace.Caption :=
                txDiskSpace.Caption + ' ' + GetHumanSize(
                args.Floats['download-dir-free-space'])
            Else
            Begin
              txDiskSpace.Hide;
              txSize.Top := (txSize.Top + txDiskSpace.Top) Div 2;
            End;
        Finally
          args.Free;
          req.Free;
        End;

        lvFilter.Row := 0;
        edSearch.Text := '';

        args := TJSONObject.Create;
        args.Add('paused', TJSONIntegerNumber.Create(1));
        i := Ini.ReadInteger(IniSec, 'PeerLimit', 0);
        If i <> 0 Then
          args.Add('peer-limit', TJSONIntegerNumber.Create(i));

        // for larazur 1.4 and up.
        // data can be in the drop-down list, but no text is selected in the window
        Try
          ss := cbDestFolder.Text;
          If (ss = '') Then
          Begin
            If (cbDestFolder.Items.Count = 0) Then
            Begin
              ss := s;
            End
            Else
            Begin
              cbDestFolder.ItemIndex := 0;
              ss := cbDestFolder.Text;
            End;
          End;
        Except
          MessageDlg('Error: LS-006. Please contact the developer',
            mtError, [mbOK], 0);
        End;
        args.Add('download-dir', TJSONString.Create(UTF8Decode(ss)));
        id := _AddTorrent(args);
        If id = 0 Then
          exit;

        DoRefresh(True);

        args := RpcObj.RequestInfo(id, ['files', 'maxConnectedPeers',
          'name', 'metadataPercentComplete']);
        If args = nil Then
        Begin
          CheckStatus(False);
          exit;
        End;
        Try
          t := args.Arrays['torrents'];
          If t.Count = 0 Then
            Raise Exception.Create(sUnableGetFilesList);

          OldName := UTF8Encode(t.Objects[0].Strings['name']);
          edSaveAs.Caption := OldName;
          edSaveAs.Caption := ExcludeInvalidChar(edSaveAs.Caption);
          // petrov - Exclude prohibited characters

          If RpcObj.RPCVersion < 15 Then
          Begin
            edSaveAs.Enabled := False;
            edSaveAs.ParentColor := True;
          End;
          edPeerLimit.Value := t.Objects[0].Integers['maxConnectedPeers'];
          FilesTree.FillTree(id, t.Objects[0].Arrays['files'], nil, nil, RebuildTree);
          Width := Ini.ReadInteger('AddTorrent', 'Width', Width);
          If (RpcObj.RPCVersion >= 7) And (lvFiles.Items.Count = 0) And
            (t.Objects[0].Floats['metadataPercentComplete'] <> 1.0) Then
          Begin
            // Magnet link
            gbContents.Hide;
            gbSaveAs.BorderSpacing.Bottom := gbSaveAs.BorderSpacing.Top;
            BorderStyle := bsDialog;
            AutoSizeForm(TCustomForm(gbContents.Parent));
            edSaveAs.Enabled := False;
            edSaveAs.ParentColor := True;
          End
          Else
            // Torrent file
            Height := Ini.ReadInteger('AddTorrent', 'Height', Height);
        Finally
          args.Free;
        End;
        OldDownloadDir := cbDestFolder.Text;
        AppNormal;

        ok := Not Ini.ReadBool('Interface', 'ShowAddTorrentWindow', True);
        If FWatchDownloading Then ok := True;
        If ok Then
          btSelectAllClick(nil)
        Else
        Begin
          HideWaitMsg;
          ok := ShowModal = mrOk;
          If BorderStyle = bsSizeable Then
          Begin
            Ini.WriteInteger('AddTorrent', 'Width', Width);
            Ini.WriteInteger('AddTorrent', 'Height', Height);
          End;
        End;

        If ok Then
        Begin
          If IsAppHidden Then
            ShowWaitMsg(Caption);
          AppBusy;
          Self.Update;

          If OldDownloadDir <> cbDestFolder.Text Then
          Begin
            TorrentAction(id, 'torrent-remove');
            id := 0;
            args := TJSONObject.Create;
            args.Add('paused', TJSONIntegerNumber.Create(1));
            args.Add('peer-limit', TJSONIntegerNumber.Create(edPeerLimit.Value));
            args.Add('download-dir',
              TJSONString.Create(UTF8Decode(cbDestFolder.Text)));
            id := _AddTorrent(args);
            If id = 0 Then
              exit;
            DoRefresh(True);
            Application.ProcessMessages;
          End;

          req := TJSONObject.Create;
          Try
            req.Add('method', 'torrent-set');
            args := TJSONObject.Create;
            args.Add('ids', TJSONArray.Create([id]));
            args.Add('peer-limit', TJSONIntegerNumber.Create(edPeerLimit.Value));

            files := TJSONArray.Create;
            For i := 0 To lvFiles.Items.Count - 1 Do
              If Not FilesTree.IsFolder(i) And (FilesTree.Checked[i] = cbChecked) Then
                files.Add(Integer(lvFiles.Items[idxFileId, i]));
            If files.Count > 0 Then
              args.Add('files-wanted', files)
            Else
              files.Free;

            files := TJSONArray.Create;
            For i := 0 To lvFiles.Items.Count - 1 Do
              If Not FilesTree.IsFolder(i) And (FilesTree.Checked[i] <> cbChecked)
              Then
                files.Add(Integer(lvFiles.Items[idxFileId, i]));
            If files.Count > 0 Then
              args.Add('files-unwanted', files)
            Else
              files.Free;
            If (RpcObj.RPCVersion >= 18) And cbStartTorrent.Checked Then
            Begin
              args.Add('sequentialDownload', 1);
              args.Add('sequential_download', 1);
            End;
            If edLabel.Text <> '' Then
            Begin
              alabels := TJSONArray.Create;
              slabels := TStringList.Create;
              SplitRegExpr(',', edLabel.Text, slabels);
              slabels.Sort;
              For s In slabels Do
              Begin
                If trim(s) <> '' Then
                  alabels.Add(trim(s));
              End;
              If alabels.Count > 0 Then
                args.Add('labels', alabels)
              Else
                alabels.Free;
              slabels.Free;
            End;
            req.Add('arguments', args);
            Try
              args := RpcObj.SendRequest(req, False);
              If args = nil Then
              Begin
                CheckStatus(False);
                exit;
              End;
            Finally
              args.Free;
            End;



            //          edSaveAs.Text := Trim(edSaveAs.Text);               // leave spaces to not rename the torrent (see below)
            edSaveAs.Text := ExcludeInvalidChar(edSaveAs.Text);
            // Exclude prohibited characters

            If OldName <> edSaveAs.Text Then
            Begin
              // Changing torrent name
              req.Free;
              req := TJSONObject.Create;
              req.Add('method', 'torrent-rename-path');
              args := TJSONObject.Create;
              args.Add('ids', TJSONArray.Create([id]));
              args.Add('path', UTF8Decode(OldName));
              args.Add('name', UTF8Decode(edSaveAs.Text));
              req.Add('arguments', args);
              args := RpcObj.SendRequest(req, False);
              Try
                If args = nil Then
                Begin
                  // CheckStatus(False); // failed to rename torrent
                  exit;
                  // we continue work (try)
                End;
              Finally
                args.Free;
              End;
            End;
          Finally
            req.Free;
          End;

          If cbStartTorrent.Checked Then
            TorrentAction(id, 'torrent-start');

          SelectTorrent(id, 2000);

          id := 0;
          If (Ini.ReadBool('Interface', 'DeleteTorrentFile', False) And
            Not IsProtocolSupported(FileName)) Or (FWatchDownloading) Then
            DeleteFileUTF8(FileName);

          Ini.WriteInteger(IniSec, 'PeerLimit', edPeerLimit.Value);
          SaveDownloadDirs(cbDestFolder, 'LastDownloadDir');
          Ini.WriteString(IniSec, 'Label', edLabel.Text);

          Result := True;
          AppNormal;
        End;
      Finally
        Free;
      End;
    Finally
      If id <> 0 Then
        TorrentAction(id, 'torrent-remove');
    End;
  Finally
    HideWaitMsg;
    Dec(FAddingTorrent);
  End;
End;

Procedure TMainForm.UpdateTray;
Begin
  {$ifndef CPUARM}
  TrayIcon.Visible := Not IsUnity And
    (Ini.ReadBool('Interface', 'TrayIconAlways', True) Or
    ((WindowState = wsMinimized) And Ini.ReadBool('Interface',
    'TrayMinimize', True)) Or (Not Self.Visible And
    Ini.ReadBool('Interface', 'TrayClose', False)));
  {$endif CPUARM}

  {$ifdef darwin}
  acShowApp.Visible := False;
  acHideApp.Visible := False;
  miTSep1.Visible := False;
  {$else}
  acHideApp.Visible := Visible And (WindowState <> wsMinimized);
  {$endif darwin}
  SetRefreshInterval;
  FCalcAvg := Ini.ReadBool('Interface', 'CalcAvg', True);
End;

Procedure TMainForm.HideApp;
Begin
  If WindowState <> wsMinimized Then
    Hide;
  HideTaskbarButton;
  UpdateTray;
End;

Procedure TMainForm.ShowApp;
Var
  i: Integer;
Begin
  ShowTaskbarButton;
  If WindowState = wsMinimized Then
    Application.Restore;
  Application.ProcessMessages;
  Show;
  Application.BringToFront;
  BringToFront;
  For i := 0 To Screen.FormCount - 1 Do
    With Screen.Forms[i] Do
      If fsModal In FormState Then
        BringToFront;
  UpdateTray;
End;

Procedure TMainForm.DownloadFinished(Const TorrentName: String);
Begin
  {$ifndef CPUARM}
  If Not TrayIcon.Visible Or Not Ini.ReadBool('Interface', 'TrayNotify', True) Then
    exit;
  TrayIcon.BalloonHint := Format(sFinishedDownload, [TorrentName]);
  TrayIcon.BalloonTitle := sDownloadComplete;
  TrayIcon.ShowBalloonHint;
  {$endif CPUARM}
End;

Procedure TMainForm.DoOpenFlagsZip(Sender: TObject; Var AStream: TStream);
Begin
  AStream := TFileStreamUTF8.Create(TUnZipper(Sender).FileName,
    fmOpenRead Or fmShareDenyWrite);
End;

Procedure TMainForm.DoCreateOutZipStream(Sender: TObject; Var AStream: TStream;
  AItem: TFullZipFileEntry);
Begin
  ForceDirectoriesUTF8(FFlagsPath);
  AStream := TFileStreamUTF8.Create(FFlagsPath + AItem.DiskFileName, fmCreate);
End;

Function TMainForm.GetFlagImage(Const CountryCode: String): Integer;
Var
  s, ImageName: String;
  pic: TPicture;
  fs: TFileStreamUTF8;
Begin
  Result := 0;
  If CountryCode = '' Then exit;
  Try
    ImageName := CountryCode + '.png';
    If FFlagsPath = '' Then
      FFlagsPath := FHomeDir + 'flags' + DirectorySeparator;
    If Not FileExistsUTF8(FFlagsPath + ImageName) Then
    Begin
      // Unzipping flag image
      If FUnZip = nil Then
      Begin
        s := GetFlagsArchive;
        If s <> '' Then
        Begin
          FUnZip := TUnZipper.Create;
          FUnZip.FileName := s;
          FUnZip.OnOpenInputStream := @DoOpenFlagsZip;
          FUnZip.OnCreateStream := @DoCreateOutZipStream;
        End
        Else
          exit;
      End;

      FUnZip.Files.Clear;
      FUnZip.Files.Add(ImageName);
      Try
        FUnZip.UnZipAllFiles;
      Except
        FreeAndNil(FUnZip);
        DeleteFileUTF8(GetFlagsArchive);
        acShowCountryFlag.Checked := False;
        MessageDlg(sUnableExtractFlag + LineEnding +
          Exception(ExceptObject).Message, mtError, [mbOK], 0);
        exit;
      End;
      If Not FileExistsUTF8(FFlagsPath + ImageName) Then exit;
    End;

    fs := nil;
    pic := TPicture.Create;
    Try
      fs := TFileStreamUTF8.Create(FFlagsPath + ImageName, fmOpenRead Or
        fmShareDenyWrite);
      pic.LoadFromStream(fs);
      If imgFlags.Count = 1 Then
      Begin
        imgFlags.Width := pic.Width;
        imgFlags.Height := pic.Height;
      End;
      Result := imgFlags.AddMasked(pic.Bitmap, clNone);
    Finally
      pic.Free;
      fs.Free;
    End;
  Except
  End;
End;

Procedure TMainForm.BeforeCloseApp;
Begin
  If WindowState = wsNormal Then
  Begin
    Ini.WriteInteger('MainForm', 'Left', Left);
    Ini.WriteInteger('MainForm', 'Top', Top);
    Ini.WriteInteger('MainForm', 'Width', Width);
    Ini.WriteInteger('MainForm', 'Height', Height);
  End;
  If WindowState <> wsMinimized Then
    Ini.WriteInteger('MainForm', 'State', Integer(WindowState));

  If VSplitter.Visible Then
    Ini.WriteInteger('MainForm', 'VSplitter', VSplitter.GetSplitterPosition);
  If HSplitter.Visible Then
    Ini.WriteInteger('MainForm', 'HSplitter', HSplitter.GetSplitterPosition);

  Ini.WriteBool('MainForm', 'FilterPane', acFilterPane.Checked);
  Ini.WriteBool('MainForm', 'InfoPane', acInfoPane.Checked);
  Ini.WriteBool('MainForm', 'StatusBar', acStatusBar.Checked);

  Ini.WriteBool('MainForm', 'Menu', acMenuShow.Checked);
  Ini.WriteBool('MainForm', 'Toolbar', acToolbarShow.Checked);


  SaveColumns(gTorrents, 'TorrentsList');
  SaveColumns(lvFiles, 'FilesList');
  SaveColumns(lvPeers, 'PeerList');
  SaveColumns(lvTrackers, 'TrackersList');

  Ini.WriteBool('PeersList', 'ResolveHost', acResolveHost.Checked);
  Ini.WriteBool('PeersList', 'ResolveCountry', acResolveCountry.Checked);
  Ini.WriteBool('PeersList', 'ShowCountryFlag', acShowCountryFlag.Checked);

  If RpcObj.Connected Then
    Ini.WriteInteger('Interface', 'LastRpcVersion', RpcObj.RPCVersion);

  Try
    Ini.UpdateFile;
  Except
    Application.HandleException(nil);
  End;

  DoDisconnect;
  Application.ProcessMessages;
End;

Function TMainForm.GetGeoIpDatabase: String;
Begin
  Result := LocateFile('GeoIP.dat', [FHomeDir, ExtractFilePath(ParamStrUTF8(0))]);
End;

Function TMainForm.GetFlagsArchive: String;
Begin
  Result := LocateFile('flags.zip', [FHomeDir, ExtractFilePath(ParamStrUTF8(0))]);
End;

Function TMainForm.DownloadGeoIpDatabase(AUpdate: Boolean): Boolean;
Const
  GeoLiteURL = 'https://dl.miyuru.lk/geoip/maxmind/country/maxmind4.dat.gz';
Var
  tmp: String;
  gz: TGZFileStream;
  fs: TFileStreamUTF8;
  buf: Array[0..65535] Of Byte;
  i: Integer;
Begin
  Result := False;
  tmp := SysToUTF8(GetTempDir(True)) + 'GeoIP.dat.gz';
  If Not FileExistsUTF8(tmp) Or AUpdate Then
  Begin
    If MessageDlg('', sGeoIPConfirm, mtConfirmation, mbYesNo, 0, mbYes) <>
      mrYes Then
      exit;
    If Not DownloadFile(GeoLiteURL, ExtractFilePath(tmp), ExtractFileName(tmp)) Then
      exit;
  End;
  Try
    FreeAndNil(FResolver);
    gz := TGZFileStream.Create(tmp, gzopenread);
    Try
      fs := TFileStreamUTF8.Create(FHomeDir + 'GeoIP.dat', fmCreate);
      Try
        Repeat
          i := gz.Read(buf, SizeOf(buf));
          fs.WriteBuffer(buf, i);
        Until i < SizeOf(buf);
      Finally
        fs.Free;
      End;
    Finally
      gz.Free;
    End;
    DeleteFileUTF8(tmp);
  Except
    DeleteFileUTF8(FHomeDir + 'GeoIP.dat');
    DeleteFileUTF8(tmp);
    Raise;
  End;
  Result := True;
End;

Procedure TMainForm.TorrentColumnsChanged;
Var
  i: Integer;
  s: String;
Begin
  s := '';
  For i := 0 To gTorrents.Columns.Count - 1 Do
    With gTorrents.Columns[i] Do
      If Visible And (Width > 0) Then
      Begin
        If TorrentFieldsMap[ID - 1] <> '' Then
        Begin
          If s <> '' Then
            s := s + ',';
          s := s + TorrentFieldsMap[ID - 1];
        End;
      End;
  RpcObj.TorrentFields := s;
  DoRefresh(True);
End;

Function TMainForm.EtaToString(ETA: Integer): String;
Const
  r1 = 60;
  r2 = 5 * 60;
  r3 = 30 * 60;
  r4 = 60 * 60;
Begin
  If (ETA < 0) Or (ETA = MaxInt) Then
    Result := ''
  Else
  Begin
    If ETA > 2 * 60 * 60 Then  // > 5 hours - round to 1 hour
      ETA := (ETA + r4 Div 2) Div r4 * r4
    Else
      If ETA > 2 * 60 * 60 Then  // > 2 hours - round to 30 mins
        ETA := (ETA + r3 Div 2) Div r3 * r3
      Else
        If ETA > 30 * 60 Then  // > 30 mins - round to 5 mins
          ETA := (ETA + r2 Div 2) Div r2 * r2
        Else
          If ETA > 2 * 60 Then   // > 2 mins - round to 1 min
            ETA := (ETA + r1 Div 2) Div r1 * r1;
    Result := SecondsToString(ETA);
  End;
End;

Function TMainForm.GetTorrentStatus(TorrentIdx: Integer): String;
Var
  i: Integer;
Begin
  i := gTorrents.Items[idxStatus, TorrentIdx];
  If i = TR_STATUS_CHECK_WAIT Then
    Result := sWaiting
  Else
    If i = TR_STATUS_CHECK Then
      Result := sVerifying
    Else
      If i = TR_STATUS_DOWNLOAD_WAIT Then
        Result := sWaiting
      Else
        If i = TR_STATUS_DOWNLOAD Then
          Result := sDownloading
        Else
          If i = TR_STATUS_SEED_WAIT Then
            Result := sWaiting
          Else
            If i = TR_STATUS_SEED Then
              Result := sSeeding
            Else
              If i = TR_STATUS_STOPPED Then
                Result := sStopped
              Else
                If i = TR_STATUS_FINISHED Then
                  Result := sFinished
                Else
                  Result := sUnknown;
End;

Function TMainForm.GetSeedsText(Seeds, SeedsTotal: Integer): String;
Begin
  If SeedsTotal <> -1 Then
    Result := Format('%d/%d', [Seeds, SeedsTotal])
  Else
    Result := Format('%d', [Seeds]);
End;

Function TMainForm.GetPeersText(Peers, PeersTotal, Leechers: Integer): String;
Begin
  Result := Format('%d', [Peers]);
  If Leechers <> -1 Then
    Result := Format('%s/%d', [Result, Leechers]);
  Dec(PeersTotal);
  If PeersTotal >= 0 Then
    Result := Format('%s (%d)', [Result, PeersTotal]);
End;

Function TMainForm.RatioToString(Ratio: Double): String;
Begin
  If (Ratio = MaxInt) Or (Ratio = -2) Then
    Result := Utf8Encode(WideString(Widechar($221E)))
  Else
    If Ratio = -1 Then
      Result := ''
    Else
      Result := Format('%.3f', [Ratio]);
End;

Function HumanReadableTime(ANow, AThen: TDateTime): String;
Var
  Years, Months, Days, Hours, Minutes, Seconds, Discard: Word;
Begin
  Try
    PeriodBetween(ANow, AThen, Years, Months, Days);
    If Sign(Anow - Athen) * CompareTime(Anow, Athen) < 0 Then Dec(Days);
    DecodeDateTime(Sign(Anow - Athen) * (Anow - AThen), discard,
      Discard, Discard, Hours,
      Minutes, Seconds, Discard);
    If Years > 0 Then
    Begin
      Result := Format(sYears, [Years]) + ' ' + Format(sMonths, [Months]);
    End
    Else If Months > 0 Then
      Begin
        Result := Format(sMonths, [Months]) + ' ' + Format(sDays, [Days]);
      End
      Else If Days > 0 Then
        Begin
          Result := Format(sDays, [Days]) + ' ' + Format(sHours, [Hours]);
        End
        Else If Hours > 0 Then
          Begin
            Result := Format(sHours, [Hours]) + ' ' + Format(sMins, [Minutes]);
          End
          Else If Minutes > 0 Then
            Begin
              Result := Format(sMins, [Minutes]) + ' ' + Format(sSecs, [Seconds]);
            End
            Else
            Begin
              Result := Format(sSecs, [Seconds]);
            End;
  Except
    Result := 'An Eternity';
  End;
End;

Function TMainForm.TorrentDateTimeToString(d: Int64; FromNow: Boolean): String;
Var
  s: String;
Begin
  If d = 0 Then
    Result := ''
  Else
  Begin
    If FromNow Then
      s := HumanReadableTime(Now, UnixToDateTime(d) + GetTimeZoneDelta)
    Else
      s := DateTimeToStr(UnixToDateTime(d) + GetTimeZoneDelta, True);
    Result := s;
  End;
End;

Procedure TMainForm.DoRefresh(All: Boolean);
Begin
  If All Then
    RpcObj.RefreshNow := RpcObj.RefreshNow + [rtTorrents, rtDetails]
  Else
    RpcObj.RefreshNow := RpcObj.RefreshNow + [rtDetails];
End;

Procedure TMainForm.acDisconnectExecute(Sender: TObject);
Begin
  DoDisconnect;
End;

Procedure TMainForm.acExportExecute(Sender: TObject);
// PETROV
Var
  s, d: String;
  FileVar1: TextFile;
  FileVar2: TextFile;
Begin
  SaveDialog1.filename := 'transgui.ini';
  If SaveDialog1.Execute Then
  Begin
    s := SaveDialog1.filename;
    d := Ini.getFileName();

    AssignFile(FileVar1, d);
    AssignFile(FileVar2, s);

    Reset(FileVar1);
    Rewrite(FileVar2);

    {$I+}
    //use exceptions
    Try

      Repeat
        Readln(FileVar1, s);
        Writeln(FileVar2, s);
      Until EOF(FileVar1);

      CloseFile(FileVar1);
      CloseFile(FileVar2);
    Except
    End;
    {$I-}
    //!use exceptions
  End;
End;

Procedure TMainForm.acImportExecute(Sender: TObject);
Var
  s, d: String;
  FileVar1: TextFile;
  FileVar2: TextFile;
  P, p1, p2, p3, p4: Integer;
Begin
  OpenDialog1.filename := 'transgui.ini';
  If OpenDialog1.Execute Then
  Begin
    s := OpenDialog1.filename;
    d := Ini.getFileName();
    p1 := 0;
    p2 := 0;
    p3 := 0;
    p4 := 0;
    // check valid Ini-file
    AssignFile(FileVar2, s);
    Reset(FileVar2);
    {$I+}
    //use exceptions
    Try
      Repeat
        Readln(FileVar2, s);
        P := Pos('[Hosts]', s);
        If P > 0 Then p1 := P;

        P := Pos('[MainForm]', s);
        If P > 0 Then p2 := P;

        P := Pos('[TorrentsList]', s);
        If P > 0 Then p3 := P;

        P := Pos('ShowCountryFlag=', s);
        If P > 0 Then p4 := P;
      Until EOF(FileVar2);
      CloseFile(FileVar2);
    Except
    End;
    {$I-}
    //!use exceptions

    If (p1 = 0) And (p2 = 0) And (p3 = 0) And (p4 = 0) Then
    Begin
      MessageDlg('Invalid file!', mtError, [mbOK], 0);
      exit;
    End;

    // rewrite ini-file
    s := OpenDialog1.filename;
    AssignFile(FileVar1, s);
    AssignFile(FileVar2, d);

    Reset(FileVar1);
    Rewrite(FileVar2);

    {$I+}
    //use exceptions
    Try
      Repeat
        Readln(FileVar1, s);
        Writeln(FileVar2, s);
      Until EOF(FileVar1);
      CloseFile(FileVar1);
      CloseFile(FileVar2);
    Except
    End;
    {$I-}
    //!use exceptions

    // Read ini now!
    CheckAppParams();
    MessageDlg(sRestartRequired, mtInformation, [mbOK], 0);
  End;
End;

Procedure TMainForm.acExitExecute(Sender: TObject);
Begin
  BeforeCloseApp;
  Application.Terminate;
End;

Procedure TMainForm.acDaemonOptionsExecute(Sender: TObject);
Var
  req, args: TJSONObject;
  s: String;
  i, j: Integer;
Begin
  With TDaemonOptionsForm.Create(Self) Do
  Try
    AppBusy;
    req := TJSONObject.Create;
    Try
      req.Add('method', 'session-get');
      args := RpcObj.SendRequest(req);
      If args <> nil Then
      Try
        edDownloadDir.Text :=
          CorrectPath(UTF8Encode(args.Strings['download-dir']));
        If RpcObj.RPCVersion >= 5 Then
        Begin
          // RPC version 5
          edPort.Value := args.Integers['peer-port'];
          cbPEX.Checked := args.Integers['pex-enabled'] <> 0;
          edMaxPeers.Value := args.Integers['peer-limit-global'];
          cbRandomPort.Checked :=
            args.Integers['peer-port-random-on-start'] <> 0;
          cbDHT.Checked := args.Integers['dht-enabled'] <> 0;
          cbSeedRatio.Checked := args.Integers['seedRatioLimited'] <> 0;
          edSeedRatio.Value := args.Floats['seedRatioLimit'];
          cbBlocklist.Checked := args.Integers['blocklist-enabled'] <> 0;

          cbAltEnabled.Checked := args.Integers['alt-speed-enabled'] <> 0;
          edAltDown.Value := args.Integers['alt-speed-down'];
          edAltUp.Value := args.Integers['alt-speed-up'];
          cbAutoAlt.Checked :=
            args.Integers['alt-speed-time-enabled'] <> 0;
          edAltTimeBegin.Text :=
            FormatDateTime('hh:nn', args.Integers['alt-speed-time-begin'] /
            MinsPerDay);
          edAltTimeEnd.Text :=
            FormatDateTime('hh:nn', args.Integers['alt-speed-time-end'] /
            MinsPerDay);
          j := args.Integers['alt-speed-time-day'];
          For i := 1 To 7 Do
          Begin
            TCheckBox(gbAltSpeed.FindChildControl(
              Format('cbDay%d', [i]))).Checked := Longbool(j And 1);
            j := j Shr 1;
          End;
          cbAutoAltClick(nil);
        End
        Else
        Begin
          // RPC versions prior to v5
          cbPortForwarding.Top := cbRandomPort.Top;
          edPort.Value := args.Integers['port'];
          cbPEX.Checked := args.Integers['pex-allowed'] <> 0;
          edMaxPeers.Value := args.Integers['peer-limit'];
          cbRandomPort.Visible := False;
          cbDHT.Visible := False;
          cbSeedRatio.Visible := False;
          edSeedRatio.Visible := False;
          btTestPort.Visible := False;
          cbBlocklist.Visible := False;
          gbAltSpeed.Visible := False;
        End;

        If RpcObj.RPCVersion >= 7 Then
        Begin
          cbIncompleteDir.Checked :=
            args.Integers['incomplete-dir-enabled'] <> 0;
          edIncompleteDir.Text :=
            UTF8Encode(args.Strings['incomplete-dir']);
          cbIncompleteDirClick(nil);
        End
        Else
        Begin
          cbIncompleteDir.Visible := False;
          edIncompleteDir.Visible := False;
        End;

        If RpcObj.RPCVersion >= 8 Then
          cbPartExt.Checked := args.Integers['rename-partial-files'] <> 0
        Else
          cbPartExt.Visible := False;

        If RpcObj.RPCVersion >= 9 Then
          cbLPD.Checked := args.Integers['lpd-enabled'] <> 0
        Else
          cbLPD.Visible := False;

        If RpcObj.RPCVersion >= 10 Then
        Begin
          edCacheSize.Value := args.Integers['cache-size-mb'];
          cbIdleSeedLimit.Checked :=
            args.Integers['idle-seeding-limit-enabled'] <> 0;
          edIdleSeedLimit.Value := args.Integers['idle-seeding-limit'];
          cbIdleSeedLimitClick(nil);
        End
        Else
        Begin
          edCacheSize.Visible := False;
          txCacheSize.Visible := False;
          txMB.Visible := False;
          cbIdleSeedLimit.Visible := False;
          edIdleSeedLimit.Visible := False;
          txMinutes.Visible := False;
        End;


        If args.IndexOfName('blocklist-url') >= 0 Then
          edBlocklistURL.Text := UTF8Encode(args.Strings['blocklist-url'])
        Else
        Begin
          edBlocklistURL.Visible := False;
          cbBlocklist.Left := cbPEX.Left;
          cbBlocklist.Caption :=
            StringReplace(cbBlocklist.Caption, ':', '', [rfReplaceAll]);
        End;
        cbBlocklistClick(nil);

        If RpcObj.RPCVersion >= 13 Then
          cbUTP.Checked := args.Integers['utp-enabled'] <> 0
        Else
          cbUTP.Visible := False;

        If RpcObj.RPCVersion >= 14 Then
        Begin
          tabQueue.TabVisible := True;
          cbDownQueue.Checked :=
            args.Integers['download-queue-enabled'] <> 0;
          edDownQueue.Value := args.Integers['download-queue-size'];
          cbUpQueue.Checked := args.Integers['seed-queue-enabled'] <> 0;
          edUpQueue.Value := args.Integers['seed-queue-size'];
          cbStalled.Checked :=
            args.Integers['queue-stalled-enabled'] <> 0;
          edStalledTime.Value := args.Integers['queue-stalled-minutes'];
        End
        Else
          tabQueue.TabVisible := False;

        cbPortForwarding.Checked :=
          args.Integers['port-forwarding-enabled'] <> 0;
        s := args.Strings['encryption'];
        If s = 'preferred' Then
          cbEncryption.ItemIndex := 1
        Else
          If s = 'required' Then
            cbEncryption.ItemIndex := 2
          Else
            cbEncryption.ItemIndex := 0;
        cbMaxDown.Checked := args.Integers['speed-limit-down-enabled'] <> 0;
        edMaxDown.Value := args.Integers['speed-limit-down'];
        cbMaxUp.Checked := args.Integers['speed-limit-up-enabled'] <> 0;
        edMaxUp.Value := args.Integers['speed-limit-up'];
      Finally
        args.Free;
      End
      Else
      Begin
        CheckStatus(False);
        exit;
      End;
    Finally
      req.Free;
    End;
    cbMaxDownClick(nil);
    cbMaxUpClick(nil);
    cbRandomPortClick(nil);
    cbSeedRatioClick(nil);
    AppNormal;

    If ShowModal = mrOk Then
    Begin
      AppBusy;
      Self.Update;
      req := TJSONObject.Create;
      Try
        req.Add('method', 'session-set');
        args := TJSONObject.Create;
        args.Add('download-dir', UTF8Decode(edDownloadDir.Text));
        args.Add('port-forwarding-enabled', Integer(cbPortForwarding.Checked) And 1);
        Case cbEncryption.ItemIndex Of
          1: s := 'preferred';
          2: s := 'required';
          Else
            s := 'tolerated';
        End;
        args.Add('encryption', s);
        args.Add('speed-limit-down-enabled', Integer(cbMaxDown.Checked) And 1);
        If cbMaxDown.Checked Then
          args.Add('speed-limit-down', edMaxDown.Value);
        args.Add('speed-limit-up-enabled', Integer(cbMaxUp.Checked) And 1);
        If cbMaxUp.Checked Then
          args.Add('speed-limit-up', edMaxUp.Value);
        If RpcObj.RPCVersion >= 5 Then
        Begin
          args.Add('peer-limit-global', edMaxPeers.Value);
          args.Add('peer-port', edPort.Value);
          args.Add('pex-enabled', Integer(cbPEX.Checked) And 1);
          args.Add('peer-port-random-on-start',
            Integer(cbRandomPort.Checked) And 1);
          args.Add('dht-enabled', Integer(cbDHT.Checked) And 1);
          args.Add('seedRatioLimited', Integer(cbSeedRatio.Checked) And 1);
          If cbSeedRatio.Checked Then
            args.Add('seedRatioLimit', edSeedRatio.Value);
          args.Add('blocklist-enabled', Integer(cbBlocklist.Checked) And 1);

          args.Add('alt-speed-enabled', Integer(cbAltEnabled.Checked) And 1);
          args.Add('alt-speed-down', edAltDown.Value);
          args.Add('alt-speed-up', edAltUp.Value);
          args.Add('alt-speed-time-enabled', Integer(cbAutoAlt.Checked) And 1);
          If cbAutoAlt.Checked Then
          Begin
            args.Add('alt-speed-time-begin',
              Round(Frac(StrToTime(edAltTimeBegin.Text)) * MinsPerDay));
            args.Add('alt-speed-time-end',
              Round(Frac(StrToTime(edAltTimeEnd.Text)) * MinsPerDay));
            j := 0;
            For i := 7 Downto 1 Do
            Begin
              j := j Shl 1;
              j := j Or (Integer(TCheckBox(gbAltSpeed.FindChildControl(
                Format('cbDay%d', [i]))).Checked) And 1);
            End;
            args.Add('alt-speed-time-day', j);
          End;
        End
        Else
        Begin
          args.Add('peer-limit', edMaxPeers.Value);
          args.Add('port', edPort.Value);
          args.Add('pex-allowed', Integer(cbPEX.Checked) And 1);
        End;
        If RpcObj.RPCVersion >= 7 Then
        Begin
          args.Add('incomplete-dir-enabled',
            Integer(cbIncompleteDir.Checked) And 1);
          If cbIncompleteDir.Checked Then
            args.Add('incomplete-dir', UTF8Decode(edIncompleteDir.Text));
        End;
        If RpcObj.RPCVersion >= 8 Then
          args.Add('rename-partial-files', Integer(cbPartExt.Checked) And 1);
        If RpcObj.RPCVersion >= 9 Then
          args.Add('lpd-enabled', Integer(cbLPD.Checked) And 1);
        If RpcObj.RPCVersion >= 10 Then
        Begin
          args.Add('cache-size-mb', edCacheSize.Value);
          args.Add('idle-seeding-limit-enabled',
            Integer(cbIdleSeedLimit.Checked) And 1);
          args.Add('idle-seeding-limit', edIdleSeedLimit.Value);
        End;
        If edBlocklistURL.Visible Then
          If cbBlocklist.Checked Then
            args.Add('blocklist-url', UTF8Decode(edBlocklistURL.Text));
        If RpcObj.RPCVersion >= 13 Then
          args.Add('utp-enabled', Integer(cbUTP.Checked) And 1);
        If RpcObj.RPCVersion >= 14 Then
        Begin
          args.Add('download-queue-enabled', Integer(cbDownQueue.Checked) And 1)
          ;
          args.Add('download-queue-size', edDownQueue.Value);
          args.Add('seed-queue-enabled', Integer(cbUpQueue.Checked) And 1);
          args.Add('seed-queue-size', edUpQueue.Value);
          args.Add('queue-stalled-enabled', Integer(cbStalled.Checked) And 1);
          args.Add('queue-stalled-minutes', edStalledTime.Value);
        End;

        req.Add('arguments', args);
        args := RpcObj.SendRequest(req, False);
        If args = nil Then
        Begin
          CheckStatus(False);
          exit;
        End;
      Finally
        args.Free;
        req.Free;
      End;
      RpcObj.RefreshNow := RpcObj.RefreshNow + [rtSession];
      AppNormal;
    End;
  Finally
    Free;
  End;
End;

Procedure TMainForm.acQMoveBottomExecute(Sender: TObject);
Begin
  TorrentAction(GetSelectedTorrents, 'queue-move-bottom');
End;

Procedure TMainForm.acQMoveDownExecute(Sender: TObject);
Begin
  TorrentAction(GetSelectedTorrents, 'queue-move-down');
End;

Procedure TMainForm.acQMoveTopExecute(Sender: TObject);
Begin
  TorrentAction(GetSelectedTorrents, 'queue-move-top');
End;

Procedure TMainForm.acQMoveUpExecute(Sender: TObject);
Begin
  TorrentAction(GetSelectedTorrents, 'queue-move-up');
End;

Procedure TMainForm.acReannounceTorrentExecute(Sender: TObject);
Begin
  TorrentAction(GetSelectedTorrents, 'torrent-reannounce');
End;

Procedure TMainForm.acRemoveTorrentAndDataExecute(Sender: TObject);
Begin
  InternalRemoveTorrent(sRemoveTorrentData, sRemoveTorrentDataMulti, True);
End;

Procedure TMainForm.acRemoveTorrentExecute(Sender: TObject);
Begin
  InternalRemoveTorrent(sRemoveTorrent, sRemoveTorrentMulti, False);
End;

Procedure TMainForm.acRenameExecute(Sender: TObject);
Begin
  If lvFiles.Focused Then
    lvFiles.EditCell(idxFileName, lvFiles.Row)
  Else
    gTorrents.EditCell(idxName, gTorrents.Row);
End;

Procedure TMainForm.acResolveCountryExecute(Sender: TObject);
Begin
  If Not acResolveCountry.Checked Then
    If GetGeoIpDatabase = '' Then
      If Not DownloadGeoIpDatabase(False) Then
        exit;

  acResolveCountry.Checked := Not acResolveCountry.Checked;
  FreeAndNil(FResolver);
  DoRefresh;
  acShowCountryFlag.Enabled := acResolveCountry.Checked;
End;

Procedure TMainForm.acResolveHostExecute(Sender: TObject);
Begin
  acResolveHost.Checked := Not acResolveHost.Checked;
  FreeAndNil(FResolver);
  DoRefresh;
End;

Procedure TMainForm.acSelectAllExecute(Sender: TObject);
Begin
  Application.ProcessMessages;
  If lvFiles.Focused Then
    lvFiles.SelectAll
  Else
    gTorrents.SelectAll;
End;

Procedure TMainForm.acSetHighPriorityExecute(Sender: TObject);
Begin
  Application.ProcessMessages;
  If lvFiles.Focused Then
    SetCurrentFilePriority('high')
  Else
    SetTorrentPriority(TR_PRI_HIGH);
End;

Procedure TMainForm.acSetLowPriorityExecute(Sender: TObject);
Begin
  Application.ProcessMessages;
  If lvFiles.Focused Then
    SetCurrentFilePriority('low')
  Else
    SetTorrentPriority(TR_PRI_LOW);
End;

Procedure TMainForm.acSetNormalPriorityExecute(Sender: TObject);
Begin
  Application.ProcessMessages;
  If lvFiles.Focused Then
    SetCurrentFilePriority('normal')
  Else
    SetTorrentPriority(TR_PRI_NORMAL);
End;

Procedure TMainForm.acSetNotDownloadExecute(Sender: TObject);
Begin
  SetCurrentFilePriority('skip');
End;

Procedure TMainForm.acSetLabelsExecute(Sender: TObject);
Var
  ids: Variant;
  i, j: Integer;
  input, ss, s: String;
  req: TJSONObject;
  aids: TJSONArray;
  alabels: TJSONArray;
  slabels: TStringList;
  args: TJSONObject;
  a: TJSONArray;
  t: TJSONObject;
  //t1:TDateTime;
  //ok:boolean;
Begin
  If gTorrents.Items.Count = 0 Then
    exit;
  Application.ProcessMessages;
  gTorrents.Tag := 1;
  gTorrents.EnsureSelectionVisible;
  If gTorrents.SelCount = 0 Then
    gTorrents.RowSelected[gTorrents.Row] := True;
  ids := GetSelectedTorrents;
  i := gTorrents.Items.IndexOf(idxTorrentId, ids[0]);
  If VarIsEmpty(gTorrents.Items[idxPath, i]) Then
    exit;
  AppBusy;
  args := TJSONObject.Create;
  aids := TJSONArray.Create;
  For i := VarArrayLowBound(ids, 1) To VarArrayHighBound(ids, 1) Do
    aids.Add(Integer(ids[i]));
  args.Add('ids', aids);
  args.Add('fields', TJSONArray.Create(['id', 'labels']));
  req := TJSONObject.Create;
  req.Add('method', 'torrent-get');
  req.Add('arguments', args);
  args := RpcObj.SendRequest(req);
  AppNormal;
  If args = nil Then
  Begin
    CheckStatus(False);
    exit;
  End;

  t := args.Arrays['torrents'][0] As TJSONObject;

  a := t.Arrays['labels'];
  s := '';
  For j := 0 To a.Count - 1 Do
  Begin
    ss := UTF8Encode(WideString(a.Strings[j]));
    If j > 0 Then s := s + ', ';
    s := s + ss;
  End;
  args.Free;
  input := s;
  If InputQuery('Set tags', 'This will overwrite any existing tags.' +
    sLineBreak + 'You can set multiple tags separated by a comma or leave empty to clear tags.'
    , input) Then
  Begin
    Application.ProcessMessages;
    AppBusy;
    req := TJSONObject.Create;
    args := TJSONObject.Create;
    aids := TJSONArray.Create;
    alabels := TJSONArray.Create;
    slabels := TStringList.Create;
    Try
      req.Add('method', 'torrent-set');
      For i := VarArrayLowBound(ids, 1) To VarArrayHighBound(ids, 1) Do
        aids.Add(Integer(ids[i]));
      args.Add('ids', aids);
      SplitRegExpr(',', input, slabels);
      slabels.Sort;
      For s In slabels Do
      Begin
        If trim(s) <> '' Then
          alabels.Add(trim(s));
      End;
      //            args.Add('labels','');
      If alabels.Count > 0 Then
        args.Add('labels', alabels);
      req.Add('arguments', args);
      args := RpcObj.SendRequest(req, False);
      If args = nil Then
        CheckStatus(False)
      Else
      Begin
        RpcObj.RequestFullInfo := True;
        DoRefresh(True);
        RpcObj.ForceRequest := 2;
        Sleep(200);
        Application.ProcessMessages;
      End;
    Finally
      args.Free;
      req.Free;
      AppNormal;
      //      alabels.Free;
      slabels.Free;
    End;

  End;
  gTorrents.Tag := 0;
End;

Procedure TMainForm.acSetupColumnsExecute(Sender: TObject);
Var
  g: TVarGrid;
  s: String;
Begin
  Application.ProcessMessages;
  If lvTrackers.Focused Then
    g := lvTrackers
  Else
    If lvPeers.Focused Then
      g := lvPeers
    Else
      If lvFiles.Focused Then
        g := lvFiles
      Else
        g := gTorrents;
  If g = gTorrents Then
    s := sTorrents
  Else
    If PageInfo.ActivePage = tabFiles Then
      s := FFilesCapt
    Else
      s := PageInfo.ActivePage.Caption;
  If Not SetupColumns(g, 0, s) Then exit;
  If g = gTorrents Then
    TorrentColumnsChanged;
End;

Procedure TMainForm.acShowAppExecute(Sender: TObject);
Begin
  ShowApp;
End;

Procedure TMainForm.acShowCountryFlagExecute(Sender: TObject);
Const
  FlagsURL =

    'https://raw.githubusercontent.com/transmission-remote-gui/transgui/master/flags.zip';
Begin
  If Not acShowCountryFlag.Checked Then
    If GetFlagsArchive = '' Then
    Begin
      If MessageDlg('', sFlagArchiveConfirm, mtConfirmation, mbYesNo,
        0, mbYes) <> mrYes Then
        exit;
      If Not DownloadFile(FlagsURL, FHomeDir) Then
        exit;
    End;
  acShowCountryFlag.Checked := Not acShowCountryFlag.Checked;
  DoRefresh;
End;

Procedure TMainForm.acStartAllTorrentsExecute(Sender: TObject);
Begin
  TorrentAction(NULL, 'torrent-start');
End;


Procedure TMainForm.acStatusBarExecute(Sender: TObject);
Begin
  acStatusBar.Checked := Not acStatusBar.Checked;
  StatusBar.Visible := acStatusBar.Checked;
  If StatusBar.Visible Then
  //      StatusBar.Top:=ClientHeight
  Else
  Begin
    acStatusBarSizes.Checked := True;
    acStatusBarSizesExecute(nil);
  End;
End;

Procedure TMainForm.acStatusBarSizesExecute(Sender: TObject);
Begin
  acStatusBarSizes.Checked := Not acStatusBarSizes.Checked;
  If acStatusBarSizes.Checked Then
  Begin
    acStatusBar.Checked := False;
    acStatusBarExecute(nil);
  End
  Else
  Begin
    StatusBar.Panels[4].Text := '';
    StatusBar.Panels[5].Text := '';
    StatusBar.Panels[6].Text := '';
    StatusBar.Panels[7].Text := '';
  End;
  Ini.WriteBool('MainForm', 'StatusBarSizes', acStatusBarSizes.Checked);
End;

Procedure TMainForm.acStopAllTorrentsExecute(Sender: TObject);
Begin
  TorrentAction(NULL, 'torrent-stop');
End;


Procedure TMainForm.gTorrentsMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
Var
  r, c, ADatacol: Integer;
Begin
  ADataCol := 0;
  gTorrents.MouseToCell(x, y, c, r);
  If c >= 0 Then ADataCol := gTorrents.ColToDataCol(c);
  If r = 0 Then gTorrents.Hint := '';
  If (ADataCol <> FCol) Or (r <> FRow) Then
  Begin
    FCol := ADataCol;
    FRow := r;
    Case ADataCol Of
      idxAddedOn, idxCompletedOn, idxLastActive:
      Begin
        Application.CancelHint;
        gTorrents.Hint :=

          TorrentDateTimeToString(gTorrents.Items[ADataCol, FRow - 1],
          Not (FFromNow));
      End
      Else
        gTorrents.Hint := '';
    End;
  End;
End;

Procedure TMainForm.gTorrentsMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If Button = mbRight Then pmTorrents.PopUp;
End;

Procedure TMainForm.LocalWatchTimerTimer(Sender: TObject);
Begin
  ReadLocalFolderWatch;
  If FPendingTorrents.Count > 0 Then
  Begin
    FWatchDownloading := True;
    TickTimerTimer(nil);
  End;
End;

Procedure TMainForm.lvFilesClick(Sender: TObject);
Begin

End;


Procedure TMainForm.lvFilesMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If Button = mbRight Then pmFiles.PopUp;
End;

Procedure TMainForm.MenuItem107Click(Sender: TObject);
Var
  req, args, args1: TJSONObject;
  id, torid: Integer;
  i, i1: Longint;
  listT, s, tText: String;
Begin
  AppBusy;
  Try
    id := 0;
    torid := RpcObj.CurTorrentId;
    tText := UTF8Encode(WideString(lvTrackers.Items[idxTrackersListName,
      lvTrackers.Row]));
    If (pos('http://plab.site/', tText) > 0) Then
      //    if (pos('http://',tText) >0) Then
    Begin
      tText := StringReplace(tText, 'http://plab.site/', 'http://plab.site1/',
        []);
      //        tText:=StringReplace(tText,'http://','https://',[]);
    End
    Else
    Begin
      If pos('http://plab.site1/', tText) > 0 Then
        //        if pos('https://',tText) >0 Then
        tText := StringReplace(tText, 'http://plab.site1/',
          'http://plab.site/', [])


      //          tText:=StringReplace(tText,'https://','http://',[])
      Else
      Begin
        AppNormal;
        exit;
      End;
    End;
    //    id:=lvTrackers.Items[idxTrackerID, lvTrackers.Row];
    //    Self.Update;
    req := TJSONObject.Create;
    Try
      Try

        req.Add('method', 'torrent-set');
        args := TJSONObject.Create;
        args.Add('ids', TJSONArray.Create([torid]));
        If (rpcObj.RPCVersion >= 17) Then
        Begin
          listT := '';
          i1 := lvTrackers.Row;
          For i := 0 To lvTrackers.Items.Count - 1 Do
          Begin
            If (i = i1) Then
              s := tText
            Else
              s := lvTrackers.Items[idxTrackersListName, i];
            listT := listT + s + #10;
          End;
          args.Add('trackerList', listT);
        End
        Else
          args.Add('trackerReplace', TJSONArray.Create([id, UTF8Encode(tText)]));
        //fix bag
        req.Add('arguments', args);
        //FreeAndNil(args);
        args1 := RpcObj.SendRequest(req, False);
        If args1 <> nil Then
          FreeAndNil(args1);
      Except
        OutputDebugString(LPCSTR(Exception(ExceptObject).Message +
          '(MenuItem107Click)'));
      End;

    Finally
      req.Free;
    End;
    Try
      TorrentAction(GetSelectedTorrents, 'torrent-reannounce');
    Except
      OutputDebugString(LPCSTR(Exception(ExceptObject).Message +
        '(torrent-reannounce)'));
    End;
    DoRefresh;
  Finally
    AppNormal;
    //    Free;
  End;

End;

Procedure TMainForm.MenuShowExecute(Sender: TObject);
Begin
  acMenuShow.Checked := Not acMenuShow.Checked;
  If acMenuShow.Checked = False Then
    MainForm.Menu := nil
  Else
    MainForm.Menu := MainMenu;
End;

Procedure TMainForm.acToolbarShowExecute(Sender: TObject);
Begin
  acToolbarShow.Checked := Not acToolbarShow.Checked;
  If acToolbarShow.Checked = False Then
    MainToolBar.Visible := False
  Else
    MainToolBar.Visible := True;
End;

Procedure TMainForm.acTorrentPropsExecute(Sender: TObject);
Begin
  TorrentProps(0);
End;

Procedure TMainForm.TorrentProps(PageNo: Integer; filtered: Boolean = False);
Const
  TR_RATIOLIMIT_GLOBAL = 0;
  // follow the global settings
  TR_RATIOLIMIT_SINGLE = 1;
  // override the global settings, seeding until a certain ratio
  TR_RATIOLIMIT_UNLIMITED = 2;
  // override the global settings, seeding regardless of ratio

  TR_IDLELIMIT_GLOBAL = 0;
  // follow the global settings
  TR_IDLELIMIT_SINGLE = 1;
  // override the global settings, seeding until a certain idle time
  TR_IDLELIMIT_UNLIMITED = 2;
  // override the global settings, seeding regardless of activity
Var
  req, args, t, tr: TJSONObject;
  i, j, id, c1, c2: Integer;
  ids, Trackers, AddT, EditT, DelT: TJSONArray;
  listT: String;
  TorrentIds: Variant;
  s: String;
  trlist, sl: TStringList;
Begin
  gTorrentsClick(nil);
  id := RpcObj.CurTorrentId;
  If id = 0 Then exit;

  AppBusy;
  trlist := nil;
  With TTorrPropsForm.Create(Self) Do
  Try
    Page.ActivePageIndex := PageNo;
    gTorrents.Tag := 1;
    gTorrents.EnsureSelectionVisible;
    If filtered Then
      TorrentIds := GetFilteredTorrents
    Else
      TorrentIds := GetSelectedTorrents;
    args := RpcObj.RequestInfos(TorrentIds, ['downloadLimit',
      'sequential_download', 'sequentialDownload', 'downloadLimitMode',
      'downloadLimited', 'uploadLimit', 'uploadLimitMode', 'uploadLimited',
      'name', 'maxConnectedPeers', 'seedRatioMode', 'seedRatioLimit',
      'seedIdleLimit', 'seedIdleMode', 'trackers']);
    If args = nil Then
    Begin
      CheckStatus(True);
      exit;
    End;
    If args.Arrays['torrents'].Count = 0 Then
    Begin
      CheckStatus(True);
      exit;
    End;
    Try
      t := args.Arrays['torrents'].Objects[0];
      c1 := args.Arrays['torrents'].Count;

      //      if gTorrents.SelCount > 1 then
      //        s:=Format(sSeveralTorrents, [gTorrents.SelCount])
      If c1 > 1 Then
        s := Format(sSeveralTorrents, [c1])
      Else
        s := UTF8Encode(t.Strings['name']);

      txName.Caption := txName.Caption + ' ' + s;
      Caption := Caption + ' - ' + s;
      If RpcObj.RPCVersion < 5 Then
      Begin
        // RPC versions prior to v5
        j := t.Integers['downloadLimitMode'];
        cbMaxDown.Checked := j = TR_SPEEDLIMIT_SINGLE;
        i := t.Integers['downloadLimit'];
        If (i < 0) Or (j = TR_SPEEDLIMIT_UNLIMITED) Then
          edMaxDown.ValueEmpty := True
        Else
          edMaxDown.Value := i;

        j := t.Integers['uploadLimitMode'];
        cbMaxUp.Checked := j = TR_SPEEDLIMIT_SINGLE;
        i := t.Integers['uploadLimit'];
        If (i < 0) Or (j = TR_SPEEDLIMIT_UNLIMITED) Then
          edMaxUp.ValueEmpty := True
        Else
          edMaxUp.Value := i;
        cbSeedRatio.Visible := False;
        edSeedRatio.Visible := False;
      End
      Else
      Begin
        // RPC version 5
        cbMaxDown.Checked := t.Booleans['downloadLimited'];
        i := t.Integers['downloadLimit'];
        If i < 0 Then
          edMaxDown.ValueEmpty := True
        Else
          edMaxDown.Value := i;

        cbMaxUp.Checked := t.Booleans['uploadLimited'];
        i := t.Integers['uploadLimit'];
        If i < 0 Then
          edMaxUp.ValueEmpty := True
        Else
          edMaxUp.Value := i;

        Case t.Integers['seedRatioMode'] Of
          TR_RATIOLIMIT_SINGLE:
            cbSeedRatio.State := cbChecked;
          TR_RATIOLIMIT_UNLIMITED:
            cbSeedRatio.State := cbUnchecked;
          Else
            cbSeedRatio.State := cbGrayed;
        End;
        edSeedRatio.Value := t.Floats['seedRatioLimit'];
      End;

      If RpcObj.RPCVersion >= 10 Then
      Begin
        Case t.Integers['seedIdleMode'] Of
          TR_IDLELIMIT_SINGLE:
            cbIdleSeedLimit.State := cbChecked;
          TR_IDLELIMIT_UNLIMITED:
            cbIdleSeedLimit.State := cbUnchecked;
          Else
            cbIdleSeedLimit.State := cbGrayed;
        End;
        edIdleSeedLimit.Value := t.Integers['seedIdleLimit'];
        cbIdleSeedLimitClick(nil);

        trlist := TStringList.Create;
        Trackers := t.Arrays['trackers'];
        For i := 0 To Trackers.Count - 1 Do
        Begin
          tr := Trackers[i] As TJSONObject;
          trlist.AddObject(Ansistring(UTF8Decode(tr.Strings['announce'])),
            TObject(PtrUInt(tr.Integers['id'])));
        End;
        edTrackers.Lines.Assign(trlist);
      End
      Else
      Begin
        cbIdleSeedLimit.Visible := False;
        edIdleSeedLimit.Visible := False;
        txMinutes.Visible := False;
        tabAdvanced.TabVisible := False;
      End;
      If RpcObj.RPCVersion >= 18 Then
      Begin
        Try
          Begin
            cbSequentialDownload.Visible := True;
            c1 := 0;
            c2 := 0;
            For i := 0 To args.Arrays['torrents'].Count - 1 Do
            Begin
              If (args.Arrays['torrents'].Objects[i].Find('sequential_download') =
                nil) And (args.Arrays['torrents'].Objects[i].Find('sequentialDownload') =
                nil) Then
              Begin
                c2 := c2 + 1;
                continue;
              End;
              If args.Arrays['torrents'].Objects[i].Find('sequential_download') <>
                nil Then
              Begin
                If args.Arrays['torrents'].Objects[i].Integers[
                  'sequential_download'] = 1 Then
                Begin
                  c1 := c1 + 1;
                  continue;
                End;
              End;
              If args.Arrays['torrents'].Objects[i].Find('sequentialDownload') <>
                nil Then
              Begin
                If args.Arrays['torrents'].Objects[i].Integers[
                  'sequentialDownload'] = 1 Then
                Begin
                  c1 := c1 + 1;
                  continue;
                End;
              End;
              c2 := c2 + 1;
            End;
            cbSequentialDownload.AllowGrayed := False;
            If (c1 <> 0) And (c2 = 0) Then
              cbSequentialDownload.State := cbChecked
            Else If (c2 <> 0) And (c1 = 0) Then
                cbSequentialDownload.State := cbUnChecked
              Else
              Begin
                cbSequentialDownload.AllowGrayed := True;
                cbSequentialDownload.State := cbGrayed;
              End;
          End;
        Except
          cbSequentialDownload.Visible := False;
        End;
      End
      Else
        cbSequentialDownload.Visible := False;
      edPeerLimit.Value := t.Integers['maxConnectedPeers'];
    Finally
      args.Free;
    End;
    cbMaxDownClick(nil);
    cbMaxUpClick(nil);
    cbSeedRatioClick(nil);
    AppNormal;
    If ShowModal = mrOk Then
    Begin
      AppBusy;
      Self.Update;
      req := TJSONObject.Create;
      Try
        req.Add('method', 'torrent-set');
        args := TJSONObject.Create;
        ids := TJSONArray.Create;
        For i := VarArrayLowBound(TorrentIds, 1) To VarArrayHighBound(
            TorrentIds, 1) Do
          ids.Add(Integer(TorrentIds[i]));
        args.Add('ids', ids);

        If RpcObj.RPCVersion >= 18 Then
        Begin
          Try
            Begin
              If cbSequentialDownload.State <> cbGrayed Then
              Begin
                args.Add('sequential_download',
                  Integer(cbSequentialDownload.Checked) And 1);
                args.Add('sequentialDownload',
                  Integer(cbSequentialDownload.Checked) And 1);
              End;
            End;
          Finally
          End;
        End;
        If RpcObj.RPCVersion < 5 Then
        Begin
          // RPC versions prior to v5
          args.Add('speed-limit-down-enabled', Integer(cbMaxDown.Checked) And 1);
          args.Add('speed-limit-up-enabled', Integer(cbMaxUp.Checked) And 1);
          If cbMaxDown.Checked Then
            args.Add('speed-limit-down', edMaxDown.Value);
          If cbMaxUp.Checked Then
            args.Add('speed-limit-up', edMaxUp.Value);
        End
        Else
        Begin
          // RPC version 5
          args.Add('downloadLimited', Integer(cbMaxDown.Checked) And 1);
          args.Add('uploadLimited', Integer(cbMaxUp.Checked) And 1);
          If cbMaxDown.Checked Then
            args.Add('downloadLimit', edMaxDown.Value);
          If cbMaxUp.Checked Then
            args.Add('uploadLimit', edMaxUp.Value);
          Case cbSeedRatio.State Of
            cbChecked:
              i := TR_RATIOLIMIT_SINGLE;
            cbUnchecked:
              i := TR_RATIOLIMIT_UNLIMITED;
            Else
              i := TR_RATIOLIMIT_GLOBAL;
          End;
          args.Add('seedRatioMode', i);
          If cbSeedRatio.State = cbChecked Then
            args.Add('seedRatioLimit', edSeedRatio.Value);
        End;

        If RpcObj.RPCVersion >= 10 Then
        Begin
          Case cbIdleSeedLimit.State Of
            cbChecked:
              i := TR_IDLELIMIT_SINGLE;
            cbUnchecked:
              i := TR_IDLELIMIT_UNLIMITED;
            Else
              i := TR_IDLELIMIT_GLOBAL;
          End;
          args.Add('seedIdleMode', i);
          If cbIdleSeedLimit.State = cbChecked Then
            args.Add('seedIdleLimit', edIdleSeedLimit.Value);

          sl := TStringList.Create;
          Try
            sl.Assign(edTrackers.Lines);
            // Removing unchanged trackers
            If (rpcObj.RPCVersion >= 17) Then
            Begin
              listT := '';
              Try
                For i := 0 To sl.Count - 1 Do
                Begin
                  s := Trim(sl[i]);
                  listT := listT + s + #10;
                End;
                args.Add('trackerList', listT);

              Finally
              End;
            End
            Else
            Begin
              i := 0;
              While i < sl.Count Do
              Begin
                s := Trim(sl[i]);
                If s = '' Then
                Begin
                  sl.Delete(i);
                  continue;
                End;
                j := trlist.IndexOf(s);
                If j >= 0 Then
                Begin
                  trlist.Delete(j);
                  sl.Delete(i);
                  continue;
                End;
                Inc(i);
              End;

              AddT := TJSONArray.Create;
              EditT := TJSONArray.Create;
              DelT := TJSONArray.Create;
              Try
                For i := 0 To sl.Count - 1 Do
                Begin
                  s := Trim(sl[i]);
                  If trlist.Count > 0 Then
                  Begin
                    EditT.Add(PtrUInt(trlist.Objects[0]));
                    EditT.Add(UTF8Decode(s));
                    trlist.Delete(0);
                  End
                  Else
                    AddT.Add(UTF8Decode(s));
                End;

                For i := 0 To trlist.Count - 1 Do
                  DelT.Add(PtrUInt(trlist.Objects[i]));

                If AddT.Count > 0 Then
                Begin
                  args.Add('trackerAdd', AddT);
                  AddT := nil;
                End;
                If EditT.Count > 0 Then
                Begin
                  args.Add('trackerReplace', EditT);
                  EditT := nil;
                End;
                If DelT.Count > 0 Then
                Begin
                  args.Add('trackerRemove', DelT);
                  DelT := nil;
                End;
              Finally
                DelT.Free;
                EditT.Free;
                AddT.Free;
              End;
            End;
          Finally
            sl.Free;
          End;
        End;

        args.Add('peer-limit', edPeerLimit.Value);
        req.Add('arguments', args);
        args := nil;
        args := RpcObj.SendRequest(req, False);
        If args = nil Then
        Begin
          CheckStatus(True);
          exit;
        End;
      Finally
        args.Free;
        req.Free;
      End;
      TorrentAction(TorrentIds, 'torrent-reannounce');
      DoRefresh;
      AppNormal;
    End;
  Finally
    gTorrents.Tag := 0;
    Free;
    trlist.Free;
  End;
End;

Procedure TMainForm.acTrackerGroupingExecute(Sender: TObject);
Begin
  acTrackerGrouping.Checked := Not acTrackerGrouping.Checked;
  Ini.WriteBool('Interface', 'TrackerGrouping', acTrackerGrouping.Checked);
  RpcObj.RefreshNow := RpcObj.RefreshNow + [rtTorrents];
End;

Procedure TMainForm.acUpdateBlocklistExecute(Sender: TObject);
Var
  req: TJSONObject;
  res: TJSONObject;
Begin
  Application.ProcessMessages;
  AppBusy;
  req := TJSONObject.Create;
  Try
    req.Add('method', 'blocklist-update');
    res := RpcObj.SendRequest(req, True, 3 * 60000);
    AppNormal;
    If res = nil Then
    Begin
      CheckStatus(False);
      exit;
    End;
    MessageDlg(Format(sBlocklistUpdateComplete, [res.Integers[('blocklist-size')]]),
      mtInformation, [mbOK], 0);
    res.Free;
  Finally
    req.Free;
  End;
End;

Procedure TMainForm.acUpdateGeoIPExecute(Sender: TObject);
Begin
  If DownloadGeoIpDatabase(True) Then
    MessageDlg(sUpdateComplete, mtInformation, [mbOK], 0);
End;

Procedure TMainForm.acVerifyTorrentExecute(Sender: TObject);
Var
  ids: Variant;
  s: String;
Begin
  If gTorrents.Items.Count = 0 Then exit;
  gTorrents.Tag := 1;
  Try
    gTorrents.EnsureSelectionVisible;
    ids := GetSelectedTorrents;
    If gTorrents.SelCount < 2 Then
      s := Format(sTorrentVerification,
        [UTF8Encode(WideString(gTorrents.Items[idxName,
        gTorrents.Items.IndexOf(idxTorrentId, ids[0])]))])
    Else
      s := Format(sTorrentsVerification, [gTorrents.SelCount]);
    If MessageDlg('', s, mtConfirmation, mbYesNo, 0, mbNo) <> mrYes Then
      exit;
  Finally
    gTorrents.Tag := 0;
  End;
  TorrentAction(ids, 'torrent-verify');
End;

Procedure TMainForm.ApplicationPropertiesEndSession(Sender: TObject);
Begin
  DeleteFileUTF8(FRunFileName);
  BeforeCloseApp;
End;

Procedure TMainForm.ApplicationPropertiesException(Sender: TObject; E: Exception);
Var
  msg: String;
  {$ifdef CALLSTACK}
  sl: TStringList;
  {$endif CALLSTACK}
Begin
  ForceAppNormal;
  msg := E.Message;
  {$ifdef CALLSTACK}
  Try
    sl := TStringList.Create;
    Try
      sl.Text := GetLastExceptionCallStack;
      Clipboard.AsText := msg + LineEnding + sl.Text;
      DebugLn(msg + LineEnding + sl.Text);
      If sl.Count > 20 Then
        Begin
          While sl.Count > 20 Do
            sl.Delete(20);
          sl.Add('...');
        End;
      msg := msg + LineEnding + '---' + LineEnding +
             'The error details has been copied to the clipboard.' + LineEnding
             + '---';
      msg := msg + LineEnding + sl.Text;
    Finally
      sl.Free;
End;
Except
  ;
  // suppress exception
End;
  {$endif CALLSTACK}
  MessageDlg(TranslateString(msg, True), mtError, [mbOK], 0);
End;

Procedure TMainForm.ApplicationPropertiesIdle(Sender: TObject; Var Done: Boolean);
Begin
  UpdateUI;
  {$ifdef LCLcarbon}
  CheckSynchronize;
  {$endif LCLcarbon}
  Done := True;
End;

Procedure TMainForm.ApplicationPropertiesMinimize(Sender: TObject);
Begin
  {$ifdef CPUARM}
  exit;
  {$endif  CPUARM}

  {$ifndef darwin}
  If Not IsUnity And Ini.ReadBool('Interface', 'TrayMinimize', True) Then
    HideApp;
  {$endif darwin}
  UpdateTray;
End;

Procedure TMainForm.ApplicationPropertiesRestore(Sender: TObject);
Begin
  UpdateTray;
  CheckClipboardLink;
End;

Procedure TMainForm.edSearchChange(Sender: TObject);
Begin
  DoRefresh(True);
  If edSearch.Text = '' Then tbSearchCancel.Enabled := False
  Else
    tbSearchCancel.Enabled := True;
End;

Procedure TMainForm.edSearchKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
Begin
  If Key = VK_ESCAPE Then edSearch.Text := '';
End;

Procedure TMainForm.FormActivate(Sender: TObject);
Begin
  CheckClipboardLink;
End;

Procedure TMainForm.FormDropFiles(Sender: TObject; Const FileNames: Array Of String);
Var
  i: Integer;
Begin
  For i := Low(FileNames) To High(FileNames) Do
    AddTorrentFile(FileNames[i]);
End;

Procedure TMainForm.FormKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
Var
  Keypressed: Word;
Begin
  If (Shift = [ssAlt]) And Not (MainForm.ActiveControl Is TVarGridStringEditor) Then
  Begin
    Keypressed := Key;
    Key := 0;
    Case Keypressed Of
      //          VK_S: edSearch.SetFocus;
      VK_G: PageInfo.PageIndex := 0;
      VK_K: PageInfo.PageIndex := 1;
      VK_P: PageInfo.PageIndex := 2;
      VK_F: PageInfo.PageIndex := 3;
      VK_1: lvFilter.Row := fltAll;
      VK_2: lvFilter.Row := fltDown;
      VK_3: lvFilter.Row := fltDone;
      VK_4: lvFilter.Row := fltActive;
      VK_5: lvFilter.Row := fltInactive;
      VK_6: lvFilter.Row := fltStopped;
      VK_7: lvFilter.Row := fltError;
      VK_8: lvFilter.Row := fltWaiting;
      Else
        Key := KeyPressed;
    End;
  End;

End;

Procedure TMainForm.FormWindowStateChange(Sender: TObject);
Begin
  {$ifdef lclgtk2}
  If WindowState = wsMinimized Then
    ApplicationPropertiesMinimize(Nil)
  Else
    ApplicationPropertiesRestore(Nil);
  {$endif lclgtk2}
End;

Procedure TMainForm.gTorrentsCellAttributes(Sender: TVarGrid;
  ACol, ARow, ADataCol: Integer; AState: TGridDrawState;
  Var CellAttribs: TCellAttributes);
Var
  j: Integer;
Begin
  If ARow < 0 Then exit;
  With CellAttribs Do
  Begin
    If ACol = gTorrents.FirstVisibleColumn Then
      ImageIndex := Integer(Sender.Items[idxStateImg, ARow]);
    If Text = '' Then exit;
    If Not VarIsEmpty(Sender.Items[idxDeleted, ARow]) Then
      With Sender.Canvas.Font Do
        Style := Style + [fsStrikeOut];
    Case ADataCol Of
      idxStatus:
        Text := GetTorrentStatus(ARow);
      idxSize, idxDownloaded, idxUploaded, idxSizeToDowload, idxSizeLeft:
        Text :=

          GetHumanSize(Sender.Items[ADataCol, ARow], 0, '?');
      idxDone:
        Text := Format('%.1f%%', [Double(Sender.Items[idxDone, ARow])]);
      idxSeeds:
        If Not VarIsNull(Sender.Items[idxSeedsTotal, ARow]) Then
          Text := GetSeedsText(Sender.Items[idxSeeds, ARow],
            Sender.Items[idxSeedsTotal, ARow]);
      idxPeers:
        Text := GetPeersText(Sender.Items[idxPeers, ARow], -1,
          Sender.Items[idxLeechersTotal, ARow]);
      idxDownSpeed, idxUpSpeed:
      Begin
        j := Sender.Items[ADataCol, ARow];
        If j > 0 Then
          Text := GetHumanSize(j, 1) + sPerSecond
        Else
          Text := '';
      End;
      idxETA:
        Text := EtaToString(Sender.Items[idxETA, ARow]);
      idxRatio:
        Text := RatioToString(Sender.Items[idxRatio, ARow]);
      idxAddedOn, idxCompletedOn, idxLastActive:
        Text :=

          TorrentDateTimeToString(Sender.Items[ADataCol, ARow], FFromNow);
      idxPriority:
        Text := PriorityToStr(Sender.Items[ADataCol, ARow], ImageIndex);
      idxQueuePos:
      Begin
        j := Sender.Items[ADataCol, ARow];
        If j >= FinishedQueue Then
          Dec(j, FinishedQueue);
        Text := IntToStr(j);
      End;
      idxSeedingTime:
      Begin
        j := Sender.Items[idxSeedingTime, ARow];
        If j > 0 Then
          Text := EtaToString(j)
        Else
          Text := '';
      End;
      idxPrivate:
      Begin
        j := Sender.Items[idxPrivate, ARow];
        If j >= 1 Then
          Text := sPrivateOn
        Else
          Text := sPrivateOff;
      End;
    End;
  End;
End;

Procedure TMainForm.gTorrentsClick(Sender: TObject);
Var
  i: Integer;
Begin
  If gTorrents.Tag <> 0 Then exit;
  RpcObj.Lock;
  Try
    If gTorrents.Items.Count > 0 Then
      i := gTorrents.Items[idxTorrentId, gTorrents.Row]
    Else
      i := 0;
    If RpcObj.CurTorrentId = i Then
      exit;
    RpcObj.CurTorrentId := i;
  Finally
    RpcObj.Unlock;
    If acStatusBarSizes.Checked Then StatusBarSizes;
  End;

  //  ClearDetailsInfo(GetPageInfoType(PageInfo.ActivePage));
  If PageInfo.ActivePage <> tabFiles Then
    ClearDetailsInfo(GetPageInfoType(PageInfo.ActivePage))
  Else
    ClearDetailsInfo();



  TorrentsListTimer.Enabled := False;
  TorrentsListTimer.Enabled := True;
End;

Procedure TMainForm.gTorrentsDblClick(Sender: TObject);
Var
  res: TJSONObject;
  s, n: String;
Begin
  If gTorrents.Items.Count = 0 Then
    exit;
  If gTorrents.Items[idxDone, gTorrents.Row] = 100.0 Then
  Begin

    // The torrent is finished. Check if it is possible to open its file/folder
    AppBusy;
    Try
      res := RpcObj.RequestInfo(gTorrents.Items[idxTorrentId, gTorrents.Row],
        ['downloadDir']);
      If res = nil Then
        CheckStatus(False);
      With res.Arrays['torrents'].Objects[0] Do
        n := IncludeProperTrailingPathDelimiter(
          UTF8Encode(Strings['downloadDir'])) +
          UTF8Encode(WideString(gTorrents.Items[idxName, gTorrents.Row]));
      s := MapRemoteToLocal(n);
      If s = '' Then
        s := n;
      If FileExistsUTF8(s) Or DirectoryExistsUTF8(s) Then
      Begin
        // File/folder exists - open it
        OpenCurrentTorrent(False);
        exit;
      End;
    Finally
      AppNormal;
    End;
  End;
  acTorrentProps.Execute;
End;

Procedure TMainForm.gTorrentsDrawCell(Sender: TVarGrid;
  ACol, ARow, ADataCol: Integer; AState: TGridDrawState; Const R: TRect;
  Var ADefaultDrawing: Boolean);
Begin
  If ARow < 0 Then exit;
  If ADataCol = idxDone Then
  Begin
    ADefaultDrawing := False;
    DrawProgressCell(Sender, ACol, ARow, ADataCol, AState, R);
  End;
End;

Procedure TMainForm.gTorrentsEditorHide(Sender: TObject);
Begin
  gTorrents.Tag := 0;
End;

Procedure TMainForm.gTorrentsEditorShow(Sender: TObject);
Begin
  gTorrents.Tag := 1;
  gTorrents.RemoveSelection;
End;

Procedure TMainForm.gTorrentsQuickSearch(Sender: TVarGrid; Var SearchText: String;
  Var ARow: Integer);
Var
  i: Integer;
  s: String;
  v: Variant;
Begin
  s := UTF8UpperCase(SearchText);
  For i := ARow To gTorrents.Items.Count - 1 Do
  Begin
    v := gTorrents.Items[idxName, i];
    If VarIsEmpty(v) Or VarIsNull(v) Then
      continue;
    If Pos(s, Trim(UTF8UpperCase(UTF8Encode(WideString(v))))) > 0 Then
    Begin
      ARow := i;
      break;
    End;
  End;
End;

Procedure TMainForm.gTorrentsResize(Sender: TObject);
Begin
  If Not FStarted Then
  Begin
    VSplitter.SetSplitterPosition(Ini.ReadInteger('MainForm',
      'VSplitter', VSplitter.GetSplitterPosition));
    HSplitter.SetSplitterPosition(Ini.ReadInteger('MainForm',
      'HSplitter', HSplitter.GetSplitterPosition));
  End;
End;

Procedure TMainForm.gTorrentsSetEditText(Sender: TObject; ACol, ARow: Integer;
  Const Value: String);
Begin
  If RenameTorrent(gTorrents.Items[idxTorrentId, ARow],
    UTF8Encode(WideString(gTorrents.Items[idxName, ARow])), Trim(Value)) Then
  Begin
    gTorrents.Items[idxName, ARow] := UTF8Decode(Trim(Value));
    FFilesTree.Clear;
  End;
End;

Procedure TMainForm.gTorrentsKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If Key = VK_APPS Then pmTorrents.PopUp;
End;

Procedure TMainForm.gTorrentsSortColumn(Sender: TVarGrid; Var ASortCol: Integer);
Begin
  If ASortCol = idxSeeds Then
    ASortCol := idxSeedsTotal;
  If ASortCol = idxPeers Then
    ASortCol := idxLeechersTotal;
End;

Procedure TMainForm.HSplitterChangeBounds(Sender: TObject);
Begin
  {$ifdef windows}
  Update;
  {$endif windows}
End;

Procedure TMainForm.lvFilesDblClick(Sender: TObject);
Begin
  acOpenFile.Execute;
End;

Procedure TMainForm.lvFilesEditorHide(Sender: TObject);
Begin
  gTorrents.Tag := 0;
  lvFiles.Tag := 0;
  lvFiles.HideSelection := True;
End;

Procedure TMainForm.lvFilesEditorShow(Sender: TObject);
Begin
  gTorrents.Tag := 1;
  lvFiles.Tag := 1;
  lvFiles.RemoveSelection;
  lvFiles.HideSelection := False;
End;

Procedure TMainForm.lvFilesKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
Begin
  If Key = VK_APPS Then pmFiles.PopUp;
End;

Procedure TMainForm.lvFilesSetEditText(Sender: TObject; ACol, ARow: Integer;
  Const Value: String);
Var
  p: String;
  i, lvl, len: Integer;
Begin
  p := FFilesTree.GetFullPath(ARow, False);
  If RenameTorrent(gTorrents.Items[idxTorrentId, gTorrents.Row], p, Trim(Value)) Then
  Begin
    FFiles[idxFileName, ARow] := UTF8Decode(Trim(Value));
    If FFilesTree.IsFolder(ARow) Then
    Begin
      // Updating path for child elements
      len := Length(p);
      p := ExtractFilePath(p) + Trim(Value);
      lvl := FFilesTree.RowLevel[ARow];
      FFiles.BeginUpdate;
      Try
        FFiles[idxFileFullPath, ARow] :=
          UTF8Decode(p + RemotePathDelimiter);
        For i := ARow + 1 To FFiles.Count - 1 Do
          If FFilesTree.RowLevel[i] > lvl Then
            FFiles[idxFileFullPath, i] :=
              UTF8Decode(p + Copy(
              UTF8Encode(WideString(FFiles[idxFileFullPath, i])), len + 1, MaxInt))
          Else
            break;
      Finally
        FFiles.EndUpdate;
      End;
    End;
  End;
End;

Procedure TMainForm.lvFilterCellAttributes(Sender: TVarGrid;
  ACol, ARow, ADataCol: Integer; AState: TGridDrawState;
  Var CellAttribs: TCellAttributes);
Var
  t: Integer;
Begin
  If ARow < 0 Then exit;
  With CellAttribs Do
  Begin
    Case ARow Of
      0: ImageIndex := imgAll;
      1: ImageIndex := imgDown;
      2: ImageIndex := imgSeed;
      3: ImageIndex := imgActive;
      4: ImageIndex := imgInactive;
      5: ImageIndex := imgStopped;
      6: ImageIndex := imgError;
      7: ImageIndex := imgWaiting
      Else
        If Text <> '' Then
          If VarIsNull(Sender.Items[-1, ARow]) Then
            ImageIndex := 5
          Else
          Begin
            t := Integer(Sender.Items[-2, ARow]);
            If t = 1 Then
              ImageIndex := 22
            Else
              ImageIndex := 44;
          End;
    End;
  End;
End;

Procedure TMainForm.lvFilterClick(Sender: TObject);
Begin
  If VarIsNull(lvFilter.Items[0, lvFilter.Row]) Then
    If (FLastFilterIndex > lvFilter.Row) Or (lvFilter.Row =
      lvFilter.Items.Count - 1) Then
      lvFilter.Row := lvFilter.Row - 1
    Else
      lvFilter.Row := lvFilter.Row + 1;
  FLastFilterIndex := lvFilter.Row;
  FilterTimer.Enabled := False;
  FilterTimer.Enabled := True;
End;

Procedure TMainForm.lvFilterDrawCell(Sender: TVarGrid;
  ACol, ARow, ADataCol: Integer; AState: TGridDrawState; Const R: TRect;
  Var ADefaultDrawing: Boolean);
Var
  i: Integer;
  RR: TRect;
Begin
  ADefaultDrawing := Not VarIsNull(Sender.Items[0, ARow]);
  If ADefaultDrawing Then exit;

  With lvFilter.Canvas Do
  Begin
    Brush.Color := lvFilter.Color;
    FillRect(R);
    i := (R.Bottom + R.Top) Div 2;
    Brush.Color := clBtnFace;
    RR := R;
    InflateRect(RR, -4, 0);
    RR.Top := i - 1;
    RR.Bottom := i + 1;
    FillRect(RR);
  End;
End;

Procedure TMainForm.lvPeersCellAttributes(Sender: TVarGrid;
  ACol, ARow, ADataCol: Integer; AState: TGridDrawState;
  Var CellAttribs: TCellAttributes);
Var
  i: Integer;
Begin
  If ARow < 0 Then exit;
  With CellAttribs Do
  Begin
    If Text = '' Then exit;
    If ACol = 0 Then
    Begin
      ImageIndex := Sender.Items[idxPeerCountryImage, ARow];
      If ImageIndex = 0 Then
        ImageIndex := -1;
    End;
    Case ADataCol Of
      idxPeerDone:
        Text := Format('%.1f%%',
          [Double(Sender.Items[ADataCol, ARow]) * 100.0]);
      idxPeerDownSpeed, idxPeerUpSpeed:
      Begin
        i := Sender.Items[ADataCol, ARow];
        If i > 0 Then
          Text :=
            GetHumanSize(i, 1) + sPerSecond
        Else
          Text := '';
      End;
    End;
  End;
End;

Procedure TMainForm.lvTrackersCellAttributes(Sender: TVarGrid;
  ACol, ARow, ADataCol: Integer; AState: TGridDrawState;
  Var CellAttribs: TCellAttributes);
Var
  f: Double;
Begin
  If ARow < 0 Then exit;
  With CellAttribs Do
  Begin
    If Text = '' Then exit;
    Case ADataCol Of
      idxTrackersListSeeds:
        If lvTrackers.Items[ADataCol, ARow] < 0 Then
          Text := '';
      idxTrackersListUpdateIn:
      Begin
        f := Double(lvTrackers.Items[ADataCol, ARow]);
        If f = 0 Then
          Text := '-'
        Else
          If f = 1 Then
            Text := sUpdating
          Else
            Text := SecondsToString(Trunc(f));
      End;
    End;
  End;
End;

Procedure TMainForm.lvTrackersDblClick(Sender: TObject);
Begin
  acEditTracker.Execute;
End;

Procedure TMainForm.lvTrackersKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If Key = VK_DELETE Then
  Begin
    Key := 0;
    acDelTracker.Execute;
  End;
End;

Procedure TMainForm.goDevelopmentSiteClick(Sender: TObject);
Begin
  goGitHub;
End;

Procedure TMainForm.MainToolBarContextPopup(Sender: TObject; MousePos: TPoint;
  Var Handled: Boolean);
Begin
  acBigToolBar.Execute;
End;

Procedure TMainForm.MenuItem101Click(Sender: TObject);
Var
  req, args, tt: TJSONObject;
  ids, t: TJSONArray;
  i: Integer;
  TorrentIds: Variant;
  Magnets: TStringList;
Begin
  TorrentIds := GetSelectedTorrents;
  req := TJSONObject.Create;
  args := TJSONObject.Create;
  Magnets := TStringList.Create;
  Try
    req.Add('method', 'torrent-get');
    ids := TJSONArray.Create;
    For i := VarArrayLowBound(TorrentIds, 1) To VarArrayHighBound(TorrentIds, 1) Do
      ids.Add(Integer(TorrentIds[i]));
    args.Add('ids', ids);
    args.Add('fields', TJSONArray.Create(['magnetLink']));
    req.Add('arguments', args);
    args := RpcObj.SendRequest(req);
    If args = nil Then
    Begin
      CheckStatus(False);
      exit;
    End;
    t := TJSONArray.Create;
    t := args.Arrays['torrents'];
    For i := 0 To t.Count - 1 Do
    Begin
      tt := t.Objects[i] As TJSONObject;
      Magnets.add(tt.Strings['magnetLink']);
    End;
    FLastClipboardLink := Magnets.Text;
    // To Avoid TransGUI detect again this existing links
    Clipboard.AsText := Magnets.Text;
  Finally
    args.Free;
    req.Free;
    Magnets.Free;
  End;
End;

Procedure TMainForm.miHomePageClick(Sender: TObject);
Begin
  GoHomePage;
End;

Procedure TMainForm.PageInfoResize(Sender: TObject);
Begin
  If FDetailsWait.Visible Then
    CenterDetailsWait;
End;

Procedure TMainForm.panReconnectResize(Sender: TObject);
Begin
  panReconnectFrame.BoundsRect := panReconnect.ClientRect;
End;

Procedure TMainForm.pbDownloadedPaint(Sender: TObject);
Begin
  If FTorrentProgress <> nil Then
    pbDownloaded.Canvas.StretchDraw(pbDownloaded.ClientRect, FTorrentProgress);
End;

Procedure TMainForm.StatusBarMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  pt: TPoint;
  rb: Boolean;
Begin
  rb := (Button = mbRight) And RpcObj.Connected;
  pt := StatusBar.ClientToScreen(Point(X, Y));
  Case StatusBar.GetPanelIndexAt(X, Y) Of
    0: If Button = mbLeft Then
        acConnOptions.Execute;
    1: If rb Then
        pmDownSpeeds.PopUp(pt.X, pt.Y);
    2: If rb Then
        pmUpSpeeds.PopUp(pt.X, pt.Y);
  End;
End;

{$ifdef LCLcarbon}

Type 
  THackApplication = Class(TApplication)
  End;
{$endif LCLcarbon}

Procedure TMainForm.TickTimerTimer(Sender: TObject);
Var
  i: Integer;
Begin
  TickTimer.Enabled := False;
  Try
    If Not FStarted Then
    Begin
      Application.ProcessMessages;
      FStarted := True;
      acConnect.Execute;
      Application.ProcessMessages;
      //      panTransfer.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;


      //      panGeneralInfo.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;
      //      panTransfer.ChildSizing.Layout:=cclNone;
      //      panGeneralInfo.ChildSizing.Layout:=cclNone;
      With panTransfer Do
        ClientHeight := Controls[ControlCount - 1].BoundsRect.Bottom +
          ChildSizing.TopBottomSpacing;
      With panGeneralInfo Do
        ClientHeight := Controls[ControlCount - 1].BoundsRect.Bottom +
          ChildSizing.TopBottomSpacing;
      panSearch.AutoSize := False;

      If Ini.ReadBool('MainForm', 'FirstRun', True) Then
      Begin
        If Not acResolveCountry.Checked Then
          acResolveCountry.Execute;
        If acResolveCountry.Checked And Not acShowCountryFlag.Checked Then
          acShowCountryFlag.Execute;
        Ini.WriteBool('MainForm', 'FirstRun', False);
      End;

      i := Ini.ReadInteger('Interface', 'LastNewVersionCheck', Trunc(Now));
      If i + Ini.ReadInteger('Interface', 'CheckNewVersionDays', 5) <=
        Trunc(Now) Then
      Begin
        If Ini.ReadBool('Interface', 'AskCheckNewVersion', True) Then
        Begin
          Ini.WriteBool('Interface', 'AskCheckNewVersion', False);
          If Not Ini.ReadBool('Interface', 'CheckNewVersion', False) Then
            If MessageDlg(Format(SCheckNewVersion, [AppName]),
              mtConfirmation, mbYesNo, 0) = mrYes Then
              Ini.WriteBool('Interface', 'CheckNewVersion', True);
        End;
        If Ini.ReadBool('Interface', 'CheckNewVersion', False) Then
          CheckNewVersion;
      End;
    End;

    CheckAddTorrents;

    If RpcObj.Connected Then
      FReconnectTimeOut := 0
    Else
      If panReconnect.Visible Then
        If Now - FReconnectWaitStart >= FReconnectTimeOut / SecsPerDay Then
          DoConnect
        Else
        Begin
          txReconnectSecs.Caption :=
            Format(sReconnect, [FReconnectTimeOut -
            Round(SecsPerDay * (Now - FReconnectWaitStart))]);
          panReconnect.Constraints.MinWidth := 450;
        End;

    If FSlowResponse.Visible Then
    Begin
      If RpcObj.RequestStartTime = 0 Then
        FSlowResponse.Visible := False;
    End
    Else
      If (RpcObj.RequestStartTime <> 0) And
        (Now - RpcObj.RequestStartTime >= 1 / SecsPerDay) Then
        FSlowResponse.Visible := True;

    If FDetailsWait.Visible Then
    Begin
      If (FDetailsWaitStart = 0) Or Not (rtDetails In RpcObj.RefreshNow) Then
      Begin
        FDetailsWaitStart := 0;
        FDetailsWait.Visible := False;
        panDetailsWait.Visible := False;
      End;
    End
    Else
      If (FDetailsWaitStart <> 0) And (Now - FDetailsWaitStart >=
        300 / MSecsPerDay) Then
      Begin
        CenterDetailsWait;
        FDetailsWait.Visible := True;
        panDetailsWait.Visible := True;
        panDetailsWait.BringToFront;
      End;

    {$ifdef LCLcarbon}
    THackApplication(Application).ProcessAsyncCallQueue;
    If Active And (WindowState <> wsMinimized) Then
      Begin
        If Not FFormActive Then
          Begin
            FFormActive := True;
            CheckClipboardLink;
          End;
      End
    Else
      FFormActive := False;
    {$endif LCLcarbon}
  Finally
    TickTimer.Enabled := True;
  End;
End;

Procedure TMainForm.FilterTimerTimer(Sender: TObject);
Begin
  FilterTimer.Enabled := False;
  FFilterChanged := True;
  DoRefresh(True);
End;

Procedure TMainForm.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  {$ifdef CPUARM}
  //  CloseAction:=caMinimize;
  BeforeCloseApp;
  exit;
  {$endif CPUARM}

  If Ini.ReadBool('Interface', 'TrayClose', False) Then
  Begin
    {$ifdef darwin}
      CloseAction := caMinimize;
    {$else}
    {$ifdef linux}
      If IsUnity Then
        CloseAction := caMinimize
      Else
    {$endif linux}
    Begin
      CloseAction := caHide;
      HideApp;
      UpdateTray;
    End;
    {$endif darwin}
    exit;
  End;
  BeforeCloseApp;
End;

Procedure TMainForm.PageInfoChange(Sender: TObject);
Begin
  If PageInfo.ActivePage.Tag <> 0 Then
    FDetailsWaitStart := Now;
  RpcObj.Lock;
  Try
    RpcObj.AdvInfo := GetPageInfoType(PageInfo.ActivePage);
    DoRefresh;
  Finally
    RpcObj.Unlock;
  End;
End;

Procedure TMainForm.tbSearchCancelClick(Sender: TObject);
Begin
  edSearch.Text := '';
End;

Procedure TMainForm.TorrentsListTimerTimer(Sender: TObject);
Begin
  TorrentsListTimer.Enabled := False;
  If RpcObj.CurTorrentId <> 0 Then
    FDetailsWaitStart := Now;
  DoRefresh;
End;

Procedure TMainForm.pmFilesPopup(Sender: TObject);
Begin
  UpdateUI;
End;

Procedure TMainForm.pmTorrentsPopup(Sender: TObject);
Begin
  UpdateUI;
End;

Procedure TMainForm.TrayIconDblClick(Sender: TObject);
//var
//  a:TWindowState;
Begin
  {$ifndef darwin}
  //  acShowApp.Execute;
  If (MainForm.Active = False) Or (MainForm.WindowState = wsMinimized) Then
    MainForm.ShowApp
  Else
    MainForm.HideApp;
  {$endif darwin}
End;

Procedure TMainForm.VSplitterChangeBounds(Sender: TObject);
Begin
  {$ifdef windows}
  Update;
  {$endif windows}
End;

Procedure TMainForm.UrlLabelClick(Sender: TObject);
Begin
  AppBusy;
  OpenURL((Sender As TLabel).Caption);
  AppNormal;
End;

Procedure TMainForm.CenterReconnectWindow;
Begin
  CenterOnParent(panReconnect);
End;

Function TMainForm.DoConnect: Boolean;
Var
  Sec, pwd: String;
  i, j: Integer;
Begin
  Result := True;
  panReconnect.Hide;
  DoDisconnect;
  Sec := 'Connection.' + FCurConn;
  If Not Ini.SectionExists(Sec) Then
    Sec := 'Connection';

  i := FPasswords.IndexOfName(FCurConn);
  pwd := Ini.ReadString(Sec, 'Password', '');
  If pwd = '-' Then
  Begin
    If i >= 0 Then
      pwd := FPasswords.ValueFromIndex[i]
    Else
    Begin
      pwd := '';
      // own dialog for entering a password (****)
      With TPasswordConnect.Create(Self) Do
      Try
        SetText(Format(SConnectTo, [FCurConn]),
          Format(SEnterPassword, [FCurConn]));
        If ShowModal <> mrOk Then
        Begin
          RpcObj.Url := '-';
          Result := False;
          exit;
        End
        Else
        Begin
          pwd := gPassw;
        End;
      Finally
        Free;
      End;
    End;
  End
  Else
    pwd := DecodeBase64(pwd);
  If i >= 0 Then
    FPasswords.Delete(i);

  RpcObj.Http.Sock.SSL.PFXfile := '';
  RpcObj.Http.Sock.SSL.KeyPassword := '';
  If Ini.ReadBool(Sec, 'UseSSL', False) Then
  Begin
    RpcObj.InitSSL;
    RpcObj.Http.Sock.SSL.PFXfile := Ini.ReadString(Sec, 'CertFile', '');
    RpcObj.Http.Sock.SSL.KeyPassword :=
      DecodeBase64(Ini.ReadString(Sec, 'CertPass', ''));
    If Not IsSSLloaded Then
    Begin
      MessageDlg(Format(sSSLLoadError, [DLLSSLName, DLLUtilName]),
        mtError, [mbOK], 0);
      exit;
    End;
    RpcObj.Url := 'https';
  End
  Else
    RpcObj.Url := 'http';
  RpcObj.Http.UserName := Ini.ReadString(Sec, 'UserName', '');
  RpcObj.Http.Password := pwd;
  RpcObj.Http.ProxyHost := '';
  RpcObj.Http.ProxyPort := '';
  RpcObj.Http.ProxyUser := '';
  RpcObj.Http.ProxyPass := '';
  RpcObj.Http.Sock.SocksIP := '';
  RpcObj.Http.Sock.SocksPort := '';
  RpcObj.Http.Sock.SocksUsername := '';
  RpcObj.Http.Sock.SocksPassword := '';
  If Ini.ReadBool(Sec, 'UseProxy', False) Then
  Begin
    If Ini.ReadBool(Sec, 'UseSockProxy', False) Then
    Begin
      RpcObj.Http.Sock.SocksIP := Ini.ReadString(Sec, 'ProxyHost', '');
      RpcObj.Http.Sock.SocksPort :=
        IntToStr(Ini.ReadInteger(Sec, 'ProxyPort', 8080));
      RpcObj.Http.Sock.SocksUsername := Ini.ReadString(Sec, 'ProxyUser', '');
      RpcObj.Http.Sock.SocksPassword :=
        DecodeBase64(Ini.ReadString(Sec, 'ProxyPass', ''));
    End
    Else
    Begin
      RpcObj.Http.ProxyHost := Ini.ReadString(Sec, 'ProxyHost', '');
      RpcObj.Http.ProxyPort :=
        IntToStr(Ini.ReadInteger(Sec, 'ProxyPort', 8080));
      RpcObj.Http.ProxyUser := Ini.ReadString(Sec, 'ProxyUser', '');
      RpcObj.Http.ProxyPass :=
        DecodeBase64(Ini.ReadString(Sec, 'ProxyPass', ''));
    End;
  End;
  If (FReconnectTimeOut = -1) And Ini.ReadBool(Sec, 'Autoreconnect', False) Then
    FReconnectTimeOut := 0;
  RpcObj.RpcPath := Ini.ReadString(Sec, 'RpcPath', '');
  RpcObj.Url := Format('%s://%s:%d', [RpcObj.Url, Ini.ReadString(Sec, 'Host', ''),
    Ini.ReadInteger(Sec, 'Port', 9091)]);
  SetRefreshInterval;
  RpcObj.InfoStatus := sConnectingToDaemon;
  CheckStatus;
  TrayIcon.Hint := RpcObj.InfoStatus;
  RpcObj.Connect;
  FPathMap.Text := StringReplace(Ini.ReadString(Sec, 'PathMap', ''),
    '|', LineEnding, [rfReplaceAll]);
  i := 0;
  While i < FPathMap.Count Do
    If Trim(FPathMap.ValueFromIndex[i]) = '' Then
      FPathMap.Delete(i)
    Else
      Inc(i);

  Ini.WriteString('Hosts', 'CurHost', FCurConn);
  If FCurConn <> Ini.ReadString('Hosts', 'Host1', '') Then
  Begin
    Ini.WriteString('Hosts', 'Host1', FCurConn);
    j := 2;
    For i := 0 To pmConnections.Items.Count - 1 Do
      With pmConnections.Items[i] Do
        If (Tag = 0) And (Caption <> FCurConn) Then
        Begin
          Ini.WriteString('Hosts', Format('Host%d', [j]), Caption);
          Inc(j);
        End;
    Ini.UpdateFile;
    UpdateConnections;
  End
  Else
    If pmConnections.Items[0].Tag = 0 Then
    Begin
      pmConnections.Items[0].Checked := True;
      miConnect.Items[0].Checked := True;
    End;
  tbConnect.Caption := pmConnections.Items[0].Caption;
End;

Procedure TMainForm.DoDisconnect;
Var
  i: Integer;
Begin
  TorrentsListTimer.Enabled := False;
  FilterTimer.Enabled := False;
  ClearDetailsInfo;
  gTorrents.Items.Clear;
  gTorrents.Enabled := False;
  gTorrents.Color := clBtnFace;
  lvPeers.Enabled := False;
  lvPeers.Color := gTorrents.Color;
  lvFiles.Enabled := False;
  lvFiles.Color := gTorrents.Color;
  lvTrackers.Enabled := False;
  lvTrackers.Color := gTorrents.Color;
  If FreeSpacePaths <> nil Then
    FreeSpacePaths.Clear;
  lvFilter.Enabled := False;
  lvFilter.Color := gTorrents.Color;
  With lvFilter Do
  Begin
    Items[0, 0] := UTF8Decode(SAll);
    // ALERT - VERIFY - PETROV
    Items[0, 1] := UTF8Decode(SDownloading);
    Items[0, 2] := UTF8Decode(SCompleted);
    Items[0, 3] := UTF8Decode(SActive);
    Items[0, 4] := UTF8Decode(SInactive);
    Items[0, 5] := UTF8Decode(sStopped);
    Items[0, 6] := UTF8Decode(sErrorState);
    Items[0, 7] := UTF8Decode(sWaiting);
  End;
  edSearch.Enabled := False;
  edSearch.Color := gTorrents.Color;
  edSearch.Text := '';
  //  FLastFilterIndex:=0;
  With gStats Do
  Begin
    BeginUpdate;
    Try
      For i := 0 To Items.Count - 1 Do
      Begin
        Items[1, i] := NULL;
        Items[2, i] := NULL;
      End;
    Finally
      EndUpdate;
    End;
    Enabled := False;
    Color := gTorrents.Color;
  End;

  RpcObj.Disconnect;

  RpcObj.InfoStatus := sDisconnected;
  CheckStatus;
  UpdateUI;
  TrayIcon.Hint := RpcObj.InfoStatus;
  gTorrents.Items.RowCnt := 0;
  FTorrents.RowCnt := 0;
  lvFilter.Row := FLastFilterIndex;
  //  lvFilter.Items.RowCnt:=StatusFiltersCount;
  TorrentsListTimer.Enabled := False;
  FilterTimer.Enabled := False;
  pmConnections.Items[0].Checked := False;
  miConnect.Items[0].Checked := False;
  FCurDownSpeedLimit := -2;
  FCurUpSpeedLimit := -2;
  FillSpeedsMenu;
  tbConnect.Caption := Format(SConnectTo, ['Transmission']);
End;

Procedure TMainForm.ClearDetailsInfo(Skip: TAdvInfoType);

  Procedure ClearChildren(AParent: TPanel);
  Var
    i: Integer;
  Begin
    AParent.AutoSize := False;
    //    AParent.ChildSizing.Layout:=cclNone;
    For i := 0 To AParent.ControlCount - 1 Do
    Begin
      If AParent.Controls[i] Is TLabel Then
        With AParent.Controls[i] As TLabel Do
        Begin
          If (Length(Name) < 5) Or (Copy(Name, Length(Name) - 4, 5) <>
            'Label') Then
            Caption := '';
          PopupMenu := pmLabels;
        End;
    End;
  End;

Var
  i, t: Integer;
Begin
  If RpcObj.CurTorrentId = 0 Then
  Begin
    Skip := aiNone;
    t := 0;
  End
  Else
    t := 1;
  FDetailsWaitStart := 0;
  RebuildTree := False;

  If Skip <> aiFiles Then
  Begin
    FFiles.Clear;
    RebuildTree := True;
    tabFiles.Caption := FFilesCapt;
  End;
  If Skip <> aiPeers Then
    lvPeers.Items.Clear;
  If Skip <> aiTrackers Then
    lvTrackers.Items.Clear;
  If Skip <> aiGeneral Then
  Begin
    ClearChildren(panGeneralInfo);
    ClearChildren(panTransfer);
    ProcessPieces('', 0, 0);
    txDownProgress.AutoSize := False;
    txDownProgress.Caption := '';

    txMagnetLink.Text := '';
  End;
  For i := 0 To PageInfo.PageCount - 1 Do
    PageInfo.Pages[i].Tag := t;
End;

Function TMainForm.SelectRemoteFolder(Const CurFolder, DialogTitle: String): String;
Var
  i, j: Integer;
  s, ss, sss, fn: String;
  dlg: TSelectDirectoryDialog;
  d: Char;
Begin
  Result := '';
  If Trim(FPathMap.Text) = '' Then
  Begin
    MessageDlg(sNoPathMapping, mtInformation, [mbOK], 0);
    exit;
  End;
  s := MapRemoteToLocal(CurFolder);
  If (s = '') Or Not DirectoryExistsUTF8(s) Then
    s := FPathMap.ValueFromIndex[0];

  If Not DirectoryExistsUTF8(s) Then
  Begin
    MessageDlg(sNoPathMapping, mtInformation, [mbOK], 0);
    exit;
  End;

  dlg := TSelectDirectoryDialog.Create(nil);
  Try
    dlg.Title := DialogTitle;
    dlg.InitialDir := s;
    If Not dlg.Execute Then
      exit;

    fn := dlg.FileName;
    For i := 0 To FPathMap.Count - 1 Do
    Begin
      s := FPathMap[i];
      j := Pos('=', s);
      If j > 0 Then
      Begin
        ss := FixSeparators(Copy(s, j + 1, MaxInt));
        sss := IncludeTrailingPathDelimiter(ss);
        If (CompareFilePath(ss, fn) = 0) Or
          (CompareFilePath(sss, Copy(fn, 1, Length(sss))) = 0) Then
        Begin
          Result := Copy(s, 1, j - 1);
          d := '/';
          For j := 1 To Length(Result) Do
            If Result[j] In ['/', '\'] Then
            Begin
              d := Result[j];
              break;
            End;
          If CompareFilePath(ss, fn) <> 0 Then
          Begin
            If (Result <> '') And (Copy(Result, Length(Result), 1) <> d) Then
              Result := Result + d;
            ss := IncludeProperTrailingPathDelimiter(ss);
            Result := Result + Copy(fn, Length(ss) + 1, MaxInt);
          End;

          Result := StringReplace(Result, DirectorySeparator, d,
            [rfReplaceAll]);
          If Copy(Result, Length(Result), 1) = d Then
            SetLength(Result, Length(Result) - 1);
        End;
      End;
    End;
  Finally
    dlg.Free;
  End;
  If Result = '' Then
    MessageDlg(sNoPathMapping, mtError, [mbOK], 0);
End;

Procedure TMainForm.ConnectionSettingsChanged(Const ActiveConnection: String;
  ForceReconnect: Boolean);
Var
  Sec: String;
Begin
  UpdateConnections;
  If (FCurConn <> ActiveConnection) Or ForceReconnect Then
  Begin
    DoDisconnect;
    If (FCurConn <> ActiveConnection) Then lvFilter.Row := 0;
    Sec := 'Connection.' + ActiveConnection;
    If Ini.ReadBool(Sec, 'Autoreconnect', False) Then
      FReconnectTimeOut := 0
    Else
      FReconnectTimeOut := -1;
    FCurConn := ActiveConnection;
    If FCurConn <> '' Then
      DoConnect;
  End;
End;

Procedure TMainForm.UpdateUI;
Var
  e: Boolean;
Begin
  e := ((Screen.ActiveForm = Self) Or Not Visible Or (WindowState = wsMinimized)) And
    Not gTorrents.EditorMode And Not lvFiles.EditorMode;

  acConnect.Enabled := e;
  acOptions.Enabled := e;
  acConnOptions.Enabled := e;
  e := RpcObj.Connected And e;
  acDisconnect.Enabled := e;
  acSelectAll.Enabled := e;
  acAddTorrent.Enabled := e;
  acAddLink.Enabled := e;
  acDaemonOptions.Enabled := e;
  acStartAllTorrents.Enabled := e And RpcObj.Connected;
  acStopAllTorrents.Enabled := acStartAllTorrents.Enabled;
  acStartTorrent.Enabled := e And (gTorrents.Items.Count > 0);
  acForceStartTorrent.Enabled :=
    acStartTorrent.Enabled And (RpcObj.RPCVersion >= 14);
  acStopTorrent.Enabled := e And (gTorrents.Items.Count > 0);
  acStartTorrentFilter.Enabled := e And (gTorrents.Items.Count > 0);
  acFilterTorrentProps.Enabled := e And (gTorrents.Items.Count > 0);
  acForceStartTorrentFilter.Enabled :=
    acStartTorrent.Enabled And (RpcObj.RPCVersion >= 14);
  acStopTorrentFilter.Enabled := e And (gTorrents.Items.Count > 0);
  acVerifyTorrent.Enabled := e And (gTorrents.Items.Count > 0);
  acRemoveTorrent.Enabled :=
    e And (gTorrents.Items.Count > 0) And Not edSearch.Focused;
  acRemoveTorrentAndData.Enabled :=
    acRemoveTorrent.Enabled And (RpcObj.RPCVersion >= 4);
  acReannounceTorrent.Enabled :=
    acVerifyTorrent.Enabled And (RpcObj.RPCVersion >= 5);
  acMoveTorrent.Enabled := acVerifyTorrent.Enabled And (RpcObj.RPCVersion >= 6);
  acSetLabels.Enabled := acVerifyTorrent.Enabled And (RpcObj.RPCVersion >= 16);
  acTorrentProps.Enabled := acVerifyTorrent.Enabled;
  acOpenContainingFolder.Enabled :=
    acTorrentProps.Enabled And (RpcObj.RPCVersion >= 4);
  pmiPriority.Enabled := e And (gTorrents.Items.Count > 0);
  miPriority.Enabled := pmiPriority.Enabled;
  acSetHighPriority.Enabled :=
    e And (gTorrents.Items.Count > 0) And
    ((Not lvFiles.Focused And (RpcObj.RPCVersion >= 5)) Or
    ((lvFiles.Items.Count > 0) And (PageInfo.ActivePage = tabFiles)));
  acSetNormalPriority.Enabled := acSetHighPriority.Enabled;
  acSetLowPriority.Enabled := acSetHighPriority.Enabled;
  miQueue.Enabled := e And (gTorrents.Items.Count > 0) And (RpcObj.RPCVersion >= 14);
  pmiQueue.Enabled := miQueue.Enabled;
  acQMoveTop.Enabled := miQueue.Enabled;
  acQMoveUp.Enabled := miQueue.Enabled;
  acQMoveDown.Enabled := miQueue.Enabled;
  acQMoveBottom.Enabled := miQueue.Enabled;
  acOpenFile.Enabled := acSetHighPriority.Enabled And (lvFiles.SelCount < 2) And
    (RpcObj.RPCVersion >= 4);
  acCopyPath.Enabled := acOpenFile.Enabled;
  acSetNotDownload.Enabled := acSetHighPriority.Enabled;
  acRename.Enabled := (RpcObj.RPCVersion >= 15) And acSetHighPriority.Enabled;
  acSetupColumns.Enabled := e;
  acUpdateBlocklist.Enabled :=
    (acUpdateBlocklist.Tag <> 0) And e And (RpcObj.RPCVersion >= 5);
  acAddTracker.Enabled := acTorrentProps.Enabled And (RpcObj.RPCVersion >= 10);
  acAdvEditTrackers.Enabled := acAddTracker.Enabled;
  acEditTracker.Enabled := acAddTracker.Enabled And (lvTrackers.Items.Count > 0);
  acReplaceTracker.Enabled := acEditTracker.Enabled;
  If lvTrackers.Items.Count > 0 Then
  Begin
    acReplaceTracker.Visible := acAddTracker.Visible And (


      //    (pos('http://',lvTrackers.Items[idxTrackersListName, lvTrackers.Row]) >0) or


      //    (pos('https://',lvTrackers.Items[idxTrackersListName, lvTrackers.Row]) >0));
      (pos('http://plab.site/', lvTrackers.Items[idxTrackersListName,
      lvTrackers.Row]) > 0) Or
      (pos('http://plab.site1/', lvTrackers.Items[idxTrackersListName,
      lvTrackers.Row]) > 0));
  End
  Else
    acReplaceTracker.Visible := False;
  acDelTracker.Enabled := acEditTracker.Enabled;
  acAltSpeed.Enabled := e And (RpcObj.RPCVersion >= 5);
  pmiDownSpeedLimit.Enabled := RpcObj.Connected;
  pmiUpSpeedLimit.Enabled := RpcObj.Connected;
End;

Procedure TMainForm.ShowConnOptions(NewConnection: Boolean);
Var
  frm: TConnOptionsForm;
Begin
  AppBusy;
  frm := TConnOptionsForm.Create(Self);
  With frm Do
  Try
    ActiveConnection := FCurConn;
    If NewConnection Then
    Begin
      Caption := SNewConnection;
      btNewClick(nil);
      If Ini.ReadInteger('Hosts', 'Count', 0) = 0 Then
      Begin
        panTop.Visible := False;
        {$ifdef LCLgtk2}
              panTop.Height := 0;
        {$endif LCLgtk2}
        With Page.BorderSpacing Do
          Top := Left;
        tabPaths.TabVisible := False;
        tabMisc.TabVisible := False;
      End
      Else
      Begin
        btNew.Hide;
        btRename.Hide;
        btDel.Hide;
        panTop.ClientHeight := btNew.Top;
      End;
      cbShowAdvancedClick(nil);
      AutoSizeForm(frm);
    End;
    AppNormal;
    ShowModal;
    ConnectionSettingsChanged(ActiveConnection, ActiveSettingChanged);
  Finally
    Free;
  End;
End;

Procedure TMainForm.SaveColumns(LV: TVarGrid; Const AName: String; FullInfo: Boolean);
Var
  i: Integer;
Begin
  For i := 0 To LV.Columns.Count - 1 Do
    With LV.Columns[i] Do
    Begin
      Ini.WriteInteger(AName, Format('Id%d', [i]), ID - 1);
      Ini.WriteInteger(AName, Format('Width%d', [i]), Width);
      If FullInfo Then
      Begin
        Ini.WriteInteger(AName, Format('Index%d', [i]), Index);
        Ini.WriteBool(AName, Format('Visible%d', [i]), Visible);
      End;
    End;
  If LV.SortColumn >= 0 Then
  Begin
    Ini.WriteInteger(AName, 'SortColumn', LV.SortColumn);
    Ini.WriteInteger(AName, 'SortOrder', Integer(LV.SortOrder));
  End;
End;

Procedure TMainForm.LoadColumns(LV: TVarGrid; Const AName: String; FullInfo: Boolean);
Var
  i, j, ColId: Integer;
Begin
  LV.Columns.BeginUpdate;
  Try
    For i := 0 To LV.Columns.Count - 1 Do
    Begin
      ColId := Ini.ReadInteger(AName, Format('Id%d', [i]), -1);
      If ColId = -1 Then continue;
      For j := 0 To LV.Columns.Count - 1 Do
        With LV.Columns[j] Do
          If ID - 1 = ColId Then
          Begin
            If FullInfo Then
            Begin
              Index := Ini.ReadInteger(AName, Format('Index%d', [i]),
                Index);
              Visible :=
                Ini.ReadBool(AName, Format('Visible%d', [i]), Visible);
            End;
            Width := Ini.ReadInteger(AName, Format('Width%d', [i]), Width);
            break;
          End;
    End;
  Finally
    LV.Columns.EndUpdate;
  End;
  LV.SortColumn := Ini.ReadInteger(AName, 'SortColumn', LV.SortColumn);
  LV.SortOrder := TSortOrder(Ini.ReadInteger(AName, 'SortOrder',
    Integer(LV.SortOrder)));
End;


//----------------------------------------------------------------
Function GetBiDi: TBiDiMode;
Var
  i: Integer;
Begin
  // PETROV - Herb off
  i := Ini.ReadInteger('Interface', 'IgnoreRightLeft', 0);
  // 0 - by default
  Ini.WriteInteger('Interface', 'IgnoreRightLeft', i);

  If (FTranslationLanguage = 'English') And (i = 0) Then
    i := 1;

  Result := bdLeftToRight;
  Case i Of
    1: Result := bdLeftToRight;
    2: Result := bdRightToLeft;
    3: Result := bdRightToLeftNoAlign;
    4: Result := bdRightToLeftReadingOnly;
  End;
End;


//----------------------------------------------------------------
Function ExcludeInvalidChar(path: String): String;
  //var
  //s_old: string;
  // l_old: integer;
Begin
  //  s_old := path;
  //path  := StringReplace(path, ':', '_', [rfReplaceAll, rfIgnoreCase]);
  path := StringReplace(path, '*', '_', [rfReplaceAll, rfIgnoreCase]);
  path := StringReplace(path, '?', '_', [rfReplaceAll, rfIgnoreCase]);
  path := StringReplace(path, '|', '_', [rfReplaceAll, rfIgnoreCase]);
  path := StringReplace(path, '<', '_', [rfReplaceAll, rfIgnoreCase]);
  path := StringReplace(path, '>', '_', [rfReplaceAll, rfIgnoreCase]);
  path := StringReplace(path, '"', '_', [rfReplaceAll, rfIgnoreCase]);
  path := StringReplace(path, '~', '_', [rfReplaceAll, rfIgnoreCase]);
  //path  := StringReplace(path, '..','_', [rfReplaceAll, rfIgnoreCase]); bag

  //  l_old := 0;
  //  if path <> s_old then begin
  //    l_old :=1;
  //  end;

  Result := path;
End;



//----------------------------------------------------------------




Function TMainForm.GetTorrentError(t: TJSONObject; Status: Integer): String;
Var
  i: Integer;
  stats: TJSONArray;
  err, gerr: Utf8string;
  NoTrackerError: Boolean;
Begin
  Result := '';
  gerr := t.Strings['errorString'];
  If RpcObj.RPCVersion >= 7 Then
  Begin
    NoTrackerError := False;
    stats := t.Arrays['trackerStats'];
    For i := 0 To stats.Count - 1 Do
      With stats.Objects[i] Do
      Begin
        err := '';
        If Booleans['hasAnnounced'] And Not
          Booleans['lastAnnounceSucceeded'] Then
          err := Strings['lastAnnounceResult'];
        If err = 'Success' Then
          err := '';
        If err = '' Then
        Begin
          // If at least one tracker is working, then report no error
          NoTrackerError := True;
          Result := '';
        End
        Else
        Begin
          If Not NoTrackerError And (Result = '') Then
            Result := sTrackerError + ': ' + UTF8Encode(err);
          // Workaround for transmission bug


          // If the global error string is equal to some tracker error string,
          // then igonore the global error string
          If gerr = err Then
            gerr := '';
        End;
      End;
  End
  Else
  Begin
    Result := UTF8Encode(t.Strings['announceResponse']);
    If Result = 'Success' Then
      Result := ''
    Else
      If Result <> '' Then
      Begin
        i := Pos('(', Result);
        If i <> 0 Then
          If Copy(Result, i, 5) = '(200)' Then
            Result := ''
          Else
            Result := sTrackerError + ': ' + Copy(Result, 1, i - 1);
      End;
  End;

  If (Result = '') Or (Status = TR_STATUS_STOPPED) Or
    (Status = TR_STATUS_FINISHED) Then
    Result := UTF8Encode(gerr);
End;

Function TMainForm.SecondsToString(j: Integer): String;
Begin
  If j < 60 Then
    Result := Format(sSecs, [j])
  Else
    If j < 60 * 60 Then
    Begin
      Result := Format(sMins, [j Div 60]);
      j := j Mod 60;
      If j > 0 Then
        Result := Format('%s, %s', [Result, Format(sSecs, [j])]);
    End
    Else
    Begin
      j := (j + 30) Div 60;
      If j < 60 * 24 Then
      Begin
        Result := Format(sHours, [j Div 60]);
        j := j Mod 60;
        If j > 0 Then
          Result := Format('%s, %s', [Result, Format(sMins, [j])]);
      End
      Else
      Begin
        j := (j + 30) Div 60;
        Result := Format(sDays, [j Div 24]);
        j := j Mod 24;
        If j > 0 Then
          Result := Format('%s, %s', [Result, Format(sHours, [j])]);
      End;
    End;
End;

Procedure TMainForm.FillTorrentsList(list: TJSONArray);
Var
  i, j, c, p, row, crow, id, StateImg: Integer;
  t: TJSONObject;
  a: TJSONArray;
  f: Double;
  ExistingRow: Boolean;
  s, ss: String;

  Function GetTorrentValue(AIndex: Integer; Const AName: String; AType: Integer):

  Boolean;
  Var
    res: Variant;
    i: Integer;
  Begin
    i := t.IndexOfName(AName);
    Result := i >= 0;
    If Result Then
      Case AType Of
        vtInteger:
          res := t.Items[i].AsInteger;
        vtExtended:
          res := t.Items[i].AsFloat;
        Else
          res := t.Items[i].AsString;
      End
    Else
      res := NULL;

    FTorrents[AIndex, row] := res;
  End;

  Function StoreSpeed(Var History: Variant; Speed: Integer): Integer;
  Var
    j, cnt: Integer;
    p: PInteger;
    IsNew: Boolean;
    res: Int64;
  Begin
    IsNew := VarIsEmpty(History);
    If IsNew Then
    Begin
      If Speed = 0 Then
      Begin
        Result := 0;
        exit;
      End;
      History := VarArrayCreate([0, SpeedHistorySize], varInteger);
    End;
    p := VarArrayLock(History);
    Try
      If IsNew Then
      Begin
        For j := 1 To SpeedHistorySize Do
          p[j] := -1;
        j := 1;
      End
      Else
      Begin
        j := Round((Now - Cardinal(p[0]) / SecsPerDay) / RpcObj.RefreshInterval);
        If j = 0 Then
          j := 1;
      End;
      p[0] := Integer(Cardinal(Round(Now * SecsPerDay)));
      // Shift speed array
      If j < SpeedHistorySize Then
        Move(p[1], p[j + 1], (SpeedHistorySize - j) * SizeOf(Integer))
      Else
        j := SpeedHistorySize;

      While j > 0 Do
      Begin
        p[j] := Speed;
        Dec(j);
      End;
      // Calc average speed
      res := Speed;
      cnt := 1;
      For j := 2 To SpeedHistorySize Do
        If p[j] < 0 Then
          break
        Else
        Begin
          Inc(res, p[j]);
          Inc(cnt);
        End;

      Result := res Div cnt;
    Finally
      VarArrayUnlock(History);
    End;
    If Result = 0 Then
      VarClear(History);
  End;

Var
  FilterIdx, OldId: Integer;
  TrackerFilter, PathFilter, LabelFilter: String;
  UpSpeed, DownSpeed: Double;
  DownCnt, SeedCnt, CompletedCnt, ActiveCnt, StoppedCnt, ErrorCnt,
  WaitingCnt, ft: Integer;
  DownSize, SeedSize, CompletedSize, ActiveSize, StoppedSize, ErrorSize,
  WaitingSize, AllSize: Double;
  IsActive: Boolean;
  Labels, Paths: TStringList;
  alabels: TStringList;
  slabels: TStringList;

  v: Variant;
  w: CountData;
  FieldExists: Array Of Boolean;
  //  req, args, args2: TJSONObject;
Begin
  If gTorrents.Tag <> 0 Then exit;
  If list = nil Then
  Begin
    ClearDetailsInfo;
    exit;
  End;


{
  for i:=1 to 1000 do begin
    t:=TJSONObject.Create;
    t.Integers['id']:=i + 10000;
    t.Strings['name']:=Format('ZName %d', [i]);
    t.Integers['status']:=TR_STATUS_STOPPED;
    t.Arrays['trackerStats']:=TJSONArray.Create;
    t.Floats['sizeWhenDone']:=0;
    t.Floats['leftUntilDone']:=0;
    t.Integers['rateDownload']:=0;
    t.Integers['rateUpload']:=0;
    list.Add(t);
  end;
}
  Paths := TStringList.Create;
  Labels := TStringList.Create;
  Try

    Try

      Paths.Sorted := True;
      Labels.Sorted := True;
      OldId := RpcObj.CurTorrentId;
      IsActive := gTorrents.Enabled;
      gTorrents.Enabled := True;
      lvFilter.Enabled := True;
      gTorrents.Color := clWindow;
      lvFilter.Color := clWindow;
      edSearch.Enabled := True;
      edSearch.Color := clWindow;
      If Not IsActive Then
        ActiveControl := gTorrents;

      For i := 0 To FTrackers.Count - 1 Do
        FTrackers.Objects[i] := nil;

      // Check fields' existence
      SetLength(FieldExists, FTorrents.ColCnt);
      If list.Count > 0 Then
      Begin
        t := list[0] As TJSONObject;
        FieldExists[idxName] := t.IndexOfName('name') >= 0;
        FieldExists[idxRatio] := t.IndexOfName('uploadRatio') >= 0;
        FieldExists[idxTracker] := t.IndexOfName('trackers') >= 0;
        FieldExists[idxPath] := t.IndexOfName('downloadDir') >= 0;
        FieldExists[idxPriority] := t.IndexOfName('bandwidthPriority') >= 0;
        FieldExists[idxQueuePos] := t.IndexOfName('queuePosition') >= 0;
        FieldExists[idxSeedingTime] := t.IndexOfName('secondsSeeding') >= 0;
        FieldExists[idxPrivate] := t.IndexOfName('isPrivate') >= 0;
        FIeldExists[idxLabels] := t.IndexOfName('labels') >= 0;
      End;

      UpSpeed := 0;
      DownSpeed := 0;
      DownCnt := 0;
      SeedCnt := 0;
      CompletedCnt := 0;
      ActiveCnt := 0;
      StoppedCnt := 0;
      ErrorCnt := 0;
      WaitingCnt := 0;
      DownSize := 0;
      SeedSize := 0;
      CompletedSize := 0;
      ActiveSize := 0;
      StoppedSize := 0;
      ErrorSize := 0;
      WaitingSize := 0;
      AllSize := 0;

      FilterIdx := lvFilter.Row;
      If VarIsNull(lvFilter.Items[0, FilterIdx]) Then
        Dec(FilterIdx);
      If FilterIdx >= StatusFiltersCount Then
        If Not VarIsNull(lvFilter.Items[-1, FilterIdx]) Then
        Begin
          ft := Integer(lvFilter.Items[-2, FilterIdx]);
          If ft = 1 Then
            PathFilter := UTF8Encode(
              WideString(lvFilter.Items[-1, FilterIdx]))
          Else
            LabelFilter :=
              UTF8Encode(WideString(lvFilter.Items[-1, FilterIdx]));
          FilterIdx := fltAll;
        End
        Else
        Begin
          TrackerFilter := UTF8Encode(WideString(lvFilter.Items[0, FilterIdx]));
          FilterIdx := fltAll;
          i := RPos('(', TrackerFilter);
          If i > 0 Then
            TrackerFilter := Trim(Copy(TrackerFilter, 1, i - 1));
        End;

      For i := 0 To FTorrents.Count - 1 Do
        FTorrents[idxTag, i] := 0;

      For i := 0 To list.Count - 1 Do
      Begin
        StateImg := -1;

        t := list[i] As TJSONObject;
        id := t.Integers['id'];
        ExistingRow := FTorrents.Find(idxTorrentId, id, row);
        If Not ExistingRow Then
          FTorrents.InsertRow(row);

        //    FTorrents[idxTorrentId, row]:=t.Integers['id'];
        FTorrents[idxTorrentId, row] := id;

        If FieldExists[idxName] Then
          FTorrents[idxName, row] := t.Strings['name'];

        j := t.Integers['status'];
        If ExistingRow And (j = TR_STATUS_SEED) And
          (FTorrents[idxStatus, row] = TR_STATUS_DOWNLOAD) Then
          DownloadFinished(UTF8Encode(WideString(FTorrents[idxName, row])));
        FTorrents[idxStatus, row] := j;
        If j = TR_STATUS_CHECK_WAIT Then StateImg := imgCheckWaiting
        Else
          If j = TR_STATUS_CHECK Then StateImg := imgCheck
          Else
            If j = TR_STATUS_DOWNLOAD_WAIT Then StateImg := imgDownQueue
            Else
              If j = TR_STATUS_DOWNLOAD Then StateImg := imgDown
              Else
                If j = TR_STATUS_SEED_WAIT Then StateImg := imgSeedQueue
                Else
                  If j = TR_STATUS_SEED Then
                    StateImg := imgSeed
                  Else
                    If j = TR_STATUS_STOPPED Then
                      StateImg := imgDone;

        //If GetTorrentError(t, j) <> '' Then
        //  If t.Strings['errorString'] <> '' Then
        //    StateImg := imgError
        //  Else
        //    If StateImg In [imgDown, imgSeed] Then
        //      Inc(StateImg, 2);

        If not (j  in [ TR_STATUS_STOPPED,TR_STATUS_CHECK,TR_STATUS_CHECK_WAIT]) Then
        Begin
          s := GetTorrentError(t, j);
          If s <> '' Then
            If t.Strings['errorString'] <> '' Then
              StateImg := imgError
            Else
              If StateImg In [imgDown, imgSeed] Then
                Inc(StateImg, 2);

          If RpcObj.RPCVersion >= 7 Then
          Begin
            s := '';
            If t.Arrays['trackerStats'].Count > 0 Then
              With t.Arrays['trackerStats'].Objects[0] Do
              Begin
                If Integer(Integers['announceState']) In [2, 3] Then
                  s := sTrackerUpdating
                Else
                  If Booleans['hasAnnounced'] Then
                    If Booleans['lastAnnounceSucceeded'] Then
                      s := sTrackerWorking
                    Else
                      s :=
                        TranslateString(
                        UTF8Encode(Strings['lastAnnounceResult']), True);

                If s = 'Success' Then
                  s := sTrackerWorking;
              End;
          End
          Else
            s := t.Strings['announceResponse'];
        End
        Else
          s := '';
        FTorrents[idxTrackerStatus, row] := UTF8Decode(s);

        If FTorrents[idxStatus, row] = TR_STATUS_CHECK Then
          f := t.Floats['recheckProgress'] * 100.0
        Else
        Begin
          //f := t.Floats['sizeWhenDone'];
          f := t.Floats['percentDone'] * 100;
          //If f <> 0 Then
          //  f := (f - t.Floats['leftUntilDone']) * 100.0 / f;
          //          If StateImg In [imgDone, imgError] Then
          If StateImg In [imgDone] Then
            If (t.Floats['leftUntilDone'] <> 0) Or
              (t.Floats['sizeWhenDone'] = 0) Then
              StateImg := imgStopped
            Else
              FTorrents[idxStatus, row] := TR_STATUS_FINISHED;
        End;
        If f < 0 Then
          f := 0;
        FTorrents[idxDone, row] := Int(f * 10.0) / 10.0;
        FTorrents[idxStateImg, row] := StateImg;
        GetTorrentValue(idxDownSpeed, 'rateDownload', vtInteger);
        j := StoreSpeed(FTorrents.ItemPtrs[idxDownSpeedHistory, row]^,
          FTorrents[idxDownSpeed, row]);
        If FCalcAvg And (StateImg In [imgDown, imgDownError]) Then
          FTorrents[idxDownSpeed, row] := j;
        GetTorrentValue(idxUpSpeed, 'rateUpload', vtInteger);
        j := StoreSpeed(FTorrents.ItemPtrs[idxUpSpeedHistory, row]^,
          FTorrents[idxUpSpeed, row]);
        If FCalcAvg And (StateImg In [imgSeed, imgSeedError]) Then
          FTorrents[idxUpSpeed, row] := j;

        GetTorrentValue(idxSize, 'totalSize', vtExtended);
        GetTorrentValue(idxSizeToDowload, 'sizeWhenDone', vtExtended);
        GetTorrentValue(idxSeeds, 'peersSendingToUs', vtInteger);
        GetTorrentValue(idxPeers, 'peersGettingFromUs', vtInteger);
        GetTorrentValue(idxETA, 'eta', vtInteger);
        v := FTorrents[idxETA, row];
        If Not VarIsNull(v) Then
          If v < 0 Then
            FTorrents[idxETA, row] := MaxInt
          Else
          Begin
            f := FTorrents[idxDownSpeed, row];
            If f > 0 Then
              FTorrents[idxETA, row] := Round(t.Floats['leftUntilDone'] / f);
          End;
        GetTorrentValue(idxDownloaded, 'downloadedEver', vtExtended);
        GetTorrentValue(idxUploaded, 'uploadedEver', vtExtended);
        GetTorrentValue(idxSizeLeft, 'leftUntilDone', vtExtended);
        GetTorrentValue(idxAddedOn, 'addedDate', vtExtended);
        GetTorrentValue(idxCompletedOn, 'doneDate', vtExtended);
        GetTorrentValue(idxLastActive, 'activityDate', vtExtended);

        If RpcObj.RPCVersion >= 7 Then
        Begin
          If t.Arrays['trackerStats'].Count > 0 Then
            With t.Arrays['trackerStats'].Objects[0] Do
            Begin
              FTorrents[idxSeedsTotal, row] := Integers['seederCount'];
              FTorrents[idxLeechersTotal, row] :=
                Integers['leecherCount'];
            End
          Else
          Begin
            FTorrents[idxSeedsTotal, row] := -1;
            FTorrents[idxLeechersTotal, row] := -1;
          End;
          If t.Floats['metadataPercentComplete'] <> 1.0 Then
          Begin
            FTorrents[idxSize, row] := -1;
            FTorrents[idxSizeToDowload, row] := -1;
          End;
        End
        Else
        Begin
          GetTorrentValue(idxSeedsTotal, 'seeders', vtInteger);
          GetTorrentValue(idxLeechersTotal, 'leechers', vtInteger);
        End;
        If FieldExists[idxRatio] Then
        Begin
          f := t.Floats['uploadRatio'];
          If f = -2 Then
            f := MaxInt;
          FTorrents[idxRatio, row] := f;
        End
        Else
          FTorrents[idxRatio, row] := NULL;
        If FieldExists[idxSeedingTime] Then
          FTorrents[idxSeedingTime, row] := t.Integers['secondsSeeding']
        Else
          FTorrents[idxSeedingTime, row] := NULL;

        If RpcObj.RPCVersion >= 7 Then
        Begin
          If t.Arrays['trackerStats'].Count > 0 Then
            s := t.Arrays['trackerStats'].Objects[0].Strings['announce']
          Else
            s := sNoTracker;
        End
        Else
          If FieldExists[idxTracker] Then
            s := UTF8Encode(t.Arrays['trackers'].Objects[
              0].Strings['announce'])
          Else
          Begin
            s := '';
            If VarIsEmpty(FTorrents[idxTracker, row]) Then
              RpcObj.RequestFullInfo := True;
          End;

        If s <> '' Then
        Begin
          j := Pos('://', s);
          If j > 0 Then
            s := Copy(s, j + 3, MaxInt);
          j := Pos('/', s);
          If j > 0 Then
            s := Copy(s, 1, j - 1);
          j := Pos('.', s);
          If j > 0 Then
          Begin
            ss := Copy(s, 1, j - 1);
            If AnsiCompareText(ss, 'bt') = 0 Then
              System.Delete(s, 1, 3)
            Else
              If (Length(ss) = 3) And
                (AnsiCompareText(Copy(ss, 1, 2), 'bt') = 0) And
                (ss[3] In ['1'..'9']) Then
                System.Delete(s, 1, 4);
          End;
          j := Pos(':', s);
          If j > 0 Then
            System.Delete(s, j, MaxInt);
          FTorrents[idxTracker, row] := UTF8Decode(s);
        End;

        If FieldExists[idxPath] Then
          FTorrents[idxPath, row] :=
            UTF8Decode(ExcludeTrailingPathDelimiter(
            UTF8Encode(t.Strings['downloadDir'])))
        Else
          If VarIsEmpty(FTorrents[idxPath, row]) Then
            RpcObj.RequestFullInfo := True;

        If Not VarIsEmpty(FTorrents[idxPath, row]) Then
        Begin
          s := UTF8Encode(WideString(FTorrents[idxPath, row]));
          j := Paths.IndexOf(s);
          If j < 0 Then
          Begin
            w := CountData.Create;
            w.Count := 1;
            w.Size := FTorrents[idxSizeToDowload, row];
            Paths.AddObject(s, TObject(w));
          End
          Else
          Begin
            w := CountData(Paths.Objects[j]);
            If w <> nil Then
            Begin
              w.Count := w.Count + 1;
              //w.Size := w.Size + t.floats['totalSize'];
              w.Size := w.Size + FTorrents[idxSizeToDowload, row];
              Paths.Objects[j] := TObject(w);
            End
            Else
            Begin
              w := CountData.Create;
              w.Count := 1;
              w.Size := FTorrents[idxSizeToDowload, row];
              Paths.AddObject(s, TObject(w));
            End;
          End;

        End;

        If FieldExists[idxPriority] Then
          FTorrents[idxPriority, row] := t.Integers['bandwidthPriority'];

        If FieldExists[idxQueuePos] Then
        Begin
          j := t.Integers['queuePosition'];
          If FTorrents[idxStatus, row] = TR_STATUS_FINISHED Then
            Inc(j, FinishedQueue);
          FTorrents[idxQueuePos, row] := j;
        End;

        If FieldExists[idxPrivate] Then
          FTorrents[idxPrivate, row] := t.Integers['isPrivate'];

        If FieldExists[idxLabels] Then
        Begin
          a := t.Arrays['labels'];
          s := '';
          alabels := TStringList.Create;
          For j := 0 To a.Count - 1 Do
          Begin
            ss := UTF8Encode(WideString(a.Strings[j]));
            alabels.Add(ss);
            If j > 0 Then s := s + ', ';
            s := s + ss;
            p := Labels.IndexOf(ss);
            If p < 0 Then
            Begin
              w := CountData.Create;
              w.Count := 1;
              w.Size := FTorrents[idxSizeToDowload, row];
              Labels.AddObject(ss, TObject(w));
            End
            Else
            Begin
              w := CountData(Labels.Objects[p]);
              If w <> nil Then
              Begin
                w.Count := w.Count + 1;
                w.Size := w.Size + FTorrents[idxSizeToDowload, row];
                Labels.Objects[p] := TObject(w);
              End
              Else
              Begin
                w := CountData.Create;
                w.Count := 1;
                w.Size := FTorrents[idxSizeToDowload, row];
                Labels.AddObject(ss, TObject(w));
              End;
            End;
          End;
          If a.Count = 0 Then
          Begin
            ss := 'Not Set';
            p := Labels.IndexOf(ss);
            If p < 0 Then
            Begin
              w := CountData.Create;
              w.Count := 1;
              w.Size := FTorrents[idxSizeToDowload, row];
              Labels.AddObject(ss, TObject(w));
            End
            Else
            Begin
              w := CountData(Labels.Objects[p]);
              If w <> nil Then
              Begin
                w.Count := w.Count + 1;
                w.Size := w.Size + FTorrents[idxSizeToDowload, row];
                Labels.Objects[p] := TObject(w);
              End
              Else
              Begin
                w := CountData.Create;
                w.Count := 1;
                w.Size := FTorrents[idxSizeToDowload, row];
                Labels.AddObject(ss, TObject(w));
              End;
            End;
          End;
          alabels.Sort;
          s := '';
          For j := 0 To alabels.Count - 1 Do
          Begin
            ss := alabels[j];
            If j > 0 Then s := s + ', ';
            s := s + ss;
          End;
          alabels.Free;
          FTorrents[idxLabels, row] := s;
        End;

        DownSpeed := DownSpeed + FTorrents[idxDownSpeed, row];
        UpSpeed := UpSpeed + FTorrents[idxUpSpeed, row];

        FTorrents[idxTag, row] := 1;
      End;
    Except
      OutputDebugString(LPCSTR(Exception(ExceptObject).Message +
        '(FillTorrentsList 1)'));
    End;
    Try

      i := 0;
      While i < FTorrents.Count Do
        If FTorrents[idxTag, i] = 0 Then
          FTorrents.Delete(i)
        Else
          Inc(i);

      gTorrents.Items.BeginUpdate;
      Try
        i := gTorrents.DefaultRowHeight;
        For i := 0 To gTorrents.Items.Count - 1 Do
          gTorrents.Items[idxTag, i] := 0;

        gTorrents.Items.Sort(idxTorrentId);

        For i := 0 To FTorrents.Count - 1 Do
        Begin
          IsActive := (FTorrents[idxDownSpeed, i] <> 0) Or
            (FTorrents[idxUpSpeed, i] <> 0);
          If IsActive Then
          Begin
            Inc(ActiveCnt);
            //ActiveSize := ActiveSize + FTorrents[idxSize, i];
            ActiveSize := ActiveSize + FTorrents[idxSizeToDowload, i];
          End;

          j := FTorrents[idxStatus, i];
          //AllSize := AllSize + FTorrents[idxSize, i];
          AllSize := AllSize + FTorrents[idxSizeToDowload, i];
          If j = TR_STATUS_DOWNLOAD Then
          Begin
            Inc(DownCnt);
            //DownSize := DownSize + FTorrents[idxSize, i];
            DownSize := DownSize + FTorrents[idxSizeToDowload, i];
          End
          Else
            If j = TR_STATUS_SEED Then
            Begin
              Inc(SeedCnt);
              //SeedSize := SeedSize + FTorrents[idxSize, i];
              SeedSize := SeedSize + FTorrents[idxSizeToDowload, i];
              Inc(CompletedCnt);
              //CompletedSize := CompletedSize + FTorrents[idxSize, i];
              CompletedSize := CompletedSize + FTorrents[idxSizeToDowload, i];
            End
            Else
              If j = TR_STATUS_FINISHED Then
              Begin
                Inc(CompletedCnt);
                //CompletedSize := CompletedSize + FTorrents[idxSize, i];
                CompletedSize := CompletedSize + FTorrents[idxSizeToDowload, i];
              End;

          If (j = TR_STATUS_CHECK) Or (j = TR_STATUS_CHECK_WAIT) Or
            (j = TR_STATUS_DOWNLOAD_WAIT) Then
          Begin
            Inc(WaitingCnt);
            //WaitingSize := WaitingSize + FTorrents[idxSize, i];
            WaitingSize := WaitingSize + FTorrents[idxSizeToDowload, i];

          End;

          StateImg := FTorrents[idxStateImg, i];
          If StateImg In [imgStopped, imgDone] Then
          Begin
            Inc(StoppedCnt);
            //StoppedSize := StoppedSize + FTorrents[idxSize, i];
            StoppedSize := StoppedSize + FTorrents[idxSizeToDowload, i];

          End
          Else
            If StateImg In [imgDownError, imgSeedError, imgError] Then
            Begin
              Inc(ErrorCnt);
              //ErrorSize := ErrorSize + FTorrents[idxSize, i];
              ErrorSize := ErrorSize + FTorrents[idxSizeToDowload, i];
            End;

          If Not VarIsEmpty(FTorrents[idxTracker, i]) Then
          Begin
            s := UTF8Encode(WideString(FTorrents[idxTracker, i]));
            j := FTrackers.IndexOf(s);
            If j < 0 Then
              j := FTrackers.Add(s);
            FTrackers.Objects[j] := TObject(ptruint(FTrackers.Objects[j]) + 1);
            If (TrackerFilter <> '') And (TrackerFilter <> s) Then
              continue;
          End;

          If (PathFilter <> '') And Not VarIsEmpty(FTorrents[idxPath, i]) And
            (UTF8Decode(PathFilter) <> FTorrents[idxPath, i]) Then
            continue;

          If (LabelFilter = 'Not Set') And Not VarIsEmpty(FTorrents[idxLabels, i])
          Then
          Begin
            If FTorrents[idxLabels, i] <> '' Then
              continue;
          End;
          If (LabelFilter <> '') And (LabelFilter <> 'Not Set') And
            Not VarIsEmpty(FTorrents[idxLabels, i]) Then
          Begin
            slabels := TStringList.Create();
            SplitRegExpr(',', String(FTorrents[idxLabels, i]), slabels);
            slabels.Sort;
            alabels := TStringList.Create();
            For s In slabels Do
            Begin
              If trim(s) <> '' Then
                alabels.Add(trim(s));
            End;

            If alabels.indexof(LabelFilter) = -1 Then
            Begin
              alabels.Free;
              slabels.Free;
              continue;
            End;
            alabels.Free;
            slabels.Free;
          End;

          Case FilterIdx Of
            fltActive:
              If Not IsActive Then
                continue;
            fltInactive:
              If (IsActive = True) Or
                ((StateImg In [imgStopped, imgDone]) = True) Then // PETROV
                continue;
            fltDown:
              If FTorrents[idxStatus, i] <> TR_STATUS_DOWNLOAD Then
                continue;
            fltDone:
              If (Not (StateImg In [imgError, imgDone])) And
                ((FTorrents[idxStatus, i] <> TR_STATUS_SEED) And
                (FTorrents[idxStatus, i] <> TR_STATUS_FINISHED)) Then
                continue;
            fltStopped:
              If Not (StateImg In [imgStopped, imgDone]) Then
                continue;
            fltError:
              If Not (StateImg In [imgDownError, imgSeedError, imgError])
              Then
                continue;
            fltWaiting:
              If (FTorrents[idxStatus, i] <> TR_STATUS_CHECK) And
                (FTorrents[idxStatus, i] <> TR_STATUS_CHECK_WAIT) And
                (FTorrents[idxStatus, i] <> TR_STATUS_DOWNLOAD_WAIT) Then
                continue;
          End;

          If edSearch.Text <> '' Then
            If UTF8Pos(UTF8UpperCase(edSearch.Text),
              UTF8UpperCase(UTF8Encode(WideString(FTorrents[idxName, i])))) = 0 Then
              continue;

          If Not gTorrents.Items.Find(idxTorrentId, FTorrents[idxTorrentId, i],
            row) Then
            gTorrents.Items.InsertRow(row);
          For j := -TorrentsExtraColumns To FTorrents.ColCnt - 1 Do
            If (j <> idxDownSpeedHistory) And (j <> idxUpSpeedHistory) Then
              gTorrents.Items[j, row] := FTorrents[j, i];
          gTorrents.Items[idxTag, row] := 1;
        End;

        i := 0;
        While i < gTorrents.Items.Count Do
          If gTorrents.Items[idxTag, i] = 0 Then
            gTorrents.Items.Delete(i)
          Else
            Inc(i);

        gTorrents.Sort;
        If gTorrents.Items.Count > 0 Then
        Begin
          If OldId <> 0 Then
          Begin
            i := gTorrents.Items.IndexOf(idxTorrentId, OldId);
            If i >= 0 Then
              gTorrents.Row := i
            Else
              If FFilterChanged Then
                OldId := 0;
          End;
          If OldId = 0 Then
            gTorrents.Row := 0;
        End;
        FFilterChanged := False;
      Finally
        gTorrents.Items.EndUpdate;
      End;
      gTorrentsClick(nil);
    Except
      OutputDebugString(LPCSTR(Exception(ExceptObject).Message +
        '(FillTorrentsList 2)'));
    End;
    Try
      crow := -1;
      lvFilter.Items.BeginUpdate;
      Application.ProcessMessages;
      AppBusy;
      Try
        lvFilter.Items[0, 0] :=
          UTF8Decode(Format('%s (%d) (%s)', [SAll, list.Count,
          Format(sTotalSize, [GetHumanSize(AllSize, 0, '?')])]));
        lvFilter.Items[0, 1] :=
          UTF8Decode(Format('%s (%d) (%s)', [SDownloading, DownCnt,
          Format(sTotalSize, [GetHumanSize(DownSize, 0, '?')])]));
        lvFilter.Items[0, 2] :=
          UTF8Decode(Format('%s (%d) (%s)', [SCompleted, CompletedCnt,
          Format(sTotalSize, [GetHumanSize(CompletedSize, 0, '?')])]));
        lvFilter.Items[0, 3] :=
          UTF8Decode(Format('%s (%d) (%s)', [SActive, ActiveCnt,
          Format(sTotalSize, [GetHumanSize(ActiveSize, 0, '?')])]));
        lvFilter.Items[0, 4] :=
          UTF8Decode(Format('%s (%d) (%s)', [SInactive, FTorrents.Count -
          ActiveCnt - StoppedCnt, Format(sTotalSize,
          [GetHumanSize(AllSize - ActiveSize - StoppedSize, 0, '?')])]));
        lvFilter.Items[0, 5] :=
          UTF8Decode(Format('%s (%d) (%s)', [sStopped, StoppedCnt,
          Format(sTotalSize, [GetHumanSize(StoppedSize, 0, '?')])]));
        lvFilter.Items[0, 6] :=
          UTF8Decode(Format('%s (%d) (%s)', [sErrorState, ErrorCnt,
          Format(sTotalSize, [GetHumanSize(ErrorSize, 0, '?')])]));
        lvFilter.Items[0, 7] :=
          UTF8Decode(Format('%s (%d) (%s)', [sWaiting, WaitingCnt,
          Format(sTotalSize, [GetHumanSize(WaitingSize, 0, '?')])]));

        j := StatusFiltersCount;

        If acFolderGrouping.Checked Then
        Begin
          lvFilter.Items[0, j] := NULL;
          Inc(j);
          If FreeSpacePaths = nil Then
            FreeSpacePaths := TStringMap.Create;
          If FreeSpacePaths.Count <> Paths.Count Then
          Begin
            FreeSpacePaths.Clear;
          End;
          c := 0;
          For i := 0 To Paths.Count - 1 Do
          Begin
            If FreeSpacePaths.IndexOf(Paths[i]) >= 0 Then
              c := c + 1;
          End;
          If c <> Paths.Count Then
          Begin
            FreeSpacePaths.Clear;
            For i := 0 To Paths.Count - 1 Do
              FreeSpacePaths.Add(Paths[i], '');
          End;
          For i := 0 To Paths.Count - 1 Do
          Begin
            s := ExtractFileName(Paths[i]);
            For row := StatusFiltersCount + 1 To j - 1 Do
              If ExtractFileName(
                UTF8Encode(WideString(lvFilter.Items[-1, row]))) = s Then
              Begin
                s := Paths[i];
                w := CountData(Paths.Objects[row - StatusFiltersCount - 1]);
                lvFilter.Items[0, row] :=
                  UTF8Decode(Format('%s (%d) (%s) %s',
                  [UTF8Encode(WideString(lvFilter.Items[-1, row])),
                  w.Count, Format(sTotalSize,
                  [GetHumanSize(w.Size, 0, '?')]),
                  FreeSpacePaths[Paths[row -
                  StatusFiltersCount - 1]]]));
              End;
            w := CountData(Paths.Objects[i]);
            lvFilter.Items[0, j] :=
              UTF8Decode(Format('%s (%d) (%s) %s', [s,
              w.Count, Format(sTotalSize, [GetHumanSize(w.Size, 0, '?')]),
              FreeSpacePaths[Paths[i]]]));
            lvFilter.Items[-1, j] := UTF8Decode(Paths[i]);
            lvFilter.Items[-2, j] := 1;
            If Paths[i] = PathFilter Then
              crow := j;
            Inc(j);
          End;

        End;
        AppNormal;

        If acLabelGrouping.Checked Then
        Begin
          lvFilter.Items[0, j] := NULL;
          Inc(j);

          For i := 0 To Labels.Count - 1 Do
          Begin
            w := CountData(Labels.Objects[i]);

            lvFilter.Items[0, j] :=
              UTF8Decode(Format('%s (%d) (%s)',
              [Labels[i], w.Count, Format(sTotalSize,
              [GetHumanSize(w.Size, 0, '?')])]));
            lvFilter.Items[-1, j] := UTF8Decode(Labels[i]);
            lvFilter.Items[-2, j] := 2;
            If Labels[i] = LabelFilter Then
              crow := j;
            Inc(j);
          End;

        End;

        row := j;

        If acTrackerGrouping.Checked Then
        Begin
          If Not VarIsNull(lvFilter.Items[0, row - 1]) Then
          Begin
            lvFilter.Items[0, row] := NULL;
            Inc(row);
          End;

          i := 0;
          While i < FTrackers.Count Do
          Begin
            j := ptruint(FTrackers.Objects[i]);
            If j > 0 Then
            Begin
              lvFilter.Items[0, row] :=
                UTF8Decode(Format('%s (%d)', [FTrackers[i], j]));
              lvFilter.Items[-1, row] := NULL;
              lvFilter.Items[-2, row] := 3;
              If FTrackers[i] = TrackerFilter Then
                crow := row;
              Inc(i);
              Inc(row);
            End
            Else
              FTrackers.Delete(i);
          End;
        End;

        lvFilter.Items.RowCnt := row;
      Finally
        lvFilter.Items.EndUpdate;
      End;
    Except
      OutputDebugString(LPCSTR(Exception(ExceptObject).Message +
        '(FillTorrentsList 3)'));
    End;
    Try
      If crow >= 0 Then
        lvFilter.Row := crow
      Else
        If lvFilter.Row >= StatusFiltersCount Then
          lvFilterClick(nil);

      CheckStatus(True);

      s := GetHumanSize(FCurDownSpeedLimit * 1024, 2, '');
      If s = '' Then s := Format(SUnlimited, [])
      Else
        s := s + Format(sPerSecond, []);
      ss := GetHumanSize(FCurUpSpeedLimit * 1024, 2, '');
      If ss = '' Then ss := Format(SUnlimited, [])
      Else
        ss := ss + Format(sPerSecond, []);
      StatusBar.Panels[1].Text :=
        Format(sDownSpeed, [GetHumanSize(DownSpeed, 1)]) + ' (' + s + ')';
      StatusBar.Panels[2].Text :=
        Format(sUpSpeed, [GetHumanSize(UpSpeed, 1)]) + ' (' + ss + ')';

      {$ifndef LCLcarbon}
      // There is memory leak in TTrayIcon implementation for Mac.
      // Disable tray icon update for Mac.
      TrayIcon.Hint := Format(sDownloadingSeeding,
        [RpcObj.InfoStatus, LineEnding, DownCnt, SeedCnt, LineEnding,
        StatusBar.Panels[1].Text, StatusBar.Panels[2].Text]);
      {$endif LCLcarbon}
    Except
      OutputDebugString(LPCSTR(Exception(ExceptObject).Message +
        '(FillTorrentsList 4)'));
    End;

  Finally


    For I := 0 To Labels.Count - 1 Do
      If Labels.Objects[I] <> nil Then
        CountData(Labels.Objects[I]).Free;

    Labels.Free;
    For I := 0 To Paths.Count - 1 Do
      If Paths.Objects[I] <> nil Then
        CountData(Paths.Objects[I]).Free;

    Paths.Free;
  End;
  DetailsUpdated;
End;

Procedure TMainForm.FillPeersList(list: TJSONArray);
Var
  i, j, row: Integer;
  port: Integer;
  d: TJSONData;
  p: TJSONObject;
  ip, s: String;
  hostinfo: PHostEntry;
  opt: TResolverOptions;
  WasEmpty: Boolean;
Begin
  If list = nil Then
  Begin
    ClearDetailsInfo;
    exit;
  End;
  WasEmpty := lvPeers.Items.Count = 0;
  lvPeers.Items.BeginUpdate;
  Try
    lvPeers.Enabled := True;
    lvPeers.Color := clWindow;
    If FResolver = nil Then
    Begin
      opt := [];
      If acResolveHost.Checked Then
        Include(opt, roResolveIP);
      If acResolveCountry.Checked Then
        Include(opt, roResolveCountry);
      If opt <> [] Then
        FResolver := TIpResolver.Create(GetGeoIpDatabase, opt);
    End;

    For i := 0 To lvPeers.Items.Count - 1 Do
      lvPeers.Items[idxPeerTag, i] := 0;

    lvPeers.Items.Sort(idxPeerIP);
    For i := 0 To list.Count - 1 Do
    Begin
      d := list[i];
      If Not (d Is TJSONObject) Then continue;
      p := d As TJSONObject;
      ip := p.Strings['address'];
      If p.IndexOfName('port') >= 0 Then
        port := p.Integers['port']
      Else
        port := 0;

      s := ip + ':' + IntToStr(port);
      If Not lvPeers.Items.Find(idxPeerIP, s, row) Then
        lvPeers.Items.InsertRow(row);
      lvPeers.Items[idxPeerIP, row] := s;
      lvPeers.Items[idxPeerPort, row] := port;

      If FResolver <> nil Then
        hostinfo := FResolver.Resolve(ip)
      Else
        hostinfo := nil;
      If hostinfo <> nil Then
        lvPeers.Items[idxPeerHost, row] := hostinfo^.HostName
      Else
        lvPeers.Items[idxPeerHost, row] := ip;

      If hostinfo <> nil Then
        lvPeers.Items[idxPeerCountry, row] := hostinfo^.CountryName
      Else
        lvPeers.Items[idxPeerCountry, row] := '';

      If acShowCountryFlag.Checked And (hostinfo <> nil) Then
      Begin
        If hostinfo^.ImageIndex = 0 Then
          hostinfo^.ImageIndex := GetFlagImage(hostinfo^.CountryCode);
        j := hostinfo^.ImageIndex;
      End
      Else
        j := 0;
      lvPeers.Items[idxPeerCountryImage, row] := j;
      lvPeers.Items[idxPeerClient, row] := p.Strings['clientName'];
      lvPeers.Items[idxPeerFlags, row] := p.Strings['flagStr'];
      lvPeers.Items[idxPeerDone, row] := p.Floats['progress'];

      If p.IndexOfName('rateToClient') >= 0 Then
        lvPeers.Items[idxPeerDownSpeed, row] := p.Integers['rateToClient'];
      If p.IndexOfName('rateToPeer') >= 0 Then
        lvPeers.Items[idxPeerUpSpeed, row] := p.Integers['rateToPeer'];

      lvPeers.Items[idxPeerTag, row] := 1;
    End;

    i := 0;
    While i < lvPeers.Items.Count Do
      If lvPeers.Items[idxPeerTag, i] = 0 Then
        lvPeers.Items.Delete(i)
      Else
        Inc(i);
    lvPeers.Sort;
    If WasEmpty And (lvPeers.Items.Count > 0) Then
      lvPeers.Row := 0;
  Finally
    lvPeers.Items.EndUpdate;
  End;
  DetailsUpdated;
End;

Function TMainForm.GetFilesCommonPath(files: TJSONArray): String;
Var
  i: Integer;
  d: TJSONData;
  f: TJSONObject;
  s: String;
Begin
  Result := '';
  For i := 0 To files.Count - 1 Do
  Begin
    d := files[i];
    If Not (d Is TJSONObject) Then continue;
    f := d As TJSONObject;
    s := UTF8Encode(f.Strings['name']);
    If i = 0 Then
      Result := ExtractFilePath(s)
    Else
    Begin
      While True Do
      Begin
        If Result = '' Then
          exit;
        If Copy(s, 1, Length(Result)) <> Result Then
        Begin
          SetLength(Result, Length(Result) - 1);
          Result := ExtractFilePath(Result);
        End
        Else
          break;
      End;
    End;
  End;
End;

Procedure TMainForm.InternalRemoveTorrent(Const Msg, MsgMulti: String;
  RemoveLocalData: Boolean);
Var
  args: TJSONObject;
  ids: Variant;
  s: String;
  i, j, id: Integer;
Begin
  If gTorrents.Items.Count = 0 Then exit;
  gTorrents.Tag := 1;
  Try
    gTorrents.EnsureSelectionVisible;
    ids := GetSelectedTorrents;
    If gTorrents.SelCount < 2 Then
      s := Format(Msg, [UTF8Encode(
        WideString(gTorrents.Items[idxName, gTorrents.Items.IndexOf(
        idxTorrentId, ids[0])]))])
    Else
      s := Format(MsgMulti, [gTorrents.SelCount]);

    s := TranslateString(s, True);
    If MessageDlg('', s, mtConfirmation, mbYesNo, 0, mbNo) <> mrYes Then exit;
  Finally
    gTorrents.Tag := 0;
  End;
  args := TJSONObject.Create;
  If RemoveLocalData Then
    args.Add('delete-local-data', TJSONIntegerNumber.Create(1));

  If TorrentAction(ids, 'torrent-remove', args) Then
  Begin
    With gTorrents Do
    Begin
      BeginUpdate;
      Try
        i := 0;
        While i < Items.Count Do
        Begin
          id := Items[idxTorrentId, i];
          For j := 0 To VarArrayHighBound(ids, 1) Do
            If id = ids[j] Then
            Begin
              Items.Items[idxDeleted, i] := 1;
              break;
            End;
          Inc(i);
        End;
      Finally
        EndUpdate;
      End;
    End;

    If RemoveLocalData Then
      RpcObj.RefreshNow := RpcObj.RefreshNow + [rtSession];
  End;
End;

Function TMainForm.IncludeProperTrailingPathDelimiter(Const s: String): String;
Var
  i: Integer;
  d: Char;
Begin
  Result := s;
  If Result = '' Then exit;
  d := '/';
  For i := 1 To Length(Result) Do
    If Result[i] In ['/', '\'] Then
    Begin
      d := Result[i];
      break;
    End;

  If Result[Length(Result)] <> d Then
    Result := Result + d;
End;

Procedure TMainForm.FillFilesList(ATorrentId: Integer;
  list, priorities, wanted: TJSONArray; Const DownloadDir: WideString);
Var
  save, i: Integer;
Begin
  If lvFiles.Tag <> 0 Then exit;
  If (list = nil) Or (priorities = nil) Or (wanted = nil) Then
  Begin
    ClearDetailsInfo;
    exit;
  End;

  If FFilesTree.IsFolder(lvFiles.Row) Then
    save := -1
  Else
    save := Integer(lvFiles.Items[idxFileId, lvFiles.Row]);
  lvFiles.Enabled := True;
  lvFiles.Color := clWindow;
  FFilesTree.DownloadDir := UTF8Encode(DownloadDir);

  FFilesTree.FillTree(ATorrentId, list, priorities, wanted, RebuildTree);
  RebuildTree := False;
  tabFiles.Caption := Format('%s (%d)', [FFilesCapt, list.Count]);
  For i := 0 To lvFiles.Items.Count - 1 Do
  Begin
    If Integer(lvFiles.Items[idxFileId, i]) = save Then
    Begin
      lvFiles.Row := i;
      break;
    End;
  End;
  DetailsUpdated;

  //  lvFiles.Row:=save;

End;

Procedure TMainForm.FillGeneralInfo(t: TJSONObject);
Var
  i, j, idx: Integer;
  s: String;
  tr: String;
  f: Double;
  alabels: TStringList;
  ja: TJSONArray;
Begin
  If (gTorrents.Items.Count = 0) Or (t = nil) Then
  Begin
    ClearDetailsInfo;
    exit;
  End;
  idx := gTorrents.Items.IndexOf(idxTorrentId, t.Integers['id']);
  If idx = -1 Then
  Begin
    ClearDetailsInfo;
    exit;
  End;

  txDownProgress.Caption := Format('%.1f%%', [Double(gTorrents.Items[idxDone, idx])]);
  txDownProgress.AutoSize := True;
  If RpcObj.RPCVersion >= 5 Then
    s := t.Strings['pieces']
  Else
    s := '';
  ProcessPieces(s, t.Integers['pieceCount'], gTorrents.Items[idxDone, idx]);

  //  panTransfer.ChildSizing.Layout:=cclNone;
  txStatus.Caption := GetTorrentStatus(idx);
  tr := GetTorrentError(t, gTorrents.Items[idxStatus, idx]);
  If tr <> '' Then
  Begin
    If t.Strings['errorString'] <> '' Then
      tr := t.Strings['errorString'];
  End;


  txError.Constraints.MinWidth := 400;
  If Ini.ReadBool('Translation', 'TranslateMsg', True) Then
    txError.Caption := TranslateString(tr, True)
  Else
    txError.Caption := tr;

  i := t.Integers['eta'];
  f := gTorrents.Items[idxDownSpeed, idx];
  If f > 0 Then
    i := Round(t.Floats['leftUntilDone'] / f);
  //txRemaining.Caption:=EtaToString(i);
  txRemaining.Caption := EtaToString(i) + ' (' + GetHumanSize(
    t.Floats['leftUntilDone']) + ')';
  txDownloaded.Caption := GetHumanSize(t.Floats['downloadedEver']);
  txUploaded.Caption := GetHumanSize(t.Floats['uploadedEver']);

  f := t.Floats['pieceSize'];
  If f > 0 Then
    i := Round(t.Floats['corruptEver'] / f)
  Else
    i := 0;
  txWasted.Caption := Format(sHashfails, [GetHumanSize(t.Floats['corruptEver']), i]);
  s := GetHumanSize(gTorrents.Items[idxDownSpeed, idx], 1) + sPerSecond;
  If t.IndexOfName('secondsDownloading') >= 0 Then
  Begin
    f := t.Integers['secondsDownloading'];
    If f > 0 Then
      s := Format('%s (%s: %s)',
        [s, SAverage, GetHumanSize(t.Floats['downloadedEver'] / f, 1) + sPerSecond]);
  End;
  txDownSpeed.Caption := s;
  txUpSpeed.Caption := GetHumanSize(gTorrents.Items[idxUpSpeed, idx], 1) + sPerSecond;
  s := RatioToString(t.Floats['uploadRatio']);
  If t.IndexOfName('secondsSeeding') >= 0 Then
  Begin
    i := t.Integers['secondsSeeding'];
    If i > 0 Then
      s := Format('%s (%s)', [s, EtaToString(i)]);
  End;
  txRatio.Caption := s;

  If RpcObj.RPCVersion < 5 Then
  Begin
    // RPC versions prior to v5
    j := t.Integers['downloadLimitMode'];
    If j = TR_SPEEDLIMIT_GLOBAL Then
      s := '-'
    Else
    Begin
      i := t.Integers['downloadLimit'];
      If (i < 0) Or (j = TR_SPEEDLIMIT_UNLIMITED) Then
        s := Utf8Encode(WideString(Widechar($221E)))
      Else
        s := GetHumanSize(i * 1024) + sPerSecond;
    End;
    txDownLimit.Caption := s;
    j := t.Integers['uploadLimitMode'];
    If j = TR_SPEEDLIMIT_GLOBAL Then
      s := '-'
    Else
    Begin
      i := t.Integers['uploadLimit'];
      If (i < 0) Or (j = TR_SPEEDLIMIT_UNLIMITED) Then
        s := Utf8Encode(WideString(Widechar($221E)))
      Else
        s := GetHumanSize(i * 1024) + sPerSecond;
    End;
    txUpLimit.Caption := s;
  End
  Else
  Begin
    // RPC version 5
    If t.Booleans['downloadLimited'] Then
    Begin
      i := t.Integers['downloadLimit'];
      If i < 0 Then
        s := Utf8Encode(WideString(Widechar($221E)))
      Else
        s := GetHumanSize(i * 1024) + sPerSecond;
    End
    Else
      s := '-';
    txDownLimit.Caption := s;

    If t.Booleans['uploadLimited'] Then
    Begin
      i := t.Integers['uploadLimit'];
      If i < 0 Then
        s := Utf8Encode(WideString(Widechar($221E)))
      Else
        s := GetHumanSize(i * 1024) + sPerSecond;
    End
    Else
      s := '-';
    txUpLimit.Caption := s;
  End;

  If RpcObj.RPCVersion >= 7 Then
    With t.Arrays['trackerStats'] Do
    Begin
      If Count > 0 Then
      Begin
        If Integer(Objects[0].Integers['announceState']) In [2, 3] Then
          f := 1
        Else
          f := Objects[0].Floats['nextAnnounceTime'];
      End
      Else
        f := 0;
    End
  Else
    f := t.Floats['nextAnnounceTime'];
  If f = 0 Then
    s := '-'
  Else
    If f = 1 Then
      s := sUpdating
    Else
      s := TorrentDateTimeToString(Trunc(f), FFromNow);
  txTrackerUpdate.Caption := s;
  txTrackerUpdate.Hint := TorrentDateTimeToString(Trunc(f), Not (FFromNow));
  txTracker.Caption := UTF8Encode(WideString(gTorrents.Items[idxTracker, idx]));
  If RpcObj.RPCVersion >= 7 Then
    If t.Arrays['trackerStats'].Count > 0 Then
      i := t.Arrays['trackerStats'].Objects[0].Integers['seederCount']
    Else
      i := -1
  Else
    i := t.Integers['seeders'];
  s := GetSeedsText(t.Integers['peersSendingToUs'], i);
  txSeeds.Caption := StringReplace(s, '/', ' ' + sOf + ' ', []) + ' ' + sConnected;
  If RpcObj.RPCVersion >= 7 Then
    If t.Arrays['trackerStats'].Count > 0 Then
      i := t.Arrays['trackerStats'].Objects[0].Integers['leecherCount']
    Else
      i := -1
  Else
    i := t.Integers['leechers'];
  s := GetPeersText(t.Integers['peersGettingFromUs'], -1, i);
  s := StringReplace(s, ' ', ' ' + sConnected + ' ', []);
  s := StringReplace(s, '/', ' ' + sOf + ' ', []);
  txPeers.Caption := StringReplace(s, ')', ' ' + sInSwarm + ')', []);
  txMaxPeers.Caption := t.Strings['maxConnectedPeers'];
  txLastActive.Caption := TorrentDateTimeToString(
    Trunc(t.Floats['activityDate']), FFromNow);
  txLastActive.Hint := TorrentDateTimeToString(Trunc(t.Floats['activityDate']),
    Not (FFromNow));
  //  panTransfer.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;

  If RpcObj.RPCVersion >= 7 Then
    txMagnetLink.Text := t.Strings['magnetLink'];


  //  panGeneralInfo.ChildSizing.Layout:=cclNone;

  s := UTF8Encode(WideString(gTorrents.Items[idxName, idx]));
  If RpcObj.RPCVersion >= 4 Then
    s := IncludeProperTrailingPathDelimiter(
      UTF8Encode(t.Strings['downloadDir'])) + s;
  txTorrentName.Caption := s;
  s := Trim(UTF8Encode(t.Strings['creator']));
  If s <> '' Then
    s := ' by ' + s;
  txCreated.Caption := TorrentDateTimeToString(Trunc(t.Floats['dateCreated']),
    FFromNow) + s;
  txCreated.Hint := TorrentDateTimeToString(Trunc(t.Floats['dateCreated']),
    Not (FFromNow)) + s;
  If gTorrents.Items[idxSize, idx] >= 0 Then
  Begin
    txTotalSize.Caption :=
      Format(sDone, [GetHumanSize(t.Floats['totalSize']),
      GetHumanSize(t.Floats['sizeWhenDone'] - t.Floats['leftUntilDone'])]);
    If t.Floats['totalSize'] = t.Floats['haveValid'] Then
      i := t.Integers['pieceCount']
    Else
      i := Trunc(t.Floats['haveValid'] / (t.Floats['pieceSize'] + 0.00000001));
    // division by 0

    txPieces.Caption := Format(sHave, [t.Integers['pieceCount'],
      GetHumanSize(t.Floats['pieceSize']), i]);
  End
  Else
  Begin
    txTotalSize.Caption := '?';
    txPieces.Caption := '?';
  End;

  txHash.Caption := t.Strings['hashString'];
  txComment.Caption := UTF8Encode(t.Strings['comment']);
  If (AnsiCompareText(Copy(txComment.Caption, 1, 7), 'http://') = 0) Or
    (AnsiCompareText(Copy(txComment.Caption, 1, 8), 'https://') = 0) Then
  Begin
    If Not Assigned(txComment.OnClick) Then
    Begin
      txComment.OnClick := @UrlLabelClick;
      txComment.Cursor := crHandPoint;
      txComment.Font.Color := clBlue;
      txComment.Font.Style := [fsUnderline];
    End;
  End
  Else
  Begin
    If Assigned(txComment.OnClick) Then
    Begin
      txComment.OnClick := nil;
      txComment.Cursor := crDefault;
      txComment.ParentFont := True;
    End;
  End;
  txAddedOn.Caption := TorrentDateTimeToString(Trunc(t.Floats['addedDate']),
    FFromNow);
  txAddedOn.Hint := TorrentDateTimeToString(Trunc(t.Floats['addedDate']),
    Not (FFromNow));
  txCompletedOn.Caption := TorrentDateTimeToString(Trunc(t.Floats['doneDate']),
    FFromNow);
  txCompletedOn.Hint := TorrentDateTimeToString(Trunc(t.Floats['doneDate']),
    Not (FFromNow));
  //  panGeneralInfo.ChildSizing.Layout:=cclLeftToRightThenTopToBottom;

  If t.IndexOfName('labels') >= 0 Then
  Begin
    ja := t.Arrays['labels'];
    s := '';
    alabels := TStringList.Create;
    For i := 0 To ja.Count - 1 Do
      alabels.Add(ja.Strings[i]);
    alabels.Sort;
    For i := 0 To alabels.Count - 1 Do
    Begin
      If i > 0 Then
      Begin
        s := s + ', ';
      End;
      s := s + alabels[i];
    End;
    txLabels.Caption := s;
    alabels.Free;

  End;
  DetailsUpdated;
End;

Procedure TMainForm.FillTrackersList(TrackersData: TJSONObject);
Var
  i, tidx, row: Integer;
  id: Integer;
  d: TJSONData;
  t: TJSONObject;
  f: Double;
  s: String;
  Trackers, TrackerStats: TJSONArray;
  WasEmpty, NoInfo: Boolean;
Begin
  If TrackersData = nil Then
  Begin
    ClearDetailsInfo;
    exit;
  End;
  Trackers := TrackersData.Arrays['trackers'];
  If RpcObj.RPCVersion >= 7 Then
    TrackerStats := TrackersData.Arrays['trackerStats']
  Else
    TrackerStats := nil;
  tidx := gTorrents.Items.IndexOf(idxTorrentId, TrackersData.Integers['id']);
  If tidx = -1 Then
  Begin
    ClearDetailsInfo;
    exit;
  End;
  i := gTorrents.Items[idxStatus, tidx];
  NoInfo := (i = TR_STATUS_STOPPED) Or (i = TR_STATUS_FINISHED);
  WasEmpty := lvTrackers.Items.Count = 0;
  lvTrackers.Items.BeginUpdate;
  Try
    lvTrackers.Enabled := True;
    lvTrackers.Color := clWindow;
    For i := 0 To lvTrackers.Items.Count - 1 Do
      lvTrackers.Items[idxTrackerTag, i] := 0;

    lvTrackers.Items.Sort(idxTrackerID);
    For i := 0 To Trackers.Count - 1 Do
    Begin
      d := Trackers[i];
      If Not (d Is TJSONObject) Then continue;
      t := d As TJSONObject;
      If t.IndexOfName('id') >= 0 Then
        id := t.Integers['id']
      Else
        id := i;
      If Not lvTrackers.Items.Find(idxTrackerID, id, row) Then
        lvTrackers.Items.InsertRow(row);
      lvTrackers.Items[idxTrackerID, row] := id;
      lvTrackers.Items[idxTrackersListName, row] := t.Strings['announce'];
      If NoInfo Then
      Begin
        lvTrackers.Items[idxTrackersListStatus, row] := NULL;
        lvTrackers.Items[idxTrackersListSeeds, row] := NULL;
        f := 0;
      End
      Else
        If TrackerStats <> nil Then
        Begin
          f := 0;
          If i < TrackerStats.Count Then
            With TrackerStats.Objects[i] Do
            Begin
              s := '';
              If Integer(Integers['announceState']) In [2, 3] Then
                s := sTrackerUpdating
              Else
                If Booleans['hasAnnounced'] Then
                  If Booleans['lastAnnounceSucceeded'] Then
                    s := sTrackerWorking
                  Else
                    s := TranslateString(
                      UTF8Encode(Strings['lastAnnounceResult']), True);

              If s = 'Success' Then
                s := sTrackerWorking;

              lvTrackers.Items[idxTrackersListStatus, row] :=
                UTF8Decode(s);
              // UTF8Decode
              lvTrackers.Items[idxTrackersListSeeds, row] :=
                Integers['seederCount'];

              If Integer(Integers['announceState']) In [2, 3] Then
                f := 1
              Else
                f := Floats['nextAnnounceTime'];
            End;
        End
        Else
        Begin
          If i = 0 Then
          Begin
            lvTrackers.Items[idxTrackersListStatus, row] :=
              gTorrents.Items[idxTrackerStatus, tidx];
            lvTrackers.Items[idxTrackersListSeeds, row] :=
              gTorrents.Items[idxSeedsTotal, tidx];
          End;
          f := TrackersData.Floats['nextAnnounceTime'];
        End;

      If f > 1 Then
      Begin
        f := (UnixToDateTime(Trunc(f)) + GetTimeZoneDelta - Now) * SecsPerDay;
        If f < 0 Then
          f := 0;
      End;
      If (TrackerStats <> nil) Or (i = 0) Then
        lvTrackers.Items[idxTrackersListUpdateIn, row] := f;

      lvTrackers.Items[idxTrackerTag, row] := 1;
    End;

    i := 0;
    While i < lvTrackers.Items.Count Do
      If lvTrackers.Items[idxTrackerTag, i] = 0 Then
        lvTrackers.Items.Delete(i)
      Else
        Inc(i);

    lvTrackers.Sort;
    If WasEmpty And (lvTrackers.Items.Count > 0) Then
      lvTrackers.Row := 0;
  Finally
    lvTrackers.Items.EndUpdate;
  End;
  DetailsUpdated;
End;

Procedure TMainForm.FillSessionInfo(s: TJSONObject);
Var
  d, u: Integer;
  str: String;
Begin
  {$ifdef LCLcarbon}
  TrayIcon.Tag := 0;
  {$endif LCLcarbon}
  If RpcObj.RPCVersion < 14 Then
  Begin
    TR_STATUS_STOPPED := TR_STATUS_STOPPED_1;
    TR_STATUS_CHECK_WAIT := TR_STATUS_CHECK_WAIT_1;
    TR_STATUS_CHECK := TR_STATUS_CHECK_1;
    TR_STATUS_DOWNLOAD_WAIT := -1;
    TR_STATUS_DOWNLOAD := TR_STATUS_DOWNLOAD_1;
    TR_STATUS_SEED_WAIT := -1;
    TR_STATUS_SEED := TR_STATUS_SEED_1;
  End
  Else
  Begin
    TR_STATUS_STOPPED := TR_STATUS_STOPPED_2;
    TR_STATUS_CHECK_WAIT := TR_STATUS_CHECK_WAIT_2;
    TR_STATUS_CHECK := TR_STATUS_CHECK_2;
    TR_STATUS_DOWNLOAD_WAIT := TR_STATUS_DOWNLOAD_WAIT_2;
    TR_STATUS_DOWNLOAD := TR_STATUS_DOWNLOAD_2;
    TR_STATUS_SEED_WAIT := TR_STATUS_SEED_WAIT_2;
    TR_STATUS_SEED := TR_STATUS_SEED_2;
  End;

  UpdateUIRpcVersion(RpcObj.RPCVersion);

  If RpcObj.RPCVersion >= 5 Then
  Begin
    {$ifdef LCLcarbon}
      If acAltSpeed.Checked <> (s.Integers['alt-speed-enabled'] <> 0) Then
        TrayIcon.Tag := 1;
    {$endif LCLcarbon}
    acAltSpeed.Checked := s.Integers['alt-speed-enabled'] <> 0;
    acUpdateBlocklist.Tag := s.Integers['blocklist-enabled'];
    acUpdateBlocklist.Enabled := acUpdateBlocklist.Tag <> 0;
  End;
  str := '';
  If s.IndexOfName('download-dir-free-space') >= 0 Then
    str := Format(SFreeSpace, [GetHumanSize(s.Floats['download-dir-free-space'])]);
  If (RpcObj.IncompleteDir <> '') Then
  Begin
    If (copy(RpcObj.IncompleteDir, 1, 3) <> copy(s.Strings['download-dir'], 1, 3)) Or
      (Pos(':', RpcObj.IncompleteDir) = 0) Then
      If s.IndexOfName('incomplete-dir-free-space') >= 0 Then
        str := str + ' ' + Format(STempSpace,
          [GetHumanSize(s.Floats['incomplete-dir-free-space'])]);
  End;
  StatusBar.Panels[3].Text := str;
  //  GetFreeSpace;

  If (RpcObj.RPCVersion >= 5) And acAltSpeed.Checked Then
  Begin
    d := s.Integers['alt-speed-down'];
    u := s.Integers['alt-speed-up'];
  End
  Else
  Begin
    If s.Integers['speed-limit-down-enabled'] <> 0 Then
      d := s.Integers['speed-limit-down']
    Else
      d := -1;
    If s.Integers['speed-limit-up-enabled'] <> 0 Then
      u := s.Integers['speed-limit-up']
    Else
      u := -1;
  End;
  {$ifdef LCLcarbon}
  UpdateUI;
  {$endif LCLcarbon}
  If (FCurDownSpeedLimit <> d) Or (FCurUpSpeedLimit <> u) Then
  Begin
    FCurDownSpeedLimit := d;
    FCurUpSpeedLimit := u;
    FillSpeedsMenu;
  End;
  {$ifdef LCLcarbon}
  If TrayIcon.Tag <> 0 Then
    TrayIcon.InternalUpdate;
  {$endif LCLcarbon}
End;

Procedure TMainForm.FillStatistics(s: TJSONObject);

  Procedure _Fill(idx: Integer; s: TJSONObject);
  Begin
    With gStats Do
    Begin
      Items[idx, 0] := UTF8Decode(GetHumanSize(s.Floats['downloadedBytes']));
      Items[idx, 1] := UTF8Decode(GetHumanSize(s.Floats['uploadedBytes']));
      Items[idx, 2] := s.Integers['filesAdded'];
      Items[idx, 3] := UTF8Decode(SecondsToString(s.Integers['secondsActive']));
    End;
  End;

Begin
  If RpcObj.RPCVersion < 4 Then
    exit;
  If s = nil Then
  Begin
    ClearDetailsInfo;
    exit;
  End;
  gStats.BeginUpdate;
  Try
    gStats.Enabled := True;
    gStats.Color := clWindow;
    _Fill(1, s.Objects['current-stats']);
    _Fill(2, s.Objects['cumulative-stats']);
  Finally
    gStats.EndUpdate;
  End;
  DetailsUpdated;
End;

Procedure TMainForm.CheckStatus(Fatal: Boolean);
Var
  s: String;
  i: Integer;
Begin
  With MainForm Do
  Begin
    s := TranslateString(RpcObj.Status, True);

    If s <> '' Then
    Begin
      RpcObj.Status := '';
      If Fatal Then
        DoDisconnect;
      ForceAppNormal;
      If Fatal //and not RpcObj.Connected
        And RpcObj.ReconnectAllowed And (FReconnectTimeOut <> -1) Then
      Begin
        FReconnectWaitStart := Now;
        If FReconnectTimeOut < 60 Then
          If FReconnectTimeOut < 10 Then
            Inc(FReconnectTimeOut, 5)
          Else
            Inc(FReconnectTimeOut, 10);
        txConnError.Caption := s;
        panReconnectFrame.Hide;
        panReconnect.AutoSize := True;
        CenterReconnectWindow;
        panReconnect.Show;
        panReconnect.BringToFront;
        TickTimerTimer(nil);
        panReconnect.AutoSize := False;
        panReconnectFrame.Show;
        CenterReconnectWindow;
      End
      Else //if not RpcObj.Connected then
        MessageDlg(s, mtError, [mbOK], 0);
    End;

    If StatusBar.Panels[0].Text <> RpcObj.InfoStatus Then
    Begin
      StatusBar.Panels[0].Text := RpcObj.InfoStatus;
      TrayIcon.Hint := RpcObj.InfoStatus;
      If (RpcObj.Connected) And (RpcObj.Http.UserName <> '') Then
        FPasswords.Values[FCurConn] := RpcObj.Http.Password;
      // Save password to cache
    End;
    If Not RpcObj.Connected Then
      For i := 1 To StatusBar.Panels.Count - 1 Do
        StatusBar.Panels[i].Text := '';
  End;
End;

Function TMainForm.TorrentAction(Const TorrentIds: Variant;
  Const AAction: String; args: TJSONObject): Boolean;
Var
  req: TJSONObject;
  ids: TJSONArray;
  i: Integer;
Begin
  If VarIsEmpty(TorrentIds) Then
    exit;
  Application.ProcessMessages;
  AppBusy;
  req := TJSONObject.Create;
  Try
    req.Add('method', AAction);
    If args = nil Then
      args := TJSONObject.Create;
    If Not VarIsNull(TorrentIds) Then
    Begin
      ids := TJSONArray.Create;
      If VarIsArray(TorrentIds) Then
      Begin
        For i := VarArrayLowBound(TorrentIds, 1) To VarArrayHighBound(
            TorrentIds, 1) Do
          ids.Add(Integer(TorrentIds[i]));
      End
      Else
        ids.Add(Integer(TorrentIds));
      args.Add('ids', ids);
    End;
    req.Add('arguments', args);
    //    args:=nil;
    args := RpcObj.SendRequest(req, False, 30000);
    Result := args <> nil;
  Finally
    args.Free;
    req.Free;
  End;
  If Not Result Then
    CheckStatus(False)
  Else
    DoRefresh(True);
  AppNormal;
End;

Function TMainForm.SetFilePriority(TorrentId: Integer;
  Const Files: Array Of Integer; Const APriority: String): Boolean;

  Function CreateFilesArray: TJSONArray;
  Var
    i: Integer;
  Begin
    Result := TJSONArray.Create;
    For i := Low(Files) To High(Files) Do
      Result.Add(Files[i]);
  End;

Var
  req, args: TJSONObject;
Begin
  AppBusy;
  req := TJSONObject.Create;
  Try
    req.Add('method', 'torrent-set');
    args := TJSONObject.Create;
    If TorrentId <> 0 Then
      args.Add('ids', TJSONArray.Create([TorrentId]));
    If APriority = 'skip' Then
      args.Add('files-unwanted', CreateFilesArray)
    Else
    Begin
      args.Add('files-wanted', CreateFilesArray);
      args.Add('priority-' + APriority, CreateFilesArray);
    End;
    req.Add('arguments', args);
    args := RpcObj.SendRequest(req, False);
    Result := args <> nil;
  Finally
    args.Free;
    req.Free;
  End;
  If Not Result Then
    CheckStatus(False)
  Else
    DoRefresh;
  AppNormal;
End;

Function TMainForm.SetCurrentFilePriority(Const APriority: String): Boolean;
Var
  Files: Array Of Integer;
  i, j, k, level: Integer;
  pri: String;
Begin
  Result := False;
  If (gTorrents.Items.Count = 0) Or (PageInfo.ActivePage <> tabFiles) Then exit;
  SetLength(Files, lvFiles.Items.Count);
  pri := APriority;
  j := 0;
  If APriority <> '' Then
  Begin
    // Priority for currently selected rows
    If lvFiles.SelCount = 0 Then
      lvFiles.RowSelected[lvFiles.Row] := True;
    level := -1;
    For i := 0 To lvFiles.Items.Count - 1 Do
    Begin
      k := FFilesTree.RowLevel[i];
      If k <= level Then
        level := -1;
      If lvFiles.RowSelected[i] Or ((level <> -1) And (k > level)) Then
      Begin
        If FFilesTree.IsFolder(i) Then
        Begin
          If level = -1 Then
            level := k;
        End
        Else
        Begin
          Files[j] := FFiles[idxFileId, i];
          Inc(j);
        End;
      End;
    End;
  End
  Else
  Begin
    // Priority based on checkbox state
    For i := 0 To FFiles.Count - 1 Do
      If Not FFilesTree.IsFolder(i) Then
      Begin
        k := FFiles[idxFilePriority, i];
        If (k <> TR_PRI_SKIP) <> (FFilesTree.Checked[i] = cbChecked) Then
        Begin
          If pri = '' Then
            If FFilesTree.Checked[i] = cbChecked Then
              pri := 'normal'
            Else
              pri := 'skip';
          Files[j] := FFiles[idxFileId, i];
          Inc(j);
        End;
      End;
  End;

  If j = 0 Then exit;
  SetLength(Files, j);
  Result := SetFilePriority(RpcObj.CurTorrentId, Files, pri);
End;

Procedure TMainForm.SetTorrentPriority(APriority: Integer);
Var
  args: TJSONObject;
Begin
  If gTorrents.Items.Count = 0 Then exit;
  args := TJSONObject.Create;
  args.Add('bandwidthPriority', TJSONIntegerNumber.Create(APriority));
  TorrentAction(GetSelectedTorrents, 'torrent-set', args);
End;

Procedure TMainForm.ProcessPieces(Const Pieces: String; PieceCount: Integer;
  Const Done: Double);
Const
  MaxPieces = 4000;
Var
  i, j, k, x, xx: Integer;
  s: String;
  R: TRect;
  bmp: TBitmap;
  c: Double;
Begin
  FLastPieces := Pieces;
  FLastPieceCount := PieceCount;
  FLastDone := Done;
  bmp := nil;
  Try
    If FTorrentProgress = nil Then
      FTorrentProgress := TBitmap.Create;
    If RpcObj.RPCVersion >= 5 Then
    Begin
      bmp := TBitmap.Create;
      If PieceCount > MaxPieces Then
      Begin
        bmp.Width := MaxPieces;
        c := MaxPieces / PieceCount;
      End
      Else
      Begin
        bmp.Width := PieceCount;
        c := 1;
      End;
      bmp.Height := 12;
      bmp.Canvas.Brush.Color := clWindow;
      bmp.Canvas.FillRect(0, 0, bmp.Width, bmp.Height);
      bmp.Canvas.Brush.Color := clHighlight;
      x := 0;
      s := DecodeBase64(Pieces);
      For i := 1 To Length(s) Do
      Begin
        j := Byte(s[i]);
        For k := 1 To 8 Do
        Begin
          If PieceCount = 0 Then
            break;
          If j And $80 <> 0 Then
          Begin
            xx := Trunc(x * c);
            bmp.Canvas.FillRect(xx, 0, xx + 1, bmp.Height);
          End;
          Inc(x);
          j := j Shl 1;
          Dec(PieceCount);
        End;
      End;
    End;

    With FTorrentProgress.Canvas Do
    Begin
      FTorrentProgress.Width := pbDownloaded.ClientWidth;
      If bmp <> nil Then
      Begin
        i := bmp.Height Div 3;
        FTorrentProgress.Height := bmp.Height + 5 + i;
        Brush.Color := clWindow;
        FillRect(0, 0, FTorrentProgress.Width, FTorrentProgress.Height);
        Brush.Color := clBtnShadow;
        R := Rect(0, i + 3, FTorrentProgress.Width, FTorrentProgress.Height);
        FillRect(R);
        InflateRect(R, -1, -1);
        If bmp.Width > 0 Then
          StretchDraw(R, bmp)
        Else
        Begin
          Brush.Color := clWindow;
          FillRect(R);
        End;
        R := Rect(0, 0, FTorrentProgress.Width, i + 2);
      End
      Else
      Begin
        FTorrentProgress.Height := 14;
        R := Rect(0, 0, FTorrentProgress.Width, FTorrentProgress.Height);
      End;
      Brush.Color := clBtnShadow;
      FillRect(R);
      InflateRect(R, -1, -1);
      x := R.Left + Round((R.Right - R.Left) * Done / 100.0);
      Brush.Color := clHighlight;
      FillRect(R.Left, R.Top, x, R.Bottom);
      Brush.Color := clWindow;
      FillRect(x, R.Top, R.Right, R.Bottom);
    End;
    If pbDownloaded.Height <> FTorrentProgress.Height Then
    Begin
      pbDownloaded.Constraints.MaxHeight := FTorrentProgress.Height;
      pbDownloaded.Height := FTorrentProgress.Height;
      panProgress.AutoSize := True;
      panProgress.AutoSize := False;
    End;
    pbDownloaded.Invalidate;
  Finally
    bmp.Free;
  End;
End;

Function TMainForm.ExecRemoteFileArray(Const FileName: String;
  SelectFile: Boolean; Ids: Variant; Userdef: Boolean): Boolean;
Var
  p: String;
  s: String;
  i: Integer;
Begin
  s := '';
  For i := VarArrayLowBound(Ids, 1) To VarArrayHighBound(Ids, 1) Do
  Begin
    If i = VarArrayLowBound(Ids, 1) Then
      s := VarToStr(Ids[i])
    Else
      s := s + ',' + VarToStr(Ids[i]);

  End;
  p := Format(FUserDefinedMenuParam, [RpcObj.Url, '"' + s + '"']);
  s := FUserDefinedMenuEx;
  Result := OpenURL(s, p);
End;

Function TMainForm.ExecRemoteFile(Const FileName: String; SelectFile: Boolean;
  TorrentId: Integer; Userdef: Boolean): Boolean;

  Procedure _Exec(s: String);
  Var
    p: String;
  Begin
    AppBusy;
    If SelectFile Then
    Begin
      //      if FileExistsUTF8(s) then begin
      {$ifdef mswindows}
      If Userdef Then
      Begin
        p := Format(FUserDefinedMenuParam,
          [s, RpcObj.Url, TorrentId.ToString()]);
        s := FUserDefinedMenuEx;
      End
      Else
      Begin
        If SelectFile Then
          p := Format(FFileManagerSelectParam, [s])
        // ALERT  //      p:=Format('/select,"%s"', [s]);
        Else
          p := Format(FFileManagerDefaultParam, [s]);
        ;
        // ALERT  //      p:=Format('/select,"%s"', [s]);
        s := FFileManagerDefault;
        //      s:='explorer.exe';
      End;
      {$else}
      p := '';
      s := ExtractFilePath(s);
      {$endif mswindows}
      //      end
      //      else begin
      //        if FileExistsUTF8(s+'.part') then begin


      //          p:=Format(FFileManagerDefaultParam, [s+'.part']); ; // ALERT  //      p:=Format('/select,"%s"', [s]);


      //          s:=FFileManagerDefault;                               //      s:='explorer.exe';
      //        end
      //        else
      //        begin
      //          p:='';
      //          s:=ExtractFilePath(s);
      //        end;
      //      end;
    End
    Else
    Begin
      If Userdef Then
      Begin
        p := Format(FUserDefinedMenuParam, [RpcObj.Url, TorrentId.ToString()]);
        s := FUserDefinedMenuEx;
      End;
    End;
    OutputDebugString(LPCSTR(s));
    {$ifdef mswindows}
    //                    if FileExistsUTF8(s+'.part') then
    //                      Result:=OpenURL(s+'.part', p)
    //                    else
    Result := OpenURL(s, p);
    {$else}
  If FLinuxOpenDoc = 0 Then
    Result := OpenURL(s, p)
              // does not work in latest linux very well!!!! old.vers
  Else
    Result := OpenDocument(s);
  // works better - new.vers
    {$endif mswindows}

    AppNormal;
    If Not Result Then
    Begin
      ForceAppNormal;
      MessageDlg(Format(sUnableToExecute, [s]), mtError, [mbOK], 0);
    End;
  End;

Var
  s, r, r1: String;
  i: Integer;
Begin
  Result := False;
  s := '';
  If FileExistsUTF8(CorrectPath(MapRemoteToLocal(FileName))) Or
    DirectoryExistsUTF8(CorrectPath(MapRemoteToLocal(FileName))) Then
    s := '"' + CorrectPath(MapRemoteToLocal(FileName)) + '"'
  Else If FileExistsUTF8(CorrectPath(MapRemoteToLocal(FileName + '.part'))) Then
      s := '"' + CorrectPath(MapRemoteToLocal(FileName)) + '.part"';
  If (s <> '') Or UserDef Then
  Begin
    If Userdef Then
    Begin
      If (lvFiles.Focused) Then
        If lvFiles.SelCount > 1 Then
        Begin
          r := '';
          If FUserDefinedMenuParamType = 'id' Then
          Begin
            For i := 0 To lvFiles.Items.Count - 1 Do
              If lvFiles.RowSelected[i] Then
                If r = '' Then r := Format('%d', [i])
                Else
                  r := r + Format(',%d', [i]);
          End
          Else
          Begin
            For i := 0 To lvFiles.Items.Count - 1 Do
              If lvFiles.RowSelected[i] Then
              Begin
                r1 := FFilesTree.GetFullPath(i);
                If (Not (FileExistsUTF8(MapRemoteToLocal(r1)) Or
                  FileExistsUTF8(MapRemoteToLocal(r1 + '.part'))) And
                  (RpcObj.IncompleteDir <> '')) Then
                  r1 := FFilesTree.GetIncompleteFullPath(i);

                If r = '' Then
                  r := '"' + CorrectPath(MapRemoteToLocal(r1)) + '"'
                Else
                  r :=
                    r + ',"' + CorrectPath(MapRemoteToLocal(r1)) + '"';
              End;
          End;
          s := r;

        End
        Else
        Begin
          If FUserDefinedMenuParamType = 'id' Then
            s := Format('%d', [lvFiles.Row]);
        End;

      // else s := '"' + s + '"';
    End;
    _Exec(s);
    exit;
  End;
  If FileExistsUTF8(CorrectPath(MapRemoteToLocal(FileName))) Or
    DirectoryExistsUTF8(CorrectPath(MapRemoteToLocal(FileName))) Then
  Begin
    _Exec(CorrectPath(MapRemoteToLocal(FileName)));
    exit;
  End;
  If FileExistsUTF8(CorrectPath(MapRemoteToLocal(FileName + '.part'))) Then
  Begin
    _Exec(CorrectPath(MapRemoteToLocal(FileName + '.part')));
    exit;
  End;

  ForceAppNormal;
  MessageDlg(sNoPathMapping, mtInformation, [mbOK], 0);



  //  MessageDlg(sNoPathMapping+LineEnding+s+LineEnding+FileName+LineEnding+CorrectPath(MapRemoteToLocal(FileName)), mtInformation, [mbOK], 0);
End;

Function TMainForm.GetFilteredTorrents: Variant;
Var
  i, j: Integer;
Begin
  With gTorrents Do
  Begin
    If Items.Count = 0 Then
    Begin
      Result := Unassigned;
      exit;
    End;
    Result := VarArrayCreate([0, gTorrents.Items.Count - 1], varinteger);
    j := 0;
    For i := 0 To gTorrents.Items.Count - 1 Do
    Begin
      Result[j] := Items[idxTorrentId, i];
      Inc(j);
    End;
    exit;
  End;
End;

Function TMainForm.GetSelectedTorrents: Variant;
Var
  i, j: Integer;
Begin
  With gTorrents Do
  Begin
    If Items.Count = 0 Then
    Begin
      Result := Unassigned;
      exit;
    End;
    If SelCount = 0 Then
      Result := VarArrayOf([Items[idxTorrentId, Row]])
    Else
    Begin
      Result := VarArrayCreate([0, SelCount - 1], varinteger);
      j := 0;
      For i := 0 To gTorrents.Items.Count - 1 Do
        If gTorrents.RowSelected[i] Then
        Begin
          Result[j] := Items[idxTorrentId, i];
          Inc(j);
        End;
    End;
  End;
End;

Function TMainForm.GetDisplayedTorrents: Variant;
Var
  i, j: Integer;
Begin
  With gTorrents Do
  Begin
    If Items.Count = 0 Then
    Begin
      Result := Unassigned;
      exit;
    End;
    Result := VarArrayCreate([0, gTorrents.Items.Count - 1], varinteger);
    j := 0;
    For i := 0 To gTorrents.Items.Count - 1 Do
      If gTorrents.RowVisible[i] Then
      Begin
        Result[j] := Items[idxTorrentId, i];
        Inc(j);
      End;
  End;
End;

Procedure TMainform.StatusBarSizes;
Var
  MMap: TMyHashMap;
  ids, cidx: Variant;
  TotalSize, TotalDownloaded, TotalSizeToDownload, TorrentDownloaded,
  TorrentSizeToDownload: Int64;
  i: Integer;
  //a, b, c, d: Int64;
Begin
  Try
    If gTorrents.Items.Count > 0 Then
    Begin
      If gTorrents.SelCount > 0 Then
        ids := GetSelectedTorrents
      Else
        ids := GetDisplayedTorrents;
      TotalSize := 0;
      TotalDownloaded := 0;
      TotalSizeToDownload := 0;

      MMap := TMyHashMap.Create;
      For i := 0 To FTorrents.Count - 1 Do
      Begin
        MMap[StrToInt(Ftorrents.Items[idxTorrentId, i])] := i;
      End;

      For i := VarArrayLowBound(ids, 1) To VarArrayHighBound(ids, 1) Do
      Begin
        Try
          cidx := MMap[ids[i]];
          TotalSize :=
            TotalSize + FTorrents.Items[idxSize, cidx];
          TorrentSizeToDownload := FTorrents.Items[idxSizetoDowload, cidx];
          TorrentDownloaded :=
            TorrentSizeToDownload * (FTorrents.Items[idxDone, cidx] / 100);
          TotalSizeToDownload :=
            TotalSizeToDownload + TorrentSizeToDownload;
          TotalDownloaded := TotalDownloaded + TorrentDownloaded;

        Except
        End;
      End;
      MMap.Free;

      StatusBar.Panels[4].Text :=
        Format(sTotalSize, [GetHumanSize(TotalSize, 0, '?')]);
      StatusBar.Panels[5].Text :=
        Format(sTotalSizeToDownload, [GetHumanSize(TotalSizeToDownload, 0, '?')]);
      StatusBar.Panels[6].Text :=
        Format(sTotalDownloaded, [GetHumanSize(TotalDownloaded, 0, '?')]);
      StatusBar.Panels[7].Text :=
        Format(sTotalRemain, [GetHumanSize(TotalSizeToDownload -
        TotalDownloaded, 0, '?')]);
    End
    Else
    Begin
      StatusBar.Panels[4].Text := Format(sTotalSize, [GetHumanSize(0, 0, '?')]);
      StatusBar.Panels[5].Text :=
        Format(sTotalSizeToDownload, [GetHumanSize(0, 0, '?')]);
      StatusBar.Panels[6].Text :=
        Format(sTotalDownloaded, [GetHumanSize(0, 0, '?')]);
      StatusBar.Panels[7].Text := Format(sTotalRemain, [GetHumanSize(0, 0, '?')]);
    End;
  Except
    gTorrents.Refresh;
  End;

End;

Procedure TMainForm.FillDownloadDirs(CB: TComboBox; Const CurFolderParam: String);
Var
  i, j, n, xx, m: Integer;
  s, IniSec: String;
  lastDt: String;
  pFD: FolderData;

  dd, mm, yy: String;
  nd, nm, ny: Integer;
Begin
  CB.Items.Clear;

  IniSec := 'AddTorrent.' + FCurConn;
  j := Ini.ReadInteger(IniSec, 'FolderCount', 0);

  For i := 0 To j - 1 Do
  Begin
    s := Ini.ReadString(IniSec, Format('Folder%d', [i]), '');
    If s <> '' Then
    Begin
      s := CorrectPath(s);

      n := 0;
      For xx := 0 To CB.Items.Count - 1 Do
      Begin
        If CB.Items[xx] = s Then
        Begin
          n := 1;
        End;
      End;

      If n = 0 Then
      Begin
        m := CB.Items.Add(s);
        pFD := FolderData.Create;
        pFD.Hit := Ini.ReadInteger(IniSec, Format('FolHit%d', [i]), 1);
        pFD.Ext := Ini.ReadString(IniSec, Format('FolExt%d', [i]), '');
        lastDt := Ini.ReadString(IniSec, Format('LastDt%d', [i]), '');
        pFD.Txt := s;
        // for debug

        Try
          pFD.Lst := EncodeDate(2000, 1, 1);
          // last time folder

          If (lastDt <> '') Then
          Begin
            dd := Copy(lastDt, 1, 2);
            mm := Copy(lastDt, 4, 2);
            yy := Copy(lastDt, 7, 4);
            nd := StrToInt(dd);
            nm := StrToInt(mm);
            ny := StrToInt(yy);
            If (nd < 1) Or (nd > 31) Then nd := 1;
            If (nm < 1) Or (nm > 12) Then nm := 1;
            If (ny < 1) Or (ny > 2222) Then ny := 2000;
            pFD.Lst := EncodeDate(ny, nm, nd);
          End
        Except
          MessageDlg('Error: LS-007. Please contact the developer',
            mtError, [mbOK], 0);
          pFD.Lst := EncodeDate(2000, 1, 1);
          // last time folder
        End;

        CB.Items.Objects[m] := pFD;
      End;
    End;
  End;

  s := CorrectPath(Ini.ReadString(IniSec, CurFolderParam, ''));
  If s <> '' Then
  Begin
    i := CB.Items.IndexOf(s);
    If i > 0 Then  // autosorting
      CB.ItemIndex := i;
    CB.Text := s;
  End
  Else
  Begin
    If CB.Items.Count > 0 Then
      CB.ItemIndex := 0;
  End;
End;

Procedure TMainForm.SaveDownloadDirs(CB: TComboBox; Const CurFolderParam: String);
Var
  i: Integer;
  IniSec: String;
  tmp, selfolder: String;
  strdate: String;
  pFD: FolderData;
Begin
  IniSec := 'AddTorrent.' + FCurConn;
  selfolder := CorrectPath(CB.Text);
  i := CB.Items.IndexOf(selfolder);

  Try
    If CurFolderParam = 'LastMoveDir' Then
    Begin
      If i < 0 Then
      Begin
        DeleteDirs(CB, 1);
        CB.Items.Add(selfolder);
        i := CB.Items.IndexOf(selfolder);
        If i >= 0 Then
        Begin
          pFD := FolderData.Create;
          If pFD <> nil Then
          Begin
            pFD.Hit := 1;
            pFD.Ext := '';
            pFD.Txt := selfolder;
            pFD.Lst := IncDay(Today, 7);
            // +7 days
            CB.Items.Objects[i] := pFD;
          End;
        End;
      End
      Else
      Begin
        pFD := CB.Items.Objects[i] As FolderData;
        If pFD <> nil Then
        Begin
          pFD.Hit := pFD.Hit + 1;
          pFD.Lst := Today;
          CB.Items.Objects[i] := pFD;
        End;
        DeleteDirs(CB, 0);
      End;
    End;
  Except


    //    MessageDlg('Error: LS-008. Please contact the developer', mtError, [mbOK], 0);
  End;

  Try
    Ini.WriteInteger(IniSec, 'FolderCount', CB.Items.Count);
    For i := 0 To CB.Items.Count - 1 Do
    Begin
      tmp := CorrectPath(CB.Items[i]);
      pFD := CB.Items.Objects[i] As FolderData;
      If pFD = nil Then continue;

      Ini.WriteString(IniSec, Format('Folder%d', [i]), tmp);
      Ini.WriteInteger(IniSec, Format('FolHit%d', [i]), pFD.Hit);
      Ini.WriteString(IniSec, Format('FolExt%d', [i]), pFD.Ext);

      DateTimeToString(strdate, 'dd.mm.yyyy', pFD.Lst);
      Ini.WriteString(IniSec, Format('LastDt%d', [i]), strdate);
    End;

    // clear string
    Ini.WriteString(IniSec, Format('Folder%d', [i + 1]), '');
    Ini.WriteInteger(IniSec, Format('FolHit%d', [i + 1]), -1);
    Ini.WriteString(IniSec, Format('FolExt%d', [i + 1]), '');
    Ini.WriteString(IniSec, Format('LastDt%d', [i + 1]), '');
  Except
    MessageDlg('Error: LS-009. Please contact the developer', mtError, [mbOK], 0);
  End;

  Ini.WriteString(IniSec, CurFolderParam, selfolder);
  // autosorting, valid from text
  Ini.UpdateFile;
End;

Procedure TMainForm.DeleteDirs(CB: TComboBox; maxdel: Integer);
Var
  i, min, max, indx, fldr: Integer;
  pFD: FolderData;
Begin
  max := Ini.ReadInteger('Interface', 'MaxFoldersHistory', 50);
  Ini.WriteInteger('Interface', 'MaxFoldersHistory', max);

  Try
    While (CB.Items.Count + maxdel) >= max Do
    Begin
      min := 9999999;
      indx := -1;
      For i := 0 To CB.Items.Count - 1 Do
      Begin
        pFD := CB.Items.Objects[i] As FolderData;
        If pFD = nil Then continue;

        fldr := DaysBetween(Today, pFD.Lst);
        If Today > pFD.Lst Then
          fldr := 0 - fldr;

        fldr := fldr + pFD.Hit;
        If fldr < min Then
        Begin
          min := fldr;
          indx := i;
        End;
      End;

      If indx > -1 Then
        CB.Items.Delete(indx);
    End;
  Except
    MessageDlg('Error: LS-010. Please contact the developer', mtError, [mbOK], 0
      );
  End;
End;

Procedure TMainForm.SetRefreshInterval;
Var
  i: TDateTime;
Begin
  If Visible And (WindowState <> wsMinimized) Then
    i := Ini.ReadInteger('Interface', 'RefreshInterval', 5)
  Else
    i := Ini.ReadInteger('Interface', 'RefreshIntervalMin', 20);
  If i < 1 Then
    i := 1;
  RpcObj.RefreshInterval := i / SecsPerDay;
End;

Procedure TMainForm.AddTracker(EditMode: Boolean);
Var
  req, args: TJSONObject;
  id, torid: Integer;
  i, i1: Longint;
  listT, s: String;
Begin
  AppBusy;
  With TAddTrackerForm.Create(Self) Do
  Try
    id := 0;
    torid := RpcObj.CurTorrentId;
    If EditMode Then
    Begin
      Caption := STrackerProps;
      edTracker.Text := UTF8Encode(
        WideString(lvTrackers.Items[idxTrackersListName, lvTrackers.Row]));
      id := lvTrackers.Items[idxTrackerID, lvTrackers.Row];
    End;
    AppNormal;
    If ShowModal = mrOk Then
    Begin
      AppBusy;
      Self.Update;
      req := TJSONObject.Create;
      Try
        req.Add('method', 'torrent-set');
        args := TJSONObject.Create;
        args.Add('ids', TJSONArray.Create([torid]));
        If EditMode Then
          If (rpcObj.RPCVersion >= 17) Then
          Begin
            listT := '';


            //            lvTrackers.Items[idxTrackersListName, lvTrackers.Row]:=UTF8Decode(edTracker.Text);
            i1 := lvTrackers.Row;
            For i := 0 To lvTrackers.Items.Count - 1 Do
            Begin
              If (i = i1) Then
                s := edTracker.Text
              Else
                s := lvTrackers.Items[idxTrackersListName, i];
              listT := listT + s + #10;
            End;
            args.Add('trackerList', listT);
          End
          Else
            args.Add('trackerReplace',
              TJSONArray.Create([id, UTF8Encode(edTracker.Text)]))  //fix bag
        Else
          args.Add('trackerAdd',
            TJSONArray.Create([UTF8Encode(edTracker.Text)]));
        //fix bag
        req.Add('arguments', args);
        args := nil;
        args := RpcObj.SendRequest(req, False);
        If args = nil Then
        Begin
          CheckStatus(False);
          exit;
        End;
      Finally
        args.Free;
        req.Free;
      End;
      TorrentAction(GetSelectedTorrents, 'torrent-reannounce');
      DoRefresh;
      AppNormal;
    End;
  Finally
    Free;
  End;
End;

Procedure TMainForm.UpdateConnections;
Var
  i, j, cnt: Integer;
  s, cur: String;
  mi: TMenuItem;
Begin
  While (pmConnections.Items.Count > 0) And (pmConnections.Items[0].Tag = 0) Do
    pmConnections.Items[0].Free;
  While (miConnect.Count > 0) And (miConnect.Items[0].Tag = 0) Do
    miConnect.Items[0].Free;
  cur := Ini.ReadString('Hosts', 'CurHost', '');
  cnt := Ini.ReadInteger('Hosts', 'Count', 0);
  j := 0;
  For i := 1 To cnt Do
  Begin
    s := Ini.ReadString('Hosts', Format('Host%d', [i]), '');
    If s <> '' Then
    Begin
      mi := TMenuItem.Create(pmConnections);
      mi.Caption := s;
      If s = cur Then
        mi.Checked := True;
      mi.OnClick := @DoConnectToHost;
      pmConnections.Items.Insert(j, mi);

      mi := TMenuItem.Create(miConnect);
      mi.Caption := s;
      If s = cur Then
        mi.Checked := True;
      mi.OnClick := @DoConnectToHost;
      miConnect.Insert(j, mi);
      Inc(j);
    End;
  End;
  sepCon1.Visible := j > 0;
  sepCon2.Visible := j > 0;
End;

Procedure TMainForm.DoConnectToHost(Sender: TObject);
Var
  mi: TMenuItem;
  Sec: String;
Begin
  mi := TMenuItem(Sender);
  If RpcObj.Connected And (FCurConn = mi.Caption) Then
    exit;
  FLastFilterIndex := 0;
  lvFilter.Items.RowCnt := StatusFiltersCount;
  DoDisconnect;
  Sec := 'Connection.' + FCurConn;
  If (FReconnectTimeOut = -1) And Ini.ReadBool(Sec, 'Autoreconnect', False) Then
    FReconnectTimeOut := 0;
  FCurConn := mi.Caption;
  DoConnect;
End;

Procedure TMainForm.DoSetDownloadSpeed(Sender: TObject);
Begin
  SetSpeedLimit('down', TMenuItem(Sender).Tag);
End;

Procedure TMainForm.DoSetUploadSpeed(Sender: TObject);
Begin
  SetSpeedLimit('up', TMenuItem(Sender).Tag);
End;

Procedure TMainForm.SetSpeedLimit(Const Dir: String; Speed: Integer);
Var
  req, args: TJSONObject;
Begin
  AppBusy;
  req := TJSONObject.Create;
  Try
    req.Add('method', 'session-set');
    args := TJSONObject.Create;
    args.Add(Format('speed-limit-%s-enabled', [Dir]), Integer(Speed >= 0) And 1);
    If Speed >= 0 Then
      args.Add(Format('speed-limit-%s', [Dir]), Speed);
    args.Add('alt-speed-enabled', 0);
    req.Add('arguments', args);
    args := RpcObj.SendRequest(req, False);
    If args = nil Then
    Begin
      CheckStatus(False);
      exit;
    End;
  Finally
    args.Free;
    req.Free;
  End;
  RpcObj.RefreshNow := RpcObj.RefreshNow + [rtSession];
  AppNormal;
End;

Function TMainForm.FixSeparators(Const p: String): String;
Begin
  //if(unix) then
  //begin
  //  Result := StringReplace(p, '\', '/', [rfReplaceAll]);
  //end
  //else
  //begin
  Result := StringReplace(p, '/', DirectorySeparator, [rfReplaceAll]);
  Result := StringReplace(Result, '\', DirectorySeparator, [rfReplaceAll]);
  //end;
End;

Function TMainForm.MapRemoteToLocal(Const RemotePath: String): String;
Var
  i, j: Integer;
  s, ss, fn: String;
Begin
  Result := '';
  fn := FixSeparators(Trim(RemotePath));
  For i := 0 To FPathMap.Count - 1 Do
  Begin
    s := FPathMap[i];
    j := Pos('=', s);
    If j > 0 Then
    Begin
      ss := FixSeparators(Copy(s, 1, j - 1));
      If (ss = fn) Or (Pos(IncludeProperTrailingPathDelimiter(ss), fn) = 1) Then
      Begin
        If ss = fn Then
          ss := Copy(s, j + 1, MaxInt)
        Else
        Begin
          ss := IncludeProperTrailingPathDelimiter(ss);
          ss := IncludeTrailingPathDelimiter(Copy(s, j + 1, MaxInt)) +
            Copy(fn, Length(ss) + 1, MaxInt);
        End;
        Result := FixSeparators(ss);
        exit;
      End;
    End;
  End;
End;

Procedure TMainForm.UpdateUIRpcVersion(RpcVersion: Integer);
Var
  vc: Boolean;
Begin
  acRemoveTorrentAndData.Visible := RPCVersion >= 4;
  acReannounceTorrent.Visible := RPCVersion >= 5;
  acUpdateBlocklist.Visible := RPCVersion >= 5;
  acMoveTorrent.Visible := RPCVersion >= 6;
  pmiPriority.Visible := RPCVersion >= 5;
  miPriority.Visible := pmiPriority.Visible;
  acOpenContainingFolder.Visible := RPCVersion >= 4;
  acOpenFile.Visible := acOpenContainingFolder.Visible;
  pmSepOpen1.Visible := acOpenContainingFolder.Visible;
  pmSepOpen2.Visible := acOpenContainingFolder.Visible;
  MenuItem101.Visible := RPCVersion >= 7;

  vc := Not sepAltSpeed.Visible And (RPCVersion >= 5);
  sepAltSpeed.Visible := RPCVersion >= 5;
  acAltSpeed.Visible := RPCVersion >= 5;
  If vc Then
  Begin
    sepAltSpeed.Left := tbStopTorrent.Left + 1;
    tbtAltSpeed.Left := sepAltSpeed.Left + 1;
  End;

  acAddTracker.Visible := RPCVersion >= 10;
  acEditTracker.Visible := acAddTracker.Visible;
  acDelTracker.Visible := acAddTracker.Visible;
  acAdvEditTrackers.Visible := acAddTracker.Visible;
  sepTrackers.Visible := acAddTracker.Visible;
  vc := Not sepQueue.Visible And (RPCVersion >= 14);
  sepQueue.Visible := RPCVersion >= 14;
  acQMoveUp.Visible := RPCVersion >= 14;
  acQMoveDown.Visible := RPCVersion >= 14;
  miQueue.Visible := RPCVersion >= 14;
  pmiQueue.Visible := RPCVersion >= 14;
  If vc Then
  Begin
    sepQueue.Left := tbStopTorrent.Left + 1;
    tbQMoveUp.Left := sepQueue.Left + 1;
    tbQMoveDown.Left := tbQMoveUp.Left + 1;
  End;
  acForceStartTorrent.Visible := RPCVersion >= 14;
  tabStats.Visible := RpcVersion >= 4;
  acRename.Visible := RpcVersion >= 15;
End;

Procedure TMainForm.CheckAddTorrents;
Var
  i: Integer;
  h: System.THandle;
  s: String;
  WasHidden: Boolean;
Begin
  h := FileOpenUTF8(FIPCFileName, fmOpenRead Or fmShareDenyWrite);
  If h <> System.THandle(-1) Then
  Begin
    i := FileSeek(h, 0, soFromEnd);
    SetLength(s, i);
    If i > 0 Then
    Begin
      FileSeek(h, 0, soFromBeginning);
      SetLength(s, FileRead(h, s[1], i));
    End;
    FileTruncate(h, 0);
    FileClose(h);
    LazFileUtils.DeleteFileUTF8(FIPCFileName);

    If s = '' Then
    Begin
      ShowApp;
      exit;
    End;

    FPendingTorrents.Text := FPendingTorrents.Text + s;
  End;

  If FAddingTorrent <> 0 Then
    exit;

  Inc(FAddingTorrent);
  Try
    If FPendingTorrents.Count > 0 Then
    Begin
      Application.ProcessMessages;
      TickTimer.Enabled := True;
      WasHidden := Not IsTaskbarButtonVisible;
      If WasHidden Then
        Application.BringToFront
      Else
        ShowApp;
      Try
        While FPendingTorrents.Count > 0 Do
        Begin
          s := FPendingTorrents[0];
          FPendingTorrents.Delete(0);
          If s <> '' Then
            DoAddTorrent(s);
        End;
      Finally
        If WasHidden Then
          HideTaskbarButton;
        FWatchDownloading := False;
      End;
    End;
  Finally
    Dec(FAddingTorrent);
  End;
End;

Procedure TMainForm.CheckClipboardLink;
Const
  strTorrentExt = '.torrent';
Var
  s: String;
Begin
  Try
    If Not FLinksFromClipboard Then
      exit;
    s := Clipboard.AsText;
    If s = FLastClipboardLink Then
      exit;
    FLastClipboardLink := s;
    If isHash(s) Then s := 'magnet:?xt=urn:btih:' + s;
    If Not IsProtocolSupported(s) Then
      exit;
    If (Pos('magnet:', LazUTF8.UTF8LowerCase(s)) <> 1) And
      (LazUTF8.UTF8LowerCase(Copy(s, Length(s) - Length(strTorrentExt) + 1, MaxInt)) <>
      strTorrentExt) Then
      exit;

    AddTorrentFile(s);
    Clipboard.AsText := '';
  Except
    // Turn off this function if an error occurs
    FLinksFromClipboard := False;
  End;
End;

Procedure TMainForm.CenterDetailsWait;
Begin
  panDetailsWait.Left := PageInfo.Left + (PageInfo.Width -
    panDetailsWait.Width) Div 2;
  panDetailsWait.Top := PageInfo.Top + (PageInfo.Height -
    panDetailsWait.Height) Div 2;
End;

Function TMainForm.GetPageInfoType(pg: TTabSheet): TAdvInfoType;
Begin
  If pg = tabGeneral Then
    Result := aiGeneral
  Else
    If pg = tabPeers Then
      Result := aiPeers
    Else
      If pg = tabFiles Then
        Result := aiFiles
      Else
        If pg = tabTrackers Then
          Result := aiTrackers
        Else
          If pg = tabStats Then
            Result := aiStats
          Else
            Result := aiNone;
End;

Procedure TMainForm.DetailsUpdated;
Begin
  FDetailsWaitStart := 0;
  PageInfo.ActivePage.Tag := 0;
End;

Function TMainForm.RenameTorrent(TorrentId: Integer;
  Const OldPath, NewName: String): Boolean;
Var
  args: TJSONObject;
Begin
  Result := False;
  If ExtractFileName(OldPath) = NewName Then
    exit;
  args := TJSONObject.Create;
  args.Add('path', UTF8Decode(OldPath));
  args.Add('name', UTF8Decode(NewName));
  Result := TorrentAction(TorrentId, 'torrent-rename-path', args);
End;

Procedure TMainForm.FilesTreeStateChanged(Sender: TObject);
Begin
  SetCurrentFilePriority('');
End;

Function TMainForm.SelectTorrent(TorrentId, TimeOut: Integer): Integer;
Var
  tt: TDateTime;
  br: Boolean;
Begin
  Result := -1;
  If TorrentId = 0 Then
    exit;
  br := False;
  tt := Now;
  While True Do
  Begin
    Application.ProcessMessages;
    Result := gTorrents.Items.IndexOf(idxTorrentId, TorrentId);
    If Result >= 0 Then
    Begin
      gTorrents.RemoveSelection;
      gTorrents.Row := Result;
      RpcObj.CurTorrentId := TorrentId;
      If Self.Visible And (Self.WindowState <> wsMinimized) And
        gTorrents.Enabled Then
        Self.ActiveControl := gTorrents;
      break;
    End;
    If br Then
      break;
    Sleep(100);
    If Now - tt >= TimeOut / MSecsPerDay Then
      br := True;
  End;
End;

Procedure TMainForm.OpenCurrentTorrent(OpenFolderOnly: Boolean; UserDef: Boolean);
Var
  res: TJSONObject;
  p, p1, p2, s: String;
  sel: Boolean;
  i, Torrent: Integer;
  files: TJSONArray;
  TorrentIds: Variant;
  //  ids:array of integer;
  //  Count:integer;
Begin
  If gTorrents.Items.Count = 0 Then
    exit;
  Application.ProcessMessages;
  AppBusy;
  Try
    sel := False;
    //    gTorrents.RemoveSelection;


    //    res:=RpcObj.RequestInfo(gTorrents.Items[idxTorrentId, gTorrents.Row], ['files', 'downloadDir']);
    TorrentIds := GetSelectedTorrents();
    If UserDef Then
    Begin
      ExecRemoteFileArray('', sel, TorrentIds, Userdef);
      gTorrents.RemoveSelection;
      AppNormal;
      exit;
    End;
    For i := 0 To gTorrents.Items.Count - 1 Do
    Begin
      If (Not gTorrents.RowSelected[i]) And (i <> gTorrents.Row) Then
        continue;
      p := '';
      Torrent := gTorrents.Items[idxTorrentId, i];
      res := RpcObj.RequestInfo(gTorrents.Items[idxTorrentId, i],
        ['files', 'downloadDir']);
      If res = nil Then
        CheckStatus(False)
      Else
      Try
        With res.Arrays['torrents'].Objects[0] Do
        Begin
          If Not UserDef Then
          Begin
            files := Arrays['files'];
            If files.Count = 0 Then exit;

            If files.Count = 1 Then
            Begin
              p := UTF8Encode((files[0] As TJSONObject).Strings['name']);
              sel := OpenFolderOnly;
            End
            Else
            Begin
              //sel:=OpenFolderOnly; // bag? missed?
              s := GetFilesCommonPath(files);
              Repeat
                p := s;
                s := ExtractFilePath(p);
              Until (s = '') Or (s = p);
            End;
          End;
          p1 := IncludeTrailingPathDelimiter(
            UTF8Encode(Strings['downloadDir'])) + p;
          If (Not (FileExistsUTF8(MapRemoteToLocal(p1)) Or
            FileExistsUTF8(MapRemoteToLocal(p1 + '.part')) Or
            DirectoryExistsUTF8(MapRemoteToLocal(p1)))) And
            (RpcObj.IncompleteDir <> '') Then
          Begin
            p2 := IncludeTrailingPathDelimiter(
              UTF8Encode(RpcObj.IncompleteDir)) + p;
            If FileExistsUTF8(MapRemoteToLocal(p2)) Or
              FileExistsUTF8(MapRemoteToLocal(p2 + '.part')) Or
              DirectoryExistsUTF8(MapRemoteToLocal(p2)) Then
              p := p2
            Else
              p := p1;
          End
          Else
            p := p1;
        End;
      Finally
        res.Free;
      End;
      ExecRemoteFile(p, sel, Torrent, Userdef);
      Sleep(1000);
    End;
    gTorrents.RemoveSelection;


  Finally
    AppNormal;
  End;
End;

Procedure myDumpAddr(Addr: Pointer; Var f: system.Text);
Begin
  Try
    WriteLn(f, BackTraceStrFunc(Addr));
  Except
    writeLn(f, SysBackTraceStr(Addr));
  End;
End;

Procedure MyDumpExceptionBackTrace(Var f: system.Text);
Var
  FrameCount: Integer;
  Frames: PPointer;
  FrameNumber: Integer;
Begin
  WriteLn(f, 'Stack trace:');
  myDumpAddr(ExceptAddr, f);
  FrameCount := ExceptFrameCount;
  Frames := ExceptFrames;
  For FrameNumber := 0 To FrameCount - 1 Do
    myDumpAddr(Frames[FrameNumber], f);
End;

Procedure TMainForm._onException(Sender: TObject; E: Exception);
Var
  f: system.Text;
  crashreportfilename: Shortstring;
Begin
  crashreportfilename := 'crashreport.txt';
  system.Assign(f, crashreportfilename);
  If FileExists(crashreportfilename) Then
    system.Append(f)
  Else
    system.Rewrite(f);

  WriteLn(f, '');
  WriteLn(f, 'v.' + AppVersion + ' crashed((');
  WriteLn(f, '');
  myDumpExceptionBackTrace(f);
  system.Close(f);
  halt(0);
End;


Procedure TMainForm.FillSpeedsMenu;

  Procedure _FillMenu(Items: TMenuItem; Const Speeds: String;
    OnClickHandler: TNotifyEvent; CurSpeed: Integer);
  Var
    sl: TStringList;
    i, j: Integer;
    mi: TMenuItem;
  Begin
    Items.Clear;
    If Not RpcObj.Connected Then
      exit;
    sl := TStringList.Create;
    Try
      sl.Delimiter := ',';
      sl.DelimitedText := Speeds;
      i := 0;
      While i < sl.Count Do
      Begin
        j := StrToIntDef(Trim(sl[i]), -1);
        If j >= 0 Then
        Begin
          sl[i] := Format('%.08d', [j]);
          Inc(i);
        End
        Else
          sl.Delete(i);
      End;
      sl.Duplicates := dupIgnore;
      sl.Sorted := True;
      sl.Add(Format('%.08d', [CurSpeed]));

      For i := 0 To sl.Count - 1 Do
      Begin
        j := StrToIntDef(Trim(sl[i]), -1);
        If j >= 0 Then
        Begin
          mi := TMenuItem.Create(Items);
          mi.Caption := Format('%d %s%s', [j, sKByte, sPerSecond]);
          mi.Tag := j;
          mi.OnClick := OnClickHandler;
          If j = CurSpeed Then
            mi.Checked := True;
          Items.Insert(0, mi);
        End;
      End;
    Finally
      sl.Free;
    End;
    If Items.Count > 0 Then
    Begin
      mi := TMenuItem.Create(Items);
      mi.Caption := '-';
      Items.Insert(0, mi);
    End;
    mi := TMenuItem.Create(Items);
    mi.Caption := SUnlimited;
    mi.Tag := -1;
    mi.OnClick := OnClickHandler;
    If CurSpeed = -1 Then
      mi.Checked := True;
    Items.Insert(0, mi);
  End;

Var
  s: String;
Begin
  s := Ini.ReadString('Connection.' + FCurConn, 'DownSpeeds', DefSpeeds);
  _FillMenu(pmDownSpeeds.Items, s, @DoSetDownloadSpeed, FCurDownSpeedLimit);
  _FillMenu(pmiDownSpeedLimit, s, @DoSetDownloadSpeed, FCurDownSpeedLimit);

  s := Ini.ReadString('Connection.' + FCurConn, 'UpSpeeds', DefSpeeds);
  _FillMenu(pmUpSpeeds.Items, s, @DoSetUploadSpeed, FCurUpSpeedLimit);
  _FillMenu(pmiUpSpeedLimit, s, @DoSetUploadSpeed, FCurUpSpeedLimit);
  {$ifdef LCLcarbon}
  TrayIcon.InternalUpdate;
  {$endif LCLcarbon}
End;

Initialization
  {$I main.lrs}


Finalization
Try
  FreeAndNil(Ini);
Except
End;
End.
