unit ParseJSONHistoryFromTelegramForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls, Spin, ComCtrls, IniPropStorage, ExtCtrls,
  ValEdit, DateTimePicker, SpinEx, fgl, fpjson, eventlog, Grids
  ;

type
  TTaskMessageType = (tmtNone, tmtText, tmtPhoto, tmtVideo, tmtAudio, tmtVoice, tmtVideoNote, tmtDocument); 
  TTaskMessageSet = set of TTaskMessageType;

  TUserDataRec = record
    Name: String;
    Count: Integer;
  end;

  { TGroupUsers }

  TGroupUsers = class(specialize TFPGMap<Int64, TUserDataRec>)
  public
    function GetCount(aID: Int64): Integer;
    procedure AddToUserList(aMsgObject: TJSONObject);
    procedure AddToUserList(const aName: String; aUserID: Int64);
    procedure SaveList(const aFileName: String);                 
    procedure LoadList(const aFileName: String);
  end;

  { TFrmMain }

  TFrmMain = class(TForm)
    Button1: TButton;
    BtnCompletedUsersLoad: TButton;
    Button3: TButton;
    ChckBxStrictFilter: TCheckBox;
    ChckBxStatLogger: TCheckBox;
    ChckBxAltDocAsMedia: TCheckBox;
    DateTimePickerN: TDateTimePicker;
    DrctryEdtStat: TDirectoryEdit;
    FlNmEdtCompletedUsers: TFileNameEdit;
    FlNmEdtCuratorsFile: TFileNameEdit;
    FlNmEdtAllUsers: TFileNameEdit;
    IniPropStorage1: TIniPropStorage;
    LblTopicID: TLabel;
    LblStat: TLabel;
    LblFailed: TLabel;
    LblCurators: TLabel;
    LblFileNameAllUsers: TLabel;
    LblFileNameCurators: TLabel;
    LblResultJSONStat: TLabel;
    LblAllUsers: TLabel;
    LblStatUpdateCount: TLabel;
    LblStatFilesCount: TLabel;
    LblTopicID1: TLabel;
    LblGroupID: TLabel;
    PgCntrl: TPageControl;
    RdGrpTaskNumFilter: TRadioGroup;
    SpnEdtTaskNum: TSpinEdit;
    SpnEdtGroupID: TSpinEditEx;
    SpnEdtForumIDN: TSpinEdit;
    TbShtMain: TTabSheet;
    VlLstEdtrMediaTypes: TValueListEditor;
    procedure Button1Click({%H-}Sender: TObject);
    procedure BtnCompletedUsersLoadClick({%H-}Sender: TObject);
    procedure Button3Click({%H-}Sender: TObject);
    procedure DrctryEdtStatChange({%H-}Sender: TObject);
    procedure FlNmEdtAllUsersChange({%H-}Sender: TObject);
    procedure FlNmEdtCompletedUsersChange(Sender: TObject);
    procedure FlNmEdtCuratorsFileChange({%H-}Sender: TObject);
    procedure FormCreate({%H-}Sender: TObject);
    procedure FormDestroy({%H-}Sender: TObject);
    procedure VlLstEdtrMediaTypesValidateEntry({%H-}Sender: TObject; {%H-}aCol, {%H-}aRow: Integer; const OldValue: string;
      var NewValue: String);
  private
    FCompletedUsers, FFailedUsers, FCuratorsUsers: TGroupUsers;
    FJSONData: TJSONObject;
    FAllUsers: TGroupUsers;
    FLog: TEventLog;
    function ForumID: Integer;
    function GroupID: Int64;
    function ForumDeadLine: Int64;
    function GetMediaCount(i: TTaskMessageType): Integer;
    procedure LoadUsers(aUsers: TGroupUsers; aLabel: TLabel; aFileNameEdit: TFileNameEdit);
    procedure ParseCompleted;
    procedure SetMediaCount(i: TTaskMessageType; AValue: Integer);
    procedure UpdateCheckGroupMedias;           
    function GetCheckGroupMedias: TTaskMessageSet;
  public
    property MediaCount [i: TTaskMessageType]: Integer read GetMediaCount write SetMediaCount;
  end;



var
  FrmMain: TFrmMain;

implementation

uses
  jsonparser, jsonscanner, StrUtils, FileUtil, DateUtils, tgtypes
  ;

const
  _MediaTypeNames: array [TTaskMessageType] of String = ('', 'text', 'photo', 'video', 'audio', 'voice',
    'video_note', 'document');

{$R *.lfm}

procedure ExtractFromMsg(aMsgObject: TJSONObject; out aID: Int64; out aName: String);
begin
  with aMsgObject.objects['from'] do
  begin
    aID:=Int64s['id'];
    aName:=Strings['first_name']+' '+Get('last_name', EmptyStr);
  end;
end;

{ TGroupUsers }

function TGroupUsers.GetCount(aID: Int64): Integer;
var
  aIndex: Integer;
begin
  aIndex:=IndexOf(aID);
  if aIndex=-1 then
    Exit(0);
  Result:=Data[aIndex].Count;
end;

procedure TGroupUsers.AddToUserList(aMsgObject: TJSONObject);
begin
  with aMsgObject.Objects['from'] do
    AddToUserList(Strings['first_name']+' '+Get('last_name', EmptyStr), aMsgObject.objects['from'].Int64s['id'])
end;

procedure TGroupUsers.AddToUserList(const aName: String; aUserID: Int64);
var
  aData: TUserDataRec;
  aIndex: Integer;
begin
  if aUserID=0 then
    Exit;
  aIndex:=IndexOf(aUserID);
  if aIndex>-1 then
  begin
    aData:=Data[aIndex];
    Inc(aData.Count);
    Data[aIndex]:=aData;
  end
  else begin
    aData.Name:=aName;
    aData.Count:=1;
    Add(aUserID, aData);
  end;
end;

procedure TGroupUsers.SaveList(const aFileName: String);
var
  aCSV: TStringList;
  i: Integer;
begin
  aCSV:=TStringList.Create;
  try
    for i:=0 to Self.Count-1 do
      aCSV.Add(Format('%d; %s', [Keys[i], Data[i].Name]));
    aCSV.Sort;
    aCSV.SaveToFile(aFileName);
  finally
    aCSV.Free;
  end;
end;

procedure TGroupUsers.LoadList(const aFileName: String);
var
  aStringList: TStringList;
  s, aID, aName: String;
begin
  Clear;
  aStringList:=TStringList.Create;
  try
    aStringList.LoadFromFile(aFileName);
    try
      for s in aStringList do
      begin
        aID:=ExtractDelimited(1, s, [' ', ';', ',']);
        aName:=TrimSet(RightStr(s, Length(s)-Length(aID)), StdWordDelims+[' ']);
        AddToUserList(aName, StrToInt64(aID));
      end;
    except
      Clear;
    end;
  finally
    aStringList.Free;
  end;
end;

{ TFrmMain }
{
procedure TFrmMain.Button1Click(Sender: TObject);
var
  aMessages: TJSONArray;
  aMsg: TJSONEnum;
  aMsgObject: TJSONObject;

  procedure AddMembers(aMembers: TJSONArray);
  var
    i: TJSONEnum;
  begin
    for i in aMembers do
      AddToUserList((i.Value as TJSONString).AsString, EmptyStr);
  end;

  procedure ServiceMessage(aMsgObject: TJSONObject);
  begin
    case aMsgObject.Strings['action'] of
      'join_group_by_link': AddToUserList(aMsgObject.Strings['actor'], aMsgObject.Strings['actor_id']);
      'invite_members':     AddMembers(aMsgObject.Arrays['members']);
    end;
  end;

begin
  FCompletedUsers.Clear;
  aMessages:=FJSONData.Arrays['messages'];
  for aMsg in aMessages do
  begin
    aMsgObject:=aMsg.Value as TJSONObject;
    case aMsgObject.Strings['type'] of
      'service': ServiceMessage(aMsgObject);
      'message': AddMsgSenderToUserList(aMsgObject);
    end;
  end;
  FCompletedUsers.SaveList('~users.csv');
end;     }

procedure TFrmMain.Button3Click(Sender: TObject);
var
  aFound: Boolean;
  i: Integer;
  aID: Int64;
  aName: String;
begin
  FFailedUsers.Clear;
  for i:=0 to FAllUsers.Count-1 do
  begin
    aID:=FAllUsers.Keys[i];
    aName:=FAllUsers.Data[i].Name;
    aFound:=FCompletedUsers.IndexOf(aID)>-1;
    if not aFound then
      if FCuratorsUsers.IndexOf(aID)=-1 then
        FFailedUsers.AddToUserList(aName, aID)
      else
        FLog.Warning('#%d: %s', [aID, aName]);
  end;  
  FFailedUsers.SaveList('~failed.csv');
  LblFailed.Caption:=Format('Failed users: %d', [FFailedUsers.Count]);
end;

procedure TFrmMain.DrctryEdtStatChange(Sender: TObject);
var
  aUpdateList, aFiles: TStringList;
  aFile, aUpdate, s: String;
begin
  FreeAndNil(FJSONData);
  LblStatUpdateCount.Caption:='--';
  LblStatFilesCount.Caption:='--';
  if DrctryEdtStat.Directory.IsEmpty then
    Exit;
  aFiles:=TStringList.Create;
  FJSONData:=TJSONObject.Create(['updates', TJSONArray.Create]);
  try
    try
      FindAllFiles(aFiles, DrctryEdtStat.Directory, '*.log');
      for aFile in aFiles do
      begin
        aUpdateList:=TStringList.Create;
        try
          aUpdateList.LoadFromFile(aFile);
          for s in aUpdateList do
          begin
            aUpdate:=Trim(Copy(s, 22, Length(s)-22+1));
            FJSONData.Arrays['updates'].Add(GetJSON(aUpdate) as TJSONObject);
          end;
        finally
          aUpdateList.Free;
        end;
      end;
    finally                                                                                
      LblStatFilesCount.Caption:=Format('Files: %d', [aFiles.Count]);
      aFiles.Free;
    end;
    LblStatUpdateCount.Caption:=Format('Updates: %d', [FJSONData.Arrays['updates'].Count]);
    ChckBxStatLogger.Checked:=True;
  except
    FreeAndNil(FJSONData);
  end;
end;

procedure TFrmMain.Button1Click(Sender: TObject);
begin
  ParseCompleted;
end;

procedure TFrmMain.BtnCompletedUsersLoadClick(Sender: TObject);
begin
  FCompletedUsers.LoadList(FlNmEdtCompletedUsers.FileName);
  BtnCompletedUsersLoad.Caption:=Format('Completed users: %d', [FCompletedUsers.Count]);
end;

procedure TFrmMain.FlNmEdtAllUsersChange(Sender: TObject);
begin
  LoadUsers(FAllUsers, LblAllUsers, FlNmEdtAllUsers);
end;

procedure TFrmMain.FlNmEdtCompletedUsersChange(Sender: TObject);
begin
  FCompletedUsers.LoadList((Sender as TFileNameEdit).FileName); 
  BtnCompletedUsersLoad.Caption:=Format('Completed users: %d', [FCompletedUsers.Count]);
end;

procedure TFrmMain.FlNmEdtCuratorsFileChange(Sender: TObject);
begin
  LoadUsers(FCuratorsUsers, LblCurators, FlNmEdtCuratorsFile);
end;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  FJSONData:=TJSONObject.Create;
  FCompletedUsers:=TGroupUsers.Create;
  FFailedUsers:=TGroupUsers.Create;
  FCuratorsUsers:=TGroupUsers.Create;
  FAllUsers:=TGroupUsers.Create;
  FLog:=TEventLog.Create(Self);
  FLog.LogType:=ltFile;
  FLog.FileName:=ChangeFileExt(ApplicationName, '.log');
  UpdateCheckGroupMedias;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  FLog.Free;
  FCuratorsUsers.Free;
  FAllUsers.Free;
  FFailedUsers.Free;
  FCompletedUsers.Free;
  FJSONData.Free;
end;

procedure TFrmMain.VlLstEdtrMediaTypesValidateEntry(Sender: TObject; aCol, aRow: Integer; const OldValue: string;
  var NewValue: String);
var
  TempInt: Longint;
begin
  if not TryStrToInt(Trim(NewValue), TempInt) then
  begin
    NewValue:=OldValue;
    raise Exception.Create('Введите целое число');
  end;
  NewValue:=TempInt.ToString;
end;

function TFrmMain.ForumID: Integer;
begin
  Result:=SpnEdtForumIDN.Value
end;

function TFrmMain.GroupID: Int64;
begin
  Result:=SpnEdtGroupID.Value;
end;

function TFrmMain.ForumDeadLine: Int64;
var
  aDeadLine: TDateTime;
begin
  aDeadLine:=DateTimePickerN.DateTime;
  Result:=DateTimeToUnix(aDeadLine, False)
end;

function TFrmMain.GetMediaCount(i: TTaskMessageType): Integer;
begin
  Result:=StrToInt(VlLstEdtrMediaTypes.Cells[1, Ord(i)]);
end;

procedure TFrmMain.LoadUsers(aUsers: TGroupUsers; aLabel: TLabel; aFileNameEdit: TFileNameEdit);
begin
  aLabel.Caption:='--';
  if aFileNameEdit.FileName.IsEmpty then
    Exit;
  aUsers.LoadList(aFileNameEdit.FileName);
  aLabel.Caption:=Format('Users: %d', [aUsers.Count]);
  aUsers.SaveList('~'+RightStr(aLabel.Name, Length(aLabel.Name)-Length('Lbl'))+'.csv');
end;

procedure TFrmMain.ParseCompleted;
var
  aUpdates: TJSONArray;
  aUpdate: TJSONEnum;
  aMsgObject: TJSONObject;
  aMediaMsgUsers, aTxtMsgUsers, aVideoNotes, aPhotoMsgUsers, aVideos, aDocuments: TGroupUsers;
  aThreadID: Integer;
  aDeadLine, aGroupID: Int64;
  aTxt: String;

  function GetListFromMedia(aMedia: TTaskMessageType): TGroupUsers;
  begin
    case aMedia of
      tmtText:      Result:=aTxtMsgUsers;
      tmtPhoto:     Result:=aPhotoMsgUsers;
      tmtVideo:     Result:=aVideos;
      tmtVideoNote: Result:=aVideoNotes;
      tmtDocument:  Result:=aDocuments;
    else
      Result:=nil;
    end;
  end;

  procedure HandleMessageN;
  var
    aID: Int64;
    aName: String;
    aMSet: TTaskMessageSet;
    m: TTaskMessageType;
    aAddToList: Boolean;
  begin
    if ChckBxStrictFilter.Checked then
    begin
      ExtractFromMsg(aMsgObject, aID, aName);
      aMSet:=GetCheckGroupMedias;
      aAddToList:=True;
      for m in aMSet do
      begin
        if m in aMSet then
        begin
          if (aMsgObject.IndexOfName(_MediaTypeNames[m])>-1) then
            GetListFromMedia(m).AddToUserList(aName, aID)
          else
            if ChckBxAltDocAsMedia.Checked and (aMsgObject.IndexOfName(_MediaTypeNames[tmtDocument])>-1) then
              GetListFromMedia(m).AddToUserList(aName, aID);
          aAddToList:=aAddToList and (GetListFromMedia(m).GetCount(aID)>=MediaCount[m]);
        end;
      end;
      if aAddToList then
        FCompletedUsers.AddToUserList(aMsgObject);
    end
    else
      FCompletedUsers.AddToUserList(aMsgObject);
  end;

  function ExtractMessage(aUpdate: TJSONData; out aMsgObject: TJSONObject): Boolean;
  begin
    with aUpdate as TJSONObject do
      Result:=Find('message', aMsgObject) or Find('edited_message', aMsgObject);
  end;

begin
  aThreadID:=ForumID;
  aGroupID:=GroupID;
  aUpdates:=FJSONData.Arrays['updates'];
  FCompletedUsers.Clear;
  aDeadLine:=ForumDeadLine;
  aVideoNotes:=TGroupUsers.Create;
  aMediaMsgUsers:=TGroupUsers.Create;   
  aTxtMsgUsers:=TGroupUsers.Create;
  aPhotoMsgUsers:=TGroupUsers.Create;
  aVideos:=TGroupUsers.Create;
  aDocuments:=TGroupUsers.Create;
  try
    for aUpdate in aUpdates do
    begin
      if not ExtractMessage(aUpdate.Value, aMsgObject) then
        Continue;
      if aMsgObject.Objects['chat'].Int64s['id']<>aGroupID then
        Continue;
      if RdGrpTaskNumFilter.ItemIndex=0 then
      begin
        if aMsgObject.Get('message_thread_id', 0)<>aThreadID then
          COntinue;
      end
      else begin
        aTxt:=aMsgObject.Get('text', EmptyStr);
        if aTxt.IsEmpty then
          aTxt:=aMsgObject.Get('caption', EmptyStr);
        if aTxt.IsEmpty then
          Continue;
        if Pos('#'+SpnEdtTaskNum.Value.ToString, aTxt)=0 then
          COntinue;
      end;
      if aMsgObject.Int64s['date']>aDeadLine then
        Continue;
      HandleMessageN
    end;
    BtnCompletedUsersLoad.Caption:=Format('Completed users: %d', [FCompletedUsers.Count]);
    FCompletedUsers.SaveList(FlNmEdtCompletedUsers.FileName);
  finally
    aDocuments.Free;
    aVideos.Free;
    aTxtMsgUsers.Free;
    aMediaMsgUsers.Free;
    aVideoNotes.Free;
    aPhotoMsgUsers.Free;
  end;
end;

procedure TFrmMain.SetMediaCount(i: TTaskMessageType; AValue: Integer);
begin
  VlLstEdtrMediaTypes.Cells[1, Ord(i)]:=AValue.ToString;
end;

procedure TFrmMain.UpdateCheckGroupMedias;
var
  i: TTaskMessageType;
begin
  VlLstEdtrMediaTypes.RowCount:=Length(_MediaTypeNames);
  for i:=Succ(Low(_MediaTypeNames)) to High(_MediaTypeNames) do
  begin
    VlLstEdtrMediaTypes.Cells[0, Ord(i)]:=_MediaTypeNames[i];
    VlLstEdtrMediaTypes.Cells[1, Ord(i)]:='0';
  end;
end;

function TFrmMain.GetCheckGroupMedias: TTaskMessageSet;
var
  m: TTaskMessageType;
  aCount: LongInt;
begin
  Result:=[];
  for m in TTaskMessageType do
  begin
    aCount:=StrToIntDef(VlLstEdtrMediaTypes.Cells[1, Ord(m)], 0);
    if aCount>0 then
      Include(Result, m);
  end;
end;

end.

