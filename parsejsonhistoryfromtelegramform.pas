unit ParseJSONHistoryFromTelegramForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls, Spin, ComCtrls, IniPropStorage, ExtCtrls,
  DateTimePicker, fgl, fpjson, eventlog
  ;

type
  TTaskMessageType = (tmtNone, tmtText, tmtPhoto, tmtVideo, tmtAudio, tmtVoice, tmtVideoNote, tmtDocument); 
  TTaskMessageSet = set of TTaskMessageType;

  { TGroupUsers }

  TGroupUsers = class(specialize TFPGMap<Int64, String>)
  private
    procedure AddToUserList(const aName, aActorID: String);
  public                                                     
    procedure AddToUserList(aMsgObject: TJSONObject);
    procedure AddToUserList(const aName: String; aUserID: Int64);
    procedure SaveList(const aFileName: String);
  end;

  { TFrmMain }

  TFrmMain = class(TForm)
    Button1: TButton;
    Button3: TButton;
    ChckBxStrictFilter: TCheckBox;
    ChckBxStatLogger: TCheckBox;
    ChckBxSIngleGroup: TCheckBox;
    ChckGrpMediaTypes: TCheckGroup;
    DateTimePicker2: TDateTimePicker;
    DateTimePicker4: TDateTimePicker;
    DateTimePicker3: TDateTimePicker;
    DateTimePicker1: TDateTimePicker;
    DateTimePicker5: TDateTimePicker;
    DateTimePicker8: TDateTimePicker;
    DateTimePickerN: TDateTimePicker;
    DateTimePicker9: TDateTimePicker;
    DrctryEdtStat: TDirectoryEdit;
    FlNmEdtCuratorsFile: TFileNameEdit;
    FlNmEdtResultJSON: TFileNameEdit;
    FlNmEdtAllUsers: TFileNameEdit;
    IniPropStorage1: TIniPropStorage;
    Lbl9: TLabel;
    LblTopicID: TLabel;
    Lbl8: TLabel;
    LblStat: TLabel;
    LblFailed: TLabel;
    LblCurators: TLabel;
    LblCompletedUsers: TLabel;
    LblFileNameAllUsers: TLabel;
    LblFileNameCurators: TLabel;
    LblForumID: TLabel;
    LblResultJSONStat: TLabel;
    LblFileNameResultJSON: TLabel;
    LblAllUsers: TLabel;
    LblStatUpdateCount: TLabel;
    LblStatFilesCount: TLabel;
    LblTopicID1: TLabel;
    PgCntrl: TPageControl;
    SpnEdtForumID1: TSpinEdit;
    SpnEdtForumID3: TSpinEdit;
    SpnEdtForumID4: TSpinEdit;
    SpnEdtForumID5: TSpinEdit;
    SpnEdtForumID8: TSpinEdit;
    SpnEdtForumIDN: TSpinEdit;
    SpnEdtForumID9: TSpinEdit;
    SpnEdtTaskNum: TSpinEdit;
    SpnEdtForumID2: TSpinEdit;
    TbShtMain: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button1Click({%H-}Sender: TObject);
    procedure Button3Click({%H-}Sender: TObject);
    procedure DrctryEdtStatChange({%H-}Sender: TObject);
    procedure FlNmEdtAllUsersChange({%H-}Sender: TObject);
    procedure FlNmEdtCuratorsFileChange({%H-}Sender: TObject);
    procedure FlNmEdtResultJSONChange({%H-}Sender: TObject);
    procedure FormCreate({%H-}Sender: TObject);
    procedure FormDestroy({%H-}Sender: TObject);
  private
    FCompletedUsers, FFailedUsers, FCuratorsUsers: TGroupUsers;
    FJSONData: TJSONObject;
    FAllUsers: TGroupUsers;
    FLog: TEventLog;
    function ForumID(aTaskNum: Byte): Integer; 
    function ForumDeadLine(aTaskNum: Byte): Int64;
    procedure LoadUsers(aUsers: TGroupUsers; aLabel: TLabel; aFileNameEdit: TFileNameEdit);
    procedure ParseCompleted(aTaskNum: Integer);
    procedure UpdateCheckGroupMedias;           
    function GetCheckGroupMedias: TTaskMessageSet;
  public

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

procedure TGroupUsers.AddToUserList(aMsgObject: TJSONObject);
begin
  with aMsgObject.Objects['from'] do
    AddToUserList(Strings['first_name']+' '+Get('last_name', EmptyStr), aMsgObject.objects['from'].Int64s['id'])
end;

procedure TGroupUsers.AddToUserList(const aName, aActorID: String);
var
  aUserID: Int64;
begin
  if aActorID.StartsWith('channel') then
    Exit;
  if not aActorID.IsEmpty then
    aUserID:=StrToInt64(RightStr(aActorID, Length(aActorID)-Length('user')))
  else
    aUserID:=0;
  AddToUserList(aName, aUserID);
end;

procedure TGroupUsers.AddToUserList(const aName: String; aUserID: Int64);
begin
  if (aUserID<>0) and (Self.IndexOf(aUserID)>-1) then
    Exit;
  Self.Add(aUserID, aName);
end;

procedure TGroupUsers.SaveList(const aFileName: String);
var
  aCSV: TStringList;
  i: Integer;
begin
  aCSV:=TStringList.Create;
  try
    for i:=0 to Self.Count-1 do
      aCSV.Add(Format('%d; %s', [Keys[i], Data[i]]));
    aCSV.Sort;
    aCSV.SaveToFile(aFileName);
  finally
    aCSV.Free;
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
    aName:=FAllUsers.Data[i];
    aFound:=FCompletedUsers.IndexOf(aID)>-1;
    if not aFound then
      if FCuratorsUsers.IndexOf(aID)=-1 then
        FFailedUsers.AddToUserList(aName, aID)
      else
        FLog.Warning('#%d: %s', [aID, aName]);
  end;  
  FFailedUsers.SaveList(Format('~failed%d.csv', [SpnEdtTaskNum.Value]));
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
  ParseCompleted(SpnEdtTaskNum.Value);
end;

procedure TFrmMain.FlNmEdtAllUsersChange(Sender: TObject);
begin
  LoadUsers(FAllUsers, LblAllUsers, FlNmEdtAllUsers);
end;

procedure TFrmMain.FlNmEdtCuratorsFileChange(Sender: TObject);
begin
  LoadUsers(FCuratorsUsers, LblCurators, FlNmEdtCuratorsFile);
end;

procedure TFrmMain.FlNmEdtResultJSONChange(Sender: TObject);
var
  aStringList: TStringList;
  aJSONData: TJSONObject;
begin
  LblResultJSONStat.Caption:='--';
  if FlNmEdtResultJSON.FileName.IsEmpty then
    Exit;
  aStringList:=TStringList.Create;
  try
    aStringList.LoadFromFile(FlNmEdtResultJSON.FileName);
    try
      aJSONData:=GetJSON(aStringList.Text) as TJSONObject;
      LblResultJSONStat.Caption:=Format('Rows: %d', [FJSONData.Arrays['rows'].Count]);
    except
      FreeAndNil(aJSONData);
    end;
  finally
    aStringList.Free;
  end;
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

function TFrmMain.ForumID(aTaskNum: Byte): Integer;
begin
  if ChckBxSIngleGroup.Checked then
    Result:=SpnEdtForumIDN.Value
  else
    case aTaskNum of
      1: Result:=SpnEdtForumID1.Value;
      2: Result:=SpnEdtForumID2.Value;
      3: Result:=SpnEdtForumID3.Value;
      4: Result:=SpnEdtForumID4.Value;
      5: Result:=SpnEdtForumID5.Value;
      8: Result:=SpnEdtForumID8.Value;  
      9: Result:=SpnEdtForumID9.Value;
    else
      raise Exception.Create(Format('Uknown task number %d', [aTaskNum]));
    end;
end;

function TFrmMain.ForumDeadLine(aTaskNum: Byte): Int64;
var
  aDeadLine: TDateTime;
begin
  if ChckBxSIngleGroup.Checked then
    aDeadLine:=DateTimePickerN.DateTime
  else
    case aTaskNum of
      1: aDeadLine:=DateTimePicker1.DateTime;
      2: aDeadLine:=DateTimePicker2.DateTime;
      3: aDeadLine:=DateTimePicker3.DateTime;
      4: aDeadLine:=DateTimePicker4.DateTime;
      5: aDeadLine:=DateTimePicker5.DateTime;
      8: aDeadLine:=DateTimePicker8.DateTime;
      9: aDeadLine:=DateTimePicker9.DateTime;
    end;
  Result:=DateTimeToUnix(aDeadLine, False)
end;

procedure TFrmMain.LoadUsers(aUsers: TGroupUsers; aLabel: TLabel; aFileNameEdit: TFileNameEdit);
var
  aStringList: TStringList;
  s, aID, aName: String;
begin
  aUsers.Clear;
  aLabel.Caption:='--';
  if aFileNameEdit.FileName.IsEmpty then
    Exit;
  aStringList:=TStringList.Create;
  try
    aStringList.LoadFromFile(aFileNameEdit.FileName);
    try
      for s in aStringList do
      begin
        aID:=ExtractDelimited(1, s, [' ', ';', ',']);
        aName:=TrimSet(RightStr(s, Length(s)-Length(aID)), StdWordDelims+[' ']);
        aUsers.AddToUserList(aName, StrToInt64(aID));
      end;
      aLabel.Caption:=Format('Users: %d', [aUsers.Count]);
      aUsers.SaveList('~'+RightStr(aLabel.Name, Length(aLabel.Name)-Length('Lbl'))+'.csv');
    except
      aUsers.Clear;
    end;
  finally
    aStringList.Free;
  end;
end;

procedure TFrmMain.ParseCompleted(aTaskNum: Integer);
var
  aUpdates: TJSONArray;
  aUpdate: TJSONEnum;
  aMsgObject: TJSONObject;
  aMediaMsgUsers, aTxtMsgUsers, aVideoNotes, aPhotoMsgUsers, aVideos: TGroupUsers;
  aThreadID: Integer;
  aDeadLine: Int64;

  function GetListFromMedia(aMedia: TTaskMessageType): TGroupUsers;
  begin
    case aMedia of
      tmtText:      Result:=aTxtMsgUsers;
      tmtPhoto:     Result:=aPhotoMsgUsers;
      tmtVideo:     Result:=aVideos;
      tmtVideoNote: Result:=aVideoNotes;
    else
      Result:=nil;
    end;
  end;

  procedure HandleMessage1;
  var
    aID: Int64;
    aName: String;
  begin
    if ChckBxStrictFilter.Checked then
    begin
      ExtractFromMsg(aMsgObject, aID, aName);
      if (aMsgObject.IndexOfName('video_note')>-1) or (aMsgObject.IndexOfName('audio')>-1) or
        (aMsgObject.IndexOfName('voice')>-1) or (aMsgObject.IndexOfName('document')>-1) or
        (aMsgObject.IndexOfName('video')>-1) or (aMsgObject.IndexOfName('photo')>-1) then
        if aMsgObject.Int64s['date']<=aDeadLine then
          FCompletedUsers.AddToUserList(aName, aID);
    end
    else
      FCompletedUsers.AddToUserList(aMsgObject);
  end;

  procedure HandleMessage2;
  var
    aID: Int64;
    aName: String;
    aIsTextExists: Boolean;
  begin
    if ChckBxStrictFilter.Checked then
    begin
      ExtractFromMsg(aMsgObject, aID, aName);
      if (aMsgObject.IndexOfName('video_note')>-1) or (aMsgObject.IndexOfName('audio')>-1) or
        (aMsgObject.IndexOfName('voice')>-1) or (aMsgObject.IndexOfName('document')>-1) or
        (aMsgObject.IndexOfName('video')>-1) or (aMsgObject.IndexOfName('photo')>-1) then
      begin
        if (aTxtMsgUsers.IndexOf(aID)>-1) or (aMsgObject.IndexOfName('caption')>-1) then
          FCompletedUsers.AddToUserList(aName, aID)
        else
          aMediaMsgUsers.AddToUserList(aName, aID);
      end
      else begin
        aIsTextExists:=aMsgObject.IndexOfName('text')>-1;
        if aIsTextExists then
        begin
          if aMediaMsgUsers.IndexOf(aID)>-1 then
            FCompletedUsers.AddToUserList(aName, aID)
          else
            aTxtMsgUsers.AddToUserList(aName, aID);
        end;
      end;
    end
    else
      FCompletedUsers.AddToUserList(aMsgObject);
  end;

  procedure HandleMessage3;
  var
    aID: Int64;
    aName: String;
  begin
    if ChckBxStrictFilter.Checked then
    begin
      ExtractFromMsg(aMsgObject, aID, aName);
      if (aMsgObject.IndexOfName('photo')>-1) then
        FCompletedUsers.AddToUserList(aName, aID)
    end
    else
      FCompletedUsers.AddToUserList(aMsgObject);
  end;

  procedure HandleMessage4;
  var
    aID: Int64;
    aName: String;
  begin
    if ChckBxStrictFilter.Checked then
    begin
      ExtractFromMsg(aMsgObject, aID, aName);
      if (aMsgObject.IndexOfName('video')>-1) or (aMsgObject.IndexOfName('document')>-1) then
        FCompletedUsers.AddToUserList(aName, aID)
    end
    else
      FCompletedUsers.AddToUserList(aMsgObject);
  end;

  procedure HandleMessage5;
  var
    aID: Int64;
    aName: String;
  begin
    if ChckBxStrictFilter.Checked then
    begin
      ExtractFromMsg(aMsgObject, aID, aName);
      if (aMsgObject.IndexOfName('video_note')>-1) then
        aVideoNotes.AddToUserList(aName, aID);
      if (aMsgObject.IndexOfName('video')>-1) then
        aMediaMsgUsers.AddToUserList(aName, aID);
      if (aMsgObject.IndexOfName('photo')>-1) then
        aPhotoMsgUsers.AddToUserList(aName, aID);
      if (aMediaMsgUsers.IndexOf(aID)>-1) and (aPhotoMsgUsers.IndexOf(aID)>-1) and (aVideoNotes.IndexOf(aID)>-1) then
        FCompletedUsers.AddToUserList(aMsgObject);
    end
    else
      FCompletedUsers.AddToUserList(aMsgObject);
  end;

  procedure HandleMessage8;
  var
    aID: Int64;
    aName: String;
  begin
    if ChckBxStrictFilter.Checked then
    begin
      ExtractFromMsg(aMsgObject, aID, aName);
      if (aMsgObject.IndexOfName('voice')>-1) then
        aMediaMsgUsers.AddToUserList(aName, aID);
      if (aMsgObject.IndexOfName('text')>-1) then
        aTxtMsgUsers.AddToUserList(aName, aID);
      if (aMsgObject.IndexOfName('photo')>-1) then
      begin
        if aPhotoMsgUsers.IndexOf(aID)>-1 then
          aTxtMsgUsers.AddToUserList(aName, aID)
        else
          aPhotoMsgUsers.AddToUserList(aName, aID);
      end;
      if (aMediaMsgUsers.IndexOf(aID)>-1) and (aPhotoMsgUsers.IndexOf(aID)>-1) and (aTxtMsgUsers.IndexOf(aID)>-1) then
        FCompletedUsers.AddToUserList(aMsgObject);
      if (aMediaMsgUsers.IndexOf(aID)>-1) and (aPhotoMsgUsers.IndexOf(aID)>-1) and (aVideoNotes.IndexOf(aID)>-1) then
        FCompletedUsers.AddToUserList(aMsgObject);
    end
    else
      FCompletedUsers.AddToUserList(aMsgObject);
  end;

  procedure HandleMessage9;
  var
    aID: Int64;
    aName: String;
  begin
    if ChckBxStrictFilter.Checked then
    begin
      ExtractFromMsg(aMsgObject, aID, aName);
      if (aMsgObject.IndexOfName('photo')>-1) then
        aPhotoMsgUsers.AddToUserList(aName, aID);
      if (aMsgObject.IndexOfName('video')>-1) then
        aVideos.AddToUserList(aName, aID);

      if (aPhotoMsgUsers.IndexOf(aID)>-1) and (aVideos.IndexOf(aID)>-1) then
        FCompletedUsers.AddToUserList(aMsgObject);
    end
    else
      FCompletedUsers.AddToUserList(aMsgObject);
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
            GetListFromMedia(m).AddToUserList(aName, aID);
          aAddToList:=aAddToList and (GetListFromMedia(m).IndexOf(aID)>-1);
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
  aThreadID:=ForumID(aTaskNum);
  aUpdates:=FJSONData.Arrays['updates'];
  FCompletedUsers.Clear;
  aDeadLine:=ForumDeadLine(aTaskNum);
  aVideoNotes:=TGroupUsers.Create;
  aMediaMsgUsers:=TGroupUsers.Create;   
  aTxtMsgUsers:=TGroupUsers.Create;
  aPhotoMsgUsers:=TGroupUsers.Create;
  aVideos:=TGroupUsers.Create;
  try
    for aUpdate in aUpdates do
    begin
      if not ExtractMessage(aUpdate.Value, aMsgObject) then
        Continue;
      if aMsgObject.Get('message_thread_id', 0)<>aThreadID then
        COntinue;
      if aMsgObject.Int64s['date']>aDeadLine then
        Continue;
      if ChckBxSIngleGroup.Checked then
        HandleMessageN
      else
        case aTaskNum of
          1: HandleMessage1;
          2: HandleMessage2;
          3: HandleMessage3;
          4: HandleMessage4;
          5: HandleMessage5;
          8: HandleMessage8; 
          9: HandleMessage9;
        else
          raise Exception.Create('Unknown task number!');
        end;
    end;
    LblCompletedUsers.Caption:=Format('Completed users: %d', [FCompletedUsers.Count]);
    FCompletedUsers.SaveList(Format('~completed%d.csv', [aTaskNum]));
  finally
    aVideos.Free;
    aTxtMsgUsers.Free;
    aMediaMsgUsers.Free;
    aVideoNotes.Free;
    aPhotoMsgUsers.Free;
  end;
end;

procedure TFrmMain.UpdateCheckGroupMedias;
var
  m: String;
begin
  for m in _MediaTypeNames do
    ChckGrpMediaTypes.Items.Add(m);
end;

function TFrmMain.GetCheckGroupMedias: TTaskMessageSet;
var
  m: TTaskMessageType;
begin
  Result:=[];
  for m in TTaskMessageType do
    if ChckGrpMediaTypes.Checked[Ord(m)] then
      Include(Result, m);
end;

end.

