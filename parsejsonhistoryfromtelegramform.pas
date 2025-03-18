unit ParseJSONHistoryFromTelegramForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, EditBtn, StdCtrls, Spin, ComCtrls, IniPropStorage, fgl, fpjson,
  eventlog
  ;

type

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
    FlNmEdtCuratorsFile: TFileNameEdit;
    FlNmEdtResultJSON: TFileNameEdit;
    FlNmEdtAllUsers: TFileNameEdit;
    IniPropStorage1: TIniPropStorage;
    LblFailed: TLabel;
    LblCurators: TLabel;
    LblCompletedUsers: TLabel;
    LblFileNameAllUsers: TLabel;
    LblFileNameCurators: TLabel;
    LblForumID: TLabel;
    LblResultJSONStat: TLabel;
    LblFileNameResultJSON: TLabel;
    LblAllUsers: TLabel;
    PgCntrl: TPageControl;
    SpnEdtForumID1: TSpinEdit;
    SpnEdtForumID3: TSpinEdit;
    SpnEdtTaskNum: TSpinEdit;
    SpnEdtForumID2: TSpinEdit;
    TbShtMain: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FlNmEdtAllUsersChange(Sender: TObject);
    procedure FlNmEdtCuratorsFileChange(Sender: TObject);
    procedure FlNmEdtResultJSONChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FCompletedUsers, FFailedUsers, FCuratorsUsers: TGroupUsers;
    FJSONData: TJSONObject;
    FAllUsers: TGroupUsers;
    FLog: TEventLog;
    function ForumID(aTaskNum: Byte): Integer;
    procedure LoadUsers(aUsers: TGroupUsers; aLabel: TLabel; aFileNameEdit: TFileNameEdit);
    procedure ParseCompleted(aTaskNum: Integer);
  public

  end;



var
  FrmMain: TFrmMain;

implementation

uses
  jsonparser, jsonscanner, StrUtils
  ;

{$R *.lfm}

procedure ExtractFromMsg(aMsgObject: TJSONObject; out aID: Int64; out aName: String);
var
  aActorID: String;
begin
  aID:=0;
  aName:=EmptyStr;
  aActorID:=aMsgObject.Strings['from_id'];
  if not aActorID.IsEmpty then
    aID:=StrToInt64(RightStr(aActorID, Length(aActorID)-Length('user')));
  aName:=aMsgObject.Strings['from'];
end;

{ TGroupUsers }

procedure TGroupUsers.AddToUserList(aMsgObject: TJSONObject);
begin
  AddToUserList(aMsgObject.Strings['from'], aMsgObject.Strings['from_id'])
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
  FFailedUsers.SaveList('~failed.csv');
  LblFailed.Caption:=Format('Failed users: %d', [FFailedUsers.Count]);
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
begin
  FreeAndNil(FJSONData);
  LblResultJSONStat.Caption:='--';
  if FlNmEdtResultJSON.FileName.IsEmpty then
    Exit;
  aStringList:=TStringList.Create;
  try
    aStringList.LoadFromFile(FlNmEdtResultJSON.FileName);
    try
      FJSONData:=GetJSON(aStringList.Text) as TJSONObject;
      LblResultJSONStat.Caption:=Format('Messages: %d', [FJSONData.Arrays['messages'].Count]);
    except
      FreeAndNil(FJSONData);
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
  case aTaskNum of
    1: Result:=SpnEdtForumID1.Value;
    2: Result:=SpnEdtForumID2.Value;
    3: Result:=SpnEdtForumID3.Value;
  else
    raise Exception.Create(Format('Uknown task number %d', [aTaskNum]));
  end;
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
  aMessages: TJSONArray;
  aMsg: TJSONEnum;
  aMsgObject: TJSONObject;
  aMediaMsgUsers, aTxtMsgUsers: TGroupUsers;

  procedure HandleMessage1;
  begin
    if (aTaskNum=1) and (aMsgObject.Get('mime_type', EmptyStr)<>'video/mp4') then
      Exit;
    FCompletedUsers.AddToUserList(aMsgObject);
  end;

  procedure HandleMessage2;
  var
    aID: Int64;
    aName: String;
    aTxtType: TJSONtype;
    aIsTextExists: Boolean;
  begin
    //aUpdateID:=aMsgObject.Int64s['id'];
    if ChckBxStrictFilter.Checked then
    begin
      ExtractFromMsg(aMsgObject, aID, aName);
      if (aMsgObject.Get('mime_type', EmptyStr)='video/mp4') or (aMsgObject.Get('photo', EmptyStr)<>EmptyStr) then
      begin
        if (aMediaMsgUsers.IndexOf(aID)>-1) or (aTxtMsgUsers.IndexOf(aID)>-1) then
          FCompletedUsers.AddToUserList(aName, aID)
        else
          aMediaMsgUsers.AddToUserList(aName, aID);
      end
      else begin
        aTxtType:=aMsgObject.Types['text'];
        aIsTextExists:=(aTxtType=jtArray) or ((aTxtType=jtString) and (aMsgObject.Strings['text']<>EmptyStr));
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

begin
  aMessages:=FJSONData.Arrays['messages'];
  FCompletedUsers.Clear;
  aMediaMsgUsers:=TGroupUsers.Create;   
  aTxtMsgUsers:=TGroupUsers.Create;
  try
    for aMsg in aMessages do
    begin
      aMsgObject:=aMsg.Value as TJSONObject;
      if (aMsgObject.Strings['type']='message') and (aMsgObject.Get('reply_to_message_id', 0)=ForumID(aTaskNum)) then
      begin
        case aTaskNum of
          1: HandleMessage1;
          2: HandleMessage2;
        else
          raise Exception.Create('Unknown task number!');
        end;
      end;
    end;
    LblCompletedUsers.Caption:=Format('Completed users: %d', [FCompletedUsers.Count]);
    FCompletedUsers.SaveList('~completed.csv');
  finally
    aTxtMsgUsers.Free;
    aMediaMsgUsers.Free;
  end;
end;

end.

