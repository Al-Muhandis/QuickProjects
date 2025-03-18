unit DeleteFailedUsersForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, EditBtn, IniPropStorage, SpinEx, tgbot_dt
  ;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    DTLongPollBot1: TDTLongPollBot;
    FlNmEdtUsers: TFileNameEdit;
    IniPropStorage1: TIniPropStorage;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    SpnEdtExChatID: TSpinEditEx;
    procedure Button1Click(Sender: TObject);
    procedure FlNmEdtUsersChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

uses
  StrUtils, eventlog
  ;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FlNmEdtUsersChange(Sender: TObject);
begin
  Memo1.Clear;
  Memo1.Lines.LoadFromFile(FlNmEdtUsers.FileName);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DTLongPollBot1.SenderBot.Logger:=TEventLog.Create(Self);
  DTLongPollBot1.SenderBot.Logger.LogType:=ltFile;
  DTLongPollBot1.SenderBot.LogDebug:=True;
  DTLongPollBot1.SenderBot.Logger.FileName:=ChangeFileExt(ApplicationName, '.log');
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  s: String;
  aStringList: TStringList;
  aID: Int64;
begin
  aStringList:=TStringList.Create;
  aStringList.LoadFromFile(FlNmEdtUsers.FileName);
  for s in aStringList do
  begin
    aID:=StrToInt64(ExtractDelimited(1, s, [' ', ',', ';']));
    DTLongPollBot1.SenderBot.banChatMember(SpnEdtExChatID.Value, aID);
    Memo2.Lines.Add(s);
  end;
  aStringList.Free;
end;

end.

