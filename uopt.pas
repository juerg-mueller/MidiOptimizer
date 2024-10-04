unit UOpt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, UMidiEvent;

type

  { TOpt }

  TOpt = class(TForm)
    Button1: TButton;
    cbxTakt: TComboBox;
    cbxViertel: TComboBox;
    edtBassCh: TEdit;
    edtRemoveCh: TEdit;
    edtBPM: TEdit;
    gbHeader: TGroupBox;
    Label1: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label8: TLabel;
    OpenDialog1: TOpenDialog;
    cbxUebernehmen: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure cbxUebernehmenChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    procedure HeaderToGUI(const HeaderDetail: TDetailHeader);
    procedure GUIToHeader(var HeaderDetail: TDetailHeader);
  end;

var
  opt: TOpt;

implementation

{$R *.lfm}

uses
  UEventArray;

{ TOpt }

procedure TOpt.Button1Click(Sender: TObject);
var
  EventArray: TEventArray;
  i: integer;
  s, t, ext: string;
  BassChannels, RemoveChannels: TChannels;

  function Optimize(var Track: TMidiEventArray): boolean;
  begin
    EventArray.RemoveExpression(Track);
    EventArray.RemoveChannels(Track, RemoveChannels);
    EventArray.RemoveShorts(Track, 32);     // 32-stel  (192/8)         192 = 2^6 * 3
    EventArray.SynchronizeWithBass(Track, BassChannels, EventArray.DetailHeader);
//    EventArray.MoveGaps(Track, 32);
    // Passt den DetailHeader an.
    EventArray.BassSynchronize(Track, BassChannels, EventArray.DetailHeader);
    result := true;
  end;

  procedure GetChannels(var Channels: TChannels; s: string);
  var
    i, k, c: integer;
    t: string;
  begin
    i := 1;
    k := 1;
    Channels := [];
    while i <= length(s) do
    begin
      while (i <= Length(s)) and (s[i] in ['0'..'9']) do
        inc(i);
      if k < i then
      begin
        t := Copy(s, k, i-k);
        c := StrToInt(t);
        if c in [0..15] then
          Channels := Channels + [c];
      end;
      while (i <= Length(s)) and not (s[i] in ['0'..'9']) do
        inc(i);
      k := i;
    end;
  end;

begin
  GetChannels(BassChannels, edtBassCh.Text);
  GetChannels(RemoveChannels, edtRemoveCh.Text);

  if OpenDialog1.Execute then
  begin
    EventArray := TEventArray.Create;
    try
      s := OpenDialog1.FileName;
      ext := ExtractFileExt(s);
      Label1.Caption := 'Lese: ' + OpenDialog1.FileName;
      if EventArray.LoadMidiFromFile(s, false) then
      begin
        if cbxUebernehmen.Checked then
        begin
          GUIToHeader(EventArray.DetailHeader);
          HeaderToGUI(EventArray.DetailHeader);
        end;
        Label1.Caption := 'Lese: ' + OpenDialog1.FileName;
        i := 0;
        t := Copy(s, 1, Length(s)-length(ext)) + '_';
        repeat
          inc(i);
          s := t + inttostr(i) + '.mid';
        until not FileExists(s);
        if (Length(EventArray.Track) = 1) and
           Optimize(EventArray.Track[0]) then
        begin
          EventArray.SaveMidiToFile(s, false);
          Label1.Caption := 'Speichere: ' + s;
          SetLength(s, Length(s)-3);
          s := s + 'txt';
          EventArray.SaveSimpleMidiToFile(s, false);
        end;
      end;
    finally
      EventArray.Free;
    end;
  end;
end;

procedure TOpt.cbxUebernehmenChange(Sender: TObject);
begin
  gbHeader.Enabled := cbxUebernehmen.Checked;
end;

procedure TOpt.FormCreate(Sender: TObject);
var
  HeaderDetail: TDetailHeader;
begin
  HeaderDetail.Clear;
  HeaderDetail.IsSet := true;
  HeaderToGUI(HeaderDetail);
end;

procedure TOpt.HeaderToGUI(const HeaderDetail: TDetailHeader);
begin
    cbxTakt.ItemIndex:= HeaderDetail.measureFact - 2;
  if HeaderDetail.measureDiv = 4 then
    cbxViertel.ItemIndex := 0
  else
    cbxViertel.ItemIndex := 1;
  edtBPM.Text := IntToStr(HeaderDetail.beatsPerMin);
end;

procedure TOpt.GUIToHeader(var HeaderDetail: TDetailHeader);
begin
  HeaderDetail.Clear;
  HeaderDetail.IsSet := true;
  HeaderDetail.measureFact := cbxTakt.ItemIndex + 2;
  if cbxViertel.ItemIndex = 0 then
    HeaderDetail.measureDiv := 4
  else
    HeaderDetail.measureDiv := 8;
  HeaderDetail.beatsPerMin := StrToIntDef(edtBPM.text, 120);
end;

end.

