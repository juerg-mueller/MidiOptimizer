unit UOpt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, UMidiEvent, UEventArray;

type

  { TOpt }

  TOpt = class(TForm)
    Button1: TButton;
    cbxTakt: TComboBox;
    cbxViertel: TComboBox;
    cbxNote: TComboBox;
    edtBassCh: TEdit;
    edtRemoveCh: TEdit;
    edtBPM: TEdit;
    gbHeader: TGroupBox;
    Label1: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label8: TLabel;
    OpenDialog1: TOpenDialog;
    cbxUebernehmen: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure cbxUebernehmenChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
  private
  public
    BassChannels, RemoveChannels: TChannels;
    procedure InitChannels;
    procedure HeaderToGUI(const HeaderDetail: TDetailHeader);
    procedure GUIToHeader(var HeaderDetail: TDetailHeader);
    procedure OptimizeFile(const FileName: string);
  end;

var
  opt: TOpt;

implementation

{$R *.lfm}


{ TOpt }

procedure TOpt.InitChannels;

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
end;

procedure TOpt.OptimizeFile(const FileName: string);
var
  EventArray: TEventArray;
  i: integer;
  s, t, ext: string;

  function Optimize(var Track: TMidiEventArray): boolean;
  var
    Wert: integer;
  begin
    EventArray.RemoveExpression(Track);
    EventArray.RemoveChannels(Track, RemoveChannels);
    case cbxNote.ItemIndex of
      0: Wert := 96;
      1: Wert := 48;
      3: Wert := 12;
      else Wert := 24;
    end;
    EventArray.RemoveShorts(Track, Wert);     // 32-stel  (192/8)         192 = 2^6 * 3
    EventArray.SynchronizeWithBass(Track, BassChannels, EventArray.DetailHeader);
//    EventArray.MoveGaps(Track, 32);
    // Passt den DetailHeader an.
    EventArray.BassSynchronize(Track, BassChannels, EventArray.DetailHeader);
    result := true;
  end;

begin
  EventArray := TEventArray.Create;
  try
    s := FileName;
    ext := ExtractFileExt(s);
    Label1.Caption := 'Lese: ' + FileName;
    if EventArray.LoadMidiFromFile(s, false) then
    begin
      if cbxUebernehmen.Checked then
      begin
        GUIToHeader(EventArray.DetailHeader);
        HeaderToGUI(EventArray.DetailHeader);
      end;
      Label1.Caption := 'Lese: ' + FileName;
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

procedure TOpt.Button1Click(Sender: TObject);


begin
  InitChannels;
  if OpenDialog1.Execute then
  begin
    OptimizeFile(OpenDialog1.FileName);
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

procedure TOpt.FormDropFiles(Sender: TObject; const FileNames: array of string);
var
  l: integer;
begin
  //
  for l := 0 to high(FileNames) do
    OptimizeFile(FileNames[l]);
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

