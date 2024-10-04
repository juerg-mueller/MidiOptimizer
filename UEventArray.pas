//
// Copyright (C) 2021 Jürg Müller, CH-5524
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation version 3 of the License.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program. If not, see http://www.gnu.org/licenses/ .
//
unit UEventArray;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  UMyMidiStream, SysUtils, Classes, UMidiEvent, UMidiSaveStream;

const
  CopyrightGriff = AnsiString('Griffschrift - Copyright by juerg5524.ch');
  Copyrightreal  = AnsiString('real Griffschrift Noten - Copyright by juerg5524.ch');


type
  TCopyright = (noCopy, prepCopy, griffCopy, realCopy);
  TChannels = set of 0..15;

  TMidiEventArray = array of TMidiEvent;
  PMidiEventArray = ^TMidiEventArray;
  TChannelEventArray = array [0..15] of TMidiEventArray;
  TTrackEventArray = array of TMidiEventArray;
  TAnsiStringArray = array of AnsiString;

  TEventArray = class
  protected
    TrackName_: TAnsiStringArray; // 03
    TrackArr_: TTrackEventArray;
  public
    Text_: AnsiString;     // 01
    Copyright: AnsiString; // 02
    Instrument: AnsiString;// 04
    DetailHeader: TDetailHeader;
    SingleTrack: TMidiEventArray;
    ChannelArray: TChannelEventArray;

    constructor Create;
    destructor Destroy; override;
    function LoadMidiFromFile(FileName: string; Lyrics: boolean): boolean;
    function SaveMidiToFile(FileName: string; Lyrics: boolean): boolean;
    function SaveSingleToFile(FileName: string; Lyrics: boolean): boolean;
    function SaveSimpleMidiToFile(FileName: string; Lyrics: boolean = false): boolean;
    procedure Clear;
    procedure Move_var_len; overload;
    function Transpose(Delta: integer): boolean; overload;
    function GetCopyright: TCopyright;
    procedure SetNewTrackCount(Count: integer);
    function TrackCount: integer;
    function Optimize(var Track: TMidiEventArray): boolean;
    function OptimizeQuarterTicks(const Track: TMidiEventArray): boolean;
    class function BassSynchronize(var Track: TMidiEventArray; Channels: TChannels; const Header: TDetailHeader): boolean;
    class function Make192Ticks(var Track: TMidiEventArray; var Header: TDetailHeader): boolean;
    class function SynchronizeWithBass(var Track: TMidiEventArray; Channels: TChannels; var Header: TDetailHeader): boolean;
    class function RemoveShorts(var Track: TMidiEventArray; shortlen: cardinal): boolean;
    class function RemoveExpression(var Track: TMidiEventArray): boolean;
    class function RemoveChannels(var Track: TMidiEventArray; Channels: TChannels): boolean;
    class function MoveGaps(var Track: TMidiEventArray; gap: byte): boolean;

    property TrackName: TAnsiStringArray read TrackName_;
    property TrackArr: TTrackEventArray read TrackArr_;
    property Track: TTrackEventArray read TrackArr_;

    class procedure ClearEvents(var Events: TMidiEventArray);
    class procedure AppendEvent(var MidiEventArray: TMidiEventArray;
                                const MidiEvent: TMidiEvent);
    class function SplitEventArray(var ChannelEvents: TChannelEventArray;
                                   const Events: TMidiEventArray;
                                   count: cardinal): boolean;
    class procedure Move_var_len(var Events: TMidiEventArray); overload;
    class procedure MakeSingleTrack(var Events: TMidiEventArray; const ChannelEvents: TChannelEventArray); overload;
    class procedure MergeTracks(var Events1: TMidiEventArray; const Events2: TMidiEventArray);
    class function GetDuration(const Events: TMidiEventArray; Index: integer): integer;
    class function Transpose(var Events: TMidiEventArray; Delta: integer): boolean; overload;
    class function HasSound(const MidiEventArr: TMidiEventArray): boolean;
    class function LyricsCount(const MidiEventArr: TMidiEventArray): integer;
    class function InstrumentIdx(const MidiEventArr: TMidiEventArray): integer;
    class function PlayLength(const MidiEventArr: TMidiEventArray): integer;
    class function MakeSingleTrack(var MidiEventArray: TMidiEventArray; const TrackArr: TTrackEventArray): boolean; overload;
    class function GetDelayEvent(const EventTrack: TMidiEventArray; iEvent: integer): integer;
    class procedure MoveLyrics(var Events: TMidiEventArray);
  end;

  PSetEvent = procedure (const Event: TMidiEvent) of object;


  procedure CopyEventArray(var OutArr: TMidiEventArray; const InArr: TMidiEventArray);


implementation

uses
{$ifdef FPC}
//  Urtmidi,
{$else}
  AnsiStrings,
//  Midi,
{$endif}
   UMidiDataStream;

function RemoveIndex(var Track: TMidiEventArray; Index: cardinal): boolean;
var
  idx: cardinal;
begin
  result := Index < Length(Track);
  for idx := Index to Length(Track)-2 do
    Track[idx] := Track[idx+1];
  if result then
    SetLength(Track, Length(Track)-1);
end;

constructor TEventArray.Create;
begin
  inherited;

  DetailHeader.Clear;
  Clear;
end;

destructor TEventArray.Destroy;
begin
  Clear;

  inherited;
end;

procedure TEventArray.Clear;
begin
  Text_ := '';
  Copyright := '';
  Instrument := '';

  DetailHeader.Clear;
  SetNewTrackCount(0);
end;

procedure TEventArray.SetNewTrackCount(Count: integer);
var
  i: integer;
begin
  for i := Count to Length(TrackArr)-1 do
  begin
    ClearEvents(TrackArr[i]);
    TrackName[i] := '';
  end;
  SetLength(TrackArr_, Count);
  SetLength(TrackName_, Count);
end;

function TEventArray.TrackCount: integer;
begin
  result := Length(TrackArr_);
end;

function TEventArray.LoadMidiFromFile(FileName: string; Lyrics: boolean): boolean;
var
  Midi: TMidiDataStream;
begin
  result := false;
  Midi := TMidiDataStream.Create;
  try
    Midi.LoadFromFile(FileName);
    result := Midi.MakeEventArray(self, Lyrics);
    MakeSingleTrack(SingleTrack, TrackArr);
    SplitEventArray(ChannelArray, SingleTrack, Length(SingleTrack));
  finally
    Midi.Free;
    if not result then
      Clear;
  end;
end;


function TEventArray.SaveSimpleMidiToFile(FileName: string; Lyrics: boolean): boolean;
var
  iTrack, iEvent: integer;
  Simple: TSimpleDataStream;
  Event: TMidiEvent;
  i: integer;
  d: double;
  takt, offset: integer;
  Events: TMidiEventArray;
  bpm: double;
  l, k: integer;

  procedure WriteMetaEvent(const Event: TMidiEvent);
  var
    i: integer;
  begin
    with Simple do
    begin
      WriteString(Format('%5d Meta-Event %d %3d %3d',
                         [event.var_len, event.command, event.d1, event.d2]));
      for i := 0 to Length(event.Bytes)-1 do
        WriteString(' ' +IntToStr(event.bytes[i]));
      WriteString('   ');
      for i := 0 to Length(event.Bytes)-1 do
        if (event.bytes[i] > ord(' ')) or
           ((event.bytes[i] = ord(' ')) and
            (i > 0) and (i < Length(event.Bytes)-1)) then
          WriteString(Char(event.bytes[i]))
        else
          WriteString('.');
    end;
  end;

begin
  Simple := TSimpleDataStream.Create;
  try
    with Simple do
    begin
      with MidiHeader do
      begin
        Clear;
        FileFormat := 1;
        TrackCount := Length(TrackArr) + 1;
        Details := DetailHeader;
      end;
      WriteHeader(MidiHeader);

      if not Lyrics then
      begin
        WriteTrackHeader(0);
        if Copyright <> '' then
        begin
          WritelnString('    0 ' + cSimpleMetaEvent + ' 255 2 ' + IntToStr(Length(Copyright)) + ' '+ Copyright);
        end;
        if DetailHeader.beatsPerMin > 0 then
        begin
          bpm := 6e7 / DetailHeader.beatsPerMin;
          l := round(bpm);
          WriteString('    0 ' + cSimpleMetaEvent + ' 255 81 3 '); // beats
          WritelnString(IntToStr(l shr 16) + ' ' + IntToStr((l shr 8) and $ff) + ' ' +
                        IntToStr(l and $ff) + ' 0');
        end;

        WriteString('    0 ' + cSimpleMetaEvent + ' 255 88 4 ' + IntToStr(DetailHeader.measureFact)); // time signature
        i := DetailHeader.measureDiv;
        k := 0;
        while i > 0 do
        begin
          i := i div 2;
          inc(k);
        end;
        WritelnString(' ' + IntToStr(k-1) + ' 24 8 0');

        WritelnString('    0 ' + cSimpleMetaEvent + ' 255 47 0'); // end of track
      end;

      for iTrack := 0 to Length(TrackArr)-1 do
      begin
        WriteTrackHeader(TrackArr[iTrack][0].var_len);
        offset := TrackArr[iTrack][0].var_len;
        for iEvent := 1 to Length(TrackArr[iTrack])-1 do
        begin
          Event := TrackArr[iTrack][iEvent];
          if Event.Event = $f then
          begin
            WriteMetaEvent(Event);
          end else
          if Event.Event in [8..14] then
          begin
            if HexOutput then
              WriteString(Format('%5d $%2.2x $%2.2x $%2.2x',
                                 [event.var_len, event.command, event.d1, event.d2]))
            else
              WriteString(Format('%5d %3d %3d %3d',
                                 [event.var_len, event.command, event.d1, event.d2]));
          end;
          if Event.Event = 9 then
          begin
            takt := Offset div MidiHeader.Details.DeltaTimeTicks;
            if MidiHeader.Details.measureDiv = 8 then
              takt := 2*takt;
            d := MidiHeader.Details.measureFact;
            WriteString(Format('  Takt: %.2f', [takt / d + 1]));
          end;
          inc(offset, Event.var_len);
          WritelnString('');
        end;
        WritelnString('    0 ' + cSimpleMetaEvent + ' 255 47 0'); // end of track
      end;
    end;
    Simple.SaveToFile(FileName);
    result := true;
  finally
    Simple.Free;
  end;
end;

function TEventArray.SaveMidiToFile(FileName: string; Lyrics: boolean): boolean;
var
  i: integer;
  SaveStream: TMidiSaveStream;
begin
  SaveStream := TMidiSaveStream.Create;
  try
    SaveStream.SetHead(DetailHeader.DeltaTimeTicks);
    SaveStream.AppendTrackHead;
    if Copyright <> '' then
      SaveStream.AppendMetaEvent(2, Copyright);
    SaveStream.AppendHeaderMetaEvents(DetailHeader);
    SaveStream.AppendTrackEnd(false);
    for i := 0 to Length(TrackArr)-1 do
    begin
      SaveStream.AppendTrackHead;
      SaveStream.AppendEvents(TrackArr[i]);
      SaveStream.AppendTrackEnd(false);
    end;
    SaveStream.Size := SaveStream.Position;
    SaveStream.SaveToFile(FileName);
  finally
    SaveStream.Free;
  end;
  result := true;
end;

function TEventArray.SaveSingleToFile(FileName: string; Lyrics: boolean): boolean;
var
  i: integer;
  SaveStream: TMidiSaveStream;
begin
  SaveStream := TMidiSaveStream.Create;
  try
    SaveStream.SetHead(DetailHeader.DeltaTimeTicks);
    SaveStream.AppendTrackHead;
    SaveStream.AppendHeaderMetaEvents(DetailHeader);
    SaveStream.AppendTrackEnd(false);
    for i := 0 to Length(TrackArr)-1 do
    begin
      SaveStream.AppendTrackHead;
      SaveStream.AppendEvents(TrackArr[i]);
      SaveStream.AppendTrackEnd(false);
    end;
    SaveStream.Size := SaveStream.Position;
    SaveStream.SaveToFile(FileName);
  finally
    SaveStream.Free;
  end;
  result := true;
end;

procedure TEventArray.Move_var_len;
var
  i: integer;
begin
  for i := 0 to Length(TrackArr)-1 do
    TEventArray.Move_var_len(TrackArr[i]);
end;

function TEventArray.Transpose(Delta: integer): boolean;
var
  i: integer;
begin
  result := true;
  for i := 0 to Length(TrackArr)-1 do
    if not TEventArray.Transpose(TrackArr[i], Delta) then
      result := false;
end;


function TEventArray.GetCopyright: TCopyright;
begin
  result := noCopy;
{$ifdef FPC}
  if AnsiStrLComp(PAnsiChar(Copyright), PAnsiChar(Copyrightreal), Length('real Griffschrift - Copyright')) = 0 then
    result := realCopy
  else
  if AnsiStrLComp(PAnsiChar(Copyright), PAnsiChar(CopyrightGriff), Length('Griffschrift - Copyright')) = 0 then
    result := griffCopy
{$else}
  if AnsiStrings.AnsiStrLComp(PAnsiChar(Copyright), PAnsiChar(Copyrightreal), Length('real Griffschrift - Copyright')) = 0 then
    result := realCopy
  else
  if AnsiStrings.AnsiStrLComp(PAnsiChar(Copyright), PAnsiChar(CopyrightGriff), Length('Griffschrift - Copyright')) = 0 then
    result := griffCopy
{$endif}
  else
  if Copyright = CopyPrep then
    result := prepCopy
  else
  if (Length(TrackName_) = 2) {and ((LowerCase(TrackName[0]) = 'melodie') or (LowerCase(TrackName[1]) = 'bass'))} then
    result := prepCopy;
end;


////////////////////////////////////////////////////////////////////////////////

class function TEventArray.HasSound(const MidiEventArr: TMidiEventArray): boolean;
var
  i: integer;
begin
  result := false;
  i := 0;
  while (i < Length(MidiEventArr)) and not result do
    if MidiEventArr[i].Event = 9 then
      result := true
    else
      inc(i);
end;

class function TEventArray.LyricsCount(const MidiEventArr: TMidiEventArray): integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to Length(MidiEventArr)-1 do
    if (MidiEventArr[i].command = $ff) and (MidiEventArr[i].d1 = 5) then
      inc(result);
end;

class function TEventArray.InstrumentIdx(const MidiEventArr: TMidiEventArray): integer;
var
  i: integer;
begin
  result := -1;
  i := 0;
  while (i < Length(MidiEventArr)) do
    if MidiEventArr[i].Event = 12 then
    begin
      result := MidiEventArr[i].d1;
      break;
    end else
      inc(i);
end;

class function TEventArray.MakeSingleTrack(var MidiEventArray: TMidiEventArray; const TrackArr: TTrackEventArray): boolean;
var
  i: integer;
begin
  SetLength(MidiEventArray, 0);
  for i := 0 to Length(TrackArr)-1 do
    TEventArray.MergeTracks(MidiEventArray, TrackArr[i]);
  result := true;
end;

class function TEventArray.PlayLength(const MidiEventArr: TMidiEventArray): integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to Length(MidiEventArr)-1 do
    if MidiEventArr[i].var_len > 0 then
      inc(result, MidiEventArr[i].var_len);
end;

class procedure TEventArray.ClearEvents(var Events: TMidiEventArray);
var
  i: integer;
begin
  for i := 0 to Length(Events)-1 do
    SetLength(Events[i].bytes, 0);
  SetLength(Events, 0);
end;

class procedure TEventArray.AppendEvent(var MidiEventArray: TMidiEventArray;
                                        const MidiEvent: TMidiEvent);
begin
  SetLength(MidiEventArray, Length(MidiEventArray)+1);
  MidiEventArray[Length(MidiEventArray)-1] := MidiEvent;
end;

class function TEventArray.SplitEventArray(var ChannelEvents: TChannelEventArray;
                                           const Events: TMidiEventArray;
                                           count: cardinal): boolean;
var
  channel: byte;
  delay: integer;
  i, iMyEvent: integer;
begin
  result := false;
  for channel := 0 to 15 do
  begin
    SetLength(ChannelEvents[channel], 1000);
    ChannelEvents[channel][0].Clear;
    iMyEvent := 1;
    delay := 0;
    for i := 0 to count-1 do
    begin
      if (i = 0) and (Events[0].command = 0) then
      begin
        delay := Events[0].var_len; // mit wave synchronisieren
      end else
      if (Events[i].Channel = channel) and
         (Events[i].Event in [8..14]) then
      begin
        if High(ChannelEvents[channel]) < iMyEvent then
          SetLength(ChannelEvents[channel], 2*Length(ChannelEvents[channel]));

        ChannelEvents[channel][iMyEvent] := Events[i];
        inc(iMyEvent);
      end else
      if Events[i].Event in [8..14] then
      begin
        if iMyEvent > 1 then
          inc(ChannelEvents[channel][iMyEvent - 1].var_len, Events[i].var_len)
        else
          inc(delay, Events[i].var_len);
      end;
    end;
    if iMyEvent > 1 then
    begin
      ChannelEvents[channel][0].var_len := delay;
      SetLength(ChannelEvents[channel], iMyEvent);
      result := true;
    end else
      SetLength(ChannelEvents[channel], 0);
  end;
end;

class procedure TEventArray.Move_var_len(var Events: TMidiEventArray);
var
  iEvent: integer;
begin
  for iEvent := length(Events)-1 downto 1 do
    if not (Events[iEvent].Event in [8, 9]) then
    begin
      inc(Events[iEvent-1].var_len, Events[iEvent].var_len);
      Events[iEvent].var_len := 0;
    end;
end;

class procedure TEventArray.MakeSingleTrack(var Events: TMidiEventArray; const ChannelEvents: TChannelEventArray);
var
  i: integer;
begin
  SetLength(Events, 0);
  for i := 0 to 15 do
  begin
    TEventArray.MergeTracks(Events, ChannelEvents[i]);
  end;
end;

class procedure TEventArray.MergeTracks(var Events1: TMidiEventArray; const Events2: TMidiEventArray);
var
  i, k: integer;
  Ev: TMidiEvent;
  iEvent: array [0..1] of integer;
  iOffset: array [0..1] of integer;
  Offset: integer;
  temp: TMidiEventArray;
  Events: array [0..1] of PMidiEventArray;

  function MidiEvent(i: integer): TMidiEvent;
  begin
    result := Events[i]^[iEvent[i]];
  end;

  function Valid(i: integer): boolean;
  begin
    result := iEvent[i] < Length(Events[i]^);
  end;

begin
  if not TEventArray.HasSound(Events1) then
  begin
    SetLength(Events1, 0);
    CopyEventArray(Events1, Events2);
    exit;
  end;
  CopyEventArray(temp, Events1);

  Events[0] := @temp;
  Events[1] := @Events2;
  SetLength(Events1, Length(Events1)+Length(Events2));

  for i := 0 to 1 do
  begin
    iEvent[i] := 0;
    iOffset[i] := 0;
  end;

  Offset := 0;
  k := 0;
  if (MidiEvent(0).command = 0) and
     (MidiEvent(1).command = 0) then
  begin
    for i := 0 to 1 do
    begin
      iOffset[i] := MidiEvent(i).var_len;
    end;
    if MidiEvent(0).var_len > MidiEvent(1).var_len then
      Events1[k].var_len := MidiEvent(1).var_len;
    Offset := Events1[k].var_len;
    inc(iEvent[0]);
    inc(iEvent[1]);
    inc(k);
  end;

  while Valid(0) and Valid(1) do
  begin
    for i := 0 to 1 do
    begin
      while Valid(i) and (iOffset[i] = Offset) do
      begin
        Ev := Events[i]^[iEvent[i]];
        if Ev.var_len > 0 then
          inc(iOffset[i], Ev.var_len);
        Ev.var_len := 0;
        Events1[k] := Ev;
        Events1[k+1].Clear;
        inc(iEvent[i]);
        if (i = 1) and (iEvent[i] = 82) then
          k := k;

        inc(k);
      end;
    end;
    inc(Offset);
    inc(Events1[k-1].var_len);
  end;

  i := 0;
  if Valid(1) then
    i := 1;
  if Valid(i) then
  begin
    if iOffset[i] > Offset then
      inc(Events1[k-1].var_len, iOffset[i] - Offset);
    while Valid(i) do
    begin
      Events1[k] := Events[i]^[iEvent[i]];
      inc(k);
      inc(iEvent[i]);
    end;
  end;

  SetLength(Events1, k);
  SetLength(temp, 0);
end;


class function TEventArray.GetDuration(const Events: TMidiEventArray; Index: integer): integer;
var
  com, d1: integer;
begin
  result := 0;
  if (Index < 0) or (Index >= Length(Events)) or
     (Events[Index].Event <> 9) then
    exit;

  com := Events[Index].command xor $10;
  d1 := Events[Index].d1;
  repeat
    inc(result, Events[Index].var_len);
    inc(Index);
  until (Index >= Length(Events)) or
        ((Events[Index].command = com) and (Events[Index].d1 = d1));
end;

class function TEventArray.Transpose(var Events: TMidiEventArray; Delta: integer): boolean;
var
  i: integer;
begin
  result := true;
  if (Delta <> 0) and (abs(Delta) <= 20) then
    for i := 0 to Length(Events)-1 do
      if (Events[i].Event in [8, 9]) then
      begin
        if (Events[i].d1 + Delta > 20) and (Events[i].d1 + Delta <= 127) then
          Events[i].d1 := Events[i].d1 + Delta
        else
          result := false;
      end;
end;

class function TEventArray.GetDelayEvent(const EventTrack: TMidiEventArray; iEvent: integer): integer;
var
  i: integer;
  cmd: integer;
begin
  result := -1;
  cmd := EventTrack[iEvent].command;
  if (cmd shr 4) <> 9 then
    exit;

  result := EventTrack[iEvent].var_len;
  i := iEvent + 1;
  dec(cmd, $10);
  while (i < Length(EventTrack)) and
        ((EventTrack[i].command <> cmd) or
         (EventTrack[iEvent].d1 <> EventTrack[i].d1)) do
  begin
    inc(result, EventTrack[i].var_len);
    inc(i);
  end;
end;

class procedure TEventArray.MoveLyrics(var Events: TMidiEventArray);
var
  i, j1, j2, k: integer;
  dist1, dist2: integer;
  Event: TMidiEvent;
begin
  i := 0;
  while i < Length(Events) do
  begin
    Event := Events[i];
    if (Event.command = $ff) and (Event.d1 = 5) then
    begin
      if Event.var_len > 0 then
      begin
        dist1 := 0;
        j1 := i;
        while (j1 > 0) do
        begin
          dec(j1);
          inc(dist1, Events[j1].var_len);
          if Events[j1].Event = 9 then
            break;
        end;
        dist2 := 0;
        j2 := i;
        while (j2 < Length(Events)) do
        begin
          inc(dist2, Events[j2].var_len);
          if Events[j2].Event = 9 then
            break;
          inc(j2);
        end;

        inc(Events[i-1].var_len, Event.var_len);
        Event.var_len := 0;

        if dist2 <= dist1 then
        begin
          if (j2 < Length(Events)) and (dist2 < 10) then
          begin
            dec(j2);
            for k := i to j2-1 do
              Events[k] := Events[k+1];
            Events[j2] := Event;
          end;
        end else
        if (j1 > 1) and (dist1 < 10) then
        begin
          for k := i-1 downto j1 do
            Events[k+1] := Events[k];
          Events[j1] := Event;
        end;
      end;
    end;
    inc(i);
  end;
end;

procedure CopyEventArray(var OutArr: TMidiEventArray; const InArr: TMidiEventArray);
var
  i: integer;
begin
  SetLength(OutArr, Length(InArr));
  for i := Low(InArr) to High(InArr) do
    OutArr[i] := InArr[i];

end;

////////////////////////////////////////////////////////////////////////////////

// Ist keine gute Idee: DeltaTimeTicks sollte durch 8 und 3 teilbar sein!!!
function TEventArray.OptimizeQuarterTicks(const Track: TMidiEventArray): boolean;
var
  idx: integer;
  d, last: integer;
  bassCh: set of 0..15;
  MidiEvent: TMidiEvent;
  ticks: integer;
  sum, w: double;
  n: integer;
  eps: double;
begin
  result := false;
  // Abstände zwischen den Bässen messen
  bassCh := [4, 5];
  ticks := DetailHeader.DeltaTimeTicks;
  if DetailHeader.GetMeasureDiv >= 8 then
    ticks := ticks div 2;
  idx := 0;
  d := 0;
  last := 0;
  eps := ticks / 4;
  sum := 0;
  n := 0;
  while idx < Length(Track) do
  begin
    MidiEvent := Track[idx]; inc(idx);
    if (MidiEvent.event = 9) and
       (MidiEvent.channel in bassCh) then
    begin
      w := d - last;
      last := d;

      if abs(w - ticks) < eps then
      begin
        sum := sum + w;
        inc(n);
      end else
      if abs(w/2 - ticks) < eps then
      begin
        sum := sum + w;
        inc(n, 2);
      end else
      if abs(w/3 - ticks) < eps then
      begin
        sum := sum + w;
        inc(n, 3);
      end else
      if abs(w/4 - ticks) < eps then
      begin
        sum := sum + w;
        inc(n, 4);
      end;
    end;
    inc(d, MidiEvent.var_len);
  end;

  if n > 20 then
  begin
    w := sum / n;
    if DetailHeader.GetMeasureDiv >= 8 then
      w := 2*w;
    DetailHeader.DeltaTimeTicks := round(w);
    result := true;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TEventArray.Optimize(var Track: TMidiEventArray): boolean;
begin
  RemoveExpression(Track);
  RemoveChannels(Track, [7]);
  RemoveShorts(Track, 20);
  result := BassSynchronize(Track, [5, 6], DetailHeader);
end;

class function TEventArray.BassSynchronize(var Track: TMidiEventArray; Channels: TChannels; const Header: TDetailHeader): boolean;
const
  diff = 32; // 32-stel
var
  idx, idx1: integer;
  p, q, v: integer;
begin
  result := false;
  if Channels = [] then
    exit;

  idx := 0;
  p := 0;
  while (idx < Length(Track)) do
  begin
    if Track[idx].Channel in Channels then
    begin
      q := p mod Header.DeltaTimeTicks;
      v := Header.DeltaTimeTicks - q;
      idx1 := idx - 1;

      if (q > 0) and (q < diff) then
      begin
        // var_len verkürzen
        while (q > 0) and (idx1 >= 0) do
        begin
          if Track[idx1].var_len >= q then
          begin
            dec(Track[idx1].var_len, q);
            dec(p, q);
            q := 0;
            result := true;
          end else begin
            dec(q, Track[idx1].var_len);
            dec(p, Track[idx1].var_len);
            Track[idx1].var_len := 0;
            result := true;
          end;
          dec(idx1);
        end;
      end else
      // var_len verlängern
      if (v > 0) and (v < diff) then
      begin
        while idx1 >= 0 do
        begin
          if Track[idx1].var_len > diff then
          begin
            inc(Track[idx1].var_len, v);
            inc(p, v);
            result := true;
            break;
          end else begin
            // Zusammenhängende Noten überspringen
            // var_len zu v dazuzählen
            inc(v, Track[idx1].var_len);
            dec(p, v);
            Track[idx1].var_len := 0;
          end;
          dec(idx1);
        end;
      end;
    end;
    inc(p, Track[idx].var_len);
    inc(idx);
  end;
end;

class function TEventArray.RemoveShorts(var Track: TMidiEventArray; shortlen: cardinal): boolean;
var
  idx, idx2: integer;
  len: integer;
  com: byte;
begin
  result := false;
  idx := 0;
  while idx < Length(Track) do
  begin
    if Track[idx].Event = 9 then
    begin
      com := Track[idx].command xor $10;
      len := Track[idx].var_len;
      idx2 := idx + 1;
      while (idx2 < Length(Track)) and
            ((Track[idx2].command <> com) or
             (Track[idx].d1 <> Track[idx2].d1)) do
      begin
        inc(len, Track[idx2].var_len);
        inc(idx2);
      end;
      if len < shortlen then
      begin
        inc(Track[idx2-1].var_len, Track[idx2].var_len);
        RemoveIndex(Track, idx2);
        if idx > 0 then
          inc(Track[idx-1].var_len, Track[idx].var_len);
        RemoveIndex(Track, idx);
        result := true;
      end else
        inc(idx);
    end else
      inc(idx);
  end;
end;

class function TEventArray.RemoveChannels(var Track: TMidiEventArray; Channels: TChannels): boolean;
var
  idx: integer;
begin
  result := false;
  if Channels = [] then
    exit;

  idx := 0;
  while idx < Length(Track) do
  begin
    if (Track[idx].Channel in Channels) and
       (Track[idx].Event in [8, 9]) then
    begin
      if idx > 0 then
        inc(Track[idx-1].var_len, Track[idx].var_len);
      RemoveIndex(Track, idx);
      result := true;
    end else
      inc(idx);
  end;
end;

class function TEventArray.RemoveExpression(var Track: TMidiEventArray): boolean;
var
  idx: integer;
begin
  result := false;
  idx := 0;
  while idx < Length(Track) do
  begin
    if (Track[idx].d1 = 11) and
       (Track[idx].Event = 11) then
    begin
      if idx > 0 then
        inc(Track[idx-1].var_len, Track[idx].var_len);
      RemoveIndex(Track, idx);
      result := true;
    end else
      inc(idx);
  end;
end;

// Kleine Zwischenräume werden nach hinten gebracht.
class function TEventArray.MoveGaps(var Track: TMidiEventArray; gap: byte): boolean;
var
  idx, i: integer;
begin
  result := false;
  idx := Length(Track)-1;
  while idx >= 0 do
  begin
    if (Track[idx].var_len > 0) and (Track[idx].var_len <= gap) and (idx >= 0) then
    begin
      i := idx + 1;
      while (i < Length(Track)) and (Track[idx].var_len = 0) do
        inc(i);
      if i < Length(Track) then
        inc(Track[i].var_len, Track[idx].var_len);
      Track[idx].var_len := 0;
      result := true;
    end;
    dec(idx);
  end;
end;

class function TEventArray.SynchronizeWithBass(var Track: TMidiEventArray; Channels: TChannels; var Header: TDetailHeader): boolean;
const
  Max = 4000;
  delta = 8;
var
  idx, i, idx1, idx2: integer;
  dur: array[0..Max] of integer;
  FirstBass: boolean;
  p, last, m: integer;
  q: double;
begin
  result := false;
  if Length(Track) < 20 then
    exit;

  for i := 0 to Max do
    dur[i] := 0;

  idx := 0;
  m := 0;
  while idx < Length(Track) do
  begin
    if Track[idx].Event = 9 then
      break;
    inc(m, Track[idx].var_len);
    Track[idx].var_len := 0;
    inc(idx);
  end;
  if idx > 0 then
    Track[idx-1].var_len := m mod Header.TicksPerMeasure;

  // alle Abstände zwischen den Bässen messen
  FirstBass := true;
  idx := 0;
  last := 0;
  p := 0;
  while idx < Length(Track) do
  begin
    if (Track[idx].Event = 9) and (Track[idx].Channel in Channels) then
    begin
      if FirstBass then
      begin
        FirstBass := false;
        last := p;
      end else begin
        if p - last <= Max then
          inc(dur[p - last]);
        last := p;
      end;
    end;
    inc(p, Track[idx].var_len);
    inc(idx);
  end;

  // Index idx mit Maximum
  m := 0;
  idx := 0;
  for i := 0 to Max do
    if dur[i] > m then
    begin
      m := dur[i];
      idx := i;
    end;

  // Bandbreite mit Maximum
  idx1 := idx-1;
  while (idx1 > 0) and (m - dur[idx1] < delta) do
    dec(idx1);
  idx2 := idx1+1;
  while (idx2 < Max) and (m - dur[idx2] < delta) do
    inc(idx2);

  idx := (idx2 + idx1) div 2;
  i := idx mod 24;
  if i <= 12 then
    dec(idx, i)
  else
    inc(idx, 24 - i);

  if (idx > 80) and (idx < 800) and
     (idx <> Header.DeltaTimeTicks) then
  begin
    result := true;
    q := Header.DeltaTimeTicks / idx;
    Header.DeltaTimeTicks := idx;
    Header.beatsPerMin := round(Header.beatsPerMin*q);
  end;
end;

class function TEventArray.Make192Ticks(var Track: TMidiEventArray; var Header: TDetailHeader): boolean;
var
  idx: integer;
  q: double;
begin
  result := not (Header.DeltaTimeTicks in [160..224]);
  if result then
  begin
    q := 192 / Header.DeltaTimeTicks;
    for idx := 0 to Length(Track)-1 do
      Track[idx].var_len := round(q*Track[idx].var_len);

    Header.DeltaTimeTicks := round(q*Header.DeltaTimeTicks);
  end;
end;

end.


