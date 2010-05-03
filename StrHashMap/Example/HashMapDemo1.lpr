program HashMapDemo1;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, contnrs, StrHashMap;

type

  { TVEpisode }

  TVEpisode = class
  private
    fEpNum, fTitle, fDirector, fWriter, fAirdate: string;
  public
    constructor Create(aEpNum, aTitle, aDirector, aWriter, aAirdate: string);
  end;

  { THashMapDemo }

  THashMapDemo = class(TCustomApplication)
  private
    Map: TStringHashMap;       // Hash map for quick lookup.
    EpList: TObjectList;       // List of episodes, for memory management only.
    EpMem: TVEpisode;          // Saved episode for later FindData demo.

    // Callback methods for Map.IterateMethod.
    // The callback should return False if it wants to stop itarating, True otherwise.
    // If the callback is a function instead of method, use Map.Iterate instead of IterateMethod.
    function CollectKeysAndTitles(AUserData: Pointer; const AStr: string; var APtr: Pointer): Boolean;
    function PrintWordCounts(AUserData: Pointer; const AStr: string; var APtr: Pointer): Boolean;

    // Fill data into variables. In real worlds cases there would be more data read from a DB.
    procedure FillData;

  protected
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure DoRun; override;
  public
  end;


// *** Code section ***

{ TVEpisode }

constructor TVEpisode.Create(aEpNum, aTitle, aDirector, aWriter, aAirdate: string);
begin
  inherited Create;
  fEpNum    := aEpNum;
  fTitle    := aTitle;
  fDirector := aDirector;
  fWriter   := aWriter;
  fAirdate  := aAirdate;
end;


{ THashMapDemo }

constructor THashMapDemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // For good performance the map's maximum data size should be estimated.
  // So, you must know about your data in advance but usually you do.
  // Bucket size should be 2's power minus 1 or a prime number or something.
  Map := TStringHashMap.Create(1023);

  // TObjectList own its members and will free them at the end.
  // A hash map should not usually own its data. Then there is a risk
  //  of memory leak if the same key is used for 2 objects.
  // In our demo the same objects are saved with many keys. A separate list owns them.
  EpList := TObjectList.create;
end;

destructor THashMapDemo.Destroy;
begin
  EpList.Free;                 // This will free the objects, too.
  Map.Free;
  inherited Destroy;
end;

// This is used as a callback method to Map.IterateMethod.
// Add Keyword and Title of each data object into a stringlist.
function THashMapDemo.CollectKeysAndTitles(AUserData: Pointer; const AStr: string;
                                           var APtr: Pointer): Boolean;
var
  SL: TStringList;
  Episode: TVEpisode;
begin
  SL := TStringList(AUserData);
  Episode := TVEpisode(APtr);
  SL.Add('Key: "' + AStr + '", Title: ' + Episode.fTitle);
  Result := True;                       // Will continue iterating.
end;

// This is also a callback method to Map.IterateMethod.
// Print each word and its count of occurrances, if > 1.
function THashMapDemo.PrintWordCounts(AUserData: Pointer; const AStr: string;
                                      var APtr: Pointer): Boolean;
var
  Ptri: PtrInt;
begin
  Ptri := PtrInt(APtr);
  if Ptri > 1 then
    writeln('Word "' + AStr + '": ' + IntToStr(Ptri));
  Result := True;                       // Will continue iterating.
end;


// Add a list of South Park episodes from 1. year.
// In real cases there would be more data read from a DB but in this demo we do it manually.
procedure THashMapDemo.FillData;
var
  Ep: TVEpisode;               // South park episodes.
  EpTitle: string;             // Episode title.
begin
  EpTitle := 'Cartman Gets an Anal Probe';
  // Episode #, Title, Director, Writer, Airdate
  Ep := TVEpisode.Create(' 1', EpTitle, 'Trey Parker', 'Trey Parker & Matt Stone', 'August 13, 1997');
  EpList.Add(Ep);              // To be freed later.
  Map[EpTitle] := Ep;          // Use the whole episode title as key.
  Map['Cartman'] := Ep;        // and then some other key words.
  Map['Anal Probe'] := Ep;

  EpTitle := 'Weight Gain 4000';
  Ep := TVEpisode.Create(' 2', EpTitle, 'Trey Parker & Matt Stone', 'Trey Parker & Matt Stone', 'August 20, 1997');
  EpList.Add(Ep);
  Map[EpTitle] := Ep;
  Map['Weight Gain'] := Ep;
  Map['4000'] := Ep;

  EpTitle := 'Volcano';
  Ep := TVEpisode.Create(' 3', EpTitle, 'Trey Parker & Matt Stone', 'Trey Parker & Matt Stone', 'August 27, 1997');
  EpList.Add(Ep);
  Map[EpTitle] := Ep;

  EpTitle := 'Big Gay Al''s Big Gay Boat Ride';
  Ep := TVEpisode.Create(' 4', EpTitle, 'Trey Parker', 'Trey Parker & Matt Stone', 'September 3, 1997');
  EpList.Add(Ep);
  Map[EpTitle] := Ep;
  Map['Big Gay'] := Ep;
  Map['Big Gay Al'] := Ep;
  Map['Al'] := Ep;
  Map['Boat Ride'] := Ep;

  EpTitle := 'An Elephant Makes Love to a Pig';
  Ep := TVEpisode.Create(' 5', EpTitle, 'Trey Parker & Matt Stone', 'Trey Parker, Matt Stone & Dan Sterling', 'September 10, 1997');
  EpList.Add(Ep);
  Map[EpTitle] := Ep;
  Map['Elephant'] := Ep;
  Map['Pig'] := Ep;
  Map['Make love'] := Ep;

  EpTitle := 'Death';
  Ep := TVEpisode.Create(' 6', EpTitle, 'Matt Stone', 'Trey Parker & Matt Stone', 'September 17, 1997');
  EpList.Add(Ep);
  Map[EpTitle] := Ep;

  EpTitle := 'Pinkeye';
  Ep := TVEpisode.Create(' 7', EpTitle, 'Trey Parker & Matt Stone', 'Trey Parker, Matt Stone & Philip Stark', 'October 29, 1997');
  EpList.Add(Ep);
  Map[EpTitle] := Ep;

  EpTitle := 'Starvin'' Marvin';
  Ep := TVEpisode.Create(' 8', EpTitle, 'Trey Parker', 'Trey Parker', 'November 19, 1997');
  EpList.Add(Ep);
  EpMem := Ep;             // For later demonstration of FindData.
  Map[EpTitle] := Ep;
  Map['Marvin'] := Ep;

  EpTitle := 'Mr. Hankey, the Christmas Poo';
  Ep := TVEpisode.Create(' 9', EpTitle, 'Trey Parker', 'Trey Parker', 'December 17, 1997');
  EpList.Add(Ep);
  Map[EpTitle] := Ep;
  Map['Mr. Hankey'] := Ep;
  Map['Christmas Poo'] := Ep;

  EpTitle := 'Damien';
  Ep := TVEpisode.Create('10', EpTitle, 'Trey Parker & Matt Stone', 'Trey Parker & Matt Stone', 'February 4, 1998');
  EpList.Add(Ep);
  Map[EpTitle] := Ep;

  EpTitle := 'Tom''s Rhinoplasty';
  Ep := TVEpisode.Create('11', EpTitle, 'Trey Parker', 'Trey Parker', 'February 11, 1998');
  EpList.Add(Ep);
  Map[EpTitle] := Ep;
  Map['Tom'] := Ep;
  Map['Rhinoplasty'] := Ep;

  EpTitle := 'Mecha-Streisand';
  Ep := TVEpisode.Create('12', EpTitle, 'Trey Parker', 'Trey Parker, Matt Stone & Philip Stark', 'February 18, 1998');
  EpList.Add(Ep);
  Map[EpTitle] := Ep;
  Map['Streisand'] := Ep;

  EpTitle := 'Cartman''s Mom Is a Dirty Slut';
  Ep := TVEpisode.Create('13', EpTitle, 'Trey Parker', 'Trey Parker & David R. Goodman', 'February 25, 1998');
  EpList.Add(Ep);
  Map[EpTitle] := Ep;
  Map['Cartman'] := Ep;
  Map['Mom'] := Ep;            // Ok, the rest was so ugly we won't add it in keywords.
end;


// Demo some features.
// Search episodes by keyword using a quick hash lookup.
// Find and FindData.
// Iterating map's data.
// Using data pointer as integer.
procedure THashMapDemo.DoRun;
var
  i: integer;
  Ptri: PtrInt;                // Sizeof(PtrInt) matches with Sizeof(Pointer).
  S: string;
  Words: TStringList;          // List of words to query.
  Ep: TVEpisode;
begin
  // List of words to look for.
  Words := TStringList.Create;
  try

  // Read data into memory. Actually in this demo just fill it manually.
  FillData;

  // Add some "random" words to look for.
  Words.Add('Elephant');
  Words.Add('Streisand');
  Words.Add('James Bond');
  Words.Add('Mr. Hankey');
  Words.Add('Cartman');
  // Query for the added strings.
  for i := 0 to Words.Count - 1 do
  begin
    Writeln('Looking for string "', Words[i], '"');
    if Map.Contains(Words[i]) then // Also: if Map.Has(...
    begin
      Ep := TVEpisode(Map[Words[i]]);
      Writeln('Episode: ', Ep.fEpNum, '  Title: ', Ep.fTitle);
      Writeln('             Director: ', Ep.fDirector);
      Writeln('             Writer: ', Ep.fWriter);
      Writeln('             Airdate: ', Ep.fAirdate);
    end
    else
      Writeln('Query string "', Words[i], '" not found!');
    Writeln();
  end;

  // Notice how searching "Cartman" gave the last episode, not the first one.
  // A hash map by definition has only unique keys. The first one was overwritten.

  // We could skip the test "if Map.Has(..." and test the returned value instead.
  // Map returns Nil if the key is not found. Like this:
  Ep := TVEpisode(Map['Not there']);
  if Assigned(Ep) then
    Writeln('Amazing! We found a string which is not there.'); // Shouldn't happen.

  // Instead of the array syntax (Map[...]) we could use method Map.Find():
  if Map.Find('Al', Ep) then
  begin
    Writeln('Found using Map.Find(''Al''..): Episode', Ep.fEpNum,
            '  Title: ', Ep.fTitle, ',   Airdate: ', Ep.fAirdate);
    Writeln();
  end;

  // There is also FindData() which looks for data elements slowly iterating
  // the whole map. A hash map is typically used for big amounts of data
  // (like hundreds of thousands) and then it matters.
  // If you need to use it then you should consider a different data model.
  if Map.FindData(EpMem, S) then
  begin                        // EpMem was set when creating the list of episodes.
    Writeln('Found using Map.FindData(): Episode', EpMem.fEpNum,
            '  Title: ', EpMem.fTitle, ',   Airdate: ', EpMem.fAirdate);
    Writeln('Stored under key "', S, '"');  // Finds the first key for this data.
    Writeln('(But don''t use FindData, it is slow!)');
    Writeln();
  end;

  // Iterating is little complicated. It should be something more object oriented.
  // We need a separate method as a callback, here CollectKeysAndTitles.
  // Note that the order of collected list is random.
  //  Hash map by definition does not support ordering.
  Words.Clear;
  Map.IterateMethod(Words, CollectKeysAndTitles); // Collect to Words list.
  // Now show the results.
  Writeln('Titles stored in the hash map:');
  // Writeln could also be in the callback method (CollectKeysAndTitles),
  //  but this demonstrates using the AUserData parameter.
  for i := 0 to Words.Count-1 do
    Writeln(Words[i]);
  Writeln();

  // And now:
  S:='This demonstrates how a hash map''s data pointer can also be' +
     ' casted and used as an integer. In this case it is counting' +
     ' occurrances of words in this text . Words are separated from text' +
     ' and the map''s data element for a word is incremented every time' +
     ' the word is encountered. End text text text text text text text text';
  Map.Clear;          // The map is ready for new demo.
  Words.Delimiter := ' ';
  Words.DelimitedText := S;
  for i := 0 to Words.Count-1 do
  begin
    S := LowerCase(Words[I]);
    if Map.Contains(S) then
    begin
      Ptri:= PtrInt(Map[S]);
      Inc(Ptri);
      Map[S] := Pointer(Ptri);
    end
    else begin
      Map[S] := Pointer(1);       // Seen once now.
      Writeln('Seen first time: "' + S + '"');
    end;
  end;
  // Now print out the words that occur more than once (in PrintWordCounts).
  // The order is random again.
  Writeln();
  Writeln('Number of unique words is ' + IntToStr(Map.Count) + '.  These occurred multiply times:');
  Writeln();
  Map.IterateMethod(Nil, PrintWordCounts);

  finally
    Words.Free;
  end;
  // stop program loop
  Terminate;
end;

var
  Application: THashMapDemo;

{$IFDEF WINDOWS}{$R HashMapDemo1.rc}{$ENDIF}

{$R *.res}

begin
  Application:=THashMapDemo.Create(nil);
  Application.Title:='HashMap demo';
  Application.Run;
  Application.Free;
end.
