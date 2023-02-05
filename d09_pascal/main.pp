
Program Main;

Uses Sysutils;

Type Strings = Array Of String;

Type TCommand = (Right, Up, Left, Down);

Type 
  TCommands = Record
    Commands : Array Of TCommand;
    OK : Boolean;
  End;

Type 
  TPoint = Record
    X, Y : Int64;
  End;

Type TPoints = array Of TPoint;

Const ROPE_LENGTH = 10;

Type TRope = array[0..(ROPE_LENGTH - 1)] Of TPoint;




Function SplitStr(Const InputString : String; Delimiter : String): Strings;

Var 
  WordStart   : Int64 = 1;
  WordEnd     : Int64;
  CurrentWord : String;
  Words       : Strings = ();
  WordCount   : Int64 = 0;
Begin
  // Continue processing the string as long as there are more characters in the input string
  While WordStart <= Length(InputString) Do
    Begin
      // Find the index of the next delimiter
      WordEnd := Pos(Delimiter, InputString, WordStart);
      // If there is no more delimiter, set the WordEnd to be the end of the string
      If WordEnd = 0 Then
        WordEnd := Length(InputString) + 1;
      // Get the current word by copying the characters between WordStart and WordEnd
      CurrentWord := Copy(InputString, WordStart, WordEnd - WordStart);
      // Increment the word count
      Inc(WordCount);
      // Resize the Words array to fit the new word
      SetLength(Words, WordCount);
      // Store the current word in the Words array
      Words[WordCount - 1] := CurrentWord;
      // Update the WordStart to be after the delimiter
      WordStart := WordEnd + 1;
    End;
  // Return the split string as an array of strings
  SplitStr := Words;
End;

Function ReadFile(Const Filename: String): TCommands;

Var 
  FileInput     : Text;
  Line          : String;
  i             : Int64 = 0;
  CommandsCount : Int64 = 0;
  Commands      : Array Of TCommand = ();
  Words         : Strings;
Begin
  // Open the input file and assign the handle to FileInput
  Assign(FileInput, Filename);
   {$I-}
  Reset(FileInput);
   {$I+}

  // Check if the file was successfully opened
  If IOResult <> 0 Then
    Begin
      // If the file couldn't be opened, print an error message and return False
      WriteLn('Error opening file');
      ReadFile.OK := False;
      Exit;
    End;

  // Read each line of the input file until the end of file (EOF) is reached
  While Not Eof(FileInput) Do
    Begin
      ReadLn(FileInput, Line);
      // Split the line into an array of strings using the SplitStr function
      Words := SplitStr(Line, ' ');
      // Repeat the command specified in the line for the number of times specified in the line
      For i:=1 To StrToInt(Words[1]) Do
        Begin
          // Increase the number of commands
          Inc(CommandsCount);
          // Resize the Commands array to fit the new command
          SetLength(Commands, CommandsCount);
          // Add the new command based on the first string in the line
          Case Words[0] Of 
            'R': Commands[CommandsCount - 1] := Right;
            'U': Commands[CommandsCount - 1] := Up;
            'L': Commands[CommandsCount - 1] := Left;
            'D': Commands[CommandsCount - 1] := Down;
          End;
        End;
    End;

  // Close the input file
  Close(FileInput);

  // Set the Commands array in the TCommands result and return OK = True
  ReadFile.Commands := Commands;
  ReadFile.OK := True;
End;




{
This function moves the head position based on the command received.
The head position is updated by incrementing or decrementing the X or Y coordinate based on the command received.
It returns the updated TPoint variable representing the head position.
}
Function MoveHead(Head : TPoint; Const Command : TCommand) : TPoint;
Begin
  Case Command Of 
    Right : Head.X := Head.X + 1;
    Up    : Head.Y := Head.Y - 1;
    Left  : Head.X := Head.X - 1;
    Down  : Head.Y := Head.Y + 1;
  End;

  MoveHead := Head;
End;

// This function returns the sign of the input integer value.
Function Sign(Const X: Integer): Integer;
Begin
  If X < 0 Then
    Sign := -1
  Else If X > 0 Then
         Sign := 1
  Else
    Sign := 0;
End;



// This function calculates the new position of the tail given the position of the head and the current tail.
Function MoveTail(Const Head, Tail : TPoint ) : TPoint;

Var NewTail : TPoint;
Begin
  NewTail.X := Tail.X;
  NewTail.Y := Tail.Y;

  // If the distance between the head and the tail in either X or Y direction is greater than 1
  If (Abs(Head.X - Tail.X) > 1)
     Or (Abs(Head.Y - Tail.Y) > 1) Then
    Begin
      // New position of the tail is calculated by adding the sign of the difference between 
      // the head and tail position in both X and Y direction to the current tail position.
      NewTail.X := Tail.X + Sign(Head.X - Tail.X);
      NewTail.Y := Tail.Y + Sign(Head.Y - Tail.Y);
    End;

  MoveTail := NewTail;
End;

// This procedure updates the position of the rope based on the movement of the head.
Procedure MoveRope(Const NewHead : TPoint; Var Rope : TRope);

Var I : Int64;
Begin
  // Loop through all elements of the rope, starting from the first element
  For I := 0 To ROPE_LENGTH - 1 Do
    Begin
      // If this is the first element, set its position to the position of the new head
      If I = 0 Then
        Begin
          Rope[0].X := NewHead.X;
          Rope[0].Y := NewHead.Y;
        End
        // Otherwise, update its position based on the previous element
      Else
        Begin
          Rope[I] := MoveTail(Rope[I-1], Rope[I]);
        End;
    End;
End;



{
The function checks if the NewRecord is already present in the array Points.
If it is not present, the function adds the NewRecord to the array Points and returns True.
}
Function AddUniq(Var Points : TPoints; Const NewRecord: TPoint): Boolean;

Var 
  I: Int64;
  PointsSize : Int64;
Begin
  PointsSize := Length(Points);
  // Loop through the existing points in the array "Points".
  For I := 0 To PointsSize - 1 Do
    Begin
      If I < PointsSize Then
        Begin
          // Check if the current point in the loop matches the "NewRecord".
          If (Points[I].X = NewRecord.X) And (Points[I].Y = NewRecord.Y) Then
            Begin
              // If it matches, return False as the point is already present in the array.
              AddUniq := False;
              Exit;
            End;
        End
    End;
  // If the point is not present in the array, increase the size of the array.
  Inc(PointsSize);
  SetLength(Points, PointsSize);
  // Add the new point to the array.
  Points[PointsSize - 1] := NewRecord;
  // Return True to indicate that the point was added successfully to the array.
  AddUniq := True;
End;

Function Part1(Const Commands : Array Of TCommand) : Int64;

Var 
  Head : TPoint = (X: 0; Y: 0);
  Tail : TPoint = (X: 0; Y: 0);
  VisitedPoints : TPoints = ();
  i : Int64;
Begin
  For i := 0 To Length(Commands) - 1 Do
    Begin
      Head := MoveHead(Head, Commands[i]);
      Tail := MoveTail(Head, Tail);
      AddUniq(VisitedPoints, Tail);
    End;
  Part1 := Length(VisitedPoints)
End;

Function Part2(Const Commands : Array Of TCommand) : Int64;

Var 
  Head : TPoint = (X: 0; Y: 0);
  Rope : TRope;
  VisitedPoints : TPoints = ();
  i : Int64;
Begin
  For i := 0 To (ROPE_LENGTH - 1) Do
    Begin
      Rope[i].X := 0;
      Rope[i].Y := 0;
    End;
  For i := 0 To Length(Commands) - 1 Do
    Begin
      Head := MoveHead(Head, Commands[i]);
      MoveRope(Head, Rope);
      AddUniq(VisitedPoints, Rope[(ROPE_LENGTH - 1)]);
    End;
  Part2 := Length(VisitedPoints)
End;

Var 
  Input : TCommands;
  Filename : String;
Begin
  If ParamCount = 1 Then
    Begin
      Filename := ParamStr(1);
      Input := ReadFile(Filename);

      If Input.OK Then
        Begin
          Writeln('Part 1: ', Part1(Input.Commands));
          Writeln('Part 2: ', Part2(Input.Commands));
        End;
    End
  Else Writeln('No command line argument was entered.');
End.
