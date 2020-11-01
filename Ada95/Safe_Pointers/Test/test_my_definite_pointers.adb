-- ---------------------------------------------------------------------
-- This software is provided as is in the hope that it might be found
-- useful.
-- You may use and modify it freely provided you keep this copyright
-- notice unchanged and mark modifications appropriately.
--
-- Bug reports and proposals for improvements are welcome. Please send
-- them to the eMail address below.
--
-- Christoph Karl Walter Grein
-- Hauptstr. 42
-- D-86926 Greifenberg
-- Germany
--
-- eMail:    Christ-Usch.Grein@T-Online.de
-- Internet: http://www.christ-usch-grein.homepage.t-online.de/
--
-- Copyright (c) 1998, 2005 Christoph Karl Walter Grein
-- ---------------------------------------------------------------------

with Ada.Exceptions;

with My_Definite_Pointers;  -- testee
use  My_Definite_Pointers;

with Test_Support;

procedure Test_My_Definite_Pointers is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   3.2
  -- Date      14 April 2005
  --====================================================================
  -- Test program for package Safe_Pointers.
  --====================================================================
  -- History:
  -- Author  Version   Date   Reason for change
  --  C.G.   1.0  16.04.1991
  --  C.G.   2.0  16.03.1997  Ada 95
  --  C.G.   3.0  22.03.1997  Bug fix (Gnat 3.01a did not complain, but
  --                          ObjectAda 7.0 did);
  --                          new design of Safe_Pointers;
  --                          add test for aliased objects
  --  C.G.   3.1  30.08.1997  Test improved (unchecked aliasing)
  --  C.G.   3.2  14.04.2005  Test slightly improved
  --====================================================================

  P, Q: Safe_Pointer;

  I: aliased Integer := -987;  -- ARM 3.10.2: must use Unchecked_Access

begin

  Test_Support.Test_Header
    (Title       => "Test Safe_Pointers",
     Description => "Manipulate safe pointers and compare them with " &
                    "normal (unsafe) ones");

  ----------------------------------------------------------------
  -- 1.
  Test_Support.Test_Step
    (Title       => "No object yet allocated",
     Description => "Perform tests with null pointers");

  Test_Support.Assert (Condition => P = Null_Pointer and Q = Null_Pointer,
                       Message   => "uninitialised P, Q");

  Test_Support.Put_Line ("P := Q;");
  P := Q;
  Test_Support.Assert (Condition => P = Null_Pointer and Q = Null_Pointer,
                       Message   => "P, Q unchanged");

  Test_Support.New_Line;
  Test_Support.Put_Line ("Deallocate P");
  Deallocate (P);
  Test_Support.Assert (Condition => P = Null_Pointer,
                       Message   => "unchanged P");
  Test_Support.Assert (Condition => P = Q,
                       Message   => "P = Q");

  declare
    I: Integer;
  begin
    I := Value (P);
    Test_Support.Assert (Condition => False,
                         Message   => "P.all did not raise Constraint_Error");
  exception
    when Constraint_Error => null;
  end;

  ----------------------------------------------------------------
  -- 2.
  Test_Support.Test_Step
    (Title       => "Allocate object",
     Description => "Perform basic assignment tests");

  Test_Support.Put_Line ("Allocate P");
  Allocate (P);
  Test_Support.Assert (Condition => P /= Q,
                       Message   => "P /= Q");

  Test_Support.New_Line;
  Test_Support.Put_Line ("Q := P;");
  Q := P;
  Test_Support.Assert (Condition => Q = P,
                       Message   => "Q = P");

  Test_Support.New_Line;
  Test_Support.Put_Line ("P.all := 123;");
  Assign (P, 123);
  Test_Support.Assert (Condition => Value (P) = 123,
                       Message   => "P.all = 123");
  Test_Support.Assert (Condition => Value (P) = Value (Q),
                       Message   => "P.all = Q.all");

  Test_Support.New_Line;
  Test_Support.Put_Line ("P := null;");
  P := Null_Pointer;
  Test_Support.Assert (Condition => P /= Q,
                       Message   => "P /= Q");
  Test_Support.Assert (Condition => Value (Q) = 123,
                       Message   => "Q.all = 123 unchanged");

  declare
    I: Integer;
  begin
    I := Value (P);
    Test_Support.Assert (Condition => False,
                         Message   => "P.all did not raise Constraint_Error");
  exception
    when Constraint_Error => null;
  end;

  Test_Support.New_Line;
  Test_Support.Put_Line ("P := Q;");
  P := Q;
  Test_Support.Assert (Condition => P = Q,
                       Message   => "P = Q");
  Test_Support.Assert (Condition => Value (P) = Value (Q),
                       Message   => "P.all = Q.all");

  ----------------------------------------------------------------
  -- 3.
  Test_Support.Test_Step
    (Title       => "Deallocate object",
     Description => "Check that all pointers to the object are deallocated; " &
                    "perform dereferencing tests");

  Test_Support.Put_Line ("Deallocate P");
  Deallocate (P);
  Test_Support.Assert (Condition => P = Null_Pointer,
                       Message   => "P = null");
  Test_Support.Assert (Condition => P = Q,     -- Q must also be deallocated
                       Message   => "P = Q");
  Test_Support.Assert (Condition => Q = Null_Pointer,
                       Message   => "Q = null");

  declare
    I: Integer;
  begin
    I := Value (P);
    Test_Support.Assert (Condition => False,
                         Message   => "P.all did not raise Constraint_Error");
  exception
    when Constraint_Error => null;
  end;

  declare
    I: Integer;
  begin
    I := Value (Q);
    Test_Support.Assert (Condition => False,
                         Message   => "Q.all did not raise Constraint_Error");
  exception
    when Constraint_Error => null;
  end;

  ----------------------------------------------------------------
  -- 4.
  Test_Support.Test_Step
    (Title       => "Allocate two objects",
     Description => "Perform further assignment tests");

  Test_Support.Put_Line ("Allocate P");
  Allocate (P, -123);

  Test_Support.Put_Line ("Allocate Q");
  Allocate (Q, -123);

  Test_Support.Assert (Condition => P /= Q,
                       Message   => "P /= Q");
  Test_Support.Assert (Condition => Value (P) = -123,
                       Message   => "P.all = -123");
  Test_Support.Assert (Condition => Value (P) = Value (Q),
                       Message   => "P.all = Q.all");

  Assign (P, -321);
  Test_Support.Assert (Condition => Value (P) /= Value (Q),
                       Message   => "P.all /= Q.all");

  Test_Support.New_Line;
  Test_Support.Put_Line ("P := Q;");
  P := Q;
  Test_Support.Assert (Condition => P = Q,
                       Message   => "P = Q");

  Test_Support.New_Line;
  Test_Support.Put_Line ("Allocate P");
  Allocate (P, 12345);
  Test_Support.Assert (Condition => P /= Q,
                       Message   => "P /= Q");
  Test_Support.Assert (Condition => Value (P) = 12345,
                       Message   => "P.all = 12345");

  ----------------------------------------------------------------
  -- 5.
  Test_Support.Test_Step
    (Title       => "Controlling",
     Description => "Perform more elaborate tests with pointers " &
                    "going out of scope");

  declare
    R: Safe_Pointer;
  begin
    Test_Support.Put_Line ("R := Q;");
    R := Q;
    Test_Support.Assert (Condition => R = Q,
                         Message   => "R = Q");
  end;

  Test_Support.Put_Line ("S := P;");
  declare
    S: Safe_Pointer := P;
  begin
    Test_Support.Assert (Condition => S = P,
                         Message   => "S = P");
  end;

  ----------------------------------------------------------------
  -- 6.
  Test_Support.Test_Step
    (Title       => "Aliasing",
     Description => "Tests operations with aliased objects");

  begin
    declare

      L: Safe_Pointer;

    begin

      Test_Support.Put_Line ("Allocate L");
      Allocate (L, 1);

      Test_Support.Assert (Condition => Value (L) = 1,
                           Message   => "L.all = 1");

      Test_Support.Put_Line ("Alias L");
      begin
        Alias (L, I'Access);  -- ARM 3.10.2
        Test_Support.Assert (Condition => False,
                             Message   => "Compiler bug if you see this output");
        Test_Support.Assert (Condition => Value (L) = -987,
                             Message   => "L.all = -987",
                             Only_Report_Error => False);
      exception
        when Program_Error =>
          Test_Support.Put_Line ("Access attribute raised Program_Error, L is destroyed");
      end;

      -- L is now no longer usable, its finalization raises Program_Error.

      Test_Support.Put_Line ("L := null;");
      L := Null_Pointer;

      Test_Support.Assert (Condition => False,
                           Message   => "You will not see this line on the output!");

    end;  -- we need a double block!
  exception

    when Program_Error =>
      Test_Support.Put_Line ("L is now ""kaputt"" - finalization raised Program_Error");

  end;

  Test_Support.New_Line;
  Test_Support.Put_Line ("Alias P unchecked");
  Alias (P, I'Unchecked_Access);  -- ARM 3.10.2
  Test_Support.Assert (Condition => Value (P) = -987,
                       Message   => "P.all = -987");

  Test_Support.Put_Line ("P := null;");
  P := Null_Pointer;
  Test_Support.Assert (Condition => P = Null_Pointer,
                       Message   => "P = null");

  ----------------------------------------------------------------
  -- 7.
  Test_Support.Test_Step
    (Title       => "Aliasing",
     Description => "Unchecked aliasing lets access out-of-scope objects");

  declare
    O: aliased Integer := 123;
    R: Safe_Pointer;
  begin
    Alias (R, O'Unchecked_Access);
    P := R;
    Test_Support.Assert (Condition => Value (P) = 123,
                         Message   => "P.all = 123");
  end;  -- out-of-scope O remains accessible!

  Test_Support.Assert (Condition => Value (P) = 123,
                       Message   => "P.all = 123",
                       Only_Report_Error => False);

  ----------------------------------------------------------------
  Test_Support.Test_Result;

exception

  when Ex: others => Test_Support.Put_Line (Ada.Exceptions.Exception_Name (Ex));
                     Test_Support.Put_Line (Ada.Exceptions.Exception_Information (Ex));
                     Test_Support.Assert (False, "Something went wrong.");
                     Test_Support.Test_Result;

end Test_My_Definite_Pointers;
