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
-- Copyright (c) 2009, 2011 Christoph Karl Walter Grein
-- ---------------------------------------------------------------------

with Ada.Text_IO;
use  Ada.Text_IO;

with Safe_Pointers.On_Definite_Types,  -- testees
     Safe_Pointers.On_Indefinite_Types,
     Safe_Pointers.On_Limited_Types;

procedure Test_Pointers is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   2.1
  -- Date      1 July 2011
  --====================================================================
  -- Does not work with Ada 95: Controlled type must be declared at the
  -- library level.
  -- Use Ada 2005, which allows declaration at any level.
  --====================================================================
  -- History:
  -- Author  Version   Date   Reason for change
  --  C.G.   2.1  01.07.2011  Remove illegal code from Limited_Test
  --  C.G.   2.0  29.12.2009  Test the new Accessor Reference function
  --  C.G.   1.0  12.02.2009  Test the new Reference function
  --  C.G.   0.0  26.06.2005  PDL
  --====================================================================

  procedure Definite_Test is
    type Test_Record is record
      X: Integer := 0;
      Y: Float   := 0.0;
    end record;
    package My_Definite_Pointers is new Safe_Pointers.On_Definite_Types (Test_Record);
    use My_Definite_Pointers;
    V: aliased Test_Record;
    Ptr: Safe_Pointer;
  begin
    Put_Line ("Definite_Test");
    Allocate  (Ptr, V);
    Reference (Ptr).Value.X := 10;              Put_Line (Integer'Image (V.X) & Integer'Image (Value (Ptr).X));
    Allocate  (Ptr, Value => (5, 5.5));         Put_Line (Integer'Image (Value (Ptr).X));
    Reference (Ptr).Value.X := -Value (Ptr).X;  Put_Line (Integer'Image (Value (Ptr).X));
    New_Line;
  end Definite_Test;

  procedure Indefinite_Test is
    package My_Indefinite_Pointers is new Safe_Pointers.On_Indefinite_Types (String);
    use My_Indefinite_Pointers;
    Ptr: Safe_Pointer;
  begin
    Put_Line ("Indefinite_Test");
    Allocate  (Ptr, Value => "Test string");  Put_Line (Value (Ptr));
    Reference (Ptr).Value.all  :=  "another val" ;  Put_Line (Value (Ptr));
    New_Line;
  end Indefinite_Test;

  procedure Limited_Test is
    type Lim_Rec is limited record
      X: Integer := 0;
      Y: Float   := 0.0;
    end record;
    package My_Limited_Pointers is new Safe_Pointers.On_Limited_Types (Lim_Rec);
    use My_Limited_Pointers;
    Ptr: Safe_Pointer;
  begin
    Put_Line ("Limited_Test");
    Allocate  (Ptr);               Put_Line (Integer'Image (Reference (Ptr).Value.X));
    Reference (Ptr).Value.X := 2;  Put_Line (Integer'Image (Reference (Ptr).Value.X));
    Deallocate (Ptr);
    begin
      Put_Line (Integer'Image (Reference (Ptr).Value.X));
    exception
      when Constraint_Error => Put_Line ("Constraint_Error");
    end;
  end Limited_Test;

begin

  Definite_Test;
  Indefinite_Test;
  Limited_Test;

end Test_Pointers;
