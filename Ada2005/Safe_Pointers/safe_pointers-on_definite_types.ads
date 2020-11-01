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
-- Copyright (c) 1998, 2005, 2009, 2011, 2014 Christoph Karl Walter
-- Grein
-- ---------------------------------------------------------------------

generic

  type Object is private;

  with procedure Free (X: in out Object) is null;

package Safe_Pointers.On_Definite_Types is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   6.3
  -- Date      14 July 2014
  --====================================================================
  -- The generic formal procedure shall be supplied if Object has
  -- components that need to be freed together with the pointer.
  --====================================================================
  -- History:
  -- Author  Version   Date   Reason for change
  --  C.G.   1.0  11.04.1991
  --  C.G.   2.0  15.03.1997  Ada 95
  --  C.G.   2.1  16.03.1997  Add procedure Alias
  --  C.G.   3.0  21.03.1997  New design: make child unit
  --  C.G.   4.0  18.03.2005  Separated common part into parent
  --  C.G.   5.0  11.02.2009  Ada 2005: new function Reference
  --  C.G.   5.1  03.12.2009  Removed unneeded with-clause
  --  C.G.   6.0  29.12.2009  Reference via Accessor; removed Alias
  --  C.G.   6.1  27.06.2011  Added generic formal Free
  --  C.G.   6.2  13.07.2011  Ada 2012 drops pragma Controlled
  --  C.G.   6.3  14.07.2014  Bug fix RM 7.5(2/2) Accessor (full def.)
  --====================================================================

  type Safe_Pointer is private;

  type Accessor (Value: not null access Object) is limited private;

  function Null_Pointer return Safe_Pointer;

  function "=" (Left, Right: Safe_Pointer) return Boolean;

  procedure Allocate   (Pointer: in out Safe_Pointer);
  procedure Allocate   (Pointer: in out Safe_Pointer; Value: in Object);
  procedure Deallocate (Pointer: in out Safe_Pointer);
  procedure Assign     (Pointer: in     Safe_Pointer; Value: in Object);
  function  Value      (Pointer:        Safe_Pointer) return    Object;
  function  Reference  (Pointer:        Safe_Pointer) return    Accessor;

private

  type Unsafe_Pointer is access Object;
  procedure Free (X: in out Unsafe_Pointer);

  package Tracker_Instance is new Generic_Tracker (Unsafe_Pointer);

  type Safe_Pointer is new Tracker_Instance.Safe_Pointer with null record;

  --

  type Limiter (Outer: not null access Accessor) is
    new Ada.Finalization.Limited_Controlled with null record;

  type Accessor (Value: not null access Object) is limited record
    Safe: Safe_Pointer;
    Self: Limiter (Accessor'Access) := (Ada.Finalization.Limited_Controlled with Outer => Accessor'Access);
  end record;

  overriding procedure Finalize (Object: in out Limiter);

end Safe_Pointers.On_Definite_Types;
