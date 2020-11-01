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
-- Copyright (c) 1998, 2005, 2009, 2011, 2014
--               Christoph Karl Walter Grein
-- ---------------------------------------------------------------------

generic

  type Object is limited private;

  with procedure Free (X: in out Object) is null;

package Safe_Pointers.On_Limited_Types is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   4.3
  -- Date      14 July 2014
  --====================================================================
  -- Ada 95 had return-by-reference types, RM 6.5(11):
  --   function Value (Pointer: Safe_Pointer) return Object;
  -- returned a constant view.
  -- Ada 2005 removed return-by-reference types and introduced instead
  -- return object creation in place.
  -- The generic formal procedure shall be supplied if Object has
  -- components that need to be freed together with the pointer.
  --====================================================================
  -- History:
  -- Author  Version   Date   Reason for change
  --  C.G.   1.0  21.03.1997
  --  C.G.   2.0  18.03.2005  Separated common part into parent
  --  C.G.   3.0  11.02.2009  Ada 2005: Removed illegal function Value;
  --                          new function Reference
  --  C.G.   3.1  03.12.2009  Removed unneeded with-clause
  --  C.G.   4.0  29.12.2009  Reference via Accessor; removed Alias
  --  C.G.   4.1  01.07.2011  Added generic formal Free
  --  C.G.   4.2  13.07.2011  Ada 2012 drops pragma Controlled
  --  C.G.   4.3  14.07.2014  Bug fix RM 7.5(2/2) Accessor (full def.)
  --====================================================================

  type Safe_Pointer is private;

  type Accessor (Value: not null access Object) is limited private;

  function Null_Pointer return Safe_Pointer;

  function "=" (Left, Right: Safe_Pointer) return Boolean;

  procedure Allocate   (Pointer: in out Safe_Pointer);
  procedure Deallocate (Pointer: in out Safe_Pointer);
  function  Reference  (Pointer:        Safe_Pointer) return Accessor;

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

end Safe_Pointers.On_Limited_Types;
