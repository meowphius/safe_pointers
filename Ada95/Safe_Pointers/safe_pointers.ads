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

with Ada.Finalization;
with Ada.Unchecked_Deallocation;

package Safe_Pointers is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   2.0
  -- Date      18 March 2005
  --====================================================================
  -- Safe pointers work like access types while keeping track of all
  -- pointers to an object.
  --
  -- For each object created, a tracker is created as well which with
  -- each allocation or assignment operation and also when a pointer is
  -- finalized updates the number of pointers designating this object.
  -- The storage occupied by an object and its tracker is automatically
  -- reclaimed if there is no pointer left designating this object after
  -- such an operation.
  -- Storage occupied by an object can also be deallocated explicitly.
  -- With this operation, any other pointers designating this object
  -- are set to a null value. Thus, if X and Y are pointers designating
  -- the same object, erroneous access through Y after deallocation of X
  -- is impossible. The exception Constraint_Error is raised instead.
  --
  -- This package serves as the parent for library units providing the
  -- pointers for several kinds of objects with different sets of opera-
  -- tions (e.g. the new operator for indefinite types is not available
  -- with a subtype indication, a qualified expression has to be given,
  -- see ARM 4.8 (2)).
  -- The following operations are provided (if applicable) as a substi-
  -- tute for the corresponding access types:
  --
  --   Null_Pointer: null (not designating anything)
  --   assignment  : P := Q; (implicit)
  --   "="         : P = Q
  --   Allocate    : P := new Object;
  --   Allocate    : P := new Object'(Value);
  --   Deallocate  : Unchecked_Deallocation (P);
  --   Alias       : P := Object'[Unchecked_]Access;
  --   Assign      : P.all := Value;
  --   Value       : P.all
  --====================================================================
  -- History:
  -- Author  Version   Date   Reason for change
  --  C.G.   1.0  21.03.1997
  --  C.G.   2.0  18.03.2005  Include common part of children
  --====================================================================

private

  generic

    type Object_Pointer is private;  -- an access type

    with procedure Free (X: in out Object_Pointer) is <>;  -- release storage

  package Generic_Tracker is

    type Track is record
      Object      : Object_Pointer;
      Count       : Natural := 0;
      Pool_Element: Boolean := True;
    end record;

    type Tracker is access Track;  pragma Controlled (Tracker);
    procedure Free is new Ada.Unchecked_Deallocation (Track, Tracker);

    Null_Track: constant Tracker := new Track;

    type Safe_Pointer is new Ada.Finalization.Controlled with record
      Track: Tracker := Null_Track;
    end record;

    procedure Adjust   (Pointer: in out Safe_Pointer);
    procedure Finalize (Pointer: in out Safe_Pointer);

  end Generic_Tracker;

end Safe_Pointers;
