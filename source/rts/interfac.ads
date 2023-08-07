--  interfac.ads: Interface datatypes.
--
--  This specification is derived from the Ada Reference Manual. In accordance
--  with the copyright of the original source, you can freely copy and modify
--  this specification, provided that if you redistribute a modified version,
--  any changes are clearly indicated.
--
--  This file is based on the distribution by the GNAT project, which is
--  distributed under the GPLv3 with the GCC runtime exception.

package Interfaces with Pure is
   type Integer_8    is range -2 **   7 .. 2 **   7 - 1 with Size =>   8;
   type Integer_16   is range -2 **  15 .. 2 **  15 - 1 with Size =>  16;
   type Integer_32   is range -2 **  31 .. 2 **  31 - 1 with Size =>  32;
   type Integer_64   is range -2 **  63 .. 2 **  63 - 1 with Size =>  64;
   type Unsigned_8   is mod 2 ** 8   with Size =>   8;
   type Unsigned_16  is mod 2 ** 16  with Size =>  16;
   type Unsigned_32  is mod 2 ** 32  with Size =>  32;
   type Unsigned_64  is mod 2 ** 64  with Size =>  64;

   function Shift_Left
      (Value  : Unsigned_8;
       Amount : Natural) return Unsigned_8
      with Import, Convention => Intrinsic, Static;

   function Shift_Right
      (Value  : Unsigned_8;
       Amount : Natural) return Unsigned_8
      with Import, Convention => Intrinsic, Static;

   function Shift_Right_Arithmetic
      (Value  : Unsigned_8;
       Amount : Natural) return Unsigned_8
      with Import, Convention => Intrinsic, Static;

   function Rotate_Left
      (Value  : Unsigned_8;
       Amount : Natural) return Unsigned_8
      with Import, Convention => Intrinsic, Static;

   function Rotate_Right
      (Value  : Unsigned_8;
       Amount : Natural) return Unsigned_8
      with Import, Convention => Intrinsic, Static;

   function Shift_Left
      (Value  : Unsigned_16;
       Amount : Natural) return Unsigned_16
      with Import, Convention => Intrinsic, Static;

   function Shift_Right
      (Value  : Unsigned_16;
       Amount : Natural) return Unsigned_16
      with Import, Convention => Intrinsic, Static;

   function Shift_Right_Arithmetic
      (Value  : Unsigned_16;
       Amount : Natural) return Unsigned_16
      with Import, Convention => Intrinsic, Static;

   function Rotate_Left
      (Value  : Unsigned_16;
       Amount : Natural) return Unsigned_16
      with Import, Convention => Intrinsic, Static;

   function Rotate_Right
      (Value  : Unsigned_16;
       Amount : Natural) return Unsigned_16
      with Import, Convention => Intrinsic, Static;

   function Shift_Left
      (Value  : Unsigned_32;
       Amount : Natural) return Unsigned_32
      with Import, Convention => Intrinsic, Static;

   function Shift_Right
      (Value  : Unsigned_32;
       Amount : Natural) return Unsigned_32
      with Import, Convention => Intrinsic, Static;

   function Shift_Right_Arithmetic
      (Value  : Unsigned_32;
       Amount : Natural) return Unsigned_32
      with Import, Convention => Intrinsic, Static;

   function Rotate_Left
      (Value  : Unsigned_32;
       Amount : Natural) return Unsigned_32
      with Import, Convention => Intrinsic, Static;

   function Rotate_Right
      (Value  : Unsigned_32;
       Amount : Natural) return Unsigned_32
      with Import, Convention => Intrinsic, Static;

   function Shift_Left
      (Value  : Unsigned_64;
       Amount : Natural) return Unsigned_64
      with Import, Convention => Intrinsic, Static;

   function Shift_Right
      (Value  : Unsigned_64;
       Amount : Natural) return Unsigned_64
      with Import, Convention => Intrinsic, Static;

   function Shift_Right_Arithmetic
      (Value  : Unsigned_64;
       Amount : Natural) return Unsigned_64
      with Import, Convention => Intrinsic, Static;

   function Rotate_Left
      (Value  : Unsigned_64;
       Amount : Natural) return Unsigned_64
      with Import, Convention => Intrinsic, Static;

   function Rotate_Right
      (Value  : Unsigned_64;
       Amount : Natural) return Unsigned_64
      with Import, Convention => Intrinsic, Static;
end Interfaces;
