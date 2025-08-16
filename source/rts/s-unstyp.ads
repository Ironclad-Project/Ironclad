--  s-unstyp.ads: Generic unsigned types.
--
--  This specification is derived from the Ada Reference Manual. In accordance
--  with the copyright of the original source, you can freely copy and modify
--  this specification, provided that if you redistribute a modified version,
--  any changes are clearly indicated.
--
--  This file is based on the distribution by the GNAT project, which is
--  distributed under the GPLv3 with the GCC runtime exception.

package System.Unsigned_Types with Pure is
   pragma Warnings (Off, "(not yet supported)", Reason => "Clear output");

   type Short_Short_Unsigned    is mod 2 ** Short_Short_Integer'Size;
   type Short_Unsigned          is mod 2 ** Short_Integer'Size;
   type Unsigned                is mod 2 ** Integer'Size;
   type Long_Unsigned           is mod 2 ** Long_Integer'Size;
   type Long_Long_Unsigned      is mod 2 ** Long_Long_Integer'Size;
   type Long_Long_Long_Unsigned is mod Max_Binary_Modulus;

   type Packed_Byte is mod 2 ** 8;
   for Packed_Byte'Size use 8;
   pragma Universal_Aliasing (Packed_Byte);
   --  Component type for Packed_Bytes1, Packed_Bytes2 and Packed_Byte4 arrays.
   --  As this type is used by the compiler to implement operations on user
   --  packed array, it needs to be able to alias any type.

   type Packed_Bytes1 is array (Natural range <>) of aliased Packed_Byte;
   for Packed_Bytes1'Alignment use 1;
   for Packed_Bytes1'Component_Size use Packed_Byte'Size;
   pragma Suppress_Initialization (Packed_Bytes1);
   --  This is the type used to implement packed arrays where no alignment
   --  is required. This includes the cases of 1,2,4 (where we use direct
   --  masking operations), and all odd component sizes (where the clusters
   --  are not aligned anyway, see, e.g. System.Pack_07 in file s-pack07
   --  for details.

   type Packed_Bytes2 is new Packed_Bytes1;
   for Packed_Bytes2'Alignment use Integer'Min (2, Standard'Maximum_Alignment);
   pragma Suppress_Initialization (Packed_Bytes2);
   --  This is the type used to implement packed arrays where an alignment
   --  of 2 (is possible) is helpful for maximum efficiency of the get and
   --  set routines in the corresponding library unit. This is true of all
   --  component sizes that are even but not divisible by 4 (other than 2 for
   --  which we use direct masking operations). In such cases, the clusters
   --  can be assumed to be 2-byte aligned if the array is aligned. See for
   --  example System.Pack_10 in file s-pack10).

   type Packed_Bytes4 is new Packed_Bytes1;
   for Packed_Bytes4'Alignment use Integer'Min (4, Standard'Maximum_Alignment);
   pragma Suppress_Initialization (Packed_Bytes4);
   --  This is the type used to implement packed arrays where an alignment
   --  of 4 (if possible) is helpful for maximum efficiency of the get and
   --  set routines in the corresponding library unit. This is true of all
   --  component sizes that are divisible by 4 (other than powers of 2, which
   --  are either handled by direct masking or not packed at all). In such
   --  cases the clusters can be assumed to be 4-byte aligned if the array
   --  is aligned (see System.Pack_12 in file s-pack12 as an example).

   type Rev_Packed_Bytes1 is new Packed_Bytes1;
   pragma Suppress_Initialization (Rev_Packed_Bytes1);
   --  This is equivalent to Packed_Bytes1, but for packed arrays with reverse
   --  scalar storage order. But the Scalar_Storage_Order attribute cannot be
   --  set directly here, see Exp_Pakd for more details.

   type Rev_Packed_Bytes2 is new Packed_Bytes2;
   pragma Suppress_Initialization (Rev_Packed_Bytes2);
   --  This is equivalent to Packed_Bytes2, but for packed arrays with reverse
   --  scalar storage order. But the Scalar_Storage_Order attribute cannot be
   --  set directly here, see Exp_Pakd for more details.

   type Rev_Packed_Bytes4 is new Packed_Bytes4;
   pragma Suppress_Initialization (Rev_Packed_Bytes4);
   --  This is equivalent to Packed_Bytes4, but for packed arrays with reverse
   --  scalar storage order. But the Scalar_Storage_Order attribute cannot be
   --  set directly here, see Exp_Pakd for more details.

   type Bits_1 is mod 2**1;
   type Bits_2 is mod 2**2;
   type Bits_4 is mod 2**4;
   --  Types used for packed array conversions

   subtype Bytes_F is Packed_Bytes4 (1 .. Float'Size / 8);
   --  Type used in implementation of Is_Negative intrinsic (see Exp_Intr)

   function Shift_Left
     (Value  : Short_Short_Unsigned;
      Amount : Natural) return Short_Short_Unsigned;

   function Shift_Right
     (Value  : Short_Short_Unsigned;
      Amount : Natural) return Short_Short_Unsigned;

   function Shift_Right_Arithmetic
     (Value  : Short_Short_Unsigned;
      Amount : Natural) return Short_Short_Unsigned;

   function Rotate_Left
     (Value  : Short_Short_Unsigned;
      Amount : Natural) return Short_Short_Unsigned;

   function Rotate_Right
     (Value  : Short_Short_Unsigned;
      Amount : Natural) return Short_Short_Unsigned;

   function Shift_Left
     (Value  : Short_Unsigned;
      Amount : Natural) return Short_Unsigned;

   function Shift_Right
     (Value  : Short_Unsigned;
      Amount : Natural) return Short_Unsigned;

   function Shift_Right_Arithmetic
     (Value  : Short_Unsigned;
      Amount : Natural) return Short_Unsigned;

   function Rotate_Left
     (Value  : Short_Unsigned;
      Amount : Natural) return Short_Unsigned;

   function Rotate_Right
     (Value  : Short_Unsigned;
      Amount : Natural) return Short_Unsigned;

   function Shift_Left
     (Value  : Unsigned;
      Amount : Natural) return Unsigned;

   function Shift_Right
     (Value  : Unsigned;
      Amount : Natural) return Unsigned;

   function Shift_Right_Arithmetic
     (Value  : Unsigned;
      Amount : Natural) return Unsigned;

   function Rotate_Left
     (Value  : Unsigned;
      Amount : Natural) return Unsigned;

   function Rotate_Right
     (Value  : Unsigned;
      Amount : Natural) return Unsigned;

   function Shift_Left
     (Value  : Long_Unsigned;
      Amount : Natural) return Long_Unsigned;

   function Shift_Right
     (Value  : Long_Unsigned;
      Amount : Natural) return Long_Unsigned;

   function Shift_Right_Arithmetic
     (Value  : Long_Unsigned;
      Amount : Natural) return Long_Unsigned;

   function Rotate_Left
     (Value  : Long_Unsigned;
      Amount : Natural) return Long_Unsigned;

   function Rotate_Right
     (Value  : Long_Unsigned;
      Amount : Natural) return Long_Unsigned;

   function Shift_Left
     (Value  : Long_Long_Unsigned;
      Amount : Natural) return Long_Long_Unsigned;

   function Shift_Right
     (Value  : Long_Long_Unsigned;
      Amount : Natural) return Long_Long_Unsigned;

   function Shift_Right_Arithmetic
     (Value  : Long_Long_Unsigned;
      Amount : Natural) return Long_Long_Unsigned;

   function Rotate_Left
     (Value  : Long_Long_Unsigned;
      Amount : Natural) return Long_Long_Unsigned;

   function Rotate_Right
     (Value  : Long_Long_Unsigned;
      Amount : Natural) return Long_Long_Unsigned;

   function Shift_Left
     (Value  : Long_Long_Long_Unsigned;
      Amount : Natural) return Long_Long_Long_Unsigned;

   function Shift_Right
     (Value  : Long_Long_Long_Unsigned;
      Amount : Natural) return Long_Long_Long_Unsigned;

   function Shift_Right_Arithmetic
     (Value  : Long_Long_Long_Unsigned;
      Amount : Natural) return Long_Long_Long_Unsigned;

   function Rotate_Left
     (Value  : Long_Long_Long_Unsigned;
      Amount : Natural) return Long_Long_Long_Unsigned;

   function Rotate_Right
     (Value  : Long_Long_Long_Unsigned;
      Amount : Natural) return Long_Long_Long_Unsigned;

   pragma Import (Intrinsic, Shift_Left);
   pragma Import (Intrinsic, Shift_Right);
   pragma Import (Intrinsic, Shift_Right_Arithmetic);
   pragma Import (Intrinsic, Rotate_Left);
   pragma Import (Intrinsic, Rotate_Right);
end System.Unsigned_Types;
