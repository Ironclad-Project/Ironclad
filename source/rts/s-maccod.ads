--  s-maccod.ads: Inline assembly utilities.
--
--  This specification is derived from the Ada Reference Manual. In accordance
--  with the copyright of the original source, you can freely copy and modify
--  this specification, provided that if you redistribute a modified version,
--  any changes are clearly indicated.
--
--  This file is based on the distribution by the GNAT project, which is
--  distributed under the GPLv3 with the GCC runtime exception.

package System.Machine_Code with SPARK_Mode => Off, Pure is
   --  All of this works inside GNAT, this is literally just placeholders
   --  because Ada wants it this way. Names and everything are basically
   --  forced.

   type Asm_Input_Operand       is private;
   type Asm_Output_Operand      is private;
   type Asm_Insn                is private;
   type Asm_Input_Operand_List  is array (Integer range <>) of
      Asm_Input_Operand;
   type Asm_Output_Operand_List is array (Integer range <>) of
      Asm_Output_Operand;

   No_Input_Operands  : constant Asm_Input_Operand;
   No_Output_Operands : constant Asm_Output_Operand;

   procedure Asm
      (Template : String;
       Outputs  : Asm_Output_Operand_List;
       Inputs   : Asm_Input_Operand_List;
       Clobber  : String  := "";
       Volatile : Boolean := False);

   procedure Asm
      (Template : String;
       Outputs  : Asm_Output_Operand := No_Output_Operands;
       Inputs   : Asm_Input_Operand_List;
       Clobber  : String  := "";
       Volatile : Boolean := False);

   procedure Asm
      (Template : String;
       Outputs  : Asm_Output_Operand_List;
       Inputs   : Asm_Input_Operand := No_Input_Operands;
       Clobber  : String  := "";
       Volatile : Boolean := False);

   procedure Asm
      (Template : String;
       Outputs  : Asm_Output_Operand := No_Output_Operands;
       Inputs   : Asm_Input_Operand  := No_Input_Operands;
       Clobber  : String  := "";
       Volatile : Boolean := False);

   function Asm
      (Template : String;
       Outputs  : Asm_Output_Operand_List;
       Inputs   : Asm_Input_Operand_List;
       Clobber  : String  := "";
       Volatile : Boolean := False) return Asm_Insn;

   function Asm
      (Template : String;
       Outputs  : Asm_Output_Operand := No_Output_Operands;
       Inputs   : Asm_Input_Operand_List;
       Clobber  : String  := "";
       Volatile : Boolean := False) return Asm_Insn;

   function Asm
      (Template : String;
       Outputs  : Asm_Output_Operand_List;
       Inputs   : Asm_Input_Operand := No_Input_Operands;
       Clobber  : String  := "";
       Volatile : Boolean := False) return Asm_Insn;

   function Asm
      (Template : String;
       Outputs  : Asm_Output_Operand := No_Output_Operands;
       Inputs   : Asm_Input_Operand  := No_Input_Operands;
       Clobber  : String  := "";
       Volatile : Boolean := False) return Asm_Insn;

   pragma Import (Intrinsic, Asm);

private

   type Asm_Input_Operand  is new Integer;
   type Asm_Output_Operand is new Integer;
   type Asm_Insn           is new Integer;

   No_Input_Operands  : constant Asm_Input_Operand  := 0;
   No_Output_Operands : constant Asm_Output_Operand := 0;
end System.Machine_Code;
