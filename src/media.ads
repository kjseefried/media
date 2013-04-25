---------------------------------------------------------------------------
-- FILE    : media.ads
-- SUBJECT : Parent package for the Media Tools library.
-- AUTHOR  : (C) Copyright 2010 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------

package Media is

   -- A general exception to be raised by subprograms that are just place holders.
   Not_Implemented : exception;

   type Byte is mod 2**8;
   for Byte'Size use 8;

   function Shift_Left(Value : Byte; Count : Natural) return Byte;
   pragma Import(Intrinsic, Shift_Left);

   function Shift_Right(Value : Byte; Count : Natural) return Byte;
   pragma Import(Intrinsic, Shift_Right);

   function Rotate_Left(Value : Byte; Count : Natural) return Byte;
   pragma Import(Intrinsic, Rotate_Left);

   function Rotate_Right(Value : Byte; Count : Natural) return Byte;
   pragma Import(Intrinsic, Rotate_Right);

end Media;
