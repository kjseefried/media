---------------------------------------------------------------------------
-- FILE    : media-images.ads
-- SUBJECT : Parent package for the Media image library.
-- AUTHOR  : (C) Copyright 2010 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------

package Media.Images is

   -- A type to represent 8 bit values used in image files. Note that this type is used for all
   -- image file information, including raw headers and other non-image data.
   --
   type Image_Byte is new Media.Byte;

   -- A general purpose, in memory representation of an image. This type is used for general
   -- image operations. There are (will be) conversions to and from this type to the various
   -- image format types defined in child packages.
   --
   type Image is private;

private

   type Image is
      record
         Place_Holder : Integer;
      end record;

end Media.Images;
