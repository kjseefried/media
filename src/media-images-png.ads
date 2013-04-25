---------------------------------------------------------------------------
-- FILE    : media-images-png.ads
-- SUBJECT : Package for manipulating PNG image files.
-- AUTHOR  : (C) Copyright 2010 by Peter C. Chapin
--
-- Please send comments or bug reports to
--
--      Peter C. Chapin <PChapin@vtc.vsc.edu>
---------------------------------------------------------------------------
private with Ada.Containers.Indefinite_Vectors;

package Media.Images.PNG is

   -- Raised when an input image is not a valid PNG image.
   Corrupt_Image : exception;

   type PNG_Image is private;

   -- Reads a PNG image from the indicated file. Raises Corrupt_Image if the format is invalid.
   procedure Read(Image : out PNG_Image; File_Name : in String);

   -- Conversion procedures to and from the raw, in-memory image format.
   procedure Convert_To_Raw(Image : in PNG_Image; Raw_Image : out Media.Images.Image);
   procedure Convert_To_PNG(Raw_Image : in Media.Images.Image; Image : out PNG_Image);

   -- Writes a PNG image to the indicated file.
   procedure Write(Image : in PNG_Image; File_Name : in String);

   -- Returns the number of PNG chunks in the image. Returns zero if the image has not been read.
   function Chunk_Count(Image : PNG_Image) return Natural;

private
   type Chunk_Type is (tEXt, zEXt);
   type CRC_Array  is array(0 .. 3) of Media.Images.Image_Byte;
   type Data_Array is array(Natural range <>) of Media.Images.Image_Byte;

   type PNG_Chunk(Size : Natural) is
      record
         Kind : Chunk_Type;
         CRC  : CRC_Array;
         Data : Data_Array(1 .. Size);
      end record;

   package Chunk_Vectors is new Ada.Containers.Indefinite_Vectors
     (Index_Type => Positive, Element_Type => PNG_Chunk);

   type PNG_Image is
      record
         Chunks : Chunk_Vectors.Vector;
      end record;

end Media.Images.PNG;
