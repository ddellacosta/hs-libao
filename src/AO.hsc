{-# LANGUAGE ForeignFunctionInterface #-}

module AO where
import Foreign.C -- get the C types
import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils
import Unsafe.Coerce (unsafeCoerce)

#include <ao/ao.h>

-- typedef struct ao_option {
-- 	char *key;
-- 	char *value;
-- 	struct ao_option *next;
-- } ao_option;

-- data AOOption = AOOption { key :: Ptr CChar, value :: Ptr CChar, next :: Ptr AOOption }

-- 
-- Call ao_initialize() to initialize the library. This loads the plugins from disk, reads the libao configuration files, and identifies an appropriate default output driver if none is specified in the configuration files.

-- void ao_initialize(void);

foreign import ccall unsafe "ao_initialize"
  ao_initialize :: IO ()


-- Call ao_default_driver_id() to get the ID number of the default output driver. This may not be successful if no audio hardware is available, it is in use, or is not in the "standard" configuration. If you want to specify a particular output driver, you may call ao_driver_id() with a string corresponding to the short name of the device (i.e. "oss", "wav", etc.) instead.

-- int      ao_default_driver_id(void);

foreign import ccall unsafe "ao_default_driver_id"
  ao_default_driver_id :: CInt
  

-- If you are using the default device, no extra options are needed. However, if you wish to to pass special options to the driver, you will need to:
-- 
--     Create an option list pointer of type (ao_option *) and initialize it to NULL.
-- 
--     Through successive calls to ao_append_option(), add any driver-specific options you need. Note that the options take the form of key/value pairs where supported keys are listed in the driver documentation. 


-- Call ao_open_live() and save the returned device pointer. If you are using a file output driver, you will need to call ao_open_file() instead.

-- ao_device*       ao_open_live(int driver_id,
--                               ao_sample_format *format,
--                               ao_option *option);

-- typedef struct ao_sample_format {
-- 	int  bits; /* bits per sample */
-- 	int  rate; /* samples per second (in a single channel) */
-- 	int  channels; /* number of audio channels */
-- 	int  byte_format; /* Byte ordering in sample, see constants below */
--         char *matrix; /* input channel location/ordering */
-- } ao_sample_format;

data AOSampleFormat = AOSampleFormat { bits :: CInt, rate :: CInt, channels :: CInt, byte_format :: CInt } deriving Show

-- https://www.haskell.org/haskellwiki/FFI_cook_book#Working_with_structs
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable AOSampleFormat where
  sizeOf _    = #{size ao_sample_format}
  alignment _ = #{alignment ao_sample_format} -- see #let above
  peek ptr = do
    bits        <- #{peek ao_sample_format, bits} ptr
    rate        <- #{peek ao_sample_format, rate} ptr
    channels    <- #{peek ao_sample_format, channels} ptr
    byte_format <- #{peek ao_sample_format, byte_format} ptr
    return (AOSampleFormat { bits = bits, rate = rate, channels = channels, byte_format = byte_format })
  poke ptr (AOSampleFormat bits rate channels byte_format) = do
    #{poke ao_sample_format, bits} ptr bits
    #{poke ao_sample_format, rate} ptr rate
    #{poke ao_sample_format, channels} ptr channels
    #{poke ao_sample_format, byte_format} ptr byte_format
    where maxLen = #{const 1024} -- totally making this up

data AOOption
type AOOptionPtr = Ptr AOOption
    
data AODevice
type AODevicePtr = Ptr AODevice

foreign import ccall unsafe "ao_open_live"
  ao_open_live :: CInt -> Ptr AOSampleFormat -> AOOptionPtr -> AODevicePtr 

aoOpenLive deviceId fmt = do
  fmtPtr <- new fmt
  return (ao_open_live deviceId fmtPtr nullPtr)
  
-- 
-- Call ao_play() to output each block of audio.

-- int                   ao_play(ao_device *device,
--                               char *output_samples,
--                               uint_32 num_bytes);

foreign import ccall unsafe "ao_play"
  ao_play :: AODevicePtr -> Ptr CChar -> CUInt -> CInt

aoPlay :: AODevicePtr -> AOSampleFormat -> CInt -> [CChar] -> IO CInt
aoPlay devicePtr fmt bufSize buffer = do
    withArray buffer (\b -> return $ ao_play devicePtr b $ unsafeCoerce bufSize)

-- Call ao_close() to close the device. Note that this will automatically free the memory that was allocated for the device. Do not attempt to free the device pointer yourself!
-- int                  ao_close(ao_device *device);

foreign import ccall unsafe "ao_close"
  ao_close :: AODevicePtr -> CInt

-- Call ao_shutdown() to close the library.
-- void ao_shutdown(void);

foreign import ccall unsafe "ao_shutdown"
  ao_shutdown :: IO ()

