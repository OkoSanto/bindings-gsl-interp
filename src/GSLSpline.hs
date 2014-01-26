{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}

module GSLSpline
       ( InterpType(..)
       , spline
       , eval
       , deriv
       , deriv2 ) where

import Foreign hiding (unsafePerformIO)
import Foreign.C.Types
import Foreign.C.String (CString, peekCString)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception

import Data.Dynamic

import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Generic as V

data InterpType = LINEAR | POLYNOMIAL |
                  CSPLINE | CSPLINE_PERIODIC |
                  AKIMA | AKIMA_PERIODIC

data MySpline = MySpline (ForeignPtr GSLSpline) (ForeignPtr GSLAcc)

type Spline = (ForeignPtr GSLSpline, ForeignPtr GSLAcc)

data GSLSpline = GSLSpline

data GSLInterpType = GSLInterpType

data GSLAcc = GSLAcc

foreign import ccall unsafe "gsl_interp.h &gsl_interp_linear"
  c_gsl_interp_linear :: Ptr (Ptr GSLInterpType)

foreign import ccall unsafe "gsl_interp.h &gsl_interp_polynomial"
  c_gsl_interp_polynomial :: Ptr (Ptr GSLInterpType)

foreign import ccall unsafe "gsl_interp.h &gsl_interp_cspline"
  c_gsl_interp_cspline :: Ptr (Ptr GSLInterpType)

foreign import ccall unsafe "gsl_interp.h &gsl_interp_cspline_periodic"
  c_gsl_interp_cspline_periodic :: Ptr (Ptr GSLInterpType)

foreign import ccall unsafe "gsl_interp.h &gsl_interp_akima"
  c_gsl_interp_akima :: Ptr (Ptr GSLInterpType)

foreign import ccall unsafe "gsl_interp.h &gsl_interp_akima_periodic"
  c_gsl_interp_akima_periodic :: Ptr (Ptr GSLInterpType)

foreign import ccall unsafe "gsl_spline.h gsl_interp_accel_alloc"
  c_gsl_interp_accel_alloc :: IO ( Ptr GSLAcc)

foreign import ccall unsafe "gsl_spline.h &gsl_interp_accel_free"
  c_gsl_interp_accel_free :: FunPtr ( Ptr GSLAcc -> IO () )

foreign import ccall unsafe "gsl_spline.h gsl_spline_alloc"
  c_gsl_spline_alloc :: Ptr GSLInterpType -> CSize -> IO (Ptr GSLSpline)

foreign import ccall unsafe "gsl_spline.h &gsl_spline_free"
  c_gsl_spline_free:: FunPtr( Ptr GSLSpline -> IO () )

foreign import ccall unsafe "gsl_spline.h gsl_spline_init"
  c_gsl_spline_init :: Ptr GSLSpline -> Ptr CDouble -> Ptr CDouble -> CSize -> IO ()

foreign import ccall unsafe "gsl_spline.h gsl_spline_eval"
  c_gsl_spline_eval :: Ptr GSLSpline -> CDouble -> Ptr GSLAcc -> IO CDouble

foreign import ccall unsafe "gsl_spline.h gsl_spline_eval_deriv"
  c_gsl_spline_eval_deriv :: Ptr GSLSpline -> CDouble -> Ptr GSLAcc -> IO CDouble

foreign import ccall unsafe "gsl_spline.h gsl_spline_eval"
  c_gsl_spline_eval_deriv2 :: Ptr GSLSpline -> CDouble -> Ptr GSLAcc -> IO CDouble

foreign import ccall unsafe "gsl_spline.h gsl_spline_eval_e"
  c_gsl_spline_eval_e :: Ptr GSLSpline -> CDouble -> Ptr GSLAcc -> Ptr CDouble -> IO CInt

foreign import ccall unsafe "gsl_spline.h gsl_spline_eval_deriv_e"
  c_gsl_spline_eval_deriv_e :: Ptr GSLSpline -> CDouble -> Ptr GSLAcc -> Ptr CDouble -> IO CInt

foreign import ccall unsafe "gsl_spline.h gsl_spline_eval_e"
  c_gsl_spline_eval_deriv2_e :: Ptr GSLSpline -> CDouble -> Ptr GSLAcc -> Ptr CDouble -> IO CInt

foreign import ccall unsafe "gsl_errno.h gsl_strerror"
  c_gsl_strerror :: CInt -> CString

data GSLException = GSLException { errCode :: CInt,
                                   errMessage :: String }
                                   deriving (Show, Typeable)

instance Exception GSLException

initSpline :: InterpType -> CSize -> Ptr CDouble -> Ptr CDouble -> IO (ForeignPtr GSLSpline)
initSpline t size x y =
  let  gsl_interp_type = case t of
         CSPLINE -> c_gsl_interp_cspline
         CSPLINE_PERIODIC -> c_gsl_interp_cspline_periodic
         LINEAR -> c_gsl_interp_linear
         POLYNOMIAL -> c_gsl_interp_polynomial
         AKIMA -> c_gsl_interp_akima
         AKIMA_PERIODIC -> c_gsl_interp_akima_periodic
  in do
    t <- peek gsl_interp_type
    splinePointer <- c_gsl_spline_alloc t size
    if splinePointer == nullPtr then ioError (userError "out of memory")
                                else c_gsl_spline_init splinePointer x y size >>
                                     newForeignPtr c_gsl_spline_free splinePointer

spline :: InterpType -> SV.Vector CDouble -> SV.Vector CDouble -> Spline
spline t xs ys =
  let spline = unsafePerformIO $
               SV.unsafeWith xs $
               \x -> SV.unsafeWith ys $
                     \y -> initSpline t (fromIntegral $ SV.length xs) x y
      acc = unsafePerformIO $ do
            pAccel <- c_gsl_interp_accel_alloc
            if pAccel == nullPtr then ioError (userError "out of memory")
                                 else newForeignPtr c_gsl_interp_accel_free pAccel
  in (spline, acc)

-- Evaluate spline without checking GSL return codes ("unsafe").
call_gsl_spline_function :: (Ptr GSLSpline -> CDouble -> Ptr GSLAcc -> IO CDouble) -> Spline -> CDouble -> CDouble
call_gsl_spline_function f (pSpline, pAcc) x =
  unsafePerformIO $ withForeignPtr pSpline $
  \pSpline -> withForeignPtr pAcc $
              \pAcc -> f pSpline x pAcc

-- EValuate spline and throw exception upon non-zero GSL return code.
call_gsl_spline_function_e :: (Ptr GSLSpline -> CDouble -> Ptr GSLAcc -> Ptr CDouble -> IO CInt) -> Spline -> CDouble -> CDouble
call_gsl_spline_function_e f (pSpline, pAcc) x =
  unsafePerformIO $ withForeignPtr pSpline $
  \pSpline -> withForeignPtr pAcc $
              \pAcc -> do 
              pResult <- mallocForeignPtr
              errCode <- withForeignPtr pResult $ \pResult -> f pSpline x pAcc pResult
              if errCode /= 0 then do errStr <- peekCString $ c_gsl_strerror errCode
                                      throw (GSLException errCode errStr)
                              else do withForeignPtr pResult peek

eval = call_gsl_spline_function_e c_gsl_spline_eval_e

deriv = call_gsl_spline_function_e c_gsl_spline_eval_deriv_e

deriv2 = call_gsl_spline_function_e c_gsl_spline_eval_deriv2_e

evalGeneral sp = realToFrac . ( eval sp ) . realToFrac

splineGeneral interp xs ys = spline interp ( V.map realToFrac $ V.convert xs) (V.map realToFrac $ V.convert ys)
